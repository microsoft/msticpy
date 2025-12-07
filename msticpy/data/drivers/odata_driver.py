# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""OData Driver class."""
from __future__ import annotations

import abc
import logging
import re
import urllib.parse
from pathlib import Path
from typing import TYPE_CHECKING, Any, ClassVar

import httpx
import pandas as pd
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.serialization import Encoding
from cryptography.x509 import Certificate, load_der_x509_certificate
from msal.application import ConfidentialClientApplication
from typing_extensions import Self

from ..._version import VERSION
from ...auth.msal_auth import MSALDelegatedAuth
from ...common.exceptions import MsticpyConnectionError, MsticpyUserConfigError
from ...common.pkg_config import get_config
from ...common.provider_settings import get_provider_settings
from ...common.utility import mp_ua_header
from .driver_base import DriverBase, DriverProps, QuerySource

if TYPE_CHECKING:
    from collections.abc import Iterable

    from msticpy.common.provider_settings import ProviderSettings

__version__: str = VERSION
__author__: str = "Pete Bryan"

_HELP_URI: str = (
    "https://msticpy.readthedocs.io/en/latest/data_acquisition"
    "/DataProviders.html#connecting-to-an-odata-source"
)

logger: logging.Logger = logging.getLogger(__name__)

# pylint: disable=too-many-instance-attributes


class OData(DriverBase):
    """Parent class to retrieve date from an oauth based API."""

    CONFIG_NAME: ClassVar[str] = ""
    _ALT_CONFIG_NAMES: ClassVar[Iterable[str]] = []

    def __init__(
        self: OData,
        *,
        debug: bool = False,
        max_threads: int = 4,
        **kwargs,
    ) -> None:
        """
        Instantiate OData driver and optionally connect.

        Parameters
        ----------
        connect: bool, optional
            Set true if you want to connect to the provider at initialization

        """
        super().__init__(**kwargs)
        logger.info("Initializing OData driver: %s", self.__class__.__name__)
        logger.debug("Debug mode: %s, Max threads: %d", debug, max_threads)
        self.oauth_url: str | None = None
        self.req_body: dict[str, str | None] | None = None
        self.api_ver: str | None = None
        self.api_root: str | None = None
        self.request_uri: str | None = None
        self.req_headers: dict[str, Any] = {
            "Content-Type": "application/json",
            "Accept": "application/json",
            "Authorization": None,
            **mp_ua_header(),
        }
        self._loaded: bool = True
        self.aad_token: str | None = None
        self._debug: bool = debug
        self.token_type = "AAD"  # nosec
        self.scopes: list[str] | None = None
        self.msal_auth: MSALDelegatedAuth | None = None

        self.set_driver_property(DriverProps.SUPPORTS_THREADING, value=True)
        self.set_driver_property(
            DriverProps.MAX_PARALLEL,
            value=max_threads,
        )
        logger.debug("OData driver initialized successfully")

    @abc.abstractmethod
    def query(
        self: Self,
        query: str,
        query_source: QuerySource | None = None,
        **kwargs,
    ) -> pd.DataFrame | str | None:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Returns
        -------
        pd.DataFrame | str | None
            A DataFrame (if successful) or
            the underlying provider result if an error.

        """

    def connect(
        self: Self,
        connection_str: str | None = None,
        *,
        delegated_auth: bool = False,
        instance: str | None = None,
        **kwargs,
    ) -> None:
        """
        Connect to oauth data source.

        Parameters
        ----------
        connection_str: Optional[str], optional
            Connect to a data source
        instance : Optional[str], optional
            Optional name of configuration instance - this
            is added as a prefix to the driver configuration key name
            when searching for configuration in the msticpyconfig.yaml
        delegated_auth: bool
            Boolean to indicate if delegated auth should be used.

        Notes
        -----
        Connection string fields:
        tenant_id
        client_id
        client_secret
        apiRoot
        apiVersion

        """
        logger.info("Connecting to OData source")
        logger.debug("Delegated auth: %s, Instance: %s", delegated_auth, instance)

        cs_dict: dict[str, Any] = {}
        if connection_str:
            logger.debug("Using provided connection string")
            self.current_connection: str = connection_str
            cs_dict = self._parse_connection_str(connection_str)
        else:
            logger.debug("Loading configuration from settings")
            cs_dict = _get_driver_settings(
                self.CONFIG_NAME,
                self._ALT_CONFIG_NAMES,
                instance,
            )
            # let user override config settings with function kwargs
            cs_dict.update(kwargs)

        logger.debug("Configuration keys present: %s", list(cs_dict.keys()))

        missing_settings: list[str] = [
            setting for setting in ("tenant_id", "client_id") if setting not in cs_dict
        ]
        auth_present: bool = (
            "username" in cs_dict
            or "client_secret" in cs_dict
            or "certificate" in cs_dict
        )
        if missing_settings:
            logger.error("Missing required connection parameters: %s", missing_settings)
            err_msg: str = (
                "You must supply the following required connection parameter(s) "
                "to the connect function or add them to your msticpyconfig.yaml. "
                ", ".join(f"'{param}'" for param in missing_settings)
            )
            raise MsticpyUserConfigError(
                err_msg,
                title="Missing connection parameters.",
                help_uri=("Connecting to OData sources.", _HELP_URI),
            )
        if not auth_present:
            logger.error("No authentication method provided")
            err_msg = (
                "You must supply either a client_secret, or username with which to "
                "to the connect function or add them to your msticpyconfig.yaml."
            )
            raise MsticpyUserConfigError(
                err_msg,
                title="Missing connection parameters.",
                help_uri=("Connecting to OData sources.", _HELP_URI),
            )

        # Default to using delegated auth if username is present
        if "username" in cs_dict:
            logger.info("Username detected, enabling delegated authentication")
            delegated_auth = True

        if delegated_auth:
            logger.info("Using delegated authentication flow")
            self._get_token_delegate_auth(kwargs, cs_dict)
        elif "certificate" in cs_dict:
            logger.info("Using certificate-based authentication flow")
            self._get_token_certificate_auth(cs_dict)
        else:
            logger.info("Using standard client secret authentication flow")
            self._get_token_standard_auth(kwargs, cs_dict)

        self.req_headers["Authorization"] = f"Bearer {self.aad_token}"
        self.api_root = cs_dict.get("apiRoot", self.api_root)
        if not self.api_root:
            logger.error("API root not configured")
            err_msg = f"Sub class {self.__class__.__name__} did not set self.api_root"
            raise ValueError(err_msg)
        api_ver: str | None = cs_dict.get("apiVersion", self.api_ver)
        # Normalize join of api_root and api_ver (api_ver should normally be set)
        if api_ver:
            self.request_uri = f"{self.api_root.rstrip('/')}/{api_ver.strip('/')}"
        else:
            self.request_uri = self.api_root.rstrip("/")

        logger.info("Request URI configured: %s", self.request_uri)
        logger.info("Connected.")
        self._connected = True

    def _get_token_certificate_auth(self: Self, cs_dict: dict[str, Any]) -> None:
        logger.debug("Starting certificate authentication")
        _check_config(cs_dict, "certificate", "application authentication")
        client_id: str = cs_dict["client_id"]
        certificate: Path = Path(cs_dict["certificate"])
        private_key: Path = Path(cs_dict["private_key"])

        logger.debug("Loading certificate from: %s", certificate)
        cert_data: Certificate = load_der_x509_certificate(certificate.read_bytes())

        client_credential: dict[str, Any] = {
            "private_key": private_key.read_text(encoding="utf-8"),
            "thumbprint": cert_data.fingerprint(hashes.SHA1()).hex(),  # nosec
            "public_certificate": cert_data.public_bytes(encoding=Encoding.PEM).decode(
                "utf-8",
            ),
        }
        if "private_key_secret" in cs_dict:
            logger.debug("Private key passphrase provided")
            client_credential["passphrase"] = cs_dict["private_key_secret"]
        if not (self.oauth_url and self.api_root):
            logger.error("OAuth URL or API root not configured")
            err_msg: str = f"Missing OAuth {self.oauth_url} or api_root {self.api_root}"
            raise MsticpyConnectionError(err_msg)
        authority: str = self.oauth_url.format(tenantId=cs_dict["tenant_id"])
        if authority.startswith("https://login"):
            auth_url: urllib.parse.ParseResult = urllib.parse.urlparse(authority)
            authority = f"{auth_url.scheme}://{auth_url.netloc}/{{tenantId}}".format(
                tenantId=cs_dict["tenant_id"],
            )
        logger.debug("Authority URL: %s", authority)

        app: ConfidentialClientApplication = ConfidentialClientApplication(
            client_id=client_id,
            client_credential=client_credential,
            authority=authority,
        )
        # Prefer precomputed scopes (OAuth v2). If not present, derive from api_root.
        scopes: list[str]
        if self.scopes:
            scopes = self.scopes
        else:
            base_root = (self.api_root or "").rstrip("/")
            scopes = [f"{base_root}/.default"]
        auth_result: dict[str, Any] | None = app.acquire_token_for_client(scopes=scopes)
        if not auth_result or "access_token" not in auth_result:
            err_msg = "Could not obtain access token"
            raise MsticpyConnectionError(err_msg)
        self.aad_token = auth_result.get("access_token", None)
        logger.info("Successfully obtained access token via certificate auth")

    def _get_token_standard_auth(
        self: Self,
        kwargs: dict[str, Any],
        cs_dict: dict[str, Any],
    ) -> None:
        logger.debug("Starting standard client secret authentication")
        _check_config(cs_dict, "client_secret", "application authentication")
        # self.oauth_url and self.req_body are correctly set in concrete
        # instances __init__
        if not (self.oauth_url and self.req_body):
            logger.error("OAuth URL or request body template not configured")
            err_msg: str = f"Missing OAuth {self.oauth_url} or req_body {self.req_body}"
            raise MsticpyConnectionError(err_msg)
        req_url: str = self.oauth_url.format(tenantId=cs_dict["tenant_id"])
        logger.debug("Token endpoint: %s", req_url)

        req_body = dict(self.req_body)
        req_body["client_id"] = cs_dict["client_id"]
        req_body["client_secret"] = cs_dict["client_secret"]

        # If this is an OAuth v2 flow (no 'resource' but we have scopes) add scope param
        # Azure AD expects space-delimited scopes; for client credentials this
        # is typically one: <resource>/.default
        if "resource" not in req_body and self.scopes and "scope" not in req_body:
            # Avoid adding empty/duplicate values
            scope_list = {scope for scope in self.scopes if scope}
            if scope_list:
                req_body["scope"] = " ".join(scope_list)
                logger.debug("Added scopes to request: %s", req_body["scope"])

        # Authenticate and obtain AAD Token for future calls
        logger.info("Requesting access token from OAuth endpoint")
        data: bytes = urllib.parse.urlencode(req_body).encode("utf-8")
        auth_response: httpx.Response = httpx.post(
            url=req_url,
            content=data,
            timeout=self.get_http_timeout(**kwargs),
            headers=mp_ua_header(),
        )
        json_response: dict[str, Any] = auth_response.json()
        self.aad_token = json_response.get("access_token")
        if not self.aad_token:
            logger.error(
                "Token acquisition failed: %s",
                json_response.get("error_description", "Unknown error"),
            )
            err_msg = (
                f"Could not obtain access token - {json_response['error_description']}"
            )
            raise MsticpyConnectionError(err_msg)
        logger.info("Successfully obtained access token via client secret")

    def _get_token_delegate_auth(
        self: Self,
        kwargs: dict[str, Any],
        cs_dict: dict[str, Any],
    ) -> None:
        logger.debug("Starting delegated authentication")
        _check_config(cs_dict, "username", "delegated authentication")
        if not (self.oauth_url and self.scopes):
            logger.error("OAuth URL or scopes not configured")
            err_msg: str = f"Missing OAuth {self.oauth_url} or scopes {self.scopes}"
            raise MsticpyConnectionError(err_msg)
        authority: str = self.oauth_url.format(tenantId=cs_dict["tenant_id"])
        if authority.startswith("https://login"):
            auth_url: urllib.parse.ParseResult = urllib.parse.urlparse(authority)
            authority = f"{auth_url.scheme}://{auth_url.netloc}/{{tenantId}}".format(
                tenantId=cs_dict["tenant_id"],
            )
        logger.debug("Authority URL: %s", authority)
        logger.debug("Scopes: %s", self.scopes)
        logger.info(
            "Initializing MSAL delegated auth for user: %s", cs_dict["username"]
        )

        self.msal_auth = MSALDelegatedAuth(
            client_id=cs_dict["client_id"],
            authority=authority,
            username=cs_dict["username"],
            scopes=self.scopes,
            auth_type=kwargs.get("auth_type", "device"),
            location=cs_dict.get("location", "token_cache.bin"),
            connect=True,
        )
        self.aad_token = self.msal_auth.token
        self.token_type = "MSAL"  # nosec
        logger.info("Successfully obtained token via delegated auth")

    # pylint: disable=too-many-branches
    def query_with_results(
        self: Self,
        query: str,
        **kwargs,
    ) -> tuple[pd.DataFrame | None, dict[str, Any]]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Tuple[pd.DataFrame, results.ResultSet]
            A DataFrame (if successful) and
            Kql ResultSet.

        """
        logger.debug("Executing query_with_results")
        if not self.connected:
            logger.info("Not connected, attempting to connect")
            self.connect(self.current_connection)
        if not self.connected:
            logger.error("Failed to establish connection")
            err_msg: str = "Source is not connected. Please call connect() and retry."
            raise ConnectionError(err_msg)

        if self._debug:
            logger.debug("Query: %s", query)

        # Build request based on whether endpoint requires data to be passed in
        # request body in or URL
        if kwargs["body"] is True:
            logger.debug("Using POST request with query in body")
            req_url: str = f"{self.request_uri}{kwargs['api_end']}"
            req_url = urllib.parse.quote(req_url, safe="%/:=&?~#+!$,;'@()*[]")
            logger.debug("Request URL: %s", req_url)
            response: httpx.Response = httpx.post(
                url=req_url,
                headers=self.req_headers,
                json={"Query": query},
                timeout=self.get_http_timeout(**kwargs),
            )
        else:
            logger.debug("Using GET request with query in URL")
            # self.request_uri set if self.connected
            req_url = f"{self.request_uri}{query}"
            logger.debug("Request URL: %s", req_url)
            response = httpx.get(
                url=req_url,
                headers=self.req_headers,
                timeout=self.get_http_timeout(**kwargs),
            )

        logger.debug("Response status code: %d", response.status_code)
        self._check_response_errors(response)

        json_response: dict[str, Any] | int = response.json()
        if isinstance(json_response, int):
            logger.warning(
                "Query did not complete successfully. Check returned response.",
            )
            return None, {"response": json_response}

        results_key: str = "Results" if "Results" in json_response else "results"
        result: dict[str, Any] = json_response.get(results_key, json_response)

        if not result:
            logger.warning("Query did not return any results.")
            return pd.DataFrame(), json_response

        result_count = len(result) if isinstance(result, list) else 1
        logger.info("Query returned %d result(s)", result_count)
        return pd.json_normalize(result), json_response

    # pylint: enable=too-many-branches

    @staticmethod
    def _check_response_errors(response: httpx.Response) -> None:
        """Check the response for possible errors."""
        if response.is_success:
            logger.debug("Response successful")
            return
        logger.warning("Response error: %s", response.json()["error"]["message"])
        if response.status_code == httpx.codes.UNAUTHORIZED:
            logger.error("Authentication failed - status code 401")
            err_msg: str = (
                "Authentication failed - possible timeout. Please re-connect."
            )
            raise ConnectionRefusedError(err_msg)
        # Raise an exception to handle hitting API limits
        if response.status_code == httpx.codes.TOO_MANY_REQUESTS:
            logger.error("API rate limit exceeded - status code 429")
            err_msg = "You have likely hit the API limit."
            raise ConnectionRefusedError(err_msg)
        response.raise_for_status()

    @staticmethod
    def _parse_connection_str(connection_str: str) -> dict[str, str]:
        """
        Split connection string components into dictionary.

        Parameters
        ----------
        connection_str : str
            Semi-colon delimited connection string

        Returns
        -------
        Dict[str, str]
            dict of key/pair values

        """
        logger.debug("Parsing connection string")
        cs_items: list[str] = connection_str.split(";")
        parsed_dict = {
            prop[0].strip(): prop[1].strip()
            for prop in [item.strip().split("=", 1) for item in cs_items]
            if len(prop) == 2 and prop[0] and prop[1]
        }
        logger.debug("Parsed connection string keys: %s", list(parsed_dict.keys()))
        return parsed_dict

    @staticmethod
    def _prepare_param_dict_from_filter(filterstr: str) -> dict[str, str]:
        """
        Parse filter string into dictionary.

        Parameters
        ----------
        filterstr : str
            OData filter string

        """
        logger.debug("Parsing filter string: %s", filterstr)
        get_params: dict[str, Any] = {}
        for filter_param in re.split(r"[\?\&]+", filterstr):
            if filter_param:
                attr: str = filter_param.split("=")[0]
                val: str = filter_param.split("=")[1]
                get_params[attr] = val
        logger.debug("Parsed %d filter parameter(s)", len(get_params))
        return get_params


_CONFIG_NAME_MAP: dict[str, tuple[str, ...]] = {
    "tenant_id": ("tenantid", "tenant_id"),
    "client_id": ("clientid", "client_id"),
    "client_secret": ("clientsecret", "client_secret"),
    "username": ("username", "user_name"),
    "private_key": ("privatekey", "private_key"),
    "certificate": ("certificate", "cert"),
    "private_key_secret": ("privatekeysecret", "private_key_secret"),
}


def _map_config_dict_name(config_dict: dict[str, str]) -> dict[str, str]:
    """Map configuration parameter names to expected values."""
    logger.debug("Mapping configuration dictionary names")
    mapped_dict: dict[str, str] = config_dict.copy()
    for provided_name in config_dict:
        for req_name, alternates in _CONFIG_NAME_MAP.items():
            if provided_name.casefold() in alternates:
                mapped_dict[req_name] = config_dict[provided_name]
                logger.debug("Mapped '%s' to '%s'", provided_name, req_name)
                break
    return mapped_dict


def _get_driver_settings(
    config_name: str,
    alt_names: Iterable[str],
    instance: str | None = None,
) -> dict[str, str]:
    """Try to retrieve config settings for OAuth drivers."""
    logger.debug(
        "Getting driver settings for: %s (instance: %s)", config_name, instance
    )
    config_key: str = (
        f"{config_name}-{instance}"
        if instance and instance != "Default"
        else config_name
    )
    drv_config: ProviderSettings | None = get_provider_settings("DataProviders").get(
        config_key,
    )

    app_config: dict[str, str] = {}
    if drv_config:
        logger.info("Found configuration for key: %s", config_key)
        app_config = dict(drv_config.args)
    else:
        logger.debug("Config not found for %s, trying alternative names", config_key)
        # Otherwise fall back on legacy settings location
        for alt_name in alt_names:
            alt_key: str = f"{alt_name}-{instance}" if instance else alt_name
            logger.debug("Trying alternative config key: %s", alt_key)
            app_config = get_config(f"{alt_key}.Args", {})
            if app_config:
                logger.info("Found configuration for alternative key: %s", alt_key)
                break

    if not app_config:
        logger.warning("No configuration found for %s or any alternatives", config_name)
        return {}
    # map names to allow for different spellings
    return _map_config_dict_name(app_config)


def _check_config(cs_config: dict, item_name: str, scope: str) -> None:
    """Check if an iteam is present in a config."""
    logger.debug("Checking for required config item: %s (scope: %s)", item_name, scope)
    if item_name not in cs_config:
        logger.error("Required config item '%s' not found for %s", item_name, scope)
        err_msg: str = (
            f"To use {scope}, you must define {item_name}"
            "or add them to your msticpyconfig.yaml."
        )
        raise MsticpyUserConfigError(
            err_msg,
            title="Missing connection parameters.",
            help_uri=("Connecting to OData sources.", _HELP_URI),
        )
    logger.debug("Required config item '%s' found", item_name)
