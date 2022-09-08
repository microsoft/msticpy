# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Help functions for Synapse pipelines notebooks."""
import os
import re
from pathlib import Path
from typing import Dict, List, Literal, Optional, Union

import httpx
import jwt

from .._version import VERSION
from ..auth.azure_auth import AzureCredEnvNames, az_connect
from ..common import pkg_config as config
from ..common.pkg_config import get_http_timeout
from ..common.provider_settings import get_provider_settings
from ..common.utility import mp_ua_header


class SparkUtilsPlaceHolder:
    """Placeholder for non-spark environments."""

    def __getattr__(self, attrib):
        """Raise not implemented error."""
        raise NotImplementedError(
            "This function can only be used in an Azure Synapse Spark environment."
        )


try:
    from notebookutils import mssparkutils

    _IN_SPARK = True
except ImportError:
    mssparkutils = SparkUtilsPlaceHolder()
    _IN_SPARK = False


__version__ = VERSION
__author__ = "Ian Hellen"

_LINKED_SERVICES_URL = (
    "https://{ws_name}.dev.azuresynapse.net/linkedservices?api-version=2020-12-01"
)

_AZ_NAME_PATTERN = re.compile(r"^https://(?P<name>[^.]+)\.(?P<suffix>.*$)")


def is_in_synapse():
    """Return True if running in Synapse Pipeline."""
    return os.environ.get("MMLSPARK_PLATFORM_INFO") == "synapse"


IdentityType = Literal["managed", "service_principal"]


def init_synapse(
    identity_type: IdentityType = "service_principal",
    storage_svc_name: Optional[str] = None,
    tenant_id: Optional[str] = None,
    cloud: str = "global",
):
    """
    Initialize Synapse Spark notebook pipeline.

    Parameters
    ----------
    identity_type : IdentityType, optional
        The authentication type to be used by MSTICPy, by default "managed".
        Options are "managed", "service_principal"
    storage_svc_name : Optional[str], optional
        Override the default storage linked service name, by default None
    tenant_id : Optional[str], optional
        Override the default tenant_id for the Azure authentication, by default None
    cloud : str
        Azure cloud - default is "global".

    Raises
    ------
    RuntimeError
        No suitable linked storage service found.
        Could not mount configuration data container.
        Could not authenticate to Azure.
        Could not retrieve secret from Key Vault.

    """
    if not is_in_synapse():
        print("Not running in Azure Synapse environment.")
        return
    mp_spark = MPSparkUtils()
    storage_svc = mp_spark.get_storage_service(storage_svc_name)
    if not storage_svc:
        raise RuntimeError("No suitable linked storage service found.")
    print(f"Using linked storage service {storage_svc.name}")

    print("Mounting storage...", end=" ")
    stor_acct_match = re.match(_AZ_NAME_PATTERN, storage_svc.url)
    mount_success = mount_container(
        store_acct_name=stor_acct_match["name"] if stor_acct_match else "",
        container=mp_spark.container,
        mount_path=mp_spark.mount_point,
        linked_service=storage_svc.name,
    )
    if not mount_success:
        raise RuntimeError("Could not mount configuration data container.")
    print(f"Mounted {storage_svc.url}/{mp_spark.container} at {mp_spark.config_path}")

    # Set msticpy settings
    print("Configuring msticpy...", end=" ")
    _configure_mp_settings(mp_spark)
    print("settings,", end=" ")

    # Set up authentication parameters
    if identity_type == "managed":
        _set_mp_azure_settings(auth_method="msi", cloud=cloud)
        _set_msi_client_id(mp_spark=mp_spark, tenant_id=tenant_id)
        print("using managed identity,", end=" ")
    else:
        # Publish SP creds from KeyVault
        _set_mp_azure_settings(auth_method="env", cloud=cloud)
        _set_azure_env_creds(mp_spark=mp_spark, tenant_id=tenant_id)
        print("using service principal,", end=" ")
    print()

    # authenticate with MSI or SP to linked KV
    creds = az_connect()
    if not creds:
        raise RuntimeError("Could not authenticate to Azure.")
    print("Testing authentication...", end=" ")

    # check key retrieval
    if not _check_kv_key_retrieval():
        raise RuntimeError("Could not retrieve secret from Key Vault.")
    print("Key Vault success.")
    print("Synapse initialization successful")


def current_mounts() -> Dict[str, str]:
    """Return dictionary of current Synapse mount points."""
    return {mnt.mountPoint: mnt.source for mnt in mssparkutils.fs.mounts()}


def mount_container(
    store_acct_name: str,
    container: str,
    mount_path: str,
    linked_service: str,
    folder: str = "",
    cloud: str = "global",
) -> bool:
    """
    Mount Azure file container to Synapse file system.

    Parameters
    ----------
    store_acct_name : str
        Storage account
    container : str
        Container name to mount
    mount_path : str
        Local path to mount
    linked_service : str
        Storage linked service name
    folder : str, optional
        Subfolder of container, if any, by default ""
    cloud : str, optional
        Azure Cloud, by default "global"

    Returns
    -------
    bool
        True if mounting successful or if mount point already connected.
        False if mounting failed or mount point connected to a different
        storage location.

    Raises
    ------
    NotImplementedError
        If using a cloud other than Azure Global (public cloud).

    """
    if cloud != "global":
        raise NotImplementedError("Only supports Azure 'global' cloud currently.")

    mount_path = mount_path if mount_path.startswith("/") else f"/{mount_path}"
    existing_mounts = current_mounts()

    storage_root = "dfs.core.windows.net/"
    folder = f"{folder}/" if folder else ""

    storage_url = f"abfss://{container}@{store_acct_name}.{storage_root}{folder}"
    if mount_path in existing_mounts:
        print(f"path '{mount_path}' already mounted to {existing_mounts[mount_path]}")
        return existing_mounts[mount_path] == storage_url

    return mssparkutils.fs.mount(
        storage_url, mount_path, {"linkedService": linked_service}
    )


# pylint: disable=too-few-public-methods
class LinkedService:
    """Azure Synapse Linked Service settings."""

    _TYPE_PROPS = "typeProperties"

    class ServiceTypes:
        """Enumeration of linked service type names."""

        AzureBlobFS = "AzureBlobFS"
        AzureSqlDW = "AzureSqlDW"
        AzureFileStorage = "AzureFileStorage"
        AzureBlobStorage = "AzureBlobStorage"
        AzureKeyVault = "AzureKeyVault"

    def __init__(self, **kwargs):
        """Initialize Linked Service instance."""
        self.res_id: str = kwargs.get("id")
        self.name: str = kwargs.get("name")
        self.entry_type: str = kwargs.get("type")
        self.etag: str = kwargs.get("etag")
        self.properties: Dict[
            str, Union[str, Dict[str, Union[Dict, str]]]
        ] = kwargs.get("properties", {})

    @property
    def svc_type(self) -> str:
        """Return type of linked service."""
        return self.properties.get("type")  # type: ignore

    def __getattr__(self, name: str):
        """Return sub property `name` from properties or typeProperties."""
        if name in self.properties:
            return self.properties[name]
        if name in self.properties.get(self._TYPE_PROPS, {}):
            return self.properties.get(self._TYPE_PROPS, {})[name]  # type: ignore
        raise AttributeError(f"Unknown attribute {name}")

    def __repr__(self):
        """Return string representation of instance."""
        return f"LinkedService(name='{self.name}', type='{self.svc_type}')"

    @property
    def azure_name(self):
        """Return Azure resource name."""
        url = self.properties.get(self._TYPE_PROPS, {}).get("url")
        if not url:
            url = self.properties.get(self._TYPE_PROPS, {}).get("baseUrl")
        if not url:
            return None
        az_name = re.search(_AZ_NAME_PATTERN, url)
        return az_name["name"]


# Note storage linked service is generated by Synapse workspace creation process
# pylint: disable=too-few-public-methods
class SynapseName:
    """Name mapping to default values."""

    storage_account_prefix = (
        "adlsforsentinel"  # + last 7 digit of Sentinel workspace id;
    )
    key_vault_name_prefix = "kvforsentinel"  # + last 7 digit of ws I’d;
    kv_linked_service = "Akvlink"
    sp_client_id_name = "clientid"
    # pylint: disable=line-too-long
    # [SuppressMessage("Microsoft.Security", "CS002:SecretInNextLine", Justification="This is an enum of property names")]
    sp_client_sec_name = "clientsecret"
    container = "sentinelfiles"

    def __init__(self, workspace_id: str):
        """Initialize the Synapse Name class."""
        self._ws_id = workspace_id

    @property
    def storage_account(self) -> str:
        """Return default storage account name."""
        return f"{self.storage_account_prefix}{self._ws_id[-7:]}"

    @property
    def key_vault(self) -> str:
        """Return default Key Vault name."""
        return f"{self.key_vault_name_prefix}{self._ws_id[-7:]}"


class MPSparkUtils:
    """MSTICPy Spark Synapse Utility class."""

    _DEF_MOUNT_POINT = "msticpy"

    def __init__(
        self, mount_point: Optional[str] = None, container: Optional[str] = None
    ):
        """Initialize MPSparkUtils class."""
        self.linked_services = [
            LinkedService(**props)
            for props in _fetch_linked_services(self.workspace_name)
        ]
        self._get_workspace_ids()
        self.mount_point = mount_point or self._DEF_MOUNT_POINT
        self.container = container or SynapseName.container

    @property
    def workspace_name(self) -> str:
        """Return the Synapse workspace name."""
        return mssparkutils.env.getWorkspaceName()

    @property
    def fs_mounts(self) -> Dict[str, str]:
        """Return a dictionary of mount points and targets."""
        return {mnt.mountPoint: mnt.source for mnt in mssparkutils.fs.mounts()}

    @property
    def job_id(self):
        """Return current job ID."""
        return mssparkutils.env.getJobId()

    @property
    def config_path(self):
        """Return mount path for MSTICPy config."""
        return Path(f"/synfs/{self.job_id}/{self.mount_point}")

    def get_service_of_type(self, svc_type: str) -> Optional[LinkedService]:
        """
        Return the first linked service of specific `svc_type`.

        Parameters
        ----------
        svc_type : str
            Service Type (AzureBlobFS, AzureBlobStorage, AzureSqlDW,
            AzureKeyVault)

        Returns
        -------
        Optional[LinkedService]
            LinkedService instance for the `svc_type`.

        """
        try:
            return next(
                iter(
                    lnk_svc
                    for lnk_svc in self.linked_services
                    if lnk_svc.svc_type == svc_type
                )
            )
        except StopIteration:
            return None

    def get_all_services_of_type(self, svc_type) -> List[LinkedService]:
        """
        Return list of Linked services of `svc_type`.

        Parameters
        ----------
        svc_type : str
            Service Type (AzureBlobFS, AzureBlobStorage, AzureSqlDW,
            AzureKeyVault)

        Returns
        -------
        List[LinkedService]
            List of LinkedService instances of type `svc_type`.

        """
        return [
            lnk_svc for lnk_svc in self.linked_services if lnk_svc.svc_type == svc_type
        ]

    def get_ws_default_storage(self) -> Optional[LinkedService]:
        """
        Return default storage linked service.

        Returns
        -------
        Optional[LinkedService]
            Default storage service.

        """
        try:
            return next(
                iter(
                    lnk_svc
                    for lnk_svc in self.linked_services
                    if lnk_svc.svc_type == LinkedService.ServiceTypes.AzureBlobFS
                    and "WorkspaceDefaultStorage" in lnk_svc.name
                )
            )
        except StopIteration:
            return None

    def get_service(self, svc_name: str) -> Optional[LinkedService]:
        """
        Return named linked service.

        Returns
        -------
        Optional[LinkedService]
            Named service service.

        """
        try:
            return next(
                iter(
                    lnk_svc
                    for lnk_svc in self.linked_services
                    if lnk_svc.name == svc_name
                )
            )
        except StopIteration:
            return None

    def get_storage_service(
        self, linked_svc_name: Optional[str] = None
    ) -> LinkedService:
        """Return linked storage service (named) or default storage."""
        storage_svc: Optional[LinkedService] = None
        if linked_svc_name:
            storage_svc = self.get_service(svc_name=linked_svc_name)
        if not storage_svc:
            storage_svc = self.get_ws_default_storage()
            if storage_svc is None:
                storage_svc = self.get_service_of_type(
                    svc_type=LinkedService.ServiceTypes.AzureBlobFS
                )
        if not storage_svc:
            raise TypeError("No linked storage service found.")
        return storage_svc

    def get_kv_secret(self, secret_name: str) -> str:
        """Return secret from linked service."""
        kv_service = self.get_service_of_type(LinkedService.ServiceTypes.AzureKeyVault)
        if not kv_service:
            raise ValueError("No linked Key Vault service found.")
        return mssparkutils.credentials.getSecret(
            kv_service.azure_name, secret_name, kv_service.name
        )

    def _get_workspace_ids(self):
        """Retrieve Synapse Managed Identity details from token."""
        ws_token = mssparkutils.credentials.getToken("Synapse")
        alg = jwt.get_unverified_header(ws_token)["alg"]
        decoded_token = jwt.decode(
            ws_token, algorithms=[alg], options={"verify_signature": False}
        )
        self.application_id = decoded_token["appid"]
        self.tenant_id = decoded_token["tid"]
        self.object_id = decoded_token["oid"]
        self.resource_id = decoded_token.get("xms_mirid")


def _fetch_linked_services(ws_name: str):
    """Fetch list of linked services via Azure Synapse API."""
    token = mssparkutils.credentials.getToken("Synapse")
    req_headers = {"Authorization": f"Bearer {token}", **mp_ua_header()}

    resp = httpx.get(
        _LINKED_SERVICES_URL.format(ws_name=ws_name),
        headers=req_headers,
        timeout=get_http_timeout(),
    )
    return resp.json().get("value")


def _set_azure_env_creds(mp_spark: MPSparkUtils, tenant_id: Optional[str] = None):
    """Publish Service Principal credentials to environment variables."""
    os.environ[AzureCredEnvNames.AZURE_TENANT_ID] = tenant_id or mp_spark.tenant_id
    os.environ[AzureCredEnvNames.AZURE_CLIENT_ID] = mp_spark.get_kv_secret(
        SynapseName.sp_client_id_name
    )
    os.environ[AzureCredEnvNames.AZURE_CLIENT_SECRET] = mp_spark.get_kv_secret(
        SynapseName.sp_client_sec_name
    )


def _set_msi_client_id(mp_spark: MPSparkUtils, tenant_id: Optional[str] = None):
    """Publish Service Principal credentials to environment variables."""
    os.environ[AzureCredEnvNames.AZURE_TENANT_ID] = tenant_id or mp_spark.tenant_id
    os.environ[AzureCredEnvNames.AZURE_CLIENT_ID] = mp_spark.application_id


def _set_mp_azure_settings(auth_method: str, cloud: str = "global"):
    """Configure in-memory settings for MP Azure authn_methods."""
    az_settings = config.settings.get("Azure")
    if not az_settings:
        config.settings["Azure"] = {
            "auth_methods": [auth_method],
            "cloud": cloud,
        }
    else:
        curr_methods = az_settings.get("auth_methods", [])
        if auth_method in curr_methods:
            curr_methods.remove(auth_method)
        az_settings["auth_methods"] = [auth_method, *curr_methods]


def _check_kv_key_retrieval() -> bool:
    """Check that we are able to get a key from Key Vault."""
    ti_settings = get_provider_settings("TIProviders")
    prov_settings = next(
        iter(
            settings
            for settings in ti_settings.values()
            if settings.args and "AuthKey" in settings.args
        )
    )
    value = prov_settings.args.get("AuthKey")
    return value is not None


def _configure_mp_settings(mp_spark: MPSparkUtils):
    """Set msticpyconfig and GeoIPLite dbfolder paths."""
    os.environ["MSTICPYCONFIG"] = str(
        mp_spark.config_path.joinpath("msticpyconfig.yaml")
    )
    os.environ["MSTICPYHOME"] = str(mp_spark.config_path)
    config.refresh_config()

    geolite_settings = (
        config.settings.get("OtherProviders", {}).get("GeoIPLite", {}).get("Args")
    )
    if geolite_settings:
        geolite_settings["DBFolder"] = str(mp_spark.config_path)
