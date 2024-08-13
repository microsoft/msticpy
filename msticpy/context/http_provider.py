# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
HTTP Lookup base class.

Input can be a single item or a pandas DataFrame containing
multiple items. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

import traceback
from abc import abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, ClassVar

import httpx
from typing_extensions import Self

from .._version import VERSION
from ..common.exceptions import MsticpyConfigError
from ..common.pkg_config import get_http_timeout
from ..common.utility import mp_ua_header
from .lookup_result import LookupStatus
from .provider_base import Provider

if TYPE_CHECKING:
    import pandas as pd

__version__ = VERSION
__author__ = "Ian Hellen"


@dataclass
class APILookupParams:
    """HTTP Lookup Params definition."""

    path: str = field(default="")
    verb: str = field(default="GET")
    full_url: bool = field(default=False)
    headers: dict[str, str] = field(default_factory=dict)
    params: dict[str, str | float] = field(default_factory=dict)
    data: dict[str, str] = field(default_factory=dict)
    auth_type: str = field(default="")
    auth_str: list[str] = field(default_factory=list)
    sub_type: str = field(default="")


class HttpProvider(Provider):
    """
    HTTP Generic lookup provider base class.

    For subclasses:

    Define Base URL of the service

    .. code:: python

        _BASE_URL = "https://my.api.org/"

    Define query parameters for different item types (keys)

    ..code:: python

        _QUERIES: dict[str, APILookupParams] = {}

    For example:

    .. code:: python

        _QUERIES = {
        # Community API
        "ipv4": APILookupParams(
            path="/v3/community/{observable}",
            headers={"key": "{AuthKey}"},
        ),
        # Enterprise API Quick Lookup
        "ipv4-quick": APILookupParams(
            ...


    Define list of required __init__ params

    .. code:: python

        _REQUIRED_PARAMS: list[str] = []

    For example:

    .. code:: python

        _REQUIRED_PARAMS = ["AuthKey"]

    In __init__

    Be sure to call

    .. code::

        super().__init__(**kwargs)``

    Supply any additional checkers/pre-processors
    with

    .. code: python

        self._preprocessors.add_check(type, check_func)

    See Also
    --------
    PreProcessor
    HttpTIProvider

    """

    # Base URL of the service
    _BASE_URL: ClassVar[str] = ""

    # Define query parameters for different item types (keys)
    _QUERIES: ClassVar[dict[str, Any]] = {}

    # List of required __init__ params
    _REQUIRED_PARAMS: ClassVar[list[str]] = []

    def __init__(
        self: HttpProvider,
        *,
        timeout: int | None = None,
        ApiID: str | None = None,  # noqa: N803
        AuthKey: str | None = None,  # noqa: N803
        Instance: str | None = None,  # noqa: N803
    ) -> None:
        """Initialize the class."""
        super().__init__()
        self._httpx_client = httpx.Client(timeout=get_http_timeout(timeout=timeout))
        self._request_params: dict[str, Any] = {
            "ApiID": None,
            "AuthKey": None,
            "Instance": None,
        }
        if ApiID:
            self._request_params["ApiID"] = ApiID.strip()
        if AuthKey:
            self._request_params["AuthKey"] = AuthKey.strip()
        if Instance:
            self._request_params["Instance"] = Instance.strip()

        missing_params: list[str] = [
            param
            for param in self._REQUIRED_PARAMS
            if param not in self._request_params
        ]

        missing_params = []

        if missing_params:
            param_list: str = ", ".join(f"'{param}'" for param in missing_params)
            error_msg: str = (
                f"Parameter values missing for Provider '{self.__class__.__name__}'. "
                f"Missing parameters are: {param_list}"
            )
            raise MsticpyConfigError(error_msg)

    @abstractmethod
    def lookup_item(
        self: Self,
        item: str,
        item_type: str | None = None,
        query_type: str | None = None,
    ) -> pd.DataFrame:
        """
        Lookup from an item value.

        Parameters
        ----------
        item : str
            item to lookup
        item_type : str, optional
            The Type of the item to lookup, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item_value
            will be returned.

        Returns
        -------
        pd.DataFrame
            The lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of the item

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        Notes
        -----
        Note: this method uses memoization (lru_cache) to cache results
        for a particular item to try avoid repeated network calls for
        the same item.

        """

    # pylint: enable=duplicate-code
    def _substitute_parms(
        self: Self,
        value: str,
        value_type: str,
        query_type: str | None = None,
    ) -> tuple[str, dict[str, Any]]:
        """
        Create requests parameters collection.

        Parameters
        ----------
        value : str
            The value of the item being queried
        value_type : str, optional
            The value type, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the type
            will be returned.

        Returns
        -------
        Tuple[str, dict[str, Any]]
            HTTP method, dictionary of parameter keys/values

        """
        req_params: dict[str, str] = {"observable": value}
        req_params.update(self._request_params)
        value_key: str = f"{value_type}-{query_type}" if query_type else value_type
        src: APILookupParams | None = self.item_query_defs.get(value_key, None)
        if not src:
            error_msg: str = f"Provider does not support this type {value_key}."
            raise LookupError(error_msg)

        # create a parameter dictionary to pass to requests
        # substitute any parameter value from our req_params dict
        req_dict: dict[str, Any] = {
            "headers": {},
            "url": (
                src.path.format(**req_params)
                if src.full_url
                else (self._BASE_URL + src.path).format(**req_params)
            ),
        }

        if src.headers:
            headers: dict[str, Any] = {
                key: val.format(**req_params) for key, val in src.headers.items()
            }
            req_dict["headers"] = headers
        if "User-Agent" not in req_dict["headers"]:
            req_dict["headers"].update(mp_ua_header())
        if src.params:
            q_params: dict[str, Any] = {
                key: val.format(**req_params) if isinstance(val, str) else val
                for key, val in src.params.items()
            }
            req_dict["params"] = q_params
        if src.data:
            q_data: dict[str, Any] = {
                key: val.format(**req_params) for key, val in src.data.items()
            }
            req_dict["data"] = q_data
        if src.auth_type and src.auth_str:
            auth_strs: tuple = tuple(p.format(**req_params) for p in src.auth_str)
            if src.auth_type == "HTTPBasic":
                req_dict["auth"] = auth_strs
            else:
                error_msg = f"Unknown auth type {src.auth_type}"
                raise NotImplementedError(error_msg)
        return src.verb, req_dict

    @staticmethod
    def _failed_response(response: dict) -> bool:
        """
        Return True if negative response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        bool
            True if the response indicated failure.

        """
        return (
            response["Status"] not in (200, LookupStatus.OK.value)
            or not response["RawResult"]
            or not isinstance(response["RawResult"], dict)
        )

    @staticmethod
    def _err_to_results(result: dict, err: Exception) -> None:
        result["Details"] = err.args
        result["RawResult"] = (
            type(err).__name__ + "\n" + str(err) + "\n" + traceback.format_exc()
        )

    @staticmethod
    def _response_message(status_code: int) -> str:
        if status_code == httpx.codes.NOT_FOUND:
            return "Not found."
        if status_code == httpx.codes.UNAUTHORIZED:
            return "Authorization failed. Check account and key details."
        if status_code == httpx.codes.FORBIDDEN:
            return "Request forbidden. Allowed query rate may have been exceeded."
        return httpx.codes.get_reason_phrase(status_code) or "Unknown HTTP status code."
