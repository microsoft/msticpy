# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
HTTP TI Provider base.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
import abc
from functools import lru_cache
from http import client
from json import JSONDecodeError
import traceback
from typing import Any, Dict, List, Tuple

import attr
from attr import Factory
import requests

from ..._version import VERSION
from ...nbtools.utility import export
from .ti_provider_base import LookupResult, TIProvider, TISeverity, TILookupStatus

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class IoCLookupParams:
    """IoC HTTP Lookup Params definition."""

    path: str = ""
    verb: str = "GET"
    full_url: bool = False
    headers: Dict[str, str] = Factory(dict)
    params: Dict[str, str] = Factory(dict)
    data: Dict[str, str] = Factory(dict)
    auth_type: str = ""
    auth_str: List[str] = Factory(list)
    sub_type: str = ""


@export
class HttpProvider(TIProvider):
    """HTTP TI provider base class."""

    _BASE_URL = ""

    _IOC_QUERIES: Dict[str, IoCLookupParams] = {}

    _REQUIRED_PARAMS: List[str] = []

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        self._requests_session = requests.Session()
        self._request_params = {}
        if "ApiID" in kwargs:
            self._request_params["API_ID"] = kwargs.pop("ApiID")
        if "AuthKey" in kwargs:
            self._request_params["API_KEY"] = kwargs.pop("AuthKey")

        for req_param in self._REQUIRED_PARAMS:
            if req_param not in self._request_params:
                raise ValueError(
                    f"{req_param} value was not found for {self.__class__.__name__}"
                )

    # pylint: disable=too-many-branches, duplicate-code
    @lru_cache(maxsize=256)
    def lookup_ioc(  # type: ignore
        self, ioc: str, ioc_type: str = None, query_type: str = None, **kwargs
    ) -> LookupResult:
        """
        Lookup a single IoC observable.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IocType, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        LookupResult
            The lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of IoC

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        Notes
        -----
        Note: this method uses memoization (lru_cache) to cache results
        for a particular observable to try avoid repeated network calls for
        the same item.

        """
        result = self._check_ioc_type(
            ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
        )

        result.provider = kwargs.get("provider_name", self.__class__.__name__)
        if result.status:
            return result

        try:
            verb, req_params = self._substitute_parms(
                result.ioc, result.ioc_type, query_type
            )
            if verb == "GET":
                response = self._requests_session.get(**req_params)
            else:
                raise NotImplementedError(f"Unsupported verb {verb}")
            result.status = response.status_code
            result.reference = req_params["url"]
            if result.status == 200:
                result.raw_result = response.json()
                result.result, severity, result.details = self.parse_results(result)
                result.set_severity(severity)
                result.status = TILookupStatus.ok.value
            else:
                result.raw_result = str(response)
                result.result = False
                result.details = self._response_message(result.status)
            return result
        except (  # pylint: disable=duplicate-code
            LookupError,
            JSONDecodeError,
            NotImplementedError,
            ConnectionError,
        ) as err:
            self._err_to_results(result, err)
            if not isinstance(err, LookupError):
                url = req_params.get("url", None) if req_params else None
                result.reference = url
            return result

    # pylint: enable=duplicate-code
    # pylint: disable=too-many-branches
    def _substitute_parms(
        self, ioc: str, ioc_type: str, query_type: str = None
    ) -> Tuple[str, Dict[str, Any]]:
        """
        Create requests parameters collection.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IocType, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        Tuple[str, Dict[str, Any]]
            HTTP method, dictionary of parameter keys/values

        """
        req_params = {"observable": ioc}
        req_params.update(self._request_params)
        if query_type:
            ioc_key = ioc_type + "-" + query_type
        else:
            ioc_key = ioc_type

        src = self._IOC_QUERIES.get(ioc_key, None)
        if not src:
            raise LookupError(f"Provider does not support IoC type {ioc_key}.")

        # create a parameter dictionary to pass to requests
        req_dict: Dict[str, Any] = {}
        # substitute any parameter value from our req_params dict
        req_dict["url"] = (
            self._BASE_URL + src.path.format(**req_params)
            if not src.full_url
            else src.path.format(observable=ioc)
        )
        if src.headers:
            headers: Dict[str, Any] = {
                key: val.format(**req_params) for key, val in src.headers.items()
            }
            req_dict["headers"] = headers
        if src.params:
            q_params: Dict[str, Any] = {
                key: val.format(**req_params) for key, val in src.params.items()
            }
            req_dict["params"] = q_params
        if src.data:
            q_data: Dict[str, Any] = {
                key: val.format(**req_params) for key, val in src.data.items()
            }
            req_dict["data"] = q_data
        if src.auth_type and src.auth_str:
            auth_strs: Tuple = tuple([p.format(**req_params) for p in src.auth_str])
            if src.auth_type == "HTTPBasic":
                req_dict["auth"] = auth_strs
            else:
                raise NotImplementedError(f"Unknown auth type {src.auth_type}")
        return src.verb, req_dict

    @abc.abstractmethod
    def parse_results(self, response: LookupResult) -> Tuple[bool, TISeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, TISeverity, Any]
            bool = positive or negative hit
            TISeverity = enumeration of severity
            Object with match details

        """

    @staticmethod
    def _failed_response(response: LookupResult) -> bool:
        """
        Return True if negative response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        bool
            True if the response indicated failure.

        """
        return (
            response.status != 200
            or not response.raw_result
            or not isinstance(response.raw_result, dict)
        )

    @staticmethod
    def _err_to_results(result: LookupResult, err: Exception):
        result.details = err.args
        result.raw_result = (
            type(err).__name__ + "\n" + str(err) + "\n" + traceback.format_exc()
        )

    @staticmethod
    def _response_message(status_code):
        if status_code == 404:
            return "Not found."
        if status_code == 401:
            return "Authorization failed. Check account and key details."
        if status_code == 403:
            return "Request forbidden. Allowed query rate may have been exceeded."
        return client.responses.get(status_code, "Unknown HTTP status code.")
