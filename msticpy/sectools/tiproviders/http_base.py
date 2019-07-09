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
from typing import List, Optional, Tuple, Dict, Any

from functools import lru_cache

# from socket import gaierror

import attr
from attr import Factory
import requests
from requests.exceptions import HTTPError

# from urllib3.exceptions import LocationParseError
# from urllib3.util import parse_url

from .ti_provider_base import TIProvider, preprocess_observable
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


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


@export
class HttpProvider(TIProvider):
    """AlientVault OTX Lookup."""

    _BASE_URL = ""

    _IOC_QUERIES: Dict[str, IoCLookupParams] = {}

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        self._requests_session = requests.Session()
        self._request_params = {}
        if "api_key" in kwargs:
            self._request_params["API_KEY"] = kwargs.pop("api_key")
        if "api_pwd" in kwargs:
            self._request_params["API_PWD"] = kwargs.pop("api_pwd")

    @lru_cache(maxsize=64)
    def lookup_ioc(
        self, ioc: str, ioc_type: str = None
    ) -> Tuple[bool, Any, Optional[str]]:
        """
        Lookup a single IoC observable.

        Note: this method uses memoization (lru_cache) to cache results
        for a particular observable to try avoid repeated network calls for
        the same item.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IocType, by default None

        Returns
        -------
        Tuple[bool, Any, Optional[str]
            The lookup result:
            Positive/Negative,
            Lookup Details (or status if failure),
            Raw Response

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        """
        if not ioc_type:
            ioc_type = self.resolve_ioc_type(ioc)
        if not self.is_supported_type(ioc_type):
            return False, f"IoC type {ioc_type} not supported.", None

        clean_ioc = preprocess_observable(ioc, ioc_type)
        if clean_ioc.status != "ok":
            return False, clean_ioc.status, None, None

        verb, req_params = self._substitute_parms(clean_ioc.observable, ioc_type)
        try:
            if verb == "GET":
                response = self._requests_session.get(**req_params)
            else:
                raise NotImplementedError(f"Unsupported verb {verb}")
            results = response.json()
            verdict = self.get_result(results)
            desc = self.get_result_details(results)
            return verdict, desc, results
        except HTTPError as http_err:
            return False, str(http_err), str(http_err)

    def _substitute_parms(self, ioc: str, ioc_type: str) -> Tuple[str, Dict[str, Any]]:
        """
        Create requests parameters collection.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IocType, by default None

        Returns
        -------
        Tuple[str, Dict[str, Any]]
            HTTP method, dictionary of parameter keys/values

        """
        req_params = {"observable": ioc}
        req_params.update(self._request_params)
        src = self._IOC_QUERIES[ioc_type]

        # create a parameter dictionary to pass to requests
        req_dict = {}
        # substitute any parameter value from our req_params dict
        req_dict["url"] = (
            self._BASE_URL + src.path.format(**req_params)
            if not src.full_url
            else src.path.format(observable=ioc)
        )
        if src.headers:
            headers = {
                key: val.format(**req_params) for key, val in src.headers.items()
            }
            req_dict["headers"] = headers
        if src.params:
            q_params = {
                key: val.format(**req_params) for key, val in src.params.items()
            }
            req_dict["params"] = q_params
        if src.data:
            q_data = {key: val.format(**req_params) for key, val in src.data.items()}
            req_dict["data"] = q_data
        if src.auth_type and src.auth_str:
            auth_strs = tuple([p.format(**req_params) for p in src.auth_str])
            if src.auth_type == "HTTPBasic":
                req_dict["auth"] = auth_strs
            else:
                raise NotImplementedError(f"Unknown auth type {src.auth_type}")
        return src.verb, req_dict

    @staticmethod
    def get_result(response: Any) -> bool:
        """
        Return True if the response indicates a hit.

        Parameters
        ----------
        response : Any
            The returned data response

        Returns
        -------
        bool
            True if a positive match

        """
        return False

    @staticmethod
    def get_result_details(response: Any) -> Any:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Any
            The returned data response

        Returns
        -------
        Any
            Object with match details

        """
        return None
