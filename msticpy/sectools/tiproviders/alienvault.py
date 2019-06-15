# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
AlienVault OTX Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from typing import List, Mapping, Tuple, Union, Iterable, Dict, Any

from functools import lru_cache
# from ipaddress import IPv4Address, ip_address
import warnings

# from socket import gaierror

import pandas as pd
import attr
from attr import Factory
import requests
from requests.exceptions import HTTPError

# from urllib3.exceptions import LocationParseError
# from urllib3.util import parse_url

from .ti_provider import TIProvider, SanitizedObservable, preprocess_observable

from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@attr.s(auto_attribs=True)
class IoCLookupParams():
    """IoC HTTP Lookup Params definition."""

    path: str = ''
    verb: str = 'GET'
    full_url: bool = False
    headers: Dict[str, str] = Factory(dict)
    params: Dict[str, str] = Factory(dict)
    data: Dict[str, str] = Factory(dict)
    auth_type: str = ''
    auth_str: List[str] = Factory(list)


@export
class OTX(TIProvider):
    """AlientVault OTX Lookup."""

    _BASE_URL = "https://otx.alienvault.com"

    _file_hash_params = IoCLookupParams(
        path="/api/v1/indicators/file/{observable}/general",
        headers={"X-OTX-API-KEY": "{API_KEY}"},
    )

    _IOC_QUERIES = {
        "ipv4": IoCLookupParams(
            path="/api/v1/indicators/IPv4/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "ipv6": IoCLookupParams(
            path="/api/v1/indicators/IPv6/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "dns": IoCLookupParams(
            path="/api/v1/indicators/domain/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "hostname": IoCLookupParams(
            path="/api/v1/indicators/hostname/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
        "md5_hash": _file_hash_params,
        "sha1_hash": _file_hash_params,
        "sha256_hash": _file_hash_params,
        "url": IoCLookupParams(
            path="/api/v1/indicators/url/{observable}/general",
            headers={"X-OTX-API-KEY": "{API_KEY}"},
        ),
    }

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        self._requests_session = None
        self._request_params = {}
        if "api_key" in kwargs:
            self._request_params["API_KEY"] = kwargs.pop("api_key")
        if "api_pwd" in kwargs:
            self._request_params["API_PWD"] = kwargs.pop("api_pwd")

    @lru_cache(maxsize=64)
    def lookup_ioc(self, ioc: str, ioc_type: str = None) -> Tuple[bool, str, str]:
        """
        Lookup a single IoC observable.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IocType, by default None

        Returns
        -------
        Tuple[bool, str, str]
            The lookup result:
            Positive/Negative, Details, Raw Response

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        """
        if not ioc_type:
            ioc_type = self.resolve_ioc_type(ioc)
        if not self.is_supported_type(ioc_type):
            return False, f"IoC type {ioc_type} not supported."

        clean_ioc = preprocess_observable(ioc, ioc_type)
        if clean_ioc.status != "ok":
            return False, None, clean_ioc.status

        verb, req_params = self._substitute_parms(clean_ioc.observable, ioc_type)
        try:
            if verb == "GET":
                response = requests.get(**req_params)
            else:
                raise NotImplementedError(f"Unsupported verb {verb}")
            results = response.json()
            verdict = self.get_result(results)
            desc = self.get_result_details(results)
            return verdict, desc, results
        except HTTPError as http_err:
            return False, None, str(http_err)

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
        req_dict["url"] = (self._BASE_URL + src.path.format(**req_params)
                           if not src.full_url
                           else src.path.format(observable=ioc))
        if src.headers:
            headers = {key: val.format(**req_params)
                       for key, val in src.headers.items()}
            req_dict["headers"] = headers
        if src.params:
            q_params = {key: val.format(**req_params)
                        for key, val in src.params.items()}
            req_dict["params"] = q_params
        if src.data:
            q_data = {key: val.format(**req_params)
                      for key, val in src.data.items()}
            req_dict["data"] = q_data
        if src.auth_type and src.auth_str:
            auth_strs = tuple([p.format(**req_params) for p in src.auth_str])
            if src.auth_type == "HTTPBasic":
                req_dict["auth"] = auth_strs
            else:
                raise NotImplementedError(f"Unknown auth type {src.auth_type}")
        return src.verb, req_dict

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame,Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Mapping (e.g. a dict) of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """


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
        return 'pulse_info' in response.keys()

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
        if 'pulse_info' in response.keys():
            pulses = response['pulse_info'].get('pulses', [])
            return {
                "pulse_count": len(pulses),
                "tags": pulses.get("tags", []),
                "reference": pulses.get("references", []),
            }

        return {}