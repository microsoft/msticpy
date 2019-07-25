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
from json import JSONDecodeError
import traceback
from typing import Any, Dict, List, Tuple

import attr
from attr import Factory
import requests

from ..._version import VERSION
from ...nbtools.utility import export
from ..iocextract import IoCType
from .ti_provider_base import LookupResult, TIProvider, preprocess_observable

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

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        self._supported_types = {
            IoCType.parse(ioc_type.split("-")[0]) for ioc_type in self._IOC_QUERIES
        }
        if IoCType.unknown in self._supported_types:
            self._supported_types.remove(IoCType.unknown)

        self._requests_session = requests.Session()
        self._request_params = {}
        if "ApiID" in kwargs:
            self._request_params["API_ID"] = kwargs.pop("ApiID")
        if "AuthKey" in kwargs:
            self._request_params["API_KEY"] = kwargs.pop("AuthKey")

    @lru_cache(maxsize=256)
    def lookup_ioc(
        self, ioc: str, ioc_type: str = None, query_type: str = None
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
        err_result = LookupResult(
            ioc=ioc,
            ioc_type=ioc_type,
            query_subtype=query_type,
            result=False,
            details="",
            raw_result=None,
            reference=None,
        )

        if not ioc_type:
            ioc_type = self.resolve_ioc_type(ioc)
        if not self.is_supported_type(ioc_type):
            err_result.details = f"IoC type {ioc_type} not supported."
            return err_result

        clean_ioc = preprocess_observable(ioc, ioc_type)
        if clean_ioc.status != "ok":
            err_result.details = clean_ioc.status
            return err_result

        try:
            verb, req_params = self._substitute_parms(
                clean_ioc.observable, ioc_type, query_type
            )
            if verb == "GET":
                response = self._requests_session.get(**req_params)
            else:
                raise NotImplementedError(f"Unsupported verb {verb}")
            result = LookupResult(ioc=ioc, ioc_type=ioc_type, query_subtype=query_type)
            result.raw_result = response.json()
            result.status = response.status_code
            result.result, result.details = self.parse_results(result)
            result.reference = req_params["url"]
            return result
        except (
            LookupError,
            JSONDecodeError,
            NotImplementedError,
            ConnectionError,
        ) as err:
            url = req_params.get("url", None) if req_params else None
            err_result.details = err.args
            err_result.raw_result = (
                type(err) + "\n" + str(err) + "\n" + traceback.format_exc()
            )
            err_result.reference = url
            return err_result

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

    @classmethod
    def usage(cls):
        """Print usage of provider."""
        print(f"{cls.__doc__} Supported query types:")
        for ioc_key in sorted(cls._IOC_QUERIES):
            ioc_key_elems = ioc_key.split("-", maxsplit=1)
            if len(ioc_key_elems) == 1:
                print(f"\tioc_type={ioc_key_elems[0]}")
            if len(ioc_key_elems) == 2:
                print(
                    f"\tioc_type={ioc_key_elems[0]}, ioc_query_type={ioc_key_elems[1]}"
                )

    @abc.abstractmethod
    def parse_results(self, response: LookupResult) -> Tuple[bool, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, Any]
            bool = positive or negative hit
            Object with match details

        """
