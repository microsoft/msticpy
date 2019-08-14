# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Kql TI Provider base.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
import abc
from collections import defaultdict
from functools import lru_cache
from typing import Any, Dict, Tuple, Union, Iterable

import attr
import pandas as pd

from ..._version import VERSION
from ...nbtools.utility import export
from ...nbtools.wsconfig import WorkspaceConfig
from ...data import QueryProvider
from ..iocextract import IoCType
from .ti_provider_base import LookupResult, TIProvider, generate_items

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class KqlTIProvider(TIProvider):
    """HTTP TI provider base class."""

    _IOC_QUERIES: Dict[str, tuple] = {
        "ipv4": ("query_name", {"ioc": "observables"})
    }

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        self._supported_types = {
            IoCType.parse(ioc_type.split("-")[0]) for ioc_type in self._IOC_QUERIES
        }
        if IoCType.unknown in self._supported_types:
            self._supported_types.remove(IoCType.unknown)

        if "query_provider" in kwargs:
            self._query_provider = kwargs.pop("query_provider")
        else:
            ws_config = WorkspaceConfig("foo")  # TODO
            if "workspace_id" in kwargs:
                workspace_id = kwargs.pop("query_provider")
            else:
                workspace_id = ws_config["workspace_id"]
            if "tenant_id" in kwargs:
                tenant_id = kwargs.pop("tenant_id")
            else:
                tenant_id = ws_config["tenant_id"]
            connect_str = f"la://{tenant_id}{workspace_id}"  # TODO
            self._query_provider = QueryProvider("LogAnalytics")
            self._query_provider.connect(connect_str)

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
        result = self._check_ioc_type(
            ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
        )

        if result.status:
            return result

        # TODO refactor to find_query function
        if query_type:
            ioc_key = ioc_type + "-" + query_type
        else:
            ioc_key = ioc_type

        query_path = self._IOC_QUERIES.get(ioc_key, None)
        if not query_path:
            raise LookupError(f"Provider does not support IoC type {ioc_key}.")

        query_obj = self._query_provider
        for path_elem in query_path.split("."):
            query_obj = getattr(query_obj, path_elem, None)
            if not query_obj:
                break

        # TODO check query_obj
        data_result = query_obj(observables=ioc)
        if data_result.empty:
            result.details = "0 rows returned."
            result.status = -1
            return result

        result = LookupResult(ioc=ioc, ioc_type=ioc_type, query_subtype=query_type)
        result.raw_result = data_result
        result.result, result.details = self.parse_results(result)
        result.reference = query_path
        return result

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        ioc_groups = defaultdict(set)
        results = []
        for ioc, ioc_type in generate_items(data, obs_col, ioc_type_col):
            result = self._check_ioc_type(
                ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
            )

            if not result.status:
                ioc_groups[result.ioc_type].append(result.ioc)

        for ioc_type, obs_set in ioc_groups.items():
            item_result = self.lookup_ioc(
                ioc=observable, ioc_type=ioc_type, query_type=query_type
            )
            results.append(pd.Series(attr.asdict(item_result)))

        return (
            pd.DataFrame(data=results)
            .rename(columns=_DF_COLUMNS_MAP)
            .drop(columns=["status"])
        )

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
