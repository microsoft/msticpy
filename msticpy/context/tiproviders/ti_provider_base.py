# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for TILookup classes.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""

from abc import abstractmethod
from typing import Any, Dict, Iterable, Tuple, Union

import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from ..provider_base import PivotProvider, Provider
from .result_severity import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class TIProvider(Provider):
    """Abstract base class for Threat Intel providers."""

    _QUERIES: Dict[str, Any] = {}

    def _check_item_type(
        self, item: str, item_type: str = None, query_subtype: str = None
    ) -> Dict:
        """
        Check Item Type and cleans up item.

        Parameters
        ----------
        item : str
            item
        item_type : str, optional
            item type, by default None
        query_subtype : str, optional
            Query sub-type, if any, by default None

        Returns
        -------
        Dict
            Dict result with resolved type and pre-processed
            item.
            Status is none-zero on failure.

        """
        return self._check_ioc_type(
            item,
            item_type,
            query_subtype,
        )

    def _check_ioc_type(
        self, ioc: str, ioc_type: str = None, query_subtype: str = None
    ) -> Dict:
        """
        Check Ioc Type and cleans up ioc.

        Parameters
        ----------
        ioc : str
            IoC
        ioc_type : str, optional
            IoC type, by default None
        query_subtype : str, optional
            Query sub-type, if any, by default None

        Returns
        -------
        Dict
            Dict result with resolved type and pre-processed
            Ioc.
            Status is none-zero on failure.

        """
        result = super()._check_item_type(
            item=ioc, item_type=ioc_type, query_subtype=query_subtype
        )
        result["Ioc"] = result.pop("Item")
        result["IocType"] = result.pop("ItemType")
        result["SafeIoc"] = result.pop("SanitizedValue")
        result["Severity"] = ResultSeverity.information.name
        return result

    def lookup_item(
        self, item: str, item_type: str = None, query_type: str = None, **kwargs
    ) -> pd.DataFrame:
        """
        Lookup a single item.

        Parameters
        ----------
        item : str
            Item value to lookup
        item_type : str, optional
            The Type of the value to lookup, by default None (type will be inferred)
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
        for a particular observable to try avoid repeated network calls for
        the same item.

        """
        return self.lookup_ioc(
            ioc=item,
            ioc_type=item_type,
            query_type=query_type,
            **kwargs,
        )

    @abstractmethod
    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        Tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """

    @abstractmethod
    def lookup_ioc(
        self,
        ioc: str,
        ioc_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup a single IoC observable.

        Parameters
        ----------
        ioc : str
            IoC Observable value
        ioc_type : str, optional
            IoC Type, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `ioc_col` parameter)
            2. Dict of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        ioc_col : str, optional
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
        return self.lookup_items(
            data,
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            query_type=query_type,
            **kwargs,
        )

    async def lookup_iocs_async(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """Call base async wrapper."""
        return await self._lookup_items_async_wrapper(
            data,
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            query_type=query_type,
            **kwargs,
        )

    async def _lookup_iocs_async_wrapper(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Async wrapper for providers that do not implement lookup_iocs_async.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        ioc_col : str, optional
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
        return await self._lookup_items_async_wrapper(
            data,
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            query_type=query_type,
            **kwargs,
        )

    @property
    def ioc_query_defs(self) -> Dict[str, Any]:
        """
        Return current dictionary of IoC query/request definitions.

        Returns
        -------
        Dict[str, Any]
            IoC query/request definitions keyed by IoCType

        """
        return self._QUERIES

    @classmethod
    def usage(cls):
        """Print usage of provider."""
        print(f"{cls.__doc__} Supported query types:")
        for ioc_key in sorted(cls._QUERIES):
            ioc_key_elems = ioc_key.split("-", maxsplit=1)
            if len(ioc_key_elems) == 1:
                print(f"\tioc_type={ioc_key_elems[0]}")
            if len(ioc_key_elems) == 2:
                print(f"\tioc_type={ioc_key_elems[0]}, query_type={ioc_key_elems[1]}")

    @staticmethod
    def resolve_ioc_type(observable: str) -> str:
        """
        Return IoCType determined by IoCExtract.

        Parameters
        ----------
        observable : str
            IoC observable string

        Returns
        -------
        str
            IoC Type (or unknown if type could not be determined)

        """
        return TIProvider.resolve_item_type(observable)


class TIPivotProvider(PivotProvider):
    """A class which provides TI pivot functions and a means of registering them."""

    @abstractmethod
    def register_pivots(
        self,
        pivot_reg: "PivotRegistration",  # type: ignore # noqa: F821
        pivot: "Pivot",  # type: ignore # noqa: F821
    ):
        """
        Register pivot functions for the TI Provider.

        Parameters
        ----------
        pivot_reg : PivotRegistration
            Pivot registration settings.
        pivot : Pivot
            Pivot library instance

        """
