# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for ContextProvider classes.

Input can be a single observable or a pandas DataFrame containing
    multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from abc import abstractmethod
from typing import Tuple, Any, List, Union, Dict, Iterable
from functools import lru_cache

import pandas as pd

from ..provider_base import Provider
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class ContextProvider(Provider):
    """Abstract base class for Context providers."""

    _REQUIRED_PARAMS: List[str] = []

    @lru_cache(maxsize=256)
    def lookup_item(
        self, item: str, item_type: str = None, query_type: str = None, **kwargs
    ) -> pd.DataFrame:
        """
        Lookup from a value.

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
        for a particular observable to try avoid repeated network calls for
        the same item.

        """
        return self.lookup_observables(
            data={item: item_type},
            query_type=query_type,
            **kwargs,
        )

    def lookup_observable(  # type: ignore
        self,
        observable: str,
        obs_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup a single observable.

        Parameters
        ----------
        observable : str
            Observable value to lookup
        obs_type : str, optional
            The Type of the value to lookup, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item_value
            will be returned.

        Returns
        -------
        pd.DataFrame
            The context lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of the item

        """
        return self.lookup_item(
            item=observable,
            item_type=obs_type,
            query_type=query_type,
            **kwargs,
        )

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
        return self._check_observable_type(
            item,
            item_type,
            query_subtype,
        )

    def _check_observable_type(
        self, obs: str, obs_type: str = None, query_subtype: str = None
    ) -> Dict:
        """
        Check Observable Type and cleans up observable.

        Parameters
        ----------
        obs : str
            Observable
        obs_type : str, optional
            Observable type, by default None
        query_subtype : str, optional
            Query sub-type, if any, by default None

        Returns
        -------
        Dict
            Dict result with resolved type and pre-processed
            Observable.
            Status is none-zero on failure.

        """
        result = super()._check_item_type(
            item=obs, item_type=obs_type, query_subtype=query_subtype
        )
        result["Observable"] = result.pop("Item")
        result["ObservableType"] = result.pop("ItemType")
        result["SafeObservable"] = result.pop("SanitizedValue")
        return result

    @abstractmethod
    def parse_results(self, response: Dict) -> Tuple[bool, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """

    def lookup_observables(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup collection of observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observables
            3. Iterable of observables
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        obs_type_col : str, optional
            DataFrame column to use for observables types, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        return self.lookup_items(
            data,
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            **kwargs,
        )

    async def lookup_observables_async(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """Call base async wrapper."""
        return await self._lookup_items_async_wrapper(
            data,
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            **kwargs,
        )

    async def _lookup_observables_async_wrapper(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
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
            2. Dict of observable, Observables Types
            3. Iterable of observables - Observables Types will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        obs_type_col : str, optional
            DataFrame column to use for Observables Types, by default None
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
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            **kwargs,
        )
