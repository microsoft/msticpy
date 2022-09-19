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

import pandas as pd

from ..provider_base import Provider, _make_sync
from ..lookup_result import LookupResult
from .context_lookup_result import ContextLookupResult, ContextLookupStatus
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class ContextProvider(Provider):
    """Abstract base class for Context providers."""

    _REQUIRED_PARAMS: List[str] = []

    def lookup_item(
        self, item: str, item_type: str = None, query_type: str = None, **kwargs
    ) -> LookupResult:
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
        LookupResult
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
        return self.lookup_observable(
            item, obs_type=item_type, query_type=query_type, **kwargs
        )

    @abstractmethod
    def lookup_observable(  # type: ignore
        self,
        observable: str,
        obs_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> ContextLookupResult:
        """
        Lookup a single item.

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
        ContextLookupResult
            The context lookup result:
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
        Note: this method uses memorization (lru_cache) to cache results
        for a particular observable to try avoid repeated network calls for
        the same item.

        """

    def _check_item_type(
        self, item: str, item_type: str = None, query_subtype: str = None
    ) -> LookupResult:
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
        LookupResult
            Lookup result with resolved ioc_type and pre-processed
            observable.
            LookupResult.status is none-zero on failure.
        """
        return self._check_observable_type(
            observable=item, observable_type=item_type, query_subtype=query_subtype
        )

    def _check_observable_type(
        self, observable: str, observable_type: str = None, query_subtype: str = None
    ) -> ContextLookupResult:
        """
        Check Observable Type and cleans up observable.

        Parameters
        ----------
        observable : str
            observable
        observable_type : str, optional
            observable type, by default None
        query_subtype : str, optional
            Query sub-type, if any, by default None

        Returns
        -------
        LookupResult
            ContextLookup result with resolved observed_type and pre-processed
            observable.
            ContextLookupResult.status is none-zero on failure.

        """
        result = ContextLookupResult(
            observable=observable,
            sanitized_value=observable,
            observable_type=observable_type or self.resolve_item_type(observable),
            query_subtype=query_subtype,
            result=False,
            details="",
            raw_result=None,
            reference=None,
        )

        if not self.is_supported_type(result.observable_type):
            result.details = f"Type {result.observable_type} not supported."
            result.status = ContextLookupStatus.NOT_SUPPORTED.value
            return result

        clean_observable = self._preprocessors.check(
            result.observable,
            result.observable_type,
            require_url_encoding=self.require_url_encoding,
        )

        result.sanitized_value = clean_observable.observable

        if clean_observable.status != "ok":
            result.details = clean_observable.status
            result.status = ContextLookupStatus.BAD_FORMAT.value

        return result

    @abstractmethod
    def parse_results(self, response: ContextLookupResult) -> Tuple[bool, Any]:
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
            self.lookup_observable,
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            **kwargs,
        ).rename(columns=ContextLookupResult.column_map())

    async def lookup_observables_async(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup collection of items.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observables, Observables Type
            3. Iterable of Observables - Types will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        obs_type_col : str, optional
            DataFrame column to use for Observables Types, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        return _make_sync(
            self.lookup_items_async(
                data,
                self.lookup_observable,
                item_col=obs_col,
                item_type_col=obs_type_col,
                query_type=query_type,
                **kwargs,
            )
        ).rename(columns=ContextLookupResult.column_map())

    async def _lookup_observables_async_wrapper(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Async wrapper for providers that do not implement lookup_items_async.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observable, Type
            3. Iterable of observables - Type will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        obs_type_col : str, optional
            DataFrame column to use for Observable Type, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoitemC type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        return self._lookup_items_async_wrapper(
            data,
            self.lookup_observable,
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            **kwargs,
        )
