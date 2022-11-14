# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Base class for Provider classes.

Input can be a single item or a pandas DataFrame containing
multiple items. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""

import asyncio
import collections
from abc import ABC, abstractmethod
from asyncio import get_event_loop
from functools import lru_cache, partial, singledispatch
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple, Union

import pandas as pd

from .._version import VERSION
from ..common.utility import export
from ..transform.iocextract import IoCExtract as ItemExtract
from ..transform.iocextract import IoCType as Type
from .lookup_result import LookupStatus
from .preprocess_observable import PreProcessor

__version__ = VERSION
__author__ = "Ian Hellen"

_ITEM_EXTRACT = ItemExtract()


@export
class Provider(ABC):
    """Abstract base class for Providers."""

    _QUERIES: Dict[str, Any] = {}

    @abstractmethod
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
            DataFrame of results.

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
        result: Dict[str, Any] = {
            "Item": item,
            "SanitizedValue": item,
            "ItemType": item_type or self.resolve_item_type(item_type),
            "QuerySubtype": query_subtype,
            "Result": False,
            "Details": "",
            "RawResult": None,
            "Reference": None,
            "Status": LookupStatus.OK.value,
        }

        if not self.is_supported_type(result["ItemType"]):
            result["Details"] = f"Type {result['ItemType']} not supported."
            result["Status"] = LookupStatus.NOT_SUPPORTED.value
            return result

        clean_item = self._preprocessors.check(
            item,
            item_type,
            require_url_encoding=self.require_url_encoding,
        )

        result["SanitizedValue"] = clean_item.observable

        if clean_item.status != "ok":
            result["Details"] = clean_item.status
            result["Status"] = LookupStatus.BAD_FORMAT.value

        return result

    # pylint: disable=unused-argument
    def __init__(self, **kwargs):
        """Initialize the provider."""
        self.description: Optional[str] = None
        self._supported_types: Set[Type] = set()
        self._supported_types = {
            Type.parse(type.split("-")[0]) for type in self._QUERIES
        }
        if Type.unknown in self._supported_types:
            self._supported_types.remove(Type.unknown)

        self.require_url_encoding = False
        self._preprocessors = PreProcessor()

    @property
    def name(self) -> str:
        """Return the name of the provider."""
        return self.__class__.__name__

    def lookup_items(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        item_col: str = None,
        item_type_col: str = None,
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
            `item_col` parameter)
            2. Dict of items
            3. Iterable of items
        item_col : str, optional
            DataFrame column to use for items, by default None
        item_type_col : str, optional
            DataFrame column to use for types, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        results = []
        for item, item_type in generate_items(data, item_col, item_type_col):
            if not item:
                continue
            item_result = self.lookup_item(
                item,
                item_type,
                query_type,
                **kwargs,
            )
            results.append(item_result)

        return pd.concat(results)

    async def lookup_items_async(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        item_col: str = None,
        item_type_col: str = None,
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
            `item_col` parameter)
            2. Dict of items, Type
            3. Iterable of items - Types will be inferred
        item_col : str, optional
            DataFrame column to use for items, by default None
        item_type_col : str, optional
            DataFrame column to use for Types, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        event_loop = get_event_loop()
        results = []
        prog_counter = kwargs.pop("prog_counter", None)
        for item, item_type in generate_items(data, item_col, item_type_col):
            item_type = kwargs.pop("item_type", item_type)
            if not item:
                continue
            get_item = partial(
                self.lookup_item,
                item=item,
                item_type=item_type,
                query_type=query_type,
                **kwargs,
            )
            item_result: pd.DataFrame = await event_loop.run_in_executor(None, get_item)
            if prog_counter:
                await prog_counter.decrement()
            results.append(item_result)

        return pd.concat(results)

    @property
    def item_query_defs(self) -> Dict[str, Any]:
        """
        Return current dictionary of IoC query/request definitions.

        Returns
        -------
        Dict[str, Any]
            IoC query/request definitions keyed by IoCType

        """
        return self._QUERIES

    @classmethod
    def is_known_type(cls, item_type: str) -> bool:
        """
        Return True if this a known IoC Type.

        Parameters
        ----------
        item_type : str
            IoCType string to test

        Returns
        -------
        bool
            True if known type.

        """
        return item_type in Type.__members__ and item_type != "unknown"

    @property
    def supported_types(self) -> List[str]:
        """
        Return list of supported types for this provider.

        Returns
        -------
        List[str]
            List of supported type names

        """
        return [item.name for item in self._supported_types]

    @classmethod
    def usage(cls):
        """Print usage of provider."""
        print(f"{cls.__doc__} Supported query types:")
        for key in sorted(cls._QUERIES):
            elements = key.split("-", maxsplit=1)
            if len(elements) == 1:
                print(f"\titem_type={elements[0]}")
            if len(elements) == 2:
                print(f"\titem_type={elements[0]}, query_type={elements[1]}")

    def is_supported_type(self, item_type: Union[str, Type]) -> bool:
        """
        Return True if the passed type is supported.

        Parameters
        ----------
        item_type : Union[str, Type]
            type name or instance

        Returns
        -------
        bool
            True if supported.

        """
        if isinstance(item_type, str):
            item_type = Type.parse(item_type)
        return item_type.name in self.supported_types

    @staticmethod
    @lru_cache(maxsize=1024)
    def resolve_item_type(item: str) -> str:
        """
        Return Type determined by ItemExtract.

        Parameters
        ----------
        item : str
            Item string

        Returns
        -------
        str
            Type (or unknown if type could not be determined)

        """
        return _ITEM_EXTRACT.get_ioc_type(item)

    async def _lookup_items_async_wrapper(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        item_col: str = None,
        item_type_col: str = None,
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
            `item_col` parameter)
            2. Dict of items, Type
            3. Iterable of items - Type will be inferred
        item_col : str, optional
            DataFrame column to use for items, by default None
        item_type_col : str, optional
            DataFrame column to use for Type, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoitemC type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        event_loop = get_event_loop()
        prog_counter = kwargs.pop("prog_counter", None)
        get_items = partial(
            self.lookup_items,
            data,
            item_col,
            item_type_col,
            query_type,
            **kwargs,
        )
        result = await event_loop.run_in_executor(None, get_items)
        if prog_counter:
            await prog_counter.decrement(len(data))  # type: ignore
        return result


class PivotProvider(ABC):
    """A class which provides pivot functions and a means of registering them."""

    @abstractmethod
    def register_pivots(
        self,
        pivot_reg: "PivotRegistration",  # type: ignore # noqa: F821
        pivot: "Pivot",  # type: ignore # noqa: F821
    ):
        """
        Register pivot functions for a Provider.

        Parameters
        ----------
        pivot_reg : PivotRegistration
            Pivot registration settings.
        pivot : Pivot
            Pivot library instance

        """


@singledispatch
def generate_items(
    data: Any, item_col: Optional[str] = None, item_type_col: Optional[str] = None
) -> Iterable[Tuple[Optional[str], Optional[str]]]:
    """
    Generate item pairs from different input types.

    Parameters
    ----------
    data : Any
        DataFrame, dictionary or iterable
    item_col : Optional[str]
        If `data` is a DataFrame, the column containing the item value.
    item_type_col : Optional[str]
        If `data` is a DataFrame, the column containing the item type.

    Returns
    -------
    Iterable[Tuple[Optional[str], Optional[str]]]] - a tuple of Observable/Type.

    """
    del item_col, item_type_col

    if isinstance(data, collections.abc.Iterable):
        for item in data:
            yield item, Provider.resolve_item_type(item)
    else:
        yield None, None


@generate_items.register(pd.DataFrame)
def _(data: pd.DataFrame, item_col: str, item_type_col: Optional[str] = None):
    for _, row in data.iterrows():
        if item_type_col is None:
            yield row[item_col], Provider.resolve_item_type(row[item_col])
        else:
            yield row[item_col], row[item_type_col]


@generate_items.register(dict)  # type: ignore
def _(data: dict, item_col: Optional[str] = None, item_type_col: Optional[str] = None):
    del item_col, item_type_col
    for item, item_type in data.items():
        if not item_type:
            item_type = Provider.resolve_item_type(item)
        yield item, item_type


def _make_sync(future):
    """Wait for an async call, making it sync."""
    try:
        event_loop = asyncio.get_event_loop()
    except RuntimeError:
        # Generate an event loop if there isn't any.
        event_loop = asyncio.new_event_loop()
        asyncio.set_event_loop(event_loop)
    return event_loop.run_until_complete(future)
