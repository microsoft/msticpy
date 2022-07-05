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

import abc
import collections
from abc import ABC, abstractmethod
from asyncio import get_event_loop
from functools import lru_cache, partial, singledispatch
from typing import Any, Dict, Iterable, List, Optional, Set, Tuple, Union

import attr
import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from ...transform.iocextract import IoCExtract, IoCType
from .lookup_result import LookupResult, LookupStatus
from .preprocess_observable import PreProcessor

# pylint: disable=unused-import
from .result_severity import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


_IOC_EXTRACT = IoCExtract()


@export
class TIProvider(ABC):
    """Abstract base class for Threat Intel providers."""

    _IOC_QUERIES: Dict[str, Any] = {}

    # pylint: disable=unused-argument
    def __init__(self, **kwargs):
        """Initialize the provider."""
        self._supported_types: Set[IoCType] = set()
        self.description: Optional[str] = None

        self._supported_types = {
            IoCType.parse(ioc_type.split("-")[0]) for ioc_type in self._IOC_QUERIES
        }
        if IoCType.unknown in self._supported_types:
            self._supported_types.remove(IoCType.unknown)

        self.require_url_encoding = False

        self._preprocessors = PreProcessor()

    @property
    def name(self) -> str:
        """Return the name of the provider."""
        return self.__class__.__name__

    @abc.abstractmethod
    def lookup_ioc(
        self, ioc: str, ioc_type: str = None, query_type: str = None, **kwargs
    ) -> LookupResult:
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
        LookupResult
            The returned results.

        """

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
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
        results = []
        for observable, ioc_type in generate_items(data, obs_col, ioc_type_col):
            if not observable:
                continue
            item_result = self.lookup_ioc(
                ioc=observable, ioc_type=ioc_type, query_type=query_type
            )
            results.append(pd.Series(attr.asdict(item_result)))

        return pd.DataFrame(data=results).rename(columns=LookupResult.column_map())

    async def lookup_iocs_async(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
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
        event_loop = get_event_loop()
        results = []
        prog_counter = kwargs.pop("prog_counter", None)
        for observable, ioc_type in generate_items(data, obs_col, ioc_type_col):
            ioc_type = kwargs.pop("ioc_type", ioc_type)
            if not observable:
                continue
            get_ioc = partial(
                self.lookup_ioc,
                ioc=observable,
                ioc_type=ioc_type,
                query_type=query_type,
                **kwargs,
            )
            item_result: LookupResult = await event_loop.run_in_executor(None, get_ioc)
            if prog_counter:
                await prog_counter.decrement()
            results.append(pd.Series(attr.asdict(item_result)))

        return pd.DataFrame(data=results).rename(columns=LookupResult.column_map())
        # return self.lookup_iocs(data, obs_col, ioc_type_col, query_type, status_queue, **kwargs)

    @abc.abstractmethod
    def parse_results(self, response: LookupResult) -> Tuple[bool, ResultSeverity, Any]:
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

    @property
    def supported_types(self) -> List[str]:
        """
        Return list of supported IoC types for this provider.

        Returns
        -------
        List[str]
            List of supported type names

        """
        return [ioc.name for ioc in self._supported_types]

    @property
    def ioc_query_defs(self) -> Dict[str, Any]:
        """
        Return current dictionary of IoC query/request definitions.

        Returns
        -------
        Dict[str, Any]
            IoC query/request definitions keyed by IoCType

        """
        return self._IOC_QUERIES

    @classmethod
    def is_known_type(cls, ioc_type: str) -> bool:
        """
        Return True if this a known IoC Type.

        Parameters
        ----------
        ioc_type : str
            IoCType string to test

        Returns
        -------
        bool
            True if known type.

        """
        return ioc_type in IoCType.__members__ and ioc_type != "unknown"

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

    def is_supported_type(self, ioc_type: Union[str, IoCType]) -> bool:
        """
        Return True if the passed type is supported.

        Parameters
        ----------
        ioc_type : Union[str, IoCType]
            IoC type name or instance

        Returns
        -------
        bool
            True if supported.

        """
        if isinstance(ioc_type, str):
            ioc_type = IoCType.parse(ioc_type)
        return ioc_type.name in self.supported_types

    @staticmethod
    @lru_cache(maxsize=1024)
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
        return _IOC_EXTRACT.get_ioc_type(observable)

    def _check_ioc_type(
        self, ioc: str, ioc_type: str = None, query_subtype: str = None
    ) -> LookupResult:
        """
        Check IoC Type and cleans up observable.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IoC type, by default None
        query_subtype : str, optional
            Query sub-type, if any, by default None

        Returns
        -------
        LookupResult
            Lookup result with resolved ioc_type and pre-processed
            observable.
            LookupResult.status is none-zero on failure.

        """
        result = LookupResult(
            ioc=ioc,
            sanitized_value=ioc,
            ioc_type=ioc_type or self.resolve_ioc_type(ioc),
            query_subtype=query_subtype,
            result=False,
            details="",
            raw_result=None,
            reference=None,
        )

        if not self.is_supported_type(result.ioc_type):
            result.details = f"IoC type {result.ioc_type} not supported."
            result.status = LookupStatus.NOT_SUPPORTED.value
            return result

        clean_ioc = self._preprocessors.check(
            ioc, result.ioc_type, require_url_encoding=self.require_url_encoding
        )

        result.sanitized_value = clean_ioc.observable

        if clean_ioc.status != "ok":
            result.details = clean_ioc.status
            result.status = LookupStatus.BAD_FORMAT.value

        return result

    async def _lookup_iocs_async_wrapper(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
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
        event_loop = get_event_loop()
        prog_counter = kwargs.pop("prog_counter", None)
        get_iocs = partial(
            self.lookup_iocs,
            data=data,
            obs_col=obs_col,
            ioc_type_col=ioc_type_col,
            query_type=query_type,
            **kwargs,
        )
        result = await event_loop.run_in_executor(None, get_iocs)
        if prog_counter:
            await prog_counter.decrement(len(data))  # type: ignore
        return result


class TIPivotProvider(ABC):
    """A class which provides pivot functions and a means of registering them."""

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


@singledispatch
def generate_items(
    data: Any, obs_col: Optional[str] = None, ioc_type_col: Optional[str] = None
) -> Iterable[Tuple[Optional[str], Optional[str]]]:
    """
    Generate item pairs from different input types.

    Parameters
    ----------
    data : Any
        DataFrame, dictionary or iterable
    obs_col : Optional[str]
        If `data` is a DataFrame, the column containing the observable value.
    ioc_type_col : Optional[str]
        If `data` is a DataFrame, the column containing the observable type.

    Returns
    -------
    Iterable[Tuple[Optional[str], Optional[str]]]] - a tuple of Observable/Type.

    """
    del obs_col, ioc_type_col

    if isinstance(data, collections.abc.Iterable):
        for item in data:
            yield item, TIProvider.resolve_ioc_type(item)
    else:
        yield None, None


@generate_items.register(pd.DataFrame)
def _(data: pd.DataFrame, obs_col: str, ioc_type_col: Optional[str] = None):
    for _, row in data.iterrows():
        if ioc_type_col is None:
            yield row[obs_col], TIProvider.resolve_ioc_type(row[obs_col])
        else:
            yield row[obs_col], row[ioc_type_col]


@generate_items.register(dict)  # type: ignore
def _(data: dict, obs_col: Optional[str] = None, ioc_type_col: Optional[str] = None):
    for obs, ioc_type in data.items():
        if not ioc_type:
            ioc_type = TIProvider.resolve_ioc_type(obs)
        yield obs, ioc_type
