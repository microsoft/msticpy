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

from typing import Iterable, List, Mapping, Optional, Tuple, Union

import pandas as pd

from .._version import VERSION
from ..common.utility import export

from .provider_base import _make_sync
from .lookup import Lookup

# used in dynamic instantiation of providers
from .tiproviders import TI_PROVIDERS
from .lookup_result import LookupResult
from .tiproviders.ti_lookup_result import TILookupResult


__version__ = VERSION
__author__ = "Ian Hellen"


@export
class TILookup(Lookup):
    """Threat Intel observable lookup from providers."""

    _NO_PROVIDERS_MSG = """
    No TI Providers are loaded - please check that
    you have correctly configured your msticpyconfig.yaml settings.
    """
    _HELP_URI = (
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
        "TIProviders.html#configuration-file"
    )

    PROVIDERS = TI_PROVIDERS
    PACKAGE = "tiproviders"

    # pylint: disable=too-many-arguments
    def lookup_ioc(
        self,
        ioc: str,
        ioc_type: str = None,
        ioc_query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> Tuple[bool, List[Tuple[str, LookupResult]]]:
        """
        Lookup single IoC in active providers.

        Parameters
        ----------
        observable : str
            IoC observable
            (`ioc` is also an alias for observable)
        ioc_type : str, optional
            One of IoCExtract.IoCType, by default None
            If none, the IoC type will be inferred
        ioc_query_type: str, optional
            The ioc query type (e.g. rep, info, malware)
        providers: List[str]
            Explicit list of providers to use
        default_providers: Optional[List[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        kwargs :
            Additional arguments passed to the underlying provider(s)

        Returns
        -------
        Tuple[bool, List[Tuple[str, TILookupResult]]]
            The result returned as a tuple(bool, list):
            bool indicates whether a TI record was found in any provider
            list has an entry for each provider result

        """
        return self.lookup_item(
            item=ioc,
            lookup_function_name="lookup_ioc",
            item_type=ioc_type,
            query_type=ioc_query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        ioc_query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup a collection of IoCs.

        Parameters
        ----------
        data : Union[pd.DataFrame, Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `ioc_col` parameter)
            2. Mapping (e.g. a dict) of [observable, IoCType]
            3. Iterable of observables - IoCTypes will be inferred
        ioc_col : str, optional
            DataFrame column to use for observables, by default None
            ("col" and "column" are also aliases for this parameter)
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        ioc_query_type: str, optional
            The ioc query type (e.g. rep, info, malware)
        providers: List[str]
            Explicit list of providers to use
        default_providers: Optional[List[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        kwargs :
            Additional arguments passed to the underlying provider(s)

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        return _make_sync(
            self._lookup_iocs_async(
                data=data,
                ioc_col=ioc_col,
                ioc_type_col=ioc_type_col,
                ioc_query_type=ioc_query_type,
                providers=providers,
                default_providers=default_providers,
                prov_scope=prov_scope,
                **kwargs,
            )
        )

    # pylint: disable=too-many-locals
    async def _lookup_iocs_async(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        ioc_query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """Lookup IoCs async."""
        return await self._lookup_items_async(
            data,
            lookup_function_name="lookup_iocs_async",
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            ioc_query_type=ioc_query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    def lookup_iocs_sync(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        ioc_col: str = None,
        ioc_type_col: str = None,
        ioc_query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup a collection of IoCs.

        Parameters
        ----------
        data : Union[pd.DataFrame, Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `ioc_col` parameter)
            2. Mapping (e.g. a dict) of [observable, IoCType]
            3. Iterable of observables - IoCTypes will be inferred
        ioc_col : str, optional
            DataFrame column to use for observables, by default None
            ("col" and "column" are also aliases for this parameter)
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        ioc_query_type: str, optional
            The ioc query type (e.g. rep, info, malware)
        providers: List[str]
            Explicit list of providers to use
        default_providers: Optional[List[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        kwargs :
            Additional arguments passed to the underlying provider(s)

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        return self.lookup_items_sync(
            data,
            lookup_function_name="lookup_iocs",
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            query_type=ioc_query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    @staticmethod
    def result_to_df(
        item_lookup: Tuple[bool, List[Tuple[str, LookupResult]]]
    ) -> pd.DataFrame:
        """
        Return DataFrame representation of IoC Lookup response.

        Parameters
        ----------
        ioc_lookup : Tuple[bool, List[Tuple[str, TILookupResult]]]
            Output from `lookup_ioc`

        Returns
        -------
        pd.DataFrame
            The response as a DataFrame with a row for each
            provider response.

        """
        return (
            Lookup.result_to_df(item_lookup)
            .rename(columns=TILookupResult.column_map())
            .drop("SafeIoc", errors="ignore", axis=1)
        )

    def _load_providers(self, **kwargs):
        """Load provider classes based on config."""
        return super()._load_providers(providers="TIProviders", **kwargs)
