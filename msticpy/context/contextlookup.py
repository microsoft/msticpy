# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for ContextLookup classes.

Input can be a single Observable observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from typing import Iterable, List, Mapping, Optional, Union

import pandas as pd

from .._version import VERSION
from ..common.utility import export

# used in dynamic instantiation of providers
from .contextproviders import CONTEXT_PROVIDERS
from .lookup import Lookup
from .provider_base import _make_sync

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class ContextLookup(Lookup):
    """Observable lookup from providers."""

    _NO_PROVIDERS_MSG = """
    No Context Providers are loaded - please check that
    you have correctly configured your msticpyconfig.yaml settings.
    """

    _HELP_URI = (
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
        "ContextProviders.html#configuration-file"
    )

    PACKAGE = "contextproviders"

    PROVIDERS = CONTEXT_PROVIDERS

    # pylint: disable=too-many-arguments
    def lookup_observable(
        self,
        observable: str,
        observable_type: Optional[str] = None,
        query_type: Optional[str] = None,
        providers: Optional[List[str]] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup single observable in active providers.

        Parameters
        ----------
        observable : str
            observable
        observable_type : str, optional
            One of IoCExtract.IOCType, by default None
            If none, the Observable type will be inferred
        query_type: str, optional
            The observable query type (e.g. rep, info, malware)
        providers: List[str]
            Explicit list of providers to use
        default_providers: Optional[List[str]] = None,
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        kwargs :
            Additional arguments passed to the underlying provider(s)

        Returns
        -------
        pd.DataFrame
            The result returned as a DataFrame:
            bool indicates whether a TI record was found in any provider
            list has an entry for each provider result

        """
        return self.lookup_item(
            item=observable,
            item_type=observable_type,
            query_type=query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    def lookup_observables(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup a collection of Observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Mapping (e.g. a dict) of [observable, ObservableType]
            3. Iterable of observables - ObservableTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
            ("col" and "column" are also aliases for this parameter)
        obs_type_col : str, optional
            DataFrame column to use for ObservableTypes, by default None
        query_type: str, optional
            The observable query type (e.g. rep, info, malware)
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
            self._lookup_observables_async(
                data=data,
                obs_col=obs_col,
                obs_type_col=obs_type_col,
                query_type=query_type,
                providers=providers,
                default_providers=default_providers,
                prov_scope=prov_scope,
                **kwargs,
            )
        )

    # pylint: disable=too-many-locals
    async def _lookup_observables_async(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """Lookup items async."""
        return await self._lookup_items_async(
            data,
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    def lookup_observables_sync(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        obs_type_col: str = None,
        query_type: str = None,
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
            `obs_col` parameter)
            2. Mapping (e.g. a dict) of [observable, IoCType]
            3. Iterable of observables - IoCTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
            ("col" and "column" are also aliases for this parameter)
        obs_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        query_type: str, optional
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
            item_col=obs_col,
            item_type_col=obs_type_col,
            query_type=query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    def _load_providers(self, **kwargs):
        """Load provider classes based on config."""
        return super()._load_providers(providers="ContextProviders", **kwargs)
