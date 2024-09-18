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
from __future__ import annotations

from typing import TYPE_CHECKING, ClassVar, Iterable, Mapping

from typing_extensions import Self

from .._version import VERSION
from ..common.utility import export

# used in dynamic instantiation of providers
from .contextproviders import CONTEXT_PROVIDERS
from .lookup import Lookup
from .provider_base import Provider, _make_sync

if TYPE_CHECKING:
    import pandas as pd
__version__ = VERSION
__author__ = "Ian Hellen"


@export
class ContextLookup(Lookup):
    """Observable lookup from providers."""

    _NO_PROVIDERS_MSG: ClassVar[
        str
    ] = """
    No Context Providers are loaded - please check that
    you have correctly configured your msticpyconfig.yaml settings.
    """

    _HELP_URI: ClassVar[str] = (
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
        "ContextProviders.html#configuration-file"
    )

    PACKAGE = "contextproviders"

    PROVIDERS: ClassVar[dict[str, tuple[str, str]]] = CONTEXT_PROVIDERS
    CUSTOM_PROVIDERS: ClassVar[dict[str, type[Provider]]] = {}

    def lookup_observable(  # pylint:disable=too-many-arguments # noqa:PLR0913
        self: Self,
        observable: str,
        observable_type: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        show_not_supported: bool = False,
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
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]] = None,
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        show_not_supported: bool, optional
            Include the not supported observables in the result DF. Defaults to False.

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
            show_not_supported=show_not_supported,
        )

    def lookup_observables(  # pylint:disable=too-many-arguments # noqa:PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        obs_col: str | None = None,
        obs_type_col: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
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
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"

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
            ),
        )

    # pylint: disable=too-many-locals
    async def _lookup_observables_async(  # pylint:disable=too-many-arguments # noqa:PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        obs_col: str | None = None,
        obs_type_col: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
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
        )

    def lookup_observables_sync(  # pylint:disable=too-many-arguments # noqa:PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        obs_col: str | None = None,
        obs_type_col: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
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
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"

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
        )

    def _load_providers(
        self: Self,
        *,
        providers: str = "ContextProviders",
    ) -> None:
        """Load provider classes based on config."""
        super()._load_providers(providers=providers)
