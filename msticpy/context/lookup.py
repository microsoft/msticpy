# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Generic Module for Lookup classes.

Input can be a single item or a pandas DataFrame containing
multiple items. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

import asyncio
import importlib
import logging
import warnings
from collections import ChainMap
from typing import (
    TYPE_CHECKING,
    Any,
    Callable,
    ClassVar,
    Iterable,
    Mapping,
    Sized,
)

import nest_asyncio
import pandas as pd
from tqdm.auto import tqdm
from typing_extensions import Self

from .._version import VERSION
from ..common.exceptions import MsticpyConfigError, MsticpyUserConfigError
from ..common.provider_settings import (
    ProviderSettings,
    get_provider_settings,
    reload_settings,
)
from ..common.utility import export, is_ipython
from ..vis.ti_browser import browse_results
from .lookup_result import LookupStatus

# used in dynamic instantiation of providers
from .provider_base import Provider, _make_sync

if TYPE_CHECKING:
    import datetime as dt
    from types import ModuleType

    from ..nbwidgets.select_item import SelectItem

__version__ = VERSION
__author__ = "Florian Bracq"

logger: logging.Logger = logging.getLogger(__name__)


class ProgressCounter:
    """Progress counter for async tasks."""

    def __init__(self: ProgressCounter, total: int) -> None:
        """Initialize the class."""
        self.total: int = total
        self._lock: asyncio.Condition = asyncio.Condition()
        self._remaining: int = total

    async def decrement(self: Self, increment: int = 1) -> None:
        """Decrement the counter."""
        if self._remaining == 0:
            return
        async with self._lock:
            self._remaining -= increment

    async def get_remaining(self: Self) -> int:
        """Get the current remaining count."""
        async with self._lock:
            return self._remaining


@export
class Lookup:
    """Item lookup from providers."""

    _NO_PROVIDERS_MSG: ClassVar[
        str
    ] = """
    No Providers are loaded - please check that
    you have correctly configured your msticpyconfig.yaml settings.
    """

    _HELP_URI: ClassVar[str] = (
        "https://msticpy.readthedocs.io/en/latest/DataEnrichment.html"
    )

    PROVIDERS: ClassVar[dict[str, tuple[str, str]]] = {}
    CUSTOM_PROVIDERS: ClassVar[dict[str, type[Provider]]]

    PACKAGE: ClassVar[str] = ""

    def __init__(
        self: Lookup,
        providers: list[str] | None = None,
        *,
        primary_providers: list[Provider] | None = None,
        secondary_providers: list[Provider] | None = None,
    ) -> None:
        """
        Initialize TILookup instance.

        Parameters
        ----------
        primary_providers : Optional[list[Provider]], optional
            Primary Providers, by default None
        secondary_providers : Optional[list[Provider]], optional
            Secondary Providers, by default None
        providers: Optional[list[str]], optional
            List of provider names to load, by default all available
            providers are loaded. To see the list of available providers
            call `TILookup.list_available_providers()`.
            Note: if primary_provides or secondary_providers is specified
            This will override the providers list.

        """
        self._providers: dict[str, Provider] = {}
        self._secondary_providers: dict[str, Provider] = {}
        self._providers_to_load: list[str] | None = providers

        if primary_providers:
            for prov in primary_providers:
                self.add_provider(prov, primary=True)
        if secondary_providers:
            warnings.warn(
                "'secondary_providers' is a deprecated parameter",
                DeprecationWarning,
                stacklevel=1,
            )
            for prov in secondary_providers:
                self.add_provider(prov, primary=False)
        if not (primary_providers or secondary_providers):
            self._load_providers()

        self._all_providers = ChainMap(self._secondary_providers, self._providers)
        if is_ipython():
            nest_asyncio.apply()

    @property
    def loaded_providers(self: Self) -> dict[str, Provider]:
        """
        Return dictionary of loaded providers.

        Returns
        -------
        dict[str, TIProvider]
            [description]

        """
        return dict(self._all_providers)

    @property
    def provider_status(self: Self) -> Iterable[str]:
        """
        Return loaded provider status.

        Returns
        -------
        Iterable[str]
            List of providers and descriptions.

        """
        prim: list[str] = [
            f"{prov_name} - {prov.description} (primary)"
            for prov_name, prov in self._providers.items()
        ]
        sec: list[str] = [
            f"{prov_name} - {prov.description} (secondary)"
            for prov_name, prov in self._secondary_providers.items()
        ]
        return prim + sec

    @property
    def configured_providers(self: Self) -> list[str]:
        """
        Return a list of available providers that have configuration details present.

        Returns
        -------
        list[str]
            List of TI Provider classes.

        """
        prim_conf = list(self._providers.keys())
        sec_conf = list(self._secondary_providers.keys())

        return prim_conf + sec_conf

    def enable_provider(self: Self, providers: str | Iterable[str]) -> None:
        """
        Set the provider(s) as primary (used by default).

        Parameters
        ----------
        providers : Union[str, Iterable[str]
            Provider name or list of names.
            Use `list_available_providers()` to see the list of loaded providers.

        Raises
        ------
        ValueError
            If the provider name is not recognized.

        """
        provs_to_enable: list[str] | Iterable[str] = (
            [providers] if isinstance(providers, str) else providers
        )
        for provider in provs_to_enable:
            if provider in self._secondary_providers:
                self._providers[provider] = self._secondary_providers[provider]
                del self._secondary_providers[provider]
            elif provider not in self._providers:
                available_providers: list[str] | None = self.list_available_providers(
                    as_list=True,
                )
                if not available_providers:
                    err_msg: str = (
                        f"Unknown provider '{provider}'. No available providers."
                    )
                else:
                    err_msg = (
                        f"Unknown provider '{provider}'. Available providers:"
                        ", ".join(available_providers)
                    )
                raise ValueError(err_msg)

    def disable_provider(self: Self, providers: str | Iterable[str]) -> None:
        """
        Set the provider as secondary (not used by default).

        Parameters
        ----------
        providers : Union[str, Iterable[str]
            Provider name or list of names.
            Use `list_available_providers()` to see the list of loaded providers.

        Raises
        ------
        ValueError
            If the provider name is not recognized.

        """
        provs_to_disable: list[str] | Iterable[str] = (
            [providers] if isinstance(providers, str) else providers
        )
        for provider in provs_to_disable:
            if provider in self._providers:
                self._secondary_providers[provider] = self._providers[provider]
                del self._providers[provider]
            elif provider not in self._secondary_providers:
                available_providers: list[str] | None = self.list_available_providers(
                    as_list=True,
                )
                if not available_providers:
                    err_msg: str = (
                        f"Unknown provider '{provider}'. No available providers."
                    )
                else:
                    err_msg = (
                        f"Unknown provider '{provider}'. Available providers:"
                        ", ".join(available_providers)
                    )
                raise ValueError(err_msg)

    def set_provider_state(self: Self, prov_dict: dict[str, bool]) -> None:
        """
        Set a dict of providers to primary/secondary.

        Parameters
        ----------
        prov_dict : dict[str, bool]
            Dictionary of provider name and bool - True if enabled/primary,
            False if disabled/secondary.

        """
        for prov, state in prov_dict.items():
            if state:
                self.enable_provider(prov)
            else:
                self.disable_provider(prov)

    @classmethod
    def browse_results(
        cls: type[Self],
        data: pd.DataFrame,
        severities: list[str] | None = None,
        *,
        height: str = "300px",
    ) -> SelectItem | None:
        """
        Return TI Results list browser.

        Parameters
        ----------
        data : pd.DataFrame
            TI Results data from TIProviders
        severities : Optional[list[str]], optional
            A list of the severity classes to show.
            By default these are ['warning', 'high'].
            Pass ['information', 'warning', 'high'] to see all
            results.
        height: str, Optional
            Height of the widget

        Returns
        -------
        SelectItem
            SelectItem browser for TI Data.

        """
        if not isinstance(data, pd.DataFrame):
            logger.info("Input data is in an unexpected format.")
            return None
        return browse_results(data=data, severities=severities, height=height)

    browse: Callable[..., SelectItem | None] = browse_results

    def provider_usage(self: Self) -> None:
        """Print usage of loaded providers."""
        print("Primary providers")
        print("-----------------")
        if self._providers:
            for prov_name, prov in self._providers.items():
                print("\nProvider class: %s", prov_name)
                prov.usage()
        else:
            print("none")
        print("\nSecondary providers")
        print("-------------------")
        if self._secondary_providers:
            for prov_name, prov in self._secondary_providers.items():
                print("\nProvider class: %s", prov_name)
                prov.usage()
        else:
            print("none")

    @classmethod
    def reload_provider_settings(cls: type[Self]) -> None:
        """Reload provider settings from config."""
        reload_settings()
        logger.info(
            "Settings reloaded. Use reload_providers to update settings for loaded providers.",
        )

    def reload_providers(self: Self) -> None:
        """Reload settings and provider classes."""
        reload_settings()
        self._load_providers()

    def add_provider(
        self: Self,
        provider: Provider,
        name: str | None = None,
        *,
        primary: bool = True,
    ) -> None:
        """
        Add a provider to the current collection.

        Parameters
        ----------
        provider : TIProvider
            Provider instance
        name : str, optional
            The name to use for the provider (overrides the class name
            of `provider`)
        primary : bool, optional
            "primary" or "secondary" if False, by default "primary"

        """
        if not name:
            name = str(provider.__class__.__name__)
        if primary:
            self._providers[name] = provider
        else:
            self._secondary_providers[name] = provider

    def lookup_item(  # pylint: disable=too-many-locals, too-many-arguments #noqa: PLR0913
        self: Self,
        item: str,
        item_type: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        show_not_supported: bool = False,
        start: dt.datetime | None = None,
        end: dt.datetime | None = None,
    ) -> pd.DataFrame:
        """
        Lookup single item in active providers.

        Parameters
        ----------
        item : str
            item to lookup
        item_type : str, optional
            One of ItemExtract.ItemType, by default None
            If none, the Item type will be inferred
        query_type: str, optional
            The query type (e.g. rep, info, malware)
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        show_not_supported: bool
            If True, display unsupported items. Defaults to False
        start: dt.datetime
            If supported by the provider, start time for the item's validity
        end: dt.datetime
            If supported by the provider, end time for the item's validity

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        return self.lookup_items(
            data={item: item_type},
            query_type=query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            show_not_supported=show_not_supported,
            start=start,
            end=end,
        )

    def lookup_items(  # pylint: disable=too-many-arguments #noqa: PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Sized,
        item_col: str | None = None,
        item_type_col: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        show_not_supported: bool = False,
        start: dt.datetime | None = None,
        end: dt.datetime | None = None,
    ) -> pd.DataFrame:
        """
        Lookup a collection of items.

        Parameters
        ----------
        data : Union[pd.DataFrame, Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `item_col` parameter)
            2. Mapping (e.g. a dict) of [item, ItemType]
            3. Iterable of items - ItemTypes will be inferred
        item_col : str, optional
            DataFrame column to use for items, by default None
            ("col" and "column" are also aliases for this parameter)
        item_type_col : str, optional
            DataFrame column to use for ItemTypes, by default None
        query_type: str, optional
            The item query type (e.g. rep, info, malware)
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        show_not_supported: bool
            If True, display unsupported items. Defaults to False
        start: dt.datetime
            If supported by the provider, start time for the item's validity
        end: dt.datetime
            If supported by the provider, end time for the item's validity

        Other Parameters
        ----------------
        progress : bool
            Use progress bar to track completion, by default True

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        return _make_sync(
            self._lookup_items_async(
                data=data,
                item_col=item_col,
                item_type_col=item_type_col,
                query_type=query_type,
                providers=providers,
                default_providers=default_providers,
                prov_scope=prov_scope,
                show_not_supported=show_not_supported,
                start=start,
                end=end,
            ),
        )

    @staticmethod
    def result_to_df(item_lookup: pd.DataFrame) -> pd.DataFrame:
        """
        Return DataFrame representation of Lookup response.

        Parameters
        ----------
        item_lookup : pd.DataFrame
            Output from `lookup_item`

        Returns
        -------
        pd.DataFrame
            The response as a DataFrame with a row for each
            provider response.

        """
        if not isinstance(item_lookup, pd.DataFrame):
            err_msg: str = f"DataFrame was expected, but {type(item_lookup)} received."
            raise TypeError(err_msg)
        return item_lookup

    async def _lookup_items_async(  # pylint: disable=too-many-locals, too-many-arguments #noqa: PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Sized,
        item_col: str | None = None,
        item_type_col: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        show_not_supported: bool = False,
        show_bad_item: bool = False,
        progress: bool = True,
        col: str | None = None,
        column: str | None = None,
        start: dt.datetime | None = None,
        end: dt.datetime | None = None,
    ) -> pd.DataFrame:
        """Lookup items async."""
        del start, end
        item_col = item_col or col or column
        selected_providers: dict[str, Any] = self._select_providers(
            providers or default_providers,
            prov_scope,
        )
        if not selected_providers:
            raise MsticpyUserConfigError(
                self._NO_PROVIDERS_MSG,
                title="No Threat Intel Provider configuration found.",
                help_uri=self._HELP_URI,
            )

        event_loop: asyncio.AbstractEventLoop = asyncio.get_event_loop()
        result_futures: list[Any] = []
        provider_names: list[str] = []

        prog_counter = ProgressCounter(
            total=len(data) * len(selected_providers),
        )

        # create a list of futures/tasks to await
        for prov_name, provider in selected_providers.items():
            provider_names.append(prov_name)
            result_futures.append(
                provider.lookup_items_async(
                    data=data,
                    item_col=item_col,
                    item_type_col=item_type_col,
                    query_type=query_type,
                    prog_counter=prog_counter if progress else None,
                ),
            )
        if progress:
            # Create a task for tqdm
            prog_task: asyncio.Task = event_loop.create_task(
                self._track_completion(prog_counter),
            )
        # collect the return values of the tasks
        results: list[pd.DataFrame] = await asyncio.gather(*result_futures)
        # cancel the progress task if results have completed.
        if progress:
            prog_task.cancel()
        return self._combine_results(
            results,
            provider_names,
            show_not_supported=show_not_supported,
            show_bad_item=show_bad_item,
        )

    def lookup_items_sync(  # pylint: disable=too-many-arguments, too-many-locals #noqa: PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        item_col: str | None = None,
        item_type_col: str | None = None,
        query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        col: str | None = None,
        column: str | None = None,
        show_not_supported: bool = False,
        show_bad_item: bool = False,
    ) -> pd.DataFrame:
        """
        Lookup a collection of items.

        Parameters
        ----------
        data : Union[pd.DataFrame, Mapping[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `item_col` parameter)
            2. Mapping (e.g. a dict) of [item, ItemType]
            3. Iterable of items - ItemTypes will be inferred
        item_col : str, optional
            DataFrame column to use for items, by default None
            ("col" and "column" are also aliases for this parameter)
        item_type_col : str, optional
            DataFrame column to use for ItemTypes, by default None
        query_type: str, optional
            The item query type (e.g. rep, info, malware)
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        col: str, Optional
            Name of the column holding the data
        column: str, Optional
            Name of the column holding the data
        show_not_supported: bool, Optional
            Set to True to include unsupported items in the result DF.
            Defaults to False
        show_bad_item: bool, Optional
            Set to True to include invalid items in the result DF.
            Defaults to False

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        item_col = item_col or col or column

        selected_providers: dict[str, Provider] = self._select_providers(
            providers or default_providers,
            prov_scope,
        )
        if not selected_providers:
            raise MsticpyUserConfigError(
                self._NO_PROVIDERS_MSG,
                title="No Threat Intel Provider configuration found.",
                help_uri=self._HELP_URI,
            )

        results: list[pd.DataFrame] = []
        provider_names: list[str] = []
        for prov_name, provider in selected_providers.items():
            provider_names.append(prov_name)
            results.append(
                provider.lookup_items(
                    data=data,
                    item_col=item_col,
                    item_type_col=item_type_col,
                    query_type=query_type,
                ),
            )
        return self._combine_results(
            results,
            provider_names,
            show_not_supported=show_not_supported,
            show_bad_item=show_bad_item,
        )

    @staticmethod
    async def _track_completion(prog_counter: ProgressCounter) -> None:
        total: float = await prog_counter.get_remaining()
        with tqdm(total=total, unit="obs", desc="Observables processed") as prog_bar:
            try:
                last_remaining: float = total
                while last_remaining:
                    new_remaining: float = await prog_counter.get_remaining()
                    incr: float = last_remaining - new_remaining
                    if incr:
                        prog_bar.update(incr)
                    last_remaining = new_remaining
                    await asyncio.sleep(0)
            except asyncio.CancelledError:
                # make progress bar get to 100% on cancel
                final_remaining: float = await prog_counter.get_remaining()
                if final_remaining:
                    prog_bar.update(total - final_remaining)

    @property
    def available_providers(self: Self) -> list[str]:
        """
        Return a list of builtin and plugin providers.

        Returns
        -------
        list[str]
            List of TI Provider classes.

        """
        return list(self.PROVIDERS) + list(self.CUSTOM_PROVIDERS)

    @classmethod
    def list_available_providers(
        cls: type[Self],
        *,
        show_query_types: bool = False,
        as_list: bool = False,
    ) -> list[str] | None:
        """
        Print a list of builtin providers with optional usage.

        Parameters
        ----------
        show_query_types : bool, optional
            Show query types supported by providers, by default False
        as_list : bool, optional
            Return list of providers instead of printing to stdout.
            Note: if you specify `show_query_types` this will be printed
            irrespective of this parameter setting.

        Returns
        -------
        Optional[list[str]]
            A list of provider names (if `return_list=True`)

        """
        providers: list[str] = []
        for provider_name in cls.PROVIDERS:
            provider_class: type[Provider] = cls.import_provider(provider_name)
            if not as_list:
                logger.info(provider_name)
            providers.append(provider_name)
            if show_query_types and provider_class:
                provider_class.usage()

        return providers if as_list else None

    @classmethod
    def import_provider(cls: type[Self], provider: str) -> type[Provider]:
        """Import provider class."""
        mod_name, cls_name = cls.PROVIDERS.get(provider, (None, None))

        if not (mod_name and cls_name):
            if hasattr(cls, "CUSTOM_PROVIDERS") and provider in cls.CUSTOM_PROVIDERS:
                return cls.CUSTOM_PROVIDERS[provider]
            err_msg: str = (
                f"No provider named '{provider}'. Possible values are: "
                ", ".join(list(cls.PROVIDERS) + list(cls.CUSTOM_PROVIDERS))
            )
            raise LookupError(err_msg)

        imp_module: ModuleType = importlib.import_module(
            f"msticpy.context.{cls.PACKAGE}.{mod_name}",
            package="msticpy",
        )
        return getattr(imp_module, cls_name)

    def _load_providers(
        self: Self,
        *,
        providers: str = "Providers",
    ) -> None:
        """Load provider classes based on config."""
        prov_settings: dict[str, ProviderSettings] = get_provider_settings(providers)
        for provider_name, settings in prov_settings.items():
            if (
                self._providers_to_load is not None
                and provider_name not in self._providers_to_load
            ) or settings.provider == "--no-load--":
                continue
            try:
                prov_name: str = settings.provider or provider_name
                provider_class: type[Provider] = self.import_provider(prov_name)
            except LookupError:
                warnings.warn(
                    f"Could not find provider class for {provider_name} "
                    f"in config section '{provider_name}'. "
                    f"Provider class name in config is '{settings.provider}'",
                    stacklevel=2,
                )
                prov_name = provider_name
                continue

            # instantiate class sending args from settings to init
            try:
                provider_instance: Provider = provider_class(**(settings.args))
            except MsticpyConfigError as mp_ex:
                # If the TI Provider didn't load, raise an exception
                err_msg: str = (
                    f"Could not load Provider {provider_name} {mp_ex.args}"
                    "To avoid loading this provider please use the 'providers' parameter"
                    " to specify which providers to load."
                )
                raise MsticpyUserConfigError(
                    err_msg,
                    title="Provider configuration error",
                    help_uri=self._HELP_URI,
                ) from mp_ex

            # set the description from settings, if one is provided, otherwise
            # use class docstring.
            provider_instance.description = (
                settings.description or provider_instance.__doc__
            )

            self.add_provider(
                provider=provider_instance,
                name=prov_name,
                primary=settings.primary,
            )

    def _select_providers(
        self: Self,
        providers: list[str] | None = None,
        prov_scope: str = "primary",
    ) -> dict[str, Provider]:
        """
        Return required subset of providers.

        Parameters
        ----------
        providers : list[str], optional
            Explicit list of provider names, by default None
        prov_scope : str, optional
            Provider scope, by default "primary"
            Other values are "all" and "secondary"

        Returns
        -------
        dict[str, TIProvider]
            Dictionary of provider names and instances.

        """
        if providers:
            return {
                prov_name: prov
                for prov_name, prov in self._all_providers.items()
                if prov_name in providers
            }
        if prov_scope == "all":
            return dict(self._all_providers)
        if prov_scope == "primary":
            return self._providers
        return self._secondary_providers

    @staticmethod
    def _combine_results(
        results: Iterable[pd.DataFrame],
        provider_names: list[str],
        *,
        show_not_supported: bool = False,
        show_bad_item: bool = False,
    ) -> pd.DataFrame:
        """Combine dataframe results into single DF."""
        result_list: list[pd.DataFrame] = []
        for prov_name, provider_result in zip(provider_names, results):
            if provider_result is None or provider_result.empty:
                continue
            result: pd.DataFrame = provider_result.copy()
            if not show_not_supported:
                result = result[result["Status"] != LookupStatus.NOT_SUPPORTED.value]
            if not show_bad_item:
                result = result[result["Status"] != LookupStatus.BAD_FORMAT.value]
            result["Provider"] = prov_name
            result_list.append(result)

        if not result_list:
            logger.info("No Item matches")
        return pd.concat(result_list, sort=False) if result_list else pd.DataFrame()
