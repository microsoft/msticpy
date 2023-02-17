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
import asyncio
import importlib
import warnings
from collections import ChainMap
from typing import Any, Dict, Iterable, List, Mapping, Optional, Tuple, Union

import nest_asyncio
import pandas as pd
from tqdm.auto import tqdm

from .._version import VERSION
from ..common.exceptions import MsticpyConfigError, MsticpyUserConfigError
from ..common.provider_settings import get_provider_settings, reload_settings
from ..common.utility import export, is_ipython
from ..vis.ti_browser import browse_results
from .lookup_result import LookupStatus

# used in dynamic instantiation of providers
from .provider_base import Provider, _make_sync

__version__ = VERSION
__author__ = "Florian Bracq"


class ProgressCounter:
    """Progress counter for async tasks."""

    def __init__(self, total: int):
        """Initialize the class."""
        self.total = total
        self._lock: asyncio.Condition = asyncio.Condition()
        self._remaining: int = total

    async def decrement(self, increment: int = 1):
        """Decrement the counter."""
        if self._remaining == 0:
            return
        async with self._lock:
            self._remaining -= increment

    async def get_remaining(self) -> int:
        """Get the current remaining count."""
        async with self._lock:
            return self._remaining


@export
class Lookup:
    """Item lookup from providers."""

    _NO_PROVIDERS_MSG = """
    No Providers are loaded - please check that
    you have correctly configured your msticpyconfig.yaml settings.
    """

    _HELP_URI = "https://msticpy.readthedocs.io/en/latest/DataEnrichment.html"

    PROVIDERS: Dict[str, Tuple[str, str]] = {}

    PACKAGE: str = ""

    def __init__(self, providers: Optional[List[str]] = None, **kwargs):
        """
        Initialize TILookup instance.

        Parameters
        ----------
        primary_providers : Optional[List[Provider]], optional
            Primary Providers, by default None
        secondary_providers : Optional[List[Provider]], optional
            Secondary Providers, by default None
        providers: Optional[List[str]], optional
            List of provider names to load, by default all available
            providers are loaded. To see the list of available providers
            call `TILookup.list_available_providers()`.
            Note: if primary_provides or secondary_providers is specified
            This will override the providers list.

        """
        self._providers: Dict[str, Provider] = {}
        self._secondary_providers: Dict[str, Provider] = {}
        self._providers_to_load = providers

        primary_providers = kwargs.pop("primary_providers", None)
        if primary_providers:
            for prov in primary_providers:
                self.add_provider(prov, primary=True)
        secondary_providers = kwargs.pop("secondary_providers", None)
        if secondary_providers:
            warnings.warn(
                "'secondary_providers' is a deprecated parameter", DeprecationWarning
            )
            for prov in secondary_providers:
                self.add_provider(prov, primary=False)
        if not (primary_providers or secondary_providers):
            self._load_providers()

        self._all_providers = ChainMap(self._secondary_providers, self._providers)
        if is_ipython():
            nest_asyncio.apply()

    @property
    def loaded_providers(self) -> Dict[str, Provider]:
        """
        Return dictionary of loaded providers.

        Returns
        -------
        Dict[str, TIProvider]
            [description]

        """
        return dict(self._all_providers)

    @property
    def provider_status(self) -> Iterable[str]:
        """
        Return loaded provider status.

        Returns
        -------
        Iterable[str]
            List of providers and descriptions.

        """
        prim = [
            f"{prov_name} - {prov.description} (primary)"
            for prov_name, prov in self._providers.items()
        ]
        sec = [
            f"{prov_name} - {prov.description} (secondary)"
            for prov_name, prov in self._secondary_providers.items()
        ]
        return prim + sec

    @property
    def configured_providers(self) -> List[str]:
        """
        Return a list of available providers that have configuration details present.

        Returns
        -------
        List[str]
            List of TI Provider classes.

        """
        prim_conf = list(self._providers.keys())
        sec_conf = list(self._secondary_providers.keys())

        return prim_conf + sec_conf

    def enable_provider(self, providers: Union[str, Iterable[str]]):
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
        provs_to_enable = [providers] if isinstance(providers, str) else providers
        for provider in provs_to_enable:
            if provider in self._secondary_providers:
                self._providers[provider] = self._secondary_providers[provider]
                del self._secondary_providers[provider]
            elif provider not in self._providers:
                raise ValueError(
                    f"Unknown provider '{provider}'. Available providers:",
                    ", ".join(self.list_available_providers(as_list=True)),  # type: ignore
                )

    def disable_provider(self, providers: Union[str, Iterable[str]]):
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
        provs_to_disable = [providers] if isinstance(providers, str) else providers
        for provider in provs_to_disable:
            if provider in self._providers:
                self._secondary_providers[provider] = self._providers[provider]
                del self._providers[provider]
            elif provider not in self._secondary_providers:
                raise ValueError(
                    f"Unknown provider '{provider}'. Available providers:",
                    ", ".join(self.list_available_providers(as_list=True)),  # type: ignore
                )

    def set_provider_state(self, prov_dict: Dict[str, bool]):
        """
        Set a dict of providers to primary/secondary.

        Parameters
        ----------
        prov_dict : Dict[str, bool]
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
        cls, data: pd.DataFrame, severities: Optional[List[str]] = None, **kwargs
    ):
        """
        Return TI Results list browser.

        Parameters
        ----------
        data : pd.DataFrame
            TI Results data from TIProviders
        severities : Optional[List[str]], optional
            A list of the severity classes to show.
            By default these are ['warning', 'high'].
            Pass ['information', 'warning', 'high'] to see all
            results.

        Other Parameters
        ----------------
        kwargs :
            passed to SelectItem constructor.

        Returns
        -------
        SelectItem
            SelectItem browser for TI Data.

        """
        if not isinstance(data, pd.DataFrame):
            print("Input data is in an unexpected format.")
            return None
        return browse_results(data=data, severities=severities, **kwargs)

    browse = browse_results

    def provider_usage(self):
        """Print usage of loaded providers."""
        print("Primary providers")
        print("-----------------")
        if self._providers:
            for prov_name, prov in self._providers.items():
                print(f"\nProvider class: {prov_name}")
                prov.usage()
        else:
            print("none")
        print("\nSecondary providers")
        print("-------------------")
        if self._secondary_providers:
            for prov_name, prov in self._secondary_providers.items():
                print(f"\nProvider class: {prov_name}")
                prov.usage()
        else:
            print("none")

    @classmethod
    def reload_provider_settings(cls):
        """Reload provider settings from config."""
        reload_settings()
        print(
            "Settings reloaded. Use reload_providers to update settings",
            "for loaded providers.",
        )

    def reload_providers(self):
        """Reload settings and provider classes."""
        reload_settings()
        self._load_providers()

    def add_provider(self, provider: Provider, name: str = None, primary: bool = True):
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
            name = provider.__class__.__name__
        if primary:
            self._providers[name] = provider
        else:
            self._secondary_providers[name] = provider

    # pylint: disable=too-many-locals
    # pylint: disable=too-many-arguments
    def lookup_item(
        self,
        item: str,
        item_type: str = None,
        query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
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
        return self.lookup_items(
            data={item: item_type},
            query_type=query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            **kwargs,
        )

    def lookup_items(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        item_col: str = None,
        item_type_col: str = None,
        query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
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
        providers: List[str]
            Explicit list of providers to use
        default_providers: Optional[List[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"

        Other Parameters
        ----------------
        progress : bool
            Use progress bar to track completion, by default True
        kwargs :
            Additional arguments passed to the underlying provider(s)

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
                **kwargs,
            )
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
        if isinstance(item_lookup, pd.DataFrame):
            return item_lookup
        raise ValueError(f"DataFrame was expected, but {type(item_lookup)} received.")

    # pylint: disable=too-many-locals
    async def _lookup_items_async(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        item_col: str = None,
        item_type_col: str = None,
        query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
    ) -> pd.DataFrame:
        """Lookup items async."""
        item_col = item_col or kwargs.pop("col", kwargs.pop("column", None))
        progress = kwargs.pop("progress", True)
        selected_providers = self._select_providers(
            providers or default_providers, prov_scope
        )
        if not selected_providers:
            raise MsticpyUserConfigError(
                self._NO_PROVIDERS_MSG,
                title="No Threat Intel Provider configuration found.",
                help_uri=self._HELP_URI,
            )

        event_loop = asyncio.get_event_loop()
        result_futures: List[Any] = []
        provider_names: List[str] = []

        prog_counter = ProgressCounter(
            total=len(data) * len(selected_providers)  # type: ignore
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
                    **kwargs,
                )
            )
        if progress:
            # Create a task for tqdm
            prog_task = event_loop.create_task(self._track_completion(prog_counter))
        # collect the return values of the tasks
        results = await asyncio.gather(*result_futures)
        # cancel the progress task if results have completed.
        prog_task.cancel()
        return self._combine_results(results, provider_names, **kwargs)

    def lookup_items_sync(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        item_col: str = None,
        item_type_col: str = None,
        query_type: str = None,
        providers: List[str] = None,
        default_providers: Optional[List[str]] = None,
        prov_scope: str = "primary",
        **kwargs,
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
        item_col = item_col or kwargs.pop("col", kwargs.pop("column", None))

        selected_providers = self._select_providers(
            providers or default_providers, prov_scope
        )
        if not selected_providers:
            raise MsticpyUserConfigError(
                self._NO_PROVIDERS_MSG,
                title="No Threat Intel Provider configuration found.",
                help_uri=self._HELP_URI,
            )

        results: List[Any] = []
        provider_names: List[str] = []
        for prov_name, provider in selected_providers.items():
            provider_names.append(prov_name)
            results.append(
                provider.lookup_items(
                    data=data,
                    item_col=item_col,
                    item_type_col=item_type_col,
                    query_type=query_type,
                    **kwargs,
                )
            )
        return self._combine_results(results, provider_names, **kwargs)

    @staticmethod
    async def _track_completion(prog_counter):
        total = await prog_counter.get_remaining()
        with tqdm(total=total, unit="obs", desc="Observables processed") as prog_bar:
            try:
                last_remaining = total
                while last_remaining:
                    new_remaining = await prog_counter.get_remaining()
                    incr = last_remaining - new_remaining
                    if incr:
                        prog_bar.update(incr)
                    last_remaining = new_remaining
                    # print(f"progress: incr {incr}, last: {last_remaining}")
                    await asyncio.sleep(0)
            except asyncio.CancelledError:
                # make progress bar get to 100% on cancel
                final_remaining = await prog_counter.get_remaining()
                if final_remaining:
                    prog_bar.update(total - final_remaining)

    @property
    def available_providers(self) -> List[str]:
        """
        Return a list of builtin providers.

        Returns
        -------
        List[str]
            List of TI Provider classes.

        """
        return list(self.PROVIDERS)

    @classmethod
    def list_available_providers(
        cls, show_query_types=False, as_list: bool = False
    ) -> Optional[List[str]]:  # type: ignore
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
        Optional[List[str]]
            A list of provider names (if `return_list=True`)

        """
        providers = []
        for provider_name in cls.PROVIDERS:
            provider_class = cls.import_provider(provider_name)
            if not as_list:
                print(provider_name)
            providers.append(provider_name)
            if show_query_types and provider_class:
                provider_class.usage()

        if as_list:
            return providers
        return None

    @classmethod
    def import_provider(cls, provider: str) -> Provider:
        """Import provider class."""
        mod_name, cls_name = cls.PROVIDERS.get(provider, (None, None))

        if not (mod_name and cls_name):
            raise LookupError(
                f"No driver available for environment {provider}.",
                "Possible values are:",
                ", ".join(list(cls.PROVIDERS)),
            )

        imp_module = importlib.import_module(
            f"msticpy.context.{cls.PACKAGE}.{mod_name}", package="msticpy"
        )
        return getattr(imp_module, cls_name)

    def _load_providers(self, **kwargs):
        """Load provider classes based on config."""
        prov_type = kwargs.get("providers", "Providers")
        prov_settings = get_provider_settings(prov_type)

        for provider_entry, settings in prov_settings.items():
            # Allow overriding provider name to use another class
            provider_name = settings.provider or provider_entry
            if (
                self._providers_to_load is not None
                and provider_name not in self._providers_to_load
            ) or provider_name == "--no-load--":
                continue
            try:
                provider_class: Provider = self.import_provider(provider_name)
            except LookupError:
                warnings.warn(
                    f"Could not find provider class for {provider_name} "
                    f"in config section {provider_entry}"
                )
                continue

            # instantiate class sending args from settings to init
            try:
                provider_instance = provider_class(**(settings.args))
            except MsticpyConfigError as mp_ex:
                # If the TI Provider didn't load, raise an exception
                raise MsticpyUserConfigError(
                    f"Could not load Provider {provider_name}",
                    *mp_ex.args,
                    "To avoid loading this provider please use the 'providers' parameter"
                    + " to specify which providers to load.",
                    title="Provider configuration error",
                    help_uri=self._HELP_URI,
                ) from mp_ex

            # set the description from settings, if one is provided, otherwise
            # use class docstring.
            provider_instance.description = (
                settings.description or provider_instance.__doc__
            )

            self.add_provider(
                provider=provider_instance, name=provider_name, primary=settings.primary
            )

    def _select_providers(
        self, providers: List[str] = None, prov_scope: str = "primary"
    ) -> Dict[str, Provider]:
        """
        Return required subset of providers.

        Parameters
        ----------
        providers : List[str], optional
            Explicit list of provider names, by default None
        prov_scope : str, optional
            Provider scope, by default "primary"
            Other values are "all" and "secondary"

        Returns
        -------
        Dict[str, TIProvider]
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
        results: Iterable[pd.DataFrame], provider_names: List[str], **kwargs
    ):
        """Combine dataframe results into single DF."""
        result_list: List[pd.DataFrame] = []
        for prov_name, provider_result in zip(provider_names, results):
            if provider_result is None or provider_result.empty:
                continue
            if not kwargs.get("show_not_supported", False):
                provider_result = provider_result[
                    provider_result["Status"] != LookupStatus.NOT_SUPPORTED.value
                ]
            if not kwargs.get("show_bad_item", False):
                provider_result = provider_result[
                    provider_result["Status"] != LookupStatus.BAD_FORMAT.value
                ]
            provider_result["Provider"] = prov_name
            result_list.append(provider_result)

        if not result_list:
            print("No Item matches")
        return pd.concat(result_list, sort=False) if result_list else None
