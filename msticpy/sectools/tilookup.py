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
from collections import ChainMap
import sys
from typing import List, Mapping, Dict, Tuple, Union, Iterable, Optional
import warnings

import attr
import pandas as pd

# used in dynamic instantiation of providers
# pylint: disable=unused-wildcard-import, wildcard-import

from .tiproviders import *  # noqa:F401, F403
from .tiproviders.ti_provider_base import TIProvider, LookupResult
from .tiproviders.ti_provider_settings import get_provider_settings, reload_settings
from ..nbtools.utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class TILookup:
    """Threat Intel observable lookup from providers."""

    def __init__(
        self,
        primary_providers: Optional[List[TIProvider]] = None,
        secondary_providers: Optional[List[TIProvider]] = None,
    ):
        """
        Initialize TILookup instance.

        Parameters
        ----------
        primary_providers : Optional[List[TIProvider]], optional
            Primary TI Providers, by default None
        secondary_providers : Optional[List[TIProvider]], optional
            Secondary TI Providers, by default None

        """
        self._providers: Dict[str, TIProvider] = {}
        self._secondary_providers: Dict[str, TIProvider] = {}

        if primary_providers:
            for prov in primary_providers:
                self.add_provider(prov, primary=True)
        if secondary_providers:
            for prov in secondary_providers:
                self.add_provider(prov, primary=False)
        if not primary_providers and not secondary_providers:
            self._load_providers()

        self._all_providers = ChainMap(self._secondary_providers, self._providers)

    @property
    def loaded_providers(self) -> Dict[str, TIProvider]:
        """
        Return dictionary of loaded providers.

        Returns
        -------
        Dict[str, TIProvider]
            [description]

        """
        return self._all_providers

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

    def reload_providers(self):
        """Reload providers based on currret settings in config."""
        self.reload_provider_settings()
        self._load_providers()

    def _load_providers(self):
        """Load provider classes based on config."""
        prov_settings = get_provider_settings()

        for provider_entry, settings in prov_settings.items():
            # Allow overriding provider name to use another class
            provider_name = settings.provider if settings.provider else provider_entry
            provider_class: TIProvider = getattr(
                sys.modules[__name__], provider_name, None
            )
            if not provider_class:
                warnings.warn(
                    f"Could not find provider class for {provider_name} "
                    + f"in config section {provider_entry}"
                )
                continue

            # instantiate class sending args from settings to init
            provider_instance = provider_class(**(settings.args))
            # set the description from settings, if one is provided, otherwise
            # use class docstring.
            provider_instance.description = (
                settings.description
                if settings.description
                else provider_instance.__doc__
            )
            self.add_provider(
                provider=provider_instance, name=provider_name, primary=settings.primary
            )

    def add_provider(
        self, provider: TIProvider, name: str = None, primary: bool = True
    ):
        """
        Add a TI provider to the current collection.

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
            name = provider.__name__
        if primary:
            self._providers[name] = provider
        else:
            self._secondary_providers[name] = provider

    # pylint: disable=too-many-arguments
    def lookup_ioc(
        self,
        observable: str,
        ioc_type: str = None,
        ioc_query_type: str = None,
        providers: List[str] = None,
        prov_scope: str = "primary",
    ) -> Tuple[bool, List[LookupResult]]:
        """
        Lookup single IoC in active providers.

        Parameters
        ----------
        observable : str
            IoC observable
        ioc_type : str, optional
            One of IoCExtract.IoCType, by default None
            If none, the IoC type will be inferred
        ioc_query_type: str, optional
            The ioc query type (e.g. rep, info, malware)
        providers: List[str]
            Explicit list of providers to use
        prov_scope : str, optional
            Use primary, secondary or all providers, by default "primary"

        Returns
        -------
        Tuple[bool, List[LookupResult]]
            The result returned as a tuple(bool, list):
            bool indicates whether a TI record was found in any provider
            list has an entry for each provider result

        """
        result_list: List[Tuple[str, LookupResult]] = []
        selected_providers = self._select_providers(providers, prov_scope)

        for prov_name, provider in selected_providers.items():
            provider_result: LookupResult = provider.lookup_ioc(
                ioc=observable, ioc_type=ioc_type, query_type=ioc_query_type
            )
            result_list.append((prov_name, provider_result))
        overall_result = any(res.result for _, res in result_list)
        return overall_result, result_list

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
        ioc_query_type: str = None,
        providers: List[str] = None,
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
        ioc_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        ioc_query_type: str, optional
            The ioc query type (e.g. rep, info, malware)
        providers: List[str]
            Explicit list of providers to use
        prov_scope : str, optional
            Use primary, secondary or all providers, by default "primary"

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        result_list: List[pd.DataFrame] = []
        selected_providers = self._select_providers(providers, prov_scope)
        for prov_name, provider in selected_providers.items():
            provider_result = provider.lookup_iocs(
                data=data,
                obs_col=obs_col,
                ioc_type_col=ioc_type_col,
                query_type=ioc_query_type,
            )
            provider_result["Provider"] = prov_name
            result_list.extend(provider_result)

        return pd.concat(result_list)

    @staticmethod
    def result_to_df(ioc_lookup: Tuple[bool, List[LookupResult]]) -> pd.DataFrame:
        """
        Return DataFrame representation of IoC Lookup response.

        Parameters
        ----------
        ioc_lookup : Tuple[bool, List[LookupResult]]
            Output from `lookup_ioc`

        Returns
        -------
        pd.DataFrame
            The response as a DataFrame with a row for each
            provider response.

        """
        return pd.DataFrame(
            {r_item[0]: pd.Series(attr.asdict(r_item[1])) for r_item in ioc_lookup[1]}
        ).T

    def _select_providers(
        self, providers: List[str] = None, prov_scope: str = "primary"
    ) -> Dict[str, TIProvider]:
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
            selected_providers = {
                prov_name: prov
                for prov_name, prov in self._all_providers.items()
                if prov_name in providers
            }
        else:
            if prov_scope == "all":
                selected_providers = self._all_providers
            elif prov_scope == "primary":
                selected_providers = self._providers
            else:
                selected_providers = self._secondary_providers
        return selected_providers
