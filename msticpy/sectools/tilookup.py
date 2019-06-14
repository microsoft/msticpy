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
from typing import List, Mapping, Dict, Tuple, Union, Iterable, Optional

import pandas as pd

from .tiproviders.ti_provider import TIProvider
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
                self.add_provider(prov, prov_type="primary")
        if secondary_providers:
            for prov in secondary_providers:
                self.add_provider(prov, prov_type="secondary")

    def add_provider(self, provider: TIProvider, prov_type: str = "primary"):
        """
        Add a TI provider to the current collection.

        Parameters
        ----------
        provider : TIProvider
            Provider instance
        prov_type : str, optional
            "primary" or "secondary", by default "primary"

        """
        if prov_type == "primary":
            self._providers[provider.__name__] = TIProvider
        else:
            self._secondary_providers[provider.__name__] = TIProvider

    def lookup_ioc(
        self, observable: str, ioc_type: str = None, primary: bool = True
    ) -> Tuple[bool, List[Tuple[str, bool, str]]]:
        """
        Lookup single IoC in active providers.

        Parameters
        ----------
        observable : str
            IoC observable
        ioc_type : str, optional
            One of IoCExtract.IoCType, by default None
            If none, the IoC type will be inferred
        primary : bool, optional
            Use primary providers, by default True
            If False, use secondary providers

        Returns
        -------
        Tuple[bool, List[Tuple[str, bool, str]]]
            The result returned as a tuple(bool, list):
            bool indicates whether a TI record was found in any provider
            list has an entry for each provider result

        """
        result_list: List[Tuple[str, bool, str]] = []
        providers = self._providers if primary else self._secondary_providers
        for provider in providers:
            provider_result = provider.lookup_ioc(observable, ioc_type)
            result_list.extend((provider.__name__, *provider_result))
        overall_result = any(res[0] for res in result_list)
        return overall_result, result_list

    def lookup_iocs(
        self,
        data: Union[pd.DataFrame, Mapping[str, str], Iterable[str]],
        obs_col: str = None,
        ioc_type_col: str = None,
        primary: bool = True,
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
        primary : bool, optional
            Use primary providers, by default True
            If False, use secondary providers

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        """
        result_list: List[pd.DataFrame] = []
        providers = self._providers if primary else self._secondary_providers
        for provider in providers:
            provider_result = provider.lookup_iocs(data, obs_col, ioc_type_col)
            result_list.extend(provider_result)

        return pd.concat(result_list)
