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
from __future__ import annotations

from typing import TYPE_CHECKING, ClassVar, Iterable, Mapping

from typing_extensions import Self

from .._version import VERSION
from ..common.utility import export
from .lookup import Lookup

# used in dynamic instantiation of providers
from .provider_base import Provider, _make_sync
from .tiproviders import TI_PROVIDERS

if TYPE_CHECKING:
    import datetime

    import pandas as pd

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class TILookup(Lookup):
    """Threat Intel observable lookup from providers."""

    _NO_PROVIDERS_MSG: ClassVar[
        str
    ] = """
    No TI Providers are loaded - please check that
    you have correctly configured your msticpyconfig.yaml settings.
    """
    _HELP_URI: ClassVar[str] = (
        "https://msticpy.readthedocs.io/en/latest/data_acquisition/"
        "TIProviders.html#configuration-file"
    )

    PROVIDERS: ClassVar[dict[str, tuple[str, str]]] = TI_PROVIDERS
    PACKAGE: ClassVar[str] = "tiproviders"
    CUSTOM_PROVIDERS: ClassVar[dict[str, type[Provider]]] = {}

    def lookup_ioc(  # pylint: disable=too-many-arguments #noqa: PLR0913
        self: Self,
        ioc: str | None = None,
        ioc_type: str | None = None,
        ioc_query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        observable: str | None = None,
        show_not_supported: bool = False,
        start: datetime.datetime | None = None,
        end: datetime.datetime | None = None,
    ) -> pd.DataFrame:
        """
        Lookup Threat Intelligence reports for a single IoC in active providers.

        Parameters
        ----------
        ioc : str
            IoC observable
        observable : str
            alias for `ioc`
        ioc_type : str, optional
            One of IoCExtract.IoCType, by default None
            If none, the IoC type will be inferred
        ioc_query_type: str, optional
            The ioc query type (e.g. rep, info, malware)
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        show_not_supported: boolean, optional
            If True, display result even if provider does not support this type of IOC.
        start: dt.datetime, optional
            Time since when IOC is considered relevant
        end: dt.datetime, optional
            Time until when IOC is considered relevant

        Returns
        -------
        pd.DataFrame
            The result returned as a tuple(bool, list):
            bool indicates whether a TI record was found in any provider
            list has an entry for each provider result

        See Also
        --------
        lookup_iocs : Lookup Threat Intelligence reports for a collection of IoCs.

        Notes
        -----
        Queries active Threat Intelligence (TI) providers for a single
        indicator of compromise (IoC). It returns results as a pandas
        DataFrame. `ioc_type` can be used to specify the type (ipv4,
        ipv6, dns, url, file_hash). If this is not supplied the
        type is inferred using regular expressions.
        By default, providers are queried asynchronously, in parallel.

        """
        ioc = ioc or observable
        if ioc is None:
            err_msg: str = "No value supplied for 'ioc' parameter"
            raise ValueError(err_msg)
        return self.lookup_item(
            item=ioc,
            item_type=ioc_type,
            query_type=ioc_query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            show_not_supported=show_not_supported,
            start=start,
            end=end,
        )

    def lookup_iocs(  # pylint: disable=too-many-arguments #noqa: PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        ioc_col: str | None = None,
        ioc_type_col: str | None = None,
        ioc_query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        prov_scope: str = "primary",
        *,
        progress: bool = True,
        start: datetime.datetime | None = None,
        end: datetime.datetime | None = None,
        col: str | None = None,
        column: str | None = None,
    ) -> pd.DataFrame:
        """
        Lookup Threat Intelligence reports for a collection of IoCs in active providers.

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
        providers: list[str]
            Explicit list of providers to use
        default_providers: Optional[list[str]], optional
            Used by pivot functions as a fallback to `providers`. If
            `providers` is specified, it will override this parameter.
        prov_scope : str, optional
            Use "primary", "secondary" or "all" providers, by default "primary"
        start: dt.datetime, optional
            Time since when IOC is considered relevant
        end: dt.datetime, optional
            Time until when IOC is considered relevant
        progress : bool
            Use progress bar to track completion, by default True

        Returns
        -------
        pd.DataFrame
            DataFrame of results

        See Also
        --------
        lookup_ioc : Lookup Threat Intelligence reports for a single IoC.

        Notes
        -----
        `lookup_iocs` queries active Threat Intelligence (TI) providers for
        threat reports. It can accept input as a Python iterable or
        a pandas dataframe. In the latter case, you also need to supply
        the `ioc_col` parameter to indicate which column the IoC value can
        be found. The `ioc_type_col` parameter is optional and can be used
        to manually specify the IoC type for each row. If this is not supplied
        the ioc types are inferred using regular expressions.
        The results are returned as a pandas DataFrame.

        """
        return _make_sync(
            self._lookup_iocs_async(
                data=data,
                ioc_col=ioc_col or column or col,
                ioc_type_col=ioc_type_col,
                ioc_query_type=ioc_query_type,
                providers=providers,
                default_providers=default_providers,
                prov_scope=prov_scope,
                start=start,
                end=end,
                progress=progress,
            ),
        )

    async def _lookup_iocs_async(  # pylint: disable=too-many-arguments #noqa:PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        ioc_col: str | None = None,
        ioc_type_col: str | None = None,
        ioc_query_type: str | None = None,
        providers: list[str] | None = None,
        default_providers: list[str] | None = None,
        *,
        progress: bool = True,
        start: datetime.datetime | None = None,
        end: datetime.datetime | None = None,
        prov_scope: str = "primary",
    ) -> pd.DataFrame:
        """Lookup IoCs async."""
        return await self._lookup_items_async(
            data,  # type: ignore[arg-type]
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            query_type=ioc_query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
            start=start,
            end=end,
            progress=progress,
        )

    def lookup_iocs_sync(  # pylint:disable=too-many-arguments # noqa: PLR0913
        self: Self,
        data: pd.DataFrame | Mapping[str, str] | Iterable[str],
        ioc_col: str | None = None,
        ioc_type_col: str | None = None,
        ioc_query_type: str | None = None,
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
            item_col=ioc_col,
            item_type_col=ioc_type_col,
            query_type=ioc_query_type,
            providers=providers,
            default_providers=default_providers,
            prov_scope=prov_scope,
        )

    def _load_providers(
        self: Self,
        *,
        providers: str = "TIProviders",
    ) -> None:
        """Load provider classes based on config."""
        return super()._load_providers(providers=providers)
