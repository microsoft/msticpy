# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Kql TI Provider base.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

import abc
import contextlib
import logging
import warnings
from collections import defaultdict
from functools import lru_cache
from typing import TYPE_CHECKING, Any, Callable, ClassVar, Iterable

import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyConfigError
from ...common.utility import export
from ...common.wsconfig import WorkspaceConfig
from ...data.core.data_providers import QueryProvider
from ..lookup_result import LookupStatus
from ..provider_base import generate_items
from .ti_provider_base import ResultSeverity, TIProvider

if TYPE_CHECKING:
    import datetime as dt

    from Kqlmagic.results import ResultSet
logger: logging.Logger = logging.getLogger(__name__)
__version__ = VERSION
__author__ = "Ian Hellen"

logger = logging.getLogger(__name__)


@export
class KqlTIProvider(TIProvider):
    """KQL TI provider base class."""

    _QUERIES: ClassVar[dict[str, tuple]] = {}

    _CONNECT_STR: ClassVar[str] = (
        "loganalytics://code().tenant('{TENANT_ID}').workspace('{WORKSPACE_ID}')"
    )

    _REQUIRED_TABLES: ClassVar[list[str]] = []

    def __init__(
        self: KqlTIProvider,
        query_provider: QueryProvider | None = None,
        connect_str: str | None = None,
        **kwargs: str,
    ) -> None:
        """Initialize a new instance of the class."""
        super().__init__()

        if isinstance(
            query_provider,
            QueryProvider,
        ):
            self._query_provider: QueryProvider = query_provider
            self._connect_str: str = connect_str or WorkspaceConfig().code_connect_str
        else:
            self._query_provider, self._connect_str = self._create_query_provider(
                **kwargs,
            )

        if not self._query_provider:
            err_msg: str = "Query provider for KQL could not be created."
            raise MsticpyConfigError(err_msg)

    @property
    def _connected(self: Self) -> bool:
        return self._query_provider.connected

    @lru_cache(maxsize=256)
    def lookup_ioc(
        self: Self,
        ioc: str,
        ioc_type: str | None = None,
        query_type: str | None = None,
    ) -> pd.DataFrame:
        """
        Lookup from a value.

        Parameters
        ----------
        ioc : str
            item to lookup
        ioc_type : str, optional
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
        return self.lookup_iocs(
            data={ioc: ioc_type},
            query_type=query_type,
        )

    # pylint: disable=too-many-locals
    def lookup_iocs(
        self: Self,
        data: pd.DataFrame | dict[str, str] | Iterable[str],
        ioc_col: str | None = None,
        ioc_type_col: str | None = None,
        query_type: str | None = None,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        ioc_col : str, optional
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
        if not self._connected:
            self._connect()
        if self._query_provider.schema and any(
            table not in self._query_provider.schema for table in self._REQUIRED_TABLES
        ):
            logger.error(
                "Required tables not found in schema: %s",
                self._REQUIRED_TABLES,
            )
            return pd.DataFrame()

        # We need to partition the IoC types to invoke separate queries
        ioc_groups: defaultdict[str, set[str]] = defaultdict(set)
        for ioc, ioc_type in generate_items(data, ioc_col, ioc_type_col):
            if not ioc:
                continue
            result = self._check_ioc_type(ioc, ioc_type, query_type)

            if result["Status"] != LookupStatus.NOT_SUPPORTED.value:
                logger.info(
                    "Check ioc type for %s (%s): %s",
                    ioc,
                    ioc_type,
                    result["Status"],
                )
                ioc_groups[result["IocType"]].add(result["Ioc"])

        all_results: list[pd.DataFrame] = []
        for ioc_type, obs_set in ioc_groups.items():
            query_obj = None
            with contextlib.suppress(LookupError):
                query_obj, query_params = self._get_query_and_params(
                    ioc=list(obs_set),
                    ioc_type=ioc_type,
                    query_type=query_type,
                )
            if not query_obj:
                logger.info("No query found for %s", ioc_type)
                warnings.warn(
                    f"Could not find query for {ioc_type}, {query_type}",
                    stacklevel=1,
                )
                continue

            # run the query
            logger.info("Running query for %s with params %s", ioc_type, query_params)
            data_result: pd.DataFrame = query_obj(**query_params)

            src_ioc_frame: pd.DataFrame = pd.DataFrame(obs_set, columns=["Ioc"])
            src_ioc_frame["IocType"] = ioc_type
            src_ioc_frame["QuerySubtype"] = query_type
            src_ioc_frame["Reference"] = query_obj("print_query", **query_params)

            lookup_status: LookupStatus = self._check_result_status(data_result)
            # If no results, add the empty dataframe to the combined results
            # and continue
            if lookup_status in {LookupStatus.QUERY_FAILED, LookupStatus.NO_DATA}:
                self._add_failure_status(src_ioc_frame, lookup_status)
                all_results.append(src_ioc_frame)
                continue

            # copy the DF to avoid pandas warnings about changing DF view
            data_result = data_result.copy()
            # Create our results columns
            data_result["Result"] = True
            data_result["Status"] = LookupStatus.OK.value
            data_result["Severity"] = self._get_severity(data_result)
            data_result["Details"] = self._get_detail_summary(data_result)
            data_result["RawResult"] = data_result.apply(lambda x: x.to_dict(), axis=1)

            combined_results_df: pd.DataFrame = self._combine_results(
                input_df=src_ioc_frame,
                results_df=data_result,
                result_ioc_key="IoC",
            )
            all_results.append(combined_results_df)

        if all_results:
            logger.info("Combining results from %d queries", len(all_results))
            return pd.concat(all_results, ignore_index=True, sort=False, axis=0)
        logger.info("No results found in data for any iocs.")
        return pd.DataFrame()

    @staticmethod
    def _add_failure_status(
        src_ioc_frame: pd.DataFrame,
        lookup_status: LookupStatus,
    ) -> None:
        """Add status info, if query produced no results."""
        src_ioc_frame["Result"] = False
        src_ioc_frame["Details"] = (
            "Query failure"
            if lookup_status == LookupStatus.QUERY_FAILED
            else "Not found"
        )
        src_ioc_frame["Status"] = lookup_status.value
        src_ioc_frame["Severity"] = ResultSeverity.information.name

    @staticmethod
    def _check_result_status(data_result: pd.DataFrame | ResultSet) -> LookupStatus:
        """Check the return value from the query."""
        if isinstance(data_result, pd.DataFrame):
            return LookupStatus.NO_DATA if data_result.empty else LookupStatus.OK
        if (
            hasattr(data_result, "completion_query_info")
            and data_result.completion_query_info["StatusCode"] == 0
            and data_result.records_count == 0
        ):
            logger.info("No results return from data provider.")
            return LookupStatus.NO_DATA
        if data_result and hasattr(data_result, "completion_query_info"):
            logger.info(
                "No results returned from data provider. %s",
                data_result.completion_query_info,
            )
        else:
            logger.info("Unknown response from provider: %s", data_result)
        return LookupStatus.QUERY_FAILED

    @abc.abstractmethod
    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """

    @staticmethod
    @abc.abstractmethod
    def _get_detail_summary(data_result: pd.DataFrame) -> pd.Series:
        pass

    @staticmethod
    @abc.abstractmethod
    def _get_severity(data_result: pd.DataFrame) -> pd.Series:
        pass

    def _create_query_provider(self: Self, **kwargs: str) -> tuple[QueryProvider, str]:
        workspace_id: str | None = self._get_spelled_variants("workspaceid", **kwargs)
        tenant_id: str | None = self._get_spelled_variants("tenantid", **kwargs)

        if not workspace_id or not tenant_id:
            # If there are no TI-Provider specific kwargs
            # WorkspaceConfig should be able to get these global values
            # If a config file or a workspace name is passed we'll use
            # those in case there are multiple workspaces set globally.
            config_file: str | None = kwargs.get("config_file")
            workspace: str | None = kwargs.get("workspace")
            ws_config: WorkspaceConfig = WorkspaceConfig(
                config_file=config_file,
                workspace=workspace,
            )
            workspace_id = ws_config["workspace_id"]
            tenant_id = ws_config["tenant_id"]
        # Either the format or connect() call will fail if these values are
        # not set or invalid.
        connect_str: str = self._CONNECT_STR.format(
            TENANT_ID=tenant_id,
            WORKSPACE_ID=workspace_id,
        )
        query_provider: QueryProvider = QueryProvider("LogAnalytics")
        logging.info("Connection string: %s", connect_str)
        return query_provider, connect_str

    def _connect(self: Self) -> None:
        """Connect to query provider."""
        logger.info("MS Sentinel TI query provider needs authenticated connection.")
        self._query_provider.connect(self._connect_str)
        logging.info("Connected to Sentinel. (%s)", self._connect_str)

    @staticmethod
    def _get_spelled_variants(name: str, **kwargs: str) -> str | None:
        """Return value with matching variant spelling key."""
        variant_dict: dict[str, list[str]] = {
            "workspaceid": ["workspace_id", "workspaceid"],
            "tenantid": ["tenant_id", "tenantid"],
        }
        variants: list[str] = variant_dict.get(name, [name.casefold()])
        return next(
            (val for key, val in kwargs.items() if key.casefold() in variants),
            None,
        )

    def _get_query_and_params(  # noqa:PLR0913
        self: Self,
        ioc: str | list[str],
        ioc_type: str,
        query_type: str | None = None,
        *,
        start: dt.datetime | None = None,
        end: dt.datetime | None = None,
    ) -> tuple[Callable, dict[str, Any]]:
        ioc_key: str = f"{ioc_type}-{query_type}" if query_type else ioc_type
        query_def: tuple | None = self._QUERIES.get(ioc_key, None)
        if not query_def:
            err_msg: str = f"Provider does not support IoC type {ioc_key}."
            raise LookupError(err_msg)

        query_name: str = query_def[0]
        query_def_params: dict[str, str] = query_def[1]
        if "ioc" not in query_def_params:
            err_msg = (
                f"No parameter name defined for observable for {ioc_type}. "
                f"Referenced query: {query_name}"
            )
            raise ValueError(err_msg)

        query_params: dict[str, Any] = {query_def_params["ioc"]: ioc}
        if start:
            query_params["start"] = start
        if end:
            query_params["end"] = end

        query_obj: Callable | None = getattr(self._query_provider, query_name, None)
        if not query_obj:
            err_msg = f"No query object name for {query_name} found in provider."
            raise ValueError(err_msg)

        return query_obj, query_params

    @staticmethod
    def _series_to_list(series: pd.Series) -> list[Any]:
        return list(series.dropna().unique())

    @staticmethod
    def _combine_results(
        input_df: pd.DataFrame,
        results_df: pd.DataFrame,
        result_ioc_key: str,
    ) -> pd.DataFrame:
        # Clean out unwanted columns from the results and merge with
        # the original IoCList
        # If we have results, we need to create our summary columns
        # merge the results with our original IoC set
        # and drop all of the columns that we are not interested in
        columns_to_drop: set[str] = set(results_df.columns.to_list())
        colums_to_keep: set[str] = {
            result_ioc_key,
            "Result",
            "Status",
            "Severity",
            "Details",
            "RawResult",
        }
        columns_to_drop = columns_to_drop - colums_to_keep

        cleaned_results_df: pd.DataFrame = results_df.copy().drop(
            columns=list(columns_to_drop),
        )
        combined_df: pd.DataFrame = input_df.copy()
        combined_df["IoCKey"] = input_df["Ioc"].str.lower()
        cleaned_results_df = cleaned_results_df.rename(
            columns={result_ioc_key: "IoCKey"},
        )
        combined_df = combined_df.merge(
            right=cleaned_results_df,
            how="left",
            on="IoCKey",
        ).drop(columns="IoCKey")
        # Fill in any NaN values from the merge
        combined_df["Result"] = combined_df["Result"].fillna(value=False)
        combined_df["Details"] = combined_df["Details"].fillna("Not found.")
        combined_df["Status"] = combined_df["Status"].fillna(LookupStatus.OK.value)
        combined_df["Severity"] = combined_df["Severity"].fillna(
            ResultSeverity.information.value,
        )
        return combined_df
