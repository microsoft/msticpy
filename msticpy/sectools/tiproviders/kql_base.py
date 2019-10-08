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
import abc
from collections import defaultdict
from functools import lru_cache
from typing import Any, Dict, Tuple, Union, Iterable, DefaultDict, Set, List, Callable
import warnings

import pandas as pd

from ..._version import VERSION
from ...nbtools.utility import export
from ...nbtools.wsconfig import WorkspaceConfig
from ...data import QueryProvider
from .ti_provider_base import (
    LookupResult,
    TIProvider,
    generate_items,
    TISeverity,
    TILookupStatus,
)

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class KqlTIProvider(TIProvider):
    """KQL TI provider base class."""

    _IOC_QUERIES: Dict[str, tuple] = {}

    _CONNECT_STR = (
        "loganalytics://code().tenant('{TENANT_ID}').workspace('{WORKSPACE_ID}')"
    )

    def __init__(self, **kwargs):
        """Initialize a new instance of the class."""
        super().__init__(**kwargs)

        if "query_provider" in kwargs and isinstance(
            kwargs["query_provider"], QueryProvider
        ):
            self._query_provider = kwargs.pop("query_provider")
        else:
            self._query_provider = self._create_query_provider(**kwargs)

        if not self._query_provider:
            raise RuntimeError("Query provider for KQL could not be created.")

    @lru_cache(maxsize=256)
    def lookup_ioc(  # type: ignore
        self, ioc: str, ioc_type: str = None, query_type: str = None, **kwargs
    ) -> LookupResult:
        """
        Lookup a single IoC observable.

        Parameters
        ----------
        ioc : str
            IoC observable
        ioc_type : str, optional
            IocType, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        LookupResult
            The lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of IoC

        Raises
        ------
        LookupError
            If a query could not be found for the ioc_type.

        Notes
        -----
        Note: this method uses memoization (lru_cache) to cache results
        for a particular observable to try avoid repeated network calls for
        the same item.

        """
        # check and lookup (if needed) ioc_type
        result = self._check_ioc_type(
            ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
        )
        result.provider = kwargs.get("provider_name", self.__class__.__name__)
        if result.status:
            return result

        try:
            query_obj, query_params = self._get_query_and_params(
                ioc=ioc, ioc_type=result.ioc_type, query_type=query_type, **kwargs
            )
        except LookupError as err:
            result.details = err.args
            result.raw_result = type(err).__name__ + "\n" + str(err) + "\n"
            return result

        if not query_obj:
            raise LookupError(
                f"Could not find query for {ioc} ({ioc_type}, {query_type})"
            )
        data_result = query_obj(**query_params)
        if not isinstance(data_result, pd.DataFrame):
            result.status = TILookupStatus.query_failed.value
        if data_result.empty:
            result.details = "Not found."
            result.status = TILookupStatus.ok.value
            return result

        result.raw_result = data_result
        result.result, severity, result.details = self.parse_results(result)
        result.set_severity(severity)
        # Save the query that was used.
        result.reference = query_obj("query", **query_params)
        return result

    # pylint: disable=too-many-locals, too-many-branches
    def lookup_iocs(  # noqa: C901, MC0001
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
        # We need to partition the IoC types to invoke separate queries
        ioc_groups: DefaultDict[str, Set[str]] = defaultdict(set)
        for ioc, ioc_type in generate_items(data, obs_col, ioc_type_col):
            if not ioc:
                continue
            result = self._check_ioc_type(
                ioc=ioc, ioc_type=ioc_type, query_subtype=query_type
            )

            if result.status != TILookupStatus.not_supported.value:
                ioc_groups[result.ioc_type].add(result.ioc)

        all_results = []
        for ioc_type, obs_set in ioc_groups.items():
            try:
                query_obj, query_params = self._get_query_and_params(
                    ioc=list(obs_set),
                    ioc_type=ioc_type,
                    query_type=query_type,
                    **kwargs,
                )
            except LookupError:
                pass
            if not query_obj:
                warnings.warn(f"Could not find query for {ioc_type}, {query_type}")
                continue

            # run the query
            data_result = query_obj(**query_params)
            if isinstance(data_result, pd.DataFrame):
                data_result = data_result.copy()
            else:
                if (
                    hasattr(data_result, "completion_query_info")
                    and data_result.completion_query_info["StatusCode"] == 0
                    and data_result.records_count == 0
                ):
                    warnings.warn("No results return from data provider.")
                else:
                    warnings.warn(
                        "No results return from data provider. "
                        + str(data_result.completion_query_info)
                        if data_result
                        else "Unknown error"
                    )

            src_ioc_frame = pd.DataFrame(obs_set, columns=["Ioc"])
            src_ioc_frame["IocType"] = ioc_type
            src_ioc_frame["QuerySubtype"] = query_type
            src_ioc_frame["Reference"] = query_obj("query", **query_params)

            # If no results, add the empty dataframe to the combined results
            # and continue
            if not isinstance(data_result, pd.DataFrame):
                src_ioc_frame["Result"] = False
                src_ioc_frame["Details"] = "Query failure"
                src_ioc_frame["Status"] = TILookupStatus.query_failed.value
                src_ioc_frame["Severity"] = TISeverity.information.value
            elif data_result.empty:
                src_ioc_frame["Result"] = False
                src_ioc_frame["Details"] = "Not found."
                src_ioc_frame["Status"] = TILookupStatus.ok.value
                src_ioc_frame["Severity"] = TISeverity.information.value
                all_results.append(src_ioc_frame)
                continue

            # Create our results columns
            data_result["Result"] = True
            data_result["Status"] = TILookupStatus.ok.value
            data_result["Severity"] = self._get_severity(data_result)
            data_result["Details"] = self._get_detail_summary(data_result)
            data_result["RawResult"] = data_result.apply(lambda x: x.to_dict(), axis=1)

            combined_results_df = self._combine_results(
                input_df=src_ioc_frame, results_df=data_result, reslt_ioc_key="IoC"
            )
            all_results.append(combined_results_df)

        return pd.concat(all_results, ignore_index=True, sort=False, axis=0)

    @abc.abstractmethod
    def parse_results(self, response: LookupResult) -> Tuple[bool, TISeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, TISeverity, Any]
            bool = positive or negative hit
            TISeverity = enumeration of severity
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

    def _create_query_provider(self, **kwargs):
        workspace_id = None
        tenant_id = None
        if "workspace_id" in kwargs:
            workspace_id = kwargs.pop("workspace_id")
        if "tenant_id" in kwargs:
            tenant_id = kwargs.pop("tenant_id")

        if not workspace_id or not tenant_id:
            # If there are no TI-Provider specific kwargs
            # WorkspaceConfig should be able to get these global values
            # If a config file or a workspace name is passed we'll use
            # those in case there are multiple workspaces set globally.
            config_file = kwargs.get("config_file")
            workspace = kwargs.get("workspace")
            ws_config = WorkspaceConfig(config_file=config_file, workspace=workspace)
            workspace_id = ws_config["workspace_id"]
            tenant_id = ws_config["tenant_id"]
        # Either the format or connect() call will fail if these values are
        # not set or invalid.
        connect_str = self._CONNECT_STR.format(
            TENANT_ID=tenant_id, WORKSPACE_ID=workspace_id
        )
        query_provider = QueryProvider("LogAnalytics")
        query_provider.connect(connect_str)
        return query_provider

    # pylint: disable=too-many-branches
    def _get_query_and_params(
        self,
        ioc: Union[str, List[str]],
        ioc_type: str,
        query_type: str = None,
        **kwargs,
    ) -> Tuple[Callable, Dict[str, Any]]:

        if query_type:
            ioc_key = ioc_type + "-" + query_type
        else:
            ioc_key = ioc_type

        query_def = self._IOC_QUERIES.get(ioc_key, None)
        if not query_def:
            raise LookupError(f"Provider does not support IoC type {ioc_key}.")

        query_name = query_def[0]
        query_def_params = query_def[1]
        query_params = {}
        if "ioc" not in query_def_params:
            raise ValueError(
                f"No parameter name defined for observable for {ioc_type}. "
                + f"Referenced query: {query_name}"
            )

        query_params[query_def_params["ioc"]] = ioc
        if "start" in kwargs:
            query_params["start"] = kwargs["start"]
        if "end" in kwargs:
            query_params["end"] = kwargs["end"]

        query_obj = getattr(self._query_provider, query_name, None)
        if not query_obj:
            raise ValueError(
                f"No query object name for {query_name} found in provider."
            )

        return query_obj, query_params

    @staticmethod
    def _series_to_list(series: pd.Series) -> List[Any]:
        return list(series.dropna().unique())

    @staticmethod
    def _combine_results(
        input_df: pd.DataFrame, results_df: pd.DataFrame, reslt_ioc_key: str
    ) -> pd.DataFrame:
        # Clean out unwanted columns from the results and merge with
        # the original IoCList
        # If we have results, we need to create our summary columns
        # merge the results with our original IoC set
        # and drop all of the columns that we are not interested in
        columns_to_drop = set(results_df.columns.to_list())
        colums_to_keep = {
            reslt_ioc_key,
            "Result",
            "Status",
            "Severity",
            "Details",
            "RawResult",
        }
        columns_to_drop = columns_to_drop - colums_to_keep

        cleaned_results_df = results_df.copy().drop(columns=columns_to_drop)
        combined_df = input_df.copy()
        combined_df["IoCKey"] = input_df["Ioc"].str.lower()
        cleaned_results_df = cleaned_results_df.rename(
            columns={reslt_ioc_key: "IoCKey"}
        )
        combined_df = combined_df.merge(
            right=cleaned_results_df, how="left", on="IoCKey"
        ).drop(columns="IoCKey")
        # Fill in any NaN values from the merge
        combined_df["Result"] = combined_df["Result"].fillna(False)
        combined_df["Details"] = combined_df["Details"].fillna("Not found.")
        combined_df["Status"] = combined_df["Status"].fillna(TILookupStatus.ok.value)
        combined_df["Severity"] = combined_df["Severity"].fillna(
            TISeverity.information.value
        )
        return combined_df
