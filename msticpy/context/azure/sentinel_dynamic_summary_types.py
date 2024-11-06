# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Sentinel Dynamic Summary classes."""
from __future__ import annotations

import dataclasses
import json
import logging
import uuid
from datetime import datetime
from functools import singledispatchmethod
from typing import Any, Callable, ClassVar, Hashable, Iterable

import numpy as np
import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError

__version__ = VERSION
__author__ = "Ian Hellen"

_TACTICS: tuple[str, ...] = (
    "Reconnaissance",
    "ResourceDevelopment",
    "InitialAccess",
    "Execution",
    "Persistence",
    "PrivilegeEscalation",
    "DefenseEvasion",
    "CredentialAccess",
    "Discovery",
    "LateralMovement",
    "Collection",
    "Exfiltration",
    "CommandAndControl",
    "Impact",
)
_TACTICS_DICT: dict[str, str] = {tactic.casefold(): tactic for tactic in _TACTICS}

_CLS_TO_API_MAP: dict[str, str] = {
    "summary_id": "summaryId",
    "summary_name": "summaryName",
    "azure_tenant_id": "azureTenantId",
    "tenant_id": "tenantId",
    "summary_description": "summaryDescription",
    "relation_name": "relationName",
    "relation_id": "relationId",
    "search_key": "searchKey",
    "tactics": "tactics",
    "techniques": "techniques",
    "source_info": "sourceInfo",
    "event_time_utc": "eventTimeUTC",
    "observable_type": "observableType",
    "observable_value": "observableValue",
    "packed_content": "packedContent",
    "summary_items": "rawContent",
    "summary_item_id": "summaryItemId",
}
_API_TO_CLS_MAP: dict[str, str] = {val: key for key, val in _CLS_TO_API_MAP.items()}

logger: logging.Logger = logging.getLogger(__name__)


class FieldList:
    """Class to hold field names."""

    def __init__(self: FieldList, fieldnames: Iterable[str]) -> None:
        """Add fields to field mapping."""
        self.__dict__.update({field.upper(): field for field in fieldnames})

    def __repr__(self: Self) -> str:
        """Return list of field attributes and values."""
        field_names: str = "\n    ".join(
            f"{key}='{val}'" for key, val in vars(self).items()
        )
        return f"Fields:\n    {field_names}"


@dataclasses.dataclass
class DynamicSummaryItem:  # pylint:disable=too-many-instance-attributes
    """
    DynamicSummaryItem class.

    Parameters
    ----------
    summary_item_id: Optional[str]
        The ID of the item
    relation_name: Optional[str] = None
        The name of the summary item relation
    relation_id: Optional[str] = None
        The ID of the summary item relation
    search_key: Optional[str] = None
        Searchable key value for summary item
    tactics: Union[str, list[str], None] = None
        Relevant MITRE tactics for the summary item
    techniques: Union[str, list[str], None] = None
        Relevant MITRE techniques for the summary item
    event_time_utc: Optional[datetime] = None
        Event time for the summary item
    observable_type: Optional[str] = None
        Observable type of the summary item
    observable_value: Optional[str] = None
        Observable value of the summary item
    packed_content: Dict[str, Any]
        Dictionary of item details.

    """

    fields: ClassVar[FieldList]
    summary_item_id: str | None = None
    relation_name: str | None = None
    relation_id: str | None = None
    search_key: str | None = None
    tactics: list[str] | str = dataclasses.field(default_factory=list)
    techniques: str | list[str] = dataclasses.field(default_factory=list)
    event_time_utc: datetime | None = None
    observable_type: str | None = None
    observable_value: str | None = None
    packed_content: dict[Hashable, Any] = dataclasses.field(default_factory=dict)

    def __post_init__(self: Self) -> None:
        """Initialize item ID if was not set explicitly."""
        self.summary_item_id = self.summary_item_id or str(uuid.uuid4())
        if isinstance(self.tactics, str):
            self.tactics = [self.tactics]
        self.tactics = _match_tactics(self.tactics)
        if isinstance(self.techniques, str):
            self.techniques = [self.techniques]

    def to_api_dict(self: Self) -> dict[str, Any]:
        """Return attributes as a JSON-serializable dictionary."""
        return {
            _CLS_TO_API_MAP.get(name, name): _convert_data_types(value)
            for name, value in dataclasses.asdict(self).items()
            if value is not None
        }


# Add helper class attribute for field names.
DynamicSummaryItem.fields = FieldList(
    [field.name for field in dataclasses.fields(DynamicSummaryItem)],
)


class DynamicSummary:
    """Dynamic Summary class."""

    fields = FieldList(
        [
            "summary_id",
            "summary_name",
            "summary_description",
            "tenant_id",
            "relation_name",
            "relation_id",
            "search_key",
            "tactics",
            "techniques",
            "source_info",
            "summary_items",
        ],
    )

    def __init__(  # pylint:disable=too-many-arguments #noqa:PLR0913
        self: DynamicSummary,
        summary_id: str | None = None,
        summary_name: str | None = None,
        summary_description: str | None = None,
        tenant_id: str | None = None,
        azure_tenant_id: str | None = None,
        search_key: str | None = None,
        tactics: str | list[str] | None = None,
        techniques: str | list[str] | None = None,
        source_info: dict[str, Any] | None = None,
        summary_items: (
            pd.DataFrame | Iterable[DynamicSummaryItem] | list[dict[str, Any]] | None
        ) = None,
    ) -> None:
        """
        Initialize a DynamicSummary instance.

        Parameters
        ----------
        summary_id : str, optional
            The summary UUID, by default auto-generated UUID
        summary_name : str, optional
            Name of the dynamic summary instance, by default None
        summary_description : str, optional
            Summary description, by default None
        tenant_id : str, optional
            Azure tenant ID, by default None
        azure_tenant_id : str, optional
            Azure tenant ID, by default None
        search_key : str, optional
            Search key column for the summarized data, by default None
        tactics : Union[str, list[str], None], optional
            Relevant MITRE tactics, by default None
        techniques : Union[str, list[str], None], optional
            Relevant MITRE techniques, by default None
        source_info : Dict[str, Any], optional
            Summary source info dictionary, by default None
        summary_items : Union[pd, DataFrame, Iterable[DynamicSummaryItem]
            list of summary items
        list[Dict[str, Any]]], optional
            Collection of summary items, by default None

        """
        self.summary_id: str = summary_id or str(uuid.uuid4())
        self.summary_name: str | None = summary_name
        self.summary_description: str | None = summary_description
        self.tenant_id: str | None = azure_tenant_id or tenant_id

        self.search_key: str | None = search_key
        tactics = tactics or []
        self.tactics: list[str] = _match_tactics(
            [tactics] if isinstance(tactics, str) else tactics,
        )
        techniques = techniques or []
        self.techniques: list[str] = (
            [techniques] if isinstance(techniques, str) else techniques
        )
        self.summary_items: list[DynamicSummaryItem] = []
        if summary_items is not None:
            self.add_summary_items(summary_items)
        self.source_info: dict[str, Any] = (
            source_info
            if isinstance(source_info, dict)
            else {"user_source": source_info}
        )
        self.source_info["source_pkg"] = f"MSTICPy {VERSION}"

        logger.info(
            "Dynamic summary created %s",
            summary_id or f"auto({self.summary_id})",
        )

    def __repr__(self: Self) -> str:
        """Return simple representation of instance."""
        attributes: dict[str, str | Any] = {
            key: f"'{val}'" if isinstance(val, str) else val
            for key, val in vars(self).items()
            if key != "summary_items" and val not in (None, pd.NaT, "", [])
        }
        return "\n".join(
            [
                "DynamicSummary(",
                *(f"  {key}={val}" for key, val in attributes.items()),
                f"  summary_items={len(self.summary_items)}",
                ")",
            ],
        )

    @classmethod
    def from_json(
        cls: type[Self],
        data: dict[str, Any] | str,
    ) -> Self:
        """Create new DynamicSummary instance from json string or dict."""
        if isinstance(data, str):
            try:
                data = json.loads(data)
            except json.JSONDecodeError as json_err:
                err_msg: str = "JSON Error decoding dynamic summary data"
                raise MsticpyUserError(err_msg) from json_err
            return cls.from_json(data)
        properties: dict[str, Any] = data.get("properties", data)
        summary_props: dict[str, Any] = {
            _API_TO_CLS_MAP.get(name, name): value
            for name, value in properties.items()
            if name != "rawContent"
        }
        summary = cls(**summary_props)
        summary_items: list[DynamicSummaryItem] = []
        try:
            raw_content_data: str = data.get("rawContent", "[]")
            raw_content: list[dict[str, Any]] = json.loads(raw_content_data)
        except json.JSONDecodeError as json_err:
            err_msg = "JSON Error decoding dynamic summary item data"
            raise MsticpyUserError(err_msg) from json_err
        for raw_item in raw_content:
            summary_item_props: dict[str, Any] = {
                _API_TO_CLS_MAP.get(name, name): (
                    pd.to_datetime(value) if name == "eventTimeUTC" else value
                )
                for name, value in raw_item.items()
            }
            summary_items.append(DynamicSummaryItem(**summary_item_props))
        summary.add_summary_items(summary_items)
        return summary

    @classmethod
    def new_dynamic_summary(  # pylint:disable=too-many-arguments # noqa: PLR0913
        cls: type[Self],
        summary_id: str | None = None,
        summary_name: str | None = None,
        summary_description: str | None = None,
        tenant_id: str | None = None,
        azure_tenant_id: str | None = None,
        search_key: str | None = None,
        tactics: str | list[str] | None = None,
        techniques: str | list[str] | None = None,
        source_info: dict[str, Any] | None = None,
        summary_items: (
            pd.DataFrame | Iterable[DynamicSummaryItem] | list[dict[str, Any]] | None
        ) = None,
    ) -> Self:
        """
        Return a new DynamicSummary object.

        Notes
        -----
        See the DynamicSummary class documentation for details
        of expected parameters.

        See Also
        --------
        DynamicSummary

        """
        return cls(
            summary_id=summary_id,
            summary_name=summary_name,
            summary_description=summary_description,
            tenant_id=tenant_id,
            azure_tenant_id=azure_tenant_id,
            search_key=search_key,
            tactics=tactics,
            techniques=techniques,
            source_info=source_info,
            summary_items=summary_items,
        )

    @staticmethod
    def df_to_dynamic_summaries(data: pd.DataFrame) -> list[DynamicSummary]:
        r"""
        Return a list of DynamicSummary objects from a DataFrame of summaries.

        Parameters
        ----------
        data : pd.DataFrame
            DataFrame containing dynamic summaries

        Returns
        -------
        list[DynamicSummary]
            List of Dynamic Summary objects.

        Examples
        --------
        Use the following steps to obtain a list of dynamic summaries
        from MS Sentinel and convert to DynamicSummary objects.

        .. code:: python

            query = \"\"\"
                DynamicSummary
                | where <some filter criteria>
                | where SummaryStatus == "Active" or SummaryDataType == "SummaryItem"
            \"\"\"
            data = qry_prov.exec_query(query)
            dyn_summaries = df_to_dynamic_summaries(data)

        """
        return [
            df_to_dynamic_summary(ds_data) for _, ds_data in data.groupby("SummaryId")
        ]

    @staticmethod
    def df_to_dynamic_summary(data: pd.DataFrame) -> DynamicSummary:
        r"""
        Return a single DynamicSummary object from a DataFrame.

        Parameters
        ----------
        data : pd.DataFrame
            DataFrame containing a single dynamic summary plus
            summary items.

        Returns
        -------
        DynamicSummary
            The DynamicSummary object.

        Examples
        --------
        Use the following steps to query a single dynamic summary
        from MS Sentinel and convert to a DynamicSummary object.

        .. code:: python

            query = \"\"\"
                DynamicSummary
                | where SummaryId == "26b95b5e-2645-4d33-91a7-ea3c1b8b4b8b"
                | where SummaryStatus == "Active" or SummaryDataType == "SummaryItem"
            \"\"\"
            data = qry_prov.exec_query(query)
            dyn_summaries = df_to_dynamic_summary(data)

        """
        return df_to_dynamic_summary(data)

    def add_summary_items(
        self: Self,
        data: Iterable[DynamicSummaryItem] | Iterable[dict[str, Any]] | pd.DataFrame,
        **kwargs,
    ) -> None:
        """
        Add list of DynamicSummaryItems replacing existing list.

        Parameters
        ----------
        data : Union[Iterable[DynamicSummaryItem], Iterable[Dict[str, Any]], pd.DataFrame]
            Iterable or DataFrame of DynamicSummary Items.
        summary_fields : Optional[Dict[str, str]], optional
            (only relevant if `data` is a DataFrame)
            Dictionary of mappings to extract from the DataFrame
            and use as SummaryItem properties, by default None.
            For example: {"col_a": "tactics", "col_b": "relation_name"}
            See DynamicSummaryItem for a list of available properties.

        See Also
        --------
        DynamicSummaryItem

        """
        if isinstance(data, pd.DataFrame):
            if data.empty:
                return
        elif not data:
            return
        self._add_summary_items(data, **kwargs)

    @singledispatchmethod
    def _add_summary_items(self: Self, data: list, **kwargs) -> None:
        """
        Add list of DynamicSummaryItems.

        Parameters
        ----------
        data : Union[Iterable[DynamicSummaryItem], Iterable[Dict[str, Any]]]
            Iterable of DynamicSummary Items.

        """
        del kwargs
        if isinstance(next(iter(data)), DynamicSummaryItem):
            logger.info(
                "_add_summary_items (list(DynamicSummaryItem)) items %d",
                len(data) if data else 0,
            )
            self.summary_items = list(data)
        else:
            self._add_summary_items_dict(data)

    @_add_summary_items.register(pd.DataFrame)
    def _(
        self: Self,
        data: pd.DataFrame,
        *,
        summary_fields: dict[str, str] | None = None,
        event_time_utc: str | None = None,
        search_key: str | None = None,
        **kwargs,
    ) -> None:
        """
        Add DataFrame of dynamic summary items.

        Parameters
        ----------
        data : pd.DataFrame
            DataFrame containing dynamic summary items.
        summary_fields : Optional[Dict[str, str]], optional
            dictionary of mappings to extract from the DataFrame
            and use as SummaryItem properties, by default None.
            For example: {"col_a": "tactics", "col_b": "relation_name"}
            See DynamicSummaryItem for a list of available properties.
        event_time_utc: Optional[datetime] = None
            Event time for the summary item
        search_key: Optional[str] = None
            Searchable key value for summary item

        See Also
        --------
        DynamicSummaryItem

        """
        logger.info("_add_summary_items (df) rows %d", len(data))
        for row in data.to_dict(orient="records"):
            summary_params: dict[str, Any] = {}
            if summary_fields:
                # if summary fields to map to dynamic summary item properties
                # extract these from the row dictionary first
                summary_params = {
                    field_name: row.get(column_name)
                    for field_name, column_name in summary_fields.items()
                }
            # if event time not in summary_fields, try to get from
            # kwargs or from data
            if "event_time_utc" not in summary_params:
                summary_params["event_time_utc"] = event_time_utc or row.get(
                    "TimeGenerated",
                )
            search_key_value: str | None = (
                row.get(self.search_key) if self.search_key else None
            )
            if search_key_value and not search_key:
                search_key = search_key_value
            # Create DynamicSummaryItem instance for each row
            self.summary_items.append(
                DynamicSummaryItem(
                    packed_content={
                        key: _convert_data_types(value) for key, value in row.items()
                    },
                    **{**summary_params, "search_key": search_key},
                    **kwargs,  # pass remaining kwargs as summary item properties
                ),
            )

    def _add_summary_items_dict(self: Self, data: Iterable[dict[str, Any]]) -> None:
        """
        Add DynamicSummary items from an iterable of dicts.

        Parameters
        ----------
        data : Iterable[Dict[str, Any]]
            Iterable of dictionaries containing summary item
            properties.

        """
        logger.info(
            "_add_summary_items (list(dict)) rows %d",
            len(list(data)) if data else 0,
        )
        summary_items: list[DynamicSummaryItem] = []
        for properties in data:
            # if search key specified, try to extract from packed_content field
            if (
                self.search_key
                and "search_key" not in properties
                and self.search_key in properties.get("packed_content", {})
            ):
                search_key_value: str = properties.get("packed_content", {}).get(
                    self.search_key,
                )
                if search_key_value:
                    properties["search_key"] = search_key_value
            summary_items.append(DynamicSummaryItem(**properties))
        self.summary_items = summary_items

    def append_summary_items(
        self: Self,
        data: Iterable[DynamicSummaryItem] | Iterable[dict[str, Any]] | pd.DataFrame,
        **kwargs,
    ) -> None:
        """
        Append list of DynamicSummaryItems to existing list.

        Parameters
        ----------
        data : Union[Iterable[DynamicSummaryItem], Iterable[Dict[str, Any]], pd.DataFrame]
            Iterable or DataFrame of DynamicSummary Items.
        summary_fields : Optional[Dict[str, str]], optional
            (only relevant if `data` is a DataFrame)
            Dictionary of mappings to extract from the DataFrame
            and use as SummaryItem properties, by default None.
            For example: {"col_a": "tactics", "col_b": "relation_name"}
            See DynamicSummaryItem for a list of available properties.

        See Also
        --------
        DynamicSummaryItem

        """
        current_items: list[DynamicSummaryItem] = self.summary_items
        self.add_summary_items(data, **kwargs)
        new_items: list[DynamicSummaryItem] = self.summary_items
        self.summary_items = current_items + new_items
        logger.info("append_summary_items %s", type(data))

    def to_json(self: Self) -> str:
        """Return JSON representation of DynamicSummary."""
        summary_properties: dict[str, Any] = {
            _CLS_TO_API_MAP.get(prop_name, prop_name): prop_value
            for prop_name, prop_value in self.__dict__.items()
            if prop_name in _CLS_TO_API_MAP and prop_value is not None
        }
        if self.summary_items:
            summary_properties[_CLS_TO_API_MAP["summary_items"]] = json.dumps(
                [item.to_api_dict() for item in self.summary_items],
            )
        return json.dumps(summary_properties)

    def to_json_api(self: Self) -> str:
        """Return API-ready JSON representation of DynamicSummary."""
        return f'{{"properties" : {self.to_json()} }}'

    def to_df(self: Self) -> pd.DataFrame:
        """Return summary items as DataFrame."""
        data = pd.DataFrame([item.packed_content for item in self.summary_items])
        if "TimeGenerated" in data.columns:
            return (
                data.assign(timestamp=pd.to_datetime(data["TimeGenerated"]))
                .drop(columns="TimeGenerated")
                .rename(columns={"timestamp": "TimeGenerated"})
            )
        return data


_DF_TO_CLS_MAP: dict[str, str] = {
    "TenantId": "ws_tenant_id",
    "TimeGenerated": "time_generated",
    "AzureTenantId": "tenant_id",
    "SummaryId": "summary_id",
    "SummaryItemId": "summary_item_id",
    "SummaryName": "summary_name",
    "RelationName": "relation_name",
    "RelationId": "relation_id",
    "SearchKey": "search_key",
    "CreatedBy": "created_by",
    "CreatedTimeUTC": "created_time_utc",
    "UpdatedBy": "updated_by",
    "UpdatedTimeUTC": "updated_time_utc",
    "SummaryDescription": "summary_description",
    "Tactics": "tactics",
    "Techniques": "techniques",
    "SummaryStatus": "summary_status",
    "SourceInfo": "source_info",
    "Query": "query",
    "QueryStartDate": "query_start_date",
    "QueryEndDate": "query_end_date",
    "EventTimeUTC": "event_time_utc",
    "ObservableType": "observable_type",
    "ObservableValue": "observable_value",
    "PackedContent": "packed_content",
    "SummaryDataType": "summary_data_type",
    "SourceSystem": "source_system",
    "Type": "type",
}
_CLS_TO_DF_MAP: dict[str, str] = {val: key for key, val in _DF_TO_CLS_MAP.items()}
_DF_SUMMARY_FIELDS: set[str] = {
    "TenantId",
    "TimeGenerated",
    "AzureTenantId",
    "SummaryId",
    "SummaryName",
    "RelationName",
    "RelationId",
    "SearchKey",
    "CreatedBy",
    "CreatedTimeUTC",
    "UpdatedBy",
    "UpdatedTimeUTC",
    "SummaryDescription",
    "Tactics",
    "Techniques",
    "SummaryStatus",
    "SourceInfo",
    "Query",
    "QueryStartDate",
    "QueryEndDate",
    "SummaryDataType",
}
_DF_SUMMARY_ITEM_FIELDS: set[str] = {
    "TimeGenerated",
    "SummaryItemId",
    "RelationName",
    "RelationId",
    "SearchKey",
    "CreatedBy",
    "CreatedTimeUTC",
    "UpdatedBy",
    "UpdatedTimeUTC",
    "SummaryDescription",
    "Tactics",
    "Techniques",
    "EventTimeUTC",
    "ObservableType",
    "ObservableValue",
    "PackedContent",
    "SummaryDataType",
}


def _get_summary_record(data: pd.DataFrame) -> pd.Series:
    """Return active dynamic summary header record."""
    ds_summary: pd.DataFrame = data[
        (data["SummaryDataType"] == "Summary") & (data["SummaryStatus"] == "Active")
    ]
    return ds_summary[list(_DF_SUMMARY_FIELDS)].rename(columns=_DF_TO_CLS_MAP).iloc[0]


def _get_summary_items(data: pd.DataFrame) -> pd.DataFrame:
    """Return summary item records for dynamic summary."""
    ds_summary_items: pd.DataFrame = data[data["SummaryDataType"] == "SummaryItem"]
    return ds_summary_items[list(_DF_SUMMARY_ITEM_FIELDS)].rename(
        columns=_DF_TO_CLS_MAP,
    )


def df_to_dynamic_summaries(data: pd.DataFrame) -> list[DynamicSummary]:
    r"""
    Return a list of DynamicSummary objects from a DataFrame of summaries.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame containing dynamic summaries

    Returns
    -------
    list[DynamicSummary]
        List of Dynamic Summary objects.

    Examples
    --------
    Use the following steps to obtain a list of dynamic summaries
    from MS Sentinel and convert to DynamicSummary objects.

    .. code:: python

        query = \"\"\"
            DynamicSummary
            | where <some filter criteria>
            | where SummaryStatus == "Active" or SummaryDataType == "SummaryItem"
        \"\"\"
        data = qry_prov.exec_query(query)
        dyn_summaries = df_to_dynamic_summaries(data)

    """
    return [df_to_dynamic_summary(ds_data) for _, ds_data in data.groupby("SummaryId")]


def df_to_dynamic_summary(data: pd.DataFrame) -> DynamicSummary:
    r"""
    Return a single DynamicSummary object from a DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame containing a single dynamic summary plus
        summary items.

    Returns
    -------
    DynamicSummary
        The DynamicSummary object.

    Examples
    --------
    Use the following steps to query a single dynamic summary
    from MS Sentinel and convert to a DynamicSummary object.

    .. code:: python

        query = \"\"\"
            DynamicSummary
            | where SummaryId == "26b95b5e-2645-4d33-91a7-ea3c1b8b4b8b"
            | where SummaryStatus == "Active" or SummaryDataType == "SummaryItem"
        \"\"\"
        data = qry_prov.exec_query(query)
        dyn_summaries = df_to_dynamic_summary(data)

    """
    dyn_summary: DynamicSummary = DynamicSummary()
    dyn_summary.__dict__.update(_get_summary_record(data).to_dict())

    items_list: list[dict[Hashable, Any]] = _get_summary_items(data).to_dict(
        orient="records",
    )
    items: list[DynamicSummaryItem] = []
    for item in items_list:
        # "fields" attrib is a ClassVar
        ds_item: DynamicSummaryItem = DynamicSummaryItem()
        for key, value in item.items():
            setattr(ds_item, str(key), value)
        items.append(ds_item)
    dyn_summary.add_summary_items(items)
    return dyn_summary


def _to_datetime_utc_str(date_time: datetime | str) -> str:
    """Convert datetime to ISO date string."""
    if not isinstance(date_time, datetime):
        return date_time
    dt_str: str = date_time.isoformat()
    return dt_str.replace("+00:00", "Z") if "+00:00" in dt_str else f"{dt_str}Z"


def _convert_dict_types(input_dict: dict[Any, Any]) -> dict[Any, Any]:
    """Convert data types in dictionary members."""
    return {name: _convert_data_types(value) for name, value in input_dict.items()}


_TYPE_CONVERTER: dict[Any, Callable] = {
    np.ndarray: list,
    datetime: _to_datetime_utc_str,
    pd.Timestamp: _to_datetime_utc_str,
    dict: _convert_dict_types,
}


def _convert_data_types(
    value: str,
    type_convert: dict[type, Callable] | None = None,
) -> str:
    """Convert a type based on dictionary of converters."""
    type_convert = type_convert or {}
    type_convert.update(_TYPE_CONVERTER)
    converter: Callable | None = type_convert.get(type(value))
    return converter(value) if converter else value


def _match_tactics(tactics: Iterable[str]) -> list[str]:
    """Return case-insensitive matches for tactics list."""
    return [
        _TACTICS_DICT[tactic.casefold()]
        for tactic in tactics
        if tactic in _TACTICS_DICT
    ]
