# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Sentinel Dynamic Summary classes."""
import dataclasses
import json
import uuid
from datetime import datetime
from functools import singledispatchmethod
from typing import Any, ClassVar, Dict, Iterable, List, Optional, Union, cast

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError

__version__ = VERSION
__author__ = "Ian Hellen"

_CLS_TO_API_MAP = {
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
_API_TO_CLS_MAP = {val: key for key, val in _CLS_TO_API_MAP.items()}


class FieldList:
    """Class to hold field names."""

    def __init__(self, fieldnames: Iterable[str]):
        """Add fields to field mapping."""
        self.__dict__.update({field.upper(): field for field in fieldnames})

    def __repr__(self):
        """Return list of field attributes and values."""
        field_names = "\n    ".join(f"{key}='{val}'" for key, val in vars(self).items())
        return f"Fields:\n    {field_names}"


# pylint: disable=too-many-instance-attributes
@dataclasses.dataclass
class DynamicSummaryItem:
    """
    DynamicSummaryItem class.

    Attributes
    ----------
    summary_item_id: Optional[str]
        The ID of the item
    relation_name: Optional[str] = None
        The name of the summary item relation
    relation_id: Optional[str] = None
        The ID of the summary item relation
    search_key: Optional[str] = None
        Searchable key value for summary item
    tactics: Union[str, List[str], None] = None
        Relevant MITRE tactics for the summary item
    techniques: Union[str, List[str], None] = None
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

    fields: ClassVar
    summary_item_id: Optional[str] = None
    relation_name: Optional[str] = None
    relation_id: Optional[str] = None
    search_key: Optional[str] = None
    tactics: Union[str, List[str], None] = dataclasses.field(  # type: ignore
        default_factory=list
    )
    techniques: Union[str, List[str], None] = dataclasses.field(  # type: ignore
        default_factory=list
    )
    event_time_utc: Optional[datetime] = None
    observable_type: Optional[str] = None
    observable_value: Optional[str] = None
    packed_content: Dict[str, Any] = dataclasses.field(default_factory=dict)

    def __post_init__(self):
        """Initialize item ID if was not set explicitly."""
        self.summary_item_id = self.summary_item_id or str(uuid.uuid4())
        if isinstance(self.tactics, str):
            self.tactics = [self.tactics]
        if isinstance(self.techniques, str):
            self.techniques = [self.techniques]

    def to_api_dict(self):
        """Return attributes as a JSON-serializable dictionary."""
        return {
            _CLS_TO_API_MAP.get(name, name): _to_datetime_utc_str(value)
            if isinstance(value, datetime)
            else _dict_dates_to_str(value)
            if isinstance(value, dict)
            else value
            for name, value in dataclasses.asdict(self).items()
            if value is not None
        }


# Add helper class attribute for field names.
DynamicSummaryItem.fields = FieldList(
    [field.name for field in dataclasses.fields(DynamicSummaryItem)]
)


def _to_datetime_utc_str(date_time):
    if not isinstance(date_time, datetime):
        return date_time
    dt_str = date_time.isoformat()
    return dt_str.replace("+00:00", "Z") if "+00:00" in dt_str else f"{dt_str}Z"


def _dict_dates_to_str(input_dict: Dict[Any, Any]) -> Dict[Any, Any]:
    return {
        name: value.isoformat() if isinstance(value, datetime) else value
        for name, value in input_dict.items()
    }


class DynamicSummary:
    """Dynamic Summary class."""

    fields = FieldList(
        ["summary_id", "summary_name", "summary_description"]
        + ["tenant_id", "relation_name", "relation_id"]  # noqa: W503
        + ["search_key", "tactics", "techniques", "source_info"]  # noqa: W503
        + ["summary_items"]  # noqa: W503
    )

    def __init__(self, summary_id: Optional[str] = None, **kwargs):
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
        relation_name : str, optional
            The relation name, by default None
        relation_id : str, optional
            The relation ID, by default None
        search_key : str, optional
            Search key for the entire summary, by default None
        tactics : Union[str, List[str], None], optional
            Relevant MITRE tactics, by default None
        techniques : Union[str, List[str], None], optional
            Relevant MITRE techniques, by default None
        source_info : str, optional
            Summary source info, by default None
        summary_items : Union[pd, DataFrame, Iterable[DynamicSummaryItem],
        List[Dict[str, Any]]], optional
            Collection of summary items, by default None

        """
        self.summary_id: str = summary_id or str(uuid.uuid4())
        self.summary_name: str = kwargs.pop("summary_name", None)
        self.summary_description: str = kwargs.pop("summary_description", None)
        self.tenant_id: str = kwargs.pop(
            "azure_tenant_id", kwargs.pop("tenant_id", None)
        )
        tactics = kwargs.pop("tactics", [])
        self.tactics = [tactics] if isinstance(tactics, str) else tactics
        techniques = kwargs.pop("techniques", [])
        self.techniques = [techniques] if isinstance(techniques, str) else techniques
        self.summary_items: List[DynamicSummaryItem] = []
        summary_items = kwargs.pop("summary_items", None)
        if summary_items is not None:
            self.add_summary_items(summary_items)
        # Add other kwargs as instance attributes
        self.__dict__.update(kwargs)

    def __repr__(self) -> str:
        """Return simple representation of instance."""
        attributes = {
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
            ]
        )

    @classmethod
    def from_json(cls, data: Union[Dict[str, Any], str]) -> "DynamicSummary":
        """Create new DynamicSummary instance from json string or dict."""
        if isinstance(data, str):
            try:
                data = json.loads(data)
            except json.JSONDecodeError as json_err:
                raise MsticpyUserError(
                    "JSON Error decoding dynamic summary data"
                ) from json_err
        data = cast(Dict[str, Any], data)
        if "properties" in data:
            data = data["properties"]
        data = cast(Dict[str, Any], data)
        summary_props = {
            _API_TO_CLS_MAP.get(name, name): value
            for name, value in data.items()
            if name != "rawContent"
        }
        summary = cls(**summary_props)
        summary_items: List[DynamicSummaryItem] = []
        try:
            raw_content = json.loads(data.get("rawContent", "[]"))
        except json.JSONDecodeError as json_err:
            raise MsticpyUserError(
                "JSON Error decoding dynamic summary item data"
            ) from json_err
        for raw_item in raw_content:
            summary_item_props = {
                _API_TO_CLS_MAP.get(name, name): pd.to_datetime(value)
                if name == "eventTimeUTC"
                else value
                for name, value in raw_item.items()
            }
            summary_items.append(DynamicSummaryItem(**summary_item_props))
        summary.add_summary_items(summary_items)
        return summary

    @classmethod
    def new_dynamic_summary(cls, **kwargs):
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
        return cls(**kwargs)

    @staticmethod
    def df_to_dynamic_summaries(data: pd.DataFrame) -> List["DynamicSummary"]:
        r"""
        Return a list of DynamicSummary objects from a DataFrame of summaries.

        Parameters
        ----------
        data : pd.DataFrame
            DataFrame containing dynamic summaries

        Returns
        -------
        List[DynamicSummary]
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
    def df_to_dynamic_summary(data: pd.DataFrame) -> "DynamicSummary":
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
        self,
        data: Union[
            Iterable[DynamicSummaryItem], Iterable[Dict[str, Any]], pd.DataFrame
        ],
        **kwargs,
    ):
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
    def _add_summary_items(self, data: list, **kwargs):
        """
        Add list of DynamicSummaryItems.

        Parameters
        ----------
        data : Union[Iterable[DynamicSummaryItem], Iterable[Dict[str, Any]]]
            Iterable of DynamicSummary Items.

        """
        del kwargs
        if isinstance(next(iter(data)), DynamicSummaryItem):
            self.summary_items = list(data)
        else:
            self._add_summary_items_dict(data)

    @_add_summary_items.register
    def _(
        self,
        data: pd.DataFrame,
        **kwargs,
    ):
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

        See Also
        --------
        DynamicSummaryItem

        """
        summary_fields = kwargs.pop("summary_fields", None)
        for row in data.to_dict(orient="records"):
            summary_params = {}
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
                summary_params["event_time_utc"] = kwargs.pop(
                    "event_time_utc", row.get("TimeGenerated")
                )
            # Create DynamicSummaryItem instance for each row
            self.summary_items.append(
                DynamicSummaryItem(
                    packed_content=row,  # type: ignore
                    **summary_params,
                    **kwargs,  # pass remaining kwargs as summary item properties
                )
            )

    def _add_summary_items_dict(self, data: Iterable[Dict[str, Any]]):
        """
        Add DynamicSummary items from an iterable of dicts.

        Parameters
        ----------
        data : Iterable[Dict[str, Any]]
            Iterable of dictionaries containing summary item
            properties.

        """
        self.summary_items = [DynamicSummaryItem(**properties) for properties in data]

    def append_summary_items(
        self,
        data: Union[
            Iterable[DynamicSummaryItem], Iterable[Dict[str, Any]], pd.DataFrame
        ],
        **kwargs,
    ):
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
        current_items = self.summary_items
        self.add_summary_items(data, **kwargs)
        new_items = self.summary_items
        self.summary_items = current_items + new_items

    def to_json(self):
        """Return JSON representation of DynamicSummary."""
        summary_properties = {
            _CLS_TO_API_MAP.get(prop_name, prop_name): prop_value
            for prop_name, prop_value in self.__dict__.items()
            if prop_name in _CLS_TO_API_MAP and prop_value is not None
        }
        if self.summary_items:
            summary_properties[_CLS_TO_API_MAP["summary_items"]] = json.dumps(
                [item.to_api_dict() for item in self.summary_items]
            )
        return json.dumps(summary_properties)

    def to_json_api(self):
        """Return API-ready JSON representation of DynamicSummary."""
        return f'{{"properties" : {self.to_json()} }}'

    def to_df(self) -> pd.DataFrame:
        """Return summary items as DataFrame."""
        data = pd.DataFrame([item.packed_content for item in self.summary_items])
        if "TimeGenerated" in data.columns:
            return (
                data.assign(timestamp=pd.to_datetime(data["TimeGenerated"]))
                .drop(columns="TimeGenerated")
                .rename(columns={"timestamp": "TimeGenerated"})
            )
        return data


_DF_TO_CLS_MAP = {
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
_CLS_TO_DF_MAP = {val: key for key, val in _DF_TO_CLS_MAP.items()}
_DF_SUMMARY_FIELDS = {
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
_DF_SUMMARY_ITEM_FIELDS = {
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
    ds_summary = data[
        (data["SummaryDataType"] == "Summary") & (data["SummaryStatus"] == "Active")
    ]
    return ds_summary[list(_DF_SUMMARY_FIELDS)].rename(columns=_DF_TO_CLS_MAP).iloc[0]


def _get_summary_items(data: pd.DataFrame) -> pd.DataFrame:
    """Return summary item records for dynamic summary."""
    ds_summary_items = data[data["SummaryDataType"] == "SummaryItem"]
    return ds_summary_items[list(_DF_SUMMARY_ITEM_FIELDS)].rename(
        columns=_DF_TO_CLS_MAP
    )


def df_to_dynamic_summaries(data: pd.DataFrame) -> List[DynamicSummary]:
    r"""
    Return a list of DynamicSummary objects from a DataFrame of summaries.

    Parameters
    ----------
    data : pd.DataFrame
        DataFrame containing dynamic summaries

    Returns
    -------
    List[DynamicSummary]
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
    dyn_summary = DynamicSummary()
    dyn_summary.__dict__.update(_get_summary_record(data).to_dict())  # type: ignore

    items_list = _get_summary_items(data).to_dict(orient="records")
    items = []
    for item in items_list:
        # pylint: disable=no-value-for-parameter
        # "fields" attrib is a ClassVar
        ds_item = DynamicSummaryItem()
        ds_item.__dict__.update(item)  # type: ignore
        items.append(ds_item)
    dyn_summary.add_summary_items(items)
    return dyn_summary
