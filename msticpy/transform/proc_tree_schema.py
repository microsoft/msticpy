# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Process Tree Schema module for Process Tree Visualization."""
from __future__ import annotations

from dataclasses import asdict, dataclass, field, fields, MISSING
from typing import Any, ClassVar

import pandas as pd
from typing_extensions import Self

from .._version import VERSION
from ..common.exceptions import MsticpyUserError

__version__ = VERSION
__author__ = "Ian Hellen"


class ProcessTreeSchemaException(MsticpyUserError):
    """Custom exception for Process Tree schema."""

    DEF_HELP_URI: ClassVar[tuple[str, str]] = (
        "MSTICPy Process Tree documentation ",
        "https://msticpy.readthedocs.io/en/latest/visualization/ProcessTree.html",
    )


@dataclass
class ProcSchema:  # pylint: disable=too-many-instance-attributes
    """
    Property name lookup for Process event schema.

    Each property maps a generic column name on to the
    schema of the input data. Most of these are mandatory,
    some are optional - not supplying them may result in
    a less complete tree.
    The `time_stamp` column should be supplied although
    defaults to 'TimeGenerated'.

    """

    process_name: str
    process_id: str
    parent_id: str
    time_stamp: str
    cmd_line: str | None = field(default=None)
    path_separator: str = field(default="\\")
    user_name: str | None = field(default=None)
    logon_id: str | None = field(default=None)
    host_name_column: str | None = field(default=None)
    parent_name: str | None = field(default=None)
    target_logon_id: str | None = field(default=None)
    user_id: str | None = field(default=None)
    event_id_column: str | None = field(default=None)
    event_id_identifier: Any | None = field(default=None)

    def __eq__(self: Self, other: object) -> bool:
        """Return False if any non-blank field values are unequal."""
        if not isinstance(other, ProcSchema):
            return False
        self_dict: dict[str, Any] = asdict(self)

        return not any(
            value and value != self_dict[field]
            for field, value in asdict(other).items()
        )

    @property
    def required_columns(self: Self) -> list[str]:
        """Return columns required for Init."""
        return [
            "process_name",
            "process_id",
            "parent_id",
            "cmd_line",
            "path_separator",
            "time_stamp",
        ]

    @property
    def column_map(self: Self) -> dict[str, str]:
        """Return a dictionary that maps fields to schema names."""
        return {
            prop: str(col)
            for prop, col in asdict(self).items()
            if prop not in {"path_separator", "event_id_identifier"}
        }

    @property
    def columns(self: Self) -> list[str]:
        """Return list of columns in schema data source."""
        return [
            col
            for prop, col in asdict(self).items()
            if prop not in {"path_separator", "event_id_identifier"}
        ]

    def get_df_cols(self: Self, data: pd.DataFrame) -> list[str]:
        """Return the subset of columns that are present in `data`."""
        return [col for col in self.columns if col in data.columns]

    @property
    def host_name(self: Self) -> str | None:
        """Return host name column."""
        return self.host_name_column

    @property
    def event_type_col(self: Self) -> str:
        """
        Return the column name containing the event identifier.

        Returns
        -------
        str
            The name of the event ID column.

        Raises
        ------
        ProcessTreeSchemaException
            If the schema is not known.

        """
        if self.event_id_column:
            return self.event_id_column
        raise ProcessTreeSchemaException(
            "Unknown schema - there is no value for the 'event_id' column."
        )

    @property
    def event_filter(self: Self) -> Any:
        """
        Return the event type/ID to process for the current schema.

        Returns
        -------
        Any
            The value of the event ID to process.

        Raises
        ------
        ProcessTreeSchemaException
            If the schema is not known.

        """
        if self.event_id_identifier:
            return self.event_id_identifier
        raise ProcessTreeSchemaException(
            "Unknown schema - there is no value for the 'event_id_identifier' in the schema."
        )

    @classmethod
    def blank_schema_dict(cls: type[Self]) -> dict[str, Any]:
        """Return blank schema dictionary."""
        return {
            cls_field.name: (
                "required"
                if (cls_field.default or cls_field.default == MISSING)
                else None
            )
            for cls_field in fields(cls)
        }


WIN_EVENT_SCH = ProcSchema(
    time_stamp="TimeGenerated",
    process_name="NewProcessName",
    process_id="NewProcessId",
    parent_name="ParentProcessName",
    parent_id="ProcessId",
    logon_id="SubjectLogonId",
    target_logon_id="TargetLogonId",
    cmd_line="CommandLine",
    user_name="SubjectUserName",
    path_separator="\\",
    user_id="SubjectUserSid",
    event_id_column="EventID",
    event_id_identifier=4688,
    host_name_column="Computer",
)

LX_EVENT_SCH = ProcSchema(
    time_stamp="TimeGenerated",
    process_name="exe",
    process_id="pid",
    parent_name=None,
    parent_id="ppid",
    logon_id="ses",
    target_logon_id=None,
    cmd_line="cmdline",
    user_name="acct",
    path_separator="/",
    user_id="uid",
    event_id_column="EventType",
    event_id_identifier="SYSCALL_EXECVE",
    host_name_column="Computer",
)

MDE_INT_EVENT_SCH = ProcSchema(
    time_stamp="CreatedProcessCreationTime",
    process_name="CreatedProcessName",
    process_id="CreatedProcessId",
    parent_name="ParentProcessName",
    parent_id="CreatedProcessParentId",
    logon_id="InitiatingProcessLogonId",
    target_logon_id="LogonId",
    cmd_line="CreatedProcessCommandLine",
    user_name="CreatedProcessAccountName",
    path_separator="\\",
    user_id="CreatedProcessAccountSid",
    host_name_column="ComputerDnsName",
)

# MDE Public and Sentinel DeviceProcessEvents schema
MDE_EVENT_SCH = ProcSchema(
    time_stamp="Timestamp",
    process_name="FileName",
    process_id="ProcessId",
    parent_name="InitiatingProcessFileName",
    parent_id="InitiatingProcessId",
    logon_id="InitiatingProcessLogonId",
    target_logon_id="LogonId",
    cmd_line="ProcessCommandLine",
    user_name="AccountName",
    path_separator="\\",
    user_id="AccountSid",
    host_name_column="DeviceName",
    event_id_column="ActionType",
    event_id_identifier="ProcessCreated",
)

# Sysmon Process Create
SYSMON_PROCESS_CREATE_EVENT_SCH = ProcSchema(
    time_stamp="UtcTime",
    process_name="Image",
    process_id="ProcessId",
    parent_name="ParentImage",
    parent_id="ParentProcessId",
    logon_id="LogonId",
    cmd_line="CommandLine",
    user_name="User",
    path_separator="\\",
    event_id_column="EventID",
    event_id_identifier=1,
    host_name_column="Computer",
)

# Osquery process (may vary depending on exact query)
OSQUERY_EVENT_SCH = ProcSchema(
    time_stamp="calendarTime",
    # host_name_column = "hostIdentifier",
    process_name="name",
    process_id="pid",
    parent_name="pcmdline",
    parent_id="parent",
    logon_id=None,
    target_logon_id=None,
    cmd_line="cmdline",
    user_name="username",
    path_separator="/",
    user_id="uid",
)

# FireEye HX processEvent from 'stateagentinspector' or 'eventbuffer' audits
HX_PROCESSEVENT_SCH = ProcSchema(
    time_stamp="starttime",
    process_name="process",
    process_id="pid",
    parent_name="parentprocess",
    parent_id="parentpid",
    cmd_line="processcmdline",
    user_name="username",
    path_separator="\\",
    event_id_column="eventtype",
    event_id_identifier="start",
    host_name_column="hostname",
)

SUPPORTED_SCHEMAS: tuple[ProcSchema, ...] = (
    WIN_EVENT_SCH,
    LX_EVENT_SCH,
    MDE_INT_EVENT_SCH,
    MDE_EVENT_SCH,
    SYSMON_PROCESS_CREATE_EVENT_SCH,
    OSQUERY_EVENT_SCH,
    HX_PROCESSEVENT_SCH,
)


# pylint: disable=too-few-public-methods
class ColNames:
    """Class to hold constant column names."""

    proc_key: str = "proc_key"
    parent_key: str = "parent_key"
    new_process_lc: str = "new_process_lc"
    parent_proc_lc: str = "parent_proc_lc"
    timestamp_orig_par: str = "timestamp_orig_par"
    EffectiveLogonId: str = "EffectiveLogonId"
    source_index: str = "source_index"
    source_index_par: str = "source_index_par"
    new_process_lc_par: str = "new_process_lc_par"
    EffectiveLogonId_par: str = "EffectiveLogonId_par"
