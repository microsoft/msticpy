# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
eventschema.

Module for DataSchema class
"""
from typing import Dict
from deprecated.sphinx import deprecated

from .query_defns import DataFamily, DataEnvironment
from .utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
class DataSchema:
    """DataSchema class for Log Analytics Queries."""

    DATA_MAPPINGS: Dict[
        DataEnvironment, Dict[DataFamily, Dict[str, Dict[str, str]]]
    ] = {DataEnvironment.LogAnalytics: {}, DataEnvironment.Kusto: {}}
    DATA_MAPPINGS[DataEnvironment.LogAnalytics] = {
        DataFamily.WindowsSecurity: {},
        DataFamily.LinuxSecurity: {},
        DataFamily.SecurityAlert: {},
    }

    _SECURITY_ALERT = {
        "table": "SecurityAlert",
        "query_project": """| project
                                            TenantId,
                                            StartTimeUtc = StartTime,
                                            EndTimeUtc = EndTime,
                                            ProviderAlertId = VendorOriginalId,
                                            SystemAlertId,
                                            ProviderName,
                                            VendorName,
                                            AlertType = AlertName,
                                            AlertName,
                                            AlertDisplayName = DisplayName,
                                            Description,
                                            Severity = AlertSeverity,
                                            IsIncident,
                                            ExtendedProperties,
                                            Entities,
                                            ConfidenceLevel,
                                            ConfidenceScore,
                                            ExtendedLinks,
                                            WorkspaceSubscriptionId,
                                            WorkspaceResourceGroup,
                                            TimeGenerated,
                                            ResourceId,
                                            SourceComputerId """,
    }

    _PROC_CREATE_WIN = {
        "table": "SecurityEvent | where EventID == 4688",
        "query_project": """| project
                                            TenantId,
                                            Account,
                                            EventID,
                                            TimeGenerated,
                                            Computer,
                                            SubjectUserSid,
                                            SubjectUserName,
                                            SubjectDomainName,
                                            SubjectLogonId,
                                            NewProcessId,
                                            NewProcessName,
                                            TokenElevationType,
                                            ProcessId,
                                            CommandLine,
                                            ParentProcessName,
                                            TargetLogonId,
                                            SourceComputerId
                                            | extend TimeCreatedUtc=TimeGenerated """,
    }

    _ACCOUNT_LOGON_WIN = {
        "table": "SecurityEvent | where EventID == 4624",
        "query_project": """| project
                                            TenantId,
                                            Account,
                                            EventID,
                                            TimeGenerated,
                                            SourceComputerId,
                                            Computer,
                                            SubjectUserName,
                                            SubjectDomainName,
                                            SubjectUserSid,
                                            TargetUserName,
                                            TargetDomainName,
                                            TargetUserSid,
                                            TargetLogonId,
                                            LogonProcessName,
                                            LogonType,
                                            AuthenticationPackageName,
                                            Status,
                                            IpAddress,
                                            WorkstationName""",
    }

    _ACCOUNT_LOGON_FAIL_WIN = {
        "table": "SecurityEvent | where EventID == 4625",
        "query_project": """| project
                                            TenantId,
                                            Account,
                                            EventID,
                                            TimeGenerated,
                                            SourceComputerId,
                                            Computer,
                                            SubjectUserName,
                                            SubjectDomainName,
                                            SubjectUserSid,
                                            TargetUserName,
                                            TargetDomainName,
                                            TargetUserSid,
                                            TargetLogonId,
                                            LogonProcessName,
                                            LogonType,
                                            AuthenticationPackageName,
                                            Status,
                                            IpAddress,
                                            WorkstationName""",
    }

    _PROC_CREATE_LX = {
        "table": "LinuxAuditD | where EventID == 14688",
        "query_project": """| project
                                            TenantId,
                                            Account,
                                            EventID,
                                            TimeGenerated,
                                            SourceComputerId
                                            Computer=node,
                                            SubjectUserSid=uid,
                                            SubjectUserName=user,
                                            SubjectDomainName,
                                            SubjectLogonId=ses,
                                            NewProcessId=pid,
                                            NewProcessName=exe,
                                            TokenElevationType,
                                            ProcessId=ppid,
                                            CommandLine=cmdline,
                                            ParentProcessName,
                                            TargetLogonId,
                                            success,
                                            audit_user,
                                            auid,
                                            group,
                                            gid,
                                            effective_user,
                                            euid,
                                            effective_group,
                                            egid,
                                            cwd,
                                            name
                                            | extend TimeCreatedUtc=TimeGenerated """,
    }
    _ACCOUNT_LOGON_LX = {
        "table": "LinuxAuditD | where EventID == 1100 or EventID == 1112",
        "query_project": """| project
                                            TenantId,
                                            Account,
                                            EventID,
                                            TimeGenerated,
                                            SourceComputerId
                                            Computer=node,
                                            SubjectUserName=acct,
                                            SubjectDomainName='',
                                            SubjectUserSid=auid,
                                            TargetUserName=user,
                                            TargetDomainName='',
                                            TargetUserSid=uid,
                                            TargetLogonId=ses,
                                            LogonProcessName=exe,
                                            LogonType=0,
                                            AuthenticationPackageName,
                                            Status=res,
                                            audit_user,
                                            IpAddress=addr,
                                            WorkstationName=hostname""",
    }
    # Add to the main dictionaries
    DATA_MAPPINGS[DataEnvironment.LogAnalytics][DataFamily.SecurityAlert] = {
        "security_alert": _SECURITY_ALERT
    }
    DATA_MAPPINGS[DataEnvironment.LogAnalytics][DataFamily.WindowsSecurity] = {
        "process_create": _PROC_CREATE_WIN,
        "account_logon": _ACCOUNT_LOGON_WIN,
        "account_logon_fail": _ACCOUNT_LOGON_FAIL_WIN,
    }
    DATA_MAPPINGS[DataEnvironment.LogAnalytics][DataFamily.LinuxSecurity] = {
        "process_create": _PROC_CREATE_LX,
        "account_logon": _ACCOUNT_LOGON_LX,
    }

    def __init__(
        self,
        environment: DataEnvironment = DataEnvironment.LogAnalytics,
        data_family: DataFamily = DataFamily.WindowsSecurity,
        data_source: str = "security_alert",
    ):
        """
        Create a new instance of the DataSchema object.

            :param environment='LogAnalytics': Specify the environment for the schema.
            :param data_family=DataFamily.WindowsSecurity: Specify the data family
                for the schema (e.g. DataFamily.SecurityAlert)
            :param data_source='security_alert': Specify the data source required.
        """
        if isinstance(environment, str):
            try:
                environment = DataEnvironment[environment]
            except KeyError:
                pass
        if environment not in DataSchema.DATA_MAPPINGS:
            raise LookupError(
                f"Unknown environment {environment}. "
                "Valid environments are:\n{self.environments}"
            )

        if isinstance(data_family, str):
            try:
                data_family = DataFamily[data_family]
            except KeyError:
                pass
        if data_family not in DataSchema.DATA_MAPPINGS[environment]:
            raise LookupError(
                f"Unknown data_family {data_family}. "
                "Valid families are:\n{self.data_families}"
            )

        if data_source not in DataSchema.DATA_MAPPINGS[environment][data_family]:
            raise LookupError(
                f"Unknown data_source {data_family}. "
                "Valid data sources are:\n{self.data_source_types}"
            )

        self.current = DataSchema.DATA_MAPPINGS[environment][data_family][data_source]

    def __getitem__(self, key):
        """Index operator overload."""
        if self.current is not None and key is not None and key in self.current:
            return self.current[key]
        raise KeyError("{} has no attribute {}".format(str(type(self)), key))

    def __contains__(self, key):
        """In operator overload."""
        if self.current is not None and key is not None and key in self.current:
            return True
        return False

    @property
    def data_environments(self):
        """Return the environments defined in the schema."""
        return self.get_data_environments()

    @property
    def data_families(self):
        """Return the data families in the schema."""
        return self.get_data_families()

    @property
    def data_source_types(self):
        """Return the data families in the schema."""
        sources = set()
        for _, data in self.DATA_MAPPINGS.items():
            for _, data_source in data.items():
                for source in data_source:
                    sources.add(source)

        return sources

    @property
    def property_names(self):
        """Return the current items available in the schema."""
        return list(self.current.keys())

    @classmethod
    def default_schemas(cls, environment=None, data_family=None):
        """
        Return data sources for specified environment and data family.

            :param environment: the data environment
            :param data_family: the data family (e.g. WindowsSecurity,
                LinuxSecurity, Office365)
        """
        if environment is None or environment not in DataSchema.DATA_MAPPINGS:
            raise LookupError(
                "Invalid value for environment. Expected one of {}".format(
                    ", ".join(cls.get_data_environments())
                )
            )

        env = DataSchema.DATA_MAPPINGS[environment]
        if data_family is None or data_family not in env:
            raise LookupError(
                "Invalid value for data_family. Expected one of {}".format(
                    ", ".join(cls.get_data_families())
                )
            )

        return env[data_family]

    @classmethod
    def get_data_environments(cls):
        """Return the environments defined in the schema."""
        return list({DataEnvironment(env_name).name for env_name in cls.DATA_MAPPINGS})

    @classmethod
    def get_data_families(cls):
        """Return the data families in the schema."""
        families = set()
        for _, data in cls.DATA_MAPPINGS.items():
            for data_map in data:
                families.add(DataFamily(data_map).name)

        return list(families)
