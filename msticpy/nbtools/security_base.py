# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for SecurityAlert class."""
import html
import re
from collections import Counter
from datetime import datetime
from typing import Any, Dict, List, Optional, Union

import pandas as pd
from deprecated.sphinx import deprecated

from .._version import VERSION
from ..common.utility import escape_windows_path, export
from ..data.core.query_defns import DataEnvironment, QueryParamProvider
from ..datamodel.entities import Account, Entity, Host, OSFamily, Process

__version__ = VERSION
__author__ = "Ian Hellen"

_ID_PROPERTIES: List[str] = [
    "AzSubscriptionId",
    "AzResourceId",
    "WorkspaceId",
    "AgentId",
    "TenantId",
    "SourceComputerId",
    "ResourceId",
    "WorkspaceSubscriptionId",
    "WorkspaceResourceGroup",
    "ProviderAlertId",
    "SystemAlertId",
    "ResourceId",
]


# pylint: disable=too-many-public-methods
@deprecated(reason="Replaced by Alert entity in datamodel", version="1.7.0")
@export
class SecurityBase(QueryParamProvider):
    """
    Security Base Class for alerts and events.

    Instantiates a security event or alert from a pandas Series
    and provides convenience access methods to retrieve properties.
    """

    def __init__(self, src_row: pd.Series = None):
        """Instantiate a security alert from a pandas Series."""
        self._source_data: pd.Series = (
            src_row if src_row is not None else pd.Series(dtype="object")
        )
        self._custom_query_params: Dict[str, Any] = {}
        self._entities: List[Entity] = []

        # Extract and cache alert ID properties
        self._ids: Dict[str, str] = {}
        if self._source_data is not None:
            for id_property in _ID_PROPERTIES:
                if id_property in self._source_data:
                    self._ids[id_property] = self._source_data[id_property]

        self.path_separator = "\\"
        self.os_family = "Windows"

    # Dunder methods
    def __getitem__(self, key):
        """Allow property get using dictionary key syntax."""
        if key in self.__dict__:
            return self.__dict__[key]
        if key in self._source_data:
            return self._source_data[key]
        raise KeyError

    def __setitem__(self, key, value):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key] = value

    def __contains__(self, key):
        """Allow property 'in' test."""
        # get attribute from source dictionary if not defined
        return key in self.__dict__ or key in self._source_data

    def __getattr__(self, name):
        """Return the value of the named property 'name'."""
        if self._source_data is not None and name in self._source_data:
            return self._source_data[name]
        raise AttributeError(f"{name} is not a valid attribute.")

    def __str__(self):
        """Return string representation of object properties."""
        str_props = [f"{prop}: {val}" for prop, val in self._source_data.items()]

        if self.entities:
            str_entities = [str(ent).replace("\n", ", ") for ent in self.entities]
            str_props += str_entities
        return "\n".join(str_props)

    def __repr__(self) -> str:
        """Return repr of item."""
        if self.properties:
            params = ", ".join(
                [f"{name}={val}" for name, val in self.properties.items()]
            )
            if len(params) > 80:
                params = params[:80] + "..."
            return f"{self.__class__.__name__}({params})"
        return f"{self.__class__.__name__}()"

    def _repr_html_(self) -> str:
        """Display in IPython."""
        return self.to_html()

    # def __getstate__(self):
    #     """Return dictionary of state for serialization/pickling."""
    #     state_dict = {}
    #     state_dict['source_data'] = (self._source_data)
    #     state_dict['Entities'] = self._entities
    #     state_dict['path_separator'] = self.path_separator
    #     state_dict['os_family'] = self.os_family
    #     return state_dict

    # def __setstate__(self, state):
    #     """Set state from dictionary for deserialization/unpickling."""
    #     self._source_data = state['source_data']
    #     self._entities = list(state['Entities'])
    #     self.path_separator = state['path_separator']
    #     self.os_family = state['os_family']

    # Properties
    @property
    def entities(self) -> List[Entity]:
        """
        Return a list of the Alert or Event entities.

        Returns
        -------
        List[Entity]
            List of the Alert or Event entities.

        """
        return self._entities

    @property
    def properties(self) -> Dict[str, Any]:
        """
        Return a dictionary of the Alert or Event properties.

        Returns
        -------
        Dict[str, Any]
            dictionary of the Alert or Event properties.

        """
        return self._source_data.to_dict()

    @property
    def hostname(self) -> str:
        """Return the Hostname (not FQDN) of the host associated with the alert."""
        return self.primary_host.HostName if self.primary_host is not None else None

    @property
    def computer(self) -> Optional[str]:
        """
        Return the Computer name of the host associated with the alert.

        (host FQDN, if available)
        """
        return self.primary_host.computer if self.primary_host is not None else None

    @property
    def ids(self) -> Dict[str, str]:
        """Return a collection of Identity properties for the alert."""
        return self._ids

    @property
    def is_in_workspace(self) -> bool:
        """Return True if the alert has a Log Analytics WorkspaceID."""
        return "WorkspaceId" in self._ids and "AgentId" in self._ids

    @property
    def is_in_log_analytics(self) -> bool:
        """Return True if the alert originates from a Log Analytics Workspace host."""
        return "TenantId" in self._ids

    @property
    def is_in_azure_sub(self) -> bool:
        """Return True if the alert originates from an Azure Security Center host."""
        if (
            "AzSubscriptionId" not in self._ids
            and "AzResourceId" not in self._ids
            and "ResourceId" in self._ids
            and self._ids["ResourceId"]
        ):
            self._ids["AzResourceId"] = self._id["ResourceId"]
            res = self._get_subscription_from_resource(self._id["ResourceId"])
            if res:
                self._ids["AzSubscriptionId"] = res

        return "AzSubscriptionId" in self._ids and "AzResourceId" in self._ids

    @property
    def primary_host(self) -> Optional[Union[Host, Entity]]:
        """
        Return the primary host entity (if any) associated with this object.

        Returns
        -------
        Optional[Host]
            primary host entity (if any)

        """
        hosts = self.get_entities_of_type("host")
        if hosts:
            return hosts[0]
        return None

    @property
    def primary_process(self) -> Optional[Union[Process, Entity]]:
        """
        Return the primary process entity (if any) associated with this object.

        Returns
        -------
        Optional[Process]
            primary process entity (if any)

        """
        procs = self.get_entities_of_type("process")
        if not procs:
            return None
        if len(procs) == 1:
            return procs[0]

        # find the first process that has a parent process property
        procs_with_parent = [
            proc
            for proc in procs
            if "ParentProcess" in proc
            and proc["ParentProcess"]
            and proc.ParentProcess.ProcessId in {proc.ProcessId for proc in procs}
        ]
        return procs_with_parent[0] if procs_with_parent else procs[0]

    @property
    def primary_account(self) -> Optional[Union[Process, Entity]]:
        """
        Return the primary account entity (if any) associated with this object.

        Returns
        -------
        Optional[Process]
            primary account entity (if any)

        """
        accts = self.get_entities_of_type("account")
        return accts[0] if accts else None

    @property
    def query_params(self) -> Dict[str, Any]:
        """
        Query parameters derived from alert.

        Returns
        -------
        Dict[str, Any]
            Dictionary of parameter names/values

        """
        try:
            host_name = self.primary_host.fqdn if self.primary_host else None
            proc_name = (
                self.primary_process.ImageFile.FullPath
                if self.primary_process and self.primary_process.ImageFile
                else None
            )
            acct_name = self.primary_account.Name if self.primary_account else None
            path_separator = self.path_separator
            if self.data_family == OSFamily.Windows:
                proc_name = escape_windows_path(proc_name)
                path_separator = escape_windows_path(self.path_separator)

            dyn_query_params = {
                "subscription_filter": self.subscription_filter(),
                "host_filter_eq": self.host_filter(operator="=="),
                "host_filter_neq": self.host_filter(operator="!="),
                "host_name": host_name,
                "account_name": acct_name,
                "process_name": proc_name,
                "logon_session_id": self.get_logon_id(),
                "process_id": (
                    self.primary_process.ProcessId if self.primary_process else None
                ),
                "path_separator": path_separator,
                "data_family": self.data_family,
                "data_environment": self.data_environment,
            }

            dyn_query_params.update(self._custom_query_params)
            return dyn_query_params
        except AttributeError:
            return {}

    @property
    def data_family(self) -> OSFamily:
        """Return the data family of the alert for subsequent queries."""
        if self.os_family == "Linux":
            return OSFamily.Linux
        if self.os_family == "Windows":
            return OSFamily.Windows
        raise ValueError("Unknown Data family.")

    @property
    def data_environment(self) -> DataEnvironment:
        """Return the data environment of the alert for subsequent queries."""
        if self.is_in_log_analytics:
            return DataEnvironment.LogAnalytics
        return DataEnvironment.Kusto

    @property
    def origin_time(self) -> datetime:
        """Return the datetime of event."""
        return self.TimeGenerated

    def get_logon_id(self, account: Account = None) -> Optional[Union[str, int]]:
        """
        Get the logon Id for the alert or the account, if supplied.

        If `account` is not supplied, return the logon id
        of the first host-logon-session or account entity.

        Parameters
        ----------
        account : Account, optional
            Account objec to use (the default is None)

        Returns
        -------
        Optional[Union[str, int]]
            The logon Id for primary account

        """
        for session in [
            e
            for e in self.entities
            if e["Type"] in ["host-logon-session", "hostlogonsession"]
        ]:
            if account is None or session["Account"] == account:
                return session["SessionId"]
        if account is None:
            for acct in [
                e for e in self.entities if e["Type"] == "account" and "LogonId" in e
            ]:
                return acct["LogonId"]
        elif "LogonId" in account:
            return account["LogonId"]
        return None

    def subscription_filter(self, operator="=="):
        """Return a KQL subscription filter clause derived from the alert properties."""
        if self.is_in_log_analytics:
            return "true"
        if self.is_in_azure_sub:
            return (
                f"AzureResourceSubscriptionId {operator} "
                f"'{self._ids['AzSubscriptionId']}'"
            )
        if self.is_in_workspace:
            return f"WorkspaceId {operator} '{self._ids['WorkspaceId']}'"

        # Otherwise we default to including everything
        return "true"

    def host_filter(self, operator="=="):
        """
        Return a KQL host filter clause derived from the alert properties.

            :param operator='==': the operator to use in the filter clause.
                '==' and '!=' typically.
        """
        if self.primary_host:
            case_insens_op = "=~" if operator == "==" else "!~"
            return f"Computer {case_insens_op} '{self.primary_host.computer}'"

        if (
            self.is_in_log_analytics
            and "SourceComputerId" in self._ids
            and self._ids["SourceComputerId"]
        ):
            return f"SourceComputerId {operator} '{self._ids['SourceComputerId']}'"
        if (
            self.is_in_azure_sub
            and "AzureResourceId" in self._ids
            and self._ids["AzResourceId"]
        ):
            return f"AzureResourceId {operator} '{self._ids['AzResourceId']}'"
        if self.is_in_workspace and "AgendId" in self._ids and self._ids["AgentId"]:
            return f"AgentId {operator} '{self._ids['AgentId']}'"
        return None

    def get_entities_of_type(self, entity_type: str) -> List[Entity]:
        """
        Return entity collection for a give entity type.

        Parameters
        ----------
        entity_type : str, optional
            The entity type.

        Returns
        -------
        List[Entity]
            The entities matching `entity_type`.

        """
        class_type = Entity.ENTITY_NAME_MAP.get(entity_type, None)
        return [
            p
            for p in self.entities
            if p["Type"] == entity_type or class_type and isinstance(p, class_type)
        ]

    def get_all_entities(self) -> pd.DataFrame:
        """
        Return a DataFrame of the Alert or Event entities.

        Returns
        -------
        DataFrame
            Pandas DataFrame of the Alert or Event entities.

        """
        entity = []
        ent_type = []
        for item in self.entities:
            if "Address" in item:
                entity.append(item["Address"])
                ent_type.append(item["Type"])
            elif "Url" in item:
                entity.append(item["Url"])
                ent_type.append(item["Type"])
            elif "HostName" in item:
                entity.append(item["HostName"])
                ent_type.append(item["Type"])
            elif "Entity" in item:
                entity.append(item["Entity"])
                ent_type.append(item["Type"])
            elif item["Type"] == "account":
                entity.append(item["Name"])
                ent_type.append(item["Type"])

        return pd.DataFrame({"Entity": entity, "Type": ent_type})

    def to_html(self, show_entities: bool = False) -> str:
        """Return the item as HTML string."""
        html_doc = pd.DataFrame(self._source_data).to_html()

        if self._source_data is not None and "ExtendedProperties" in self._source_data:
            ext_prop_title = "<br/><h3>ExtendedProperties:</h3>"
            ext_prop_html = pd.DataFrame(
                pd.Series(self._source_data["ExtendedProperties"])
            ).to_html()
            html_doc = html_doc + ext_prop_title + ext_prop_html

        if show_entities and self.entities:
            entity_title = "<br/><h3>Entities:</h3><br/>"
            entity_html = "<br/>".join(
                [self._format_entity(ent) for ent in self.entities]
            )
            html_doc = html_doc + entity_title + entity_html
        else:
            e_counts = Counter([ent["Type"] for ent in self.entities])
            e_counts_str = ", ".join([f"{e}: {c}" for e, c in e_counts.items()])
            html_doc = html_doc + f"<h3>Entity counts: </h3>{e_counts_str}"
        return html_doc

    @staticmethod
    def _format_entity(entity):
        str_entity = str(entity)
        if str_entity:
            str_entity = str_entity.replace("\n", ", ")
        return html.escape(str_entity)

    def _find_os_family(self):
        """Return OSFamily and path separator to use from entities or file paths."""
        self.path_separator = "\\"
        self.os_family = "Windows"

        # Use OSFamily if any entities have this property set
        os_family_entities = [e for e in self.entities if "OSFamily" in e]
        if os_family_entities:
            for os_entity in os_family_entities:
                if os_entity["OSFamily"] == "Linux":
                    self.os_family = "Linux"
                    self.path_separator = "/"
                    break
        else:
            # Otherwise try to infer from the file paths
            files = [e for e in self.entities if e["Type"] == "file"]
            if files:
                for file in files:
                    if "Directory" in file and "/" in file["Directory"]:
                        self.os_family = "Linux"
                        self.path_separator = "/"
                        break
            else:
                for proc in [
                    e
                    for e in self.entities
                    if e["Type"] == "process" and "ImageFile" in e
                ]:
                    file = proc["ImageFile"]
                    if "Directory" in file and "/" in file["Directory"]:
                        self.os_family = "Linux"
                        self.path_separator = "/"
                        break

    @staticmethod
    def _get_subscription_from_resource(resource_id) -> Optional[str]:
        """Extract subscription Id from resource string."""
        sub_regex = r"^/subscriptions/([^/]+)/"
        sub_ids = re.findall(sub_regex, resource_id, re.RegexFlag.I)
        if sub_ids:
            return sub_ids[0]

        return None
