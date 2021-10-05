# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MailCluster Entity class."""
from typing import Any, Dict, List, Mapping, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name, too-many-instance-attributes


@export
class MailCluster(Entity):
    """
    MailCluster Entity class.

    Attributes
    ----------
    NetworkMessageIds : List[str]
        NetworkMessageIds of the Mail cluster
    CountByDeliveryStatus : Dict[str, int] = {}
        CountByDeliveryStatus of the Mail cluster
    CountByThreatType : Dict[str, int] = {}
        CountByThreatType of the Mail cluster
    CountByProtectionStatus : Dict[str, int] = {}
        CountByProtectionStatus of the Mail cluster
    Threats : List[str]
        Threats of the Mail cluster
    Query : str
        Query of the Mail cluster
    QueryTime : datetime
        QueryTime of the Mail cluster
    MailCount : int
        MailCount of the Mail cluster
    IsVolumeAnomaly : bool
        IsVolumeAnomaly of the Mail cluster
    Source : str
        Source of the Mail cluster
    ClusterSourceIdentifier : str
        ClusterSourceIdentifier of the Mail cluster
    ClusterSourceType : str
        ClusterSourceType of the Mail cluster
    ClusterQueryStartTime : datetime
        ClusterQueryStartTime of the Mail cluster
    ClusterQueryEndTime : datetime
        ClusterQueryEndTime of the Mail cluster
    ClusterGroup : str
        ClusterGroup of the Mail cluster

    """

    ID_PROPERTIES = ["Query", "Source"]

    def __init__(
        self,
        src_entity: Mapping[str, Any] = None,
        src_event: Mapping[str, Any] = None,
        **kwargs,
    ):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)
        src_event : Mapping[str, Any], optional
            Create entity from event properties
            (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.NetworkMessageIds: List[str] = []
        self.CountByDeliveryStatus: Dict[str, int] = {}
        self.CountByThreatType: Dict[str, int] = {}
        self.CountByProtectionStatus: Dict[str, int] = {}
        self.Threats: List[str] = []
        self.Query: Optional[str] = None
        self.QueryTime: Any = None
        self.MailCount: int = 0
        self.IsVolumeAnomaly: bool = False
        self.Source: Optional[str] = None
        self.ClusterSourceIdentifier: Optional[str] = None
        self.ClusterSourceType: Optional[str] = None
        self.ClusterQueryStartTime: Any = None
        self.ClusterQueryEndTime: Any = None
        self.ClusterGroup: Optional[str] = None

        super().__init__(src_entity=src_entity, **kwargs)
        if src_event is not None:
            self._create_from_event(src_event)

    def _create_from_event(self, src_event):
        self.NetworkMessageIds = src_event["NetworkMessageIds"]
        self.CountByThreatType = src_event["CountByThreatType"]
        self.CountByProtectionStatus = src_event["CountByProtectionStatus"]
        self.Query = src_event["Query"]
        self.QueryTime = src_event["QueryTime"]
        self.MailCount = src_event["MailCount"]
        self.Source = src_event["Source"]

    @property
    def description_str(self):
        """Return Entity Description."""
        return self.Query or self.NetworkMessageIds or self.__class__.__name__

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        hash_val = hash(str(self.NetworkMessageIds))
        return f"{self.__class__.__name__} - {hash_val}"

    _entity_schema = {
        "NetworkMessageIds": None,
        "CountByDeliveryStatus": None,
        "CountByThreatType": None,
        "CountByProtectionStatus": None,
        "Threats": None,
        "Query": None,
        "QueryTime": None,
        "MailCount": None,
        "IsVolumeAnomaly": None,
        "Source": None,
        "ClusterSourceIdentifier": None,
        "ClusterSourceType": None,
        "ClusterQueryStartTime": None,
        "ClusterQueryEndTime": None,
        "ClusterGroup": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
