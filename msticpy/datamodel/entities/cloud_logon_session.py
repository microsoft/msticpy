# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""CloudApplication Entity class."""
from typing import Any, Mapping, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Pete Bryan"


# pylint: disable=invalid-name


@export
class CloudLogonSession(Entity):
    """
    CloudLogonSession Entity class.

    Attributes
    ----------
    SessionId : str
        The loggon session ID
    Account : str
        The Account
    UserAgent : str
        The UserAgent
    StartTime: str
        The time the session started

    """

    ID_PROPERTIES = ["Name"]

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
        src_event:  Mapping[str, Any], optional
            Create entity from an event

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.SessionId: Optional[str] = None
        self.Account: Optional[str] = None
        self.UserAgent: Optional[str] = None
        self.StartTime: Optional[str] = None
        super().__init__(src_entity=src_entity, **kwargs)
        if src_event:
            self._create_from_event(src_event)

    def _create_from_event(self, src_event):
        self.SessionId = src_event.get("SessionId")
        self.Account = src_event.get("Account")
        self.UserAgent = src_event.get("UserAgent")
        self.StartTime = src_event.get("StartTimeUtc")

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return self.Account or self.__class__.__name__

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        desc = f"{self.StartTime} - {self.Account} - {self.UserAgent}"
        return desc or self.__class__.__name__

    _entity_schema = {
        # Name (type System.String)
        "SessionId": None,
        "Account": None,
        "UserAgent": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
