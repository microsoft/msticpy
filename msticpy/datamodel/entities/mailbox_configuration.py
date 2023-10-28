# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MailboxConfiguration Entity class."""
from typing import Any, Mapping, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class MailboxConfiguration(Entity):
    """
    MailboxConfiguration Entity class.

    Attributes
    ----------
    ConfigType : str
        The	MailboxConfigurationType represented by this entity.
        Could be of type: MailForwardingRule, OwaSettings, EWSSettings MailDelegation
        or UserInboxRule
    MailboxPrimaryAddress : str
        PrimaryAddress of the Mailbox
    DisplayName : str
        DisplayName of the Mailbox
    Upn : str
        Upn of the Mailbox
    ConfigId : str
        A mailbox can have more than one configuration entity of same configurationType.
        This unique id equivalent to URN.
    ExternalDirectoryObjectId : str
        ExternalDirectoryObjectId of the Mailbox

    """

    ID_PROPERTIES = ["ConfigId"]

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
        self.MailboxPrimaryAddress: Optional[str] = None
        self.DisplayName: Optional[str] = None
        self.Upn: Optional[str] = None
        self.ExternalDirectoryObjectId: Optional[str] = None
        self.ConfigType: Optional[str] = None
        self.ConfigId: Optional[str] = None

        super().__init__(src_entity=src_entity, **kwargs)
        if src_event:
            self._create_from_event(src_event)

    def _create_from_event(self, src_event):
        self.MailboxPrimaryAddress = src_event.get("MailboxPrimaryAddress")
        self.Upn = src_event.get("Upn")
        self.DisplayName = src_event.get("DisplayName")
        self.ConfigId = src_event.get("ConfigId")
        self.ExternalDirectoryObjectId = src_event.get("ExternalDirectoryObjectId")
        self.ConfigType = src_event.get("ConfigType")

    @property
    def description_str(self):
        """Return Entity Description."""
        return (
            f"{self.MailboxPrimaryAddress} - {self.ConfigId}" or self.__class__.__name__
        )

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.MailboxPrimaryAddress or self.__class__.__name__

    _entity_schema = {
        "MailboxPrimaryAddress": None,
        "DisplayName": None,
        "Upn": None,
        "ExternalDirectoryObjectId": None,
        "RiskLevel": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
