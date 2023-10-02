# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""OAuthApplication Entity class."""
from typing import Any, List, Mapping, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class OAuthApplication(Entity):
    """
    OAuthApplication Entity class.

    Attributes
    ----------
    OAuthAppId : str
        Global ID of the application (in AAD, "Application ID" of the application object)
    OAuthObjectId : str
        Object ID of the service principal in AAD
    Name : str
        Name of the app
    TenantId : str
        AAD tenant ID in which the app was installed
    PublisherName : str
        The publisher name of the app
    Risk: str
        The app risk - like Low, Medium, High or Unknown
    Permissions: List[str]
        List of permissions that were requested by the application, and their severities
    RedirectURLs : List[str]
        List of redirect urls
    AuthorizedBy : int
        How many users consented the app

    """

    ID_PROPERTIES = ["OAuthAppId", "OAuthObjectId"]

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of the entity type.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            Create entity from existing entity or
            other mapping object that implements entity properties.
            (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        self.OAuthAppId: Optional[str] = None
        self.OAuthObjectId: Optional[str] = None
        self.Name: Optional[str] = None
        self.TenantId: Optional[str] = None
        self.PublisherName: Optional[str] = None
        self.Risk: Optional[str] = None
        self.Permissions: List[str] = []
        self.RedirectURLs: List[str] = []
        self.AuthorizedBy: int = 0

        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self):
        """Return Entity Description."""
        return self.Name or self.OAuthAppId or self.__class__.__name__

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.Name or self.OAuthAppId or self.__class__.__name__

    _entity_schema = {
        # OAuthAppId (type System.String)
        "OAuthAppId": None,
        # OAuthObjectId (type System.String)
        "OAuthObjectId": None,
        # TenantId (type System.String)
        "TenantId": None,
        "PublisherName": None,
        "Risk": None,
        "Permissions": None,
        "RedirectURLs": None,
        "AuthorizedBy": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
