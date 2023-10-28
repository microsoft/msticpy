# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""ServicePrincipal Entity class."""
from typing import Any, Mapping, Optional

from ..._version import VERSION
from ...common.utility import export
from .entity import Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class ServicePrincipal(Entity):
    """
    ServicePrincipal Entity class.

    Attributes
    ----------
    ServicePrincipalName : str
        ServicePrincipal DistinguishedName
    ServicePrincipalObjectId : str
        ServicePrincipal UUID
    AppId : str
        SecurityGroup ObjectGuid
    AppOwnerTenantId : str
        The tenant id where the application is registered.
        This is applicable only to service principals backed by applications.
    TenantId : str
        The AAD tenant id of Service Principal
    ServicePrincipalType : str
        The type of the service principal: 'Unknown', 'Application',
        'ManagedIdentity', 'Legacy'

    """

    ID_PROPERTIES = ["DistinguishedName", "SID", "ObjectGuid"]

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
        self.ServicePrincipalName: Optional[str] = None
        self.ServicePrincipalObjectId: Optional[str] = None
        self.AppId: Optional[str] = None
        self.AppOwnerTenantId: Optional[str] = None
        self.TenantId: Optional[str] = None
        self.ServicePrincipalType: Optional[str] = None

        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self):
        """Return Entity Description."""
        return self.DistinguishedName or self.__class__.__name__

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.__class__.__name__

    _entity_schema = {
        # ServicePrincipalName (type System.String)
        "ServicePrincipalName": None,
        # ServicePrincipalObjectId (type System.String)
        "ServicePrincipalObjectId": None,
        # AppId (type System.String)
        "AppId": None,
        "AppOwnerTenantId": None,
        "TenantId": None,
        "ServicePrincipalType": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
