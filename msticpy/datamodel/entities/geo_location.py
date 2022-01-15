# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""GeoLocation Entity class."""
from typing import Any, Mapping, Optional, Tuple

from ..._version import VERSION
from ...common.utility import export
from .entity import ContextObject, Entity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class GeoLocation(Entity, ContextObject):
    """
    GeoLocation class.

    Attributes
    ----------
    CountryCode : str
        GeoLocation CountryCode
    CountryName : str
        GeoLocation CountryName
    State : str
        GeoLocation State
    City : str
        GeoLocation City
    Longitude : float
        GeoLocation Longitude
    Latitude : float
        GeoLocation Latitude
    Asn : str
        GeoLocation Asn

    """

    ID_PROPERTIES = ["Longitude", "Latitude", "City", "State", "CountryCode"]

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
        self.CountryCode: Optional[str] = None
        self.CountryName: Optional[str] = None
        self.State: Optional[str] = None
        self.City: Optional[str] = None
        self.Longitude: Optional[float] = None
        self.Latitude: Optional[float] = None
        self.Asn: Optional[str] = None
        super().__init__(src_entity=src_entity, **kwargs)

    @property
    def description_str(self) -> str:
        """Return Entity Description."""
        return f"{self.CountryCode}; {self.State}; {self.City}"

    @property
    def name_str(self) -> str:
        """Return Entity Name."""
        return self.CountryCode or self.__class__.__name__

    @property
    def coordinates(self) -> Tuple[float, float]:
        """Return Latitude/Longitude as a tuple of floats."""
        if self.Latitude and self.Longitude:
            return self.Latitude, self.Longitude
        return (0.0, 0.0)

    _entity_schema = {
        # str
        "CountryCode": None,
        # str
        "CountryName": None,
        # str
        "State": None,
        # str
        "City": None,
        # double?
        "Longitude": None,
        # double?
        "Latitude": None,
        # int
        "Asn": None,
        "TimeGenerated": None,
        "StartTime": None,
        "EndTime": None,
    }
