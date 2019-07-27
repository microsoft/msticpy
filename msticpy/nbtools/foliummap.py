# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Folium map class."""
from typing import Iterable
from numbers import Number
import warnings

import folium

# pylint: enable=locally-disabled, unused-import
from .utility import export
from .entityschema import IpAddress
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-arguments
@export
class FoliumMap:
    """Wrapper class for Folium/Leaflet mapping."""

    def __init__(
        self,
        title: str = "layer1",
        zoom_start: int = 7,
        tiles=None,
        width: str = "100%",
        height: str = "100%",
    ):
        """
        Create an instance of the folium map.

        Parameters
        ----------
        title : str, optional
            Name of the layer (the default is 'layer1')
        zoom_start : int, optional
            The zoom level of the map (the default is 7)
        tiles : [type], optional
            Custom set of tiles or tile URL (the default is None)
        width : str, optional
            Map display width (the default is '100%')
        height : str, optional
            Map display height (the default is '100%')

        Attributes
        ----------
        folium_map : folium.Map
            The map object.

        """
        self.folium_map = folium.Map(
            zoom_start=zoom_start, tiles=tiles, width=width, height=height
        )
        folium.TileLayer(name=title).add_to(self.folium_map)

    def __repr__(self):
        """Return folium map."""
        return self.folium_map

    def add_ip_cluster(self, ip_entities: Iterable[IpAddress], **kwargs):
        """
        Add a collection of IP Entities to the map.

        Parameters
        ----------
        ip_entities : Iterable[IpAddress]
            a iterable of IpAddress Entities

        Other Parameters
        ----------------
            kwargs: icon properties to use for displaying this cluster

        """
        for ip_entity in ip_entities:
            if not (
                isinstance(ip_entity.Location.Latitude, Number)
                and isinstance(ip_entity.Location.Longitude, Number)
            ):
                warnings.warn(
                    "Invalid location information for IP: " + ip_entity.Address,
                    RuntimeWarning,
                )
                continue
            loc_props = ", ".join(
                [
                    f"{key}={val}"
                    for key, val in ip_entity.Location.properties.items()
                    if val
                ]
            )
            popup_text = "{loc_props}<br>{IP}".format(
                IP=ip_entity.Address, loc_props=loc_props
            )
            tooltip_text = "{City}, {CountryName}".format(
                **ip_entity.Location.properties
            )

            if ip_entity.AdditionalData:
                addl_props = ", ".join(
                    [
                        f"{key}={val}"
                        for key, val in ip_entity.AdditionalData.items()
                        if val
                    ]
                )
                popup_text = f"{popup_text}<br>{addl_props}"
                tooltip_text = f"{tooltip_text}, {addl_props}"
            marker = folium.Marker(
                location=[ip_entity.Location.Latitude, ip_entity.Location.Longitude],
                popup=popup_text,
                tooltip=tooltip_text,
                icon=folium.Icon(**kwargs),
            )
            marker.add_to(self.folium_map)
