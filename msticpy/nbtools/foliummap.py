# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Folium map class."""
from collections.abc import Iterable
from numbers import Number
import warnings

import folium

# pylint: enable=locally-disabled, W0611
from . utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


@export
class FoliumMap(object):
    """Wrapper class for Folium/Leaflet mapping."""

    def __init__(self, title: str = 'layer1', zoom_start: int = 7, tiles=None,
                 width: str = '100%', height: str = '100%'):
        """
        Create an instance of the folium map.

        Keyword Arguments:
            title {str} -- Name of the layer (default: {'layer1'})
            zoom_start {int} -- The zoom level of the map (default: {7})
            tiles {[type]} -- Custom set of tiles or tile URL (default: {None})
            width {str} -- Map display width (default: {'100%'})
            height {str} -- Map display height (default: {'100%'})

        """
        self.folium_map = folium.Map(
            zoom_start=zoom_start, tiles=tiles, width=width, height=height)
        self.folium_map.add_tile_layer(name=title)

    def add_ip_cluster(self, ip_entities: Iterable, **kwargs):
        """
        Add a collection of IP Entities to the map.

        Arguments:
            ip_entities {Iterable} -- a list of IpAddress Entities

        Keyword Arguments:
            kwargs: icon properties to use for displaying this cluster

        """
        for ip_entity in ip_entities:
            if not (isinstance(ip_entity.Location.Latitude, Number) and
                    isinstance(ip_entity.Location.Longitude, Number)):
                warnings.warn("Invalid location information for IP: " + ip_entity.Address,
                              RuntimeWarning)
                continue
            loc_props = ', '.join([f'{key}={val}' for key, val in
                                   ip_entity.Location.properties.items() if val])
            popup_text = "{loc_props}<br>{IP}".format(IP=ip_entity.Address,
                                                      loc_props=loc_props)
            tooltip_text = '{City}, {CountryName}'.format(
                **ip_entity.Location.properties)

            if ip_entity.AdditionalData:
                addl_props = ', '.join([f'{key}={val}' for key, val in
                                        ip_entity.AdditionalData.items() if val])
                popup_text = f'{popup_text}<br>{addl_props}'
                tooltip_text = f'{tooltip_text}, {addl_props}'
            marker = folium.Marker(
                location=[ip_entity.Location.Latitude,
                          ip_entity.Location.Longitude],
                popup=popup_text,
                tooltip=tooltip_text,
                icon=folium.Icon(**kwargs)
            )
            marker.add_to(self.folium_map)
