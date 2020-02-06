# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Folium map class."""
from typing import Iterable, List, Tuple, Union
from numbers import Number
import statistics as stats
import warnings

import folium

# pylint: enable=locally-disabled, unused-import
from .utility import export
from .entityschema import IpAddress, GeoLocation, Entity
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-many-arguments, too-few-public-methods
@export
class FoliumMap:
    """Wrapper class for Folium/Leaflet mapping."""

    def __init__(
        self,
        title: str = "layer1",
        zoom_start: float = 2.5,
        tiles=None,
        width: str = "100%",
        height: str = "100%",
        location: list = None,
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
        location : list, optional
            Location to center map on

        Attributes
        ----------
        folium_map : folium.Map
            The map object.

        """
        if not location:
            location = [47.67, -122.13]

        self.folium_map = folium.Map(
            zoom_start=zoom_start,
            tiles=tiles,
            width=width,
            height=height,
            location=location,
        )
        folium.TileLayer(name=title).add_to(self.folium_map)
        self.locations: List[Tuple[Number, Number]] = []

    def _repr_html_(self):
        """Return folium map as HTML."""
        # pylint: disable=protected-access
        return self.folium_map._repr_html_()
        # pylint: enable=protected-access

    def center_map(self):
        """Calculate and set map center based on current coordinates."""
        self.folium_map.location = _get_center_coords(self.locations)

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
        geo_entity = GeoLocation()  # type: ignore
        geo_entity.CountryCode = "Unknown"  # type: ignore
        geo_entity.CountryName = "Unknown"  # type: ignore
        geo_entity.State = "Unknown"  # type: ignore
        geo_entity.City = "Unknown"  # type: ignore
        geo_entity.Longitude = 0.0  # type: ignore
        geo_entity.Latitude = 0.0  # type: ignore

        for ip_entity in ip_entities:
            if ip_entity.Location is None:
                ip_entity.Location = geo_entity  # type: ignore

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
            self.locations.append(
                (ip_entity.Location.Latitude, ip_entity.Location.Longitude)
            )


def get_map_center(entities: List[Entity], mode: str = "modal"):
    """
    Calculate median point between IP Entity locations.

    Parameters
    ----------
    ip_entities: list
        A list of ip entities to get a center point from
    mode : str, optional
        The averaging method to use, by default "modal"
        "modal" and "mean" are the supported values.

    Returns
    -------
    Tuple
        The Lattitude and Longitude calculated

    """
    ip_entities: List[IpAddress] = []
    loc_entities: List[GeoLocation] = []
    if not entities:
        return (0, 0)
    if isinstance(entities[0], IpAddress):
        return get_center_ip_entities(entities)  # type: ignore
    loc_props = [
        p_name
        for p_name, p_val in entities[0].properties.items()
        if isinstance(p_val, (IpAddress, GeoLocation))
    ]
    for entity in entities:
        for prop in loc_props:
            loc_entity = entity[prop]
            if isinstance(loc_entity, IpAddress):
                ip_entities.append(loc_entity)
            elif isinstance(loc_entity, GeoLocation):
                loc_entities.append(loc_entity)
    locs_ips = _extract_locs_ip_entities(ip_entities)
    return get_center_geo_locs(locs_ips + loc_entities, mode=mode)


def _extract_locs_ip_entities(ip_entities: List[IpAddress]):
    return [ip["Location"] for ip in ip_entities if bool(ip.Location)]


def get_center_ip_entities(
    ip_entities: List[IpAddress], mode: str = "modal"
) -> Tuple[Union[int, float], Union[int, float]]:
    """
    Return the geographical center of the IP address locations.

    Parameters
    ----------
    ip_entities : List[IpAddress]
        IpAddress entities with location information
    mode : str, optional
        The averaging method to us, by default "modal"
        "modal" and "mean" are the supported values.

    Returns
    -------
    Tuple[Union[int, float], Union[int, float]]
        Tuple of latitude, longitude

    """
    ip_locs_longs = _extract_locs_ip_entities(ip_entities)
    return get_center_geo_locs(ip_locs_longs, mode=mode)


def _extract_coords_loc_entities(loc_entities: List[GeoLocation]):
    return [
        (loc["Latitude"], loc["Longitude"])
        for loc in loc_entities
        if "Latitude" in loc and "Logitude" in loc
    ]


def get_center_geo_locs(
    loc_entities: List[GeoLocation], mode: str = "modal"
) -> Tuple[Union[int, float], Union[int, float]]:
    """
    Return the geographical center of the geo locations.

    Parameters
    ----------
    loc_entities : List[GeoLocation]
        GeoLocation entities with location information
    mode : str, optional
        The averaging method to use, by default "modal"
        "modal" and "mean" are the supported values.

    Returns
    -------
    Tuple[Union[int, float], Union[int, float]]
        Tuple of latitude, longitude

    """
    lat_longs = _extract_coords_loc_entities(loc_entities)
    return _get_center_coords(lat_longs, mode=mode)


def _get_center_coords(
    locations: Iterable[Tuple[Union[int, float], Union[int, float]]],
    mode: str = "modal",
) -> Tuple[Union[int, float], Union[int, float]]:
    locs = list(locations)
    if mode == "modal":
        try:
            return (
                stats.mode([loc[0] for loc in locs]),
                stats.mode([loc[1] for loc in locs]),
            )
        except stats.StatisticsError:
            pass
    return (stats.mean([loc[0] for loc in locs]), stats.mean([loc[1] for loc in locs]))
