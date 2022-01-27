# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Folium map class."""
import math
import statistics as stats
import warnings
from typing import Iterable, List, Tuple

import folium
from folium.plugins import MarkerCluster, FeatureGroupSubGroup
import pygeohash

from .._version import VERSION
from ..datamodel.entities import Entity, GeoLocation, IpAddress

# pylint: enable=locally-disabled, unused-import
from ..common.utility import export

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
        self.locations: List[Tuple[float, float]] = []

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
        geo_entity = GeoLocation()
        geo_entity.CountryCode = "Unknown"
        geo_entity.CountryName = "Unknown"
        geo_entity.State = "Unknown"
        geo_entity.City = "Unknown"
        geo_entity.Longitude = 0.0
        geo_entity.Latitude = 0.0

        for ip_entity in ip_entities:
            if ip_entity.Location is None:
                ip_entity.Location = geo_entity

        for ip_entity in ip_entities:
            if ip_entity.Location is None:
                continue
            if (
                not (
                    isinstance(ip_entity.Location.Latitude, (int, float))
                    and isinstance(ip_entity.Location.Longitude, (int, float))
                )
                or math.isnan(ip_entity.Location.Latitude)
                or math.isnan(ip_entity.Location.Longitude)
            ):
                warnings.warn(
                    "Invalid location information for IP: " + ip_entity.Address,
                    RuntimeWarning,
                )
                continue
            loc_props = ", ".join(
                f"{key}={val}"
                for key, val in ip_entity.Location.properties.items()
                if val
            )

            popup_text = f"{loc_props}<br>IP: {ip_entity.Address}"
            if (
                "City" in ip_entity.Location.properties
                or "CountryName" in ip_entity.Location.properties
            ):
                tooltip_text = (
                    f"{ip_entity.Location.City}, {ip_entity.Location.CountryName}"
                )
            else:
                tooltip_text = (
                    f"{ip_entity.Location.Latitude}, {ip_entity.Location.Longitude}"
                )

            if ip_entity.AdditionalData:
                addl_props = ", ".join(
                    f"{key}={val}"
                    for key, val in ip_entity.AdditionalData.items()
                    if val
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

    def add_geoloc_cluster(self, geo_locations: Iterable[GeoLocation], **kwargs):
        """
        Add a collection of GeoLocation objects to the map.

        Parameters
        ----------
        geo_locations : Iterable[GeoLocation]
            Iterable of GeoLocation entities.

        """
        ip_entities = [IpAddress(Address="na", Location=geo) for geo in geo_locations]
        self.add_ip_cluster(ip_entities=ip_entities, **kwargs)

    def add_locations(self, locations: Iterable[Tuple[float, float]], **kwargs):
        """
        Add a collection of lat/long tuples to the map.

        Parameters
        ----------
        locations : Iterable[Tuple[float, float]]
            Iterable of location tuples.

        """
        geo_entities = [
            GeoLocation(Latitude=lat, Longitude=long) for lat, long in locations
        ]
        self.add_geoloc_cluster(geo_locations=geo_entities, **kwargs)

    def add_geo_hashes(self, geohashes: Iterable[str], **kwargs):
        """
        Add decoded geohashes to the map.

        Parameters
        ----------
        geohashes : Iterable[str]
            Iterable of geolocation hashes

        """
        geo_entities = []
        for geohash in geohashes:
            decoded_location = decode_geo_hash(geohash)
            geo_entities.append(
                GeoLocation(Latitude=decoded_location[0], Longitude=decoded_location[1])
            )

        self.add_geoloc_cluster(geo_locations=geo_entities, **kwargs)

    def add_marker_clusters(self, clusters: Iterable[MarkerCluster]):
        """
        Add MarkerClusters and to the map.

        Parameters
        ----------
        clusters: Iterable[MarkerCluster]
            Iterable of MarkerClusters

        """
        for cluster in clusters:
            self.folium_map.add_child(cluster)

    def add_feature_sub_groups(self, subgroups: Iterable[FeatureGroupSubGroup]):
        """
        Add FeatureGroupSubGroups and to the map.

        Parameters
        ----------
        subgroups: Iterable[FeatureGroupSubGroup]
            Iterable of FeatureGroupSubGroups

        """
        for subgroup in subgroups:
            self.folium_map.add_child(subgroup)

    def save_map(self, path: str):
        """
        Save the map to `path`.

        Parameters
        ----------
        path: str
            File path to save the current map

        """
        self.folium_map.save(path)

    def add_locations_to_feature_subgroup(
        self,
        locations: Iterable[Tuple[float, float]],
        subgroup: FeatureGroupSubGroup,
        **kwargs,
    ):
        """
        Create markers from locations and add the FeatureGroupSubGroup.

        Parameters
        ----------
        locations: Iterable[Tuple[float, float]]
            Collection of Latitude/Longitude coordinates to be added
            to the FeatureGroupSubGroup
        subgroup: FeatureGroupSubGroup
            Subgroup to add locations to, then add to the map

        """
        for point in locations:
            marker = self.create_marker(location=point, **kwargs)
            marker.add_to(subgroup)
            self.locations.append(point)

        subgroups = [subgroup]

        self.add_feature_sub_groups(subgroups)

    def add_locations_to_marker_cluster(
        self, locations: Iterable[Tuple[float, float]], cluster: MarkerCluster, **kwargs
    ):
        """
        Create markers from locations and add to MarkerCluster.

        Parameters
        ----------
        locations: Iterable[Tuple[float, float]]
            Collection of Latitude/Longitude coordinates to be added
            to the MarkerCluster
        cluster: MarkerCluster
            Marker cluster to add locations to, then add to the map

        """
        for point in locations:
            marker = self.create_marker(location=point, **kwargs)
            marker.add_to(cluster)
            self.locations.append(point)

        clusters = [cluster]

        self.add_marker_clusters(clusters)

    def create_new_cluster_with_locations(
        self, locations: Iterable[Tuple[float, float]], name: str, **kwargs
    ):
        """
        Create a MarkerCluster with locations.

        Parameters
        ----------
        locations: Iterable[Tuple[float, float]]
            Collection of Latitude/Longitude coordinates to be added to the
            MarkerCluster
        name: str
            Name of Marker Cluster to create, add locations to,
            then add to the map

        """
        marker_cluster = MarkerCluster(name=name)

        self.add_locations_to_marker_cluster(
            locations=locations, cluster=marker_cluster, **kwargs
        )

    def create_new_subgroup_with_locations(
        self,
        locations: Iterable[Tuple[float, float]],
        subgroup_name: str,
        cluster_name: str,
        **kwargs,
    ):
        """
        Create subgroup of markers from locations.

        Parameters
        ----------
        locations: Iterable[Tuple[float, float]]
            Collection of Latitude/Longitude coordinates to be added
            to the FeatureGroupSubGroup
        subgroup_name: str
            Name of FeatureGroupSubGroup to create, add locations to,
            then add to the map
        cluster_name : str
            Name of the cluster

        Notes
        -----
        This function creates a marker cluster and FeatureGroupSubGroup,
        then add the locations to the subgroup, then add the subgroup to the map.

        """
        marker_cluster = MarkerCluster(name=cluster_name)
        feature_subgroup = FeatureGroupSubGroup(marker_cluster, name=subgroup_name)

        self.add_locations_to_feature_subgroup(
            locations=locations, subgroup=feature_subgroup, **kwargs
        )

    def enable_layer_control(self):
        """
        Enable Layer Control on the map.

        Parameters
        ----------
        None

        """
        folium.LayerControl().add_to(self.folium_map)

    def create_new_cluster_with_geohashes(
        self, geohashes: Iterable[str], name: str, **kwargs
    ):
        """
        Create a MarkerCluster and add geohash locations.

        Parameters
        ----------
        geohashes: Iterable[str]
            Collection of geohashes to be decoded and added to the MarkerCluster
        name: str
            Name of Marker Cluster to create, add locations to, then add to the map

        """
        locations = decode_geohash_collection(geohashes)
        self.create_new_cluster_with_locations(locations=locations, name=name, **kwargs)

    def create_new_subgroup_with_geohashes(
        self, geohashes: Iterable[str], subgroup_name: str, cluster_name: str, **kwargs
    ):
        """
        Create a FeatureSubGroup with collection of geohash locations.

        Parameters
        ----------
        geohashes: Iterable[str]
            Collection of geohashes to be decoded and added to
            the FeatureGroupSubGroup
        subgroup_name: str
            Name of SubGroup to create, add locations to, then add to the map
        cluster_name: str
            Name of the Marker Cluster to create and add the SubGroup to

        """
        locations = decode_geohash_collection(geohashes)

        self.create_new_subgroup_with_locations(
            locations=locations,
            subgroup_name=subgroup_name,
            cluster_name=cluster_name,
            **kwargs,
        )

    @staticmethod
    def create_marker(
        location: Tuple[float, float],
        tooltip: str = None,
        popup: str = None,
        **kwargs,
    ) -> folium.Marker:
        """
        Create and return a Folium Marker at a given location.

        Parameters
        ----------
        location: Tuple[float,float]
            Latitude/Longitude coordinates for the Marker
        tooltip: str [Optional]
            Tooltip text for the Marker
        popup: str [Optional]
            Popup text for the Marker

        Returns
        -------
        Marker
            A Folium Marker at the given location coordinates

        """
        return folium.Marker(
            location=location, tooltip=tooltip, popup=popup, icon=folium.Icon(**kwargs)
        )

    @staticmethod
    def create_marker_cluster(name: str):
        """
        Create and return a MarkerCluster with name.

        Parameters
        ----------
        name: str
            Name of the MarkerCluster

        Returns
        -------
        MarkerCluster
            A Folium MarkerCluster with the provided name

        """
        return MarkerCluster(name=name)

    @staticmethod
    def create_feature_sub_group_of_marker_cluster(
        cluster: MarkerCluster, name: str
    ) -> FeatureGroupSubGroup:
        """
        Return a FeatureGroupSubGroup with `name` for a MarkerCluster.

        Parameters
        ----------
        cluster: MarkerCluster
            Folium MarkerCluster to add FeatureGroupSubGroup to
        name: str
            Desired name of the MarkerCluster

        Returns
        -------
        FeatureGroupSubGroup
            A Folium FeatureGroupSubGroup with the provided name as part
            of the given MarkerCluster

        """
        return FeatureGroupSubGroup(cluster, name=name)


def decode_geo_hash(geohash: str) -> Tuple[float, float, float, float]:
    """
    Decode a geohash.

    Parameters
    ----------
    geohash: str
        A string representation of a location

    Returns
    -------
    Tuple
        Tuple representation of a geohash, format of:
        (Latitude, Longitude,
        Latitude Error interval, Longitude Error Interval)

    """
    return pygeohash.decode_exactly(geohash)


def decode_geohash_collection(geohashes: Iterable[str]):
    """
    Return collection of geohashes decoded into location coordinates.

    Parameters
    ----------
    geohashes: Iterable[str]
        Collection of geohashes to be decoded

    Returns
    -------
    Iterable[Tuple[float, float]]
        Collection of location coordinates in Latitude/Longitude

    """
    locations = []

    for geohash in geohashes:
        exact_location = decode_geo_hash(geohash)
        locations.append((exact_location[0], exact_location[1]))

    return locations


def get_map_center(entities: Iterable[Entity], mode: str = "modal"):
    """
    Calculate median point between Entity IP locations.

    Parameters
    ----------
    entities : Iterable[Entity]
        An iterable of entities containing IpAddress geolocation information.
        The entities can be IpAddress entities or other entities that
        have IpAddress properties.
        The entities must all be of the same type.
    mode : str, optional
        The averaging method to use, by default "median".
        "median" and "mean" are the supported values.

    Returns
    -------
    Tuple
        The Latitude and Longitude calculated

    Notes
    -----
    The function uses the first entity in the `entities` to determine
    how to process the collection. E.g. if the first entity has properties
    src_ip and dest_ip of type `IpAddress`, these are the only properties
    that will be processed for the remainder of the entities.

    """
    ip_entities: List[IpAddress] = []
    loc_entities: List[GeoLocation] = []
    if not entities:
        return (0, 0)
    entities = list(entities)
    if isinstance(entities[0], IpAddress):
        return get_center_ip_entities(entities)  # type: ignore
    loc_props = [
        p_name
        for p_name, p_val in entities[0].properties.items()
        if isinstance(p_val, (IpAddress, GeoLocation))
    ]
    for entity in entities:
        for prop in loc_props:
            if prop not in entity:
                continue
            loc_entity = entity[prop]
            if isinstance(loc_entity, IpAddress):
                ip_entities.append(loc_entity)
            elif isinstance(loc_entity, GeoLocation):
                loc_entities.append(loc_entity)
    locs_ips = _extract_locs_ip_entities(ip_entities)
    return get_center_geo_locs(locs_ips + loc_entities, mode=mode)


def _extract_locs_ip_entities(ip_entities: Iterable[IpAddress]):
    """Return the list of IP entities that have a Location property."""
    if isinstance(ip_entities[0], list):  # type: ignore
        return [
            ip[0]["Location"]  # type: ignore
            for ip in ip_entities
            if bool(ip[0].Location)  # type: ignore
        ]
    return [ip["Location"] for ip in ip_entities if bool(ip.Location)]


def get_center_ip_entities(
    ip_entities: Iterable[IpAddress], mode: str = "median"
) -> Tuple[float, float]:
    """
    Return the geographical center of the IP address locations.

    Parameters
    ----------
    ip_entities : Iterable[IpAddress]
        IpAddress entities with location information
    mode : str, optional
        The averaging method to us, by default "median".
        "median" and "mean" are the supported values.

    Returns
    -------
    Tuple[Union[int, float], Union[int, float]]
        Tuple of latitude, longitude

    """
    ip_locs_longs = _extract_locs_ip_entities(ip_entities)
    return get_center_geo_locs(ip_locs_longs, mode=mode)


def _extract_coords_loc_entities(loc_entities: Iterable[GeoLocation]):
    """Return list of coordinate tuples from GeoLocation entities."""
    return [
        (loc["Latitude"], loc["Longitude"])
        for loc in loc_entities
        if "Latitude" in loc and "Longitude" in loc
    ]


def get_center_geo_locs(
    loc_entities: Iterable[GeoLocation], mode: str = "median"
) -> Tuple[float, float]:
    """
    Return the geographical center of the geo locations.

    Parameters
    ----------
    loc_entities : Iterable[GeoLocation]
        GeoLocation entities with location information
    mode : str, optional
        The averaging method to use, by default "median".
        "median" and "mean" are the supported values.

    Returns
    -------
    Tuple[Union[int, float], Union[int, float]]
        Tuple of latitude, longitude

    """
    lat_longs = _extract_coords_loc_entities(loc_entities)
    return _get_center_coords(lat_longs, mode=mode)


def _get_center_coords(
    locations: Iterable[Tuple[float, float]], mode: str = "median"
) -> Tuple[float, float]:
    """Return the center (median) of the coordinates."""
    if not locations:
        return 0, 0
    locs = list(locations)
    if mode == "median":
        try:
            return (
                stats.median([loc[0] for loc in locs if not math.isnan(loc[0])]),
                stats.median([loc[1] for loc in locs if not math.isnan(loc[1])]),
            )
        except stats.StatisticsError:
            pass
    return (
        stats.mean([loc[0] for loc in locs if not math.isnan(loc[0])]),
        stats.mean([loc[1] for loc in locs if not math.isnan(loc[1])]),
    )
