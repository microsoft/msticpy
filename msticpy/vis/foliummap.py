# pylint: disable=too-many-lines
# -------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Folium map class."""
from __future__ import annotations

import contextlib
import itertools
import math
import statistics as stats
import sys
from typing import Any, Callable, Generator, Iterable

import folium
import pandas as pd
from folium.plugins import FeatureGroupSubGroup, MarkerCluster
from typing_extensions import Self

from .._version import VERSION
from ..common.exceptions import MsticpyMissingDependencyError
from ..common.utility import export
from ..context.geoip import GeoLiteLookup
from ..datamodel.entities import Entity, GeoLocation, IpAddress

try:
    import pygeohash
except ImportError:
    pygeohash = None  # pylint: disable=invalid-name


__version__ = VERSION
__author__ = "Ian Hellen"

_GEO_LITE: GeoLiteLookup = GeoLiteLookup()


@export
class FoliumMap:
    """Wrapper class for Folium/Leaflet mapping."""

    def __init__(
        self: FoliumMap,
        title: str = "OpenStreetMap",
        zoom_start: int = 2,
        tiles: str | folium.TileLayer | None = None,
        width: str = "100%",
        height: str = "100%",
        location: list[float] | None = None,
    ) -> None:
        """
        Create an instance of the folium map.

        Parameters
        ----------
        title : str, optional
            Name of the main tile layer (the default is 'OpenStreetMap')
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
            control_scale=True,
        )
        folium.TileLayer(name=title).add_to(self.folium_map)
        self.locations: list[tuple[float, float]] = []

    def _repr_html_(self: Self) -> str:
        """Return folium map as HTML."""
        # pylint: disable=protected-access
        return self.folium_map._repr_html_()
        # pylint: enable=protected-access

    def center_map(self: Self) -> None:
        """Calculate and set map center based on current coordinates."""
        self.folium_map.location = _get_center_coords(self.locations)

    def add_ip_cluster(
        self: Self,
        ip_entities: Iterable[IpAddress],
        layer: str | None = None,
        **kwargs,
    ) -> None:
        """
        Add a collection of IP Entities to the map.

        Parameters
        ----------
        ip_entities : Iterable[IpAddress]
            a iterable of IpAddress Entities
        layer : str, optional
            If not none, it will add the entities to a new layer.

        Other Parameters
        ----------------
        kwargs: icon properties to use for displaying this cluster

        """
        ip_entities = _get_location_for_ip_entities(ip_entities)

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
                continue
            popup_text: str = _get_popup_text(ip_entity)
            tooltip_text: str = _get_tooltip_text(ip_entity)

            marker = folium.Marker(
                location=[ip_entity.Location.Latitude, ip_entity.Location.Longitude],
                popup=popup_text,
                tooltip=tooltip_text,
                icon=folium.Icon(**kwargs),
            )
            if layer:
                marker_target: folium.FeatureGroup = folium.FeatureGroup(name=layer)
                marker_target.add_to(self.folium_map)
                folium.LayerControl().add_to(self.folium_map)
                marker.add_to(marker_target)
            else:
                marker_target_map: folium.Map = self.folium_map
                marker.add_to(marker_target_map)
            self.locations.append(
                (ip_entity.Location.Latitude, ip_entity.Location.Longitude)
            )

    def add_ips(
        self: Self,
        ip_addresses: Iterable[str],
        **kwargs,
    ) -> None:
        """
        Add a collection of GeoLocation objects to the map.

        Parameters
        ----------
        ip_addresses : Iterable[str]
            Iterable of ip strings.
        layer : str, optional
            If not none, it will add the entities to a new layer.

        Other Parameters
        ----------------
        kwargs: icon properties to use for displaying this cluster

        """
        _, ip_entities = _GEO_LITE.lookup_ip(ip_addr_list=ip_addresses)
        self.add_ip_cluster(ip_entities=ip_entities, **kwargs)

    def add_geoloc_cluster(
        self: Self, geo_locations: Iterable[GeoLocation], **kwargs
    ) -> None:
        """
        Add a collection of GeoLocation objects to the map.

        Parameters
        ----------
        geo_locations : Iterable[GeoLocation]
            Iterable of GeoLocation entities.
        layer : str, optional
            If not none, it will add the entities to a new layer.

        Other Parameters
        ----------------
        kwargs: icon properties to use for displaying this cluster

        """
        ip_entities: list[IpAddress] = [
            IpAddress(Address="na", Location=geo) for geo in geo_locations
        ]
        self.add_ip_cluster(ip_entities=ip_entities, **kwargs)

    def add_locations(
        self: Self, locations: Iterable[tuple[float, float]], **kwargs
    ) -> None:
        """
        Add a collection of lat/long tuples to the map.

        Parameters
        ----------
        locations : Iterable[tuple[float, float]]
            Iterable of location tuples.
        layer : str, optional
            If not none, it will add the entities to a new layer.

        Other Parameters
        ----------------
        kwargs: icon properties to use for displaying this cluster

        """
        geo_entities: list[GeoLocation] = [
            GeoLocation(Latitude=lat, Longitude=long) for lat, long in locations
        ]
        self.add_geoloc_cluster(geo_locations=geo_entities, **kwargs)

    def add_geo_hashes(self: Self, geohashes: Iterable[str], **kwargs) -> None:
        """
        Add decoded geohashes to the map.

        Parameters
        ----------
        geohashes : Iterable[str]
            Iterable of geolocation hashes
        layer : str, optional
            If not none, it will add the entities to a new layer.

        Other Parameters
        ----------------
        kwargs: icon properties to use for displaying this cluster

        """
        geo_entities: list[GeoLocation] = []
        for geohash in geohashes:
            decoded_location = decode_geo_hash(geohash)
            geo_entities.append(
                GeoLocation(Latitude=decoded_location[0], Longitude=decoded_location[1])
            )

        self.add_geoloc_cluster(geo_locations=geo_entities, **kwargs)

    def add_marker_clusters(self: Self, clusters: Iterable[MarkerCluster]) -> None:
        """
        Add MarkerClusters and to the map.

        Parameters
        ----------
        clusters: Iterable[MarkerCluster]
            Iterable of MarkerClusters

        """
        for cluster in clusters:
            self.folium_map.add_child(cluster)

    def add_feature_sub_groups(
        self: Self,
        subgroups: Iterable[FeatureGroupSubGroup],
    ) -> None:
        """
        Add FeatureGroupSubGroups and to the map.

        Parameters
        ----------
        subgroups: Iterable[FeatureGroupSubGroup]
            Iterable of FeatureGroupSubGroups

        """
        for subgroup in subgroups:
            self.folium_map.add_child(subgroup)

    def save_map(self: Self, path: str) -> None:
        """
        Save the map to `path`.

        Parameters
        ----------
        path: str
            File path to save the current map

        """
        self.folium_map.save(path)

    def add_locations_to_feature_subgroup(
        self: Self,
        locations: Iterable[tuple[float, float]],
        subgroup: FeatureGroupSubGroup,
        **kwargs,
    ) -> None:
        """
        Create markers from locations and add the FeatureGroupSubGroup.

        Parameters
        ----------
        locations: Iterable[tuple[float, float]]
            Collection of Latitude/Longitude coordinates to be added
            to the FeatureGroupSubGroup
        subgroup: FeatureGroupSubGroup
            Subgroup to add locations to, then add to the map

        """
        for point in locations:
            marker: folium.Marker = self.create_marker(location=point, **kwargs)
            marker.add_to(subgroup)
            self.locations.append(point)

        subgroups: list[FeatureGroupSubGroup] = [subgroup]

        self.add_feature_sub_groups(subgroups)

    def add_locations_to_marker_cluster(
        self: Self,
        locations: Iterable[tuple[float, float]],
        cluster: MarkerCluster,
        **kwargs,
    ) -> None:
        """
        Create markers from locations and add to MarkerCluster.

        Parameters
        ----------
        locations: Iterable[tuple[float, float]]
            Collection of Latitude/Longitude coordinates to be added
            to the MarkerCluster
        cluster: MarkerCluster
            Marker cluster to add locations to, then add to the map

        """
        for point in locations:
            marker: folium.Marker = self.create_marker(location=point, **kwargs)
            marker.add_to(cluster)
            self.locations.append(point)

        clusters: list[MarkerCluster] = [cluster]

        self.add_marker_clusters(clusters)

    def create_new_cluster_with_locations(
        self: Self,
        locations: Iterable[tuple[float, float]],
        name: str,
        **kwargs,
    ) -> None:
        """
        Create a MarkerCluster with locations.

        Parameters
        ----------
        locations: Iterable[tuple[float, float]]
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
        self: Self,
        locations: Iterable[tuple[float, float]],
        subgroup_name: str,
        cluster_name: str,
        **kwargs,
    ) -> None:
        """
        Create subgroup of markers from locations.

        Parameters
        ----------
        locations: Iterable[tuple[float, float]]
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

    def enable_layer_control(self: Self) -> None:
        """
        Enable Layer Control on the map.

        Parameters
        ----------
        None

        """
        folium.LayerControl().add_to(self.folium_map)

    def create_new_cluster_with_geohashes(
        self: Self,
        geohashes: Iterable[str],
        name: str,
        **kwargs,
    ) -> None:
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
        self: Self,
        geohashes: Iterable[str],
        subgroup_name: str,
        cluster_name: str,
        **kwargs,
    ) -> None:
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
        location: tuple[float, float],
        tooltip: str | None = None,
        popup: str | None = None,
        **kwargs,
    ) -> folium.Marker:
        """
        Create and return a Folium Marker at a given location.

        Parameters
        ----------
        location: tuple[float,float]
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
    def create_marker_cluster(name: str) -> MarkerCluster:
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


@export
def decode_geo_hash(geohash: str) -> tuple[float, float, float, float]:
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

    Raises
    ------
    MsticpyMissingDependencyError
        If pygeohash is not installed.

    """
    if pygeohash is not None:
        return pygeohash.decode_exactly(geohash)
    raise MsticpyMissingDependencyError(packages="pygeohash")


@export
def decode_geohash_collection(geohashes: Iterable[str]) -> list[tuple[float, float]]:
    """
    Return collection of geohashes decoded into location coordinates.

    Parameters
    ----------
    geohashes: Iterable[str]
        Collection of geohashes to be decoded

    Returns
    -------
    Iterable[tuple[float, float]]
        Collection of location coordinates in Latitude/Longitude

    """
    locations: list[tuple[float, float]] = []

    for geohash in geohashes:
        exact_location: tuple[float, float] = decode_geo_hash(geohash)
        locations.append((exact_location[0], exact_location[1]))

    return locations


def _get_tooltip_text(ip_entity: IpAddress) -> str:
    """Return tooltip text for marker."""
    return "<br>".join(
        str(line)
        for line in [
            ip_entity.Address,
            (
                ip_entity.Location.City
                if ip_entity.Location and ip_entity.Location.City
                else "Unknown city"
            ),
            (
                ip_entity.Location.CountryCode
                if ip_entity.Location and ip_entity.Location.CountryCode
                else "Unknown country"
            ),
            *(list(ip_entity.AdditionalData.items())),
        ]
    )


def _get_popup_text(ip_entity: IpAddress) -> str:
    """Return popup text for marker."""
    return "<br>".join(
        str(line)
        for line in [
            ip_entity.Address,
            *(
                list(
                    ip_entity.Location.properties.values() if ip_entity.Location else []
                )
            ),
            *(list(ip_entity.AdditionalData.items())),
        ]
    )


if sys.version_info > (3, 10):
    from typing import TypeAlias

    IconMapper: TypeAlias = (
        Callable[[str], dict[str, Any]]  # pylint: disable=unsubscriptable-object
        | dict[str, Any]  # pylint: disable=unsubscriptable-object
        | None
    )

else:
    from typing import Dict, Union

    IconMapper = Union[Callable[[str], Dict[str, Any]], Dict[str, Any], None]


# pylint: disable=too-many-locals, too-many-arguments
def plot_map(
    data: pd.DataFrame,
    ip_column: str | None = None,
    lat_column: str | None = None,
    long_column: str | None = None,
    layer_column: str | None = None,
    icon_column: str | None = None,
    icon_map: IconMapper = None,
    popup_columns: list[str] | None = None,
    tooltip_columns: list[str] | None = None,
    **kwargs,
) -> FoliumMap:
    """
    Plot folium map from DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        The input DataFrame, must have either an IP address
        column or latitude and longitude columns.
    ip_column : str|None, optional
        The name of the IP Address column, by default None
    lat_column : str|None, optional
        The name of the location 'latitude' column, by default None
    long_column : str|None, optional
        The name of the location 'longitude' column, by default None
    layer_column : str|None, optional
        The column to group markers into for displaying on different
        map layers, by default None
    icon_column : str|None, optional
        Optional column containing the name of the icon to use
        for the marker in this row, by default None
    icon_map : IconMapper, optional
        Mapping dictionary or function, by default None
        See Notes for more details.
    popup_columns : Optional[list[str]], optional
        List of columns to use for the popup text, by default None
    tooltip_columns : Optional[list[str]], optional
        List of columns to use for the tooltip text, by default None


    Other Parameters
    ----------------
    marker_cluster : bool, optional
        Use marker clustering, default is True.
    default_color : str, optional
        Default color for marker icons, by default "blue"
    title : str, optional
        Name of the layer (the default is 'layer1')
        (passed to FoliumMap constructor)
    zoom_start : int, optional
        The zoom level of the map (the default is 7)
        (passed to FoliumMap constructor)
    tiles : [type], optional
        Custom set of tiles or tile URL (the default is None)
        (passed to FoliumMap constructor)
    width : str, optional
        Map display width (the default is '100%')
        (passed to FoliumMap constructor)
    height : str, optional
        Map display height (the default is '100%')
        (passed to FoliumMap constructor)
    location : list, optional
        Location to center map on

    Returns
    -------
    folium.Map
        Folium Map object.

    Raises
    ------
    ValueError
        If neither `ip_column` nor `lat_column` and `long_column` are passed.
    LookupError
        If one of the passed columns does not exist in `data`

    Notes
    -----
    There are two ways of providing custom icon settings based on the
    the row of the input DataFrame.

    If `icon_map` is a dict it should contain keys that map to the
    value of `icon_col` and values that a dicts of valid
    folium Icon properties ("color", "icon_color", "icon", "angle", "prefix").
    The dict should include a "default" entry that will be used if the
    value in the DataFrame[icon_col] doesn't match any key.
    For example:

    .. code:: python

        icon_map = {
            "high": {
                "color": "red",
                "icon": "warning",
            },
            "medium": {
                "color": "orange",
                "icon": "triangle-exclamation",
                "prefix": "fa",
            },
            "default": {
                "color": "blue",
                "icon": "info-sign",
            },
        }

    If icon_map is a function it should take a single str parameter
    (the item key) and return a dict of icon properties. It should
    return a default set of values if the key does not match a known
    key. The `icon_col` value for each row will be passed to this
    function and the return value used to populate the Icon arguments.

    For example:

    .. code::python

        def icon_mapper(icon_key):
            if icon_key.startswith("bad"):
                return {
                    "color": "red",
                    "icon": "triangle-alert",
                }
            ...
            else:
                return {
                    "color": "blue",
                    "icon": "info-sign",
                }

    FontAwesome icon (prefix "fa") names are available at https://fontawesome.com/
    GlyphIcons icons (prefix "glyphicon") are available at https://www.glyphicons.com/

    """
    folium_map = FoliumMap(**kwargs)
    if ip_column and not (lat_column and long_column):
        # resolve IP location and merge with input data.
        data = data.merge(
            # pylint: disable=no-member
            _GEO_LITE.lookup_ips(data, column=ip_column),
            # pylint: enable=no-member
            left_on=ip_column,
            right_on="IpAddress",
            suffixes=("_src", None),
        ).dropna(axis="index", subset=["Latitude", "Longitude"])
        lat_column, long_column = ["Latitude", "Longitude"]
        if not tooltip_columns:
            tooltip_columns = _default_columns(data, [ip_column, "CountryCode", "City"])
        if not popup_columns:
            popup_columns = _default_columns(
                data,
                [ip_column, "CountryOrRegionName", "City", lat_column, long_column],
            )
    else:
        if not tooltip_columns:
            tooltip_columns = []
            tooltip_columns.extend(col for col in (lat_column, long_column) if col)
        if not popup_columns:
            popup_columns = []
            popup_columns.extend(col for col in (lat_column, long_column) if col)

    _validate_columns(
        data,
        ip_column,
        lat_column,
        long_column,
        [layer_column, icon_column],
    )
    popup_columns = _validate_optional_columns(data, popup_columns)
    tooltip_columns = _validate_optional_columns(data, tooltip_columns)

    folium_map.locations.extend(
        data.apply(lambda row: (row[lat_column], row[long_column]), axis=1)
    )
    # common dictionary of kwargs to _create_feature_group
    static_kwargs: dict[str, Any] = {
        "lat_column": lat_column,
        "long_column": long_column,
        "icon_column": icon_column,
        "icon_map": icon_map,
        "popup_cols": popup_columns,
        "tooltip_cols": tooltip_columns,
        "use_marker_cluster": kwargs.pop("marker_cluster", True),
    }
    if layer_column is None:
        feature_group: folium.FeatureGroup = _create_feature_group(
            data=data,
            layer_name="All locations",
            def_layer_color=kwargs.pop("default_color", "blue"),
            **static_kwargs,
        )
        feature_group.add_to(folium_map.folium_map)
    else:
        for index, (layer, layer_df) in enumerate(data.groupby(layer_column)):
            def_layer_color: str = _get_icon_layer_color(index)
            feature_group = _create_feature_group(
                data=layer_df,
                layer_name=str(layer),
                def_layer_color=def_layer_color,
                **static_kwargs,
            )
            feature_group.add_to(folium_map.folium_map)
        folium.LayerControl().add_to(folium_map.folium_map)
    if "location" not in kwargs:
        folium_map.center_map()
    return folium_map


# pylint: enable=too-many-locals, too-many-arguments


def _default_columns(data, defaults: Iterable[str]) -> list[str]:
    return [col_name for col_name in defaults if col_name in data.columns]


def _validate_columns(
    data: pd.DataFrame,
    ip_column: str | None = None,
    lat_column: str | None = None,
    long_column: str | None = None,
    other_columns: list[str | None] | None = None,
) -> None:
    """Validate required columns and that optional cols are in the data."""
    if not ip_column and not (lat_column and long_column):
        raise ValueError(
            "Data must have either an IpAddress ('ip_column')",
            "or latitude ('lat_column') and longitude ('long_column')",
        )
    param_cols: list[str] = []
    for param in other_columns or []:
        if not param:
            continue
        if isinstance(param, list):
            param_cols.extend(param)
        else:
            param_cols.append(param)
    missing_columns: set[str] = {col for col in param_cols if col not in data.columns}
    if missing_columns:
        raise LookupError(
            "The following columns are not in the supplied DataFrame",
            ",".join(f"'{col}'" for col in missing_columns),
        )


def _validate_optional_columns(
    data: pd.DataFrame, optional_columns: Iterable[str]
) -> list[str]:
    """Validate that optional columns are in the data."""
    return [col for col in optional_columns if col in data.columns]


# pylint: disable=too-many-arguments
def _create_feature_group(
    data: pd.DataFrame,
    layer_name: str,
    lat_column: str,
    long_column: str,
    icon_column: str | None,
    icon_map: IconMapper,
    popup_cols: list[str],
    tooltip_cols: list[str],
    def_layer_color: str,
    use_marker_cluster: bool = True,
) -> folium.FeatureGroup:
    """Create folium feature group."""
    feature_group = folium.FeatureGroup(name=layer_name)
    if use_marker_cluster:
        container: MarkerCluster = MarkerCluster(name=layer_name)
        container.add_to(feature_group)
    data.apply(
        lambda row: folium.Marker(
            location=(row[lat_column], row[long_column]),
            tooltip=_create_marker_text(row, tooltip_cols),
            popup=_create_marker_text(row, popup_cols),
            icon=_create_mapped_icon(row, icon_column, icon_map, def_layer_color),
        ).add_to(feature_group),
        axis=1,
    )
    return feature_group


# pylint: enable=too-many-arguments


def _create_marker_text(row: pd.Series, columns: list[str]) -> str:
    """Return HTML formatted text for tooltips and popups."""
    return "<br>".join(f"{col}: {row[col]}" for col in columns)


def _get_icon_layer_color(layer_index: int) -> str:
    """Get a color from folium.color options."""
    col_options = folium.Icon.color_options - {"white"}
    return list(col_options)[layer_index % len(col_options)]


def _create_mapped_icon(
    row: pd.Series,
    icon_column: str | None = None,
    icon_map: IconMapper | None = None,
    def_layer_color: str = "blue",
) -> folium.Icon:
    """Return folium Icon from mapping or defaults."""
    icon_kwargs: dict[str, Any] = {}
    if isinstance(icon_map, dict):
        icon_kwargs = icon_map.get(  # type: ignore[assignment]
            row[icon_column], icon_map.get("default", {})
        )
    elif callable(icon_map):
        icon_kwargs = icon_map(row[icon_column])
    elif icon_column:
        icon_kwargs = {"icon": row[icon_column]}
    if "color" not in icon_kwargs:
        icon_kwargs["color"] = def_layer_color
    return folium.Icon(**icon_kwargs)


def _get_location_for_ip_entities(
    ip_entities: Iterable[IpAddress],
) -> Generator[IpAddress, None, None]:
    for ip_entity in ip_entities:
        if (
            ip_entity.Location is None
            or ip_entity.Location.Longitude is None
            or ip_entity.Location.Latitude is None
        ):
            _, ip_res_list = _GEO_LITE.lookup_ip(ip_entity=ip_entity)
            if ip_res_list:
                ip_entity.Location = ip_res_list[0].Location
        yield ip_entity


@export
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
    ip_entities: list[IpAddress] = []
    loc_entities: list[GeoLocation] = []
    if not entities:
        return (0, 0)
    entities = list(entities)
    if isinstance(entities[0], IpAddress):
        return get_center_ip_entities(entities)
    loc_props: list[str] = [
        p_name
        for p_name, p_val in entities[0].properties.items()
        if isinstance(p_val, (IpAddress, GeoLocation))
    ]
    for entity, prop in itertools.product(entities, loc_props):
        if prop not in entity:
            continue
        loc_entity: GeoLocation | IpAddress | None = entity[prop]
        if isinstance(loc_entity, IpAddress):
            ip_entities.append(loc_entity)
        elif isinstance(loc_entity, GeoLocation):
            loc_entities.append(loc_entity)
    locs_ips: list[GeoLocation] = _extract_locs_ip_entities(ip_entities)
    return get_center_geo_locs(locs_ips + loc_entities, mode=mode)


def _extract_locs_ip_entities(
    ip_entities: Iterable[IpAddress] | Iterable[list[IpAddress]],
) -> list[GeoLocation]:
    """Return the list of IP entities that have a Location property."""
    locations: list[GeoLocation] = []
    for ip in ip_entities:
        if isinstance(ip, list) and len(ip) > 0 and ip[0].Location is not None:
            locations.append(ip[0].Location)
        elif isinstance(ip, IpAddress) and ip.Location is not None:
            locations.append(ip.Location)
    return locations


@export
def get_center_ip_entities(
    ip_entities: Iterable[IpAddress],
    mode: str = "median",
) -> tuple[float, float]:
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
    tuple[Union[int, float], Union[int, float]]
        Tuple of latitude, longitude

    """
    ip_locs_longs: list[GeoLocation] = _extract_locs_ip_entities(ip_entities)
    return get_center_geo_locs(ip_locs_longs, mode=mode)


def _extract_coords_loc_entities(
    loc_entities: Iterable[GeoLocation],
) -> list[tuple[float, float]]:
    """Return list of coordinate tuples from GeoLocation entities."""
    return [
        (loc.Latitude, loc.Longitude)
        for loc in loc_entities
        if loc.Latitude and loc.Longitude
    ]


@export
def get_center_geo_locs(
    loc_entities: Iterable[GeoLocation],
    mode: str = "median",
) -> list[float]:
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
    tuple[Union[int, float], Union[int, float]]
        Tuple of latitude, longitude

    """
    lat_longs: list[tuple[float, float]] = _extract_coords_loc_entities(loc_entities)
    return _get_center_coords(lat_longs, mode=mode)


def _get_center_coords(
    locations: Iterable[tuple[float, float]],
    mode: str = "median",
) -> list[float]:
    """Return the center (median) of the coordinates."""
    if not locations:
        return [0, 0]
    locs = list(locations)
    if mode == "median":
        with contextlib.suppress(stats.StatisticsError):
            return [
                stats.median([loc[0] for loc in locs if not math.isnan(loc[0])]),
                stats.median([loc[1] for loc in locs if not math.isnan(loc[1])]),
            ]
    return [
        stats.mean([loc[0] for loc in locs if not math.isnan(loc[0])]),
        stats.mean([loc[1] for loc in locs if not math.isnan(loc[1])]),
    ]
