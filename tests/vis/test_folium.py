# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit tests for Folium wrapper."""
import math
from pathlib import Path
from typing import Any, Optional

import attr
import folium
import pandas as pd

try:
    import pygeohash
except ImportError:
    pygeohash = None  # pylint: disable=invalid-name
import pytest
import pytest_check as check

from msticpy.datamodel.entities import GeoLocation, Host, IpAddress
from msticpy.vis.foliummap import (
    FoliumMap,
    get_center_geo_locs,
    get_center_ip_entities,
    get_map_center,
    plot_map,
)

from ..unit_test_lib import TEST_DATA_PATH, get_test_data_path

# pylint: disable=redefined-outer-name

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "FoliumMap.ipynb"
_MP_CONFIG_PATH = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")


@pytest.fixture(scope="module")
def geo_loc_df():
    """Return dataframe of IP addresses."""
    ip_locs_file = Path(TEST_DATA_PATH).joinpath("ip_locs.csv")
    return pd.read_csv(ip_locs_file, index_col=0)


def test_folium_map(geo_loc_df):
    """Test folium map component."""
    # Create IP and GeoLocation Entities from the dataframe
    geo_locs = list(geo_loc_df.apply(create_geo_entity, axis=1).values)
    ip_entities = list(geo_loc_df.apply(create_ip_entity, axis=1).values)

    folium_map = FoliumMap(
        width="50%", height="50%", location=(47.5982328, -122.331), zoom_start=14
    )
    check.is_instance(folium_map.folium_map, folium.Map)

    for ip in ip_entities:
        ip.AdditionalData.update({"name": "test", "role": "testrole"})
    folium_map = FoliumMap(zoom_start=9)
    folium_map.add_ip_cluster(ip_entities=ip_entities, color="orange")
    ip_entities_clean = [
        ip
        for ip in ip_entities
        if ip.Location
        and ip.Location.Latitude
        and not math.isnan(ip.Location.Latitude)
        and not math.isnan(ip.Location.Longitude)
    ]
    check.equal(len(ip_entities_clean), len(folium_map.locations))

    folium_map.add_ip_cluster(ip_entities=ip_entities_clean)
    folium_map.center_map()

    folium_map.enable_layer_control()

    folium_map.add_geoloc_cluster(
        geo_locations=geo_locs, color="orange", layer="GLEntities"
    )
    check.equal(len(ip_entities_clean) * 3, len(folium_map.locations))

    geo_tuples = geo_loc_df.apply(lambda x: (x.Latitude, x.Longitude), axis=1).values
    folium_map.add_locations(locations=geo_tuples, color="orange", layer="Tuples")
    check.equal(len(ip_entities_clean) * 4, len(folium_map.locations))


def test_centering_algorithms(geo_loc_df):
    """Test centering algorithms."""
    geo_locs = list(geo_loc_df.apply(create_geo_entity, axis=1).values)
    ip_entities = list(geo_loc_df.apply(create_ip_entity, axis=1).values)

    center = get_center_geo_locs(geo_locs)
    check.is_true(math.isclose(center[0], 38.7095))
    check.is_true(math.isclose(center[1], -93.6112))
    center = get_center_geo_locs(geo_locs, mode="mean")
    check.is_true(math.isclose(center[0], 39.847162352941176))
    check.is_true(math.isclose(center[1], -87.36079411764706))
    center = get_center_ip_entities(ip_entities)
    check.is_true(math.isclose(center[0], 38.7095))
    check.is_true(math.isclose(center[1], -93.6112))
    hosts = []
    for ip in ip_entities:
        new_host = Host(HostName=ip.Address)
        new_host.ip = ip
        hosts.append(new_host)

    center = get_map_center(entities=hosts)
    check.is_true(math.isclose(center[0], 39.847162352941176))
    check.is_true(math.isclose(center[1], -87.36079411764706))


def test_add_ips(geo_loc_df):
    """Test adding list of IPs."""
    ips = geo_loc_df.AllExtIPs
    folium_map = FoliumMap(
        width="50%", height="50%", location=(47.5982328, -122.331), zoom_start=14
    )
    folium_map.add_ips(ips)
    check.equal(len(ips), len(folium_map.locations))


def test_create_new_subgroup_with_locations(geo_loc_df):
    """Test adding location tuples."""
    locations = geo_loc_df.apply(
        lambda row: (row.Latitude, row.Longitude), axis=1
    ).values
    folium_map = FoliumMap()
    folium_map.create_new_subgroup_with_locations(locations, "locations", "loc_cluster")
    check.equal(len(locations), len(folium_map.locations))


def test_create_new_subgroup_with_geohashes(geo_loc_df):
    """Test adding geohashes to subgroup."""
    if pygeohash is None:
        return
    geo_hashes = geo_loc_df.apply(
        lambda row: pygeohash.encode(row.Latitude, row.Longitude), axis=1
    ).values
    folium_map = FoliumMap()
    folium_map.create_new_subgroup_with_geohashes(
        geo_hashes, "locations", "loc_cluster"
    )
    check.equal(len(geo_hashes), len(folium_map.locations))


def test_marker_clustering():
    """Test marker clustering."""
    folium_map = FoliumMap(zoom_start=5)
    locations = [
        (47.5982328, -122.331),
        (49.278431, -123.112679),
        (37.776718, -122.416733),
    ]

    folium_map.create_new_cluster_with_locations(
        locations=locations, name="Microsoft Campuses"
    )

    check.equal(len(locations), len(folium_map.locations))


def test_subgroups():
    """Test subgroups."""
    folium_map = FoliumMap(zoom_start=5)
    marker_cluster = folium_map.create_marker_cluster(name="All Campuses")

    clusters = [marker_cluster]

    folium_map.add_marker_clusters(clusters=clusters)

    subgroup_us = folium_map.create_feature_sub_group_of_marker_cluster(
        cluster=marker_cluster, name="US Campuses"
    )
    subgroup_ca = folium_map.create_feature_sub_group_of_marker_cluster(
        cluster=marker_cluster, name="Canadian Campuses"
    )

    locations_us = [(47.5982328, -122.331), (37.776718, -122.416733)]
    locations_ca = [(49.278431, -123.112679)]

    folium_map.add_locations_to_feature_subgroup(
        locations=locations_us, subgroup=subgroup_us, color="blue"
    )
    folium_map.add_locations_to_feature_subgroup(
        locations=locations_ca, subgroup=subgroup_ca, color="red"
    )

    check.equal(len(locations_us) + len(locations_ca), len(folium_map.locations))


def test_geohash():
    """Test geohash."""
    folium_map = FoliumMap(zoom_start=5)
    geohashes = ["c23n8", "c2b2q"]
    folium_map.create_new_cluster_with_geohashes(
        geohashes=geohashes, name="Microsoft Campuses"
    )
    check.equal(len(geohashes), len(folium_map.locations))
    folium_map = FoliumMap(zoom_start=5)
    folium_map.add_geo_hashes(geohashes)
    check.equal(len(geohashes), len(folium_map.locations))


# @pytest.mark.skipif(
#     not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
# )
# def test_folium_map_notebook():
#     """Run folium notebook."""
#     nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
#     exec_notebook(nb_path=nb_path, mp_config=_MP_CONFIG_PATH)


def create_ip_entity(row):
    """Return IP entity - test helper."""
    ip_ent = IpAddress(Address=row["AllExtIPs"])
    geo_loc = create_geo_entity(row)
    ip_ent.Location = geo_loc
    return ip_ent


def create_geo_entity(row):
    """Return GeoLocation entity - test helper."""
    # get subset of fields for GeoLocation
    loc_props = row[
        ["CountryCode", "CountryName", "State", "City", "Longitude", "Latitude"]
    ]
    return GeoLocation(**loc_props.to_dict())


icon_map = {
    "three": {"icon": "fire", "color": "orange", "prefix": "fa"},
    "two": {"icon": "eye", "color": "red", "prefix": "fa"},
    "default": {"icon": "flag", "color": "blue", "prefix": "fa"},
}


def icon_map_func(key):
    """Test function for plot_map"""
    return icon_map.get(key, icon_map.get("default"))


_IP_COL = "AllExtIPs"
_LAT_COL = "Latitude"
_LONG_COL = "Longitude"
_COUNTRY_COL = "CountryName"


@attr.s(auto_attribs=True)
class PlotMapTest:
    """Plot map test class."""

    name: str
    ip_column: Optional[str] = None
    lat_column: Optional[str] = None
    long_column: Optional[str] = None
    layer_column: Optional[str] = None
    icon_column: Optional[str] = None
    icon_map: Any = None
    popup_columns: Optional[list] = None
    tooltip_columns: Optional[list] = None
    exception: Any = None


_STD_ARGS = dict(ip_column=_IP_COL, lat_column=_LAT_COL, long_column=_LONG_COL)

_PM_TEST_PARAMS = [
    (PlotMapTest(name="ip_and_loc", **_STD_ARGS)),  # type: ignore
    (PlotMapTest(name="ip_only", ip_column=_IP_COL)),
    (PlotMapTest(name="loc_only", lat_column=_LAT_COL, long_column=_LONG_COL)),
    (PlotMapTest(name="missing_params", exception=ValueError)),
    (PlotMapTest(name="missing_column", ip_column="IPAddr", exception=LookupError)),
    (PlotMapTest(name="layer", **_STD_ARGS, layer_column=_COUNTRY_COL)),  # type: ignore
    (PlotMapTest(name="icon_dict", **_STD_ARGS, icon_column=_COUNTRY_COL, icon_map=icon_map)),  # type: ignore
    (PlotMapTest(name="icon_map", **_STD_ARGS, icon_column=_COUNTRY_COL, icon_map=icon_map_func)),  # type: ignore
    (PlotMapTest(name="popup", **_STD_ARGS, popup_columns=["CountryName", "State", "City"])),  # type: ignore
    (PlotMapTest(name="tooltip", **_STD_ARGS, tooltip_columns=["CountryName", "State", "City"])),  # type: ignore
]

_PM_IDS = [pmt.name for pmt in _PM_TEST_PARAMS]


@pytest.mark.parametrize("plot_test", _PM_TEST_PARAMS, ids=_PM_IDS)
def test_plot_map(plot_test, geo_loc_df):
    """Test plot_map with different parameters."""
    plot_kwargs = attr.asdict(plot_test)
    plot_kwargs.pop("name", None)
    expected_exception = plot_kwargs.pop("exception", None)
    if expected_exception:
        with pytest.raises(expected_exception):
            plot_map(data=geo_loc_df, **plot_kwargs)
    else:
        folium_map = plot_map(data=geo_loc_df, **plot_kwargs)
        check.equal(len(geo_loc_df), len(folium_map.locations))
    if plot_test.layer_column:
        fm_dict = folium_map.folium_map.to_dict()
        check.equal(
            len(
                [key for key in fm_dict["children"] if key.startswith("feature_group")]
            ),
            geo_loc_df[plot_test.layer_column].nunique(),
        )
        check.is_true(
            any(key for key in fm_dict["children"] if key.startswith("layer_control"))
        )

        # test pd accessor
        folium_map_from_pd = geo_loc_df.mp_plot.folium_map(**plot_kwargs)
        check.equal(len(geo_loc_df), len(folium_map_from_pd.locations))
