# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit tests for Folium wrapper."""
import math
import os
import unittest
from pathlib import Path

import folium
import nbformat
import pandas as pd
import pytest
from nbconvert.preprocessors import CellExecutionError, ExecutePreprocessor

from msticpy.datamodel.entities import GeoLocation, Host, IpAddress
from msticpy.nbtools.foliummap import (
    FoliumMap,
    get_center_geo_locs,
    get_center_ip_entities,
    get_map_center,
)

from ..unit_test_lib import TEST_DATA_PATH

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "FoliumMap.ipynb"


class TestFoliumMap(unittest.TestCase):
    """Unit test class."""

    def test_folium_map(self):
        """Test folium map component."""
        # Read in some data
        ip_locs_file = Path(TEST_DATA_PATH).joinpath("ip_locs.csv")
        geo_loc_df = pd.read_csv(ip_locs_file, index_col=0)

        # Create IP and GeoLocation Entities from the dataframe
        geo_locs = list(geo_loc_df.apply(create_geo_entity, axis=1).values)
        ip_entities = list(geo_loc_df.apply(create_ip_entity, axis=1).values)

        folium_map = FoliumMap(
            width="50%", height="50%", location=(47.5982328, -122.331), zoom_start=14
        )
        self.assertIsInstance(folium_map.folium_map, folium.Map)

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
        self.assertEqual(len(ip_entities_clean), len(folium_map.locations))

        folium_map.add_ip_cluster(ip_entities=ip_entities_clean)
        folium_map.center_map()

        folium_map.add_geoloc_cluster(geo_locations=geo_locs, color="orange")
        self.assertEqual(len(ip_entities_clean) * 3, len(folium_map.locations))

        geo_tuples = geo_loc_df.apply(
            lambda x: (x.Latitude, x.Longitude), axis=1
        ).values
        folium_map.add_locations(locations=geo_tuples, color="orange")
        self.assertEqual(len(ip_entities_clean) * 4, len(folium_map.locations))

    def test_centering_algorithms(self):
        """Test centering algorithms."""
        ip_locs_file = Path(TEST_DATA_PATH).joinpath("ip_locs.csv")
        geo_loc_df = pd.read_csv(ip_locs_file, index_col=0)
        geo_locs = list(geo_loc_df.apply(create_geo_entity, axis=1).values)
        ip_entities = list(geo_loc_df.apply(create_ip_entity, axis=1).values)

        center = get_center_geo_locs(geo_locs)
        self.assertAlmostEqual(center[0], 38.7095)
        self.assertAlmostEqual(center[1], -93.6112)
        center = get_center_geo_locs(geo_locs, mode="mean")
        self.assertAlmostEqual(center[0], 39.847162352941176)
        self.assertAlmostEqual(center[1], -87.36079411764706)
        center = get_center_ip_entities(ip_entities)
        self.assertAlmostEqual(center[0], 38.7095)
        self.assertAlmostEqual(center[1], -93.6112)
        hosts = []
        for ip in ip_entities:
            new_host = Host(HostName=ip.Address)
            new_host.ip = ip
            hosts.append(new_host)

        center = get_map_center(entities=hosts)
        self.assertAlmostEqual(center[0], 39.847162352941176)
        self.assertAlmostEqual(center[1], -87.36079411764706)

    def test_marker_clustering(self):
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

        self.assertEqual(len(locations), len(folium_map.locations))

    def test_subgroups(self):
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

        self.assertEqual(
            len(locations_us) + len(locations_ca), len(folium_map.locations)
        )

    def test_geohash(self):
        """Test geohash."""
        folium_map = FoliumMap(zoom_start=5)
        geohashes = ["c23n8", "c2b2q"]

        folium_map.create_new_cluster_with_geohashes(
            geohashes=geohashes, name="Microsoft Campuses"
        )

        self.assertEqual(len(geohashes), len(folium_map.locations))

    @pytest.mark.skipif(
        not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
    )
    def test_folium_map_notebook(self):
        """Run folium notebook."""
        nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
        abs_path = Path(_NB_FOLDER).absolute()
        with open(nb_path, encoding="utf-8") as f:
            nb = nbformat.read(f, as_version=4)
        ep = ExecutePreprocessor(timeout=600, kernel_name="python3")

        try:
            ep.preprocess(nb, {"metadata": {"path": abs_path}})
        except CellExecutionError:
            nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
            msg = f"Error executing the notebook '{nb_path}'.\n"
            msg += f"See notebook '{nb_err}' for the traceback."
            print(msg)
            with open(nb_err, mode="w", encoding="utf-8") as f:
                nbformat.write(nb, f)
            raise


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
