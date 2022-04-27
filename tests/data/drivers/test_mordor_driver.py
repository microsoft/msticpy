# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mordor data driver test."""
import contextlib
import io
import json
import os
import shutil
from datetime import datetime
from pathlib import Path
from unittest.mock import patch

import pandas as pd
import pytest
import pytest_check as check

from msticpy.data import QueryProvider
from msticpy.data.drivers.mordor_driver import (
    _MITRE_TACTICS_CACHE,
    _MITRE_TECH_CACHE,
    _MORDOR_CACHE,
    MordorDriver,
    MordorEntry,
    download_mdr_file,
    search_mdr_data,
)

from ...unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

_SAVE_FOLDER = "mordor_test"


# pylint: disable=redefined-outer-name, protected-access, global-statement


@pytest.fixture(scope="session")
def save_folder(tmp_path_factory):
    """Query Provider fixture."""
    cache_folder = tmp_path_factory.mktemp(_SAVE_FOLDER)
    for file in get_test_data_path().joinpath("mordor").glob("*.pkl"):
        shutil.copy(str(file), str(cache_folder))
    shutil.copy(
        str(get_test_data_path().joinpath("mordor").joinpath("mordor_cache.json")),
        str(cache_folder),
    )
    mordor_cache_file = Path(cache_folder).joinpath("mordor_cache.json")
    mdr_cache = json.loads(mordor_cache_file.read_text(encoding="utf-8"))
    for item in mdr_cache.values():
        item["mp_last_updated"] = pd.Timestamp.utcnow().isoformat()
    Path(mordor_cache_file).write_text(
        json.dumps(mdr_cache, indent=4), encoding="utf-8"
    )

    return str(cache_folder)


def get_mdr_data_paths(save_folder):
    """Mock get_mdr_data_paths."""
    mdr_cache = json.loads(
        Path(save_folder).joinpath("mordor_cache.json").read_text(encoding="utf-8")
    )
    return list(mdr_cache.keys())


@pytest.fixture
@patch("msticpy.data.drivers.mordor_driver.get_mdr_data_paths")
def qry_provider(get_paths, save_folder):
    """Query Provider fixture."""
    get_paths.return_value = get_mdr_data_paths(save_folder)
    qry_prov = QueryProvider("Mordor", save_folder=save_folder)
    qry_prov.connect()
    return qry_prov


@pytest.fixture
def mdr_driver(qry_provider):
    """Test fixture to create mordor driver."""
    return qry_provider._query_provider


def test_mordor_load(mdr_driver: MordorDriver):
    """Check basic load of driver."""
    check.is_true(mdr_driver.loaded)
    check.is_true(mdr_driver.connected)
    check.is_false(mdr_driver.use_query_paths)
    check.is_true(mdr_driver.has_driver_queries)

    check.is_instance(mdr_driver.mitre_techniques, pd.DataFrame)
    check.is_instance(mdr_driver.mitre_tactics, pd.DataFrame)
    check.is_in("T1078", mdr_driver.mitre_techniques.index)
    check.is_in("TA0001", mdr_driver.mitre_tactics.index)

    check.is_true(len(mdr_driver.mordor_data) > 50)

    _, first_item = next(iter(mdr_driver.mordor_data.items()))
    check.is_instance(first_item.title, str)
    check.is_instance(first_item.id, str)
    check.is_instance(first_item.contributors, list)
    check.is_instance(first_item.creation_date, datetime)
    check.is_instance(first_item.files, list)
    check.is_true(len(first_item.files) > 0)
    check.is_instance(first_item.attack_mappings, list)
    for attack in first_item.attack_mappings:
        check.is_in("technique", attack)
        check.is_in("tactics", attack)


def test_mordor_cache(save_folder, qry_provider):
    """Test to see if connecting has created valid cache files."""
    del qry_provider
    tactics_cache = Path(save_folder).joinpath(_MITRE_TACTICS_CACHE)
    tech_cache = Path(save_folder).joinpath(_MITRE_TECH_CACHE)
    mordor_cache = Path(save_folder).joinpath(_MORDOR_CACHE)

    check.is_true(tactics_cache.is_file())
    check.is_true(tech_cache.is_file())
    check.is_true(mordor_cache.is_file())

    tactics_df = pd.read_pickle(tactics_cache)
    check.is_instance(tactics_df, pd.DataFrame)
    check.greater_equal(len(tactics_df), 10)
    techniques_df = pd.read_pickle(tech_cache)
    check.is_instance(techniques_df, pd.DataFrame)
    check.greater_equal(len(techniques_df), 50)

    json_text = Path(mordor_cache).read_text(encoding="utf-8")
    md_metadata = json.loads(json_text)
    check.is_instance(md_metadata, dict)
    item = next(iter(md_metadata.values()))
    check.is_instance(item, dict)
    mdr_entry_dict = item.copy()
    mdr_entry_dict.pop("mp_last_updated")
    mordor_entry = MordorEntry(**mdr_entry_dict)
    check.is_instance(mordor_entry, MordorEntry)


def test_mordor_search(mdr_driver: MordorDriver):
    """Test search functionality."""
    results = search_mdr_data(mdr_driver.mordor_data, "AWS")
    check.greater_equal(len(results), 1)

    subset = search_mdr_data(mdr_driver.mordor_data, "Empire")
    check.greater_equal(len(subset), 39)

    emp_power = search_mdr_data(mdr_driver.mordor_data, "Empire+Power")
    check.greater_equal(len(emp_power), 18)
    check.greater_equal(
        len(search_mdr_data(mdr_driver.mordor_data, "Empire, Windows")), 50
    )

    subset_search = search_mdr_data(mdr_driver.mordor_data, "Power", subset=subset)
    check.equal(len(emp_power), len(subset_search))

    result_set = mdr_driver.search_queries("AWS")
    check.greater_equal(len(list(result_set)), 1)
    check.is_true(any(hit for hit in result_set if "atomic.aws.collection" in hit))


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_mordor_download(mdr_driver: MordorDriver, save_folder):
    """Test file download."""
    entry = next(iter(mdr_driver.mordor_data.values()))
    files = entry.get_file_paths()

    file_path = files[0]["file_path"]
    d_frame = download_mdr_file(file_path, save_folder=save_folder)

    check.is_instance(d_frame, pd.DataFrame)
    check.greater_equal(len(d_frame), 10)


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_mordor_query_provider(qry_provider):
    """Test query functions from query provider."""
    queries = qry_provider.list_queries()
    check.greater_equal(len(queries), 50)

    check.is_true(hasattr(qry_provider, "atomic"))
    check.is_true(hasattr(qry_provider, queries[0]))

    test_query = "atomic.windows.credential_access.host.empire_mimikatz_logonpasswords"
    q_func = getattr(qry_provider, test_query)
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        q_func("?")
    check.is_in("Query:", output.getvalue())
    check.is_in("Data source:  OTRF", output.getvalue())
    check.is_in("Mordor ID:", output.getvalue())
    check.is_in("Mitre Techniques:", output.getvalue())

    f_path = q_func("print")
    check.is_in("https://raw.githubusercontent.com/OTRF/Security-Datasets", f_path)

    d_frame = q_func()
    check.is_instance(d_frame, pd.DataFrame)
    check.greater_equal(len(d_frame), 10)
