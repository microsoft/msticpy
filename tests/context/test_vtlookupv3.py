# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""vtlookupv3 unit tests."""
import json
import os
import sys
from typing import Any, Dict, Iterator, Optional
from unittest.mock import patch

import ipywidgets as widgets
import pandas as pd
import pytest
import pytest_check as check
from bokeh.layouts import LayoutDOM
from vt.object import Object as VtObject

from msticpy.context.vtlookupv3 import vtlookupv3

# import this to make pandas plot accessors work
# pylint: disable=unused import
from msticpy.vis import mp_pandas_plot  # noqa: F401
from msticpy.vis.vtobject_browser import VTObjectBrowser

from ..unit_test_lib import get_test_data_path

# pylint: disable=redefined-outer-name, protected-access, unspecified-encoding
# pylint: disable=no-value-for-parameter

VTLookupV3 = vtlookupv3.VTLookupV3
VT_API_NOT_FOUND = vtlookupv3.VT_API_NOT_FOUND
MsticpyVTNoDataError = vtlookupv3.MsticpyVTNoDataError
VTFileBehavior = vtlookupv3.VTFileBehavior
VTBrowser = None  # vtlookupv3.VTObjectBrowser

print(VTLookupV3.__module__)
__author__ = "Ian Hellen"

_VT_MODULE_PATH = "msticpy.context.vtlookupv3"


@patch(f"{_VT_MODULE_PATH}.vt")
def create_vt_client(vt_lib) -> VTLookupV3:
    """Test simple lookup of IoC."""
    vt_lib.Client = VTClient
    vt_lib.APIError = VTAPIError

    return VTLookupV3()


@pytest.fixture
def vt_client(monkeypatch):
    """Return a VTLookup instance."""
    vt_lib_fb = sys.modules[f"{_VT_MODULE_PATH}.vtfile_behavior"]
    vt = getattr(vt_lib_fb, "vt")
    monkeypatch.setattr(vt, "Client", VTClient)
    monkeypatch.setattr(vt, "APIError", VTAPIError)
    return create_vt_client()


_D_ROOT = get_test_data_path()
_TEST_URLS = ["one", "two", "three"]


class VTAPIError(Exception):
    """Mock VT API Error."""

    def __init__(self, code, message, *args):
        """Initialize."""
        ex_args = [message, code, *args]
        super().__init__(*ex_args)


class VTRelationCounters:
    """Mock result for VT relation counters."""

    def __init__(self, count=0):
        """Initialize."""
        self.count = count

    @property
    def relationships(self):
        """Return relationships dict."""
        return {"contacted_urls": {"meta": {"count": self.count}}}


class VTClient:
    """Mock V3 VT client."""

    _OBJ_FILES = ["vt3_url_0.json", "vt3_url_1.json", "vt3_url_2.json"]
    _REL_LINK_FILE = "vt3_related_links.json"
    _FB_SUM_FILE = "vt3_behavior_summary.json"
    _FB_MS_FILE = "vt3_behavior_ms_sysinternals.json"
    _FILE_SUMMARY = "vt3_file_1.json"

    _URL_OBJS = [
        json.loads(_D_ROOT.joinpath(url_file).read_text()) for url_file in _OBJ_FILES
    ]
    _URL_LINKS = json.loads(_D_ROOT.joinpath(_REL_LINK_FILE).read_text())
    _VT_FB_SUMMARY = json.loads(_D_ROOT.joinpath(_FB_SUM_FILE).read_text())
    _VT_FB_MSSYS = json.loads(_D_ROOT.joinpath(_FB_MS_FILE).read_text())
    _VT_FILE_SUMMARY = json.loads(_D_ROOT.joinpath(_FILE_SUMMARY).read_text())

    def __init__(self, apikey: Optional[str] = None):
        """Initialize the class."""
        self._vt_key = apikey

    def get_object(self, path: str, *path_args, params=None) -> Any:
        """Fetch the object from VT."""
        del path_args, params
        if "relationship_counters" in path:
            if "file" in path:
                return VTRelationCounters(count=3)
            return VTRelationCounters(count=0)
        if "one" in path:
            return VtObject.from_dict(self._URL_OBJS[0])
        if "two" in path:
            return VtObject.from_dict(self._URL_OBJS[1])
        if "three" in path:
            return VtObject.from_dict(self._URL_OBJS[2])
        if "file" in path:
            return VtObject.from_dict(self._VT_FILE_SUMMARY)
        if "not-found" in path:
            raise VTAPIError(message=VT_API_NOT_FOUND, code=404)
        raise VTAPIError(message="Some error", code=404)

    def get_data(self, path: str, *path_args, params=None) -> Dict[str, Any]:
        """Fetch the file behavior data from VT."""
        del path_args, params
        if "behaviour_summary" in path:
            return self._VT_FB_SUMMARY
        if "file_behaviours" in path:
            return self._VT_FB_MSSYS
        raise VTAPIError(message=VT_API_NOT_FOUND, code=404)

    def iterator(
        self, path: str, *path_args, params=None, cursor=None, limit=0, batch_size=0
    ) -> Iterator:
        """Return an iterator of VT objects."""
        del path_args, params, cursor, limit, batch_size
        if "relationships" in path:
            return iter(VtObject.from_dict(url_data) for url_data in self._URL_LINKS)
        return iter(VtObject.from_dict(url_data) for url_data in self._URL_OBJS)

    def close(self):
        """Close the client connection."""
        return None


def test_init_vt_lookup_class():
    """Test fetching a key from settings."""
    test_vt_key = "somerandomvalue"

    vt_client = VTLookupV3(vt_key=test_vt_key)
    check.equal(vt_client._vt_key, test_vt_key)

    try:
        curr_vt_key = os.environ.get("VIRUSTOTAL_AUTH", "")
        os.environ["VIRUSTOTAL_AUTH"] = test_vt_key
        vt_client = VTLookupV3(vt_key=test_vt_key)
        check.equal(vt_client._vt_key, test_vt_key)
    finally:
        os.environ["VIRUSTOTAL_AUTH"] = curr_vt_key


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_lookup_ioc(vt_client):
    """Test simple lookup of IoC."""
    url = _TEST_URLS[0]

    # Simple lookup
    result_df = vt_client.lookup_ioc(url, vt_type="url")
    check.equal(result_df.shape, (1, 7))
    check.equal(
        result_df.iloc[0].id,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].type, "url")

    # all properties
    result_df = vt_client.lookup_ioc(url, vt_type="url", all_props=True)
    check.equal(result_df.shape, (1, 287))

    # Invalid type
    with pytest.raises(ValueError) as vt_error:
        result_df = vt_client.lookup_ioc(url, vt_type="invalid_type")
    check.equal(vt_error.value.args[0], "'invalid_type' is not a valid VTEntityType")

    # Nonexistent URL
    result_df = vt_client.lookup_ioc("not-found", vt_type="url")
    check.equal(result_df.shape, (1, 5))
    check.equal(result_df.iloc[0].type, "url")
    check.equal(result_df.iloc[0].id, "not-found")
    check.equal(result_df.iloc[0].last_submission_date, "Not found")

    # Other failure
    with pytest.raises(MsticpyVTNoDataError):
        result_df = vt_client.lookup_ioc("fail", vt_type="url")


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_lookup_iocs(vt_client):
    """Test lookup of multiple IoCs."""
    # Simple lookup
    obs_df = pd.DataFrame(
        {
            "observable": _TEST_URLS,
            "type": ["url"] * 3,
        }
    )

    # Simple lookup of multiple items
    result_df = vt_client.lookup_iocs(
        obs_df, observable_column="observable", observable_type_column="type"
    )
    check.equal(result_df.shape, (3, 7))
    check.equal(
        result_df.iloc[0].id,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].type, "url")

    # All properties
    result_df = vt_client.lookup_iocs(
        obs_df,
        observable_column="observable",
        observable_type_column="type",
        all_props=True,
    )
    check.equal(result_df.shape, (3, 399))
    check.equal(
        result_df.iloc[0].id,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(
        result_df.iloc[1].id,
        "19e1199c6aa6e817845cc025cd7c8979cec22f8c94bc7416ff16b8808706cd54",
    )
    check.equal(result_df.iloc[0].type, "url")

    # With unknown item
    obs_df = pd.DataFrame(
        {"observable": _TEST_URLS + ["not-found"], "type": ["url"] * 4}
    )
    result_df = vt_client.lookup_iocs(
        obs_df, observable_column="observable", observable_type_column="type"
    )
    check.equal(result_df.shape, (4, 7))
    check.equal(result_df.iloc[3].type, "url")
    check.equal(result_df.iloc[3].id, "not-found")
    check.equal(result_df.iloc[3].last_submission_date, "Not found")


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_lookup_ioc_relationship(vt_client: VTLookupV3):
    """Test lookup relationship links."""
    file = "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8"

    # Simple lookup
    result_df = vt_client.lookup_ioc_relationships(
        file, vt_type="file", relationship="contacted_urls"
    )
    check.equal(len(result_df), 3)
    result_df_noidx = result_df.reset_index()
    check.equal(
        result_df_noidx.iloc[0].target,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].relationship_type, "contacted_urls")

    # failed lookup
    result_df = vt_client.lookup_ioc_relationships(
        file, vt_type="ip_address", relationship="contacted_urls"
    )
    check.equal(result_df.shape, (1, 6))
    check.equal(result_df.iat[0, 2], "Not found")


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_lookup_ioc_related(vt_client: VTLookupV3):
    """Test lookup related objects."""
    file = "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8"

    # Lookup related
    result_df = vt_client.lookup_ioc_related(
        file, vt_type="file", relationship="contacted_urls"
    )
    check.equal(result_df.shape, (3, 403))
    result_df_noidx = result_df.reset_index()
    check.equal(
        result_df_noidx.iloc[0].target,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].relationship_type, "contacted_urls")


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_lookup_iocs_relationships(vt_client: VTLookupV3):
    """Test lookup of IoC relationships for multiple IoCs."""
    # Lookup related items for multi IoCs
    files = [
        f"380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f{idx}"
        for idx in range(3)
    ]
    obs_df = pd.DataFrame(
        {
            "observable": files,
            "type": ["file"] * 3,
        }
    )
    result_df = vt_client.lookup_iocs_relationships(
        observables_df=obs_df,
        observable_column="observable",
        observable_type_column="type",
        relationship="contacted_urls",
    )
    check.equal(len(result_df), 9)
    result_df_noidx = result_df.reset_index()
    for idx in range(0, 9, 3):
        check.equal(
            result_df_noidx.iloc[idx].target,
            "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
        )
        check.equal(result_df.iloc[idx].relationship_type, "contacted_urls")


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_get_object(vt_client: VTLookupV3):
    """Test simple get_object api."""
    result_df = vt_client.get_object(_TEST_URLS[0], vt_type="url")
    check.equal(result_df.shape, (1, 286))
    check.equal(
        result_df.iloc[0].id,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].type, "url")


_EXP_KEYS = {
    "processes_injected",
    "processes_terminated",
    "files_opened",
    "processes_created",
    "registry_keys_set",
    "processes_tree",
}
_FB_TESTS = [
    ("Summary", None, _EXP_KEYS),
    ("microsoft_sysinternals", "microsoft_sysinternals", _EXP_KEYS),
]

_FB_TEST_IDS = [test[0] for test in _FB_TESTS]


@pytest.mark.filterwarnings("ignore::UserWarning")
@pytest.mark.parametrize("name, sandbox, keys", _FB_TESTS, ids=_FB_TEST_IDS)
def test_file_behavior(vt_client: VTLookupV3, name, sandbox, keys):
    """Test get_file_behavior api."""
    del name
    file = "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8"
    vt_file_behavior = vt_client.get_file_behavior(file_id=file, sandbox=sandbox)
    check.is_instance(vt_file_behavior, VTFileBehavior)
    check.is_true(vt_file_behavior.has_behavior_data)
    check.is_false(keys - vt_file_behavior.categories.keys())

    browser = vt_file_behavior.browse()
    check.is_instance(browser, widgets.VBox)

    proc_tree = vt_file_behavior.process_tree
    check.is_instance(proc_tree, LayoutDOM)

    check.is_instance(vt_file_behavior.process_tree_df, pd.DataFrame)


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_get_object_browser(vt_client: VTLookupV3):
    """Test object browser."""
    del vt_client
    vt_browser = VTObjectBrowser()
    # emulate looking up a file using UI
    vt_browser.txt_file_id.value = "file"
    vt_browser.btn_lookup.click()

    check.equal(vt_browser._current_data.shape, (1, 584))
    check.equal(
        vt_browser._current_data.iloc[0].id,
        "03bd9a94482f180bb047626cb2f27ccf8daa0e201345480b43585580e09c311b",
    )
    check.equal(vt_browser._current_data.iloc[0].type, "file")
    check.equal(set(vt_browser._current_data.columns), set(vt_browser.data_sel.options))

    # Check that it auto-loads from init
    vt_browser = VTObjectBrowser("file")
    check.equal(vt_browser._current_data.shape, (1, 584))
    check.equal(
        vt_browser._current_data.iloc[0].id,
        "03bd9a94482f180bb047626cb2f27ccf8daa0e201345480b43585580e09c311b",
    )
