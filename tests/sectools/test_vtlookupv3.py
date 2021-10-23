# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""vtlookupv3 unit tests."""
import json
import os
from typing import Any, Optional, Iterator

import pandas as pd
from unittest.mock import patch
import pytest
import pytest_check as check

from msticpy.sectools.vtlookupv3 import (
    VTLookupV3,
    _VT_API_NOT_FOUND,
    MsticpyVTNoDataError,
)
from vt.object import Object as VtObject

from ..unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access, unspecified-encoding


@patch(VTLookupV3.__module__ + ".vt")
def create_vt_client(vt_lib) -> VTLookupV3:
    """Test simple lookup of IoC."""
    vt_lib.Client = VTClient
    vt_lib.APIError = VTAPIError

    return VTLookupV3()


@pytest.fixture
def vt_client():
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

    _URL_OBJS = [
        json.loads(_D_ROOT.joinpath(url_file).read_text()) for url_file in _OBJ_FILES
    ]
    _URL_LINKS = json.loads(_D_ROOT.joinpath(_REL_LINK_FILE).read_text())

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
        if "not-found" in path:
            raise VTAPIError(message=_VT_API_NOT_FOUND, code=404)
        raise VTAPIError(message="Some error", code=404)

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


@patch(VTLookupV3.__module__ + ".vt")
def test_lookup_ioc(vt_lib):
    """Test simple lookup of IoC."""
    vt_lib.Client = VTClient
    vt_lib.APIError = VTAPIError
    vt_client = VTLookupV3()
    url = _TEST_URLS[0]

    # Simple lookup
    result_df = vt_client.lookup_ioc(url, vt_type="url")
    check.equal(result_df.shape, (1, 6))
    check.equal(
        result_df.iloc[0].name,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].type, "url")

    # all properties
    result_df = vt_client.lookup_ioc(url, vt_type="url", all_props=True)
    check.equal(result_df.shape, (1, 286))

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


@patch(VTLookupV3.__module__ + ".vt")
def test_lookup_iocs(vt_lib):
    """Test lookup of multiple IoCs."""
    vt_lib.Client = VTClient
    vt_lib.APIError = VTAPIError
    vt_client = VTLookupV3()

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
    check.equal(result_df.shape, (3, 6))
    check.equal(
        result_df.iloc[0].name,
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
    check.equal(result_df.shape, (3, 398))
    check.equal(
        result_df.iloc[0].name,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(
        result_df.iloc[1].name,
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


def test_lookup_ioc_relationship(vt_client: VTLookupV3):
    """Test lookup relationship links."""
    file = "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8"

    # Simple lookup
    result_df = vt_client.lookup_ioc_relationships(
        file, vt_type="file", relationship="contacted_urls"
    )
    check.equal(result_df.shape, (3, 4))
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


def test_lookup_ioc_related(vt_client: VTLookupV3):
    """Test lookup related objects."""
    file = "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8"

    # Lookup related
    result_df = vt_client.lookup_ioc_related(
        file, vt_type="file", relationship="contacted_urls"
    )
    check.equal(result_df.shape, (3, 400))
    result_df_noidx = result_df.reset_index()
    check.equal(
        result_df_noidx.iloc[0].target,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].relationship_type, "contacted_urls")


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
    check.equal(result_df.shape, (9, 4))
    result_df_noidx = result_df.reset_index()
    for idx in range(0, 9, 3):
        check.equal(
            result_df_noidx.iloc[idx].target,
            "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
        )
        check.equal(result_df.iloc[idx].relationship_type, "contacted_urls")


def test_get_object(vt_client: VTLookupV3):
    """Test simple get_object api."""
    result_df = vt_client.get_object(_TEST_URLS[0], vt_type="url")
    check.equal(result_df.shape, (1, 286))
    check.equal(
        result_df.iloc[0].id,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].type, "url")


def test_get_object(vt_client: VTLookupV3):
    """Test simple get_object api."""
    result_df = vt_client.get_object(_TEST_URLS[0], vt_type="url")
    check.equal(result_df.shape, (1, 286))
    check.equal(
        result_df.iloc[0].id,
        "380269259e1f607fb07769fee779f0dc3144924f865e76a3c05c8898295d02f8",
    )
    check.equal(result_df.iloc[0].type, "url")
