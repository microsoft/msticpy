# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test TI Browser."""

from html import escape
from html.parser import HTMLParser
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.vis.ti_browser import (
    _reference_html,
    _ti_detail_table,
    get_ti_select_options,
    raw_results,
    ti_details_display,
)

from ..unit_test_lib import TEST_DATA_PATH

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def ti_results():
    """Test fixture to read Dataframe."""
    df_path = Path(TEST_DATA_PATH).joinpath("ti_results.df.pkl")
    return pd.read_pickle(df_path)


def test_extract_options(ti_results):
    """Test extracting select list options from data."""
    output = get_ti_select_options(ti_results)
    check.equal(len(output), 9)
    output = get_ti_select_options(ti_results, severities=["warning"])
    check.equal(len(output), 4)


def test_display_function(ti_results):
    """Test getting and executing the display function."""
    disp_func = ti_details_display(ti_results)
    for _, row in ti_results.iterrows():
        html = disp_func((row.Ioc, [row.Provider]))
        check.is_in("Reference:", html.data)
        check.is_in(f"Provider: {row.Provider}", html.data)


@pytest.mark.parametrize(
    "field", ["Ioc", "IocType", "Provider", "Severity", "Details", "RawResult", "Reference"]
)
def test_display_escapes_provider_fields(field):
    """Render every provider-controlled field as text, not HTML."""
    marker = '<em>unexpected&"quoted"</em>'
    record = {
        "Ioc": "example.org",
        "IocType": "dns",
        "Provider": "test-provider",
        "Severity": "high",
        "Details": {"name": "ordinary details"},
        "RawResult": {"name": "ordinary response"},
        "Reference": "https://example.org/",
    }
    record[field] = marker
    data = pd.DataFrame([record])
    output = ti_details_display(data)((record["Ioc"], [record["Provider"]])).data
    assert "<em>" not in output
    assert escape(marker) in output
    assert "<h2 " in output
    assert "<table " in output


def test_display_escapes_nested_details():
    """Escape keys and values at every nesting level."""
    marker = '<em>unexpected&"quoted"</em>'
    output = _ti_detail_table({marker: {marker: [marker], "count": 42}})
    assert "<em>" not in output
    assert escape(marker) in output
    assert "<td class='cell_ti'>42</td>" in output


@pytest.mark.parametrize("details", ["<em>unexpected</em>", 42, None, ["<em>item</em>"]])
def test_display_scalar_details(details):
    """Return escaped scalar and list details without breaking fragment joining."""
    output = _ti_detail_table(details)
    assert escape(str(details)) in output
    assert "<em>" not in output


def test_raw_results_escapes_markup():
    """Escape raw responses before inserting application-owned formatting."""
    output = raw_results({"<em>key</em>": "<em>value</em>\nnext line"})
    assert "<em>" not in output
    assert "&lt;em&gt;key&lt;/em&gt;" in output
    assert "&lt;em&gt;value&lt;/em&gt;" in output
    assert "<details>" in output
    assert "<pre " in output


class _LinkParser(HTMLParser):
    """Collect rendered hyperlink attributes."""

    def __init__(self):
        """Initialize the hyperlink collection."""
        super().__init__()
        self.links = []

    def handle_starttag(self, tag, attrs):
        """Record anchor attributes after HTML entity decoding."""
        if tag == "a":
            self.links.append(dict(attrs))


@pytest.mark.parametrize(
    ("reference", "is_link"),
    [
        ("https://example.org/?q='quoted'&id=1", True),
        ("http://example.org/", True),
        ("HTTPS://example.org/", True),
        ("javascript:void(0)", False),
        ("data:text/html,<em>unexpected</em>", False),
        ("//example.org/", False),
        ("/relative/path", False),
        ("https:///missing-host", False),
        ("https://[invalid", False),
        (None, False),
    ],
)
def test_reference_link_validation(reference, is_link):
    """Only absolute HTTP/HTTPS references become safely quoted hyperlinks."""
    output = _reference_html(reference)
    parser = _LinkParser()
    parser.feed(output)
    if is_link:
        assert parser.links == [{"href": reference}]
        assert escape(str(reference)) in output
    else:
        assert not parser.links
        assert output == escape(str(reference))
