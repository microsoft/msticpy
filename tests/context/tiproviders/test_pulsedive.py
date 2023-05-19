# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test Pulsedive TI Provider."""
import json
import re

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.context.tilookup import TILookup
from msticpy.context.tiproviders.pulsedive import PDlookup

from ...unit_test_lib import custom_mp_config, get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name

_INDICATOR_TEXT = r"""
{
    "qid": null,
    "iid": 53929,
    "indicator": "pulsedive.com",
    "type": "domain",
    "risk": "high",
    "risk_recommended": "none",
    "manualrisk": 0,
    "retired": null,
    "stamp_added": "2017-10-04 01:20:55",
    "stamp_updated": "2023-03-07 17:00:19",
    "riskfactors": [
        {
            "rfid": 70,
            "description": "Linked IPs return PTR records",
            "risk": "none"
        }
    ],
    "threats": ["zeus"],
    "feeds": [],
    "comments": [
        {
            "cid": 125987,
            "username": "sherd",
            "title": "Founder",
            "comment": "Have you seen our Pro tier?",
            "stamp_added": "2019-12-18 21:13:46",
            "stamp_updated": "2020-11-10 10:03:01"
        }
    ]
}
"""
_INDICATOR_RESP = json.loads(_INDICATOR_TEXT)
_PAYLOAD = r"""
{
    "qid": 774460693,
    "iid": 53929,
    "indicator": "pulsedive.com",
    "type": "domain",
    "risk": "none",
    "risk_recommended": "none",
    "stamp_updated": "2023-03-08 02:28:58",
    "stamp_seen": "2023-03-08 02:28:58",
    "stamp_probed": "2023-02-27 08:56:57",
    "stamp_retired": null,
    "recent": 0,
    "submissions": 487,
    "riskfactors": [
        {
            "rfid": 70,
            "description": "Linked IPs return PTR records",
            "risk": "none"
        }
    ],
    "redirects": {
        "from": [],
        "to": [
            {
                "iid": 642010,
                "indicator": "https:\/\/pulsedive.com\/"
            }
        ]
    },
    "threats": [],
    "feeds": [],
    "comments": [
        {
            "cid": 125987,
            "username": "sherd",
            "title": "Founder",
            "comment": "Have you seen our Pro tier? \ud83d\ude0e",
            "stamp_added": "2019-12-18 21:13:46",
            "stamp_updated": "2020-11-10 10:03:01"
        }
    ],
    "attributes": {
    },
    "properties": {
    },
    "links": {
    }
}
"""
_SCAN_RESP = {
    "success": "Submitted indicator.",
    "data": json.loads(_PAYLOAD),
    "qid": 774460693,
    "status": "done",
}
_QUERY_RESP = {
    "success": "Submitted indicator.",
    "results": json.loads(_PAYLOAD),
    "qid": 774460693,
    "status": "done",
}
_SUBMITTED_TEXT = """
{
  "success": "Added request to queue.",
  "qid": 774460693
}
"""
_SUBMITTED_RESP = json.loads(_SUBMITTED_TEXT)


@respx.mock
def test_pdlookup_lookup_ioc():
    """Test PDLookup functions."""
    route = respx.get(re.compile(r"https://pulsedive\.com/api/info\.php.*")).respond(
        200, json=_INDICATOR_RESP
    )

    pd_lookup = PDlookup(pd_key="ACCESS")
    result = pd_lookup.lookup_ioc("21.21.21.21")
    check.is_instance(result, pd.DataFrame)
    check.equal(len(result), 1)
    check.equal(
        str(respx.calls[0].request.url),
        "https://pulsedive.com/api/info.php?pretty=1&key=ACCESS&indicator=21.21.21.21",
    )
    respx.get(re.compile(r"https://pulsedive\.com/api/info\.php.*")).respond(
        404, json={}
    )
    with pytest.raises(ValueError) as err:
        result = pd_lookup.lookup_ioc("21.bad.ip.21")


@respx.mock
def test_pdlookup_lookup_threat():
    """Test PDLookup functions."""
    respx.get(re.compile(r"https://pulsedive\.com/api/info\.php.*")).respond(
        200, json=_INDICATOR_RESP
    )

    pd_lookup = PDlookup(pd_key="ACCESS")
    result = pd_lookup.lookup_threat("zeus")
    check.is_instance(result, pd.DataFrame)
    check.equal(len(result), 1)
    check.equal(
        str(respx.calls[0].request.url),
        "https://pulsedive.com/api/info.php?pretty=1&key=ACCESS&threat=zeus",
    )
    respx.get(re.compile(r"https://pulsedive\.com/api/info\.php.*")).respond(
        404, json={}
    )
    with pytest.raises(ValueError) as err:
        result = pd_lookup.lookup_threat("no-zeus")


@respx.mock
def test_pdlookup_scan():
    """Test calling scan interface"""
    respx.post(re.compile(r"https://pulsedive\.com/api/analyze\.php.*")).respond(
        200, json=_SUBMITTED_RESP
    )

    respx.get(re.compile(r"https://pulsedive\.com/api/analyze\.php.*")).respond(
        200, json=_SCAN_RESP
    )

    pd_lookup = PDlookup(pd_key="ACCESS")
    result = pd_lookup.scan("https://evil.com")
    check.is_instance(result, pd.DataFrame)
    check.equal(len(result), 1)


@respx.mock
def test_pdlookup_scan_fail():
    """Test calling scan interface with failure."""
    respx.post(re.compile(r"https://pulsedive\.com/api/analyze\.php.*")).respond(
        404, json=""
    )

    pd_lookup = PDlookup(pd_key="ACCESS")
    with pytest.raises(ValueError):
        result = pd_lookup.scan("https://evil.com")


@respx.mock
def test_pdlookup_query():
    """Test calling query interface"""
    respx.get(re.compile(r"https://pulsedive\.com/api/explore\.php.*")).respond(
        200, json=_QUERY_RESP
    )

    pd_lookup = PDlookup(pd_key="ACCESS")
    result = pd_lookup.explore("https://evil.com")
    check.is_instance(result, pd.DataFrame)
    check.equal(len(result), 1)


IOCS = (
    ("212.3.45.67", "supported"),
    ("evil.com", "supported"),
    ("https://evil.com/api?name=test", "supported"),
    ("evildr@evil.com", "not-supported"),
)


@pytest.mark.filterwarnings("ignore::UserWarning")
@respx.mock
@pytest.mark.parametrize("ioc, expected", IOCS)
def test_pulsedive_tiprov(ioc, expected):
    """Test standard TI provider."""
    respx.get(re.compile(r"https://pulsedive\.com/api/info\.php.*")).respond(
        200, json=_INDICATOR_RESP
    )

    mp_conf_path = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(mp_conf_path):
        ti_lookup = TILookup()
        result = ti_lookup.lookup_ioc(ioc, providers=["Pulsedive"])

    check.is_instance(result, pd.DataFrame)

    if expected == "not-supported":
        check.equal(len(result), 0)
    else:
        check.equal(len(result), 1)
