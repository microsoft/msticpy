# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous data provider driver tests."""
import json
import re
from hashlib import sha256

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.data.core.query_defns import Formatters
from msticpy.data.drivers.cybereason_driver import CybereasonDriver

from ...unit_test_lib import custom_mp_config, get_test_data_path

MP_PATH = str(get_test_data_path().parent.joinpath("msticpyconfig-test.yaml"))
# pylint: disable=protected-access

_CR_RESULT = {
    "data": {
        "resultIdToElementDataMap": {
            "id1": {
                "simpleValues": {
                    "osType": {"totalValues": 1, "values": ["WINDOWS"]},
                    "totalMemory": {
                        "totalValues": 1,
                        "values": ["8589463552"],
                    },
                    "group": {
                        "totalValues": 1,
                        "values": ["00000000-0000-0000-0000-000000000000"],
                    },
                    "osVersionType": {
                        "totalValues": 1,
                        "values": ["Windows_10"],
                    },
                },
                "elementValues": {
                    "users": {
                        "totalValues": 5,
                        "elementValues": [],
                        "totalSuspicious": 0,
                        "totalMalicious": 0,
                        "guessedTotal": 0,
                    }
                },
            }
        },
        "paginationToken": None,
        "totalResults": 1,
    },
    "status": "SUCCESS",
    "message": "",
    "expectedResults": 0,
    "failures": 0,
}

_CR_PAGINATED_RESULT = [
    {
        "data": {
            "resultIdToElementDataMap": {
                "id1": {
                    "simpleValues": {
                        "osType": {"totalValues": 1, "values": ["WINDOWS"]},
                        "totalMemory": {
                            "totalValues": 1,
                            "values": ["8589463552"],
                        },
                        "group": {
                            "totalValues": 1,
                            "values": ["00000000-0000-0000-0000-000000000000"],
                        },
                        "osVersionType": {
                            "totalValues": 1,
                            "values": ["Windows_10"],
                        },
                    },
                    "elementValues": {
                        "users": {
                            "totalValues": 5,
                            "elementValues": [],
                            "totalSuspicious": 0,
                            "totalMalicious": 0,
                            "guessedTotal": 0,
                        }
                    },
                }
            },
            "paginationToken": None,
            "totalResults": 2,
        },
        "status": "SUCCESS",
        "message": "",
        "expectedResults": 0,
        "failures": 0,
    },
    {
        "data": {
            "resultIdToElementDataMap": {
                "id2": {
                    "simpleValues": {
                        "osType": {"totalValues": 1, "values": ["WINDOWS"]},
                        "totalMemory": {
                            "totalValues": 1,
                            "values": ["8589463552"],
                        },
                        "group": {
                            "totalValues": 1,
                            "values": ["00000000-0000-0000-0000-000000000000"],
                        },
                        "osVersionType": {
                            "totalValues": 1,
                            "values": ["Windows_10"],
                        },
                    },
                    "elementValues": {
                        "users": {
                            "totalValues": 5,
                            "elementValues": [],
                            "totalSuspicious": 0,
                            "totalMalicious": 0,
                            "guessedTotal": 0,
                        }
                    },
                }
            },
            "paginationToken": None,
            "totalResults": 2,
        },
        "status": "SUCCESS",
        "message": "",
        "expectedResults": 0,
        "failures": 0,
    },
]

_CR_QUERY = {
    "query": """
    {
        "queryPath" : [
            {
                "requestedType": "File",
                "filters":[
                    {
                        "facetName": "elementDisplayName",
                        "values": [ "{fileName}" ],
                        "filterType":"MatchesWildcard"
                    },
                    {
                        "facetName": "{timefield}",
                        "values": [ "{start}", "{end}" ],
                        "filterType":"Between"
                    }
                ],
                "isResult": true
            }
        ],
        "customFields": ["{customFields}"]
    }
    """,
    "params": {
        "fileName": ["file1", "file2"],
        "timefield": "creationTime",
        "start": 1667471841766,
        "end": 1667471841767,
        "customFields": ["elementDisplayName"],
    },
    "result": {
        "queryPath": [
            {
                "requestedType": "File",
                "filters": [
                    {
                        "facetName": "elementDisplayName",
                        "values": ["file1", "file2"],
                        "filterType": "MatchesWildcard",
                    },
                    {
                        "facetName": "creationTime",
                        "values": [1667471841766, 1667471841767],
                        "filterType": "Between",
                    },
                ],
                "isResult": True,
            }
        ],
        "customFields": ["elementDisplayName"],
    },
}


@pytest.fixture(scope="module", name="driver")
def cybereason_driver():
    """Generate Cybereason Driver for testing."""
    driver = CybereasonDriver()
    _cr_pre_checks(driver)
    return driver


# Test helper functions for Cybereason tests
def _cr_pre_checks(driver: CybereasonDriver):
    check.is_instance(driver, CybereasonDriver)
    check.is_true(driver._loaded)
    check.is_false(driver.connected)
    check.is_false(driver.client.cookies)


@respx.mock
def test_connect(driver):
    """Test connect."""
    connect = respx.post(
        re.compile(r"^https://[a-zA-Z0-9\-]+\.cybereason\.net/login\.html")
    ).respond(200)
    with custom_mp_config(MP_PATH):
        driver.connect()
        check.is_true(connect.called)
        check.is_true(driver._connected)


@respx.mock
def test_query(driver):
    """Test query calling returns data in expected format."""
    connect = respx.post(
        re.compile(r"^https://[a-zA-Z0-9\-]+\.cybereason\.net/login\.html")
    ).respond(200)
    query = respx.post(
        re.compile(
            r"^https://[a-zA-Z0-9\-]+\.cybereason\.net/rest/visualsearch/query/simple"
        )
    ).respond(200, json=_CR_RESULT)
    with custom_mp_config(MP_PATH):
        driver.connect()
        data = driver.query('{"test": "test"}')
        check.is_true(connect.called or driver.connected)
        check.is_true(query.called)
        check.is_instance(data, pd.DataFrame)


@respx.mock
def test_paginated_query(driver):
    """Test query calling returns data in expected format."""
    connect = respx.post(
        re.compile(r"^https://[a-zA-Z0-9\-]+\.cybereason\.net/login.html")
    ).respond(200)
    query1 = respx.post(
        re.compile(
            r"^https://[a-zA-Z0-9\-]+\.cybereason\.net/rest/visualsearch/query/simple"
        ),
        params={"page": 0},
    ).respond(200, json=_CR_PAGINATED_RESULT[0])
    query2 = respx.post(
        re.compile(
            r"^https://[a-zA-Z0-9\-]+\.cybereason\.net/rest/visualsearch/query/simple"
        ),
        params={"page": 1},
    ).respond(200, json=_CR_PAGINATED_RESULT[1])
    with custom_mp_config(MP_PATH):
        driver.connect()
        data = driver.query('{"test": "test"}', page_size=1)
        check.is_true(connect.called or driver.connected)
        check.is_true(query1.called)
        check.is_true(query2.called)
        check.is_instance(data, pd.DataFrame)


def test_custom_param_handler(driver):
    """Test query formatter returns data in expected format."""
    query = _CR_QUERY.get("query", "")
    parameters = _CR_QUERY.get("params", {})
    updated_query = driver.formatters[Formatters.PARAM_HANDLER](
        query,
        parameters,
    )
    check.is_instance(updated_query, str)
    for parameter in parameters:
        check.is_not_in(f"{{{parameter}}}", updated_query)
    parsed_updated_query = json.loads(updated_query)
    hash_orig = sha256(
        json.dumps(parsed_updated_query, sort_keys=True).encode()
    ).hexdigest()
    hash_expected = sha256(
        json.dumps(_CR_QUERY["result"], sort_keys=True).encode()
    ).hexdigest()
    check.equal(hash_orig, hash_expected)
