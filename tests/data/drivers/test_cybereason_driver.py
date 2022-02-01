# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous data provider driver tests."""
import re

import respx
import pandas as pd
import pytest
import pytest_check as check

from msticpy.data.drivers.cybereason_driver import CybereasonDriver

from ...unit_test_lib import get_test_data_path, custom_mp_config


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
        }
    },
    "status": "SUCCESS",
    "message": "",
    "expectedResults": 0,
    "failures": 0,
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
    connect = respx.post(re.compile(r"https://.*.cybereason.net/login.html")).respond(
        200
    )
    with custom_mp_config(MP_PATH):
        driver.connect()
        check.is_true(connect.called)
        check.is_true(driver._connected)


@respx.mock
def test_query(driver):
    """Test query calling returns data in expected format."""
    query = respx.post(
        re.compile(r"https://.*.cybereason.net/rest/visualsearch/query/simple")
    ).respond(200, json=_CR_RESULT)
    with custom_mp_config(MP_PATH):
        data = driver.query('{"test": "test"}')
        check.is_true(query.called)
        check.is_instance(data, pd.DataFrame)
