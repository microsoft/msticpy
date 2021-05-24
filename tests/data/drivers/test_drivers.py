# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
from pytest import raises

from msticpy.common.exceptions import MsticpyException
from msticpy.data import DataEnvironment
from msticpy.data.drivers import import_driver

from msticpy.data.drivers.mdatp_driver import MDATPDriver
from msticpy.data.drivers.security_graph_driver import SecurityGraphDriver
from msticpy.data.drivers.resource_graph_driver import ResourceGraphDriver

_JSON_RESP = {
    "token_type": "Bearer",
    "expires_in": "3599",
    "ext_expires_in": "3599",
    "expires_on": "1582155956",
    "not_before": "1582152056",
    "resource": "https://api.securitycenter.windows.com",
    "access_token": None,
}


def test_MDATP():
    driver_cls = import_driver(DataEnvironment.MDATP)
    mdatp = driver_cls()
    assert isinstance(mdatp, MDATPDriver)
    with raises(ConnectionError):
        mdatp.connect(
            connection_str="tenant_id=Test;client_id=Test;client_secret=Test;apiRoot=Test;apiVersion=Test"
        )
    with raises(MsticpyException):
        mdatp.connect(app_name="MDATP_TEST")


def test_SecurityGraph():
    driver_cls = import_driver(DataEnvironment.SecurityGraph)
    sec_graph = driver_cls()
    assert isinstance(sec_graph, SecurityGraphDriver)


def test_ResourceGraph():
    driver_cls = import_driver(DataEnvironment.ResourceGraph)
    resource_graph = driver_cls()
    assert isinstance(resource_graph, ResourceGraphDriver)
