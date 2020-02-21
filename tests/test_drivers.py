# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
from pytest import raises
from ..msticpy.data.drivers import MDATPDriver, SecurityGraphDriver
from ..msticpy.nbtools.utility import MsticpyException

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
    mdatp = MDATPDriver()
    assert type(mdatp) == MDATPDriver
    with raises(ConnectionError):
        mdatp.connect(
            connection_str="tenant_id=Test;client_id=Test;client_secret=Test;apiRoot=Test;apiVersion=Test"
        )
    with raises(MsticpyException):
        mdatp.connect(app_name="MDATP_TEST")


def test_SecurityGraph():
    sec_graph = SecurityGraphDriver()
    assert type(sec_graph) == SecurityGraphDriver
