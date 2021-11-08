# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Miscellaneous data provider driver tests."""
import pytest
import pytest_check as check

from msticpy.common.exceptions import MsticpyException, MsticpyConnectionError
from msticpy.data import DataEnvironment, QueryProvider
from msticpy.data.drivers import import_driver

from msticpy.data.drivers.mdatp_driver import MDATPDriver
from msticpy.data.drivers.security_graph_driver import SecurityGraphDriver

_RGE_IMP_OK = False
try:
    from msticpy.data.drivers.resource_graph_driver import ResourceGraphDriver

    _RGE_IMP_OK = True
except ImportError:
    pass


# pylint: disable=protected-access


_JSON_RESP = {
    "token_type": "Bearer",
    "expires_in": "3599",
    "ext_expires_in": "3599",
    "expires_on": "1582155956",
    "not_before": "1582152056",
    "resource": "https://api.securitycenter.windows.com",
    "access_token": None,
}

_MDEF_TESTS = [
    (DataEnvironment.MDE, "https://api.securitycenter.microsoft.com"),
    (DataEnvironment.MDATP, "https://api.securitycenter.microsoft.com"),
    (DataEnvironment.MD365, "https://api.security.microsoft.com"),
]


@pytest.mark.parametrize("env, api", _MDEF_TESTS)
def test_MDE_driver(env, api):
    """Test class MDE driver."""
    driver_cls = import_driver(env)
    driver = driver_cls(data_environment=env)
    check.is_instance(driver, MDATPDriver)
    check.equal(driver.api_root, api)

    qry_prov = QueryProvider(env.name)
    driver = qry_prov._query_provider
    check.is_instance(driver, MDATPDriver)
    check.equal(driver.api_root, api)
    with pytest.raises(MsticpyConnectionError):
        # expect a connection failure
        qry_prov.connect(
            connection_str="tenant_id=Test;client_id=Test;client_secret=Test;apiRoot=Test;apiVersion=Test"
        )
    with pytest.raises(MsticpyException):
        # expect an error due to missing parameters
        qry_prov.connect(app_name="MDATP_TEST")


def test_SecurityGraph():
    """Test security graph driver."""
    driver_cls = import_driver(DataEnvironment.SecurityGraph)
    sec_graph = driver_cls()
    assert isinstance(sec_graph, SecurityGraphDriver)


@pytest.mark.skipif(not _RGE_IMP_OK, reason="Partial msticpy install")
def test_ResourceGraph():
    """Test resource graph driver."""
    driver_cls = import_driver(DataEnvironment.ResourceGraph)
    resource_graph = driver_cls()
    assert isinstance(resource_graph, ResourceGraphDriver)
