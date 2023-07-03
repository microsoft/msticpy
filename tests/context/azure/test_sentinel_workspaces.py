# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel unit tests."""
import re
from collections import namedtuple

import pandas as pd
import pytest
import pytest_check as check
import respx

from msticpy.auth.azure_auth_core import AzureCloudConfig
from msticpy.context.azure import MicrosoftSentinel
from msticpy.data import QueryProvider

# pylint: disable=protected-access

_PORTAL_URLS = [
    "https://ms.portal.azure.com/#@microsoft.onmicrosoft.com/resource/subscriptions/d1d8779d-38d7-4f06-91db-9cbc8de0176f/resourceGroups/soc/providers/Microsoft.OperationalInsights/workspaces/cybersecuritysoc/Overview",
    "https://ms.portal.azure.com/#blade/Microsoft_Azure_Security_Insights/MainMenuBlade/0/id/%2Fsubscriptions%2F40dcc8bf-0478-4f3b-b275-ed0a94f2c013%2Fresourcegroups%2Fasihuntomsworkspacerg%2Fproviders%2Fmicrosoft.securityinsightsarg%2Fsentinel%2Fasihuntomsworkspacev4",
    "https://portal.azure.com/#asset/Microsoft_Azure_Security_Insights/Incident/subscriptions/d1d8779d-38d7-4f06-91db-9cbc8de0176f/resourceGroups/soc-purview/providers/Microsoft.OperationalInsights/workspaces/aipdstim/providers/Microsoft.SecurityInsights/Incidents/a31d57c2-f973-87e3-2081-517e08940301",
    "https://ms.portal.azure.com/#asset/Microsoft_Azure_Security_Insights/Incident/subscriptions/d1d8779d-9999-4f06-91db-9cbc8de0176f/resourceGroups/soc-na/providers/Microsoft.OperationalInsights/workspaces/non-existent/Overview",
]

_TENANT_LOOKUP_RESP = {
    "token_endpoint": "https://login.microsoftonline.com/72f988bf-86f1-41af-91ab-2d7cd011db47/oauth2/token",
    "token_endpoint_auth_methods_supported": [
        "client_secret_post",
        "private_key_jwt",
        "client_secret_basic",
    ],
    "jwks_uri": "https://login.microsoftonline.com/common/discovery/keys",
    "response_modes_supported": ["query", "fragment", "form_post"],
    "subject_types_supported": ["pairwise"],
    "id_token_signing_alg_values_supported": ["RS256"],
    "response_types_supported": [
        "code",
        "id_token",
        "code id_token",
        "token id_token",
        "token",
    ],
}

_WS_RES_GRAPH_DATA = [
    {
        "workspaceName": "CyberSecuritySOC",
        "workspaceId": "8ecf8077-cf51-4820-aadd-14040956f35d",
        "tenantId": "4b2462a4-bbee-495a-a0e1-f23ae524cc9c",
        "subscriptionId": "d1d8779d-38d7-4f06-91db-9cbc8de0176f",
        "resourceGroup": "soc",
        "id": (
            "/subscriptions/d1d8779d-38d7-4f06-91db-9cbc8de0176f/resourceGroups/SOC/"
            "providers/Microsoft.OperationalInsights/workspaces/CyberSecuritySoc"
        ),
    },
    {
        "workspaceName": "ASIHuntOMSWorkspaceV4",
        "workspaceId": "52b1ab41-869e-4138-9e40-2a4457f09bf0",
        "tenantId": "72f988bf-86f1-41af-91ab-2d7cd011db47",
        "subscriptionId": "40dcc8bf-0478-4f3b-b275-ed0a94f2c013",
        "resourceGroup": "asihuntomsworkspacerg",
        "id": (
            "/subscriptions/40dcc8bf-0478-4f3b-b275-ed0a94f2c013/resourceGroups/asihuntomsworkspacerg/"
            "providers/Microsoft.OperationalInsights/workspaces/ASIHuntOMSWorkspaceV4"
        ),
    },
    {
        "workspaceName": "AipDstim",
        "workspaceId": "0f926592-f41a-48b8-8a57-95422c50db93",
        "tenantId": "4b2462a4-bbee-495a-a0e1-f23ae524cc9c",
        "subscriptionId": "d1d8779d-38d7-4f06-91db-9cbc8de0176f",
        "resourceGroup": "soc-purview",
        "id": (
            "/subscriptions/d1d8779d-38d7-4f06-91db-9cbc8de0176f/resourceGroups/SOC-Purview/"
            "providers/Microsoft.OperationalInsights/workspaces/AipDstim"
        ),
    },
    {
        "workspaceName": "AipDstim",
        "workspaceId": "0f926592-f41a-48b8-8a57-95422c50db93",
        "tenantId": "4b2462a4-bbee-495a-a0e1-f23ae524cc9c",
        "subscriptionId": "40dcc8bf-0478-4f3b-b275-ed0a94f2c013",
        "resourceGroup": "soc-purview2",
        "id": (
            "/subscriptions/40dcc8bf-0478-4f3b-b275-ed0a94f2c013/resourceGroups/SOC-Purview2/"
            "providers/Microsoft.OperationalInsights/workspaces/AipDstim"
        ),
    },
]


def _get_ws_results(**kwargs):
    """Return results DF."""
    df = pd.DataFrame(_WS_RES_GRAPH_DATA)

    if kwargs.get("workspace_id"):
        return df[df.workspaceId.str.casefold() == kwargs["workspace_id"].casefold()]
    if kwargs.get("resource_id"):
        return df[df.id.str.casefold() == kwargs["resource_id"].casefold()]
    if kwargs.get("workspace_name") and kwargs.get("resource_group"):
        return df[
            (df.workspaceName.str.casefold() == kwargs["workspace_name"].casefold())
            & (
                df.resourceGroup.str.casefold()
                == kwargs.get("resource_group", "").casefold()
            )
        ]
    if kwargs.get("workspace_name") and kwargs.get("resource_group"):
        return df[
            (df.workspaceName.str.casefold() == kwargs["subscription_id"].casefold())
            & (
                df.subscriptionId.str.casefold()
                == kwargs.get("subscription_id", "").casefold()
            )
        ]
    if kwargs.get("workspace_name"):
        return df[
            df.workspaceName.str.casefold() == kwargs["workspace_name"].casefold()
        ]
    return pd.DataFrame(columns=df.columns)


TestExpected = namedtuple("TestExpected", "ws_name, sub_id, res_group, ws_id, ten_id")

_TEST_URL_LOOKUP = [
    (
        _PORTAL_URLS[0],
        TestExpected(
            "cybersecuritysoc",
            "d1d8779d-38d7-4f06-91db-9cbc8de0176f",
            "soc",
            "8ecf8077-cf51-4820-aadd-14040956f35d",
            "72f988bf-86f1-41af-91ab-2d7cd011db47",
        ),
        "cybersecuritysoc",
    ),
    (
        _PORTAL_URLS[1],
        TestExpected(
            "asihuntomsworkspacev4",
            "40dcc8bf-0478-4f3b-b275-ed0a94f2c013",
            "asihuntomsworkspacerg",
            "52b1ab41-869e-4138-9e40-2a4457f09bf0",
            "72f988bf-86f1-41af-91ab-2d7cd011db47",
        ),
        "asihuntomsworkspacev4",
    ),
    (
        _PORTAL_URLS[2],
        TestExpected(
            "aipdstim",
            "d1d8779d-38d7-4f06-91db-9cbc8de0176f",
            "soc-purview",
            "0f926592-f41a-48b8-8a57-95422c50db93",
            "4b2462a4-bbee-495a-a0e1-f23ae524cc9c",
        ),
        "aipdstim",
    ),
    (
        _PORTAL_URLS[3],
        TestExpected(
            "non-existent",
            "d1d8779d-9999-4f06-91db-9cbc8de0176f",
            "soc-na",
            "unknown",
            "unknown",
        ),
        "non-existent",
    ),
]


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
@respx.mock
def test_ws_details_from_url(url, expected, wk_space, monkeypatch):
    """Testing retrieving workspace details from portal url."""
    del wk_space
    login_endpoint = AzureCloudConfig().endpoints.active_directory
    respx.get(re.compile(f"{login_endpoint}.*")).respond(200, json=_TENANT_LOOKUP_RESP)

    _patch_qry_prov(monkeypatch)
    ws_details = MicrosoftSentinel.get_workspace_details_from_url(url)
    ws_details = next(iter(ws_details.values()))

    check.equal(ws_details["WorkspaceName"].casefold(), expected.ws_name.casefold())
    check.equal(ws_details["SubscriptionId"].casefold(), expected.sub_id.casefold())
    check.equal(ws_details["ResourceGroup"].casefold(), expected.res_group.casefold())
    check.equal(ws_details["WorkspaceId"].casefold(), expected.ws_id.casefold())
    check.equal(ws_details["TenantId"].casefold(), expected.ten_id.casefold())


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
def test_get_workspace_name(url, expected, wk_space, monkeypatch):
    """Testing retrieving workspace details from portal url."""
    del url, wk_space
    _patch_qry_prov(monkeypatch)

    ws_name = MicrosoftSentinel.get_workspace_name(workspace_id=expected.ws_id)
    if ws_name is None:
        check.equal(expected.ws_name.casefold(), "non-existent")
    else:
        check.equal(ws_name.casefold(), expected.ws_name.casefold())


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
def test_get_workspace_id(url, expected, wk_space, monkeypatch):
    """Testing retrieving workspace details from portal url."""
    del url
    _patch_qry_prov(monkeypatch)

    # with only ws name parameter we might get multiple results
    ws_id = MicrosoftSentinel.get_workspace_id(workspace_name=wk_space)
    if expected.ws_id == "unknown":
        check.is_none(ws_id)
    else:
        multi_res_id = _get_ws_results(workspace_name=wk_space)
        check.greater_equal(len(multi_res_id), 1)
        check.equal(ws_id.casefold(), expected.ws_id.casefold())

    ws_id = MicrosoftSentinel.get_workspace_id(
        workspace_name=wk_space, resource_group=expected.res_group
    )
    if expected.ws_id == "unknown":
        check.is_none(ws_id)
    else:
        check.equal(ws_id.casefold(), expected.ws_id.casefold())

    ws_id = MicrosoftSentinel.get_workspace_id(workspace_name=wk_space)


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
def test_get_workspace_settings_by_id(url, expected, wk_space, monkeypatch):
    """Testing retrieving workspace details from portal url."""
    _patch_qry_prov(monkeypatch)

    ws_settings = MicrosoftSentinel.get_workspace_settings(workspace_id=expected.ws_id)
    if wk_space == "non-existent":
        check.is_false(ws_settings)
    else:
        ws_details = next(iter(ws_settings.values()))
        check.equal(ws_details["WorkspaceName"].casefold(), expected.ws_name.casefold())
        check.equal(ws_details["SubscriptionId"].casefold(), expected.sub_id.casefold())
        check.equal(
            ws_details["ResourceGroup"].casefold(), expected.res_group.casefold()
        )
        check.equal(ws_details["WorkspaceId"].casefold(), expected.ws_id.casefold())

    resource_id = MicrosoftSentinel.get_resource_id_from_url(url)
    ws_settings = MicrosoftSentinel.get_workspace_settings(resource_id=resource_id)
    if wk_space == "non-existent":
        check.is_false(ws_settings)
    else:
        ws_details = next(iter(ws_settings.values()))
        check.equal(ws_details["WorkspaceName"].casefold(), expected.ws_name.casefold())
        check.equal(ws_details["SubscriptionId"].casefold(), expected.sub_id.casefold())
        check.equal(
            ws_details["ResourceGroup"].casefold(), expected.res_group.casefold()
        )
        check.equal(ws_details["WorkspaceId"].casefold(), expected.ws_id.casefold())


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
def test_get_workspace_settings_by_name(url, expected, wk_space, monkeypatch):
    """Testing retrieving workspace details from portal url."""
    del url
    _patch_qry_prov(monkeypatch)

    # with only ws name parameter we might get multiple results
    ws_settings = MicrosoftSentinel.get_workspace_settings_by_name(
        workspace_name=wk_space
    )
    if expected.ws_id == "unknown":
        check.is_false(ws_settings)
    else:
        ws_details = next(iter(ws_settings.values()))
        multi_res_id = _get_ws_results(workspace_name=wk_space)
        check.greater_equal(len(multi_res_id), 1)
        check.equal(ws_details["WorkspaceId"].casefold(), expected.ws_id.casefold())

    ws_settings = MicrosoftSentinel.get_workspace_settings_by_name(
        workspace_name=wk_space, subscription_id=expected.sub_id
    )
    if wk_space == "non-existent":
        check.is_false(ws_settings)
    else:
        ws_details = next(iter(ws_settings.values()))
        check.equal(ws_details["WorkspaceName"].casefold(), expected.ws_name.casefold())
        check.equal(ws_details["SubscriptionId"].casefold(), expected.sub_id.casefold())
        check.equal(
            ws_details["ResourceGroup"].casefold(), expected.res_group.casefold()
        )
        check.equal(ws_details["WorkspaceId"].casefold(), expected.ws_id.casefold())


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
def test_get_resource_id(url, expected, wk_space):
    """Test get resource IDs."""
    res_id = MicrosoftSentinel.get_resource_id_from_url(url)

    check.is_not_none(res_id)
    if wk_space != "non-existent":
        expected_id = next(
            iter(
                ws["id"]
                for ws in _WS_RES_GRAPH_DATA
                if ws["workspaceName"].casefold() == wk_space.casefold()
                and ws["subscriptionId"].casefold() == expected.sub_id.casefold()
            )
        )
        check.equal(res_id.casefold(), expected_id.casefold())


def test_get_resource_id_bad():
    """Check format/error handling on resource IDs."""
    res_id = MicrosoftSentinel.get_resource_id_from_url("not a resource ID")
    check.is_none(res_id)

    res_id = MicrosoftSentinel.get_resource_id_from_url(
        "https://foo/#page/subscriptions/not_res_id"
    )
    check.is_none(res_id)

    # Can't find a
    dubious_res_url = (
        "https://foo/#page/subscriptions/999"
        "/resourcegroups/1234"
        "/providers/Microsoft.OperationalInsights"
        "/workspaces/0"
    )
    res_id = MicrosoftSentinel.get_resource_id_from_url(dubious_res_url)
    check.is_not_none(res_id)


@pytest.mark.parametrize(
    "url, expected, wk_space", _TEST_URL_LOOKUP, ids=[i[2] for i in _TEST_URL_LOOKUP]
)
@respx.mock
def test_fail_tenantid_lookup(url, expected, wk_space, monkeypatch):
    """Test when tenant ID lookup fails."""
    login_endpoint = AzureCloudConfig().endpoints.active_directory
    respx.get(re.compile(f"{login_endpoint}.*")).respond(404, json={})

    _patch_qry_prov(monkeypatch)
    ws_details = MicrosoftSentinel.get_workspace_details_from_url(url)
    ws_details = next(iter(ws_details.values()))

    check.equal(ws_details["WorkspaceName"].casefold(), expected.ws_name.casefold())
    check.equal(ws_details["SubscriptionId"].casefold(), expected.sub_id.casefold())
    check.equal(ws_details["ResourceGroup"].casefold(), expected.res_group.casefold())
    check.equal(ws_details["WorkspaceId"].casefold(), expected.ws_id.casefold())
    if wk_space == "cybersecuritysoc":
        check.equal(
            ws_details["TenantId"].casefold(), "4b2462a4-bbee-495a-a0e1-f23ae524cc9c"
        )
    else:
        check.equal(ws_details["TenantId"].casefold(), expected.ten_id.casefold())


def test_param_checks():
    """Test checks for missing params."""
    with pytest.raises(ValueError):
        MicrosoftSentinel.get_workspace_settings()
    with pytest.raises(ValueError):
        MicrosoftSentinel.get_workspace_name()


def _patch_qry_prov(patcher):
    qry_prov = QueryProvider("ResourceGraph")
    setattr(MicrosoftSentinel, "_RES_GRAPH_PROV", qry_prov)
    qry_prov._query_provider._loaded = True
    qry_prov._query_provider._connected = True
    patcher.setattr(qry_prov, "connect", lambda: True)

    resg_queries = getattr(qry_prov, "Sentinel")
    patcher.setattr(
        resg_queries, "get_sentinel_workspace_for_resource_id", _get_ws_results
    )
    patcher.setattr(
        resg_queries, "get_sentinel_workspace_for_workspace_id", _get_ws_results
    )
    patcher.setattr(resg_queries, "list_sentinel_workspaces_for_name", _get_ws_results)
