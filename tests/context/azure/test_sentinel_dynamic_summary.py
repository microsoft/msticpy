# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Sentinel Dynamic Summaries unit tests."""
import json
import re
import uuid
from copy import deepcopy
from datetime import datetime, timezone
from unittest.mock import Mock, patch

import pandas as pd
import pytest
import pytest_check as check
import respx
import yaml

from msticpy.common.exceptions import MsticpyAzureConnectionError
from msticpy.common.wsconfig import WorkspaceConfig
from msticpy.context.azure import MicrosoftSentinel
from msticpy.context.azure.sentinel_dynamic_summary import SentinelQueryProvider
from msticpy.context.azure.sentinel_dynamic_summary_types import (
    _API_TO_CLS_MAP,
    DynamicSummary,
)

from ...unit_test_lib import custom_mp_config, get_test_data_path

# pylint: disable=redefined-outer-name, protected-access

_DYN_SUMMARY_RESP = {
    "properties": {
        "summaryId": "08e0f35a-ec55-43a6-9dfc-3e563b329166",
        "summaryName": "test",
        "summaryDescription": "Test description",
        "sourceInfo": {"TI": "TI Source"},
        "rawContent": """[
            {
                "summaryItemId": "5b1222bb-68e1-41ce-8688-00df680f43df",
                "relationName": "TI",
                "searchKey": "http://38.75.137.9:9088/static/encrypt.min.js",
                "tactics": [
                "exploitation",
                "discovery"
                ],
                "eventTimeUTC": "2022-12-13T19:47:13.507395",
                "observableType": "url",
                "observableValue": "http://38.75.137.9:9088/static/encrypt.min.js",
                "packedContent": {
                "index": "OTX",
                "Ioc": "http://38.75.137.9:9088/static/encrypt.min.js",
                "IocType": "url",
                "QuerySubtype": null,
                "Provider": "OTX",
                "Result": true,
                "Severity": 2,
                "Details": {
                    "pulse_count": 3,
                    "names": [
                    "Underminer EK",
                    "Exploit kits",
                    "Exploit kits: summer 2019 review"
                    ],
                    "tags": [
                    [],
                    [
                        "Exploit Kit",
                        "EK"
                    ],
                    [
                        "Exploit Kit",
                        "EK"
                    ]
                    ],
                    "references": [
                    [
                        "https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/"
                    ],
                    [
                        "https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/"
                    ],
                    [
                        "https://blog.malwarebytes.com/threat-analysis/2019/07/exploit-kits-summer-2019-review/"
                    ]
                    ]
                },
                "TimeGenerated": "2022-12-13T19:47:13.507395"
                }
            },
            {
                "summaryItemId": "aa7943cb-db40-4fb9-802c-78c4e95e271d",
                "relationName": "TI",
                "searchKey": "http://38.75.137.9:9088/static/encrypt.min.js",
                "tactics": [
                "exploitation",
                "discovery"
                ],
                "eventTimeUTC": "2022-12-13T19:47:13.507395",
                "observableType": "url",
                "observableValue": "http://38.75.137.9:9088/static/encrypt.min.js",
                "packedContent": {
                "index": "VirusTotal",
                "Ioc": "http://38.75.137.9:9088/static/encrypt.min.js",
                "IocType": "url",
                "QuerySubtype": null,
                "Provider": "VirusTotal",
                "Result": false,
                "Severity": 0,
                "Details": "Request forbidden. Allowed query rate may have been exceeded.",
                "TimeGenerated": "2022-12-13T19:47:13.507395"
                }
            },
            {
                "summaryItemId": "12e2bde6-bebc-4f0e-90fa-c669b47a6740",
                "relationName": "TI",
                "searchKey": "http://38.75.137.9:9088/static/encrypt.min.js",
                "tactics": [
                "exploitation",
                "discovery"
                ],
                "eventTimeUTC": "2022-12-13T19:47:13.507395",
                "observableType": "url",
                "observableValue": "http://38.75.137.9:9088/static/encrypt.min.js",
                "packedContent": {
                "index": "XForce",
                "Ioc": "http://38.75.137.9:9088/static/encrypt.min.js",
                "IocType": "url",
                "QuerySubtype": null,
                "Provider": "XForce",
                "Result": false,
                "Severity": 0,
                "Details": "Not found.",
                "TimeGenerated": "2022-12-13T19:47:13.507395"
                }
            },
            {
                "summaryItemId": "15de4147-6acb-4dd9-9a88-272435a4df30",
                "relationName": "TI",
                "searchKey": "http://38.75.137.9:9088/static/encrypt.min.js",
                "tactics": [
                "exploitation",
                "discovery"
                ],
                "eventTimeUTC": "2022-12-13T19:47:13.507395",
                "observableType": "url",
                "observableValue": "http://38.75.137.9:9088/static/encrypt.min.js",
                "packedContent": {
                "index": "AzSTI",
                "Ioc": "http://38.75.137.9:9088/static/encrypt.min.js",
                "IocType": "url",
                "QuerySubtype": null,
                "Provider": "AzSTI",
                "Result": false,
                "Severity": 0,
                "Details": "Not found.",
                "TimeGenerated": "2022-12-13T19:47:13.507395"
                }
            },
            {
                "summaryItemId": "ebbae5e7-f9ac-449c-a11f-f51b0149c114",
                "relationName": "TI",
                "searchKey": "http://38.75.137.9:9088/static/encrypt.min.js",
                "tactics": [
                "exploitation",
                "discovery"
                ],
                "eventTimeUTC": "2022-12-13T19:47:13.507395",
                "observableType": "url",
                "observableValue": "http://38.75.137.9:9088/static/encrypt.min.js",
                "packedContent": {
                "index": "OPR",
                "Ioc": "http://38.75.137.9:9088/static/encrypt.min.js",
                "IocType": "url",
                "QuerySubtype": null,
                "Provider": "OPR",
                "Result": false,
                "Severity": 0,
                "Details": "IoC type url not supported.",
                "TimeGenerated": "2022-12-13T19:47:13.507395"
                }
            },
            {
                "summaryItemId": "e20d85f3-20cf-44b3-9e9c-c857866f7d31",
                "relationName": "TI",
                "searchKey": "http://38.75.137.9:9088/static/encrypt.min.js",
                "tactics": [
                "exploitation",
                "discovery"
                ],
                "eventTimeUTC": "2022-12-13T19:47:13.507395",
                "observableType": "url",
                "observableValue": "http://38.75.137.9:9088/static/encrypt.min.js",
                "packedContent": {
                "index": "Tor",
                "Ioc": "http://38.75.137.9:9088/static/encrypt.min.js",
                "IocType": "url",
                "QuerySubtype": null,
                "Provider": "Tor",
                "Result": true,
                "Severity": 0,
                "Details": "IoC type url not supported.",
                "TimeGenerated": "2022-12-13T19:47:13.507395"
                }
            }
        ]""",
    }
}


@pytest.fixture(scope="module")
def ti_data():
    """Return TI data dataframe."""
    df_path = get_test_data_path().joinpath("ti_results.df.pkl")
    ti_df = pd.read_pickle(df_path)

    ti_slim = ti_df[
        ["Ioc", "IocType", "QuerySubtype", "Provider", "Result", "Severity", "Details"]
    ].reset_index()
    ti_slim["TimeGenerated"] = datetime.now(tz=timezone.utc)
    return ti_slim


@pytest.fixture(scope="module")
def list_responses():
    """Return multiple response list from test data."""
    resp_list = []
    for _ in range(5):
        resp = _DYN_SUMMARY_RESP["properties"].copy()
        resp["summary_id"] = str(uuid.uuid4())
        resp_list.append(resp)
    return {"value": resp_list}


def _get_test_ws_settings():
    """Get test workspace settings from config file."""
    test_config = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    settings = yaml.safe_load(test_config.read_text(encoding="utf-8"))
    az_ws_settings = settings.get("AzureSentinel", {}).get("Workspaces", {})
    return next(
        iter((key, val) for key, val in az_ws_settings.items() if key != "Default")
    )


def _set_default_workspace(self, sub_id, workspace=None):
    """Mock set_default_workspace for MSSentinel."""
    del sub_id, workspace
    ws_key, settings = _get_test_ws_settings()
    self._default_workspace = ws_key
    self.workspace_config = WorkspaceConfig.from_settings(settings)


@pytest.fixture
@patch(f"{MicrosoftSentinel.__module__}.get_token")
@patch(f"{MicrosoftSentinel.__module__}.AzureData.connect")
def sentinel_loader(mock_creds, get_token, monkeypatch):
    """Generate MicrosoftSentinel for testing."""
    monkeypatch.setattr(
        MicrosoftSentinel, "set_default_workspace", _set_default_workspace
    )
    mock_creds.return_value = None
    get_token.return_value = "fd09863b-5cec-4833-ab9c-330ad07b0c1a"
    mock_creds.return_value = None
    ws_key, settings = _get_test_ws_settings()
    with custom_mp_config(
        get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    ):
        sent = MicrosoftSentinel(
            sub_id=settings.get(
                "SubscriptionId", "fd09863b-5cec-4833-ab9c-330ad07b0c1a"
            ),
            res_grp=settings.get("ResourceGroup", "RG"),
            ws_name=settings.get("WorkspaceName", "Default"),
        )
        sent._default_workspace = ws_key
        sent.connect(workspace=ws_key)
        sent.connected = True
        sent.token = "fd09863b-5cec-4833-ab9c-330ad07b0c1a"  # nosec
    return sent


@respx.mock
def test_sent_list_dynamic_summary(sentinel_loader, list_responses):
    """Test Sentinel dynamic_summary feature."""
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=list_responses
    )
    dyn_summaries = sentinel_loader.list_dynamic_summaries()
    check.is_instance(dyn_summaries, pd.DataFrame)
    check.equal(len(dyn_summaries), 5)
    check.equal(dyn_summaries["summaryName"].iloc[0], "test")


@respx.mock
def test_sent_get_dynamic_summary(sentinel_loader):
    """Test Sentinel dynamic_summary feature."""
    get_resp = deepcopy(_DYN_SUMMARY_RESP)
    del get_resp["properties"]["rawContent"]
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json=get_resp
    )
    dyn_summary = sentinel_loader.get_dynamic_summary(summary_id="test")
    check.is_instance(dyn_summary, DynamicSummary)
    check.equal(len(dyn_summary.summary_items), 0)
    check.equal(dyn_summary.summary_name, "test")


@respx.mock
def test_sent_dynamic_summary_create_params(sentinel_loader, ti_data):
    """Test Sentinel dynamic_summary creation."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        201, json={"name": "test_id"}
    )
    sum_id = sentinel_loader.create_dynamic_summary(
        name="Test Summary",
        description="This is a test summary",
        data=ti_data,
        tactics=["discovery", "exploitation"],
        techniques=["T1000"],
        search_key="TI stuff",
        source_info={"Source": "unit_test"},
    )
    check.equal(sum_id, "test_id")


@respx.mock
def test_sent_dynamic_summary_create_df(sentinel_loader):
    """Test Sentinel dynamic_summary creation."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        201, json={"name": "test_id"}
    )
    dyn_summary = DynamicSummary.from_json(json.dumps(_DYN_SUMMARY_RESP))
    sum_id = sentinel_loader.create_dynamic_summary(summary=dyn_summary)
    check.equal(sum_id, "test_id")


@respx.mock
def test_sent_dynamic_summary_delete(sentinel_loader):
    """Test Sentinel dynamic_summary deletion."""
    respx.delete(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json={"name": "test_id"}
    )
    sum_id = sentinel_loader.delete_dynamic_summary(
        "a55463ed-dce0-4ba4-83ca-6f6d0e5d5acf"
    )
    check.equal(sum_id, "test_id")


@respx.mock
def test_sent_dynamic_summary_update_param(sentinel_loader, ti_data):
    """Test Sentinel dynamic_summary creation."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json={"name": "test_id"}
    )
    sum_id = sentinel_loader.update_dynamic_summary(
        summary_id="test_id",
        name="Test Summary",
        description="This is a test summary",
        data=ti_data,
        tactics=["discovery", "exploitation"],
        techniques=["T1000"],
        search_key="TI stuff",
        source_info={"Source": "unit_test"},
    )
    check.equal(sum_id, "test_id")


@respx.mock
def test_sent_dynamic_summary_update_df(sentinel_loader):
    """Test Sentinel dynamic_summary creation."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        200, json={"name": "test_id"}
    )
    dyn_summary = DynamicSummary.from_json(json.dumps(_DYN_SUMMARY_RESP))
    sum_id = sentinel_loader.update_dynamic_summary(summary=dyn_summary)
    check.equal(sum_id, "test_id")


_TEST_SUMMARY = DynamicSummary(summary_name="test summary")

_CONNECT_FAILURES = [
    ("list_dynamic_summaries", {}, Exception),
    ("get_dynamic_summary", {"summary_id": "12345"}, MsticpyAzureConnectionError),
    ("create_dynamic_summary", {"name": "12345"}, MsticpyAzureConnectionError),
    ("delete_dynamic_summary", {"summary_id": "12345"}, MsticpyAzureConnectionError),
    (
        "update_dynamic_summary",
        {"summary_id": "12345", "summary": _TEST_SUMMARY},
        MsticpyAzureConnectionError,
    ),
]


@respx.mock
@pytest.mark.parametrize("func, params, expected", _CONNECT_FAILURES)
def test_connection_failures(func, params, expected, sentinel_loader):
    """Test connection failures for different APIs."""
    respx.put(re.compile(r"https://management\.azure\.com/.*")).respond(
        400, json={"name": "test_id"}
    )
    respx.get(re.compile(r"https://management\.azure\.com/.*")).respond(
        400, json={"name": "test_id"}
    )
    respx.delete(re.compile(r"https://management\.azure\.com/.*")).respond(
        400, json={"name": "test_id"}
    )
    meth = getattr(sentinel_loader, func)
    with pytest.raises(expected):
        meth(**params)


def test_new_dynamic_summary(sentinel_loader):
    """Test dyn summary class factory."""
    new_ds = sentinel_loader.new_dynamic_summary(
        summary_id="test_id",
        name="Test Summary",
        description="This is a test summary",
        data=ti_data,
        tactics=["discovery", "exploitation"],
        techniques=["T1000"],
        search_key="TI stuff",
        source_info={"Source": "unit_test"},
    )
    check.is_instance(new_ds, DynamicSummary)
    check.equal(new_ds.summary_id, "test_id")


@patch(SentinelQueryProvider.__module__ + ".QueryProvider")
def test_sent_get_dynamic_summary_plus_items(qry_prov, sentinel_loader):
    """Test Sentinel dynamic_summary feature."""
    qry_prov_instance = Mock()
    qry_prov.return_value = qry_prov_instance
    qry_prov_instance.connect.return_value = None
    qry_prov_instance.MSSentinel = Mock()

    dyn_summary_df = pd.read_json(
        get_test_data_path().joinpath("dynamic_summary_df.json")
    )
    qry_prov_instance.MSSentinel.get_dynamic_summary_by_id.return_value = dyn_summary_df

    dyn_summary = sentinel_loader.get_dynamic_summary(
        summary_id="test", summary_items=True
    )

    check.equal(dyn_summary.summary_name, "test2")
    summary_items = dyn_summary_df[dyn_summary_df["SummaryDataType"] == "SummaryItem"]
    check.equal(len(dyn_summary.summary_items), len(summary_items))


def test_dynamic_summary_class(ti_data):
    """Test DynamicSummary class."""
    ds = DynamicSummary(
        summary_name="test",
        summary_description="Test description",
        source_info="TI",
    )
    # pylint: disable=unexpected-keyword-arg
    ds.add_summary_items(
        data=ti_data,
        summary_fields={
            "observable_type": "IocType",
            "observable_value": "Ioc",
            "search_key": "Ioc",
        },
        relation_name="TI",
        tactics=["exploitation", "discovery"],
    )
    check.equal(len(ds.summary_items), len(ti_data))
    check.equal(ds.summary_name, "test")

    check.is_instance(ds.to_df(), pd.DataFrame)
    check.is_true(ds.to_df().compare(ti_data).empty)

    json_txt = ds.to_json()
    ds2 = DynamicSummary.from_json(json_txt)
    check.equal(ds.summary_id, ds2.summary_id)
    check.equal(ds.summary_name, ds2.summary_name)
    for idx, item in enumerate(ds.summary_items):
        check.equal(item.summary_item_id, ds2.summary_items[idx].summary_item_id)
        check.equal(item.search_key, ds2.summary_items[idx].search_key)
        check.equal(item.observable_type, ds2.summary_items[idx].observable_type)
        check.equal(item.observable_value, ds2.summary_items[idx].observable_value)
        check.equal(
            len(item.packed_content), len(ds2.summary_items[idx].packed_content)
        )


def test_df_to_dynamic_summaries():
    """Test conversion of DFs to dynamic summaries."""
    dyn_summary_df = pd.read_json(
        get_test_data_path().joinpath("dynamic_summary_df.json")
    )

    ds_list = DynamicSummary.df_to_dynamic_summaries(dyn_summary_df)
    check.equal(len(ds_list), 1)
    check.is_instance(ds_list[0], DynamicSummary)
    check.equal(ds_list[0].summary_name, "test2")

    d_summary = DynamicSummary.df_to_dynamic_summary(dyn_summary_df)
    check.is_instance(d_summary, DynamicSummary)
    check.equal(d_summary.summary_name, "test2")


def test_add_summary_items_dict():
    """Test adding summary items as dictionaries."""
    json_items = _DYN_SUMMARY_RESP["properties"]["rawContent"]
    items_list = json.loads(json_items)
    mapped_items = [
        {_API_TO_CLS_MAP[name]: value for name, value in item.items()}
        for item in items_list
    ]
    dyn_summary = DynamicSummary(summary_name="add_items_dict")
    dyn_summary.add_summary_items(data=mapped_items)

    check.equal(len(dyn_summary.summary_items), len(mapped_items))

    dyn_summary.append_summary_items(mapped_items)
    check.equal(len(dyn_summary.summary_items), 2 * len(mapped_items))
