# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
from typing import Dict, Any
import json
import warnings
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common import pkg_config
from msticpy.context.contextlookup import ContextLookup

from ..unit_test_lib import custom_mp_config, get_test_data_path

_TEST_DATA = get_test_data_path()


# pylint: disable=protected-access, redefined-outer-name
@pytest.fixture
def context_lookup():
    """Return TILookup instance."""
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            return ContextLookup()


_OBS_IPS = [
    "10.0.0.1",
    "192.168.0.1",
]


# This class will mock httpx.Client()
class HTTPResponse:
    """Class for mocked response."""

    def __init__(self, json_data, status_code):
        """Initialize the class."""
        self.json_data = json_data
        self.status_code = status_code

    def json(self):
        """Return Python representation of json data."""
        return self.json_data


class RequestSession:
    """Mock httpx session."""

    def get(self, *args, **kwargs):
        """Return results of httpx.get."""
        if "url" not in kwargs:
            if args:
                kwargs.update({"url": args[0]})
            else:
                return HTTPResponse(None, 404)
        for url_match, resp_data in _PROVIDER_RESPONSES.items():
            if not kwargs["url"].startswith(url_match):
                continue
            if "params" in resp_data and isinstance(resp_data["response"], str):
                str_response = self._format_json_response(resp_data, **kwargs)
                check.is_not_none(json.loads(str_response))
                return HTTPResponse(json.loads(str_response), 200)
            elif isinstance(resp_data["response"], dict):
                return HTTPResponse(resp_data["response"], 200)
            elif callable(resp_data["response"]):
                check.is_not_none(
                    resp_data["response"](**(resp_data["params"]), **kwargs)
                )
                return HTTPResponse(
                    resp_data["response"](**(resp_data["params"]), **kwargs), 200
                )
        return HTTPResponse(None, 404)

    @staticmethod
    def _get_observable_from_query(resp_data, **kwargs):
        """Extract the part of the query containing the observable."""
        path = resp_data.get("obs_param")
        if path is None:
            return kwargs["url"]
        kwargs_item = kwargs
        for elem in path.split("."):
            kwargs_item = kwargs_item.get(elem)
        return kwargs_item

    @staticmethod
    def _format_json_response(resp_data, **kwargs):
        """Replace dynamic params in the response string."""
        format_params = resp_data["params"].copy()
        if "query" in format_params:
            format_params["query"] = kwargs["params"]["query"]
        if "url" in format_params:
            format_params["url"] = kwargs["url"]
        response_str = resp_data["response"]
        for token, replace in format_params.items():
            if f"{{[[{token}]]}}" in response_str:
                response_str = response_str.replace(f"{{[[{token}]]}}", str(replace))
        return response_str


_TEST_OBS = {
    "10.0.0.1": "ipv4",
    "User01": "user",
    "host": "hostname",
    "user@domain.com": "email",
}

_CONTEXT_PROVIDER_TESTS = ["ServiceNow"]


@pytest.mark.parametrize("provider_name", _CONTEXT_PROVIDER_TESTS)
def test_context_provider(context_lookup, provider_name):
    """Test individual providers."""
    context_provider = context_lookup.loaded_providers[provider_name]
    saved_session = context_provider._httpx_client
    context_provider._httpx_client = RequestSession()

    # Lookup multiple Observables
    for observable, _ in _TEST_OBS.items():
        result = context_lookup.lookup_observable(
            observable=observable,
            providers=[provider_name],
            show_not_supported=True,
        )
        verify_result(result)

    # Check if lookup works with observable parameter
    for observable, observable_type in _TEST_OBS.items():
        result = context_lookup.lookup_observable(
            observable=observable,
            observable_type=observable_type,
            providers=[provider_name],
            show_not_supported=True,
        )
        verify_result(result)

    results_df = context_lookup.lookup_observables(
        data=_OBS_IPS, providers=[provider_name]
    )
    check.equal(2, len(results_df))
    check.equal(1, len(results_df[results_df["Result"]]))

    # test the sync version of the API
    results_df = context_lookup.lookup_observables_sync(
        data=_OBS_IPS, providers=[provider_name]
    )
    check.equal(2, len(results_df))
    check.equal(1, len(results_df[results_df["Result"]]))

    context_provider._httpx_client = saved_session


# pylint: disable=pointless-statement
def verify_result(result):
    """Verify return results."""
    check.is_not_none(result)
    check.is_instance(result, pd.DataFrame)
    check.is_false(result.empty)
    check.equal(1, len(result))
    for lu_result in result.to_dict(orient="records"):
        check.is_in(
            lu_result["Provider"],
            ["ServiceNow"],
        )
        check.is_not_none(lu_result["Observable"])
        check.is_not_none(lu_result["ObservableType"])
        if lu_result["Result"]:
            check.is_not_none(lu_result["Details"])
            check.is_not_none(lu_result["RawResult"])
            check.is_not_none(lu_result["Reference"])
            # exercise summary functions of Lookup class


def test_json_responses():
    """Tests any json string test responses for correct formatting."""
    for url, resp_data in _PROVIDER_RESPONSES.items():
        print(url, resp_data.keys())

        if isinstance(resp_data["response"], str):
            kwargs = {
                "url": "http://foo",
                "params": {"one": "two", "query": "query_str"},
            }

            repl_str = RequestSession._format_json_response(resp_data, **kwargs)
            print(url)
            print(repl_str)
            json.loads(repl_str)


_PROVIDER_RESPONSES: Dict[str, Any] = {
    "https://test.service-now.com/api/now/table/sys_user": {
        "response": {
            "result": [
                {
                    "user_name": "user@domain.com",
                    "active": "true",
                    "business_criticality": "Critical",
                    "company": "ACME",
                    "department": "Department 42",
                    "email": "user@domain.com",
                    "failed_attempts": "0",
                    "first_name": "First",
                    "introduction": "Mr.",
                    "last_name": "LAST",
                    "manager": "Boss",
                    "middle_name": "",
                    "name": "First LAST",
                    "phone": "",
                    "preferred_language": "English",
                    "title": "Job Title",
                    "vip": "false",
                }
            ]
        }
    },
    "https://test.service-now.com/api/now/table/cmdb_ci_computer": {
        "response": {
            "result": [
                {
                    "assigned_to": "User01",
                    "assignment_group": "Group 01",
                    "busines_criticality": None,
                    "comments": "",
                    "company": "ACME",
                    "created_on_on": "2022-11-01 00:00:0",
                    "fqdn": "host.fqdn",
                    "hostname": "host",
                    "install_date": "2022-10-31 00:00:00",
                    "install_status": "Active",
                    "internet_facing": "true",
                    "ip_address": "10.0.0.1",
                    "last_discovered": "2022-11-02 00:00:0",
                    "location": "World",
                    "subcategory": "Computer",
                    "support_group": "Group 02",
                    "sys_class_name": "Computer",
                    "sys_created_on": "2022-10-30 00:00:00",
                    "sys_id": "1234",
                    "sys_tags": "",
                    "sys_updated_on": "2022-11-03 00:00:00",
                }
            ]
        }
    },
}
