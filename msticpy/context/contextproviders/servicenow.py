# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Service Now Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
import datetime as dt
from typing import Any, Dict, Tuple

import attr

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .http_context_provider import HttpContextProvider

__version__ = VERSION
__author__ = "Florian Bracq"


_DEF_HEADERS = {
    "Content-Type": "application/json",
    "Accept": "application/json",
    "Accept-Charset": "UTF-8",
    "cache-control": "no-cache",
}


# pylint: disable=too-few-public-methods
@attr.s
class _ServiceNowParams(APILookupParams):
    # override LookupParams to set common defaults
    def __attrs_post_init__(self):
        self.auth_str = ["{API_ID}", "{API_KEY}"]
        self.auth_type = "HTTPBasic"


@export
class ServiceNow(HttpContextProvider):
    """ServiceNow Lookup."""

    _BASE_URL = "https://{INSTANCE}.service-now.com/api/now/table"

    _SERVICE_NOW_PARAMS = {
        "sysparm_display_value": True,
        "sysparm_exclude_reference_link": True,
        "sysparm_limit": 10,
    }

    _QUERIES = {
        "ipv4": _ServiceNowParams(
            path="/cmdb_ci_computer",
            params={
                **_SERVICE_NOW_PARAMS,
                "sysparm_query": "ip_addressLIKE{observable}",
            },
            headers=_DEF_HEADERS,
        ),
        "ipv6": _ServiceNowParams(
            path="/cmdb_ci_computer",
            params={
                **_SERVICE_NOW_PARAMS,
                "sysparm_query": "ip_addressLIKE{observable}",
            },
            headers=_DEF_HEADERS,
        ),
        "hostname": _ServiceNowParams(
            path="/cmdb_ci_computer",
            params={**_SERVICE_NOW_PARAMS, "sysparm_query": "nameLIKE{observable}"},
            headers=_DEF_HEADERS,
        ),
        "email": _ServiceNowParams(
            path="/sys_user",
            params={**_SERVICE_NOW_PARAMS, "sysparm_query": "email={observable}"},
            headers=_DEF_HEADERS,
        ),
        "user": _ServiceNowParams(
            path="/sys_user",
            params={**_SERVICE_NOW_PARAMS, "sysparm_query": "nameLIKE{observable}"},
            headers=_DEF_HEADERS,
        ),
    }

    _REQUIRED_PARAMS = ["API_ID", "API_KEY", "INSTANCE"]

    def parse_results(self, response: Dict[str, Any]) -> Tuple[bool, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        Tuple[bool, Any]
            bool = positive or negative hit
            Object with match details

        """
        if self._failed_response(response) or not isinstance(
            response.get("RawResult", {}), dict
        ):
            return False, "Not found."

        if response["ObservableType"] in ["ipv4", "ipv6"]:
            # As Service Now IP field is a CSV-like string and not a list
            # the IP returned might not be an exact match.
            # Therefore, we must validate before returning it
            response["RawResult"]["result"] = [
                result
                for result in response["RawResult"]["result"]
                if response["Observable"]
                in (result.get("ip_address", "").replace(" ", "").split(","))
            ]
            if not response["RawResult"]["result"]:
                return False, "Not found."

        results = response["RawResult"]["result"]

        result_dict = [
            {
                "permalink": f"{response['Reference']}/{result.get('sys_id')}",
                "sys_class_name": result.get("sys_class_name", ""),
                "sys_id": result.get("sys_id", ""),
                "sys_tags": result.get("sys_tags", ""),
                "company": result.get("company", None),
                "location": result.get("location", ""),
                "sys_updated_on": dt.datetime.strptime(
                    result["sys_updated_on"], "%Y-%m-%d %H:%M:%S"
                )
                if "sys_updated_on" in result and result["sys_updated_on"]
                else "",
                "sys_created_on": dt.datetime.strptime(
                    result["sys_created_on"], "%Y-%m-%d %H:%M:%S"
                )
                if "sys_created_on" in result and result["sys_created_on"]
                else "",
                **(
                    getattr(self, f"_parse_result_{response['ObservableType']}")(result)
                ),
            }
            for result in results
        ]

        return True, result_dict

    def _parse_result_ipv4(self, result: Dict[str, Any]) -> Dict[str, Any]:
        """
        Return a dictionary of relevant fields for a "Computer" Service Now object.

        Parameters
        ----------
        result : Dict[str, Any]
            Entry returned by Service Now

        Returns
        -------
        Dict[str, Any]

            Subset of releavnt keys parsed

        """
        return {
            "assignment_group": result.get("assignment_group", None),
            "busines_criticality": result.get("busines_criticality", ""),
            "category": result.get("category", None),
            "comments": result.get("comments", None),
            "fqdn": result.get("fqdn", None),
            "hostname": result.get("hostname", None),
            "install_status": result.get("install_status", ""),
            "internet_facing": result.get("internet_facing", False),
            "ip_address": result.get("ip_address", "").split(","),
            "subcategory": result.get("subcategory", None),
            "support_group": result.get("support_group", None),
            "sys_class_name": result.get("sys_class_name", ""),
            "last_discovered": dt.datetime.strptime(
                result["last_discovered"], "%Y-%m-%d %H:%M:%S"
            )
            if "last_discovered" in result and result["last_discovered"]
            else "",
            "install_date": dt.datetime.strptime(
                result["install_date"], "%Y-%m-%d %H:%M:%S"
            )
            if "install_date" in result and result["install_date"]
            else "",
            "created_on": dt.datetime.strptime(
                result["created_on"], "%Y-%m-%d %H:%M:%S"
            )
            if "created_on" in result and result["created_on"]
            else "",
        }

    def _parse_result_user(self, result: Dict[str, Any]) -> Dict[str, Any]:
        """
        Return a dictionary of relevant fields for a "Computer" Service Now object.

        Parameters
        ----------
        result : Dict[str, Any]
            Entry returned by Service Now

        Returns
        -------
        Dict[str, Any]

            Subset of releavnt keys parsed

        """
        return {
            "account": result.get("employee_number", ""),
            "active": result.get("active", "false"),
            "business_criticality": result.get("business_criticality", ""),
            "department": result.get("department", ""),
            "email": result.get("email", ""),
            "failed_attempts": result.get("failed_attempts", 0),
            "first_name": result.get("first_name", ""),
            "introduction": result.get("introduction", ""),
            "last_name": result.get("last_name", ""),
            "manager": result.get("manager", ""),
            "middle_name": result.get("middle_name", ""),
            "name": result.get("name", ""),
            "phone": result.get("phone", "false"),
            "preferred_language": result.get("preferred_language", ""),
            "title": result.get("title", ""),
            "user_name": result.get("user_name", ""),
            "vip": result.get("vip", "false"),
            "last_login": dt.datetime.strptime(result["last_login"], "%Y-%m-%d")
            if "last_login" in result and result["last_login"]
            else "",
            "last_login_time": dt.datetime.strptime(
                result["last_login_time"], "%Y-%m-%d %H:%M:%S"
            )
            if "last_login_time" in result and result["last_login_time"]
            else "",
        }

    # Define aliases
    _parse_result_ipv6 = _parse_result_ipv4
    _parse_result_dns = _parse_result_ipv4
    _parse_result_hostname = _parse_result_ipv4
    _parse_result_email = _parse_result_user
