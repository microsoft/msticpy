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
from __future__ import annotations

import datetime as dt
from dataclasses import dataclass
from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .http_context_provider import HttpContextProvider

__version__ = VERSION
__author__ = "Florian Bracq"


_DEF_HEADERS: dict[str, str] = {
    "Content-Type": "application/json",
    "Accept": "application/json",
    "Accept-Charset": "UTF-8",
    "cache-control": "no-cache",
}


# pylint: disable=too-few-public-methods
@dataclass
class _ServiceNowParams(APILookupParams):
    # override LookupParams to set common defaults
    def __attrs_post_init__(self: Self) -> None:
        self.auth_str = ["{ApiID}", "{AuthKey}"]
        self.auth_type = "HTTPBasic"


@export
class ServiceNow(HttpContextProvider):
    """ServiceNow Lookup."""

    _BASE_URL: ClassVar[str] = "https://{Instance}.service-now.com/api/now/table"

    _SERVICE_NOW_PARAMS: ClassVar[dict[str, Any]] = {
        "sysparm_display_value": True,
        "sysparm_exclude_reference_link": True,
        "sysparm_limit": 10,
    }

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
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

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["ApiID", "AuthKey", "Instance"]

    def parse_results(self: Self, response: dict[str, Any]) -> tuple[bool, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        tuple[bool, Any]
            bool = positive or negative hit
            Object with match details

        """
        if self._failed_response(response) or not isinstance(
            response.get("RawResult", {}),
            dict,
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

        results: list[dict[str, Any]] = response["RawResult"]["result"]

        result_dict: list[dict[str, Any]] = [
            {
                "permalink": f"{response['Reference']}/{result.get('sys_id')}",
                "sys_class_name": result.get("sys_class_name", ""),
                "sys_id": result.get("sys_id", ""),
                "sys_tags": result.get("sys_tags", ""),
                "company": result.get("company", None),
                "location": result.get("location", ""),
                "sys_updated_on": (
                    dt.datetime.strptime(
                        result["sys_updated_on"],
                        "%Y-%m-%d %H:%M:%S",
                    ).replace(tzinfo=dt.timezone.utc)
                    if result.get("sys_updated_on")
                    else ""
                ),
                "sys_created_on": (
                    dt.datetime.strptime(
                        result["sys_created_on"],
                        "%Y-%m-%d %H:%M:%S",
                    ).replace(tzinfo=dt.timezone.utc)
                    if result.get("sys_created_on")
                    else ""
                ),
                **(
                    getattr(self, f"_parse_result_{response['ObservableType']}")(result)
                ),
            }
            for result in results
        ]

        return True, result_dict

    def _parse_result_ipv4(self: Self, result: dict[str, Any]) -> dict[str, Any]:
        """
        Return a dictionary of relevant fields for a "Computer" Service Now object.

        Parameters
        ----------
        result : dict[str, Any]
            Entry returned by Service Now

        Returns
        -------
        dict[str, Any]

            Subset of releavnt keys parsed

        """
        return {
            "assignment_group": result.get("assignment_group"),
            "busines_criticality": result.get("busines_criticality", ""),
            "category": result.get("category"),
            "comments": result.get("comments"),
            "fqdn": result.get("fqdn"),
            "hostname": result.get("hostname"),
            "install_status": result.get("install_status", ""),
            "internet_facing": result.get("internet_facing", False),
            "ip_address": result.get("ip_address", "").split(","),
            "subcategory": result.get("subcategory"),
            "support_group": result.get("support_group"),
            "sys_class_name": result.get("sys_class_name", ""),
            "last_discovered": (
                dt.datetime.strptime(
                    result["last_discovered"],
                    "%Y-%m-%d %H:%M:%S",
                ).replace(tzinfo=dt.timezone.utc)
                if result.get("last_discovered")
                else ""
            ),
            "install_date": (
                dt.datetime.strptime(
                    result["install_date"],
                    "%Y-%m-%d %H:%M:%S",
                ).replace(tzinfo=dt.timezone.utc)
                if result.get("install_date")
                else ""
            ),
            "created_on": (
                dt.datetime.strptime(result["created_on"], "%Y-%m-%d %H:%M:%S").replace(
                    tzinfo=dt.timezone.utc,
                )
                if result.get("created_on")
                else ""
            ),
        }

    def _parse_result_user(self: Self, result: dict[str, Any]) -> dict[str, Any]:
        """
        Return a dictionary of relevant fields for a "Computer" Service Now object.

        Parameters
        ----------
        result : dict[str, Any]
            Entry returned by Service Now

        Returns
        -------
        dict[str, Any]

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
            "last_login": (
                dt.datetime.strptime(result["last_login"], "%Y-%m-%d").replace(
                    tzinfo=dt.timezone.utc,
                )
                if result.get("last_login")
                else ""
            ),
            "last_login_time": (
                dt.datetime.strptime(
                    result["last_login_time"],
                    "%Y-%m-%d %H:%M:%S",
                ).replace(tzinfo=dt.timezone.utc)
                if result.get("last_login_time")
                else ""
            ),
        }

    # Define aliases
    _parse_result_ipv6 = _parse_result_ipv4
    _parse_result_dns = _parse_result_ipv4
    _parse_result_hostname = _parse_result_ipv4
    _parse_result_email = _parse_result_user
