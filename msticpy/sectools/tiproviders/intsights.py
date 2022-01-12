# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Intsights Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.
"""
import datetime as dt
from typing import Any, Tuple

import attr
from .ti_provider_base import LookupResult, TISeverity
from .http_base import HttpProvider, IoCLookupParams
from ...common.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Florian Bracq"


_DEF_HEADERS = {"Content-Type": "application/json", "Accept": "application/json"}


# pylint: disable=too-few-public-methods
@attr.s
class _IntSightsParams(IoCLookupParams):
    # override IoCLookupParams to set common defaults
    def __attrs_post_init__(self):
        self.auth_str = ["{API_ID}", "{API_KEY}"]
        self.auth_type = "HTTPBasic"


@export
class IntSights(HttpProvider):
    """IntSights Lookup."""

    _BASE_URL = "https://api.intsights.com"

    _IOC_QUERIES = {
        "ipv4": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "ipv6": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "dns": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "url": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "md5_hash": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "sha1_hash": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "sha256_hash": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "email": _IntSightsParams(
            path="/public/v2/iocs/ioc-by-value",
            params={"iocValue": "{observable}"},
            headers=_DEF_HEADERS,
        ),
    }

    _REQUIRED_PARAMS = ["API_ID", "API_KEY"]

    def parse_results(self, response: LookupResult) -> Tuple[bool, TISeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : LookupResult
            The returned data response

        Returns
        -------
        Tuple[bool, TISeverity, Any]
            bool = positive or negative hit
            TISeverity = enumeration of severity
            Object with match details

        """
        if self._failed_response(response) or not isinstance(response.raw_result, dict):
            return False, TISeverity.information, "Not found."

        if response.raw_result["Whitelist"] == "True":
            return False, TISeverity.information, "Whitelisted."

        sev = response.raw_result["Severity"]
        result_dict = {
            "threat_actors": response.raw_result["RelatedThreatActors"],
            "geolocation": response.raw_result.get("Geolocation", ""),
            "response_code": response.status,
            "tags": response.raw_result["Tags"] + response.raw_result["SystemTags"],
            "malware": response.raw_result["RelatedMalware"],
            "campaigns": response.raw_result["RelatedCampaigns"],
            "sources": response.raw_result["Sources"],
            "score": response.raw_result["Score"],
            "first_seen": dt.datetime.strptime(
                response.raw_result["FirstSeen"], "%Y-%m-%dT%H:%M:%S.%fZ"
            ),
            "last_seen": dt.datetime.strptime(
                response.raw_result["LastSeen"], "%Y-%m-%dT%H:%M:%S.%fZ"
            ),
            "last_update": dt.datetime.strptime(
                response.raw_result["LastUpdate"], "%Y-%m-%dT%H:%M:%S.%fZ"
            ),
        }

        severity = (
            TISeverity.information
            if sev == "Low"
            else TISeverity.warning
            if sev == "Medium"
            else TISeverity.high
            if sev == "High"
            else TISeverity.unknown
        )

        return True, severity, result_dict
