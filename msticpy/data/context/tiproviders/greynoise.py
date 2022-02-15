# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
GreyNoise Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from typing import Any, Tuple

from ...._version import VERSION
from .http_base import HttpProvider, IoCLookupParams
from .ti_provider_base import LookupResult, TISeverity

__version__ = VERSION
__author__ = "Pete Bryan"


class GreyNoise(HttpProvider):
    """GreyNoise Lookup."""

    _BASE_URL = "https://api.greynoise.io"

    _IOC_QUERIES = {
        # Community API
        "ipv4": IoCLookupParams(
            path="/v3/community/{observable}",
            headers={"key": "{API_KEY}"},
        ),
        # Enterprise API Quick Lookup
        "ipv4-quick": IoCLookupParams(
            path="/v2/noise/quick/{observable}",
            headers={"key": "{API_KEY}"},
        ),
        # Enterprise API Full Lookup
        "ipv4-full": IoCLookupParams(
            path="/v2/noise/context/{observable}",
            headers={"key": "{API_KEY}"},
        ),
    }

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
        result = True
        result_dict = {}
        # If community API response extract key elements
        if "riot" in response.raw_result:
            result_dict.update(
                {
                    "Classification": response.raw_result.get("classification"),
                    "Name": response.raw_result.get("name"),
                    "Last Seen": response.raw_result.get("last_seen"),
                    "Message": response.raw_result.get("message"),
                    "Noise": response.raw_result.get("noise"),
                    "RIOT": response.raw_result.get("riot"),
                }
            )
        # If enterprise full lookup response extract key elements
        if "actor" in response.raw_result:
            result_dict.update(
                {
                    "Classification": response.raw_result.get("classification"),
                    "First Seen": response.raw_result.get("first_seen"),
                    "Last Seen": response.raw_result.get("last_seen"),
                    "Actor": response.raw_result.get("actor"),
                    "Tags": response.raw_result.get("tags"),
                    "VPN": response.raw_result.get("vpn_service", False),
                    "Metadata": response.raw_result.get("metadata"),
                }
            )
        # If enterprise quick lookup just return raw data is its so small
        if "code" in response.raw_result:
            result_dict = response.raw_result

        severity = TISeverity.information
        if response.raw_result["classification"] == "malicious":
            severity = TISeverity.high
        return result, severity, result_dict
