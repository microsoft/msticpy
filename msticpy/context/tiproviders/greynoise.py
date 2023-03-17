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
from typing import Any, Dict, Tuple

from ..._version import VERSION
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Pete Bryan"


class GreyNoise(HttpTIProvider):
    """GreyNoise Lookup."""

    _BASE_URL = "https://api.greynoise.io"

    _QUERIES = {
        # Community API
        "ipv4": APILookupParams(
            path="/v3/community/{observable}",
            headers={"key": "{API_KEY}"},
        ),
        # Enterprise API Quick Lookup
        "ipv4-quick": APILookupParams(
            path="/v2/noise/quick/{observable}",
            headers={"key": "{API_KEY}"},
        ),
        # Enterprise API Full Lookup
        "ipv4-full": APILookupParams(
            path="/v2/noise/context/{observable}",
            headers={"key": "{API_KEY}"},
        ),
    }

    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        Tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """
        if self._failed_response(response) or not isinstance(
            response["RawResult"], dict
        ):
            return False, ResultSeverity.information, "Not found."
        result = True
        result_dict = {}
        # If community API response extract key elements
        if "riot" in response["RawResult"]:
            result_dict.update(
                {
                    "Classification": response["RawResult"].get("classification"),
                    "Name": response["RawResult"].get("name"),
                    "Last Seen": response["RawResult"].get("last_seen"),
                    "Message": response["RawResult"].get("message"),
                    "Noise": response["RawResult"].get("noise"),
                    "RIOT": response["RawResult"].get("riot"),
                }
            )
        # If enterprise full lookup response extract key elements
        if "actor" in response["RawResult"]:
            result_dict.update(
                {
                    "Classification": response["RawResult"].get("classification"),
                    "First Seen": response["RawResult"].get("first_seen"),
                    "Last Seen": response["RawResult"].get("last_seen"),
                    "Actor": response["RawResult"].get("actor"),
                    "Tags": response["RawResult"].get("tags"),
                    "VPN": response["RawResult"].get("vpn_service", False),
                    "Metadata": response["RawResult"].get("metadata"),
                }
            )
        # If enterprise quick lookup just return raw data is its so small
        if "code" in response["RawResult"]:
            result_dict = response["RawResult"]

        severity = ResultSeverity.information
        if response["RawResult"]["classification"] == "malicious":
            severity = ResultSeverity.high
        return result, severity, result_dict
