# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
IPQualityScore Provider.

This provider offers contextual lookup services and fraud scoring for IP addresses.
https://www.ipqualityscore.com/
"""
from __future__ import annotations

from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Pete Bryan"


class IPQualityScore(HttpTIProvider):
    """IP Quality Score Lookup."""

    _BASE_URL = "https://www.ipqualityscore.com/api/json"

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        # Supported API Types
        "ipv4": APILookupParams(
            path="/ip/{AuthKey}/{observable}",
        ),
    }

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["AuthKey"]

    MEDIUM_SEVERITY: ClassVar[int] = 50
    HIGH_SEVERITY: ClassVar[int] = 80

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
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
            response["RawResult"],
            dict,
        ):
            return False, ResultSeverity.information, "Not found."
        result = True
        result_dict: dict[str, Any] = {}
        result_dict.update(
            {
                "FraudScore": response["RawResult"].get("fraud_score"),
                "ISP": response["RawResult"].get("ISP"),
                "ASN": response["RawResult"].get("ASN"),
                "CountryOrRegion": response["RawResult"].get("country_code"),
                "Region": response["RawResult"].get("city"),
                "City": response["RawResult"].get("region"),
                "Organization": response["RawResult"].get("organization"),
                "Latitude": response["RawResult"].get("latitude"),
                "Longitude": response["RawResult"].get("longitude"),
                "IsMobile": response["RawResult"].get("mobile"),
                "IsProxy": response["RawResult"].get("proxy"),
                "IsTor": response["RawResult"].get("active_tor"),
                "IsVPN": response["RawResult"].get("active_vpn"),
                "IsBot": response["RawResult"].get("bot_status"),
                "AbuseStatus": response["RawResult"].get("recent_abuse"),
            },
        )

        severity = ResultSeverity.information
        if (
            response["RawResult"]["fraud_score"] > self.MEDIUM_SEVERITY
            and response["RawResult"]["fraud_score"] < self.HIGH_SEVERITY
        ):
            severity = ResultSeverity.warning
        elif response["RawResult"]["fraud_score"] >= self.HIGH_SEVERITY:
            severity = ResultSeverity.high
        return result, severity, result_dict
