# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
VPNDetection Provider.

Flags whether an IP address is a VPN, Tor exit or relay (iCloud Private Relay and
similar), with its country and ASN. The API answers without a key; a key adds the
proxy and hosting flags and the VPN's provider on paid plans.
https://vpndetection.io/
"""

from __future__ import annotations

from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Uman Shahzad"

_ANONYMIZER_FLAGS = (
    "is_vpn",
    "is_tor",
    "is_relay",
    "is_resproxy",
    "is_dcproxy",
    "is_mobproxy",
    "is_pubproxy",
)


class VPNDetection(HttpTIProvider):
    """VPNDetection Lookup."""

    _BASE_URL: ClassVar[str] = "https://api.vpndetection.io"

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        "ipv4": APILookupParams(
            path="/{observable}",
            headers={"Authorization": "Bearer {AuthKey}"},
        ),
        "ipv6": APILookupParams(
            path="/{observable}",
            headers={"Authorization": "Bearer {AuthKey}"},
        ),
    }

    def _substitute_parms(
        self: Self,
        value: str,
        value_type: str,
        query_type: str | None = None,
    ) -> tuple[str, dict[str, Any]]:
        """Drop the Authorization header when no AuthKey is configured."""
        verb, req_dict = super()._substitute_parms(value, value_type, query_type)
        if not self._request_params.get("AuthKey"):
            req_dict["headers"].pop("Authorization", None)
        return verb, req_dict

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
        raw: dict[str, Any] = response["RawResult"]
        vpn: dict[str, Any] = raw.get("vpn") or {}
        location: dict[str, Any] = raw.get("location") or {}
        asn: dict[str, Any] = raw.get("asn") or {}
        result_dict: dict[str, Any] = {
            "IsVPN": raw.get("is_vpn"),
            "VPNProvider": vpn.get("provider_name") or vpn.get("provider"),
            "IsTor": raw.get("is_tor"),
            "IsRelay": raw.get("is_relay"),
            "IsHosting": raw.get("is_hosting"),
            "IsResidentialProxy": raw.get("is_resproxy"),
            "IsDatacenterProxy": raw.get("is_dcproxy"),
            "IsMobileProxy": raw.get("is_mobproxy"),
            "IsPublicProxy": raw.get("is_pubproxy"),
            "IsBogon": raw.get("is_bogon"),
            "CountryOrRegion": location.get("country"),
            "ASN": asn.get("asn"),
            "ASNName": asn.get("name"),
            "ASNType": asn.get("type"),
        }
        severity = ResultSeverity.information
        if any(raw.get(flag) for flag in _ANONYMIZER_FLAGS):
            severity = ResultSeverity.warning
        return True, severity, result_dict
