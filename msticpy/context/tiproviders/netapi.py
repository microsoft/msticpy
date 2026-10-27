# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
NetAPI Provider.

Looks up domains, hostnames and URLs in the NetAPI compromised feed: the
hosts that public threat feeds (CERT Polska, blackbook and others) report for
malware or phishing. The default query answers whether the
observable is listed now (seen within the last 24 hours), was listed in the
past only, or was never listed. The ``rank`` query subtype returns the
position of a domain in the NetAPI Top 1M, a daily popularity ranking.

The API needs no key. Anonymous use is limited to 30 requests a minute and
1,000 a day per IP address.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables.

"""

from __future__ import annotations

from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Slava Olesik"

_HEADERS: dict[str, str] = {"User-Agent": "netapi-msticpy/1.0"}

_CHECK = APILookupParams(
    path="/api-json/",
    headers=_HEADERS,
    params={"method": "compromised-check", "value": "{observable}"},
)
_RANK = APILookupParams(
    path="/api-json/",
    headers=_HEADERS,
    params={"method": "domain-rank", "domain": "{observable}"},
)


class NetAPI(HttpTIProvider):
    """NetAPI Lookup."""

    _BASE_URL: ClassVar[str] = "https://netapi.com"

    # The feed matches a hostname or a URL through its registered domain, so
    # every name type uses the same query.
    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        "dns": _CHECK,
        "hostname": _CHECK,
        "url": _CHECK,
        "dns-rank": _RANK,
        "hostname-rank": _RANK,
    }

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """
        Return the details of the response.

        Parameters
        ----------
        response : Dict
            The returned data response

        Returns
        -------
        tuple[bool, ResultSeverity, Any]
            bool = positive or negative hit
            ResultSeverity = enumeration of severity
            Object with match details

        """
        if self._failed_response(response):
            return False, ResultSeverity.information, "Not found."

        raw: dict[str, Any] = response["RawResult"]
        if "in_top_1m" in raw:
            return self._parse_rank(raw)
        return self._parse_listing(raw)

    @staticmethod
    def _parse_listing(raw: dict[str, Any]) -> tuple[bool, ResultSeverity, Any]:
        """Return the compromised feed listing of the observable."""
        if not raw.get("listed_ever"):
            return False, ResultSeverity.information, "Not listed, now or in the past."

        # A past listing is mostly an incident that is over, often a legitimate
        # site that was compromised and cleaned up, so only a current listing
        # carries a severity.
        severity = ResultSeverity.high if raw.get("listed_now") else ResultSeverity.information
        registrar = raw.get("registrar")
        details: dict[str, Any] = {
            "Listed Now": bool(raw.get("listed_now")),
            "Matched Value": raw.get("matched_value"),
            "First Seen": raw.get("first_seen"),
            "Last Seen": raw.get("last_seen"),
            "Reported By": raw.get("block_reason"),
            "Registrar": (
                registrar.get("brand_name") or registrar.get("name")
                if isinstance(registrar, dict)
                else None
            ),
        }
        return True, severity, details

    @staticmethod
    def _parse_rank(raw: dict[str, Any]) -> tuple[bool, ResultSeverity, Any]:
        """Return the Top 1M position of the registered domain."""
        details: dict[str, Any] = {
            "Domain": raw.get("domain"),
            "Rank": raw.get("rank"),
            "Snapshot Date": raw.get("snapshot_date"),
        }
        if not raw.get("in_top_1m"):
            return False, ResultSeverity.information, details
        return True, ResultSeverity.information, details
