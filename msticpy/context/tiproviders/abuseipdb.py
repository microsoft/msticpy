# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
AbuseIPDB Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .result_severity import ResultSeverity
from .ti_http_provider import HttpTIProvider

__version__ = VERSION
__author__ = "Rubén Revuelta Briz"


@export
class AbuseIPDB(HttpTIProvider):
    """AbuseIPDB Lookup."""

    PROVIDER_NAME: ClassVar[str] = "AbuseIPDB"

    _BASE_URL: ClassVar[str] = "https://api.abuseipdb.com/"

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        "ipv4": APILookupParams(
            path="api/v2/check",
            params={"ipAddress": "{observable}"},
            headers={"Key": "{AuthKey}", "Accept": "application/json"},
        ),
    }

    HIGH_SEVERITY: ClassVar[int] = 50

    # aliases
    _QUERIES["ipv6"] = _QUERIES["ipv4"]

    def parse_results(self: Self, response: dict) -> tuple[bool, ResultSeverity, Any]:
        """Return the details of the response."""
        if self._failed_response(response) or not isinstance(
            response["RawResult"],
            dict,
        ):
            return False, ResultSeverity.information, "Not found."

        data: dict[str, Any] = response["RawResult"]["data"]
        result_dict: dict[str, Any] = {
            "countryCode": data.get("countryCode"),
            "usage": data.get("usageType"),
            "isp": data.get("isp"),
            "domain": data.get("domain"),
            "hostNames": data.get("hostnames"),
            "lastReportedAt": data.get("lastReportedAt"),
        }

        score: int = data.get("abuseConfidenceScore", 0)

        if score > self.HIGH_SEVERITY:
            result_severity = ResultSeverity.high
        elif score > 0:
            result_severity = ResultSeverity.warning
        else:
            result_severity = ResultSeverity.information

        return (True, result_severity, result_dict)
