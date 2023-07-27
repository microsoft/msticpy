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
from typing import Any, Dict, Tuple

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

    PROVIDER_NAME = "AbuseIPDB"

    _BASE_URL = "https://api.abuseipdb.com/"

    _QUERIES = {
        "ipv4": APILookupParams(
            path="api/v2/check",
            params={"ipAddress": "{observable}"},
            headers={"Key": "{AuthKey}", "Accept": "application/json"},
        )
    }

    # aliases
    _QUERIES["ipv6"] = _QUERIES["ipv4"]

    def parse_results(self, response: Dict) -> Tuple[bool, ResultSeverity, Any]:
        """Return the details of the response."""
        if self._failed_response(response) or not isinstance(
            response["RawResult"], dict
        ):
            return False, ResultSeverity.information, "Not found."

        data = response["RawResult"]["data"]
        result_dict = {
            "countryCode": data.get("countryCode", None),
            "usage": data.get("usageType", None),
            "isp": data.get("isp", None),
            "domain": data.get("domain", None),
            "hostNames": data.get("hostnames", None),
            "lastReportedAt": data.get("lastReportedAt", None),
        }

        score = data.get("abuseConfidenceScore", None)

        if score == 0:
            result_severity = ResultSeverity.information
        elif score <= 50:
            result_severity = ResultSeverity.warning
        elif score > 50:
            result_severity = ResultSeverity.high

        return (True, result_severity, result_dict)
