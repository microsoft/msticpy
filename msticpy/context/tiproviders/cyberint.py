# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Cyberint Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing requires an API key.
https://cyberint.com/
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
__author__ = "Meroujan Antonyan"


_DEF_HEADERS: dict[str, str] = {
    "Cookie": "access_token={AuthKey}",
    "Accept": "application/json",
}


@export
class Cyberint(HttpTIProvider):
    """Cyberint Lookup."""

    PROVIDER_NAME: ClassVar[str] = "Cyberint"

    _BASE_URL: ClassVar[str] = "https://{Instance}.cyberint.io/"

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        "ipv4": APILookupParams(
            path="ioc/api/v1/ipv4",
            params={
                "value": "{observable}",
            },
            headers=_DEF_HEADERS,
        ),
        "dns": APILookupParams(
            path="ioc/api/v1/domain",
            params={
                "value": "{observable}",
            },
            headers=_DEF_HEADERS,
        ),
        "url": APILookupParams(
            path="ioc/api/v1/url",
            params={
                "value": "{observable}",
            },
            headers=_DEF_HEADERS,
        ),
        "sha256_hash": APILookupParams(
            path="ioc/api/v1/file/sha256",
            params={
                "value": "{observable}",
            },
            headers=_DEF_HEADERS,
        ),
    }

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["Instance", "AuthKey"]

    HIGH_SEVERITY: ClassVar[int] = 50

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

        data: dict[str, Any] = response["RawResult"].get("data", {})

        score: int = 0
        if data.get("risk"):
            score = (data.get("risk", {})).get("malicious_score", 0)

        if score > self.HIGH_SEVERITY:
            result_severity = ResultSeverity.high
        elif score > 0:
            result_severity = ResultSeverity.warning
        else:
            result_severity = ResultSeverity.information

        return (True, result_severity, data)
