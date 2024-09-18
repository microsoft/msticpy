# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
IBM XForce Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Any, ClassVar

from typing_extensions import Self

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@dataclass
class _XForceParams(APILookupParams):
    # override APILookupParams to set common defaults
    def __attrs_post_init__(self: Self) -> None:
        self.auth_str = ["{ApiID}", "{AuthKey}"]
        self.auth_type = "HTTPBasic"


@export
class XForce(HttpTIProvider):
    """IBM XForce Lookup."""

    _BASE_URL: ClassVar[str] = "https://api.xforce.ibmcloud.com"

    _QUERIES: ClassVar[dict[str, _XForceParams]] = {
        "ipv4": _XForceParams(path="/ipr/{observable}"),
        "ipv4-rep": _XForceParams(path="/ipr/history/{observable}"),
        "ipv4-malware": _XForceParams(path="/ipr/malware/{observable}"),
        "ipv4-whois": _XForceParams(path="/whois/{observable}"),
        "dns-passivedns": _XForceParams(path="/resolve/{observable}"),
        "hostname-whois": _XForceParams(path="/whois/{observable}"),
        "file_hash": _XForceParams(path="/malware/{observable}"),
        "url": _XForceParams(path="/url/{observable}"),
        "url-malware": _XForceParams(path="/url/malware/{observable}"),
    }

    # aliases
    _QUERIES["ipv6"] = _QUERIES["ipv4"]
    _QUERIES["ipv6-rep"] = _QUERIES["ipv4-rep"]
    _QUERIES["ipv6-malware"] = _QUERIES["ipv4-malware"]
    _QUERIES["ipv6-whois"] = _QUERIES["ipv4-whois"]
    _QUERIES["md5_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha1_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha256_hash"] = _QUERIES["file_hash"]
    _QUERIES["dns"] = _QUERIES["url"]
    _QUERIES["dns-malware"] = _QUERIES["url-malware"]
    _QUERIES["ipv4-passivedns"] = _QUERIES["dns-passivedns"]
    _QUERIES["ipv6-passivedns"] = _QUERIES["dns-passivedns"]
    _QUERIES["hostname-whois"] = _QUERIES["ipv4-whois"]
    _QUERIES["dns-whois"] = _QUERIES["ipv4-whois"]

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["ApiID", "AuthKey"]

    HIGH_SEVERITY: ClassVar[int] = 5
    MEDIUM_SEVERITY: ClassVar[int] = 2

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
        severity: ResultSeverity = ResultSeverity.information
        if self._failed_response(response) or not isinstance(
            response["RawResult"],
            dict,
        ):
            return False, severity, "Not found."
        result = True
        result_dict: dict[str, Any] = {}
        if (
            response["IocType"] in ["ipv4", "ipv6", "url", "dns"]
            and not response["QuerySubtype"]
        ):
            # For some IocTypes "dns" and "url", the response structure differs
            report: dict[str, Any] = response["RawResult"].get(
                "result",
                response["RawResult"],
            )
            score: int = report.get("score", 0)
            if score is None:
                score = 0
            result_dict.update(
                {
                    "score": report.get("score", 0),
                    "cats": report.get("cats"),
                    "categoryDescriptions": report.get("categoryDescriptions"),
                    "reason": report.get("reason"),
                    "reasonDescription": report.get("reasonDescription", 0),
                    "tags": report.get("tags", 0),
                },
            )
            severity = (
                ResultSeverity.information
                if score < self.MEDIUM_SEVERITY
                else (
                    ResultSeverity.warning
                    if self.MEDIUM_SEVERITY <= score < self.HIGH_SEVERITY
                    else ResultSeverity.high
                )
            )
        if (
            response["IocType"] in ["file_hash", "md5_hash", "sha1_hash", "sha256_hash"]
            or response["QuerySubtype"] == "malware"
        ):
            malware: dict[str, Any] | None = response["RawResult"].get("malware")
            if malware:
                result_dict.update(
                    {
                        "risk": malware.get("risk"),
                        "family": malware.get("family"),
                        "reasonDescription": response["RawResult"].get(
                            "reasonDescription",
                            0,
                        ),
                    },
                )
                severity = ResultSeverity.high
        if response["IocType"] in ["dns", "ipv4", "ipv6", "hostname"] and response[
            "QuerySubtype"
        ] in ["info", "passivedns", "whois"]:
            records: int = response["RawResult"].get("total_rows", 0)
            contact: int = response["RawResult"].get("contact", 0)
            if records:
                result_dict["records"] = records
            elif contact:
                result_dict["contact"] = contact
        return result, severity, result_dict
