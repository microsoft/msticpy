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
from typing import Any, Dict, Tuple

import attr

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .ti_http_provider import HttpTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s
class _XForceParams(APILookupParams):
    # override APILookupParams to set common defaults
    def __attrs_post_init__(self):
        self.auth_str = ["{API_ID}", "{API_KEY}"]
        self.auth_type = "HTTPBasic"


@export
class XForce(HttpTIProvider):
    """IBM XForce Lookup."""

    _BASE_URL = "https://api.xforce.ibmcloud.com"

    _QUERIES = {
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

    _REQUIRED_PARAMS = ["API_ID", "API_KEY"]

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
        severity = ResultSeverity.information
        if self._failed_response(response) or not isinstance(
            response["RawResult"], dict
        ):
            return False, severity, "Not found."
        result = True
        result_dict = {}
        if (
            response["IocType"] in ["ipv4", "ipv6", "url", "dns"]
            and not response["QuerySubtype"]
        ):
            score = response["RawResult"].get("score", 0)
            result_dict.update(
                {
                    "score": response["RawResult"].get("score", 0),
                    "cats": response["RawResult"].get("cats"),
                    "categoryDescriptions": response["RawResult"].get(
                        "categoryDescriptions"
                    ),
                    "reason": response["RawResult"].get("reason"),
                    "reasonDescription": response["RawResult"].get(
                        "reasonDescription", 0
                    ),
                    "tags": response["RawResult"].get("tags", 0),
                }
            )
            severity = (
                ResultSeverity.information
                if score < 2
                else ResultSeverity.warning
                if 2 <= score < 5
                else ResultSeverity.high
            )
        if (
            response["IocType"] in ["file_hash", "md5_hash", "sha1_hash", "sha256_hash"]
            or response["QuerySubtype"] == "malware"
        ):
            malware = response["RawResult"].get("malware")
            if malware:
                result_dict.update(
                    {
                        "risk": malware.get("risk"),
                        "family": malware.get("family"),
                        "reasonDescription": response["RawResult"].get(
                            "reasonDescription", 0
                        ),
                    }
                )
                severity = ResultSeverity.high
        if response["IocType"] in ["dns", "ipv4", "ipv6", "hostname"] and response[
            "QuerySubtype"
        ] in ["info", "passivedns", "whois"]:
            records = response["RawResult"].get("total_rows", 0)
            contact = response["RawResult"].get("contact", 0)
            if records:
                result_dict["records"] = records
            elif contact:
                result_dict["contact"] = contact
        return result, severity, result_dict
