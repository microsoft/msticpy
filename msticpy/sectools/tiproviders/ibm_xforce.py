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
from typing import Any, Tuple

import attr

from .ti_provider_base import LookupResult, TISeverity
from .http_base import HttpProvider, IoCLookupParams
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s
class _XForceParams(IoCLookupParams):
    # override IoCLookupParams to set common defaults
    def __attrs_post_init__(self):
        self.auth_str = ["{API_ID}", "{API_KEY}"]
        self.auth_type = "HTTPBasic"


@export
class XForce(HttpProvider):
    """IBM XForce Lookup."""

    _BASE_URL = "https://api.xforce.ibmcloud.com"

    _IOC_QUERIES = {
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

    # pylint: disable=duplicate-code
    # aliases
    _IOC_QUERIES["ipv6"] = _IOC_QUERIES["ipv4"]
    _IOC_QUERIES["ipv6-rep"] = _IOC_QUERIES["ipv4-rep"]
    _IOC_QUERIES["ipv6-malware"] = _IOC_QUERIES["ipv4-malware"]
    _IOC_QUERIES["ipv6-whois"] = _IOC_QUERIES["ipv4-whois"]
    _IOC_QUERIES["md5_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["sha1_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["sha256_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["dns"] = _IOC_QUERIES["url"]
    _IOC_QUERIES["dns-malware"] = _IOC_QUERIES["url-malware"]
    _IOC_QUERIES["ipv4-passivedns"] = _IOC_QUERIES["dns-passivedns"]
    _IOC_QUERIES["ipv6-passivedns"] = _IOC_QUERIES["dns-passivedns"]
    _IOC_QUERIES["hostname-whois"] = _IOC_QUERIES["ipv4-whois"]
    _IOC_QUERIES["dns-whois"] = _IOC_QUERIES["ipv4-whois"]

    _REQUIRED_PARAMS = ["API_ID", "API_KEY"]

    # pylint: disable=too-many-branches
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
        severity = TISeverity.information
        if self._failed_response(response) or not isinstance(response.raw_result, dict):
            return False, severity, "Not found."
        result = True
        result_dict = {}
        if (
            response.ioc_type in ["ipv4", "ipv6", "url", "dns"]
            and not response.query_subtype
        ):
            score = response.raw_result.get("score", 0)
            result_dict.update(
                {
                    "score": response.raw_result.get("score", 0),
                    "cats": response.raw_result.get("cats"),
                    "categoryDescriptions": response.raw_result.get(
                        "categoryDescriptions"
                    ),
                    "reason": response.raw_result.get("reason"),
                    "reasonDescription": response.raw_result.get(
                        "reasonDescription", 0
                    ),
                    "tags": response.raw_result.get("tags", 0),
                }
            )
            severity = (
                TISeverity.information
                if score == 0
                else TISeverity.warning
                if score == 1
                else TISeverity.high
            )
        if (
            response.ioc_type in ["file_hash", "md5_hash", "sha1_hash", "sha256_hash"]
            or response.query_subtype == "malware"
        ):
            malware = response.raw_result.get("malware")
            if malware:
                result_dict.update(
                    {
                        "risk": malware.get("risk"),
                        "family": malware.get("family"),
                        "reasonDescription": response.raw_result.get(
                            "reasonDescription", 0
                        ),
                    }
                )
                severity = TISeverity.high
        if response.ioc_type in [
            "dns",
            "ipv4",
            "ipv6",
            "hostname",
        ] and response.query_subtype in ["info", "passivedns", "whois"]:
            records = response.raw_result.get("total_rows", 0)
            contact = response.raw_result.get("contact", 0)
            if records:
                result_dict.update({"records": records})
            elif contact:
                result_dict.update({"contact": contact})
        return result, severity, result_dict
