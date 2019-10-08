# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
VirusTotal Provider.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from typing import Any, Tuple

from .ti_provider_base import LookupResult, TISeverity
from .http_base import HttpProvider, IoCLookupParams
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


_DEF_HEADERS = {"User-Agent": "VirusTotal", "Content-Type": "application/json"}
_GZIP_HEADERS = {"Accept-Encoding": "gzip, deflate"}


@export
class VirusTotal(HttpProvider):
    """VirusTotal Lookup."""

    _BASE_URL = "https://www.virustotal.com/"

    _PARAMS = {"apikey": "{API_KEY}"}
    _IOC_QUERIES = {
        "ipv4": IoCLookupParams(
            path="vtapi/v2/ip-address/report",
            params={**_PARAMS, "ip": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "dns": IoCLookupParams(
            path="vtapi/v2/domain/report",
            params={**_PARAMS, "domain": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "file_hash": IoCLookupParams(
            path="vtapi/v2/file/report",
            params={**_PARAMS, "resource": "{observable}"},
            headers={**_DEF_HEADERS, **_GZIP_HEADERS},
        ),
        "url": IoCLookupParams(
            path="vtapi/v2/url/report",
            params={**_PARAMS, "resource": "{observable}"},
            headers={**_DEF_HEADERS, **_GZIP_HEADERS},
        ),
    }

    # pylint: disable=duplicate-code
    # aliases
    _IOC_QUERIES["md5_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["sha1_hash"] = _IOC_QUERIES["file_hash"]
    _IOC_QUERIES["sha256_hash"] = _IOC_QUERIES["file_hash"]

    _REQUIRED_PARAMS = ["API_KEY"]

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
        if self._failed_response(response) or not isinstance(response.raw_result, dict):
            return False, TISeverity.information, "Not found."

        result_dict = {}
        result_dict["verbose_msg"] = response.raw_result.get("verbose_msg", None)
        result_dict["response_code"] = response.raw_result.get("response_code", None)

        if response.ioc_type in [
            "url",
            "md5_hash",
            "sha1_hash",
            "sha256_hash",
            "file_hash",
        ]:
            result_dict["resource"] = response.raw_result.get("resource", None)
            result_dict["permalink"] = response.raw_result.get("permalink", None)
            result_dict["positives"] = response.raw_result.get("positives", 0)
        if "detected_urls" in response.raw_result:
            result_dict["detected_urls"] = [
                item["url"]
                for item in response.raw_result["detected_urls"]
                if "url" in item
            ]
            # positives are listed per detected_url so we need to
            # pull those our and sum them.
            positives = sum(
                [
                    item["positives"]
                    for item in response.raw_result["detected_urls"]
                    if "positives" in item
                ]
            )
            result_dict["positives"] = positives

        severity = (
            TISeverity.high if result_dict["positives"] > 0 else TISeverity.information
        )
        return True, severity, result_dict

    # pylint: enable=duplicate-code
