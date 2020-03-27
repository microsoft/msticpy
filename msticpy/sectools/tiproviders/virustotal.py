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
import datetime as dt
from typing import Any, Tuple, Dict

from .ti_provider_base import LookupResult, TISeverity
from .http_base import HttpProvider, IoCLookupParams
from ...common.utility import export
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

    # pylint: disable=duplicate-code, too-many-branches
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

        else:
            if "detected_urls" in response.raw_result:
                self._extract_url_results(
                    response=response,
                    result_dict=result_dict,
                    hit_type="detected_urls",
                    item_type="url",
                    date_name="scan_date",
                )

            if "detected_downloaded_samples" in response.raw_result:
                self._extract_url_results(
                    response=response,
                    result_dict=result_dict,
                    hit_type="detected_downloaded_samples",
                    item_type="sha256",
                    date_name="date",
                )

        if "positives" in result_dict:
            if result_dict["positives"] > 1:
                severity = TISeverity.high
            elif result_dict["positives"] > 0:
                severity = TISeverity.warning
            else:
                severity = TISeverity.information
        else:
            severity = TISeverity.unknown

        return True, severity, result_dict

    # pylint: enable=duplicate-code, too-many-branches

    @staticmethod
    def _extract_url_results(
        response: LookupResult,
        result_dict: Dict[str, Any],
        hit_type: str,
        item_type: str,
        date_name: str,
    ):
        if not isinstance(response.raw_result, dict):
            return
        time_scope = dt.datetime.now() - dt.timedelta(days=30)
        result_dict[hit_type] = [
            item[item_type]
            for item in response.raw_result[hit_type]
            if item_type in item
            and dt.datetime.strptime(item[date_name], "%Y-%m-%d %H:%M:%S") > time_scope
        ]
        # positives are listed per detected_url so we need to
        # pull those our and sum them.
        positives = sum(
            [
                item["positives"]
                for item in response.raw_result[hit_type]
                if "positives" in item
                and dt.datetime.strptime(item[date_name], "%Y-%m-%d %H:%M:%S")
                > time_scope
            ]
        )
        result_dict["positives"] = positives
