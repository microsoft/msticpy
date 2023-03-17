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
from typing import Any, Dict, Tuple

from ..._version import VERSION
from ...common.utility import export
from ..http_provider import APILookupParams
from .result_severity import ResultSeverity
from .ti_http_provider import HttpTIProvider

__version__ = VERSION
__author__ = "Ian Hellen"


_DEF_HEADERS = {"User-Agent": "VirusTotal", "Content-Type": "application/json"}
_GZIP_HEADERS = {"Accept-Encoding": "gzip, deflate"}


@export
class VirusTotal(HttpTIProvider):
    """VirusTotal Lookup."""

    _BASE_URL = "https://www.virustotal.com/"

    _PARAMS = {"apikey": "{API_KEY}"}
    _QUERIES = {
        "ipv4": APILookupParams(
            path="vtapi/v2/ip-address/report",
            params={**_PARAMS, "ip": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "dns": APILookupParams(
            path="vtapi/v2/domain/report",
            params={**_PARAMS, "domain": "{observable}"},
            headers=_DEF_HEADERS,
        ),
        "file_hash": APILookupParams(
            path="vtapi/v2/file/report",
            params={**_PARAMS, "resource": "{observable}"},
            headers={**_DEF_HEADERS, **_GZIP_HEADERS},
        ),
        "url": APILookupParams(
            path="vtapi/v2/url/report",
            params={**_PARAMS, "resource": "{observable}"},
            headers={**_DEF_HEADERS, **_GZIP_HEADERS},
        ),
    }

    # aliases
    _QUERIES["md5_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha1_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha256_hash"] = _QUERIES["file_hash"]

    _REQUIRED_PARAMS = ["API_KEY"]

    _VT_DETECT_RESULTS = {
        "detected_urls": ("url", "scan_date"),
        "detected_downloaded_samples": ("sha256", "date"),
        "detected_communicating_samples": ("sha256", "date"),
    }

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
        if self._failed_response(response) or not isinstance(
            response["RawResult"], dict
        ):
            return False, ResultSeverity.information, "Not found."

        result_dict = {
            "verbose_msg": response["RawResult"].get("verbose_msg", None),
            "response_code": response["RawResult"].get("response_code", None),
            "positives": 0,
        }

        if response["IocType"] in [
            "url",
            "md5_hash",
            "sha1_hash",
            "sha256_hash",
            "file_hash",
        ]:
            result_dict["resource"] = response["RawResult"].get("resource", None)
            result_dict["permalink"] = response["RawResult"].get("permalink", None)
            result_dict["positives"] = response["RawResult"].get("positives", 0)

        else:
            for hit_type, params in self._VT_DETECT_RESULTS.items():
                if hit_type in response["RawResult"]:
                    self._extract_url_results(
                        response=response,
                        result_dict=result_dict,
                        hit_type=hit_type,
                        item_type=params[0],
                        date_name=params[1],
                    )

        if "positives" in result_dict:
            if result_dict["positives"] > 1:
                severity = ResultSeverity.high
            elif result_dict["positives"] > 0:
                severity = ResultSeverity.warning
            else:
                severity = ResultSeverity.information
        else:
            severity = ResultSeverity.unknown

        return True, severity, result_dict

    # pylint: enable=duplicate-code, too-many-branches

    @staticmethod
    def _extract_url_results(
        response: Dict,
        result_dict: Dict[str, Any],
        hit_type: str,
        item_type: str,
        date_name: str,
    ):
        if not isinstance(response["RawResult"], dict):
            return
        time_scope = dt.datetime.now() - dt.timedelta(days=30)
        result_dict[hit_type] = [
            item[item_type]
            for item in response["RawResult"][hit_type]
            if item_type in item
            and dt.datetime.strptime(item[date_name], "%Y-%m-%d %H:%M:%S") > time_scope
        ]
        # positives are listed per detected_url so we need to
        # pull those our and sum them.
        positives = sum(
            item["positives"]
            for item in response["RawResult"][hit_type]
            if "positives" in item
            and dt.datetime.strptime(item[date_name], "%Y-%m-%d %H:%M:%S") > time_scope
        )
        result_dict["positives"] += positives
