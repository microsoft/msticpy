# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Microsoft Sentinel TI provider class.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
from typing import Any, Dict, Tuple

import pandas as pd

from ..._version import VERSION
from ...common.utility import export
from .kql_base import KqlTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class AzSTI(KqlTIProvider):
    """Microsoft Sentinel TI provider class."""

    _QUERIES: Dict[str, tuple] = {
        "ipv4": ("ThreatIntelligence.list_indicators_by_ip", {"ioc": "observables"}),
        "file_hash": (
            "ThreatIntelligence.list_indicators_by_hash",
            {"ioc": "observables"},
        ),
        "windows_path": (
            "ThreatIntelligence.list_indicators_by_filepath",
            {"ioc": "observables"},
        ),
        "dns": ("ThreatIntelligence.list_indicators_by_domain", {"ioc": "observables"}),
        "url": ("ThreatIntelligence.list_indicators_by_url", {"ioc": "observables"}),
    }

    # aliases
    _QUERIES["ipv6"] = _QUERIES["ipv4"]
    _QUERIES["md5_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha1_hash"] = _QUERIES["file_hash"]
    _QUERIES["sha256_hash"] = _QUERIES["file_hash"]
    _QUERIES["linux_path"] = _QUERIES["windows_path"]
    _QUERIES["hostname"] = _QUERIES["dns"]

    _REQUIRED_TABLES = ["ThreatIntelligenceIndicator"]

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
        if response["RawResult"] is None:
            return False, ResultSeverity.information, "No data"

        severity = ResultSeverity.warning
        # if this is a series (single row) return a dictionary
        if isinstance(response["RawResult"], pd.Series):
            extracted_data = response["RawResult"][
                ["Action", "ThreatType", "ThreatSeverity", "Active", "ConfidenceScore"]
            ].to_dict()
            if extracted_data["Action"].lower() in ["alert", "block"]:
                severity = ResultSeverity.high
            return True, ResultSeverity.warning, extracted_data
        # if this is a dataframe (multiple rows)
        # concatenate the values for each column/record into a list
        # and return as a dictionary
        if isinstance(response["RawResult"], pd.DataFrame):
            d_frame = response["RawResult"]
            if d_frame["Action"].str.lower().isin(["alert", "block"]).any():
                severity = ResultSeverity.high

            return (
                True,
                severity,
                {
                    "Action": self._series_to_list(d_frame["Action"]),
                    "ThreatType": self._series_to_list(d_frame["ThreatType"]),
                    "ThreatSeverity": self._series_to_list(d_frame["ThreatSeverity"]),
                    "Active": self._series_to_list(d_frame["Active"]),
                    "Description": self._series_to_list(d_frame["Description"]),
                    "ConfidenceScore": self._series_to_list(d_frame["ConfidenceScore"]),
                },
            )
        return False, ResultSeverity.information, "No data"

    @staticmethod
    def _get_detail_summary(data_result: pd.DataFrame) -> pd.Series:
        # For the input frame return details in a series with
        # Details in dict
        return data_result.apply(
            lambda x: {
                "Action": x.Action,
                "ThreatType": x.ThreatType,
                "ThreatSeverity": x.ThreatSeverity,
                "Active": x.Active,
                "Description": x.Description,
                "ConfidenceScore": x.ConfidenceScore,
            },
            axis=1,
        )

    @staticmethod
    def _get_severity(data_result: pd.DataFrame) -> pd.Series:
        # For the input frame return severity in a series
        return data_result.apply(
            lambda x: ResultSeverity.high.name
            if x.Action.lower() in ["alert", "block"]
            else ResultSeverity.warning.name,
            axis=1,
        )
