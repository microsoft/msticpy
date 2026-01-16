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

from __future__ import annotations

from typing import Any, ClassVar

import pandas as pd
from typing_extensions import Self

from ..._version import VERSION
from ...common.utility import export
from .kql_base import KqlTIProvider
from .ti_provider_base import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


@export
class AzSTI(KqlTIProvider):
    """Microsoft Sentinel TI provider class."""

    _QUERIES: ClassVar[dict[str, tuple]] = {
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

    _REQUIRED_TABLES: ClassVar[list[str]] = ["ThreatIntelIndicators"]

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
        if response["RawResult"] is None:
            return False, ResultSeverity.information, "No data"

        severity = ResultSeverity.warning
        # if this is a series (single row) return a dictionary
        if isinstance(response["RawResult"], pd.Series):
            result_series = response["RawResult"]
            # Extract fields from the new schema
            extracted_data: dict[str, Any] = {}
            
            # Map new fields to old field names for compatibility
            extracted_data["ConfidenceScore"] = result_series.get("Confidence", 0)
            extracted_data["Active"] = result_series.get("IsActive", False)
            
            # Extract data from Data column (STIX format) if available
            if "Data" in result_series and pd.notna(result_series["Data"]):
                stix_data = result_series["Data"]
                if isinstance(stix_data, dict):
                    # Extract labels as ThreatType
                    labels = stix_data.get("labels", [])
                    extracted_data["ThreatType"] = labels[0] if labels else "unknown"
                    extracted_data["Description"] = stix_data.get("description", "")
                else:
                    extracted_data["ThreatType"] = "unknown"
                    extracted_data["Description"] = ""
            else:
                extracted_data["ThreatType"] = "unknown"
                extracted_data["Description"] = ""
            
            # Set default values for fields not in new schema
            extracted_data["Action"] = "alert"  # Default action
            extracted_data["ThreatSeverity"] = "unknown"
            
            # Determine severity based on confidence or labels
            if extracted_data["ConfidenceScore"] >= 80:
                severity = ResultSeverity.high
            
            return True, severity, extracted_data
        
        # if this is a dataframe (multiple rows)
        # concatenate the values for each column/record into a list
        # and return as a dictionary
        if isinstance(response["RawResult"], pd.DataFrame):
            d_frame: pd.DataFrame = response["RawResult"]
            
            # Map new fields to old field names for compatibility
            if "Confidence" in d_frame.columns:
                d_frame["ConfidenceScore"] = d_frame["Confidence"]
            if "IsActive" in d_frame.columns:
                d_frame["Active"] = d_frame["IsActive"]
            
            # Extract data from Data column if available
            if "Data" in d_frame.columns:
                # Extract ThreatType from labels in Data
                d_frame["ThreatType"] = d_frame["Data"].apply(
                    lambda x: x.get("labels", ["unknown"])[0] if isinstance(x, dict) and x.get("labels") else "unknown"
                )
                d_frame["Description"] = d_frame["Data"].apply(
                    lambda x: x.get("description", "") if isinstance(x, dict) else ""
                )
            else:
                d_frame["ThreatType"] = "unknown"
                d_frame["Description"] = ""
            
            # Set default values for fields not in new schema
            d_frame["Action"] = "alert"
            d_frame["ThreatSeverity"] = "unknown"
            
            # Determine severity based on confidence
            if "ConfidenceScore" in d_frame.columns and (d_frame["ConfidenceScore"] >= 80).any():
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
        # Map new schema fields to old field names
        if "Confidence" in data_result.columns:
            data_result["ConfidenceScore"] = data_result["Confidence"]
        if "IsActive" in data_result.columns:
            data_result["Active"] = data_result["IsActive"]
        
        # Extract from Data column if available
        if "Data" in data_result.columns:
            data_result["ThreatType"] = data_result["Data"].apply(
                lambda x: x.get("labels", ["unknown"])[0] if isinstance(x, dict) and x.get("labels") else "unknown"
            )
            data_result["Description"] = data_result["Data"].apply(
                lambda x: x.get("description", "") if isinstance(x, dict) else ""
            )
        else:
            data_result["ThreatType"] = "unknown"
            data_result["Description"] = ""
        
        # Set defaults for missing fields
        if "Action" not in data_result.columns:
            data_result["Action"] = "alert"
        if "ThreatSeverity" not in data_result.columns:
            data_result["ThreatSeverity"] = "unknown"
        
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
        # Map Confidence to ConfidenceScore if needed
        if "Confidence" in data_result.columns and "ConfidenceScore" not in data_result.columns:
            data_result["ConfidenceScore"] = data_result["Confidence"]
        
        # Set default Action if not present
        if "Action" not in data_result.columns:
            data_result["Action"] = "alert"
        
        return data_result.apply(
            lambda x: (
                ResultSeverity.high.name
                if (hasattr(x, "Action") and x.Action.lower() in ["alert", "block"]) 
                   or (hasattr(x, "ConfidenceScore") and x.ConfidenceScore >= 80)
                else ResultSeverity.warning.name
            ),
            axis=1,
        )
