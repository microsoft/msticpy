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

    # Confidence threshold for high severity classification
    _HIGH_CONFIDENCE_THRESHOLD: ClassVar[int] = 80

    def parse_results(  # noqa: PLR0912, PLR0915  # pylint: disable=too-many-branches
        self: Self, response: dict
    ) -> tuple[bool, ResultSeverity, Any]:
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
            extracted_data: dict[str, Any] = {}

            # Try to use old schema fields first (for backward compatibility)
            if "ThreatType" in result_series.index:
                extracted_data["ThreatType"] = result_series["ThreatType"]
            elif "Data" in result_series.index and pd.notna(result_series["Data"]):
                # Extract from new schema Data column (STIX format)
                stix_data = result_series["Data"]
                if isinstance(stix_data, dict):
                    labels = stix_data.get("labels", [])
                    extracted_data["ThreatType"] = labels[0] if labels else "unknown"
                else:
                    extracted_data["ThreatType"] = "unknown"
            else:
                extracted_data["ThreatType"] = "unknown"

            # Handle Description field
            if "Description" in result_series.index:
                extracted_data["Description"] = result_series["Description"]
            elif "Data" in result_series.index and pd.notna(result_series["Data"]):
                stix_data = result_series["Data"]
                if isinstance(stix_data, dict):
                    extracted_data["Description"] = stix_data.get("description", "")
                else:
                    extracted_data["Description"] = ""
            else:
                extracted_data["Description"] = ""

            # Handle ConfidenceScore field (old schema) or Confidence (new schema)
            if "ConfidenceScore" in result_series.index:
                extracted_data["ConfidenceScore"] = result_series["ConfidenceScore"]
            elif "Confidence" in result_series.index:
                extracted_data["ConfidenceScore"] = result_series["Confidence"]
            else:
                extracted_data["ConfidenceScore"] = 0

            # Handle Active field (old schema) or IsActive (new schema)
            if "Active" in result_series.index:
                extracted_data["Active"] = result_series["Active"]
            elif "IsActive" in result_series.index:
                extracted_data["Active"] = result_series["IsActive"]
            else:
                extracted_data["Active"] = False

            # Handle Action field (old schema only, default for new schema)
            if "Action" in result_series.index:
                extracted_data["Action"] = result_series["Action"]
            else:
                extracted_data["Action"] = "alert"

            # Handle ThreatSeverity field (old schema only)
            if "ThreatSeverity" in result_series.index:
                extracted_data["ThreatSeverity"] = result_series["ThreatSeverity"]
            else:
                extracted_data["ThreatSeverity"] = "unknown"

            # Determine severity
            if extracted_data["Action"].lower() in ["alert", "block"]:
                severity = ResultSeverity.high
            elif extracted_data["ConfidenceScore"] >= self._HIGH_CONFIDENCE_THRESHOLD:
                severity = ResultSeverity.high

            return True, severity, extracted_data

        # if this is a dataframe (multiple rows)
        # concatenate the values for each column/record into a list
        # and return as a dictionary
        if isinstance(response["RawResult"], pd.DataFrame):
            d_frame: pd.DataFrame = response["RawResult"].copy()

            # Handle ThreatType field
            if "ThreatType" not in d_frame.columns:
                if "Data" in d_frame.columns:
                    d_frame["ThreatType"] = d_frame["Data"].apply(
                        lambda x: x.get("labels", ["unknown"])[0]
                        if isinstance(x, dict) and x.get("labels")
                        else "unknown"
                    )
                else:
                    d_frame["ThreatType"] = "unknown"

            # Handle Description field
            if "Description" not in d_frame.columns:
                if "Data" in d_frame.columns:
                    d_frame["Description"] = d_frame["Data"].apply(
                        lambda x: x.get("description", "") if isinstance(x, dict) else ""
                    )
                else:
                    d_frame["Description"] = ""

            # Handle ConfidenceScore field
            if "ConfidenceScore" not in d_frame.columns and "Confidence" in d_frame.columns:
                d_frame["ConfidenceScore"] = d_frame["Confidence"]
            elif "ConfidenceScore" not in d_frame.columns:
                d_frame["ConfidenceScore"] = 0

            # Handle Active field
            if "Active" not in d_frame.columns and "IsActive" in d_frame.columns:
                d_frame["Active"] = d_frame["IsActive"]
            elif "Active" not in d_frame.columns:
                d_frame["Active"] = False

            # Handle Action field
            if "Action" not in d_frame.columns:
                d_frame["Action"] = "alert"

            # Handle ThreatSeverity field
            if "ThreatSeverity" not in d_frame.columns:
                d_frame["ThreatSeverity"] = "unknown"

            # Determine severity
            if d_frame["Action"].str.lower().isin(["alert", "block"]).any():
                severity = ResultSeverity.high
            elif (
                "ConfidenceScore" in d_frame.columns
                and (d_frame["ConfidenceScore"] >= self._HIGH_CONFIDENCE_THRESHOLD).any()
            ):
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
        # Handle both old and new schema fields
        data_result = data_result.copy()

        # Handle ThreatType field
        if "ThreatType" not in data_result.columns and "Data" in data_result.columns:
            data_result["ThreatType"] = data_result["Data"].apply(
                lambda x: x.get("labels", ["unknown"])[0]
                if isinstance(x, dict) and x.get("labels")
                else "unknown"
            )
        elif "ThreatType" not in data_result.columns:
            data_result["ThreatType"] = "unknown"

        # Handle Description field
        if "Description" not in data_result.columns and "Data" in data_result.columns:
            data_result["Description"] = data_result["Data"].apply(
                lambda x: x.get("description", "") if isinstance(x, dict) else ""
            )
        elif "Description" not in data_result.columns:
            data_result["Description"] = ""

        # Handle ConfidenceScore field
        if (
            "ConfidenceScore" not in data_result.columns
            and "Confidence" in data_result.columns
        ):
            data_result["ConfidenceScore"] = data_result["Confidence"]
        elif "ConfidenceScore" not in data_result.columns:
            data_result["ConfidenceScore"] = 0

        # Handle Active field
        if "Active" not in data_result.columns and "IsActive" in data_result.columns:
            data_result["Active"] = data_result["IsActive"]
        elif "Active" not in data_result.columns:
            data_result["Active"] = False

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
        data_result = data_result.copy()

        # Map Confidence to ConfidenceScore if needed
        if (
            "Confidence" in data_result.columns
            and "ConfidenceScore" not in data_result.columns
        ):
            data_result["ConfidenceScore"] = data_result["Confidence"]
        elif "ConfidenceScore" not in data_result.columns:
            data_result["ConfidenceScore"] = 0

        # Set default Action if not present
        if "Action" not in data_result.columns:
            data_result["Action"] = "alert"

        # Use class constant for threshold
        high_threshold = AzSTI._HIGH_CONFIDENCE_THRESHOLD

        return data_result.apply(
            lambda x: (
                ResultSeverity.high.name
                if (hasattr(x, "Action") and x.Action.lower() in ["alert", "block"])
                or (hasattr(x, "ConfidenceScore") and x.ConfidenceScore >= high_threshold)
                else ResultSeverity.warning.name
            ),
            axis=1,
        )
