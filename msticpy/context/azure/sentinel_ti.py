# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Analytics Features."""
from datetime import datetime
from typing import Optional

import httpx
import pandas as pd
from azure.common.exceptions import CloudError

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import _azs_api_result_to_df, _build_sent_data, get_http_timeout

__version__ = VERSION
__author__ = "Pete Bryan"


_INDICATOR_ITEMS = {
    "valid_to": "validUntil",
    "description": "description",
    "threat_types": "threatTypes",
    "name": "displayName",
    "confidence": "confidence",
    "revoked": "revoked",
    "source": "source",
}

_IOC_TYPE_MAPPING = {
    "dns": "domain-name",
    "url": "url",
    "ipv4": "ipv4-addr",
    "ipv6": "ipv6-addr",
    "md5_hash": "MD5",
    "sha1_hash": "SHA-1",
    "sha256_hash": "SHA-256",
}


class SentinelTIMixin:
    """Mixin class for Sentinel Hunting feature integrations."""

    def get_all_indicators(
        self,
        limit: Optional[int] = None,
        orderby: Optional[str] = None,
    ) -> pd.DataFrame:
        """
        Return all TI indicators in a Microsoft Sentinel workspace.

        Parameters
        ----------
        limit: int, optional
            If set returns top n results
        orderby : Optional[str], optional
            Order results by a specific column

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        appendix = "/indicators?"
        if limit:
            appendix += f"&$top={limit}"
        if orderby:
            appendix += f"&$orderby={orderby}"
        return self._list_items(  # type: ignore
            item_type="ti", api_version="2021-10-01", appendix=appendix
        )  # type: ignore

    def get_ti_metrics(self) -> pd.DataFrame:
        """
        Return metrics about TI indicators in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        return self._list_items(  # type: ignore
            item_type="ti", api_version="2021-10-01", appendix="/metrics"
        )  # type: ignore

    def create_indicator(
        self,
        indicator: str,
        ioc_type: str,
        name: str = "TI Indicator",
        confidence: int = 0,
        silent: bool = False,
        **kwargs,
    ) -> str:
        """
        Create a new indicator within the Microsoft Sentinel workspace.

        Parameters
        ----------
        indicator : str
            The indicator to create - i.e. IP address, domain name etc.
        ioc_type : str
            The type of indicator to create - can be:
            "dns", "url", "ipv4", "ipv6", "md5_hash", "sha1_hash", "sha256_hash"
        name : str, optional
            A common name to give to the indicator default is 'TI Indicator'
        confidence : int, optional
            A score between 0-100 of the confidence in the indicator, defualt is 0
        silent : bool, optional
            If True no output is displayed, defaults to False
        description : str, optional
            An description of the indicator
        labels : list, optional
            A list of string object labels to associate with the indicator
        kill_chain_phases : list, optional
            A list of string objects relating to the kill chain phases
            an indicator is assocaited with
        threat_types : list, optional
            A list of threat types associated with the indicator (list of string objects)
        external_references : list, optional
            A list of URLs that provide an external reference for the indicator
        valid_from : datetime, optional
            A datetime from which the indicator is valid from, defaults to now
        valid_to : datetime, optional
            A datetime to which the indicator is valid until

        Returns
        -------
        The ID of the created indicator

        Raises
        ------
        MsticpyUserError
            If invalid ioc_type or confidence value provided
        CloudError
            If API call fails

        """
        self.check_connected()  # type: ignore
        ti_url = self.sent_urls["ti"] + "/createIndicator"  # type: ignore
        params = {"api-version": "2021-10-01"}
        if ioc_type not in _IOC_TYPE_MAPPING:
            raise MsticpyUserError(
                """ioc_type must be one of -
                 'dns', 'url', 'ipv4', 'ipv6', 'md5_hash', 'sha1_hash', 'sha256_hash'"""
            )
        normalized_ioc_type = _IOC_TYPE_MAPPING[ioc_type]
        pattern_type = normalized_ioc_type
        value = "value"
        if normalized_ioc_type in ["SHA-256", "SHA-1", "MD5"]:
            pattern_type = "file"
            value = f"hashes.'{normalized_ioc_type}'"

        if confidence > 100 or confidence < 0:
            raise MsticpyUserError("confidence must be between 0 and 100")
        data_items = {
            "displayName": name,
            "confidence": confidence,
            "pattern": f"[{pattern_type}:{value} = '{indicator}']",
            "patternType": pattern_type,
            "revoked": "false",
            "source": "MSTICPy",
        }
        data_items.update(_build_additional_indicator_items(**kwargs))
        data = _build_sent_data(data_items, props=True)
        data["kind"] = "indicator"
        response = httpx.post(
            ti_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        if not silent:
            print("Indicator created.")

        return response.json().get("name")

    def bulk_create_indicators(
        self,
        data: pd.DataFrame,
        indicator_column: str = "Observable",
        indicator_type_column: str = "IoCType",
        **kwargs,
    ):
        """
        Bulk create indicators from a DataFrame.

        Parameters
        ----------
        data : pd.DataFrame
            A dataframe containing indicators and indicator types
        indicator_column : str, optional
            The column containing indicator values to create, by default "Observable"
        indicator_type_column : str, optional
            The column containing indicator type values, by default "IoCType"
        confidence_column : str, optional
            The column containing indicator confidence values, by default 0 value used.

        """
        for row in data.iterrows():
            confidence = (
                row[1][kwargs["confidence_column"]]
                if "confidence_column" in kwargs
                else 0
            )
            try:
                self.create_indicator(
                    indicator=row[1][indicator_column],
                    ioc_type=row[1][indicator_type_column],
                    confidence=confidence,
                    silent=True,
                )
            except CloudError:
                print(f"Error creating indicator {row[1][indicator_column]}")
        print(f"{len(data.index)} indicators created.")

    def get_indicator(self, indicator_id: str) -> dict:
        """
        Get a specific indicator by its ID.

        Parameters
        ----------
        indicator_id : str
            The GUID of the indicator to get

        Returns
        -------
        dict
            Indicator details

        Raises
        ------
        CloudError
            If API call fails.

        """
        self.check_connected()  # type: ignore
        ti_url = self.sent_urls["ti"] + f"/indicators/{indicator_id}"  # type: ignore
        params = {"api-version": "2021-10-01"}
        response = httpx.get(
            ti_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
        )
        if response.status_code != 200:
            raise CloudError(response=response)
        return response.json()

    def update_indicator(self, indicator_id: str, **kwargs):
        """
        Update an existing indicator within the Microsoft Sentinel workspace.

        Parameters
        ----------
        indicator_id : str
            The GUID of the indicator to update
        name : str, optional
            A common name to give to the indicator default is 'TI Indicator'
        confidence : int, optional
            A score between 0-100 of the confidence in the indicator
        description : str, optional
            An description of the indicator
        labels : list, optional
            A list of string object labels to associate with
            the indicator
        kill_chain_phases : list, optional
            A list of string objects relating to the kill chain phases
            an indicator is assocaited with
        threat_types : list, optional
            A list of threat types associated with the indicator (list of string objects)
        external_references : list, optional
            A list of URLs that provide an external reference for the indicator
        valid_from : datetime, optional
            A datetime from which the indicator is valid from, defaults to now
        valid_to : datetime, optional
            A datetime to which the indicator is valid until

        Raises
        ------
        CloudError
            If API call fails

        """
        self.check_connected()  # type: ignore
        ti_url = self.sent_urls["ti"] + f"/indicators/{indicator_id}"  # type: ignore
        indicator_details = self.get_indicator(indicator_id)
        data_items = _build_additional_indicator_items(**kwargs)
        data_items.pop("validFrom")
        full_data_items = _add_missing_items(data_items, indicator_details)
        data = _build_sent_data(full_data_items, props=True)
        data["etag"] = indicator_details["etag"]
        data["kind"] = "indicator"
        params = {"api-version": "2021-10-01"}
        response = httpx.put(
            ti_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        print("Indicator updated.")

    def add_tag(self, indicator_id: str, tag: str):
        """
        Add a tag to an existing indicator.

        Parameters
        ----------
        indicator_id : str
            The GUID of the indicator to add a tag to.
        tag : str
            The tag to add.

        """
        self.check_connected()  # type: ignore
        indicator_details = self.get_indicator(indicator_id)
        tags = [tag]
        if "threatIntelligenceTags" in indicator_details["properties"]:
            tags += indicator_details["properties"]["threatIntelligenceTags"]
        self.update_indicator(indicator_id=indicator_id, labels=tags)

    def delete_indicator(self, indicator_id: str):
        """
        Delete a specific TI indicator.

        Parameters
        ----------
        indicator_id : str
            The GUID of the indicator to delete

        Raises
        ------
        CloudError
            If API call fails

        """
        self.check_connected()  # type: ignore
        ti_url = self.sent_urls["ti"] + f"/indicators/{indicator_id}"  # type: ignore
        params = {"api-version": "2021-10-01"}
        response = httpx.delete(
            ti_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 204):
            raise CloudError(response=response)
        print("Indicator deleted.")

    def query_indicators(self, **kwargs) -> pd.DataFrame:
        """
        Query for indicators in a Sentinel workspace.

        Parameters
        ----------
        includeDisabled : bool, optional
            Parameter to include/exclude disabled indicators.
        keywords : str, optional
            Keyword for searching threat intelligence indicators
            Use this to search for specific indicator values.
        maxConfidence : int, optional
            Maximum confidence.
        maxValidUntil : str, optional
            End time for ValidUntil filter.
        minConfidence : int, optional
            Minimum confidence.
        minValidUntil : str, optional
            Start time for ValidUntil filter.
        pageSize : int, optional
            Maximum number of results to return in one page.
        patternTypes : list, optional
            A list of IoC types to include.
        sortBy : List, optional
            Columns to sort by and sorting order as:
            [{"itemKey": COLUMN_NAME, "sortOrder": ascending/descending}]
        sources: list, optional
            A list of indicator sources to include
        threatTypes: list, optional
            A list of Threat types to include

        Returns
        -------
        pd.DataFrame
            A set of matching indicators

        Raises
        ------
        CloudError
            If API call fails

        """
        self.check_connected()  # type: ignore
        ti_url = self.sent_urls["ti"] + "/queryIndicators"  # type: ignore
        data_items = dict(kwargs)
        params = {"api-version": "2021-10-01"}
        response = httpx.post(
            ti_url,
            headers=get_api_headers(self.token),  # type: ignore
            params=params,
            content=str(data_items),
            timeout=get_http_timeout(),
        )
        if response.status_code != 200:
            raise CloudError(response=response)
        return _azs_api_result_to_df(response)


def _build_additional_indicator_items(**kwargs) -> dict:
    """Add in additional data items for indicators."""
    data_items = {
        "validFrom": kwargs["valid_from"].isoformat()
        if "valid_from" in kwargs
        else datetime.now().isoformat()
    }
    for item, value in kwargs.items():
        if item in _INDICATOR_ITEMS:
            data_items[_INDICATOR_ITEMS[item]] = value
    if "valid_to" in kwargs:
        data_items["validUntil"] = kwargs["valid_to"].isoformat()
    else:
        data_items["validUntil"] = datetime.now().isoformat()
    if "external_references" in kwargs:
        ext_refs = [
            {"sourceName": "MSTICPy", "url": ref}
            for ref in kwargs["external_references"]
        ]
        data_items["externalReferences"] = ext_refs
    if "kill_chain_phases" in kwargs:
        kill_chain = [
            {
                "killChainName": "Lockheed Martin - Intrusion Kill Chain",
                "phaseName": phase,
            }
            for phase in kwargs["kill_chain_phases"]
        ]
        data_items["killChainPhases"] = kill_chain
    if "labels" in kwargs:
        data_items["labels"] = kwargs["labels"]
        data_items["threatIntelligenceTags"] = kwargs["labels"]
    return data_items


_REQUIRED_ITEMS = ["pattern", "patternType", "source"]


def _add_missing_items(data_items, indicator) -> dict:
    """Add missing required items to requests based on existing values."""
    for req_item in _REQUIRED_ITEMS:
        if req_item not in data_items:
            data_items[req_item] = indicator["properties"][req_item]
    return data_items
