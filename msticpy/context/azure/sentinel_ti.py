# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mixin Classes for Sentinel Analytics Features."""
from __future__ import annotations

import datetime as dt
import logging
from typing import TYPE_CHECKING, Any

import httpx
from azure.common.exceptions import CloudError
from typing_extensions import Self

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from .azure_data import get_api_headers
from .sentinel_utils import (
    SentinelUtilsMixin,
    _azs_api_result_to_df,
    extract_sentinel_response,
    get_http_timeout,
)

if TYPE_CHECKING:
    import pandas as pd

__version__ = VERSION
__author__ = "Pete Bryan"

logger: logging.Logger = logging.getLogger(__name__)
_INDICATOR_ITEMS: dict[str, str] = {
    "valid_to": "validUntil",
    "description": "description",
    "threat_types": "threatTypes",
    "name": "displayName",
    "confidence": "confidence",
    "revoked": "revoked",
    "source": "source",
}

_IOC_TYPE_MAPPING: dict[str, str] = {
    "dns": "domain-name",
    "url": "url",
    "ipv4": "ipv4-addr",
    "ipv6": "ipv6-addr",
    "md5_hash": "MD5",
    "sha1_hash": "SHA-1",
    "sha256_hash": "SHA-256",
}

MAX_CONFIDENCE: int = 100


class SentinelTIMixin(SentinelUtilsMixin):
    """Mixin class for Sentinel Hunting feature integrations."""

    def get_all_indicators(
        self: Self,
        limit: int | None = None,
        orderby: str | None = None,
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
        return self._list_items(
            item_type="ti",
            api_version="2021-10-01",
            appendix=appendix,
        )

    def get_ti_metrics(self: Self) -> pd.DataFrame:
        """
        Return metrics about TI indicators in a Microsoft Sentinel workspace.

        Returns
        -------
        pd.DataFrame
            A table of the custom hunting queries.

        """
        return self._list_items(
            item_type="ti",
            api_version="2021-10-01",
            appendix="/metrics",
        )

    def create_indicator(  # pylint:disable=too-many-arguments, too-many-locals #noqa:PLR0913
        self: Self,
        indicator: str,
        ioc_type: str,
        name: str = "TI Indicator",
        confidence: int = 0,
        *,
        silent: bool = False,
        description: str | None = None,
        labels: list | None = None,
        kill_chain_phases: list | None = None,
        threat_types: list | None = None,
        external_references: list | None = None,
        valid_from: dt.datetime | None = None,
        valid_to: dt.datetime | None = None,
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
        self.check_connected()
        ti_url: str = self.sent_urls["ti"] + "/createIndicator"
        params: dict[str, str] = {"api-version": "2021-10-01"}
        if ioc_type not in _IOC_TYPE_MAPPING:
            err_msg: str = """ioc_type must be one of -
                 'dns', 'url', 'ipv4', 'ipv6', 'md5_hash', 'sha1_hash', 'sha256_hash'"""
            raise MsticpyUserError(err_msg)
        normalized_ioc_type: str = _IOC_TYPE_MAPPING[ioc_type]
        pattern_type: str = normalized_ioc_type
        value: str = "value"
        if normalized_ioc_type in ["SHA-256", "SHA-1", "MD5"]:
            pattern_type = "file"
            value = f"hashes.'{normalized_ioc_type}'"

        if confidence > MAX_CONFIDENCE or confidence < 0:
            err_msg = "confidence must be between 0 and 100"
            raise MsticpyUserError(err_msg)
        data_items: dict[str, Any] = {
            "displayName": name,
            "confidence": confidence,
            "pattern": f"[{pattern_type}:{value} = '{indicator}']",
            "patternType": pattern_type,
            "revoked": "false",
            "source": "MSTICPy",
        }
        data_items.update(
            _build_additional_indicator_items(
                valid_from=valid_from,
                valid_to=valid_to,
                external_references=external_references,
                kill_chain_phases=kill_chain_phases,
                labels=labels,
                name=name,
                threat_types=threat_types,
                confidence=confidence,
                description=description,
            ),
        )
        data: dict[str, Any] = extract_sentinel_response(data_items, props=True)
        data["kind"] = "indicator"
        if not self._token:
            err_msg = "Token not found, can't create indicator."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.post(
            ti_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        if not silent:
            logger.info("Indicator created.")

        return response.json().get("name")

    def bulk_create_indicators(
        self: Self,
        data: pd.DataFrame,
        indicator_column: str = "Observable",
        indicator_type_column: str = "IoCType",
        *,
        confidence_column: str | None = None,
    ) -> None:
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
            confidence: int = row[1][confidence_column] if confidence_column else 0
            try:
                self.create_indicator(
                    indicator=row[1][indicator_column],
                    ioc_type=row[1][indicator_type_column],
                    confidence=confidence,
                    silent=True,
                )
            except CloudError:
                logger.exception(
                    "Error creating indicator %s",
                    row[1][indicator_column],
                )
        logger.info("%s indicators created.", len(data.index))

    def get_indicator(self: Self, indicator_id: str) -> dict:
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
        self.check_connected()
        ti_url: str = self.sent_urls["ti"] + f"/indicators/{indicator_id}"
        params: dict[str, str] = {"api-version": "2021-10-01"}
        if not self._token:
            err_msg = "Token not found, can't get indicator."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.get(
            ti_url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )
        if not response.is_success:
            raise CloudError(response=response)
        return response.json()

    def update_indicator(  # pylint:disable=too-many-arguments,too-many-locals #noqa:PLR0913
        self: Self,
        indicator_id: str,
        *,
        name: str | None = None,
        confidence: int = 0,
        description: str | None = None,
        labels: list[str] | None = None,
        kill_chain_phases: list | None = None,
        threat_types: list | None = None,
        external_references: list | None = None,
        valid_from: dt.datetime | None = None,
        valid_to: dt.datetime | None = None,
    ) -> None:
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
        self.check_connected()
        ti_url: str = self.sent_urls["ti"] + f"/indicators/{indicator_id}"
        indicator_details: dict[str, Any] = self.get_indicator(indicator_id)
        data_items: dict[str, Any] = _build_additional_indicator_items(
            valid_from=valid_from,
            valid_to=valid_to,
            external_references=external_references,
            kill_chain_phases=kill_chain_phases,
            labels=labels,
            name=name,
            threat_types=threat_types,
            confidence=confidence,
            description=description,
        )
        data_items.pop("validFrom")
        full_data_items: dict[str, Any] = _add_missing_items(
            data_items,
            indicator_details,
        )
        data: dict[str, Any] = extract_sentinel_response(full_data_items, props=True)
        data["etag"] = indicator_details["etag"]
        data["kind"] = "indicator"
        params: dict[str, str] = {"api-version": "2021-10-01"}
        if not self._token:
            err_msg = "Token not found, can't update indicator."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.put(
            ti_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data),
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 201):
            raise CloudError(response=response)
        logger.info("Indicator updated.")

    def add_tag(self: Self, indicator_id: str, tag: str) -> None:
        """
        Add a tag to an existing indicator.

        Parameters
        ----------
        indicator_id : str
            The GUID of the indicator to add a tag to.
        tag : str
            The tag to add.

        """
        self.check_connected()
        indicator_details: dict[str, Any] = self.get_indicator(indicator_id)
        tags: list[str] = [tag]
        if "threatIntelligenceTags" in indicator_details["properties"]:
            tags += indicator_details["properties"]["threatIntelligenceTags"]
        self.update_indicator(indicator_id=indicator_id, labels=tags)

    def delete_indicator(self: Self, indicator_id: str) -> None:
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
        self.check_connected()
        ti_url: str = self.sent_urls["ti"] + f"/indicators/{indicator_id}"
        params: dict[str, str] = {"api-version": "2021-10-01"}
        if not self._token:
            err_msg = "Token not found, can't delete indicator."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.delete(
            ti_url,
            headers=get_api_headers(self._token),
            params=params,
            timeout=get_http_timeout(),
        )
        if response.status_code not in (200, 204):
            raise CloudError(response=response)
        logger.info("Indicator deleted.")

    def query_indicators(  # pylint:disable=too-many-arguments, too-many-locals #noqa:PLR0913
        self: Self,
        *,
        include_disabled: bool = False,
        keywords: str | None = None,
        min_confidence: int = 0,
        max_confidence: int = 100,
        max_valid_until: str | None = None,
        min_valid_until: str | None = None,
        page_size: int | None = None,
        pattern_types: list[str] | None = None,
        sort_by: list[str] | None = None,
        sources: list[str] | None = None,
        threat_types: list[str] | None = None,
    ) -> pd.DataFrame:
        """
        Query for indicators in a Sentinel workspace.

        Parameters
        ----------
        include_disabled : bool, optional
            Parameter to include/exclude disabled indicators.
        keywords : str, optional
            Keyword for searching threat intelligence indicators
            Use this to search for specific indicator values.
        max_confidence : int, optional
            Maximum confidence.
        max_valid_until : str, optional
            End time for ValidUntil filter.
        min_confidence : int, optional
            Minimum confidence.
        min_valid_until : str, optional
            Start time for ValidUntil filter.
        page_size : int, optional
            Maximum number of results to return in one page.
        pattern_types : list, optional
            A list of IoC types to include.
        sort_by : List, optional
            Columns to sort by and sorting order as:
            [{"itemKey": COLUMN_NAME, "sortOrder": ascending/descending}]
        sources: list, optional
            A list of indicator sources to include
        threat_types: list, optional
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
        self.check_connected()
        ti_url: str = self.sent_urls["ti"] + "/queryIndicators"
        data_items: dict[str, Any] = {
            "includeDisabled": include_disabled,
            "maxConfidence": max_confidence,
            "minConfidence": min_confidence,
        }
        if keywords:
            data_items["keywords"] = keywords
        if max_valid_until:
            data_items["maxValidUntil"] = max_valid_until
        if min_valid_until:
            data_items["minValidUntil"] = min_valid_until
        if page_size:
            data_items["pageSize"] = page_size
        if pattern_types:
            data_items["patternTypes"] = pattern_types
        if sort_by:
            data_items["sortBy"] = sort_by
        if sources:
            data_items["sources"] = sources
        if threat_types:
            data_items["threatTypes"] = threat_types
        params: dict[str, str] = {"api-version": "2021-10-01"}
        if not self._token:
            err_msg = "Token not found, can't query indicators."
            raise ValueError(err_msg)
        response: httpx.Response = httpx.post(
            ti_url,
            headers=get_api_headers(self._token),
            params=params,
            content=str(data_items),
            timeout=get_http_timeout(),
        )
        if not response.is_success:
            raise CloudError(response=response)
        return _azs_api_result_to_df(response)


def _build_additional_indicator_items(  # pylint:disable=too-many-arguments #noqa: PLR0913
    *,
    valid_from: dt.datetime | None = None,
    valid_to: dt.datetime | None = None,
    external_references: list[str] | None = None,
    kill_chain_phases: list[str] | None = None,
    labels: list[str] | None = None,
    name: str | None = None,
    confidence: int = 0,
    description: str | None = None,
    threat_types: list | None = None,
    revoked: bool | None = None,
    source: str | None = None,
) -> dict:
    """Add in additional data items for indicators."""
    data_items: dict[str, Any] = {
        "validFrom": (
            valid_from.isoformat()
            if valid_from
            else dt.datetime.now(tz=dt.timezone.utc).isoformat()
        ),
        "confidence": confidence,
    }
    if name:
        data_items["displayName"] = name
    if description:
        data_items["description"] = description
    if threat_types:
        data_items["threatTypes"] = threat_types
    if revoked:
        data_items["revoked"] = revoked
    if source:
        data_items["source"] = source
    if valid_to:
        data_items["validUntil"] = valid_to.isoformat()
    else:
        data_items["validUntil"] = dt.datetime.now(tz=dt.timezone.utc).isoformat()
    if external_references:
        ext_refs: list[dict[str, Any]] = [
            {"sourceName": "MSTICPy", "url": ref} for ref in external_references
        ]
        data_items["externalReferences"] = ext_refs
    if kill_chain_phases:
        kill_chain: list[dict[str, Any]] = [
            {
                "killChainName": "Lockheed Martin - Intrusion Kill Chain",
                "phaseName": phase,
            }
            for phase in kill_chain_phases
        ]
        data_items["killChainPhases"] = kill_chain
    if labels:
        data_items["labels"] = labels
        data_items["threatIntelligenceTags"] = labels
    return data_items


_REQUIRED_ITEMS: list[str] = ["pattern", "patternType", "source"]


def _add_missing_items(
    data_items: dict[str, Any],
    indicator: dict[str, Any],
) -> dict[str, Any]:
    """Add missing required items to requests based on existing values."""
    for req_item in _REQUIRED_ITEMS:
        if req_item not in data_items:
            data_items[req_item] = indicator["properties"][req_item]
    return data_items
