# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# Author: Thomas Roccia - @fr0gger_
# --------------------------------------------------------------------------
"""Pulsedive TI Provider."""
from __future__ import annotations

from enum import Enum
from time import sleep
from typing import TYPE_CHECKING, Any, ClassVar, NamedTuple

import httpx
import pandas as pd
from pandas import json_normalize
from typing_extensions import Self

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ...common.provider_settings import get_provider_settings
from ...common.utility import mp_ua_header
from ..http_provider import APILookupParams
from .result_severity import LookupResult, ResultSeverity
from .ti_http_provider import HttpTIProvider

if TYPE_CHECKING:
    from ...common.provider_settings import ProviderSettings

__version__ = VERSION
__author__ = "Thomas Roccia | @fr0gger_"

_BASE_URL = "https://pulsedive.com/api/"

_QUERY_OBJECTS_MAPPINGS: dict[str, dict[str, str]] = {
    "indicator": {"indicator": "observable"},
    "threat": {"threat": "observable"},
    "explore": {"q": "query"},
    "scan": {"scan": "observable"},
}


class PDQuery(NamedTuple):
    """Query object for Pulsedive queries."""

    query_type: str
    data: str


class PDEntityType(Enum):
    """
    Enum class for Pulsedive entity types.

    This class defines the different types of entities that can be queried through
    the Pulsedive API. The available options are:

    INDICATOR: Queries information about a specific indicator of compromise (IOC)
    THREAT: Queries information about a specific threat
    EXPLORE: Executes a general search across the Pulsedive data set
    SCAN: Submits an indicator of compromise (IOC) for scanning

    """

    INDICATOR = "indicator"
    THREAT = "threat"
    EXPLORE = "explore"
    SCAN = "scan"


class PDlookup:
    """
    PDlookup: A class to interact with the Pulsedive API.

    This class allows you to look up indicators of compromise (IOCs) and
    explore different types of data available in the Pulsedive API.

    """

    _SUPPORTED_PD_TYPES: ClassVar[set[PDEntityType]] = {
        PDEntityType.INDICATOR,
        PDEntityType.THREAT,
        PDEntityType.EXPLORE,
        PDEntityType.SCAN,
    }

    def __init__(self: PDlookup, pd_key: str | None = None) -> None:
        """
        Init function to get the API key if necessary.

        Parameters
        ----------
        pd_key: str
            An API key for the Pulsedive API.

        """
        self.pd_key: str | None = pd_key or self._get_pd_api_key()

    def lookup_ioc(
        self: Self,
        observable: str,
        pd_type: str = "indicator",
    ) -> pd.DataFrame:
        """
        Lookup an indicator of compromise (IOC) in the Pulsedive API.

        Parameters
        ----------
        observable : str
            The IOC to lookup.
        pd_type : str
            The lookup type to perform, default is "indicator".

        Returns
        -------
        pd.DataFrame
            A Pandas DataFrame with the API's response.

        Notes
        -----
        In the case of `pd_type="explore"`, the `observable`
        value should be a pulsedive query string.
        See https://pulsedive.com/explore/

        See Also
        --------
        PDlookup.lookup_threat
        PDlookup.explore
        PDlookup.scan

        """
        if pd_type == PDEntityType.THREAT.value:
            return self.lookup_threat(observable=observable)
        if pd_type == PDEntityType.EXPLORE.value:
            return self.explore(query=observable)
        if pd_type == PDEntityType.SCAN.value:
            return self.scan(observable=observable)
        return self._make_pd_request(
            _build_query_string(observable, pd_type=PDEntityType.INDICATOR.value),
        )

    def lookup_threat(self: Self, observable: str) -> pd.DataFrame:
        """
        Lookup a Threat name in the Pulsedive API.

        Parameters
        ----------
        observable : str
            The IOC to lookup.

        Returns
        -------
        pd.DataFrame
            A Pandas DataFrame with the API's response.

        """
        return self._make_pd_request(
            _build_query_string(observable, pd_type=PDEntityType.THREAT.value),
        )

    def explore(self: Self, query: str) -> pd.DataFrame:
        """
        Perform a search query on the Pulsedive API.

        Parameters
        ----------
        query : str
            The query to perform.

        Returns
        -------
        pd.DataFrame
            A Pandas DataFrame with the API's response.

        """
        return self._make_pd_request(
            _build_query_string(query, pd_type=PDEntityType.EXPLORE.value),
        )

    def scan(self: Self, observable: str) -> pd.DataFrame:
        """
        Scan an observable in the Pulsedive API.

        Parameters
        ----------
        observable : str
            The observable to scan.

        Returns
        -------
        pd.DataFrame
            A Pandas DataFrame with the API's response.

        """
        return self._make_pd_request(
            _build_query_string(observable, pd_type=PDEntityType.SCAN.value),
        )

    def _check_valid_type(self: Self, pd_type: str) -> None:
        if pd_type not in _QUERY_OBJECTS_MAPPINGS:
            err_msg: str = (
                f"Invalid pd_type: {pd_type}."
                f"Should be one of: {', '.join(_QUERY_OBJECTS_MAPPINGS.keys())}"
            )
            raise ValueError(err_msg)

    def _make_pd_request(self: Self, pd_query: PDQuery) -> pd.DataFrame:
        """
        Make a request to the PulseDive API with the provided data.

        Parameters
        ----------
        pd_query : PDQuery
            Request data

        Returns
        -------
        pd.DataFrame
            Dataframe containing the results of the API call

        """
        if pd_query.query_type == "scan":
            # if the key is "scan", make a POST request to the analyze endpoint
            data: dict[str, Any] = {
                "value": pd_query.data,
                "probe": 1,
                **self._get_default_params(),
            }
            headers: dict[str, str] = {
                "Content-Type": "application/x-www-form-urlencoded",
                **mp_ua_header(),
            }
            resp: httpx.Response = httpx.post(
                f"{_BASE_URL}analyze.php",
                data=data,
                headers=headers,
            )
            if not resp.is_success:
                err_msg: str = (
                    f"API request failed with status code {resp.status_code}"
                    f" and message {resp.text}"
                )
                raise ValueError(err_msg)
            return self._poll_for_results(resp)

        if pd_query.query_type == "q":
            # if the key is "q", make a GET request to the explore endpoint
            query_url: str = f"{_BASE_URL}explore.php"
        else:
            query_url = f"{_BASE_URL}info.php"
        params: dict[str, Any] = {
            **self._get_default_params(),
            pd_query.query_type: pd_query.data,
        }
        resp = httpx.get(
            query_url,
            params=params,
            headers=mp_ua_header(),
            timeout=get_http_timeout(),
        )
        if not resp.is_success:
            err_msg = (
                f"API request failed with status code {resp.status_code}"
                f" and message {resp.text}"
            )
            raise ValueError(err_msg)
        json_resp: dict[str, Any] = resp.json()
        if pd_query.query_type == "q":
            json_resp = json_resp["results"]
        return json_normalize(json_resp)

    def _get_default_params(self: Self) -> dict[str, Any]:
        """Return default query parameters."""
        return {"pretty": 1, "key": self.pd_key}

    def _poll_for_results(self: Self, resp: httpx.Response) -> pd.DataFrame:
        """Poll API for results for this query ID."""
        # if the initial request is successful, get the qid and
        # check the status until it is 'success'
        qid = resp.json()["qid"]
        params: dict[str, Any] = {"qid": qid, **self._get_default_params()}
        headers: dict[str, str] = mp_ua_header()
        timeout: httpx.Timeout = get_http_timeout()
        url: str = f"{_BASE_URL}analyze.php"
        resp = httpx.get(
            url,
            params=params,
            headers=headers,
            timeout=timeout,
        )
        status: str = resp.json()["status"]
        while status == "processing":
            sleep(0.5)  # wait a little to avoid hammering UI
            resp = httpx.get(url, params=params, headers=headers, timeout=timeout)
            status = resp.json()["status"]

        return json_normalize(resp.json()["data"])

    def _get_pd_api_key(self: Self) -> str | None:
        """Return Pulsedive api key from configuration."""
        pd_settings: ProviderSettings | None = get_provider_settings("TIProviders").get(
            "Pulsedive",
        )
        if pd_settings and "AuthKey" in pd_settings.args:
            return pd_settings.args["AuthKey"]
        return None


def _build_query_string(data: str, pd_type: str) -> PDQuery:
    """
    Build query string for the API request based on the provided data and pd_type.

    Parameters
    ----------
    data : Any
        Data to be mapped to query_item
    pd_type : str
        type of data to map

    Returns
    -------
    Dict[str, Any]
        Query items in dictionary

    """
    query_items = _QUERY_OBJECTS_MAPPINGS[pd_type]

    # check if the provided data is empty
    if not data:
        err_msg: str = "data is empty."
        raise ValueError(err_msg)

    return PDQuery(next(iter(query_items.keys())), data)


# -------------------------------
# MSTICPY Standard TI Provider

_QUERY_DEF = APILookupParams(
    path="info.php",
    params={"indicator": "{observable}", "pretty": 1, "key": "{API_KEY}"},
)


class Pulsedive(HttpTIProvider):
    """Pulsedive TI Lookup."""

    _BASE_URL = _BASE_URL

    _QUERIES: ClassVar[dict[str, APILookupParams]] = {
        ioc_type: _QUERY_DEF for ioc_type in ("ipv4", "ipv6", "dns", "hostname", "url")
    }

    _REQUIRED_PARAMS: ClassVar[list[str]] = ["API_KEY"]
    _RISK_MAP: ClassVar[dict[str, ResultSeverity]] = {
        "high": ResultSeverity.high,
        "medium": ResultSeverity.warning,
        "low": ResultSeverity.information,
        "none": ResultSeverity.unknown,
    }

    def __init__(
        self: Pulsedive,
        *,
        timeout: int | None = None,
        ApiID: str | None = None,  # noqa: N803
        AuthKey: str | None = None,  # noqa: N803
        Instance: str | None = None,  # noqa: N803
    ) -> None:
        """Set OTX specific settings."""
        super().__init__(
            timeout=timeout,
            ApiID=ApiID,
            AuthKey=AuthKey,
            Instance=Instance,
        )
        self.require_url_encoding = True

    def parse_results(self: Self, response: dict) -> LookupResult:
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
            response["RawResult"],
            dict,
        ):
            return LookupResult(
                status=False,
                severity=ResultSeverity.information,
                details={"status": "Not found."},
            )
        severity: ResultSeverity = self._RISK_MAP[response.get("risk", "none")]

        details: dict[str, Any] = {
            "indicator_id": response.get("iid"),
            "threats": response.get("threats", []),
            "last_seen": response.get("stamp_seen", ""),
            "risk_factors": response.get("riskfactors", {}),
            "comments": response.get("comments", {}),
        }

        return LookupResult(status=True, severity=severity, details=details)
