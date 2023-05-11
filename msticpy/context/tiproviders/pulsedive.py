# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# Author: Thomas Roccia - @fr0gger_
# --------------------------------------------------------------------------
"""Pulsedive TI Provider."""

from enum import Enum
from time import sleep
from typing import Dict, NamedTuple, Optional, Set

import httpx
import pandas as pd
from pandas import json_normalize

from ..._version import VERSION
from ...common.pkg_config import get_http_timeout
from ...common.provider_settings import get_provider_settings
from ...common.utility import mp_ua_header
from ..http_provider import APILookupParams
from .result_severity import LookupResult, ResultSeverity
from .ti_http_provider import HttpTIProvider

__version__ = VERSION
__author__ = "Thomas Roccia | @fr0gger_"

_BASE_URL = "https://pulsedive.com/api/"

_QUERY_OBJECTS_MAPPINGS = {
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

    _SUPPORTED_PD_TYPES: Set[PDEntityType] = {
        PDEntityType.INDICATOR,
        PDEntityType.THREAT,
        PDEntityType.EXPLORE,
        PDEntityType.SCAN,
    }

    def __init__(self, pd_key=None):
        """
        Init function to get the API key if necessary.

        Parameters
        ----------
        pd_key (str): An API key for the Pulsedive API.

        """
        self.pd_key = pd_key or self._get_pd_api_key()

    def lookup_ioc(self, observable: str, pd_type: str = "indicator") -> pd.DataFrame:
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
            _build_query_string(observable, pd_type=PDEntityType.INDICATOR.value)
        )

    def lookup_threat(self, observable: str) -> pd.DataFrame:
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
            _build_query_string(observable, pd_type=PDEntityType.THREAT.value)
        )

    def explore(self, query: str) -> pd.DataFrame:
        """Perform a search query on the Pulsedive API.

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
            _build_query_string(query, pd_type=PDEntityType.EXPLORE.value)
        )

    def scan(self, observable: str) -> pd.DataFrame:
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
            _build_query_string(observable, pd_type=PDEntityType.SCAN.value)
        )

    def _check_valid_type(self, pd_type):
        if pd_type not in _QUERY_OBJECTS_MAPPINGS:
            raise ValueError(
                f"Invalid pd_type: {pd_type}.",
                f"Should be one of: {', '.join(_QUERY_OBJECTS_MAPPINGS.keys())}",
            )

    def _make_pd_request(self, pd_query: PDQuery) -> pd.DataFrame:
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
            data = {"value": pd_query.data, "probe": 1, **self._get_default_params()}
            headers = {
                "Content-Type": "application/x-www-form-urlencoded",
                **mp_ua_header(),
            }
            resp = httpx.post(f"{_BASE_URL}analyze.php", data=data, headers=headers)
            if resp.status_code != 200:
                raise ValueError(
                    f"API request failed with status code {resp.status_code}",
                    f" and message {resp.text}",
                )
            return self._poll_for_results(resp)

        if pd_query.query_type == "q":
            # if the key is "q", make a GET request to the explore endpoint
            query_url = f"{_BASE_URL}explore.php"
        else:
            query_url = f"{_BASE_URL}info.php"
        params = {**self._get_default_params(), pd_query.query_type: pd_query.data}
        resp = httpx.get(
            query_url, params=params, headers=mp_ua_header(), timeout=get_http_timeout()
        )
        if resp.status_code != 200:
            raise ValueError(
                f"API request failed with status code {resp.status_code}",
                f" and message {resp.text}",
            )
        json_resp = resp.json()
        if pd_query.query_type == "q":
            json_resp = json_resp["results"]
        return json_normalize(json_resp)

    def _get_default_params(self):
        """Return default query parameters."""
        return {"pretty": 1, "key": self.pd_key}

    def _poll_for_results(self, resp):
        """Poll API for results for this query ID."""
        # if the initial request is successful, get the qid and
        # check the status until it is 'success'
        qid = resp.json()["qid"]
        params = {"qid": qid, **self._get_default_params()}
        headers = mp_ua_header()
        timeout = get_http_timeout()
        url = f"{_BASE_URL}analyze.php"
        resp = httpx.get(
            url,
            params=params,
            headers=headers,
            timeout=timeout,
        )
        status = resp.json()["status"]
        while status == "processing":
            sleep(0.5)  # wait a little to avoid hammering UI
            resp = httpx.get(url, params=params, headers=headers, timeout=timeout)
            status = resp.json()["status"]

        return json_normalize(resp.json()["data"])

    def _get_pd_api_key(self) -> Optional[str]:
        """Return Pulsedive api key from configuration."""
        pd_settings = get_provider_settings("TIProviders").get("Pulsedive")
        if pd_settings and "AuthKey" in pd_settings.args:
            return pd_settings.args["AuthKey"]
        return None


def _build_query_string(data, pd_type) -> PDQuery:
    """
    Build query string for the API request based on the provided data and pd_type.

    Parameters
    ----------
    data : Any
        Data to be mapped to query_item

    pd_type : str

    Returns
    -------
    Dict[str, Any]
        Query items in dictionary

    """
    query_items = _QUERY_OBJECTS_MAPPINGS[pd_type]

    # check if the provided data is empty
    if not data:
        raise ValueError("data is empty.")

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

    _QUERIES = {
        ioc_type: _QUERY_DEF for ioc_type in ("ipv4", "ipv6", "dns", "hostname", "url")
    }

    _REQUIRED_PARAMS = ["API_KEY"]
    _RISK_MAP = {
        "high": ResultSeverity.high,
        "medium": ResultSeverity.warning,
        "low": ResultSeverity.information,
        "none": ResultSeverity.unknown,
    }

    def __init__(self, **kwargs):
        """Set OTX specific settings."""
        super().__init__(**kwargs)
        self.require_url_encoding = True

    def parse_results(self, response: Dict) -> LookupResult:
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
            return LookupResult(
                False, ResultSeverity.information, {"status": "Not found."}
            )
        severity = self._RISK_MAP[response.get("risk", "none")]

        details = {
            "indicator_id": response.get("iid"),
            "threats": response.get("threats", []),
            "last_seen": response.get("stamp_seen", ""),
            "risk_factors": response.get("riskfactors", {}),
            "comments": response.get("comments", {}),
        }

        return LookupResult(True, severity, details)
