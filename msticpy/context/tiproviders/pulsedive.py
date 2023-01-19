# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# Author: Thomas Roccia - @fr0gger_
# --------------------------------------------------------------------------
"""Pulsedive TI Provider."""

from enum import Enum
from typing import Set

import httpx
import pandas as pd
from pandas import json_normalize

from ..._version import VERSION

__version__ = VERSION
__author__ = "Thomas Roccia | @fr0gger_"

_BASE_URL = "https://pulsedive.com/api/"

_QUERY_OBJECTS_MAPPINGS = {
    "indicator": {"indicator": "observable"},
    "threat": {"threat": "observable"},
    "explore": {"q": "query"},
    "scan": {"scan": "observable"},
    }


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

    Attributes:
    pd_key (str): An API key for the Pulsedive API.

    Methods:
    _make_pd_request(data): Makes a request to the Pulsedive API with the provided data.
    lookup_ioc(observable: str, pd_type: str) -> pd.DataFrame: Lookup an indicator of
    compromise in the Pulsedive API.
    explore(query: str, pd_type: str) -> pd.DataFrame: Explore different types of data
    available in the Pulsedive API.
    scan(observable: str, pd_type: str) -> pd.DataFrame: Submits an indicator of compromise
    to the Pulsedive API for scanning.
    """

    _SUPPORTED_PD_TYPES: Set[PDEntityType] = {
        PDEntityType.INDICATOR,
        PDEntityType.THREAT,
        PDEntityType.EXPLORE,
        PDEntityType.SCAN,
    }

    def __init__(self, pd_key=None):
        """Init function to get the API key if necessary."""
        self.pd_key = pd_key  # or_get_mb_api_key()

    def _make_pd_request(self, data):
        """
        Makes a request to the PulseDive API with the provided data.

        Parameters:
        - data (dict): a dictionary containing the query parameters to send in the request

        Returns:
        - pd.DataFrame: a dataframe containing the results of the API call
        """

        for key, value in data.items():
            if key == "q":
                # if the key is "q", make a GET request to the explore endpoint
                res = httpx.get(f'{_BASE_URL}explore.php?{key}={value}\
                    &pretty=1&key={self.pd_key}', timeout=None)
                if res.status_code == 200:
                    jspd = res.json()
                    return json_normalize(jspd['results'])

            elif key == "scan":
                # if the key is "scan", make a POST request to the analyze endpoint
                data = {"value": value, "probe": 1, "pretty": 1, "key": self.pd_key}
                headers = {"Content-Type": "application/x-www-form-urlencoded"}
                res = httpx.post(f"{_BASE_URL}analyze.php?", data=data, headers=headers)
                if res.status_code == 200:
                    # if the initial request is successful, get the qid and
                    # check the status until it is 'success'
                    qid = res.json()['qid']
                    res = httpx.get(f"{_BASE_URL}analyze.php?qid={qid}\
                        &pretty=1&key={self.pd_key}")
                    status = res.json()['status']
                    print(status)
                    while status == 'processing':
                        result = httpx.get(f"{_BASE_URL}analyze.php?qid={qid}\
                            &pretty=1&key={self.pd_key}")
                        if next(iter(result.json())) != "error":
                            break
                    return json_normalize(result.json()["data"])
            else:
                # if the key is not "q" or "scan", make a GET request to the info endpoint
                res = httpx.get(f"{_BASE_URL}info.php?{key}={value}\
                    &pretty=1&key={self.pd_key}", timeout=None)
                if res.status_code == 200:
                    return json_normalize(res.json())

                raise ValueError(f'API request failed with status \
                    code {res.status_code} and message {res.text}')
            return None


    def lookup_ioc(self, observable: str, pd_type: str) -> pd.DataFrame:
        """
        Lookup an indicator of compromise (IOC) in the Pulsedive API.
        :param observable: The IOC to lookup.
        :param pd_type: The type of the IOC. Should be one of the PDEntityType Enum options.
        :return: A Pandas DataFrame with the API's response.
        """
        if pd_type not in _QUERY_OBJECTS_MAPPINGS:
            raise ValueError(f"Invalid pd_type : {pd_type}. \
            Should be one of {_QUERY_OBJECTS_MAPPINGS.keys()}")

        query_parameters = _build_query_string(observable, pd_type)
        return self._make_pd_request(query_parameters)

    def explore(self, query: str, pd_type: str) -> pd.DataFrame:
        """
        Perform a search query on the Pulsedive API.
        :param query: The query to perform.
        :param pd_type: The type of the query. Should be one of the PDEntityType Enum options.
        :return: A Pandas DataFrame with the API's response.
        """
        if pd_type not in _QUERY_OBJECTS_MAPPINGS:
            raise ValueError(f"Invalid pd_type : {pd_type}. \
                Should be one of {_QUERY_OBJECTS_MAPPINGS.keys()}")

        query_parameters = _build_query_string(query, pd_type)

        return self._make_pd_request(query_parameters)

    def scan(self, observable: str, pd_type: str) -> pd.DataFrame:
        """
        Scan an observable in the Pulsedive API.
        :param observable: The observable to scan.
        :param pd_type: The type of the observable. Should be one of the PDEntityType Enum options.
        :return: A Pandas DataFrame with the API's response.
        """
        if pd_type not in _QUERY_OBJECTS_MAPPINGS:
            raise ValueError(f"Invalid pd_type : {pd_type}. \
                Should be one of {_QUERY_OBJECTS_MAPPINGS.keys()}")

        query_parameters = _build_query_string(observable, pd_type)
        return self._make_pd_request(query_parameters)

def _build_query_string(data, pd_type):
    """
    Builds the query string for the API request based on the provided data and pd_type.
    """
    query_items = _QUERY_OBJECTS_MAPPINGS[pd_type]

    # check if the provided data is empty
    if not data:
        raise ValueError("data is empty.")

    # assign the provided data to the corresponding key in the query_items dictionary
    query_items = {list(query_items.keys())[0]: data}

    # return the final query_items dictionary
    return query_items
