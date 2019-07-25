# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Security Graph OData Driver class."""
from typing import Tuple, Union, Any, Dict
import re
import urllib

import requests

import pandas as pd

from .driver_base import DriverBase
from ...nbtools.utility import export
from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_OAUTH_URL = "https://login.microsoftonline.com/{tenantId}/oauth2/v2.0/token"


@export
class SecurityGraphDriver(DriverBase):
    """KqlDriver class to execute kql queries."""

    _REQ_BODY = {
        "client_id": None,
        "client_secret": None,
        "grant_type": "client_credentials",
        "scope": "https://graph.microsoft.com/.default",
    }
    _REQ_HEADER = {
        "Content-Type": "application/json",
        "Accept": "application/json",
        "Authorization": None,
    }

    _DEF_API_ROOT = "https://graph.microsoft.com/"
    _DEF_API_VER = "v1.0"

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite KqlDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        super().__init__()
        self._loaded = True
        self.aad_token = None
        self.req_headers = None
        self.api_root = ""
        self._debug = kwargs.get("debug", False)

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_string : str
            Connect to a data source

        Notes
        -----
        Connection string fields:
        tenantId
        clientId
        clientSecret
        apiRoot
        apiVersion

        """
        if connection_str:
            self.current_connection = connection_str
            cs_dict = self._parse_connection_str(connection_str)
        else:
            cs_dict = kwargs

        req_url = _OAUTH_URL.format(tenantId=cs_dict["tenantId"])
        req_body = dict(self._REQ_BODY)
        req_body["client_id"] = cs_dict["clientId"]
        req_body["client_secret"] = cs_dict["clientSecret"]

        # authenticate and obtain AAD Token for future calls
        data = urllib.parse.urlencode(req_body).encode("utf-8")
        response = requests.post(url=req_url, data=data)
        json_response = response.json()

        self.aad_token = json_response.get("access_token", None)
        if not self.aad_token:
            raise ConnectionError("Could not obtain access token")

        self.req_headers = dict(self._REQ_HEADER)
        self.req_headers["Authorization"] = "Bearer " + self.aad_token
        self.api_root = cs_dict.get("apiRoot", self._DEF_API_ROOT) + cs_dict.get(
            "apiVersion", self._DEF_API_VER
        )

        print("Connected.")
        self._connected = True

        json_response["access_token"] = None
        return json_response

    def query(self, query: str) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Union[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) or
            Kql ResultSet if an error.

        """
        return self.query_with_results(query)[0]

    # pylint: disable=too-many-branches
    def query_with_results(self, query: str) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The kql query to execute

        Returns
        -------
        Tuple[pd.DataFrame, results.ResultSet]
            A DataFrame (if successfull) and
            Kql ResultSet.

        """
        if not self.connected:
            self.connect(self.current_connection)
            if not self.connected:
                raise ConnectionError(
                    "Source is not connected. ", "Please call connect() and retry."
                )

        if self._debug:
            print(query)

        req_url = self.api_root + query
        req_url = urllib.parse.quote(req_url, safe="%/:=&?~#+!$,;'@()*[]")
        response = requests.get(url=req_url, headers=self.req_headers)
        if response.status_code != requests.codes["ok"]:
            if response.status_code == 401:
                raise ConnectionRefusedError(
                    "Authentication failed - possible ", "timeout. Please re-connect."
                )
            response.raise_for_status()

        json_response = response.json()
        if isinstance(json_response, int):
            print(
                "Warning - query did not complete successfully.",
                "Check returned response.",
            )
            return None, json_response

        if "value" in json_response:
            result = json_response["value"]
        else:
            result = json_response

        if not result:
            print("Warning - query did not return any results.")
            return None, json_response
        return pd.io.json.json_normalize(result), result

    # pylint: enable=too-many-branches

    @staticmethod
    def _parse_connection_str(connection_str: str) -> Dict[str, str]:
        """
        Split connection string components into dictionary.

        Parameters
        ----------
        connection_str : str
            Semi-colon delimited connection string

        Returns
        -------
        Dict[str, str]
            dict of key/pair values

        """
        cs_items = connection_str.split(";")
        cs_dict = {
            prop[0]: prop[1]
            for prop in [item.strip().split("=") for item in cs_items]
            if prop[0] and prop[1]
        }
        return cs_dict

    @staticmethod
    def _prepare_param_dict_from_filter(filterstr: str) -> Dict[str, str]:
        """
        Parse filter string into dictionary.

        Parameters
        ----------
        filterstr : str
            OData filter string

        """
        get_params = {}
        for filter_param in re.split(r"[\?\&]+", filterstr):
            if filter_param:
                attr = filter_param.split("=")[0]
                val = filter_param.split("=")[1]
                get_params[attr] = val
        return get_params
