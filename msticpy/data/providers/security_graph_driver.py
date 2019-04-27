# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Security Graph OData Driver class."""
from typing import Tuple, Union, Any, Dict
import json
import urllib

import pandas as pd

from . provider_base import DataProviderBase
from ... nbtools.utility import export
from ... _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


_OAUTH_URL = "https://login.microsoftonline.com/{tenant}/oauth2/v2.0/token"


@export
class SecurityGraphDriver(DataProviderBase):
    """KqlDriver class to execute kql queries."""

    _REQ_BODY = {
        'client_id': None,
        'client_secret': None,
        'grant_type': 'client_credentials',
        'scope': 'https://graph.microsoft.com/.default'
    }
    _REQ_HEADER = {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        'Authorization': None
    }

    _DEF_API_ROOT = 'https://graph.microsoft.com/'
    _DEF_API_VER = 'v1.0'

    def __init__(self, connection_str: str = None, **kwargs):
        """
        Instantiaite KqlDriver and optionally connect.

        Parameters
        ----------
        connection_str : str, optional
            Connection string

        """
        self._loaded = True
        self.aad_token = None
        self.req_headers = None
        self.api_root = None
        self._debug = kwargs.get('debug', False)

        if connection_str:
            self.current_connection = connection_str
            self.connect(connection_str)

        super().__init__()

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
        appId
        appSecret
        apiRoot
        apiVersion

        """
        if connection_str:
            self.current_connection = connection_str
            cs_dict = self._parse_connection_str(connection_str)
        else:
            cs_dict = kwargs

        req_url = _OAUTH_URL.format(cs_dict['tenantId'])
        req_body = dict(self._REQ_BODY)
        req_body['client_id'] = cs_dict['appId']
        req_body['client_secret'] = cs_dict['appSecret']

        # authenticate and obtain AAD Token for future calls
        data = urllib.parse.urlencode(req_body).encode("utf-8")
        req = urllib.request.Request(req_url, data)
        response = urllib.request.urlopen(req)
        json_response = json.loads(response.read().decode())
        self._aad_token = json_response['access_token']
        self._req_headers = dict(self._REQ_HEADER)
        self._req_headers['Authorization'] = "Bearer " + self._aad_token

        self.api_root = (cs_dict.get('apiRoot', self._DEF_API_ROOT)
                         + cs_dict.get('apiVersion', self._DEF_API_VER))

        if not self.aad_token:
            print("Connected.")
            self._connected = True
        json_response['access_token'] = None
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

    def query_with_results(self, query: str) -> Tuple[pd.DataFrame,
                                                      Any]:
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
            raise ConnectionError('Source is not connected. ',
                                  'Please call connect() and retry.')

        if self._debug:
            print(query)
        req = urllib.request.Request(query, headers=self.req_headers)
        try:
            response = urllib.request.urlopen(req)
        except urllib.error.HTTPError as http_error:
            if http_error.code == 401:
                raise ConnectionRefusedError('Authentication failed - possible ',
                                             'timeout. Please re-connect.')
            else:
                raise http_error

        json_response = json.loads(response.read().decode())
        if isinstance(json_response, int):
            print("Warning - query did not complete successfully.",
                  "Check returned response.")
            return None, json_response

        if 'value' in json_response:
            result = json_response["value"]
        else:
            result = json_response

        if not result:
            print("Warning - query did not complete successfully.",
                  "Check returned response.")
            return None, json_response
        return pd.io.json.json_normalize(result), result

    def _parse_connection_str(self, connection_str: str) -> Dict[str, str]:
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
        cs_items = connection_str.split(';')

        cs_dict = {key.strip(): val.strip() for key, val in cs_items.split('=')}
        return cs_dict
