# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""OData Driver class."""
import abc
from typing import Tuple, Any, Dict, Union, Optional
import re
import urllib

import requests

import pandas as pd

from .driver_base import DriverBase
from ...nbtools import pkg_config as config
from ...nbtools.utility import MsticpyException
from ..._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


class OData(DriverBase):
    """Parent class to retreive date from an oauth based API."""

    # pylint: disable=too-many-instance-attributes
    # Large number needed due to variability of APIs
    def __init__(self, **kwargs):
        """
        Instantiaite MDATPDriver and optionally connect.

        Parameters
        ----------
        connect: bool, optional
            Set true if you want to connect to the provider at initialization

        """
        super().__init__()
        self.req_headers = {
            "Content-Type": "application/json",
            "Accept": "application/json",
            "Authorization": None,
        }
        self.oauth_url: Optional[str] = None
        self.req_body: Optional[Dict[str, Optional[str]]] = None
        self.api_ver: Optional[str] = None
        self.api_root: Optional[str] = None
        self._loaded = True
        self.aad_token = None
        self._debug = kwargs.get("debug", False)

    # pylint: enable=too-many-instance-attributes
    @abc.abstractmethod
    def query(self, query: str) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to oauth data source.

        Parameters
        ----------
        connection_str: str, optional
            Connect to a data source

        Notes
        -----
        Connection string fields:
        tenant_id
        client_id
        clien_secret
        apiRoot
        apiVersion

        """
        if connection_str:
            self.current_connection = connection_str
            cs_dict = self._parse_connection_str(connection_str)
        elif kwargs:
            cs_dict = kwargs
            # Allow user to specify location of connection variables in config file.
            if "app_name" in cs_dict:
                app_config = config.settings.get(cs_dict["app_name"])
                if not app_config:
                    raise MsticpyException(
                        f"No configuration settings found for {cs_dict['app_name']}."
                    )
                cs_dict = app_config["Args"]
        else:
            raise MsticpyException("No connection details provided.")

        # self.oauth_url and self.req_body are correctly set in concrete
        # instances __init__
        req_url = self.oauth_url.format(tenantId=cs_dict["tenant_id"])  # type: ignore
        req_body = dict(self.req_body)  # type: ignore
        req_body["client_id"] = cs_dict["client_id"]
        req_body["client_secret"] = cs_dict["client_secret"]

        # Authenticate and obtain AAD Token for future calls
        data = urllib.parse.urlencode(req_body).encode("utf-8")
        response = requests.post(url=req_url, data=data)
        json_response = response.json()
        self.aad_token = json_response.get("access_token", None)
        if not self.aad_token:
            raise ConnectionError("Could not obtain access token")

        self.req_headers["Authorization"] = "Bearer " + self.aad_token
        self.api_root = cs_dict.get(  # type: ignore
            "apiRoot", self.api_root
        ) + cs_dict.get(  # type: ignore
            "apiVersion", self.api_ver  # type: ignore
        )

        print("Connected.")
        self._connected = True

        json_response["access_token"] = None
        return json_response

    # pylint: disable=too-many-branches
    def query_with_results(  # noqa: MC0001
        self, query: str, **kwargs
    ) -> Tuple[pd.DataFrame, Any]:  # noqa: MC0001
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

        # Build request based on whether endpoint requires data to be passed in
        # request body in or URL
        if kwargs["body"] is True:
            req_url = self.api_root + kwargs["api_end"]
            req_url = urllib.parse.quote(req_url, safe="%/:=&?~#+!$,;'@()*[]")
            body = {"Query": query}
            response = requests.post(
                url=req_url, headers=self.req_headers, data=str(body)
            )
        else:
            # api_root set if self.connected
            req_url = self.api_root + query  # type: ignore
            response = requests.get(url=req_url, headers=self.req_headers)
        if response.status_code != requests.codes["ok"]:
            if response.status_code == 401:
                raise ConnectionRefusedError(
                    "Authentication failed - possible ", "timeout. Please re-connect."
                )
            # Raise an exception to handle hittng API limits
            if response.status_code == 429:
                raise ConnectionRefusedError("You have likely hit the API limit. ")
            response.raise_for_status()

        json_response = response.json()
        if isinstance(json_response, int):
            print(
                "Warning - query did not complete successfully.",
                "Check returned response.",
            )
            return None, json_response

        if "Results" in json_response:
            result = json_response["Results"]
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
