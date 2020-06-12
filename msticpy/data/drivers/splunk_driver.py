# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Splunk Driver class."""
from typing import Any, Optional, Tuple, Union

import pandas as pd
import splunklib.client as client
import splunklib.results as results

from .driver_base import DriverBase, QuerySource
from ..._version import VERSION
from ...common.utility import MsticpyException, export


__version__ = VERSION
__author__ = "Ashwin Patil"

@export
class SplunkDriver(DriverBase):
    """Driver to connect and query from Splunk. """
    def __init__(self, **kwargs):
        """Instantiate Splunk Driver"""
        super().__init__()
        self.host = None
        self.port = 8089
        self.username = None
        self.password = None
        self.http_scheme: Optional[str] = "https"
        self.verify: Optional[bool] = False
        self.service = None
        self._loaded = True
        self._connected = False
        self._debug = kwargs.get("debug", False)

    def connect(self, **kwargs):
        """
        Connection to Splunk via splunk-sdk.

        Returns
        -------
        [type]
            Splunk service object if connected successfully

        """
        if kwargs:
            cs_dict = kwargs
            cs_dict['port'] = kwargs.get("port", 8089)
            cs_dict['http_scheme'] = kwargs.get("http_scheme", "https")
            cs_dict['verify'] = kwargs.get("verify", False)
            try:
                self.service = client.connect(
                    host=cs_dict['host'],
                    port=cs_dict['port'],
                    username=cs_dict['username'],
                    password=cs_dict['password'],
                    http_scheme=cs_dict['http_scheme'],
                    verify=cs_dict['verify']
                    )
                self._connected = True
                print('Connected to Splunk successfully !!')
                # return self.service
            except Exception as err:
                raise Exception(f"Error connecting to Splunk: {err}")
        else:
            raise MsticpyException("No connection details provided")

    def query(self, query: str, query_source: QuerySource = None) -> Tuple[pd.DataFrame, Any]:
        """
        Execute splunk query and retrieve results via OneShot search mode.

        Parameters
        ----------
        query : str
            Splunk query to execute via OneShot search mode

        Returns
        -------
        Tuple[pd.DataFrame, Any]
            Query results in a dataframe.

        """
        del query_source
        if not self._connected:
            raise ConnectionError(
            "Source is not connected.", "Please call connect() and retry"
            )
        query_results = self.service.jobs.oneshot(query)
        reader = results.ResultsReader(query_results)
        json_response = []
        for row in reader:
            json_response.append(row)
        if isinstance(json_response, int):
            print("Warning - query did not return any results.")
            return None, json_response
        return pd.DataFrame(pd.io.json.json_normalize(json_response))

    def query_with_results(self, query: str) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            Query to execute against splunk instance.

        Returns
        -------
        Union[pd.DataFrame,Any]
            A DataFrame (if successful) or
            the underlying provider result if an error occurs.

        """

    def list_savedsearches(self, **kwargs) -> Union[pd.DataFrame, Any]:
        """ Returns list of saved searches in dataframe

        Returns
        -------
        pd.DataFrame
            Dataframe with list of saved searches with name and query columns.

        """
        if not self.connected:
            raise ConnectionError(
            "Source is not connected.", "Please call connect() and retry"
            )
        savedsearches = self.service.saved_searches

        out_df = pd.DataFrame(columns=['name', 'query'])

        namelist = []
        querylist = []
        for savedsearch in savedsearches:
            namelist.append(savedsearch.name)
            querylist.append(savedsearch["search"])
        out_df['name'] = namelist
        out_df['query'] = querylist

        return out_df

    def list_firedalerts(self, **kwargs) -> Union[pd.DataFrame, Any]:
        """ Returns list of fired alerts in dataframe

        Returns
        -------
        pd.DataFrame
            Dataframe with list of fired alerts with alert name and count columns.

        """
        if not self.connected:
            raise ConnectionError(
            "Source is not connected.", "Please call connect() and retry"
            )
        firedalerts = self.service.fired_alerts

        out_df = pd.DataFrame(columns=['name', 'count'])

        alert_names = []
        alert_counts = []
        for alert in firedalerts:
            alert_names.append(alert.name)
            alert_counts.append(alert.count)
        out_df['name'] = alert_names
        out_df['count'] = alert_counts

        return out_df

