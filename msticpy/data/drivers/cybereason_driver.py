# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Cybereason Driver class."""
from typing import Any, Dict, Optional, Tuple, Union, List

import json
import datetime as dt
import httpx
import pandas as pd

from ..._version import VERSION
from ...common.provider_settings import get_provider_settings, ProviderArgs
from ...common.exceptions import MsticpyUserConfigError
from .driver_base import DriverBase, QuerySource

__version__ = VERSION
__author__ = "Florian Bracq"

_HELP_URI = (
    "https://msticpy.readthedocs.io/en/latest/data_acquisition/DataProviders.html"
)


CybereasonSettings = Dict[str, Dict[str, Union[str, ProviderArgs]]]


class CybereasonDriver(DriverBase):
    """Class to interact with Cybereason."""

    CONFIG_NAME = "Cybereason"

    _CONFIG_NAME_MAP = {
        "tenant_id": ("tenantid", "tenant_id"),
        "client_id": ("clientid", "client_id"),
        "client_secret": ("clientsecret", "client_secret"),
    }

    def __init__(self, **kwargs):
        """Instantiate Cybereason driver."""
        super().__init__(**kwargs)
        self.base_url: str = "https://{tenant_id}.cybereason.net"
        self.auth_endpoint: str = "/login.html"
        self.req_body: Dict[str, Any] = {
            "queryPath": [],
            "totalResultLimit": 1000,
            "perGroupLimit": 100,
            "perFeatureLimit": 100,
            "templateContext": "SPECIFIC",
            "queryTimeout": 2 * 60 * 1000,  # 2 minutes in milliseconds
        }
        self.search_endpoint: str = "/rest/visualsearch/query/simple"
        self._loaded = True
        self.client = httpx.Client(follow_redirects=True)
        self.formatters = {
            "datetime": self._format_datetime,
            "list": self._format_list,
        }

        self._debug = kwargs.get("debug", False)

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        data, response = self.query_with_results(query)
        if isinstance(data, pd.DataFrame):
            return data
        return response

    def connect(
        self,
        connection_str: Optional[str] = None,
        **kwargs,
    ):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str: Optional[str], optional
            Connect to a data source
        instance : Optional[str], optional
            Optional name of configuration instance - this
            is added as a prefix to the driver configuration key name
            when searching for configuration in the msticpyconfig.yaml

        Notes
        -----
        Connection string fields:
            instance
            client_id
            client_secret

        """
        cs_dict: Dict[str, Any] = {}

        instance = kwargs.pop("instance", None)
        cs_dict = CybereasonDriver._get_driver_settings(self.CONFIG_NAME, instance)
        # let user override config settings with function kwargs
        cs_dict.update(kwargs)

        missing_settings = [
            setting
            for setting in ("tenant_id", "client_id", "client_secret")
            if setting not in cs_dict
        ]
        if missing_settings:
            raise MsticpyUserConfigError(
                "You must supply the following required connection parameter(s)",
                "to the connect function or add them to your msticpyconfig.yaml.",
                ", ".join(f"'{param}'" for param in missing_settings),
                title="Missing connection parameters.",
                help_uri=("Connecting to OData sources.", _HELP_URI),
            )

        # self.auth_endpoint and self.req_body are correctly set in concrete
        # instances __init__
        self.client.base_url = httpx.URL(
            self.base_url.format(tenant_id=cs_dict["tenant_id"])
        )
        req_body: Dict[str, str] = {
            "username": cs_dict["client_id"],
            "password": cs_dict["client_secret"],
        }

        # Authenticate and obtain cookie for future calls
        response = self.client.post(self.auth_endpoint, data=req_body)
        response.raise_for_status()

        print("Connected.")
        self._connected = True

        return self._connected

    @staticmethod
    def _flatten_result(entry: Dict[str, Any]) -> Dict[str, Any]:
        """
        Flatten Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        entry: Dict[str, Any]
            Entry to flatten

        Returns
        -------
        Dict[str, Any]

        """
        result = {}
        # Retrieve simpleValues and add them to the output
        simple_values: Dict[str, Any] = entry.get("simpleValues", {})
        result = CybereasonDriver._flatten_simple_values(simple_values)

        elt_value = entry.get("elementValues", {})  # List or Dict
        result.update(**CybereasonDriver._flatten_element_values(elt_value))
        return result

    @staticmethod
    def _flatten_simple_values(simple_values: Dict[str, Any]) -> Dict[str, Any]:
        """
        Flatten "simpleValues from Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        simple_values: Dict[str, Any]
            Entry to flatten

        Returns
        -------
        Dict[str, Any]

        """
        result = {}
        for name, values in simple_values.items():
            if values["totalValues"] == 1:
                if "Time" in name:
                    result[name] = CybereasonDriver._format_to_datetime(
                        int(values["values"][0])
                    )
                else:
                    result[name] = values["values"][0]
            elif values["totalValues"] > 1:
                result[name] = values["values"]
        return result

    @staticmethod
    def _flatten_element_values(
        element_values: Union[Dict[str, Any], List[Dict[str, Any]]]
    ) -> Dict[str, Any]:
        """
        Flatten "elementValues from Cybereason result to a format that can be handled by pandas.

        Parameters
        ----------
        element_values: Union[Dict[str, Any], List[str]]
            Entry to flatten

        Returns
        -------
        Dict[str, Any]

        """
        result = {}
        if isinstance(element_values, list):
            for values in element_values:
                result[values["elementType"]] = values["name"]
        elif isinstance(element_values, dict):
            for key, values in element_values.items():
                flattened = CybereasonDriver._flatten_result(values)
                if flattened:
                    for subkey, subvalues in flattened.items():
                        result[f"{key}.{subkey}"] = subvalues
        return result

    # pylint: disable=too-many-branches
    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
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

        json_query = json.loads(query)
        body = self.req_body
        body.update(json_query)
        if "customFields" in kwargs:
            body.update({"customFields": kwargs["customFields"]})
        response = self.client.post(self.search_endpoint, json=body)

        self._check_response_errors(response)

        json_response = response.json()
        if json_response["status"] != "SUCCESS":
            print(
                "Warning - query did not complete successfully.",
                "Check returned response.",
            )
            return None, json_response

        result = json_response.get("data", json_response)
        result = result.get("resultIdToElementDataMap", result)
        result = [CybereasonDriver._flatten_result(v) for v in result.values()]

        if not result:
            print("Warning - query did not return any results.")
            return None, json_response
        return pd.json_normalize(result), json_response

    # pylint: enable=too-many-branches

    @staticmethod
    def _check_response_errors(response):
        """Check the response for possible errors."""
        if response.status_code == httpx.codes.OK:
            return
        print(response.json()["error"]["message"])
        if response.status_code == 401:
            raise ConnectionRefusedError(
                "Authentication failed - possible ", "timeout. Please re-connect."
            )
        # Raise an exception to handle hitting API limits
        if response.status_code == 429:
            raise ConnectionRefusedError("You have likely hit the API limit. ")
        response.raise_for_status()

    # Parameter Formatting method
    @staticmethod
    def _format_datetime(date_time: dt.datetime) -> int:
        """Return datetime formatted as timestamp in milliseconds."""
        return int(date_time.timestamp() * 1000)

    # Parameter Formatting method
    @staticmethod
    def _format_to_datetime(timestamp: int) -> Union[dt.datetime, int]:
        """Return datetime from a timestamp in milliseconds."""
        try:
            return dt.datetime.fromtimestamp(timestamp // 1000)
        except TypeError:
            return timestamp

    # Parameter Formatting method
    @staticmethod
    def _format_list(item_list: List[Any]) -> str:
        """Return formatted list parameter."""
        fmt_list = []
        for item in item_list:
            if isinstance(item, str):
                fmt_list.append(f'"{item}"')
            else:
                fmt_list.append(f"{item}")
        return ",".join(fmt_list)

    # Retrieve configuration parameters with aliases
    @staticmethod
    def _map_config_dict_name(config_dict: Dict[str, str]):
        """Map configuration parameter names to expected values."""
        mapped_dict = config_dict.copy()
        for provided_name in config_dict:
            for req_name, alternates in CybereasonDriver._CONFIG_NAME_MAP.items():
                if provided_name.casefold() in alternates:
                    mapped_dict[req_name] = config_dict[provided_name]
                    break
        return mapped_dict

    # Read values from configuration
    @staticmethod
    def _get_driver_settings(
        config_name: str, instance: Optional[str] = None
    ) -> Dict[str, str]:
        """Try to retrieve config settings for Cybereason drivers."""
        config_key = f"{config_name}-{instance}" if instance else config_name
        drv_config = get_provider_settings("DataProviders").get(config_key)
        app_config: Dict[str, str] = {}
        if drv_config:
            app_config = dict(drv_config.args)

        if not app_config:
            return {}
        # map names to allow for different spellings
        return CybereasonDriver._map_config_dict_name(app_config)
