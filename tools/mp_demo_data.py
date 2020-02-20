# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Demo QueryProvider."""
from functools import partial
from pathlib import Path
import pickle
from typing import List, Dict, Union, Any, Iterable
from time import sleep

import pandas as pd
import yaml

from msticpy.data.data_providers import AttribHolder
from msticpy.data import QueryProvider


# pylint: disable=too-few-public-methods
class _DataDriver:
    """Demo data provider."""

    def __init__(self):
        """Initialize demo_provider."""
        self.connected = False
        self.loaded = True
        self.connection_str = ""

    def connect(self, connection_str="default", **kwargs):
        """Connect to data source."""
        del kwargs
        self.connected = True
        self.connection_str = connection_str
        print("Connected.")


# pylint: enable=too-few-public-methods


class QueryProviderDemo(QueryProvider):
    """Query provider for demo data."""

    _DATA_DEFS = {
        "SecurityAlert": {"list_alerts": "data/alerts_list.pkl"},
        "WindowsSecurity": {
            "get_process_tree": "data/process_tree.pkl",
            "list_host_processes": "data/processes_on_host.pkl",
            "list_host_logons": ".data/host_logons.pkl",
            "list_host_logon_failures": "data/failedLogons.pkl",
            "list_host_events": "data/all_events_df.pkl",
        },
        "Network": {
            "list_azure_network_flows_by_ip": "data/az_net_comms_df.pkl",
            "list_azure_network_flows_by_host": "data/az_net_comms_df.pkl",
        },
    }

    # pylint: disable=super-init-not-called
    def __init__(self, **kwargs):
        """Initialize Demo query provider."""
        self._environment = kwargs.get("data_environment", "default environment")
        print(f"{self.__class__.__name__} initialized for {self._environment}")
        data_src_file = kwargs.get("data_src_file")
        if not data_src_file:
            data_srcs = self._DATA_DEFS
            # raise ValueError("no query definition file name")
        else:
            with open(data_src_file, "r") as src_file:
                data_srcs = yaml.safe_load(src_file)
        self._query_store = {}
        self._query_provider = _DataDriver()
        self.all_queries = AttribHolder()
        self._add_demo_query_functions(data_srcs)

    def _add_demo_query_functions(self, data_defs: Dict[str, Dict[str, str]]):
        for family, queries in data_defs.items():
            if not hasattr(self, family):
                setattr(self, family, AttribHolder())
            query_family = getattr(self, family)

            for query_name, file_name in queries.items():

                # Create the partial function
                query_func = partial(
                    self._execute_query,
                    data_family=family,
                    query_name=query_name,
                    data_file=file_name,
                )

                setattr(query_family, query_name, query_func)
                setattr(self.all_queries, query_name, query_func)
                self._query_store[f"{family}.{query_name}"] = file_name

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connection string for the data source

        """
        return self._query_provider.connect(connection_str=connection_str, **kwargs)

    @property
    def schema(self) -> Dict[str, Dict]:
        """
        Return current data schema of connection.

        Returns
        -------
        Dict[str, Dict]
            Data schema of current connection.

        """
        return {}

    @property
    def schema_tables(self) -> List[str]:
        """
        Return list of tables in the data schema of the connection.

        Returns
        -------
        List[str]
            Tables in the of current connection.

        """
        return []

    def import_query_file(self, query_file: str):
        """
        Import a yaml data source definition.

        Parameters
        ----------
        query_file : str
            Path to the file to import

        """
        raise NotImplementedError()

    def list_queries(self) -> List[str]:
        """
        Return list of family.query in the store.

        Returns
        -------
        Iterable[str]
            List of queries

        """
        return list(self._query_store.items())

    def query_help(self, query_name):
        """Print help for query."""
        print(f"query_prov.{self._query_store[query_name]}(**kwargs)")

    def exec_query(self, query: str) -> Union[pd.DataFrame, Any]:
        """
        Execute simple query string.

        Parameters
        ----------
        query : str
            [description]

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results - a DataFrame if successful
            or a KqlResult if unsuccessful.

        """
        raise NotImplementedError()

    def _execute_query(self, *args, **kwargs) -> Union[pd.DataFrame, Any]:
        if not self._query_provider.loaded:
            raise ValueError("Provider is not loaded.")
        if not self._query_provider.connected:
            raise ValueError(
                "No connection to a data source.",
                "Please call connect(connection_str) and retry.",
            )
        sleep(1)
        query_name = kwargs.pop("query_name")
        data_file = kwargs.pop("data_file")
        return read_pd_df(data_file, query_name)


def read_pd_df(data_file, query_name):
    """Read DataFrame from file."""
    if not Path(data_file).is_file():
        raise FileNotFoundError(
            f"Data file {data_file} for query {query_name} not found."
        )

    if data_file.lower().endswith("csv"):
        return pd.read_csv(
            data_file, infer_datetime_format=True, parse_dates=["TimeGenerated"]
        )
    return pd.read_pickle(data_file)


class TILookupDemo:
    """TILookup demo class."""

    _DATA_DEFS = {"ipv4": "data/ti_results_ipv4.pkl", "url": "data/ti_results_url.pkl"}

    def lookup_ioc(self, ioc_type, **kwargs):
        """Lookup single IoC."""
        del kwargs
        sleep(1)
        return read_pd_df(self._DATA_DEFS.get(ioc_type), ioc_type)

    @staticmethod
    def result_to_df(results):
        """Convert IoC results to DataFrame."""
        if isinstance(results, pd.DataFrame):
            return results
        return pd.DataFrame()


# pylint: disable=too-few-public-methods
class GeoLiteLookupDemo:
    """GeoLitLookup demo class."""

    _DATA_DEFS = {"ip_locs": "data/ip_locations.pkl"}

    def lookup_ip(
        self,
        ip_address: str = None,
        ip_addr_list: Iterable = None,
        ip_entity: Any = None,
    ):
        """Look up location."""
        del ip_address, ip_addr_list, ip_entity
        with open(self._DATA_DEFS["ip_locs"], "rb") as iploc_file:
            ip_locs = pickle.load(iploc_file)  # noqa: B301
        return str(ip_locs), ip_locs


# pylint: enable=too-few-public-methods


_ASN_DATA = pd.read_pickle("data/az_whois.df.pkl")


def get_whois_info_demo(ip_addr, show_progress=False):
    """Lookup Whois data from dataframe."""
    sleep(0.02)
    if show_progress:
        print(".", end="")
    if "ExtASN" not in _ASN_DATA.columns:
        return "Unknown", {}
    match_row = _ASN_DATA[_ASN_DATA["AllExtIPs"] == ip_addr]
    asn_text = match_row["ExtASN"].unique()[0]
    if isinstance(asn_text, tuple):
        return asn_text[0], {}
    return asn_text, {}
