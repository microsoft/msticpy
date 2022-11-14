#  -------------------------------------------------------------------------
#  Copyright (c) Microsoft Corporation. All rights reserved.
#  Licensed under the MIT License. See License.txt in the project root for
#  license information.
#  --------------------------------------------------------------------------
"""Elastic Driver class."""
import json
from datetime import datetime
from typing import Any, Dict, Iterable, Optional, Tuple, Union

import pandas as pd

from ..._version import VERSION
from ...common.exceptions import MsticpyUserConfigError
from ...common.utility import check_kwargs, export
from ..core.query_defns import Formatters
from .driver_base import DriverBase, QuerySource

__version__ = VERSION
__author__ = "Neil Desai, Ian Hellen"


ELASTIC_CONNECT_ARGS: Dict[str, str] = {
    # TBD - you may not need these - mainly for user
    # help/error messages (see _get_connect_args)
}

_ELASTIC_REQUIRED_ARGS: Dict[str, str] = {
    # TBD
}


@export
class ElasticDriver(DriverBase):
    """Driver to connect and query from Elastic Search."""

    def __init__(self, **kwargs):
        """Instantiate Elastic Driver."""
        super().__init__(**kwargs)
        self.service = None
        self._loaded = True
        self._connected = False
        self._debug = kwargs.get("debug", False)

        self.formatters = {
            Formatters.PARAM_HANDLER: self._custom_param_handler,
            Formatters.DATETIME: self._format_datetime,
            Formatters.LIST: self._format_list,
        }

    def connect(self, connection_str: str = None, **kwargs):
        """
        Connect to Elastic cluster.

        Parameters
        ----------
        connection_str : Optional[str], optional
            Connection string with Splunk connection parameters

        Other Parameters
        ----------------
        kwargs :
            Connection parameters can be supplied as keyword parameters.

        Notes
        -----
        Default configuration is read from the DataProviders/Splunk
        section of msticpyconfig.yaml, if available.

        """
        # cs_dict = self._get_connect_args(connection_str, **kwargs)

        # TBD

        self._connected = True
        print("connected")

    def _get_connect_args(
        self, connection_str: Optional[str], **kwargs
    ) -> Dict[str, Any]:
        """Check and consolidate connection parameters."""
        cs_dict: Dict[str, Any] = {}
        # Fetch any config settings
        cs_dict.update(self._get_config_settings("Elastic"))
        # If a connection string - parse this and add to config
        if connection_str:
            cs_items = connection_str.split(";")
            cs_dict.update(
                {
                    cs_item.split("=")[0].strip(): cs_item.split("=")[1]
                    for cs_item in cs_items
                }
            )
        elif kwargs:
            # if connection args supplied as kwargs
            cs_dict.update(kwargs)
            check_kwargs(cs_dict, list(ELASTIC_CONNECT_ARGS.keys()))

        missing_args = set(_ELASTIC_REQUIRED_ARGS) - cs_dict.keys()
        if missing_args:
            raise MsticpyUserConfigError(
                "One or more connection parameters missing for Elastic connector",
                ", ".join(missing_args),
                f"Required parameters are {', '.join(_ELASTIC_REQUIRED_ARGS)}",
                "All parameters:",
                *[f"{arg}: {desc}" for arg, desc in ELASTIC_CONNECT_ARGS.items()],
                title="no Elastic connection parameters",
            )
        return cs_dict

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query and retrieve results.

        Parameters
        ----------
        query : str
            Elastic query to execute
        query_source : QuerySource
            The query definition object

        Other Parameters
        ----------------
        kwargs :
            Not used

        Returns
        -------
        Union[pd.DataFrame, Any]
            Query results in a dataframe.
            or query response if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err("Elastic")

        # TBD
        # Run query and return results
        return pd.DateFrame()

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            Query to execute.

        Returns
        -------
        Union[pd.DataFrame,Any]
            A DataFrame (if successful) or
            the underlying provider result if an error occurs.

        """
        raise NotImplementedError(f"Not supported for {self.__class__.__name__}")

    # Parameter Formatting methods
    # If not needed, remove these and remove from self.formatters
    # dict in __init__
    @staticmethod
    def _format_datetime(date_time: datetime) -> str:
        """Return datetime-formatted string."""
        return f'"{date_time.isoformat(sep=" ")}"'

    @staticmethod
    def _format_list(param_list: Iterable[Any]) -> str:
        """Return formatted list parameter."""
        fmt_list = [f'"{item}"' for item in param_list]
        return ",".join(fmt_list)

    @staticmethod
    def _custom_param_handler(query: str, param_dict: Dict[str, Any]) -> str:
        """Replace parameters in query template for Elastic JSON queries."""
        query_dict = json.loads(query)

        start = param_dict.pop("start", None)
        end = param_dict.pop("end", None)
        if start or end:
            time_range = {
                "range": {"@timestamp": {"format": "strict_date_optional_time"}}
            }
            if start:
                time_range["range"]["@timestamp"]["gte"] = start
            if end:
                time_range["range"]["@timestamp"]["lte"] = end
            query_dict["query"]["bool"]["filter"].append(time_range)

        add_query_items = param_dict.pop("add_query_items", None)
        if add_query_items:
            # "add_query_items" expects additional custom filter parameters
            # as a Python dict (e.g. add_query_items={"match_phrase: {"field": "value"}})
            query_dict["query"]["bool"]["filter"].extend(add_query_items)

        if param_dict:
            filter_terms = [
                {"match_phrase": {field: value}} for field, value in param_dict.items()
            ]
            query_dict["query"]["bool"]["filter"].extend(filter_terms)
        return json.dumps(query_dict, indent=2)
