# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Intake kql driver."""
from datetime import datetime, timedelta
from numbers import Number
import re
from typing import Dict, List, Tuple, Optional, Union
from collections import ChainMap

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def _value_or_default(src_dict: Dict, prop_name: str, default: Dict):
    """Return value from dict or emtpy dict."""
    src_value = src_dict.get(prop_name)
    return src_value if src_value is not None else default


# pylint: disable=too-many-instance-attributes
class QuerySource:
    """
    Query definition class for templated queries.

    Attributes
    ----------
    name: str
        The query name
    metadata: Dict[str, Any]
        The consolidated metadata for the query
    params: dict[str, Any]
        The dictionary of parameter definitions for the query.
    query_store: QueryStore
        The query store object that the query belongs to

    """

    def __init__(self, name: str, source: dict, defaults: dict, metadata: dict):
        """
        Initialize query source definition.

        Parameters
        ----------
        name : str
            The query name
        source : dict
            The data source definition settings
        defaults : dict
            The default settings (if source-specific setting
            not supplied)
        metadata : dict
            The global metadata from the source file.

        Notes
        -----
        A data source can belong to multiple families (e.g. a query
        that joins data from several sources)

        """
        self.name = name
        self._source = source
        self.defaults = defaults
        self._global_metadata = dict(metadata) if metadata else dict()
        self.query_store: Optional["QueryStore"] = None  # type: ignore

        # consolidate source metadata - source-specifc
        # overrides global
        # add an empty dict in case neither has defined params
        self.metadata = ChainMap(
            _value_or_default(self._source, "metadata", {}),
            _value_or_default(self.defaults, "metadata", {}),
            self._global_metadata,
        )
        # make ChainMap for parameters from with source
        # higher priority than default
        # add an empty dict in case neither has defined params
        self.params = ChainMap(
            _value_or_default(self._source, "parameters", {}),
            _value_or_default(self.defaults, "parameters", {}),
        )

        self._query = self["args.query"]
        self._replace_query_macros()

    def __getitem__(self, key: str):
        """
        Getitem override - allows access to properties via dotted notation.

        Parameters
        ----------
        key : str
            The hiearchical path to the property (e.g. `source.description`)

        """
        path_elems = key.split(".")
        cur_node = self._source
        for elem in path_elems:
            cur_node = cur_node.get(elem, None)
            if cur_node is None:
                raise KeyError(f"{elem} value of {key} is not a valid path")
        return cur_node

    @property
    def description(self) -> str:
        """
        Return description of the query.

        Returns
        -------
        str
            Query description.

        """
        return self["description"]

    @property
    def query(self) -> str:
        """
        Return the query template.

        Returns
        -------
        str
            The template query.

        """
        return self._query

    @property
    def default_params(self) -> Dict[str, dict]:
        """
        Return the set of parameters with default values.

        Returns
        -------
        Iterable[dict]
            List of parameters

        """
        return {
            p_key: p_props
            for p_key, p_props in self.params.items()
            if "default" in p_props
        }

    @property
    def required_params(self) -> Dict[str, dict]:
        """
        Return the set of parameters with no default values.

        Returns
        -------
        Iterable[dict]
            List of parameters

        """
        return {
            p_key: p_props
            for p_key, p_props in self.params.items()
            if "default" not in p_props
        }

    @property
    def data_families(self) -> List[str]:
        """
        Return the list of data families used by the query.

        Returns
        -------
        List[str]
            The list of data families. A data family is
            usually equivalent to a table or entity set.

        """
        return self.metadata["data_families"]

    def create_query(self, **kwargs) -> str:
        """
        Return query with values from kwargs and defaults substituted.

        Parameters
        ----------
        kwargs: Mapping[str, Any]
            Set of parameter name, value pairs used to
            populate the template query.

        Returns
        -------
        str
            The populated query

        Raises
        ------
        ValueError
            If one or more parameters with no default values
            are not supplied.

        Notes
        -----
        Parameters supplied as arguments will override any
        parameter defaults (see `default_params` property).

        """
        param_dict = {
            name: value.get("default", None) for name, value in self.params.items()
        }
        param_dict.update(kwargs)
        missing_params = {
            name: value for name, value in param_dict.items() if value is None
        }
        if missing_params:
            raise ValueError(
                "These required parameters were not set: ", f"{missing_params.keys()}"
            )

        # Handle formatting for datetimes and cases where a format
        # template has been supplied
        for p_name, settings in self.params.items():
            param_value = param_dict[p_name]
            # special case of datetime specified as a number - we
            # interpret this as an offset from utcnow
            if settings["type"] == "datetime":
                if isinstance(param_value, datetime):
                    param_dict[p_name] = param_value
                elif isinstance(param_value, Number):
                    param_dict[p_name] = datetime.utcnow() + timedelta(  # type: ignore
                        param_value
                    )
                else:
                    tm_delta = self._parse_timedelta(str(param_value))
                    param_dict[p_name] = datetime.utcnow() + tm_delta

            if settings["type"] == "list":
                param_dict[p_name] = self._parse_param_list(param_value)

            # if the parameter requires custom formatting
            fmt_template = settings.get("format", None)
            if fmt_template:
                param_dict[p_name] = fmt_template.format(param_dict[p_name])
            elif settings["type"] == "datetime" and isinstance(
                param_dict[p_name], datetime
            ):
                # If this is a datetime and no specific formatting requested,
                # format as a isoformat (Odata requires strings with no
                # spaces and TZ suffix)
                param_dict[p_name] = param_dict[p_name].isoformat(sep="T") + "Z"

        return self._query.format(**param_dict)

    @staticmethod
    def _parse_timedelta(time_range: str = "0") -> timedelta:
        """Parse time period string and return equivalent timedelta."""
        tr_regex = r"(?P<sign>[+\-]?)\s*(?P<value>[\d]+)\s*(?P<unit>[wdhm]?)"
        m_time = re.match(tr_regex, time_range)

        if not m_time or "value" not in m_time.groupdict():
            return timedelta(0)
        days = weeks = secs = 0
        tm_val = int(m_time.groupdict()["sign"] + m_time.groupdict()["value"])
        tm_unit = (
            m_time.groupdict()["unit"].lower() if m_time.groupdict()["unit"] else "d"
        )
        if tm_unit == "d" or tm_unit not in "wdhm":
            days = tm_val
        elif tm_unit == "w":
            weeks = tm_val
        elif tm_unit == "h":
            secs = tm_val * 60 * 60
        elif tm_unit == "m":
            secs = tm_val * 60
        return timedelta(days=days, weeks=weeks, seconds=secs)

    @staticmethod
    def _parse_param_list(param_value: Union[str, List]) -> str:
        """Parse list, comma-delim str or str."""
        if isinstance(param_value, list):
            return ",".join([f"'{item}'" for item in param_value])
        if isinstance(param_value, str) and "," in param_value:
            return ",".join([f"'{item.strip()}'" for item in param_value.split(",")])
        return f"'{param_value}'"

    def help(self):
        """Print help for query."""
        print("Query: ", self.name)
        if self.query_store is not None:
            print("Data source: ", self.query_store.environment)
        print(self.create_doc_string())
        print("Query:")
        print(self.query)

    def create_doc_string(self) -> str:
        """
        Return a doc string for the query.

        Returns
        -------
        str
            New-line delimited docstring dynamically
            created from query definition properties.

        """
        param_block = ["Parameters", "----------"]
        for p_name, p_props in sorted(self.params.items()):
            if "default" in p_props:
                optional = " (optional)"
                def_value = p_props["default"]
                if isinstance(def_value, str) and len(def_value) > 50:
                    def_value = def_value[:50] + "..."
            else:
                optional = ""
                def_value = None
            param_block.append(f'{p_name}: {p_props.get("type", "Any")}{optional}')
            param_block.append(f'    {p_props.get("description", "no description")}')
            if def_value:
                param_block.append(f"    (default value is: {def_value})")
        doc_string = [f"{self.description}", ""]
        return "\n".join(doc_string + param_block)

    def validate(self) -> Tuple[bool, List[str]]:
        """
        Validate the source to ensure that all required properties are present.

        Returns
        -------
        bool
            True if validation is successful.

        """
        req_source_items = {"args"}
        # match items surrounded by single {} but not double
        param_pattern = r"(?!{{){([^}]+)}(?!})"

        valid_failures = []

        # Need req_source_items AND query item to be present
        source_props = self._source.keys() | self.defaults.keys()
        if not req_source_items.issubset(source_props):
            msg = (
                f"Source {self.name} does not have all required "
                + f"elements: {req_source_items - source_props}"
            )
            valid_failures.append(msg)
        if not self._query:
            msg = (
                f'Source {self.name} does not have "query" property '
                + "in args element."
            )
            valid_failures.append(msg)

        # Now get the query and the parameter definitions from the source and
        # check that every parameter specified in the query has a corresponding
        # 'parameter definition in either the source or the defaults.
        source_params = self.params.keys()
        q_params = set(re.findall(param_pattern, self._query))

        missing_params = q_params - source_params
        if missing_params:
            msg = (
                f"Source {self.name} has parameters that are defined in "
                + "the query but not included in either defaults or "
                + "query-specific parameters element(s)\n"
                + f"Missing parameters are {missing_params}"
            )
            valid_failures.append(msg)

        missing_types = {
            p_name for p_name, p_props in self.params.items() if "type" not in p_props
        }
        if missing_types:
            msg = (
                f"Source {self.name} has parameters that are defined in "
                + 'the query but do not have a valid "type" property\n'
                + f"Parameters with missing types are {missing_types}"
            )
            valid_failures.append(msg)
        return (not valid_failures, valid_failures)

    def _replace_query_macros(self):
        """Replace any macro strings in the query with substitutions."""
        replace_keys = re.findall(r"\$\<([^>]+)\>\$?", self._query)
        if not replace_keys:
            return
        replace_values = {}
        if "query_macros" in self._source:
            replace_values = {
                name: properties.get("value", "")
                for name, properties in self["query_macros"].items()
            }
        for key in replace_keys:
            if key in replace_keys:
                replacement = replace_values.get(key, "")
                self._query = self._query.replace(f"$<{key}>$", replacement)
        self._query = re.sub("\n{2,}", "\n", self._query)
