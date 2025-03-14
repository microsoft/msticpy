# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider loader."""
from __future__ import annotations

import logging
from functools import partial
from pathlib import Path
from typing import TYPE_CHECKING, Any, Iterable

from typing_extensions import Self

from ..._version import VERSION
from ...common.pkg_config import get_config
from ...common.utility import export, valid_pyname
from ...nbwidgets.query_time import QueryTime
from .. import drivers
from ..drivers.driver_base import DriverBase, DriverProps
from .param_extractor import extract_query_params
from .query_container import QueryContainer
from .query_defns import DataEnvironment, DataFamily
from .query_provider_connections_mixin import QueryProviderConnectionsMixin
from .query_provider_utils_mixin import QueryProviderUtilsMixin
from .query_store import QueryStore

if TYPE_CHECKING:
    import pandas as pd

    from msticpy.data.core.query_source import QuerySource

__version__ = VERSION
__author__ = "Ian Hellen"

_HELP_FLAGS: tuple[str, str] = ("help", "?")
_DEBUG_FLAGS: tuple[str, str, str] = ("print", "debug_query", "print_query")
_COMPATIBLE_DRIVER_MAPPINGS: dict[str, list[str]] = {
    "mssentinel": ["m365d"],
    "mde": ["m365d"],
    "mssentinel_new": ["mssentinel", "m365d"],
    "kusto_new": ["kusto"],
    "m365dgraph": ["mde", "m365d"],
}

logger: logging.Logger = logging.getLogger(__name__)


# These are mixin classes that do not have an __init__ method
# pylint: disable=super-init-not-called
@export
class QueryProvider(QueryProviderConnectionsMixin, QueryProviderUtilsMixin):
    """
    Container for query store and query execution provider.

    Instances of this class hold the query set and execution
    methods for a specific data environment.

    """

    def __init__(
        self: QueryProvider,
        data_environment: str | DataEnvironment,
        driver: DriverBase | None = None,
        query_paths: list[str] | None = None,
        **kwargs,
    ) -> None:
        """
        Query provider interface to queries.

        Parameters
        ----------
        data_environment : Union[str, DataEnvironment]
            Name or Enum of environment for the QueryProvider
        driver : DriverBase, optional
            Override the builtin driver (query execution class)
            and use your own driver (must inherit from
            `DriverBase`)
        query_paths : List[str]
            Additional paths to look for query definitions.
        kwargs :
            Other arguments are passed to the data provider driver.

        Notes
        -----
        Additional keyword arguments are passed to the data provider driver.
        The driver may support additional keyword arguments, use
        the QueryProvider.driver_help() method to get a list of these
        parameters.

        See Also
        --------
        DataProviderBase : base class for data query providers.

        """
        # import at runtime to prevent circular import
        # pylint: disable=import-outside-toplevel, cyclic-import
        from ...init.pivot_init.pivot_data_queries import add_data_queries_to_entities

        # pylint: enable=import-outside-toplevel
        setattr(self.__class__, "_add_pivots", add_data_queries_to_entities)

        data_environment, self.environment_name = QueryProvider._check_environment(
            data_environment,
        )

        self._driver_kwargs: dict[str, Any] = kwargs.copy()
        if driver is None:
            self.driver_class: type[DriverBase] = drivers.import_driver(
                data_environment
            )
            if issubclass(self.driver_class, DriverBase):
                driver = self.driver_class(data_environment=data_environment, **kwargs)
            else:
                err_msg: str = (
                    f"Could not find suitable data provider for {data_environment}"
                )
                raise LookupError(err_msg)
        else:
            self.driver_class = driver.__class__
        # allow the driver to override the data environment used for selecting queries
        self.environment_name = (
            driver.get_driver_property(DriverProps.EFFECTIVE_ENV)
            or self.environment_name
        )
        logger.info("Using data environment %s", self.environment_name)
        logger.info("Driver class: %s", self.driver_class.__name__)

        self._additional_connections: dict[str, DriverBase] = {}
        self._query_provider = driver
        # replace the connect method docstring with that from
        # the driver's connect method
        self.__class__.connect.__doc__ = driver.connect.__doc__
        self.all_queries: QueryContainer = QueryContainer()

        # Add any query files
        data_env_queries: dict[str, QueryStore] = {}
        self._query_paths: list[str] | None = query_paths
        if driver.use_query_paths:
            logger.info("Using query paths %s", query_paths)
            data_env_queries.update(
                self._read_queries_from_paths(query_paths=query_paths),
            )
        self.query_store: QueryStore = data_env_queries.get(
            self.environment_name,
            QueryStore(self.environment_name),
        )
        logger.info("Adding query functions to provider")
        self._add_query_functions()
        self._query_time = QueryTime(units="day")
        logger.info("Initialization complete.")

    @classmethod
    def _check_environment(
        cls: type[Self],
        data_environment: str | DataEnvironment,
    ) -> tuple[str | DataEnvironment, str]:
        """Check environment against known names."""
        if isinstance(data_environment, str):
            data_env: DataEnvironment = DataEnvironment.parse(data_environment)
            if data_env != DataEnvironment.Unknown:
                data_environment = data_env
                environment_name: str = data_environment.name
            elif data_environment in drivers.CUSTOM_PROVIDERS:
                environment_name = data_environment
            else:
                err_msg: str = f"Unknown data environment {data_environment}"
                raise TypeError(err_msg)
        elif isinstance(data_environment, DataEnvironment):
            environment_name = data_environment.name
        else:
            err_msg = f"Unknown data environment type {data_environment} ({type(data_environment)})"
            raise TypeError(err_msg)
        return data_environment, environment_name

    def __getattr__(self: Self, name: str) -> Any:
        """Return the value of the named property 'name'."""
        if "." in name:
            parent_name, child_name = name.split(".", maxsplit=1)
            parent = getattr(self, parent_name, None)
            if parent:
                return getattr(parent, child_name)
        err_msg: str = f"{name} is not a valid attribute."
        raise AttributeError(err_msg)

    @property
    def environment(self: Self) -> str:
        """Return the environment name."""
        return self.environment_name

    def connect(self: Self, connection_str: str | None = None, **kwargs) -> None:
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : str
            Connection string for the data source

        """
        logger.info("Calling connect on driver")
        self._query_provider.connect(connection_str=connection_str, **kwargs)

        # If the driver has any attributes to expose via the provider
        # add those here.
        for attr_name, attr in self._query_provider.public_attribs.items():
            setattr(self, attr_name, attr)

        refresh_query_funcs: bool = False
        # if the driver supports dynamic filtering of queries,
        # filter the query store based on connect-time parameters
        if self._query_provider.filter_queries_on_connect:
            self.query_store.apply_query_filter(self._query_provider.query_usable)
            refresh_query_funcs = True
        # Add any built-in or dynamically retrieved queries from driver
        if self._query_provider.has_driver_queries:
            logger.info("Adding driver queries to provider")
            driver_queries: Iterable[dict[str, Any]] = (
                self._query_provider.driver_queries
            )
            self._add_driver_queries(queries=driver_queries)
            refresh_query_funcs = True

        if refresh_query_funcs:
            self._add_query_functions()

        # Since we're now connected, add Pivot functions
        logger.info("Adding query pivot functions")
        self._add_pivots(lambda: self._query_time.timespan)

    @property
    def query_time(self: Self) -> QueryTime:
        """Return the default QueryTime control for queries."""
        return self._query_time

    def _execute_query(
        self: Self,
        *args: str,
        query_name: str,
        query_path: str | DataFamily | None = None,
        split_query_by: str | None = None,
        split_by: str | None = None,
        **kwargs,
    ) -> pd.DataFrame | str | None:
        if not self._query_provider.loaded:
            err_msg: str = "Provider is not loaded."
            raise ValueError(err_msg)
        if (
            not self._query_provider.connected
            and not _help_flag(*args)
            and not _debug_flag(*args, **kwargs)
        ):
            err_msg = (
                "No connection to a data source."
                " Please call connect(connection_str) and retry."
            )
            raise ValueError(err_msg)

        query_source: QuerySource = self.query_store.get_query(
            query_path=query_path,
            query_name=query_name,
        )
        if _help_flag(*args):
            query_source.help()
            return None

        params, missing = extract_query_params(query_source, *args, **kwargs)
        logger.debug("Template query: %s", query_source.query)
        logger.info("Parameters for query: %s", params)
        query_options: dict[str, bool] = {
            "default_time_params": self._check_for_time_params(params, missing),
        }
        if missing:
            query_source.help()
            err_msg = f"No values found for these parameters: {missing}"
            raise ValueError(err_msg)

        split_by = split_query_by or split_by
        if split_by:
            logger.info("Split query selected - interval - %s", split_by)
            split_result: pd.DataFrame | str | None = self._exec_split_query(
                split_by=split_by,
                query_source=query_source,
                query_params=params,
                debug=_debug_flag(*args, **kwargs),
                args=args,
                **kwargs,
            )
            if split_result is not None:
                return split_result
            # if split queries could not be created, fall back to default
        query_str: str = query_source.create_query(
            formatters=self._query_provider.formatters,
            **params,
        )
        # This looks for any of the "print query" debug args in args or kwargs
        if _debug_flag(*args, **kwargs):
            return query_str

        # Handle any query options passed and run the query
        query_options.update(self._get_query_options(params, kwargs))
        logger.info(
            "Running query '%s...' with params: %s",
            query_str[:40],
            query_options,
        )
        return self.exec_query(query_str, query_source=query_source, **query_options)

    def _check_for_time_params(
        self: Self,
        params: dict[str, Any],
        missing: list[str],
    ) -> bool:
        """Fall back on builtin query time if no time parameters were supplied."""
        defaults_added = False
        if "start" in missing:
            missing.remove("start")
            params["start"] = self._query_time.start
            defaults_added = True
        if "end" in missing:
            missing.remove("end")
            params["end"] = self._query_time.end
            defaults_added = True
        return defaults_added

    def _get_query_folder_for_env(
        self: Self,
        root_path: str,
        environment: str,
    ) -> list[Path]:
        """Return query folder for current environment."""
        data_envs: list[str] = [environment]
        if environment.casefold() in _COMPATIBLE_DRIVER_MAPPINGS:
            data_envs += _COMPATIBLE_DRIVER_MAPPINGS[environment.casefold()]
        return [
            _resolve_package_path(root_path).joinpath(data_env.casefold())
            for data_env in data_envs
        ]

    def _read_queries_from_paths(
        self: Self,
        query_paths: list[str] | None,
    ) -> dict[str, QueryStore]:
        """Fetch queries from YAML files in specified paths."""
        settings: dict[str, Any] = get_config("QueryDefinitions", {})
        all_query_paths: list[Path | str] = []
        for def_qry_path in settings.get("Default", []):
            # only read queries from environment folder
            builtin_qry_paths: list[Path] = self._get_query_folder_for_env(
                def_qry_path,
                self.environment_name,
            )
            all_query_paths.extend(
                str(qry_path) for qry_path in builtin_qry_paths if qry_path.is_dir()
            )

        for custom_path in settings.get("Custom", {}):
            custom_qry_path: str | None = _resolve_path(custom_path)
            if custom_qry_path:
                all_query_paths.append(custom_qry_path)
        for param_path in query_paths or []:
            param_qry_path: str | None = _resolve_path(param_path)
            if param_qry_path:
                all_query_paths.append(param_qry_path)
        if all_query_paths:
            logger.info("Reading queries from %s", all_query_paths)
            return QueryStore.import_files(
                source_path=all_query_paths,
                recursive=True,
                driver_query_filter=self._query_provider.query_attach_spec,
            )
        # if no queries - just return an empty store
        return {self.environment_name: QueryStore(self.environment_name)}

    def _add_query_functions(self: Self) -> None:
        """Add queries to the module as callable methods."""
        for qual_query_name in self.list_queries():
            query_path: list[str] = qual_query_name.split(".")
            query_name: str = query_path[-1]
            current_node: Self | QueryContainer = self
            for container_name in query_path[:-1]:
                valid_container: str = valid_pyname(container_name)
                if hasattr(current_node, valid_container):
                    current_node = getattr(current_node, valid_container)
                else:
                    new_node: QueryContainer = QueryContainer()
                    setattr(current_node, valid_container, new_node)
                    current_node = new_node

            query_cont_name: str = ".".join(query_path[:-1])

            # Create the partial function
            query_func = partial(
                self._execute_query,
                query_path=query_cont_name,
                query_name=query_name,
            )
            query_func.__doc__ = self.query_store.get_query(
                query_path=query_cont_name,
                query_name=query_name,
            ).create_doc_string()

            query_name = valid_pyname(query_name)
            setattr(current_node, query_name, query_func)
            setattr(self.all_queries, query_name, query_func)

    def _add_driver_queries(self: Self, queries: Iterable[dict[str, str]]) -> None:
        """Add driver queries to the query store."""
        for query in queries:
            self.query_store.add_query(
                name=query["name"],
                query=query["query"],
                query_paths=query.get(
                    "query_paths",
                    query.get("query_container", "default"),
                ),
                description=query["description"],
            )
        # For now, just add all of the functions again (with any connect-time acquired
        # queries) - we could be more efficient than this but unless there are 1000s of
        # queries it should not be noticeable.
        self._add_query_functions()

    @staticmethod
    def _get_query_options(
        params: dict[str, Any],
        kwargs: dict[str, Any],
    ) -> dict[str, Any]:
        # sourcery skip: inline-immediately-returned-variable, use-or-for-fallback
        """Return any kwargs not already in params."""
        query_options: dict[str, Any] = kwargs.pop("query_options", {})
        if not query_options:
            # Any kwargs left over we send to the query provider driver
            query_options = {
                key: val for key, val in kwargs.items() if key not in params
            }
        query_options["time_span"] = {
            "start": params.get("start"),
            "end": params.get("end"),
        }
        return query_options


def _resolve_package_path(config_path: str) -> Path:
    """Resolve path relative to current package."""
    return (
        Path(config_path)
        if Path(config_path).is_absolute()
        else Path(__file__).resolve().parent.parent.joinpath(config_path)
    )


def _resolve_path(config_path: str) -> str | None:
    """Resolve path."""
    if not Path(config_path).is_absolute():
        config_path = str(Path(config_path).expanduser().resolve())
    if not Path(config_path).is_dir():
        logger.warning(
            "Warning: Custom query definitions path %s not found",
            config_path,
        )
        return None
    return config_path


def _help_flag(*args: str) -> bool:
    """Return True if help parameter passed."""
    return any(help_flag for help_flag in _HELP_FLAGS if help_flag in args)


def _debug_flag(*args: str, **kwargs: str) -> bool:
    """Return True if debug/print args passed."""
    return any(db_arg for db_arg in _DEBUG_FLAGS if db_arg in args) or any(
        db_arg for db_arg in _DEBUG_FLAGS if kwargs.get(db_arg, False)
    )
