# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""query manager helper functions for use with IPython/Jupyter queries."""
import sys
from functools import partial
import re
from typing import Union, List, Iterable, Mapping, Dict, Tuple, Any, Optional

from deprecated.sphinx import deprecated

from .query_schema import DataSchema
from .query_builtin_queries import query_definitions
from .query_defns import KqlQuery, QueryParamProvider, DataFamily, DataEnvironment
from .utility import export
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

# module constants
_DATA_FAMILY_NAME = "data_family"
_DATA_ENVIRONMENT_NAME = "data_environment"


# utility functions
@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def print_kql(query_string: str):
    """
    Print kql query stripped of comments and newline characters.

    Parameters
    ----------
    query_string : str
        The query string to print

    """
    clean_qry = re.sub(r"(//[^\"\'\n]+)", " ", query_string, re.MULTILINE).strip()
    for line in clean_qry.split("\n"):
        print(line.strip())


# @deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def clean_kql_query(query_string: str) -> str:
    """
    Return kql query stripped of comments and newline characters.

    Parameters
    ----------
    query_string : str
        Input query

    Returns
    -------
    str
        Cleaned query.

    """
    remove_comments = re.sub(r"(//[^\"\'\n]+)", " ", query_string, re.MULTILINE).strip()
    # get rid of newlines and returns
    return re.sub(r"(\s*\n\s*)", " ", remove_comments)


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def query_help(queryname: str):
    """
    Display help on the named query.

    Parameters
    ----------
    queryname : str
        The name of the query

    """
    if queryname not in query_definitions:
        print("Unknown query: ", queryname)
        return

    kql_query = query_definitions[queryname]
    print("Query: ", queryname)
    print(query_definitions[queryname].description)
    print("Designed to be executed with data_source: ", kql_query.data_source)
    print(
        "Supported data families: ",
        ", ".join([str(fam) for fam in kql_query.data_families]),
    )
    print(
        "Supported data environments: ",
        ", ".join([str(env) for env in kql_query.data_environments]),
    )

    req_params = required_params(kql_query.query)
    req_params.remove("table")
    req_params.remove("query_project")

    print("Query parameters:")
    print(req_params)
    if kql_query.optional_params:
        print("Optional parameters:")
        print(", ".join([str(param) for param in kql_query.optional_params]))
    print("Query:")
    print_kql(kql_query.query)


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def add_query(kql_query: Optional[KqlQuery] = None, **kwargs):
    """
    Add a query to the current set.

    Parameters
    ----------
    kql_query : KqlQuery, optional
        KqlQuery object to add
        (the default is None, which prints help)
    kwargs : Mapping[str, Any]
        If kql_query is not supplied the kwargs must
        include `name`, `query` and `data_source`
        keyword parameters.

    """
    if kql_query is None:
        def_data_families = [DataEnvironment.LogAnalytics]
        def_data_environments = [DataFamily.WindowsSecurity, DataFamily.LinuxSecurity]
        if "name" not in kwargs or "query" not in kwargs or "data_source" not in kwargs:
            raise ValueError(
                "If kql_query is not supplied the kwargs",
                " must include name, query and data_source.",
            )
        kql_query = KqlQuery(
            name=kwargs["name"],
            query=kwargs["query"],
            description=kwargs.get("description", None),
            data_source=kwargs["data_source"],
            data_families=kwargs.get("data_families", def_data_families),
            data_environments=kwargs.get("data_environments", def_data_environments),
        )
    query_definitions[kql_query.name] = kql_query
    _add_queries_to_module(__name__)

    kql_modules = [m for m in sys.modules if m.endswith("msticpy.nbtools.kql")]
    if len(kql_modules) == 1:
        _add_queries_to_module(kql_modules[0])


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def list_queries() -> List[str]:
    """Return list of currently defined queries."""
    return list(query_definitions.keys())


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def replace_query_params(query_name: str, *args, **kwargs) -> str:
    """
    Return the parameterized query for query_name.

    Parameters
    ----------
    query_name : str
        The query to use

    Other Parameters
    ----------------
    args : Tuple[QueryParamProvider]
        objects that implement QueryParamProvider
        (from which query parameters can be extracted).
    provs : Iterable[QueryParamProvider]
        this should be a collection of objects that
        implement QueryParamProvider (from which query
        parameters can be extracted).
    kwargs : Mapping[str, Any]
        custom parameter list to populate queries
        (override default values and values extracted
        from QueryParamProviders).

    Returns
    -------
    str
        Populated query

    Raises
    ------
    LookupError
        query_name cannot be found

    """
    return replace_prov_query_params(query_name=query_name, provs=args, **kwargs)


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def replace_prov_query_params(query_name: str, **kwargs) -> str:
    """
    Return the parameterized query for query_name.

    Parameters
    ----------
    query_name : str
        The query to use

    Other Parameters
    ----------------
    provs : Iterable[QueryParamProvider]
        this should be a collection of objects that
        implement QueryParamProvider (from which query
        parameters can be extracted).
    kwargs : Dict[str, Any]
        custom parameter list to populate queries
        (override default values and values extracted
        from QueryParamProviders).

    Returns
    -------
    str
        Populated query

    Raises
    ------
    LookupError
        query_name cannot be found
    ValueError
        query parameter value could not be found.

    """
    if query_name not in query_definitions:
        raise LookupError(f'Unknown query "{query_name}"')

    kql_query = query_definitions[query_name]
    if "provs" in kwargs:
        p_args = kwargs.pop("provs")
        query_params = _get_query_params(kql_query, *p_args, **kwargs)
    else:
        query_params = _get_query_params(kql_query, **kwargs)
    return kql_query.query.format(**query_params)


# pylint: disable=duplicate-code
def _get_query_params(kql_query: KqlQuery, *args, **kwargs) -> Dict[str, Any]:
    """
    Get the parameters needed for the query.

    Parameters
    ----------
    kql_query : KqlQuery
        query object

    args : Tuple[QueryParamProvider]
        objects that implement QueryParamProvider
        (from which query parameters can be extracted).
    kwargs : Dict[str, Any]
        custom parameter list to populate queries
        (override default values and values extracted
        from QueryParamProviders).

    Returns
    -------
    Dict[str, Any]
        Dictionary of parameter names and values to be used
            in the query

    """
    # get the required parameters for this query and build a dictionary
    req_param_names = required_params(kql_query.query)
    req_params: Dict[str, Any] = {param: None for param in req_param_names}

    # Iterate through required parameters. If any are set in the supplied
    # provider objects, assign them to our output dictionary
    query_providers = [prov for prov in args if isinstance(prov, QueryParamProvider)]
    for provider in query_providers:
        for param in req_param_names:
            if param in provider.query_params:
                req_params[param] = provider.query_params[param]

    # If any custom parameters have been supplied add these
    # overriding any parameters from the QueryParamProviders
    if kwargs:
        req_params.update(kwargs)

    data_family, data_environment = _get_data_family_and_env(
        kql_query, query_providers, kwargs
    )

    if not data_family:
        supp_families = ", ".join(DataSchema.get_data_families())
        raise LookupError(
            "Could not find a valid data_family value. "
            f"Valid families are: {supp_families}"
        )
    if not data_environment:
        supp_envs = ", ".join(DataSchema.get_data_environments())
        raise LookupError(
            "Could not find a valid data_environment value. "
            f"Valid environments are: {supp_envs}"
        )

    # Create the data schema and get any unset parameters from
    # the data schema
    data_schema = DataSchema(
        environment=data_environment,
        data_family=data_family,
        data_source=kql_query.data_source,
    )

    for param, value in req_params.items():
        if not value and param in data_schema:
            req_params[param] = data_schema[param]

    # If we have missing parameters try to retrieve them
    # as attributes of the object
    missing_params = [p_name for p_name, p_value in req_params.items() if not p_value]
    if missing_params:
        _get_missing_params(args, missing_params, req_params, kql_query)

    return req_params


# pylint: enable=duplicate-code


def _get_missing_params(
    args: Tuple[Any, ...],
    missing_params: List[str],
    req_params: Dict[str, Any],
    kql_query: KqlQuery,
):
    """
    Get missing params from arguments.

    Parameters
    ----------
    args : Tuple[Any]
        Args list from calling funtion
    missing_params : List[str]
        The list of missing parameters to get
    req_params : Dict[str, str]
        Dictionary of required parameters
    kql_query : KqlQuery
        The query object

    """
    for other_object in [
        obj for obj in args if not isinstance(obj, QueryParamProvider)
    ]:
        for m_param in missing_params:
            if m_param in other_object:
                req_params[m_param] = getattr(other_object, m_param)
        missing_params = [
            p_name for p_name, p_value in req_params.items() if not p_value
        ]

    if missing_params:
        # check for and remove optional parameters from the missing params list
        for m_param in missing_params:
            if m_param in kql_query.optional_params:
                req_params[m_param] = ""
        missing_params = [
            p_name
            for p_name in missing_params
            if p_name not in kql_query.optional_params
        ]

    if missing_params:
        # If still have missing params then we error out
        query_help(kql_query.name)
        mssg = (
            "The following required parameters for this query were not set:",
            ", ".join(missing_params),
        )
        raise ValueError(mssg)


def _get_data_family_and_env(  # noqa: C901
    kql_query: KqlQuery,
    providers: Iterable[QueryParamProvider],
    custom_params: Mapping[str, Any],
) -> Tuple[Optional[DataFamily], Optional[DataEnvironment]]:
    """Get the data_family and environment."""
    data_family = None  # type: Optional[DataFamily]
    data_environment = None  # type: Optional[DataEnvironment]

    # If there is only one data family for this query, then use that
    if len(kql_query.data_families) == 1:
        data_family = kql_query.data_families[0]
    if len(kql_query.data_environments) == 1:
        data_environment = kql_query.data_environments[0]

    if data_family and data_environment:
        return data_family, data_environment

    candidate_families = set()
    candidate_environments = set()
    for provider in providers:
        family, env = _get_env_and_family(provider.query_params)
        if family != DataFamily.Unknown:
            candidate_families.add(family)
        if env != DataEnvironment.Unknown:
            candidate_environments.add(env)

    if custom_params:
        # If we haven't yet worked out the data family and environment
        # try to get this from one of custom_params
        family, env = _get_env_and_family(custom_params)

        if family != DataFamily.Unknown:
            candidate_families.add(family)
        if env != DataEnvironment.Unknown:
            candidate_environments.add(env)

    # get the intersection of families and environments that we found and those
    # supported by the query. If it is 1 item we are good to go.
    usable_families = candidate_families & set(kql_query.data_families)
    if len(usable_families) == 1:
        data_family = usable_families.pop()
    usable_environments = candidate_environments & set(kql_query.data_environments)
    if len(usable_environments) == 1:
        data_environment = usable_environments.pop()

    return data_family, data_environment


def _get_env_and_family(
    params: Mapping[str, Any]
) -> Tuple[Optional[DataFamily], Optional[DataEnvironment]]:
    """
    Extract environment and family from params dictionary.

    Parameters
    ----------
    params : Mapping[str, Any]
        Input dictionary

    Returns
    -------
    Tuple[Optional[DataFamily],Optional[DataEnvironment]]
        Tuple of family and environment, if found.

    """
    family = None
    environment = None

    if _DATA_FAMILY_NAME in params:
        family = DataFamily.parse(params[_DATA_FAMILY_NAME])
    if _DATA_ENVIRONMENT_NAME in params:
        environment = DataEnvironment.parse(params[_DATA_ENVIRONMENT_NAME])
    return family, environment


@deprecated(reason="Superceded by msticpy.data.QueryProvider", version="0.2.0")
@export
def required_params(kql_query: Union[KqlQuery, str]) -> List[str]:
    """
    Return the set of required parameters for the query.

    Parameters
    ----------
    kql_query : Union[KqlQuery, str]
        The KqlQuery object or Kql string

    Returns
    -------
    List[str]
        The list of required parameters.

    """
    if isinstance(kql_query, KqlQuery):
        query_string = kql_query.query
    else:
        query_string = kql_query
    param_pattern = r"{([\w\d_-]+)}"
    return list(set(re.findall(param_pattern, query_string)))


# pylint: disable=duplicate-code
def _add_queries_to_module(module_name):
    """Add queries to the module as callable methods."""
    if module_name not in sys.modules:
        raise LookupError(f"Module {module_name} was not found sys.modules")
    for query_name in query_definitions:
        module = sys.modules[module_name]
        query_func = partial(replace_prov_query_params, query_name=query_name)
        query_func.__doc__ = replace_prov_query_params.__doc__
        setattr(module, query_name, query_func)


# pylint: disable=duplicate-code


# Add all queries defined in builtin queries module as functions
_add_queries_to_module(__name__)
