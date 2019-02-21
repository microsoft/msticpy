# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""query manager helper functions for use with IPython/Juptyer queries."""
import sys
from functools import partial
import re

from . query_schema import DataSchema
from . query_builtin_queries import query_definitions
from . query_defns import KqlQuery, QueryParamProvider, DataFamily, DataEnvironment
from . utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

# module constants
_DATA_FAMILY_NAME = 'data_family'
_DATA_ENVIRONMENT_NAME = 'data_environment'


# utility functions
@export
def print_kql(query_string: str):
    """Print kql query stripped of comments and newline characters."""
    clean_qry = re.sub(r'(//[^\"\'\n]+)', ' ', query_string, re.MULTILINE).strip()
    for line in clean_qry.split('\n'):
        print(line.strip())


@export
def clean_kql_query(query_string: str) -> str:
    """Return kql query stripped of comments and newline characters."""
    remove_comments = re.sub(r'(//[^\"\'\n]+)', ' ', query_string, re.MULTILINE).strip()
    # get rid of newlines and returns
    return re.sub(r'(\s*\n\s*)', ' ', remove_comments)


@export
def query_help(queryname: str):
    """Print query usage."""
    if queryname not in query_definitions:
        print('Unknown query: ', queryname)
        return

    kql_query = query_definitions[queryname]
    print('Query: ', queryname)
    print(query_definitions[queryname].description)
    print('Designed to be executed with data_source: ',
          kql_query.data_source)
    print('Supported data families: ',
          ', '.join([str(fam) for fam in kql_query.data_families]))
    print('Supported data environments: ',
          ', '.join([str(env) for env in kql_query.data_environments]))

    req_params = required_params(kql_query.query)
    req_params.remove('table')
    req_params.remove('query_project')

    print('Query parameters:')
    print(req_params)
    if kql_query.optional_params:
        print('Optional parameters:')
        print(', '.join([str(param) for param in kql_query.optional_params]))
    print('Query:')
    print_kql(kql_query.query)


@export
def add_query(kql_query: KqlQuery = None, **kwargs):
    """
    Add a query to the current set.

        :param kql_query:KqlQuery:
    """
    if kql_query is None:
        def_data_families = [DataEnvironment.LogAnalytics]
        def_data_environments = [
            DataFamily.WindowsSecurity, DataFamily.LinuxSecurity]
        if 'name' not in kwargs or 'query' not in kwargs or 'data_source' not in kwargs:
            raise ValueError(
                'If kql_query is not supplied the kwargs must include name, query and data_source.')
        kql_query = KqlQuery(name=kwargs['name'],
                             query=kwargs['query'],
                             description=kwargs.get('description', None),
                             data_source=kwargs['data_source'],
                             data_families=kwargs.get(
                                 'data_families', def_data_families),
                             data_environments=kwargs.get('data_environments',
                                                          def_data_environments))
    query_definitions[kql_query.name] = kql_query
    _add_queries_to_module(__name__)

    kql_modules = [m for m in sys.modules if m.endswith('msticpy.nbtools.kql')]
    if len(kql_modules) == 1:
        _add_queries_to_module(kql_modules[0])


@export
def list_queries():
    """Return list of currently defined queries."""
    return list(query_definitions.keys())


@export
def replace_query_params(query_name: str, *args, **kwargs) -> str:
    """
    Return the parameterized query for query_name.

    Arguments:
        query_name {string}: The query to use
        args {QueryParamProvider}: objects that
                implement QueryParamProvider (from which query
                parameters can be extracted).
        kwargs:
            {string:[QueryParamProvider]} - for the key 'provs'
                this should be a collection of objects that
                implement QueryParamProvider (from which query
                parameters can be extracted).
                OR
            {string:value pairs} -- custom parameter list
                (override default values and values extracted
                from QueryParamProviders).
    Raises:
        LookupError -- query_name cannot be found

    Returns:
        string -- substituted query

    """
    return replace_prov_query_params(query_name=query_name,
                                     provs=args,
                                     **kwargs)


@export
def replace_prov_query_params(query_name: str, **kwargs) -> str:
    """
    Return the parameterized query for query_name.

    Arguments:
        query_name {string} -- The query to use
        kwargs
            {string:[QueryParamProvider]} - for the key 'provs'
                this should be a collection of objects that
                implement QueryParamProvider (from which query
                parameters can be extracted).
                OR
            {string:value pairs} -- custom parameter list
                (override default values and values extracted
                from QueryParamProviders).
    Raises:
        LookupError -- query_name cannot be found.
        ValueError -- query parameter value could not be found.

    Returns:
        string -- substituted query

    """
    if query_name not in query_definitions:
        raise LookupError(f'Unknown query "{query_name}"')

    kql_query = query_definitions[query_name]
    if 'provs' in kwargs:
        p_args = kwargs.pop('provs')
        query_params = _get_query_params(kql_query, *p_args, **kwargs)
    else:
        query_params = _get_query_params(kql_query, **kwargs)
    return kql_query.query.format(**query_params)


def _get_query_params(kql_query, *args, **kwargs):
    """
    Get the parameters needed for the query.

    Arguments:
        kql_query {KqlQuery} -- query object
        args {list} -- set of source objects to extract parameter
            values from
        kwargs {string:value pairs} -- custom parameter list
            (overrides auto-extracted values)

    Raises:
        LookupError -- Could not find valid data_family or
            data_environment
        ValueError -- Values for one or more required parameters
            could not be found

    Returns:
        dict -- Dictionary of parameter names and values to be used
            in the query

    """
    # get the required parameters for this query and build a dictionary
    req_param_names = required_params(kql_query.query)
    req_params = {param: None for param in req_param_names}

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

    data_family, data_environment = _get_data_family_and_env(kql_query, query_providers, kwargs)

    if not data_family:
        supp_families = ', '.join(DataSchema.get_data_families())
        raise LookupError('Could not find a valid data_family value. '
                          f'Valid families are: {supp_families}')
    if not data_environment:
        supp_envs = ', '.join(DataSchema.get_data_environments())
        raise LookupError('Could not find a valid data_environment value. '
                          f'Valid environments are: {supp_envs}')

    # Create the data schema and get any unset parameters from
    # the data schema
    data_schema = DataSchema(environment=data_environment,
                             data_family=data_family,
                             data_source=kql_query.data_source)

    for param, value in req_params.items():
        if not value and param in data_schema:
            req_params[param] = data_schema[param]

    # If we have missing parameters try to retrieve them
    # as attributes of the object
    missing_params = [p_name for p_name, p_value in req_params.items() if not p_value]
    if missing_params:
        for other_object in [obj for obj in args if not isinstance(obj, QueryParamProvider)]:
            for m_param in missing_params:
                if m_param in other_object:
                    req_params[m_param] = getattr(other_object, m_param)
            missing_params = [p_name for p_name, p_value in req_params.items() if not p_value]

        if missing_params:
            # check for and remove optional parameters from the missing params list
            for m_param in missing_params:
                if m_param in kql_query.optional_params:
                    req_params[m_param] = ''
            missing_params = [p_name for p_name in missing_params
                              if p_name not in kql_query.optional_params]

        if missing_params:
            # If still have missing params then we error out
            query_help(kql_query.name)
            mssg = ('The following required parameters for this query were not set:',
                    ', '.join(missing_params))
            raise ValueError(mssg)

    return req_params


def _get_data_family_and_env(kql_query, providers, custom_params):
    """Get the data_family and environment."""
    data_family = None
    data_environment = None

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
        if _DATA_FAMILY_NAME in provider.query_params:
            fam_value = DataFamily.parse(provider.query_params[_DATA_FAMILY_NAME])
            if fam_value:
                candidate_families.add(fam_value)
        if _DATA_ENVIRONMENT_NAME in provider.query_params:
            env_value = DataEnvironment.parse(provider.query_params[_DATA_ENVIRONMENT_NAME])
            if env_value:
                candidate_environments.add(env_value)

    if custom_params:
        # If we haven't yet worked out the data family and environment
        # try to get this from one of custom_params
        if _DATA_FAMILY_NAME in custom_params:
            fam_value = DataFamily.parse(custom_params[_DATA_FAMILY_NAME])
            if fam_value:
                candidate_families.add(fam_value)
        if _DATA_ENVIRONMENT_NAME in custom_params:
            env_value = DataEnvironment.parse(custom_params[_DATA_ENVIRONMENT_NAME])
            if env_value:
                candidate_environments.add(env_value)

    # get the intersection of families and environments that we found and those
    # supported by the query. If it is 1 item we are good to go.
    usable_families = candidate_families & set(kql_query.data_families)
    if len(usable_families) == 1:
        data_family = usable_families.pop()
    usable_environments = candidate_environments & set(kql_query.data_environments)
    if len(usable_environments) == 1:
        data_environment = usable_environments.pop()

    return data_family, data_environment


@export
def required_params(kql_query: any) -> list:
    """
    Return the set of required parameters for the query.

        :param query_string:
    """
    if isinstance(kql_query, KqlQuery):
        query_string = kql_query.query
    else:
        query_string = kql_query
    param_pattern = r'{([\w\d_-]+)}'
    return list(set(re.findall(param_pattern, query_string)))


def _add_queries_to_module(module_name):
    """Add queries to the module as callable methods."""
    if module_name not in sys.modules:
        raise LookupError(f'Module {module_name} was not found sys.modules')
    for query_name in query_definitions:
        module = sys.modules[module_name]
        query_func = partial(replace_prov_query_params, query_name=query_name)
        query_func.__doc__ = replace_prov_query_params.__doc__
        setattr(module, query_name, query_func)


# Add all queries defined in builtin queries module as functions
_add_queries_to_module(__name__)
