# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Dynamic TI provider to create provider from yaml/json definition."""
import operator
import re
from typing import Any, Dict, List, NamedTuple, Optional, Tuple, Union, cast

import attr

from ..._version import VERSION
from ...common.utility import valid_pyname
from .http_provider import HttpTIProvider, IoCLookupParams
from .lookup_result import LookupResult
from .result_severity import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


TIProviderDef = Dict[str, Union[str, Dict[str, Any]]]


def create_ti_provider(provider_def: TIProviderDef):
    """Return an TI Provider class from a definition."""
    _validate_definition(provider_def, _PROVIDER_VALIDATION)

    cls_name = valid_pyname(cast(str, provider_def[_Fields.NAME]))
    cls_attributes = {
        "_BASE_URL": cast(str, provider_def.get(_Fields.BASE_URL)),
        _Fields.DESCRIPTION: cast(str, provider_def.get(_Fields.DESCRIPTION)),
        "_REQUIRED_PARAMS": cast(str, provider_def.get(_Fields.REQUIRED_PARAMETERS)),
        "_IOC_QUERIES": _parse_queries(provider_def),
        "parse_results": parse_results,
    }

    cls = type(cls_name, (HttpTIProvider,), cls_attributes)
    cls.__doc__ = (
        f"TI Provider for {provider_def.get('description', provider_def['name'])}."
    )
    return cls


def _parse_queries(provider_def: TIProviderDef) -> Dict[str, IoCLookupParams]:
    """Parse dictionary of TI query definitions."""
    queries = cast(Dict[str, Dict], provider_def[_Fields.QUERIES])
    # create the main queries
    ioc_queries = {
        query_name: _create_param(query_def, provider_def)
        for query_name, query_def in queries.items()
    }
    # add any aliases for ioc types.
    for query_name, query_def in queries.items():
        aliases = query_def.get(_Fields.ALIASES, [])
        for alias in aliases:
            ioc_queries[alias] = ioc_queries[query_name]
    return ioc_queries


def _create_param(
    query: Dict[str, Any], provider_def: Dict[str, Any]
) -> IoCLookupParams:
    """Return IoCLookupParams instance for a query."""
    macros = provider_def.get(_Fields.MACROS, {})
    query_replaced = {
        key: _replace_macros(value, macros)
        for key, value in query.items()
        if key in attr.fields_dict(IoCLookupParams)
    }
    ioc_params = IoCLookupParams(**query_replaced)
    _set_request_default(ioc_params, _Fields.VERB, provider_def, "GET")
    _set_request_default(ioc_params, _Fields.HEADERS, provider_def, {})
    _set_request_default(ioc_params, _Fields.PARAMS, provider_def, {})
    _set_request_default(ioc_params, _Fields.DATA, provider_def, {})
    _set_request_default(ioc_params, _Fields.AUTH_TYPE, provider_def, "HTTPBasic")
    _set_request_default(ioc_params, _Fields.AUTH_STR, provider_def, [])
    _set_request_default(ioc_params, "sub_type", provider_def, "")
    return ioc_params


def _set_request_default(ioc_param, attrib, provider_def, default):
    """Return IoC param attribute value or a default."""
    curr_value = getattr(ioc_param, attrib, None)
    if not curr_value:
        req_def_value = provider_def.get(_Fields.REQUEST_DEFAULTS, {}).get(
            attrib, default
        )
        setattr(ioc_param, attrib, req_def_value)


def _replace_macros(
    query_param: Union[List[str], str], macros: Dict[str, str]
) -> Union[List[str], str]:
    """Replace any specified replaceable tokens in the query definition."""
    if not isinstance(query_param, (str, list)):
        return query_param
    if isinstance(query_param, str):
        for token, replacement in macros.items():
            query_param = query_param.replace(f"{{{token}}}", replacement)
        return query_param

    replaced_list = []
    for token, replacement in macros.items():
        for item in query_param:
            if not isinstance(item, str):
                replaced_list.append(item)
            else:
                replaced_list.append(item.replace(f"{{{token}}}", replacement))
    return replaced_list


# Parse Results Definition and supporting functions


def parse_results(
    self, response: LookupResult, result_processing
) -> Tuple[bool, ResultSeverity, Any]:
    """
    Return the details of the response.

    Parameters
    ----------
    response : LookupResult
        The returned data response
    result_processing : Dict
        Rule dictionary for extracting severity
        and summary from response.

    Returns
    -------
    Tuple[bool, ResultSeverity, Any]
        bool = positive or negative hit
        ResultSeverity = enumeration of severity
        Object with match details

    """
    del self
    sev_rules = result_processing[_Fields.SEVERITY]
    severity = _match_severity(
        response=response,
        sev_rules=sev_rules,
        def_result_key=sev_rules.get(_Fields.KEY),
    )
    summary_rules = result_processing[_Fields.SUMMARY]
    summary = _get_summary_items(
        response=response,
        summary_rules=result_processing[_Fields.SUMMARY],
        def_result_key=summary_rules.get(_Fields.KEY),
    )
    return True, severity, summary


def _get_path_from_dict(response, path):
    """Retrieve dictionary value formatted as dotted path."""
    value = response
    for path_part in path.split("."):
        value = value.get(path_part, {})
    return value


def _match_severity(response, sev_rules, def_result_key):
    """Return the match for a severity rule."""
    sev = _Fields.INFORMATION
    for sev, condition in sev_rules[_Fields.CONDITIONS].items():
        print("_match_severity", sev, condition)
        if condition is None:
            # default match
            break
        if _match_severity_condition_str(response, condition, def_result_key):
            break
    return ResultSeverity.parse(sev)


_OP_FUNCS = {
    "in": lambda a, b: a in b,
    "contains": lambda a, b: b in a,
    "ne": lambda a, b: a != b,
    "eq": operator.eq,
    "gt": operator.gt,
    "ge": operator.ge,
    "lt": lambda a, b: a < b,
    "le": lambda a, b: a <= b,
    "matches": lambda a, b: bool(re.match(b, a, re.IGNORECASE)),
}


# pylint: disable=too-many-branches
def _match_severity_condition_str(response, condition, def_key):
    """Return match for individual severity condition."""
    cond_key = None
    # DEBUG
    print("_match_severity_condition_str", condition, def_key)
    if isinstance(condition, dict):
        if _Fields.AND in condition:
            cond_list = condition[_Fields.AND]
            return all(
                _match_severity_condition_str(response, sub_cond, def_key)
                for sub_cond in cond_list
            )
        if _Fields.OR in condition:
            return any(
                _match_severity_condition_str(response, sub_cond, def_key)
                for sub_cond in cond_list
            )
        if _Fields.NOT in condition:
            return not _match_severity_condition_str(response, condition, def_key)
        # otherwise, assume that the condition dict is {"key": "condition"}
        cond_key, condition = next(iter(condition.items()))
    # If this is a nested condition (like {"key": "condition"} or {"key": {"and": {...}}})
    # recurse to evaluate nested conditions
    if isinstance(condition, dict):
        return _match_severity_condition_str(response, condition, cond_key)
    key = cond_key or def_key
    key_value = _get_path_from_dict(response, key)
    # DEBUG
    print("_match_severity_condition_str", condition, key, key_value)
    if not isinstance(condition, str):
        raise ValueError(f"Invalid condition expression '{condition}'")
    cond_terms = condition.split()
    if len(cond_terms) == 2:
        cond_left, cond_op, cond_right = [_Fields.VALUE, *cond_terms]  # type: ignore
    elif len(cond_terms) == 3:
        cond_left, cond_op, cond_right = cond_terms
    else:
        raise ValueError(f"Invalid condition expression '{condition}'")
    if cond_right.isnumeric():
        cond_right = float(cond_right)
    op_func = _OP_FUNCS.get(cond_op.casefold())
    if op_func is None:
        raise TypeError(
            f"Operator '{cond_op}' not recognized. Valid operators are:",
            ", ".join(f"'{legal_op}'" for legal_op in _OP_FUNCS),
        )
    if cond_left == _Fields.VALUE:
        return op_func(key_value, cond_right)
    if cond_left == _Fields.LEN:
        return op_func(len(key_value), cond_right)
    raise ValueError(f"Unknown left side operand in condition {cond_left}.")


def _get_summary_items(response, summary_rules, def_result_key):
    """Return summary items from response."""
    summary_dict = {}
    for name, rule in summary_rules.get(_Fields.FIELDS, {}).items():
        print("_get_summary_items", def_result_key, rule)
        item_key = (
            rule.get(_Fields.KEY, def_result_key)
            if isinstance(rule, dict)
            else def_result_key
        )
        key_value = _get_path_from_dict(response, item_key)
        if not key_value:
            continue
        action = rule.get(_Fields.ACTION, _Fields.DATA)
        if action == _Fields.DATA:
            summary_dict[name] = key_value
        elif action in (_Fields.COUNT, _Fields.LEN):
            summary_dict[name] = len(key_value)
        elif isinstance(action, dict):
            op_name, operand = next(iter(action.items()))
            if op_name != _Fields.GET_ITEM:
                print(f"Unsupported operand {op_name}")
                continue
            if op_name == _Fields.GET_ITEM:
                op_func = operator.itemgetter(operand)
                if isinstance(key_value, list):
                    item_list = [op_func(item) for item in key_value]
                elif isinstance(key_value, dict):
                    item_list = op_func(key_value)
            if rule.get(_Fields.FLATTEN, False):
                item_list = [elem for item in item_list for elem in item]
            summary_dict[name] = item_list
        elif action in (_Fields.KEYS, _Fields.LIST_KEYS):
            summary_dict[name] = (
                list(key_value.keys()) if isinstance(key_value, dict) else key_value
            )
        elif action in (_Fields.VALUES, _Fields.LIST_VALUES):
            summary_dict[name] = (
                list(key_value.values()) if isinstance(key_value, dict) else key_value
            )
    return summary_dict


# End ParseResult definition


# Definition validation
class TIDefCheck(NamedTuple):
    """Validation class for checking definition files."""

    required: bool
    prim_type: Union[type, str]
    elem_type: Union[
        None, type, Tuple[type, type], Tuple[type, Tuple[type, type]]
    ] = None
    legal_keys: Optional[List[str]] = None


def _validate_definition(provider_def, validation_rules):
    """Validate TI provider definition."""
    validation_errors = []
    for v_key, validation in validation_rules.items():
        print(v_key, validation)
        if isinstance(validation, dict):
            print("dict", provider_def.get(v_key), validation)
            # need to recurse here. - split main body of checks from top function
            validation_errors.extend(
                _validate_definition(provider_def.get(v_key), validation)
            )
            continue
        if validation.required and v_key not in provider_def:
            validation_errors.append(f"Missing required key {v_key}")

        if isinstance(validation.prim_type, str):
            if validation.prim_type == _Fields.QUERIES:
                validation_errors.extend(_validate_queries(provider_def.get(v_key)))
                continue
        else:
            validation_errors.extend(
                _validate_type(
                    provider_def.get(v_key),
                    validation.prim_type,
                    validation.elem_type,
                    validation.legal_keys,
                )
            )
    return validation_errors


def _validate_type(value: Any, val_type, elem_type, legal_keys):
    validation_errors = []
    print("_validate_type", value, val_type)
    if not isinstance(value, val_type):
        validation_errors.append(f"not expected type {val_type.__name__}")
        return validation_errors
    if elem_type:
        if isinstance(val_type, list):
            list_types = [
                f"elem {idx} not {elem_type.__name__}"
                for idx, elem in value
                if not isinstance(elem, elem_type)
            ]
            validation_errors.extend(list_types)
        elif isinstance(val_type, dict):
            elem_key_type, elem_val_type = elem_type
            dict_type_errs = []
            for idx, (elem_key, elem_value) in enumerate(value.items()):
                if not isinstance(elem_key, elem_key_type):
                    dict_type_errs.append(
                        f"elem key {idx} not {elem_key_type.__name__}"
                    )
                if not isinstance(elem_value, elem_val_type):
                    dict_type_errs.append(f"elem value {idx} not {elem_value.__name__}")

            validation_errors.extend(dict_type_errs)

            if legal_keys:
                invalid_keys = list(value.keys() - legal_keys)
                validation_errors.extend(
                    f"Unknown keys found: {', '.join(invalid_keys)}"
                )
    return validation_errors


def _validate_queries(query_dict: Dict[str, Dict[str, Any]]) -> List[str]:
    validation_errors = []
    missing_path = [
        query for query, params in query_dict.items() if _Fields.PATH not in params
    ]
    if missing_path:
        validation_errors.append(
            f"The following queries have a missing 'path' key: {', '.join(missing_path)}"
        )
    invalid_path = [
        query
        for query, params in query_dict.items()
        if isinstance(params[_Fields.PATH], str) and len(params[_Fields.PATH]) > 0
    ]
    if invalid_path:
        validation_errors.append(
            f"The following queries have an invalid 'path' value: {', '.join(invalid_path)}"
        )
    invalid_ioc = [
        query
        for query in query_dict
        if not HttpTIProvider.is_known_type(query.split(query, maxsplit=1)[0])
    ]
    if invalid_ioc:
        validation_errors.append(
            f"The following queries have an invalid IOC type: {', '.join(invalid_ioc)}"
        )
    known_keys = {_Fields.PATH, _Fields.ALIASES}
    unknown_keys = {
        query: list(params.keys() - known_keys)
        for query, params in query_dict.items()
        if params.keys() - known_keys
    }
    if unknown_keys:
        mssg_keys = [f"{key}: [{','.join(val)}]" for key, val in unknown_keys.items()]
        validation_errors.append(
            f"The following queries have unknown entries: {', '.join(mssg_keys)}"
        )
    return validation_errors


# pylint: disable=too-few-public-methods
class _Fields:
    """Static class for field and definition item names."""

    ACTION = "action"
    ALIASES = "aliases"
    AND = "and"
    AUTH_STR = "auth_str"
    AUTH_TYPE = "auth_type"
    BASE_URL = "base_url"
    CONDITIONS = "conditions"
    COUNT = "count"
    DATA = "data"
    DESCRIPTION = "description"
    FIELDS = "fields"
    FLATTEN = "flatten"
    GET_ITEM = "get_item"
    HEADERS = "headers"
    HIGH = "high"
    INFORMATION = "information"
    KEY = "key"
    KEYS = "keys"
    LEN = "len"
    LIST_KEYS = "list_keys"
    LIST_VALUES = "list_values"
    MACROS = "macros"
    NAME = "name"
    NAMES = "names"
    NOT = "not"
    OR = "or"
    PARAMS = "params"
    PATH = "path"
    PULSE_COUNT = "pulse_count"
    QUERIES = "queries"
    REFERENCES = "references"
    REQUEST_DEFAULTS = "request_defaults"
    REQUIRED_PARAMETERS = "required_parameters"
    RESULT_PROCESSING = "result_processing"
    SECTIONS_AVAILABLE = "sections_available"
    SECTIONS_AVAILABLE_K = "sections_available_k"
    SEVERITY = "severity"
    SUMMARY = "summary"
    TAGS = "tags"
    VALUE = "value"
    VALUES = "values"
    VERB = "verb"
    WARNING = "warning"


# Dictionary used by validation functions - outlines
# Basic structure of definition file.
_PROVIDER_VALIDATION = {
    _Fields.NAME: TIDefCheck(True, str),
    _Fields.DESCRIPTION: TIDefCheck(True, str),
    _Fields.BASE_URL: TIDefCheck(False, str),
    _Fields.REQUIRED_PARAMETERS: TIDefCheck(True, list, str),
    _Fields.REQUEST_DEFAULTS: TIDefCheck(
        True,
        dict,
        (str, (str, dict)),
        [
            _Fields.HEADERS,
            _Fields.PARAMS,
            _Fields.DATA,
            _Fields.AUTH_TYPE,
            _Fields.AUTH_STR,
        ],
    ),
    _Fields.MACROS: TIDefCheck(False, dict, (str, str)),
    _Fields.QUERIES: TIDefCheck(True, "queries_dict"),
    _Fields.RESULT_PROCESSING: {
        _Fields.SEVERITY: TIDefCheck(True, _Fields.SEVERITY),
        _Fields.SUMMARY: TIDefCheck(True, _Fields.SUMMARY),
    },
}
