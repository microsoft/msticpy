# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Dynamic TI provider to create provider from yaml/json definition."""
import operator
from typing import Any, Dict, List, NamedTuple, Tuple, Union, cast

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

    cls_name = valid_pyname(cast(str, provider_def["name"]))
    cls_attributes = {
        "_BASE_URL": cast(str, provider_def.get("base_url")),
        "description": cast(str, provider_def.get("description")),
        "_REQUIRED_PARAMS": cast(str, provider_def.get("required_parameters")),
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
    queries = cast(Dict[str, Dict], provider_def["queries"])
    # create the main queries
    ioc_queries = {
        query_name: _create_param(query_def, provider_def)
        for query_name, query_def in queries.items()
    }
    # add any aliases for ioc types.
    for query_name, query_def in queries.items():
        aliases = query_def.get("aliases", [])
        for alias in aliases:
            ioc_queries[alias] = ioc_queries[query_name]
    return ioc_queries


def _create_param(
    query: Dict[str, Any], provider_def: Dict[str, Any]
) -> IoCLookupParams:
    """Return IoCLookupParams instance for a query."""
    macros = provider_def.get("macros", {})
    query_replaced = {
        key: _replace_macros(value, macros)
        for key, value in query.items()
        if key in attr.fields_dict(IoCLookupParams)
    }
    ioc_params = IoCLookupParams(**query_replaced)
    _set_request_default(ioc_params, "verb", provider_def, "GET")
    _set_request_default(ioc_params, "headers", provider_def, {})
    _set_request_default(ioc_params, "params", provider_def, {})
    _set_request_default(ioc_params, "data", provider_def, {})
    _set_request_default(ioc_params, "auth_type", provider_def, "HTTPBasic")
    _set_request_default(ioc_params, "auth_str", provider_def, [])
    _set_request_default(ioc_params, "sub_type", provider_def, "")
    return ioc_params


def _set_request_default(ioc_param, attrib, provider_def, default):
    """Return IoC param attribute value or a default."""
    curr_value = getattr(ioc_param, attrib, None)
    if not curr_value:
        req_def_value = provider_def.get("request_defaults", {}).get(attrib, default)
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
    sev_rules = result_processing["severity"]
    severity = _match_severity(
        response=response,
        sev_rules=sev_rules,
        def_result_key=sev_rules.get("key"),
    )
    summary_rules = result_processing["summary"]
    summary = _get_summary_items(
        response=response,
        summary_rules=result_processing["summary"],
        def_result_key=summary_rules.get("key"),
    )
    return True, severity, summary


def _get_path_from_dict(response, path):
    """Retrive dictionary value formatted as dotted path."""
    value = response
    for path_part in path.split("."):
        value = value.get(path_part, {})
    return value


def _match_severity(response, sev_rules, def_result_key):
    """Return the match for a severity rule."""
    sev = "information"
    for sev, condition in sev_rules["conditions"].items():
        print("_match_severity", sev, condition)
        if condition is None:
            # default match
            break
        if _match_severity_condition_str(response, condition, def_result_key):
            break
    return ResultSeverity.parse(sev)


def _match_severity_condition_str(response, condition, def_key):
    """Return match for individual severity condition."""
    key = condition.get("key", def_key) if isinstance(condition, dict) else def_key
    # DEBUG
    print("_match_severity_condition_str", condition, key)
    if isinstance(condition, dict):
        if "and" in condition:
            cond_list = condition["and"]
            return all(
                _match_severity_condition_str(response, sub_cond, def_key)
                for sub_cond in cond_list
            )
        if "or" in condition:
            return any(
                _match_severity_condition_str(response, sub_cond, def_key)
                for sub_cond in cond_list
            )
    key_value = _get_path_from_dict(response, key)
    # DEBUG
    print("_match_severity_condition_str", key_value)
    if not isinstance(condition, str):
        raise ValueError(f"Invalid condition expression '{condition}'")
    cond_terms = condition.split()
    if len(cond_terms) == 2:
        cond_left, cond_op, cond_right = "value", *cond_terms
    elif len(cond_terms) == 3:
        cond_left, cond_op, cond_right = cond_terms
    else:
        raise ValueError(f"Invalid condition expression '{condition}'")
    if cond_right.isnumeric():
        cond_right = float(cond_right)
    op_func = getattr(operator, cond_op)
    if cond_left == "value":
        return op_func(key_value, cond_right)
    if cond_left == "len":
        return op_func(len(key_value), cond_right)
    raise ValueError(f"Unknown left side operand in condition {cond_left}.")


def _get_summary_items(response, summary_rules, def_result_key):
    """Return summary items from response."""
    summary_dict = {}
    for name, rule in summary_rules.items():
        if name == "key":
            continue

        item_key = (
            rule.get("key", def_result_key)
            if isinstance(rule, dict)
            else def_result_key
        )
        key_value = _get_path_from_dict(response, item_key)
        if not key_value:
            continue
        action = rule.get("action", "value")
        if action in ("value", "data"):
            summary_dict[name] = key_value
        elif action in ("count", "len"):
            summary_dict[name] = len(key_value)
        elif isinstance(action, dict):
            op_name, operand = next(iter(action.items()))
            if op_name != "get_item":
                continue
            op_func = operator.itemgetter(operand)
            if isinstance(key_value, list):
                item_list = [op_func(item) for item in key_value]
            elif isinstance(key_value, dict):
                item_list = op_func(key_value)
            if rule.get("flatten", False):
                item_list = [elem for item in item_list for elem in item]
            summary_dict[name] = item_list
        elif action == "keys":
            summary_dict[name] = (
                list(key_value.keys()) if isinstance(key_value, dict) else key_value
            )
        elif action == "values":
            summary_dict[name] = (
                list(key_value.values()) if isinstance(key_value, dict) else key_value
            )
    return summary_dict


# Definition validation
class TIDefCheck(NamedTuple):
    """Validation class for checking definition files."""

    required: bool
    prim_type: Union[type, str]
    elem_type: Union[
        None, type, Tuple[type, type], Tuple[type, Tuple[type, type]]
    ] = None
    legal_keys: List[str] = None


_PROVIDER_VALIDATION = {
    "name": TIDefCheck(True, str),
    "description": TIDefCheck(True, str),
    "base_url": TIDefCheck(False, str),
    "required_parameters": TIDefCheck(True, list, str),
    "request_defaults": TIDefCheck(
        True,
        dict,
        (str, (str, dict)),
        [
            "headers",
            "params",
            "data",
            "auth_type",
            "auth_str",
        ],
    ),
    "macros": TIDefCheck(False, dict, (str, str)),
    "queries": TIDefCheck(True, "queries_dict"),
    "result_processing": {
        "severity": TIDefCheck(True, "severity"),
        "summary": TIDefCheck(True, "summary"),
    },
}


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
            if validation.prim_type == "queries":
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
        query for query, params in query_dict.items() if "path" not in params
    ]
    if missing_path:
        validation_errors.append(
            f"The following queries have a missing 'path' key: {', '.join(missing_path)}"
        )
    invalid_path = [
        query
        for query, params in query_dict.items()
        if isinstance(params["path"], str) and len(params["path"]) > 0
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
    known_keys = {"path", "aliases"}
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
