# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for SQL to KQL Conversion.

This is an experiment conversion utility built to support a limited subset
of ANSI SQL.

It relies on mo_sql_parsing https://github.com/klahnakoski/mo-sql-parsing
(a maintained fork from the deprecated https://github.com/mozilla/moz-sql-parser)
to parse the SQL syntax tree. Some hacky additions have been done to
allow table renaming and support for non ANSI SQL operators such as
RLIKE.

For a more complete translation help with SQL to KQL see
https://docs.microsoft.com/en-us/azure/data-explorer/kusto/query/sqlcheatsheet

Known limitations
-----------------

- Does not support aggregate functions in SELECT with no GROUP BY clause
- Does not support IN, EXISTS, HAVING operators
- Only partial support for AS naming (should work in SELECT expressions)

"""

import random
import re
from collections import namedtuple
from typing import Any, Dict, List, Optional, Tuple, Union

from ..common.exceptions import MsticpyImportExtraError

try:
    from mo_sql_parsing import parse
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without mo-sql-parsing installed",
        title="Error importing mo_sql_parsing for sql_to_kql",
        extra="sql",
    ) from imp_err


from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_DEBUG = False

FuncFormat = namedtuple("FuncFormat", "default, cust_arg_fmt, cust_func_format")

SQL_KQL_FUNC_MAP = {
    "add": FuncFormat(None, None, "{p0} + {p1}"),
    "avg": FuncFormat("avg", None, None),
    "base64": FuncFormat("base64_encode_tostring", None, None),
    "concat": FuncFormat("strcat", None, None),
    "count": FuncFormat("count", None, None),
    "if": FuncFormat("iif", None, None),
    "iif": FuncFormat("iif", None, None),
    "ifnull": FuncFormat(None, None, "iif(isnull({p0}), {p1}, {p0}))"),
    "in": FuncFormat("in", None, None),
    "instr": FuncFormat("indexof", None, None),
    "int": FuncFormat("toint", None, None),
    "isnotnull": FuncFormat("isnotnull", None, None),
    "isnull": FuncFormat("isnull", None, None),
    "left": FuncFormat("NA", None, None),
    "length": FuncFormat("strlen", None, None),
    "like": FuncFormat("NA", None, None),
    "locate": FuncFormat("indexof", None, None),
    "lower": FuncFormat("tolower", None, None),
    "lcase": FuncFormat("tolower", None, None),
    "ltrim": FuncFormat("trim_start", None, None),
    "max": FuncFormat("max", None, None),
    "mean": FuncFormat("mean", None, None),
    "min": FuncFormat("min", None, None),
    "position": FuncFormat("indexof", None, None),
    "regexp_extract": FuncFormat("extract", "{p1}, {p0}", None),  # swap params 0, 1
    "replace": FuncFormat("replace", None, None),
    "reverse": FuncFormat("reverse", None, None),
    "rtrim": FuncFormat("trim_end", None, None),
    "split": FuncFormat("split", None, None),
    "string": FuncFormat("tostring", None, None),
    "str": FuncFormat("tostring", None, None),
    "substr": FuncFormat("substring", None, None),
    "substring": FuncFormat("substring", None, None),
    "sum": FuncFormat("sum", None, None),
    "todatetime": FuncFormat("to_date", None, None),
    "translate": FuncFormat("translate", None, None),
    "trim": FuncFormat("trim", None, None),
    "unbase64": FuncFormat("base64_decode_tostring", None, None),
    "upper": FuncFormat("toupper", None, None),
}


AND = "and"
AS = "as"
ASC = "asc"
BETWEEN = "between"
CASE = "case"
COLLATE_NOCASE = "collate nocase"
CROSS_JOIN = "cross join"
DESC = "desc"
DISTINCT = "distinct"
ELSE = "else"
END = "end"
FROM = "from"
FULL_JOIN = "full join"
FULL_OUTER_JOIN = "full outer join"
GROUP_BY = "groupby"
HAVING = "having"
IN = "in"
INNER_JOIN = "inner join"
IS = "is"
IS_NOT = "is not"
JOIN = "join"
LEFT_JOIN = "left join"
LEFT_OUTER_JOIN = "left outer join"
LIKE = "like"
LIMIT = "limit"
NOT = "not"
NOT_BETWEEN = "not_between"
NOT_IN = "nin"
NOT_LIKE = "not_like"
NOT_RLIKE = "not_rlike"
OFFSET = "offset"
ON = "on"
OR = "or"
ORDER_BY = "orderby"
RIGHT_JOIN = "right join"
RIGHT_OUTER_JOIN = "right outer join"
RLIKE = "rlike"
SELECT = "select"
SELECT_DISTINCT = "select_distinct"
THEN = "then"
UNION = "union"
UNION_ALL = "union all"
USING = "using"
WHEN = "when"
WHERE = "where"
WITH = "with"

JOIN_KEYWORDS = {
    FULL_JOIN: "outer",
    FULL_OUTER_JOIN: "outer",
    INNER_JOIN: "inner",
    JOIN: "inner",
    LEFT_JOIN: "left",
    LEFT_OUTER_JOIN: "left",
    RIGHT_JOIN: "right",
    RIGHT_OUTER_JOIN: "right",
    CROSS_JOIN: "CROSS_JOIN_TODO",
}
JOIN_KEYWORDS = {kw.replace("_", " "): kql for kw, kql in JOIN_KEYWORDS.items()}

BINARY_OPS = {
    "concat": "concat",
    "mul": "*",
    "div": "/",
    "mod": "%",
    "add": "+",
    "sub": "-",
    "binary_and": "binary_and",
    "binary_or": "binary_or",
    "binary_xor": "binary_xor",
    "lt": "<",
    "lte": "<=",
    "gt": ">",
    "gte": ">=",
    "not_between": "not_between",
    "or": "or",
    "and": "and",
    "eq": "==",
    "neq": "!=",
    "nin": "!in",
    "rlike": "matches regex",
    "not_rlike": "not matches regex",
    "is": "==",
    "is_not": "!=",
}

# noqa: MC0001


def sql_to_kql(sql: str, target_tables: Dict[str, str] = None) -> str:
    """Parse SQL and return KQL equivalent."""
    # ensure literals are surrounded by single quotes
    sql = _single_quote_strings(sql)

    # replace table names
    if target_tables:
        for table in target_tables:
            sql = sql.replace(table, target_tables[table])
    # replace keywords
    # sql = _remap_kewords(sql)
    parsed_sql = parse(sql)
    query_lines = _parse_query(parsed_sql)
    return "\n".join(line for line in query_lines if line.strip())


def _parse_query(parsed_sql: Dict[str, Any]) -> List[str]:  # noqa: MC0001
    """Translate query or subquery."""
    query_lines: List[str] = []
    if isinstance(parsed_sql, str):
        return [parsed_sql]
    if FROM in parsed_sql:
        _process_from(parsed_sql[FROM], query_lines)
    if WHERE in parsed_sql:
        query_lines.append(f"| where {_parse_expression(parsed_sql[WHERE])}")

    if GROUP_BY in parsed_sql:
        _process_group_by(parsed_sql, query_lines)
        # Get rid of the SELECT statement since we've processed it in
        # the groupby
        parsed_sql.pop(SELECT, parsed_sql.pop(SELECT_DISTINCT, None))

    distinct_select: List[Dict[str, Any]] = []
    if SELECT_DISTINCT in parsed_sql:
        distinct_select.extend(parsed_sql[SELECT_DISTINCT])
        _process_select(
            parsed_sql[SELECT_DISTINCT], parsed_sql[SELECT_DISTINCT], query_lines
        )
    if SELECT in parsed_sql:
        _process_select(parsed_sql[SELECT], parsed_sql[SELECT], query_lines)
    if ORDER_BY in parsed_sql:
        query_lines.append(f"| order by {_create_order_by(parsed_sql[ORDER_BY])}")
    if distinct_select:
        query_lines.append(
            f"| distinct {', '.join(_create_distinct_list(distinct_select))}"
        )
    if LIMIT in parsed_sql:
        query_lines.append(f"| limit {parsed_sql[LIMIT]}")
    if UNION in parsed_sql:
        union_subquery = {UNION_ALL: parsed_sql[UNION]}
        query_lines.extend(_parse_query(union_subquery))
        query_lines.append("| distinct *")
    if UNION_ALL in parsed_sql:
        union_l_expr = "\n".join(_parse_query(parsed_sql[UNION_ALL][0]))
        query_lines.append(union_l_expr)
        union_r_expr = "\n  ".join(_parse_query(parsed_sql[UNION_ALL][1]))
        query_lines.append(f"| union ({union_r_expr}\n)")

    return query_lines


def _process_from(
    from_expr: Union[List[Dict[str, Any]], Dict[str, Any], str], query_lines: List[str]
):
    """Process FROM clause."""
    if isinstance(from_expr, dict) and UNION in from_expr:
        query_lines.extend(_parse_query(from_expr))
    elif isinstance(from_expr, dict):
        query_lines.extend(_parse_query(from_expr))
    elif isinstance(from_expr, str):
        query_lines.append((from_expr))
        return
    elif isinstance(from_expr, list):
        for from_item in from_expr:
            if isinstance(from_item, str):
                query_lines.append((from_item))
            elif isinstance(from_item, dict) and "value" in from_item:
                query_lines.extend(_parse_query(from_item.get("value")))  # type: ignore

    join_expr = from_expr if isinstance(from_expr, list) else [from_expr]
    join_list = _get_join_list(join_expr)
    for join_item in join_list:
        join_line = _parse_join(join_item)
        if join_line:
            query_lines.append(join_line)


def _process_select(
    parsed_sql: Dict[str, Any],
    expr_list: Union[List[Dict[Any, Any]], Dict[Any, Any]],
    query_lines: List[str],
):
    """Process SELECT clause."""
    # Expressions
    if parsed_sql == "*":
        return
    _db_print(expr_list, type(expr_list))
    select_list = expr_list if isinstance(expr_list, list) else [expr_list]
    select_list = _get_expr_list(select_list)
    project_items = []
    extend_items = []
    for item in select_list:
        value = _parse_expression(item["value"])
        name = item.get("name")
        if value != item["value"]:
            # if this isn't a simple rename - add to extend
            name = name or _gen_expr_name(item["value"])
            extend_items.append(f"{name} = {value}")
            project_items.append(name)
        else:
            if name:
                project_items.append(f"{name} = {value}")
            else:
                project_items.append(value)
    if extend_items:
        query_lines.append(f"| extend {', '.join(extend_items)}")
    if project_items:
        query_lines.append(f"| project {', '.join(project_items)}")


def _gen_expr_name(value):
    """Generate random expression name."""
    pref = "expr"
    if isinstance(value, str):
        return value
    if isinstance(value, dict):
        first_val = next(iter(value.values()))
        if isinstance(first_val, str):
            return first_val
        # prob a function so base the name on that
        pref = next(iter(value.keys()))
    # otherwise just generate a rand value as suffix
    suffix = str(random.randint(1000, 9999))  # nosec
    return f"{pref}_{suffix}"


def _get_expr_list(expr_list):
    if (
        isinstance(expr_list, list)
        and len(expr_list) == 1
        and isinstance(expr_list[0], dict)
        and isinstance(expr_list[0].get("value"), list)
    ):
        return expr_list[0]["value"]
    return expr_list


def _get_expr_value(expr_val):
    if isinstance(expr_val, dict) and "value" in expr_val:
        return expr_val["value"]
    if isinstance(expr_val, list):
        return _get_expr_list(expr_val)
    return expr_val


def _process_group_by(parsed_sql: Dict[str, Any], query_lines: List[str]):
    """Process GROUP BY clause."""
    group_by_expr = parsed_sql[GROUP_BY]
    group_by_expr = (
        group_by_expr if isinstance(group_by_expr, list) else [group_by_expr]
    )
    by_clause = ", ".join(val["value"] for val in group_by_expr if val.get("value"))

    expr_list = parsed_sql.get(SELECT, parsed_sql.get(SELECT_DISTINCT, []))
    group_by_expr_list = []
    expr_list = _get_expr_value(expr_list)
    for expr in expr_list:
        name_expr = ""
        if "name" in expr:
            name_expr = f"{expr.get('name')} = "
        if isinstance(expr.get("value"), str):
            group_by_expr_list.append(f"{name_expr}any({expr['value']})")
        else:
            group_by_expr_list.append(
                f"{name_expr}{_parse_expression(expr.get('value'))}"
            )
    query_lines.append(f"| summarize {', '.join(group_by_expr_list)} by {by_clause}")


# pylint: disable=too-many-return-statements, too-many-branches
def _parse_expression(expression):  # noqa: MC0001
    """Return parsed expression."""
    if _is_literal(expression)[0]:
        return _quote_literal(expression)
    if not isinstance(expression, dict):
        return expression
    if AND in expression:
        return "\n  and ".join(
            [f"({_parse_expression(expr)})" for expr in expression[AND]]
        )
    if OR in expression:
        return "\n  or ".join(
            [f"({_parse_expression(expr)})" for expr in expression[OR]]
        )
    if NOT in expression:
        return f" not ({_parse_expression(expression[NOT])})"
    if BETWEEN in expression:
        args = expression[BETWEEN]
        betw_expr = f"{_parse_expression(args[1])} .. {_parse_expression(args[2])}"
        return f"{args[0]} between ({betw_expr})"
    if NOT_BETWEEN in expression:
        args = expression[NOT_BETWEEN]
        betw_expr = f"{_parse_expression(args[1])} .. {_parse_expression(args[2])}"
        return f"{args[0]} !between ({betw_expr})"
    if IN in expression or NOT_IN in expression:
        sql_op = IN if IN in expression else NOT_IN
        kql_op = IN if IN in expression else "!in"
        args = expression[sql_op]

        right = _quote_literal(args[1])
        if isinstance(right, list):
            _db_print(args[1])
            arg_list = ", ".join([str(_parse_expression(l_item)) for l_item in right])
            return f"{args[0]} {kql_op} ({arg_list})"
        sub_query = "\n".join(_parse_query(right))
        return f"{args[0]} {kql_op} ({sub_query})"

    # Handle other operators
    oper = next(iter(expression.keys())) if expression else None
    if oper in BINARY_OPS:
        right = _parse_expression(expression[oper][1])
        left = _parse_expression(expression[oper][0])
        return f"{left} {BINARY_OPS[oper]} {right}"
    if LIKE in expression:
        return _process_like(expression)

    # For everything else, assume it's a function
    if expression:
        func, operand = next(iter(expression.items()))
        #         set_trace()
        if isinstance(operand, list):
            return _map_func(func, *operand)
        return _map_func(func, operand)
    return "EXPRESSION {expression} not resolved."


# pylint: enable=too-many-return-statements, too-many-branches


def _map_func(func: str, *args) -> str:
    """Return KQL function for SQL function."""
    func = func.lower().strip()
    args_dict = {f"p{idx}": arg for idx, arg in enumerate(args)}
    def_arg_fmt = ", ".join(f"{{{arg}}}" for arg in args_dict)
    if func not in SQL_KQL_FUNC_MAP:
        func_fmt = f"{func}({def_arg_fmt}) // WARNING unmapped function\n"
        return func_fmt.format(**args_dict)
    func_map = SQL_KQL_FUNC_MAP[func]

    if (
        func == "count"
        and isinstance(args[0], dict)
        and next(iter(args[0])) == "distinct"
    ):
        func_arg = _get_expr_value(args[0]["distinct"])
        return f"dcount({func_arg})"

    if not func_map.cust_arg_fmt and not func_map.cust_func_format:
        func_fmt = f"{func_map.default}({def_arg_fmt})"
        return func_fmt.format(**args_dict)
    if func_map.cust_arg_fmt:
        func_fmt = f"{func_map.default}({func_map.cust_arg_fmt})"
        return func_fmt.format(**args_dict)
    if func_map.cust_func_format:
        func_fmt = f"{func_map.cust_func_format}"
        return func_fmt.format(**args_dict)
    raise ValueError(f"Could not map function or args {func}{args_dict}")


def _quote_literal(expr: Union[str, List[str], Any]) -> Any:
    """Quote string if it is a literal."""
    literal, expr = _is_literal(expr)
    if not literal:
        return expr
    if isinstance(expr, str):
        return _quote(expr)
    if isinstance(expr, list):
        return [_quote(memb) for memb in expr]
    return expr


def _is_literal(expr: Union[Dict[str, Any], Any]) -> Tuple[bool, Any]:
    """Check if literal string."""
    if isinstance(expr, dict) and "literal" in expr:
        return True, expr["literal"]
    return False, expr


def _quote(expr: str) -> str:
    """Quote a string, if not already quoted."""
    if expr.startswith("'") and expr.endswith("'"):
        return expr
    return f"'{expr}'"


def _single_quote_strings(sql: str) -> str:
    """Replace unquoted double-quotes with single-quotes."""
    return re.sub(r"(?<![\\])\"", "'", sql)


def _format_order_item(item: Dict[str, Any]) -> str:
    """Return ORDER BY item with sort direction."""
    if "sort" in item:
        return f"{item['value']} {item['sort'].lower()}"
    return f"{item['value']}"


def _get_join_list(parsed_sql: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Return list of JOIN sub-expressions."""
    if not isinstance(parsed_sql, list):
        return []
    join_list = []
    for from_source in parsed_sql:
        if not isinstance(from_source, dict):
            continue
        join = JOIN_KEYWORDS & from_source.keys()
        if join:
            join_list.append(from_source)
    return join_list


def _rewrite_table_refs(join_expr: Union[Any, str, List], table_expr: str) -> str:
    """Rewrite dotted prefixes."""
    p_expr = _parse_expression(join_expr)
    prefixes = set(re.findall(r"(\w+)\.", p_expr))
    if not prefixes:
        return p_expr
    if f"{table_expr}" in prefixes:
        p_expr = p_expr.replace(f"{table_expr}.", "$right.")
        prefixes.remove(f"{table_expr}")
    for prefix in prefixes:
        p_expr = p_expr.replace(
            f"{prefix}.",
            "$right." if prefix.casefold() == table_expr.casefold() else "$left.",
        )
    return p_expr


def _parse_join(join_expr) -> Optional[str]:
    """Return translated JOIN expression."""
    join_type_set = JOIN_KEYWORDS & join_expr.keys()
    if not join_type_set:
        return None
    join_type = join_type_set.pop()
    table_expr = join_expr[join_type]
    kql_join_type = JOIN_KEYWORDS[join_type]
    if "value" in table_expr and "select" in table_expr["value"]:
        table_expr = table_expr["value"]

    p_table_expr = "\n  ".join(_parse_query(table_expr))
    if "name" in join_expr[join_type]:
        table_name = join_expr[join_type]["name"]
    else:
        table_name = p_table_expr.split(" ", maxsplit=1)[0].strip()
    on_expr = _parse_expression(join_expr["on"])
    on_expr = _rewrite_table_refs(on_expr, table_name)
    _db_print(table_expr, kql_join_type, p_table_expr)

    return f"| join kind={kql_join_type} ({p_table_expr}) on {on_expr}"


def _process_like(expression: Dict[str, Any]) -> str:
    """Process Like clause."""
    left = _parse_expression((expression[LIKE][0]))
    literal, right = _is_literal(expression[LIKE][1])
    if not (literal and isinstance(right, str)):
        raise ValueError(
            f"Right side operand {right} isn't usable in LIKE expression",
            f"{left} LIKE {right}",
        )
    if re.match("^[^%_]+[%_]$", right):
        oper = "startswith"
        right = right.replace("%", "").replace("_", "")
    elif re.match("^[%_][^%_]+$", right):
        oper = "endswith"
        right = right.replace("%", "").replace("_", "")
    elif re.match("^[%_][^%_]+[%_]$", right):
        oper = "contains"
        right = right.replace("%", "").replace("_", "")
    else:
        oper = "matches regex"
        right = right.replace("_", ".").replace("%", ".*")
    right = _quote(right)
    return f"{left} {oper} {right}"


def _create_distinct_list(distinct_select):
    distinct_list = []
    for distinct_item in distinct_select:
        if "name" in distinct_item:
            distinct_list.append(distinct_item["name"])
        else:
            val = _parse_expression(_get_expr_value(distinct_item))
            if val != distinct_item.get("value"):
                # If value was a complex expression we can't use
                # it directly so just revert to distinct *
                distinct_list = ["*"]
                break
            distinct_list.append(val)
    return distinct_list


def _create_order_by(order_by):
    if isinstance(order_by, list):
        return ", ".join(_format_order_item(item) for item in order_by)
    return _format_order_item(order_by)


def _db_print(*args, **kwargs):
    if _DEBUG:
        print(*args, **kwargs)
