# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for SQL to KQL Conversion.

Supports Limited subset of ANSI SQL

"""
import re
from typing import List, Tuple, Any, Union, Dict, Optional

import moz_sql_parser
from moz_sql_parser import parse

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# TODO:
# count takes no argument
# ordering of $left/$right?
# renaming of columns after summarize

SPARK_KQL_FUNC_MAP = {
    "avg": ("avg", None, None),
    "base64": ("base64_encode_tostring", None, None),
    "concat": ("strcat", None, None),
    "count": ("count", None, None),
    "if": ("iif", None, None),
    "iif": ("iif", None, None),
    "ifnull": (None, None, "iif(isnull({p0}), {p1}, {p0}))"),
    "in": ("in", None, None),
    "instr": ("indexof", None, None),
    "int": ("toint", None, None),
    "isnotnull": ("isnotnull", None, None),
    "isnull": ("isnull", None, None),
    "left": ("NA", None, None),
    "length": ("strlen", None, None),
    "like": ("NA", None, None),
    "locate": ("indexof", None, None),
    "lower": ("tolower", None, None),
    "lcase": ("tolower", None, None),
    "ltrim": ("trim_start", None, None),
    "max": ("max", None, None),
    "mean": ("mean", None, None),
    "min": ("min", None, None),
    "position": ("indexof", None, None),
    "regex_extract": ("extract", "{p1}, {p0}", None),  # swap params 0, 1
    "replace": ("replace", None, None),
    "reverse": ("reverse", None, None),
    "rtrim": ("trim_end", None, None),
    "split": ("split", None, None),
    "string": ("tostring", None, None),
    "str": ("tostring", None, None),
    "substr": ("substring", None, None),
    "substring": ("substring", None, None),
    "sum": ("sum", None, None),
    "todatetime": ("to_date", None, None),
    "translate": ("translate", None, None),
    "trim": ("trim", None, None),
    "unbase64": ("base64_decode_tostring", None, None),
    "upper": ("toupper", None, None),
}


# unmapped_funcs = {
#     "NA": "base64_decode_toarray",
#     "NA": "countof",
#     "NA": "extract_all",
#     "NA": "extractjson",
#     "NA": "isempty",
#     "NA": "isnotempty",
#     "NA": "parse_csv",
#     "NA": "parse_ipv4",
#     "NA": "parse_json",
#     "NA": "parse_url",
#     "NA": "parse_urlquery",
#     "NA": "parse_version",
#     "NA": "right",
#     "NA": "rlike",
#     "NA": "strcat_delim",
#     "NA": "strcmp",
#     "NA": "strrep",
#     "NA": "url_decode",
#     "NA": "url_encode",
# }

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
NOT_BETWEEN = "not between"
NOT_IN = "not in"
NOT_LIKE = "not like"
OFFSET = "offset"
ON = "on"
OR = "or"
ORDER_BY = "orderby"
RIGHT_JOIN = "right join"
RIGHT_OUTER_JOIN = "right outer join"
SELECT = "select"
THEN = "then"
UNION = "union"
UNION_ALL = "union all"
USING = "using"
WHEN = "when"
WHERE = "where"
WITH = "with"

# override/add keywords
RLIKE = "rlike"


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


BINARY_OPS = {val: key for key, val in moz_sql_parser.keywords.binary_ops.items()}
BINARY_OPS["eq"] = "=="
BINARY_OPS["neq"] = "!="
BINARY_OPS["nin"] = "!in"
BINARY_OPS["rlike"] = "not matches regex"
BINARY_OPS["nlike"] = "not matches regex"
BINARY_OPS["concat"] = "+"
BINARY_OPS["is"] = "=="
BINARY_OPS["is not"] = "!="


REMAPPED_KEYWORDS = {"RLIKE": "LIKE"}


def sql_to_kql(sql: str, target_tables: Dict[str, str] = None) -> str:
    """Parse SQL and return KQL equivalent."""
    # ensure literals are surrounded by single quotes
    sql = _single_quote_strings(sql)

    # replace table names
    if target_tables:
        for table in target_tables:
            sql = sql.replace(table, target_tables[table])
    # replace keywords
    sql = _remap_kewords(sql)
    parsed_sql = parse(sql)
    query_lines = _parse_query(parsed_sql)
    return "\n".join([line for line in query_lines if line.strip()])


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
        parsed_sql.pop(SELECT)

    distinct_select = False
    if SELECT in parsed_sql:
        distinct_select, expr_list = _is_distinct(parsed_sql[SELECT])
        _process_select(parsed_sql[SELECT], expr_list, query_lines)

    if ORDER_BY in parsed_sql:
        if isinstance(parsed_sql[ORDER_BY], list):
            order_list = ", ".join(
                [_format_order_item(item) for item in parsed_sql[ORDER_BY]]
            )
        else:
            order_list = _format_order_item(parsed_sql[ORDER_BY])
        query_lines.append(f"| order by {order_list}")
    if distinct_select:
        query_lines.append("| distinct *")
    if LIMIT in parsed_sql:
        query_lines.append(f"| limit {parsed_sql[LIMIT]}")
    if UNION in parsed_sql:
        union_subquery = {UNION_ALL: parsed_sql[UNION]}
        query_lines.extend(_parse_query(union_subquery))
        query_lines.append(f"| distinct *")
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
        query_lines.extend(_parse_query(from_expr["value"]))
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
    if parsed_sql != "*":
        print(expr_list, type(expr_list))
        select_list = expr_list if isinstance(expr_list, list) else [expr_list]
        project_expr = ", ".join(
            [_parse_expression(item["value"]) for item in select_list]
        )
        query_lines.append(f"| project {project_expr}")


def _process_group_by(parsed_sql: Dict[str, Any], query_lines: List[str]):
    """Process GROUP BY clause."""
    group_by_expr = parsed_sql[GROUP_BY]
    group_by_expr = (
        group_by_expr if isinstance(group_by_expr, list) else [group_by_expr]
    )
    by_clause = ", ".join([val["value"] for val in group_by_expr if val.get("value")])
    _, expr_list = _is_distinct(parsed_sql[SELECT])
    group_by_expr_list = []
    for expr in expr_list:
        if isinstance(expr.get("value"), str):
            group_by_expr_list.append(f"any({expr['value']})")
        else:
            group_by_expr_list.append(_parse_expression(expr.get("value")))
    query_lines.append(f"| summarize {', '.join(group_by_expr_list)} by {by_clause}")


# pylint: disable=too-many-return-statements, too-many-branches
def _parse_expression(expression):  # noqa: MC0001
    """Return parsed expression."""
    if _is_literal(expression)[0]:
        return _quote_literal(expression)
    if not isinstance(expression, dict):
        return expression
    if AND in expression:
        return "\n  and ".join([_parse_expression(expr) for expr in expression[AND]])
    if OR in expression:
        return "\n  or ".join([_parse_expression(expr) for expr in expression[OR]])
    if NOT in expression:
        return f" not ({_parse_expression(expression[NOT])})"
    if BETWEEN in expression:
        args = expression[BETWEEN]
        return f"{args[0]} between ({args[1]} .. {args[2]})"
    if NOT_BETWEEN in expression:
        args = expression[NOT_BETWEEN]
        return f"{args[0]} not between ({args[1]} .. {args[2]})"
    if IN in expression or NOT_IN in expression:
        sql_op = IN if IN in expression else NOT_IN
        kql_op = IN if IN in expression else "!in"
        args = expression[sql_op]

        right = _quote_literal(args[1])
        if isinstance(right, list):
            print(args[1])
            arg_list = ", ".join([str(_parse_expression(l_item)) for l_item in right])
            return f"{args[0]} {kql_op} ({arg_list})"
        sub_query = "\n".join(_parse_query(right))
        return f"{args[0]} {kql_op} ({sub_query})"

    # Handle other operators
    oper = list(expression.keys())[0] if expression else None
    if oper in BINARY_OPS:
        right = _quote_literal(expression[oper][1])
        left = _quote_literal(expression[oper][0])
        return f"{left} {BINARY_OPS[oper]} {right}"
    if LIKE in expression:
        return _process_like(expression)

    # For everything else, assume it's a function
    if expression:
        func, operand = next(iter(expression.items()))
        #         set_trace()
        return _map_func(func, operand)
    return "EXPRESSION {expression} not resolved."


# pylint: enable=too-many-return-statements, too-many-branches


def _map_func(func: str, *args) -> str:
    """Return KQL function for SQL function."""
    func = func.lower().strip()
    args_dict = {f"p{idx}": arg for idx, arg in enumerate(args)}
    def_arg_fmt = ", ".join([f"{{{arg}}}" for arg in args_dict])
    if func not in SPARK_KQL_FUNC_MAP:
        func_fmt = f"{func}({def_arg_fmt}) // WARNING unmapped function\n"
        return func_fmt.format(**args_dict)
    func_map = SPARK_KQL_FUNC_MAP[func]

    if func == "count" and isinstance(args[0], dict):
        if next(iter(args[0])) == "distinct":
            return f"dcount({args[0]['distinct']})"

    if not func_map[1] and not func_map[2]:
        func_fmt = f"{func_map[0]}({def_arg_fmt})"
        return func_fmt.format(**args_dict)
    if func_map[1]:
        func_fmt = f"{func_map[0]}({func_map[1]})"
        return func_fmt.format(**args_dict)
    if func_map[2]:
        func_fmt = f"{func_map[2]}"
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


def _remap_kewords(sql: str) -> str:
    """Replace keywords in source SQL statement."""
    for repl_kw in REMAPPED_KEYWORDS:
        sql = re.sub(f"\\s{repl_kw}\\s", f" {REMAPPED_KEYWORDS[repl_kw]} ", sql)
    return sql


def _is_distinct(
    expression_list: Union[Dict[str, Any], List[Dict[str, Any]]]
) -> Tuple[bool, List[Dict[str, Any]]]:
    """Check for DISTINCT in SELECT clause."""
    expr_list_out = []
    distinct = False
    if isinstance(expression_list, dict):
        expression_list = [expression_list]
    if isinstance(expression_list, list):
        for expr in expression_list:
            print(expr)
            if "value" in expr and DISTINCT in expr["value"]:
                distinct = True
                expr_list_out.append({"value": expr["value"][DISTINCT]})
            else:
                expr_list_out.append(expr)
    return distinct, expr_list_out


def _format_order_item(item: Dict[str, Any]) -> str:
    """Return ORDER BY item with sort direction."""
    if "sort" in item:
        return f"{item['value']} {item['sort'].lower()}"
    return f"{item['value']}"


def _get_join_list(parsed_sql: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
    """Return list of JOIN sub-expressions."""
    join_list = []
    if isinstance(parsed_sql, list):
        for from_source in parsed_sql:
            if not isinstance(from_source, dict):
                continue
            join = JOIN_KEYWORDS & from_source.keys()
            if join:
                join_list.append(from_source)
        return join_list
    return []


def _rewrite_table_refs(join_expr: Union[Any, str, List], table_expr: str) -> str:
    """Rewrite dotted prefixes."""
    p_expr = _parse_expression(join_expr)
    prefixes = re.findall(r"\w+\.", p_expr)
    if not prefixes:
        return p_expr
    if f"{table_expr}." in prefixes:
        p_expr = p_expr.replace(f"{table_expr}.", "$right.")
        prefixes.remove(f"{table_expr}.")
    for prefix in prefixes:
        p_expr = p_expr.replace(prefix, "$left.")
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
    if "name" in join_expr:
        table_name = join_expr["name"]
    else:
        table_name = p_table_expr.split(" ")[0].strip()
    on_expr = _parse_expression(join_expr["on"])
    on_expr = _rewrite_table_refs(on_expr, table_name)
    print(table_expr, kql_join_type, p_table_expr)

    return f"| join kind={kql_join_type} ({p_table_expr}) on {on_expr}"


def _process_like(expression: Dict[str, Any]) -> str:
    """Process Like clause."""
    left = _quote_literal(expression[LIKE][0])
    literal, right = _is_literal(expression[LIKE][1])
    if isinstance(right, str):
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
        if isinstance(left, dict):
            left = _parse_expression(left)
    right = _quote(right) if literal else right
    return f"{left} {oper} {right}"
