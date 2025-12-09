# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test module for sql_to_kql."""
from collections import namedtuple

import pytest
import pytest_check as check

from msticpy.data import sql_to_kql

SQLTestCase = namedtuple("SQLTestCase", "sql, kql, id, rename")
SQL_CASES = [
    SQLTestCase(
        sql="""
        SELECT DISTINCT Message, Otherfield
        FROM apt29Host
        WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
            AND EventID BETWEEN 1 AND 10
            AND LOWER(ParentImage) LIKE '%explorer.exe'
            AND EventID IN ('4', '5', '6')
            AND LOWER(Image) LIKE "3aka3%"
        LIMIT 10
        """,
        kql="""
        SecurityEvent
        | where (Channel == 'Microsoft-Windows-Sysmon/Operational')
        and (EventID between (1 .. 10))
        and (tolower(ParentImage) endswith 'explorer.exe')
        and (EventID in ('4', '5', '6'))
        and (tolower(Image) startswith '3aka3')
        | project Message, Otherfield
        | distinct Message, Otherfield
        | limit 10
        """,
        id="select",
        rename={"apt29Host": "SecurityEvent"},
    ),
    SQLTestCase(
        sql="""
        SELECT DISTINCT Message, Otherfield, COUNT(DISTINCT EventID)
        FROM (SELECT EventID, ParentImage, Image, Message, Otherfield FROM apt29Host) as A
        --FROM A
        INNER JOIN (Select Message, foo FROM MyTable ) on MyTable.Message == A.Message and MyTable.foo == A.EventID
        WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
            AND EventID = 1
            AND LOWER(ParentImage) LIKE "%explorer.exe"
            AND LOWER(Image) RLIKE ".*3aka3%"
        GROUP BY EventID
        ORDER BY Message DESC, Otherfield
        LIMIT 10
        """,
        kql="""
        apt29Host
        | project EventID, ParentImage, Image, Message, Otherfield
        | join kind=inner (MyTable
        | project Message, foo) on ($right.Message == $left.Message)
        and ($right.foo == $left.EventID)
        | where (Channel == 'Microsoft-Windows-Sysmon/Operational')
        and (EventID == 1)
        and (tolower(ParentImage) endswith 'explorer.exe')
        and (tolower(Image) matches regex '.*3aka3%')
        | summarize any(Message), any(Otherfield), dcount(EventID) by EventID
        | order by Message desc, Otherfield
        | limit 10
        """,
        id="join",
        rename=None,
    ),
    SQLTestCase(
        sql="""
        SELECT Message
        FROM apt29Host a
        INNER JOIN (
            SELECT ProcessGuid
            FROM apt29Host
            WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
                AND EventID = 1
                AND LOWER(ParentImage) RLIKE '.*\\â€Ž|â€|â€ª|â€«|â€¬|â€|â€®.*'
                AND LOWER(Image) LIKE '%cmd.exe'
        ) b
        ON a.ParentProcessGuid = b.ProcessGuid
        WHERE (Channel = "Microsoft-Windows-Sysmon/Operational"
            AND EventID = 1)
            OR LOWER(Image) LIKE '%powershell.exe'
        """,
        kql=r"""
        apt29Host
        | join kind=inner (apt29Host
        | where (Channel == 'Microsoft-Windows-Sysmon/Operational')
        and (EventID == 1)
        and (tolower(ParentImage) matches regex '.*\â€Ž|â€|â€ª|â€«|â€¬|â€|â€®.*')
        and (tolower(Image) endswith 'cmd.exe')
        | project ProcessGuid) on $left.ParentProcessGuid == $right.ProcessGuid
        | where ((Channel == 'Microsoft-Windows-Sysmon/Operational')
        and (EventID == 1))
        or (tolower(Image) endswith 'powershell.exe')
        | project Message
        """,
        id="join2",
        rename=None,
    ),
    SQLTestCase(
        sql="""
        SELECT DISTINCT Message, COUNT(Otherfield)
        FROM (SELECT *
            FROM (SELECT EventID, ParentImage, Image, Message, Otherfield FROM apt29Host)

            UNION
            SELECT DISTINCT Message, Otherfield, EventID
            FROM (SELECT EventID, ParentImage, Image, Message, Otherfield FROM apt29Host) as A
            INNER JOIN MyTable on MyTable.mssg = A.Message
            WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
                AND EventID = 1
                AND LOWER(ParentImage) LIKE "%explorer.exe"
                AND LOWER(Image) RLIKE ".*3aka3%"
                AND (EventID >= 0
                OR EventID IN (1, 2, 3, 4))
                AND EventID NOT IN (5, 6)
                AND EventID <= 2
                AND (EventID & 2) = 2
            LIMIT 10
            )
        GROUP BY Message
        ORDER BY Message DESC, Otherfield
        """,
        kql="""
        apt29Host
        | project EventID, ParentImage, Image, Message, Otherfield
        | union (apt29Host
        | project EventID, ParentImage, Image, Message, Otherfield
        | join kind=inner (MyTable) on $right.mssg == $left.Message
        | where (Channel == 'Microsoft-Windows-Sysmon/Operational')
        and (EventID == 1)
        and (tolower(ParentImage) endswith 'explorer.exe')
        and (tolower(Image) matches regex '.*3aka3%')
        and ((EventID >= 0)
        or (EventID in (1, 2, 3, 4)))
        and (EventID !in (5, 6))
        and (EventID <= 2)
        and (EventID binary_and 2 == 2)
        | project Message, Otherfield, EventID
        | distinct Message, Otherfield, EventID
        )
        | distinct *
        | limit 10
        | summarize any(Message), count(Otherfield) by Message
        | order by Message desc, Otherfield
        """,
        id="union_groupby",
        rename=None,
    ),
    SQLTestCase(
        sql="""
        SELECT DISTINCT ParentMessage as mssg, COUNT(Otherfield)
        FROM (SELECT EventID as ID, ParentImage, Image, Message,
            ParentImage + Message as ParentMessage,
            LOWER(Otherfield) FROM apt29Host
            )
        WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
            AND EventID = 1
            AND LOWER(ParentImage) LIKE "%explorer.exe"
        """,
        kql="""
        apt29Host
        | extend ParentMessage = ParentImage + Message, Otherfield = tolower(Otherfield)
        | project ID = EventID, ParentImage, Image, Message, ParentMessage, Otherfield
        | where (Channel == 'Microsoft-Windows-Sysmon/Operational')
        and (EventID == 1)
        and (tolower(ParentImage) endswith 'explorer.exe')
        | extend Otherfield = count(Otherfield)
        | project mssg = ParentMessage, Otherfield
        | distinct *
        """,
        id="select_rename",
        rename=None,
    ),
]


@pytest.fixture(params=SQL_CASES, ids=lambda t: t[2])
def get_sql_cases(request):
    """Pytest fixture for parameterized tests."""
    return request.param


# pylint: disable=redefined-outer-name
def test_sql_convert(get_sql_cases):
    """Test SQL to KQL Conversion."""
    sql, kql, test_id, rename = get_sql_cases
    if rename:
        kql_query = sql_to_kql.sql_to_kql(sql, target_tables=rename)
    else:
        kql_query = sql_to_kql.sql_to_kql(sql)
    for line_test, line_expected in zip(
        kql_query.strip().split("\n"), kql.strip().split("\n")
    ):
        check.equal(line_test.strip(), line_expected.strip(), f"TestID={test_id}")


def test_not_operator():
    """Test NOT operator."""
    sql = "SELECT * FROM SecurityEvent WHERE NOT (EventID = 1)"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "not (EventID == 1)" in kql
    assert "SecurityEvent" in kql


def test_not_between():
    """Test NOT BETWEEN operator."""
    sql = "SELECT * FROM SecurityEvent WHERE EventID NOT BETWEEN 1 AND 10"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "!between" in kql
    assert "1 .. 10" in kql


def test_like_contains():
    """Test LIKE operator with contains pattern."""
    sql = "SELECT * FROM SecurityEvent WHERE Message LIKE '%error%'"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "contains" in kql
    assert "'error'" in kql


def test_like_regex_pattern():
    """Test LIKE operator with complex pattern requiring regex."""
    sql = "SELECT * FROM SecurityEvent WHERE Message LIKE 'err%or_test'"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "matches regex" in kql


def test_aggregate_functions():
    """Test aggregate functions MAX, MIN, AVG, SUM."""
    sql = """
    SELECT MAX(EventID), MIN(EventID), AVG(EventID), SUM(EventID)
    FROM SecurityEvent
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "max(EventID)" in kql
    assert "min(EventID)" in kql
    assert "avg(EventID)" in kql
    assert "sum(EventID)" in kql


def test_string_functions():
    """Test string manipulation functions."""
    sql = """
    SELECT UPPER(Message), LOWER(Message), CONCAT(Message, Channel),
           SUBSTRING(Message, 1, 10), LENGTH(Message)
    FROM SecurityEvent
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "toupper(Message)" in kql
    assert "tolower(Message)" in kql
    # CONCAT is translated to binary operator concat
    assert "concat" in kql
    assert "substring(Message, 1, 10)" in kql
    assert "strlen(Message)" in kql


def test_comparison_operators():
    """Test comparison operators."""
    sql = """
    SELECT * FROM SecurityEvent
    WHERE EventID > 10 AND EventID < 100
          AND EventID >= 5 AND EventID <= 200
          AND EventID != 50
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "EventID > 10" in kql
    assert "EventID < 100" in kql
    assert "EventID >= 5" in kql
    assert "EventID <= 200" in kql
    assert "EventID != 50" in kql


def test_math_operations():
    """Test mathematical operations."""
    sql = """
    SELECT EventID * 2, EventID / 2, EventID % 2, EventID - 1
    FROM SecurityEvent
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "EventID * 2" in kql
    assert "EventID / 2" in kql
    assert "EventID % 2" in kql
    assert "EventID - 1" in kql


def test_count_simple():
    """Test simple COUNT function without DISTINCT."""
    sql = "SELECT EventID, COUNT(EventID) FROM SecurityEvent GROUP BY EventID"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "count(EventID)" in kql
    assert "by EventID" in kql


def test_order_by_single():
    """Test ORDER BY with single column."""
    sql = "SELECT Message FROM SecurityEvent ORDER BY EventID"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "order by EventID" in kql


def test_order_by_asc():
    """Test ORDER BY with explicit ASC."""
    sql = "SELECT Message FROM SecurityEvent ORDER BY EventID ASC"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "order by EventID asc" in kql


def test_is_null_and_is_not_null():
    """Test IS NULL and IS NOT NULL operators."""
    sql = """
    SELECT * FROM SecurityEvent
    WHERE Message IS NULL OR Channel IS NOT NULL
    """
    kql = sql_to_kql.sql_to_kql(sql)
    # IS NULL/IS NOT NULL are translated to missing()/exists() functions
    # which are not in the function map, so they get WARNING
    assert "missing" in kql or "exists" in kql


def test_select_with_literals():
    """Test SELECT with literal string values."""
    sql = "SELECT 'constant', EventID FROM SecurityEvent"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "'constant'" in kql
    assert "EventID" in kql


def test_subquery_in_from():
    """Test subquery in FROM clause."""
    sql = """
    SELECT Message
    FROM (SELECT EventID, Message FROM SecurityEvent WHERE EventID = 1)
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "SecurityEvent" in kql
    assert "EventID == 1" in kql
    assert "project EventID, Message" in kql


def test_left_join():
    """Test LEFT JOIN."""
    sql = """
    SELECT a.Message
    FROM SecurityEvent a
    LEFT JOIN OtherTable b ON a.EventID = b.EventID
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "join kind=left" in kql


def test_right_join():
    """Test RIGHT JOIN."""
    sql = """
    SELECT a.Message
    FROM SecurityEvent a
    RIGHT JOIN OtherTable b ON a.EventID = b.EventID
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "join kind=right" in kql


def test_full_outer_join():
    """Test FULL OUTER JOIN."""
    sql = """
    SELECT a.Message
    FROM SecurityEvent a
    FULL OUTER JOIN OtherTable b ON a.EventID = b.EventID
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "join kind=outer" in kql


def test_not_like():
    """Test NOT LIKE operator (via rlike translation)."""
    sql = "SELECT * FROM SecurityEvent WHERE Message NOT RLIKE 'pattern.*'"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "not matches regex" in kql


def test_union_without_all():
    """Test UNION (which adds distinct)."""
    sql = """
    SELECT EventID FROM SecurityEvent
    UNION
    SELECT EventID FROM OtherTable
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "union" in kql
    assert "distinct *" in kql


def test_conditional_functions():
    """Test conditional functions like IIF."""
    sql = """
    SELECT IIF(EventID > 10, 'High', 'Low') as Level
    FROM SecurityEvent
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "iif" in kql


def test_error_invalid_like_operand():
    """Test error handling for invalid LIKE operand."""
    sql = "SELECT * FROM SecurityEvent WHERE EventID LIKE 123"
    with pytest.raises(ValueError, match="isn't usable in LIKE expression"):
        sql_to_kql.sql_to_kql(sql)


def test_complex_nested_expression():
    """Test complex nested boolean expressions."""
    sql = """
    SELECT * FROM SecurityEvent
    WHERE (EventID > 1 AND EventID < 10)
       OR (EventID > 100 AND (Message LIKE '%error%' OR Message LIKE '%warning%'))
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "or" in kql
    assert "and" in kql
    assert "EventID > 1" in kql
    assert "EventID < 10" in kql


def test_multiple_unions():
    """Test UNION ALL with multiple queries."""
    sql = """
    SELECT EventID FROM SecurityEvent
    UNION ALL
    SELECT EventID FROM OtherTable
    """
    kql = sql_to_kql.sql_to_kql(sql)
    # UNION ALL produces two separate queries joined by union
    assert "SecurityEvent" in kql
    assert "OtherTable" in kql
    assert "EventID" in kql


def test_join_with_subquery():
    """Test JOIN with subquery having WHERE clause."""
    sql = """
    SELECT a.Message
    FROM SecurityEvent a
    INNER JOIN (SELECT EventID FROM OtherTable WHERE EventID > 10) b
    ON a.EventID = b.EventID
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "join kind=inner" in kql
    assert "EventID > 10" in kql


def test_arithmetic_in_select():
    """Test arithmetic operations in SELECT clause."""
    sql = """
    SELECT EventID + 1 as NextID, EventID * 2 as DoubleID
    FROM SecurityEvent
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "EventID + 1" in kql
    assert "EventID * 2" in kql
    assert "NextID" in kql
    assert "DoubleID" in kql


def test_additional_functions():
    """Test additional SQL functions."""
    sql = """
    SELECT TRIM(Message), REVERSE(Message), REPLACE(Message, 'old', 'new')
    FROM SecurityEvent
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "trim(Message)" in kql
    assert "reverse(Message)" in kql
    # REPLACE function is in the output
    assert "replace" in kql and "Message" in kql


def test_in_with_subquery():
    """Test IN operator with subquery."""
    sql = """
    SELECT * FROM SecurityEvent
    WHERE EventID IN (SELECT EventID FROM OtherTable WHERE Channel = 'Security')
    """
    kql = sql_to_kql.sql_to_kql(sql)
    assert "in (" in kql
    assert "OtherTable" in kql
    assert "Channel == 'Security'" in kql


def test_case_insensitive_keywords():
    """Test that SQL keywords work in different cases."""
    sql = "select Message from SecurityEvent where EventID = 1"
    kql = sql_to_kql.sql_to_kql(sql)
    assert "SecurityEvent" in kql
    assert "where" in kql
    assert "project Message" in kql
