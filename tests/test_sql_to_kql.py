# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os
import unittest
from pathlib import Path

import pytest

from ..msticpy.data_proc import sql_to_kql


class TestSqlToKql(unittest.TestCase):
    """Unit test class."""

    def test_sql_statements(self):
        sql1 = """
SELECT DISTINCT Message, Otherfield
FROM apt29Host
WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
    AND EventID BETWEEN 1 AND 10
    AND LOWER(ParentImage) LIKE '%explorer.exe'
    AND EventID IN ('4', '5', '6')
    AND LOWER(Image) LIKE "3aka3%"
LIMIT 10
"""
        kql1 = """
SecurityEvent
| where Channel == 'Microsoft-Windows-Sysmon/Operational'
  and EventID between (1 .. 10)
  and tolower(ParentImage) endswith 'explorer.exe'
  and EventID in ('4', '5', '6')
  and tolower(Image) startswith '3aka3'
| project Message, Otherfield
| distinct *
| limit 10
"""
        kql_query = sql_to_kql.sql_to_kql(sql1, {"apt29Host": "SecurityEvent"})
        for line_test, line_expected in zip(
            kql_query.strip().split("\n"), kql1.strip().split("\n")
        ):
            self.assertEqual(line_test.strip(), line_expected.strip())

        sql2 = """
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
"""
        kql2 = """
apt29Host
| project EventID, ParentImage, Image, Message, Otherfield
| join kind=inner (MyTable
  | project Message, foo) on $right.Message == $left.Message
  and $right.foo == $left.EventID
| where Channel == 'Microsoft-Windows-Sysmon/Operational'
  and EventID == 1
  and tolower(ParentImage) endswith 'explorer.exe'
  and tolower(Image) startswith '.*3aka3'
| summarize any(Message), any(Otherfield), dcount(EventID) by EventID
| order by Message desc, Otherfield
| limit 10
"""

        kql_query = sql_to_kql.sql_to_kql(sql2)
        for line_test, line_expected in zip(
            kql_query.strip().split("\n"), kql2.strip().split("\n")
        ):
            self.assertEqual(line_test.strip(), line_expected.strip())

        sql3 = """
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
WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
    AND EventID = 1
    AND LOWER(Image) LIKE '%powershell.exe'
"""
        kql3 = """
apt29Host
| join kind=inner (apt29Host
  | where Channel == 'Microsoft-Windows-Sysmon/Operational'
  and EventID == 1
  and tolower(ParentImage) matches regex '.*\â€Ž|â€|â€ª|â€«|â€¬|â€|â€®.*'
  and tolower(Image) endswith 'cmd.exe'
  | project ProcessGuid) on $left.ParentProcessGuid == $left.ProcessGuid
| where Channel == 'Microsoft-Windows-Sysmon/Operational'
  and EventID == 1
  and tolower(Image) endswith 'powershell.exe'
| project Message
"""
        kql_query = sql_to_kql.sql_to_kql(sql3)
        for line_test, line_expected in zip(
            kql_query.strip().split("\n"), kql3.strip().split("\n")
        ):
            self.assertEqual(line_test.strip(), line_expected.strip())

        sql4 = """
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
        LIMIT 10
    )
GROUP BY Message
ORDER BY Message DESC, Otherfield
"""
        kql4 = """
apt29Host
| project EventID, ParentImage, Image, Message, Otherfield
| union (apt29Host
  | project EventID, ParentImage, Image, Message, Otherfield
  | join kind=inner (MyTable) on $right.mssg == $left.Message
  | where Channel == 'Microsoft-Windows-Sysmon/Operational'
  and EventID == 1
  and tolower(ParentImage) endswith 'explorer.exe'
  and tolower(Image) startswith '.*3aka3'
  | project Message, Otherfield, EventID
  | distinct *
  | limit 10
)
| distinct *
| summarize any(Message), count(Otherfield) by Message
| order by Message desc, Otherfield
"""
        kql_query = sql_to_kql.sql_to_kql(sql4)
        for line_test, line_expected in zip(
            kql_query.strip().split("\n"), kql4.strip().split("\n")
        ):
            self.assertEqual(line_test.strip(), line_expected.strip())
