SQL TO KQL Conversion (Experimental)
====================================

The ``sql_to_kql`` module is a simple converter to KQL based on
`mo-sql-parsing <https://github.com/klahnakoski/mo-sql-parsing>`__. It is an
experimental feature built to help us convert a few queries but we
thought that it was useful enough to include in MSTICPy.

It supports a subset of ANSI SQL-92 which includes the following:

-  SELECT (including column renaming and functions)
-  FROM (including from subquery)
-  WHERE (common string and int operations, LIKE, some common
   functions)
-  LIMIT
-  UNION, UNION ALL
-  JOIN - only tested for relatively simple join expressions
-  GROUP BY
-  ORDER BY

SQL comments are removed from the output but should not break
the parser.

It does not support HAVING, multiple SQL statements or anything complex
and fancy like Common Table Expressions. It also does not
support modification such as INSERT and UPDATE nor any DML.

It does support a few additional Spark SQL extensions like RLIKE.

Caveat Emptor!
--------------

This module is included in MSTICPy in the hope that it might be useful
to others. We do not intend to expand its capabilities.

It is also not guaranteed to produce perfectly-executing KQL - there
will likely be things that you have to fix up in the output query. You
will, for example, nearly always need change the names of the fields
used since the source data tables are unlikely to exactly match the
schema of your Kusto/Azure Sentinel target.

The module does include an elementary table name mapping function that
we demonstrate below.

.. code:: ipython3

    from msticpy.data.sql_to_kql import sql_to_kql


Simple SQL Query
----------------

.. code:: ipython3

    sql = """
    SELECT DISTINCT Message, Otherfield
    FROM apt29Host
    WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
        AND EventID BETWEEN 1 AND 10
        AND LOWER(ParentImage) LIKE '%explorer.exe'
        AND EventID IN ('4', '5', '6')
        AND LOWER(Image) LIKE "3aka3%"
    LIMIT 10
    """

    kql = sql_to_kql(sql)
    print(kql)


.. parsed-literal::

    apt29Host
    | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID between (1 .. 10)
      and tolower(ParentImage) endswith 'explorer.exe'
      and EventID in ('4', '5', '6')
      and tolower(Image) startswith '3aka3'
    | project Message, Otherfield
    | distinct Message, Otherfield
    | limit 10


SQL Joins
---------

.. code:: ipython3

    sql="""
    SELECT DISTINCT Message, Otherfield, COUNT(DISTINCT EventID)
    FROM (SELECT EventID, ParentImage, Image, Message, Otherfield FROM apt29Host) as A
    --FROM A
    INNER JOIN (Select Message, evt_id FROM MyTable ) on MyTable.Message == A.Message and MyTable.evt_id == A.EventID
    WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
        AND EventID = 1
        AND LOWER(ParentImage) LIKE "%explorer.exe"
        AND LOWER(Image) RLIKE ".*3aka3%"
    GROUP BY EventID
    ORDER BY Message DESC, Otherfield
    LIMIT 10
    """

    kql = sql_to_kql(sql)
    print(kql)


.. parsed-literal::

    apt29Host
    | project EventID, ParentImage, Image, Message, Otherfield
    | join kind=inner (MyTable
      | project Message, evt_id) on $right.Message == $left.Message
      and $right.evt_id == $left.EventID
    | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID == 1
      and tolower(ParentImage) endswith 'explorer.exe'
      and tolower(Image) startswith '.*3aka3'
    | summarize any(Message), any(Otherfield), dcount(EventID) by EventID
    | order by Message desc, Otherfield
    | limit 10


Table Renaming
--------------

.. code:: ipython3

    sql="""
    SELECT DISTINCT Message, Otherfield, COUNT(DISTINCT EventID)
    FROM (SELECT EventID, ParentImage, Image, Message, Otherfield FROM apt29Host) as A
    INNER JOIN (Select Message, evt_id FROM MyTable ) on MyTable.Message == A.Message and MyTable.evt_id == A.EventID
    WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
        AND EventID = 1
        AND LOWER(ParentImage) LIKE "%explorer.exe"
        AND LOWER(Image) RLIKE ".*3aka3%"
    GROUP BY EventID
    ORDER BY Message DESC, Otherfield
    LIMIT 10
    """

    table_map = {"apt29Host": "SecurityEvent", "MyTable": "SigninLogs"}

    kql = sql_to_kql(sql, table_map)
    print(kql)


.. parsed-literal::

    SecurityEvent
    | project EventID, ParentImage, Image, Message, Otherfield
    | join kind=inner (SigninLogs
      | project Message, evt_id) on $right.Message == $left.Message
      and $right.evt_id == $left.EventID
    | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID == 1
      and tolower(ParentImage) endswith 'explorer.exe'
      and tolower(Image) startswith '.*3aka3'
    | summarize any(Message), any(Otherfield), dcount(EventID) by EventID
    | order by Message desc, Otherfield
    | limit 10


Join with Aliases
-----------------

.. code:: ipython3

    sql="""
    SELECT Message
    FROM apt29Host a
    INNER JOIN (
        SELECT ProcessGuid
        FROM apt29Host
        WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
            AND EventID = 1
            AND LOWER(ParentImage) RLIKE '.*partial_string.*'
            AND LOWER(Image) LIKE '%cmd.exe'
    ) b
    ON a.ParentProcessGuid = b.ProcessGuid
    WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
        AND EventID = 1
        AND LOWER(Image) LIKE '%powershell.exe'
    """

    kql = sql_to_kql(sql, table_map)
    print(kql)


.. parsed-literal::

    SecurityEvent
    | join kind=inner (SecurityEvent
      | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID == 1
      and tolower(ParentImage) matches regex '.*partial.string.*'
      and tolower(Image) endswith 'cmd.exe'
      | project ProcessGuid) on $left.ParentProcessGuid == $right.ProcessGuid
    | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID == 1
      and tolower(Image) endswith 'powershell.exe'
    | project Message


Unions and Group By
-------------------

.. code:: ipython3

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
        LIMIT 10
        )
    GROUP BY Message
    ORDER BY Message DESC, Otherfield
    """

    kql = sql_to_kql(sql, table_map)
    print(kql)


.. parsed-literal::

    SecurityEvent
    | project EventID, ParentImage, Image, Message, Otherfield
    | union (SecurityEvent
      | project EventID, ParentImage, Image, Message, Otherfield
      | join kind=inner (SigninLogs) on $right.mssg == $left.Message
      | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID == 1
      and tolower(ParentImage) endswith 'explorer.exe'
      and tolower(Image) startswith '.*3aka3'
      | project Message, Otherfield, EventID
      | distinct Message, Otherfield, EventID
    )
    | distinct *
    | limit 10
    | summarize any(Message), count(Otherfield) by Message
    | order by Message desc, Otherfield


Aliased and Calculated Select Columns
-------------------------------------

.. code:: ipython3

    sql="""
    SELECT DISTINCT Message as mssg, COUNT(Otherfield)
    FROM (SELECT EventID as ID, ParentImage, Image, Message,
        ParentImage + Message as ParentMessage,
        LOWER(Otherfield) FROM apt29Host
        )
    WHERE Channel = "Microsoft-Windows-Sysmon/Operational"
        AND EventID = 1
        AND LOWER(ParentImage) LIKE "%explorer.exe"
    """
    kql = sql_to_kql(sql, table_map)
    print(kql)


.. parsed-literal::

    SecurityEvent
    | extend ParentMessage = ParentImage + Message, Otherfield = tolower(Otherfield)
    | project ID = EventID, ParentImage, Image, Message, ParentMessage, Otherfield
    | where Channel == 'Microsoft-Windows-Sysmon/Operational'
      and EventID == 1
      and tolower(ParentImage) endswith 'explorer.exe'
    | extend Otherfield = count(Otherfield)
    | project mssg = Message, Otherfield
    | distinct *

