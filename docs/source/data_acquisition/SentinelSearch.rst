Microsoft Sentinel Search
=========================

Create Search
-------------

You can trigger a Search job with 'create_search'.
When calling this function, you can pass the following parameters:

- 'query': the KQL query to run for the search.
- 'start': the start time of the search. The default is 90 days ago.
- 'end': the end time of the search. The default is now.
- 'search_name': the name to give the search. The default is a random GUID.
- 'timespan': if not passing start and end times you can provide a TimeSpan object.
- 'limit': the max number of results to return, default is 1000.

See :py:meth:`create_search <msticpy.context.azure.MicrosoftSentinel.create_search>`

.. code:: ipython3

    sentinel.create_search(query="SecurityEvent | where * contains 'infected.exe'", search_name="docssearch")

Check Search Status
-------------------

Complex Searches can take some time to complete. You can check the status of a search
job with `check_search_status`.

Pass the function a Search job name and it will display the current status.
If the Search results are ready for querying it will return True, otherwise False.

.. code:: ipython3

    sentinel.check_search_status("docssearch")

If this funciton returns True you can run queries against the KQL table with the
Search name to see the results. Note the table name has '_SRCH' appended to
the name provider to `create_search`:

.. code:: ipython3

    qry_prov.exec_query("docssearch_SRCH | take 10")


Delete a Search
---------------

Once a Search job is not longer useful you can delete it with `delete_search`.
This deletes the table associated with the search.

.. code:: ipython3

    sentinel.delete_search("docssearch")