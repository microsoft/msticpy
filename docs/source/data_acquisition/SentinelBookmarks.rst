Microsoft Sentinel Bookmarks
============================

List Bookmarks
--------------

`list_bookmarks` returns a list of all the bookmarks saved in the workspace. This includes details of the bookmark, who
created it, when and with what details. It also includes query text that can be executed with a
`QueryProvider` in order to get the details of the bookmark's logs.

See :py:meth:`list_bookmarks <msticpy.context.azure.sentinel_core.MicrosoftSentinel.list_bookmarks>`

.. code:: ipython3

    sentinel.list_bookmarks()

Create Bookmarks
----------------

You can create a bookmark with `create_bookmark`. When creating a bookmark the following details
are needed:
-name: The name of the bookmark to use.
-query: The KQL query that generated the bookmark.
-results: The results of the query to include with the bookmark, in a string format, by default None
-notes: Any notes you want associated with the bookmark, by default None
-labels: Any labels you want associated with the bookmark, by default None

See :py:meth:`create_bookmark <msticpy.context.azure.sentinel_core.MicrosoftSentinel.create_bookmark>`

.. code:: ipython3

    sentinel.create_bookmark(name="Custom Bookmark", query="SecurityAlert | take 10", notes="Found by Pete")

Delete Bookmarks
----------------

Bookmarks can be deleted by calling `delete_bookmark` and passing in a bookmark name, or a GUID of a bookmark.
If the bookmark cannot be found then an error will be raised.

See :py:meth:`delete_bookmark <msticpy.context.azure.sentinel_core.MicrosoftSentinel.delete_bookmark>`

.. code:: ipython3

    sentinel.delete_bookmark(bookmark="8c9f6b38-53dc-458a-b436-350845224e3a")