Microsoft Sentinel Watchlists
=============================

List Watchlists
---------------

You can get details of the Watchlists in your workspace with 'list_watchlists'.

See :py:meth:`list_watchlists <msticpy.context.azure.MicrosoftSentinel.list_watchlists>`

.. code:: ipython3

    sentinel.list_watchlists()

Create Watchlists
-----------------

New watchlists can be created with `create_watchlist`. A number of things are needed when creating a new
watchlist.
-watchlist_name: The name of the Watchlist you want to create.
-description: A description of the Watchlist to be created.
-search_key: The search key is used to optimize query performance when using watchlists for joins with other data. This should be the key column that will be used in the Watchlist when joining to other data tables.
-provider: This is the label attached to the Watchlist showing who created it, by default "MSTICPy"
-source: The source of the data to be put in the watchlist, by default "Notebook"
-data: A dataframe with the data you want to upload to the watchlist. This must include a column with the same name as the search_key specified.

An error will be raised if the Watchlist name already exists.

.. code:: ipython3

    sentinel.create_watchlist(watchlist_name="Custom Watchlist",
        description="A Watchlist created from a notebook",
        search_key="IPAddress",
        data = df,
    )

Items in a Watchlist
--------------------

You can get the items in a Watchlist with `list_watchlist_items`. Call this and pass it the name of the
watchlist to get items from.

See :py:meth:`list_watchlist_items <msticpy.context.azure.MicrosoftSentinel.list_watchlist_items>`

.. code:: ipython3

    sentinel.list_watchlist_items(watchlist_name="Custom Watchlist")

Each row in the returned DataFrame will represent an item in the Watchlist, and will include details such
as who created it and when. Columns containing the data items from the Watchlist are prefixed with 'properties.itemsKeyValue.'.

You can also add items to a Watchlist with `add_watchlist_item` by passing in a Watchlist name and
the items to add. The items to add can be in the form of a Pandas Series, DataFrame or as a dictionary of items.

See :py:meth:`add_watchlist_item <msticpy.context.azure.MicrosoftSentinel.add_watchlist_item>`

.. code:: ipython3

    sentinel.add_watchlist_item(watchlist_name="Custom Watchlist",
        item={"IPAddress": ", "13.107.6.152", "Location": "USA"}
    )

An error will be raised if the item you are trying to add already exists in the Watchlist.

Delete Watchlists
-----------------

Existing Watchlists can be deleted with `delete_watchlist` by passing it the Watchlist name to be
deleted.

See :py:meth:`delete_watchlist <msticpy.context.azure.MicrosoftSentinel.delete_watchlist>`

.. code:: ipython3

    sentinel.delete_watchlist(watchlist_name="Custom Watchlist")
