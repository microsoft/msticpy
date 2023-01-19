Microsoft Sentinel Dynamic Summaries
====================================

Dynamic Summaries are a Sentinel feature that allow you to persist results of
query jobs in a summarized/serialized form. This might be useful for keeping
results of daily watch jobs, for example. We will be using it in MSTICPy notebooks
to publish more complex result sets from automated notebook runs.

Operations available include:

- Retrieve list of current dynamic Summaries
- Retrieve a full dynamic summary
- Create a dynamic summary
- Delete a dynamic summary
- Update an existing dynamic summary

There is also a MSTICPy Python class
:py:class:`DynamicSummary <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary>`
that lets you work with dynamic summary data in your notebook or Python application.

List Dynamic Summaries
----------------------

``list_dynamic_summaries`` returns a list of all the dynamic summaries saved in the workspace.
This includes the name and ID of the summaries, who
created it, when and with what details.

.. note:: There is current no way to filter the list of results returned

See :py:meth:`list_dynamic_summaries <msticpy.context.azure.sentinel_core.MicrosoftSentinel.list_dynamic_summaries>`

.. code:: python

    sentinel.list_dynamic_summaries()


=========================  ====================================  ======================================  ===========================================  ============================  ======================  ==========================  ============================  ===========================  ===============================  ========================  ==================================  ====================  =======================  ===============================  ====================================
id                         name                                  etag                                    type                                         systemData.createdAt          systemData.createdBy    systemData.createdByType    systemData.lastModifiedAt     systemData.lastModifiedBy    systemData.lastModifiedByType    properties.summaryName    properties.sourceInfo.TI Records    properties.tactics    properties.techniques    properties.summaryDescription    properties.tenantId
=========================  ====================================  ======================================  ===========================================  ============================  ======================  ==========================  ============================  ===========================  ===============================  ========================  ==================================  ====================  =======================  ===============================  ====================================
/subscriptions/40dcc8bf..  cea27320-829c-4654-bbf0-b14367483418  "00002e10-0000-0a00-0000-639a7fa00000"  Microsoft.SecurityInsights/dynamicsummaries  2022-12-15T01:59:59.1574875Z  ianhelle@microsoft.com  User                        2022-12-15T01:59:59.1574875Z  ianhelle@microsoft.com       User                             test2                     misc                                []                    []                       Test description                 72f988bf-86f1-41af-91ab-2d7cd011db47
/subscriptions/40dcc8bf..  5b574f4f-047c-4056-97aa-136b42b1bc5a  "0000c010-0000-0a00-0000-639a836b0000"  Microsoft.SecurityInsights/dynamicsummaries  2022-12-15T02:16:10.7127404Z  ianhelle@microsoft.com  User                        2022-12-15T02:16:10.7127404Z  ianhelle@microsoft.com       User                             test3                     misc                                []                    []                       Test description                 72f988bf-86f1-41af-91ab-2d7cd011db47
/subscriptions/40dcc8bf..  fba7525c-3e67-4b7f-9a78-36c7cf0f6423  "0000062d-0000-0a00-0000-639d0c900000"  Microsoft.SecurityInsights/dynamicsummaries  2022-12-15T02:29:27.5201639Z  ianhelle@microsoft.com  User                        2022-12-17T00:25:51.7860328Z  ianhelle@microsoft.com       User                             test4                     misc                                []                    []                       A new description                72f988bf-86f1-41af-91ab-2d7cd011db47
=========================  ====================================  ======================================  ===========================================  ============================  ======================  ==========================  ============================  ===========================  ===============================  ========================  ==================================  ====================  =======================  ===============================  ====================================

Create a Dynamic Summary
------------------------

To create a dynamic summary, you may find it easier to work with the MSTICPy
:py:class:`DynamicSummary <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary>`
class. You can read more about this in the `DynamicSummary Class`_ section below.

You can create a Dynamic Summary in MS Sentinel using the
:py:meth:`create_dynamic_summary<msticpy.context.azure.sentinel_core.MicrosoftSentinel.create_dynamic_summary>`.
This creates and uploads the summary object to the Sentinel workspace.

You can supply the properties of the dynamic summary as parameters to the function:

.. code:: python

    sentinel.connect()
    sentinel.create_dynamic_summary(
        name="My_XYZ_Summary",
        description="Summarizing the running of the XYZ job.",
        data=summary_df,
    )

You can supply additional properties of the summary by adding additional
keyword parameters to the ``create_dynamic_summary`` call. See
:py:class:`DynamicSummary <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary>`
for a list of available properties.

.. code:: python

    sentinel.connect()
    sentinel.create_dynamic_summary(
        name="My_XYZ_Summary",
        description="Summarizing the running of the XYZ job.",
        data=summary_df,
        tactics=["discovery", "exploitation"],
        techniques=["T1064", "T1286"],
        search_key="host.domain.dom",
    )


You can also create a ``DynamicSummary`` object and pass that as the
single parameter to ``create_dynamic_summary``.

.. code:: python

    dyn_summary = sentinel.new_dynamic_summary(
        summary_name="My new summary",
        summary_description="Description of summary",
        source_info={"TI Records": "misc"},
        summary_items=ti_summary_df,
    )
    sentinel.create_dynamic_summary(dyn_summary)

Get a Dynamic Summary
---------------------

You can retrieve a DynamicSummary using the
:py:meth:`get_dynamic_summary<msticpy.context.azure.sentinel_core.MicrosoftSentinel.get_dynamic_summary>`
method.

.. code:: python

    dyn_summary = sentinel.get_dynamic_summary(
        summary_id="cea27320-829c-4654-bbf0-b14367483418"
    )
    dyn_summary

.. parsed-literal::

    DynamicSummary(id=cea27320-829c-4654-bbf0-b14367483418, name=test2, items=0)

.. note:: The Sentinel API does not return any Summary Item records,
    only the metadata properties associated with the DynamicSummary record.
    Use the ``summary_items`` parameter described next.

Supplying a ``summary_items=True`` parameter will re-route the
request for Dynamic Summary data to the MS Sentinel ``DynamicSummary`` table.
It will execute a query to retrieve the summary items along with the
summary metadata.

.. code:: python

    dyn_summary = sentinel.get_dynamic_summary(
        summary_id="cea27320-829c-4654-bbf0-b14367483418",
        summary_items=True
    )
    dyn_summary

.. parsed-literal::

    Please wait. Loading Kqlmagic extension...done
    Connecting...
    popup schema 52b1ab41-869e-4138-9e40-2a4457f09bf0@loganalytics
    connected
    DynamicSummary(
        summary_id='cea27320-829c-4654-bbf0-b14367483418'
        summary_name='test2'
        summary_description='Test description'
        tenant_id='72f988bf-86f1-41af-91ab-2d7cd011db47'
        tactics='[]'
        techniques='[]'
        ws_tenant_id='52b1ab41-869e-4138-9e40-2a4457f09bf0'
        updated_time_utc='2022-12-15 01:59:59.157487500+00:00'
        source_info='{'TI Records': 'misc'}'
        created_by='user@microsoft.com'
        summary_data_type='Summary'
        time_generated='2022-12-15 02:00:03.152763400+00:00'
        created_time_utc='2022-12-15 01:59:59.157487500+00:00'
        summary_status='Active'
        updated_by='user@microsoft.com'
        summary_items=6
    )

.. note:: Because this triggers a QueryProvider to load it may involve
    some additional initial delay.

You can display the summary items as a DataFrame.

.. code:: python

    dyn_summary.to_df()

==========  =============================================  =========  ==============  ==========  ========  ==========  ============================================  ================================
index       Ioc                                            IocType    QuerySubtype    Provider    Result      Severity  Details                                       TimeGenerated
==========  =============================================  =========  ==============  ==========  ========  ==========  ============================================  ================================
OTX         hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        OTX         True               2  {'pulse_count': 3, 'names': ['Underminer EK'  2022-12-15 01:55:15.135136+00:00
VirusTotal  hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        VirusTotal  False              0  Request forbidden. Allowed query rate may ha  2022-12-15 01:55:15.135136+00:00
XForce      hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        XForce      False              0  Not found.                                    2022-12-15 01:55:15.135136+00:00
AzSTI       hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        AzSTI       False              0  Not found.                                    2022-12-15 01:55:15.135136+00:00
OPR         hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        OPR         False              0  IoC type url not supported.                   2022-12-15 01:55:15.135136+00:00
Tor         hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        Tor         True               0  IoC type url not supported.                   2022-12-15 01:55:15.135136+00:00
==========  =============================================  =========  ==============  ==========  ========  ==========  ============================================  ================================

Update a Dynamic Summary
------------------------

You can add additional summary items to an existing Dynamic Summary
using the
:py:meth:`update_dynamic_summary <msticpy.context.azure.sentinel_core.MicrosoftSentinel.update_dynamic_summary>`
method. You can also change existing properties of the DynamicSummary.

.. warning:: if the summary_id supplied does not exist a new DynamicSummary
    record will be created.

.. code:: python

    dyn_summary.summary_description = "A new description"
    dyn_summary.summary_id = "fba7525c-3e67-4b7f-9a78-36c7cf0f6423"
    sentinel.update_dynamic_summary(dyn_summary)

    ds_upd = sentinel.get_dynamic_summary(dyn_summary.summary_id)
    ds_upd.to_json()

.. parsed-literal::

    Dynamic summary created/updated.

    '{"summaryId": "122f7de3-7276-490b-9db0-11e9f07873d0", "summaryName": "test4",
    "summaryDescription": "A new description", "tenantId": "72f988bf-86f1-41af-91ab-2d7cd011db47",
    "tactics": [], "techniques": [], "rawContent": [], "sourceInfo": {"TI Records": "misc"}}'

.. note:: If you have summary items in the Dynamic Summary that you pass
    to the ``update_dynamic_summary`` method, the items will be appended
    to any existing items.

Delete a Dynamic Summary
------------------------

Dynamic Summaries can be deleted by calling
:py:meth:`delete_dynamic_summary <msticpy.context.azure.sentinel_core.MicrosoftSentinel.delete_dynamic_summary>`
and passing in the ``summary_id``.

.. note:: Since MS Sentinel/Log Analytics tables are append-only, the records
    will not be removed from the ``DynamicSummary`` table but the summary will
    be deactivated and marked as *Deleted*.

.. code:: python

    sentinel.delete_dynamic_summary(summary_id="cea27320-829c-4654-bbf0-b14367483418")

DynamicSummary Class
--------------------

API reference: :py:class:`DynamicSummary <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary>`

This is Python class to encapsulate a Sentinel Dynamic Summary object. It is
used only for local manipulation of the Summary object and does not
affect the version stored in Sentinel unless you upload the changes using
one of the APIs described earlier.

Using ``DynamicSummary`` you can:

- prepare a new summary object in your code or interactively before uploading to Sentinel
- extract summary items as a data frame.
- use it to amend/update an existing dynamic summary.
- view a summary of the Dynamic summary

The most important methods are described below.

DynamicSummary initializer
~~~~~~~~~~~~~~~~~~~~~~~~~~

:py:class:`DynamicSummary <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary>`

You can create a ``DynamicSummary`` object by supplying the required attributes as
parameters or create a "bare" class and add them as attributes.

.. code:: python

    import msticpy as mp
    dyn_summary = mp.MicrosoftSentinel.new_dynamic_summary(
        summary_name="My new summary",
        summary_description="Description of summary",
        source_info={"TI Records": "misc"},
    )

    dyn_summary.add_summary_items(data=ti_df)
    dyn_summary

.. parsed-literal::

    DynamicSummary(id=49f627af-1f05-42e6-9951-6a2bd7b9b233, name=My new summary, items=6)

You can also pass a DataFrame to ``new_summary`` as ``summary_items`` instead
of adding them with a separate call to
:py:meth:`add_summary_items <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.add_summary_items>`.

Adding and appending Summary items
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``add_summary_items`` method takes one of:

- A pandas DataFrame
- A list/iterable of
  :py:class:`DynamicSummaryItem <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummaryItem>`.
- A list/iterable of dictionaries, each of which contains the keys and values
  need for the summary item.

You can specify additional properties for the summary items by adding additional
parameters to ``add_summary_items``.
See :py:meth:`add_summary_items <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.add_summary_items>`
for a list of available properties.

.. code:: python

    dyn_summary.add_summary_items(
        data=summary_df,
        tactics=["discovery", "exploitation"],
        techniques=["T1064", "T1286"],
        observable_type="Account",
        observable_value="user@some.dom",
    )

If your source data is in a DataFrame you can also use the DataFrame
rows for some or all of the DynamicSummaryItem properties. Use the
``summary_fields`` parameter to specify which columns should be used
to populate the property value for that row.

.. code:: Python

    dyn_summary.add_summary_items(
        data=summary_df,
        tactics=["discovery", "exploitation"],
        techniques=["T1064", "T1286"],
        observable_type="Account",
        observable_value="user@some.dom",
        summary_fields={
             "user_name": "observable_value",
             "user_name", "search_key",
        }
    )

``add_summary_items`` will remove any existing summary items and
replace with the new set specified.

:py:meth:`append_summary_items <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.append_summary_items>`
works in the same as ``add_summary_items`` but will add to the current set without
erasing existing summary items. This is useful for updating an existing
summary with new rows.

.. code:: python

    dyn_summary = sentinel.get_dynamic_summary(summary_id="123123...")
    dyn_summary.append_summary_items(data=new_items_df)
    sentinel.update_dynamic_summary(dyn_summary)

Output SummaryItems as DataFrame
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can retrieve the summary items as a DataFrame using the
:py:meth:`to_df <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.to_df>`
method.

.. code:: python

    dyn_summary.to_df()


====  ==========  =============================================  =========  ==============  ==========
  ..  index       Ioc                                            IocType    QuerySubtype    Provider
====  ==========  =============================================  =========  ==============  ==========
   0  OTX         hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        OTX
   1  VirusTotal  hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        VirusTotal
   2  XForce      hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        XForce
   3  AzSTI       hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        AzSTI
   4  OPR         hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        OPR
   5  Tor         hXXp://38[.]75[.]37[.]1/static/encrypt.min.js  url                        Tor
====  ==========  =============================================  =========  ==============  ==========

Convert Dynamic Summary to/from JSON
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The instance method
:py:meth:`to_json <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.to_json>`
returns the summary object
serialized to a JSON string. The
:py:meth:`to_json_api <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.to_json_api>`
method also does this
but adds a wrapper layer that's expected by the Sentinel API.

The class method
:py:meth:`from_json <msticpy.context.azure.sentinel_dynamic_summary_types.DynamicSummary.from_json>`
will return a DynamicSummary instance from
the JSON data. The input to this can either be the simple format (as returned
by ``to_json()``) or the API wrapped format (``to_json_api()``)

View contents of the Dynamic Summary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can view a text representation of a DynamicSummary object
by running it in a cell or printing it with the Python ``print`` function

.. code:: python

    print(dyn_summary)

.. parsed-literal::

    DynamicSummary(
        summary_id='cea27320-829c-4654-bbf0-b14367483418'
        summary_name='test2'
        summary_description='Test description'
        tenant_id='72f988bf-86f1-41af-91ab-2d7cd011db47'
        tactics='[]'
        techniques='[]'
        ws_tenant_id='52b1ab41-869e-4138-9e40-2a4457f09bf0'
        updated_time_utc='2022-12-15 01:59:59.157487500+00:00'
        source_info='{'TI Records': 'misc'}'
        created_by='user@microsoft.com'
        summary_data_type='Summary'
        time_generated='2022-12-15 02:00:03.152763400+00:00'
        created_time_utc='2022-12-15 01:59:59.157487500+00:00'
        summary_status='Active'
        updated_by='user@microsoft.com'
        summary_items=6
    )

Using the ``fields`` attribute for legal field names
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When creating a DynamicSummary or DynamicSummaryItem you
frequently need to specify the field names as parameters.
To help prevent typos, both classes have a ``fields`` attribute
that contains the names of all legal fields.

You can list all of the fields for each class just by running
(in a notebook/IPython) or printing the ``fields`` attribute.

.. code:: python

    DynamicSummary.fields

.. parsed-literal::

    Fields:
        SUMMARY_ID='summary_id'
        SUMMARY_NAME='summary_name'
        SUMMARY_DESCRIPTION='summary_description'
        TENANT_ID='tenant_id'
        RELATION_NAME='relation_name'
        RELATION_ID='relation_id'
        SEARCH_KEY='search_key'
        TACTICS='tactics'
        TECHNIQUES='techniques'
        SOURCE_INFO='source_info'
        SUMMARY_ITEMS='summary_items'

This example shows how you might use the ``fields`` attribute in
code.

.. code:: python

    dyn_summary.add_summary_items(
        data=df,
        summary_fields = {
            DynamicSummaryItem.fields.EVENT_TIME_UTC: "TimeGenerated",
            DynamicSummaryItem.fields.SEARCH_KEY: "UserPrincipalName",
        }
    )
