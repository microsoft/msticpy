Microsoft Sentinel Incidents
============================

List Incidents
--------------

It is possible to return a list incidents within a workspace, as well as get the details of a specific incident.
Whilst it is possible to access these incident details via the Incident table in the Workspace, you can also interact
with them via the Microsoft Sentinel APIs which are utilized in these functions.

See :py:meth:`list_incidents <msticpy.context.azure_sentinel.MicrosoftSentinel.list_incidents>`

.. code:: ipython3

    sentinel.list_incidents()

This returns a DataFrame with details of incidents. By default this will return the 50 latest incidents.
It is possible to pass a set of parameters to `.list_incidents` to adjust the incidents returned via the `params` parameter.
These parameters follow the format of the
`Microsoft Sentinel API <https://learn.microsoft.com/rest/api/securityinsights/stable/incidents/list>`__
and include the following key items:

 - $top: this controls how many incidents are returned
 - $filter: this accepts an OData query that filters the returned item. https://learn.microsoft.com/graph/filter-query-parameter
 - $orderby: this allows for sorting results by a specific column

.. code:: ipython3

    # Set parameters to return 500 incidents where the Tile includes 'MSTICPy' and the incidents occurred since a set time
    params = {"$top" : 500, "$filter": "contains(properties/title, 'MSTICPy') and properties/createdTimeUtc gt 2023-03-21T12:00:00Z"}}
    sentinel.list_incidents(params)

To get details of a single incident you can call `.get_incident` and pass the ID of an incident.
This ID can be found in the name column of the DataFrame returned by `.get_incidents` and appears in the form of a GUID.
You can also provide an incident name and it will attempt to get an ID from this. If there are no matches, or multiple
matches an error will be raised.

See :py:meth:`get_incident <msticpy.context.azure.sentinel_core.MicrosoftSentinel.get_incident>`

.. code:: ipython3

    sentinel.get_incident(incident = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c")

When calling `get_incident` there are a number of boolean flags you can set to return additional information
related to the incident.
- entities: Returns Entities related to the incident.
- alerts: Returns details of the Alerts related to the incident.
- comments: Returns the Comments related to the incident.
- bookmarks: Returns details of the Bookmarks related to the incident.

Update Incidents
----------------

Via the Microsoft Sentinel API it is possible to update incidents, this includes updating details such as Severity and Status,
as well as adding comments to an incident.

To interact with an incident use `.post_comment` or `.update_incident`.

To update the incident's features you need to pass `.update_incident` a dictionary of parameters and values to update.
Details of what parameters can be updated can be found in the `Microsoft Sentinel documentation.
<https://docs.microsoft.com/rest/api/securityinsights/stable/incidents/create-or-update>`_

.. note:: When modifying severity, status, or title there is no need to include the 'properties.' in the key name within the update_items dictionary

See :py:meth:`update_incident <msticpy.context.azure_sentinel.MicrosoftSentinel.update_incident>`

.. code:: ipython3

    sentinel.update_incident(incident = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c",
                update_items = {"severity":"High"},
                )

Posting comments to an incident uses the `.post_comment` function. Simply pass this function a comment as a string,
along with an incident and workspace ID. If successful  a "Comment posted." message will be displayed.

See :py:meth:`post_comment <msticpy.context.azure_sentinel.MicrosoftSentinel.post_comment>`

.. code:: ipython3

    sentinel.post_comment(incident = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c",
                comment = "This is my comment",
                )

You can also attach bookmarks to an incident. To do this you need to pass in the incident ID or name, and a
bookmark ID or name. This will then create a link between the incident and the bookmark.

.. code:: ipython3

    sentinel.add_bookmark_to_incident(incident = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c",
                bookmark = "f91f3c99-2651-47fc-b625-141d4a7e50ff",
                )


Create Incidents
----------------

As well as interacting with existing incidents you can create them from scratch with `create_incident`.
With this function you need to specify a number of elements about the incident including:
- title: The name to give the incident
- severity: The severity of the incident. This can be "Informational", "Low", "Medium" or "High"

Optionally you can also provide the following details:
- status: The status to give the incident. Options are "New", "Active" or "Closed"
- description: A description to give the incident.
- first_activity_time: A datetime object of the first event related to the incident.
- last_activity_time: A datetime object of the last event related to the incident.
- labels: A list of labels to attach to the incident
- bookmarks: A list of bookmark IDs to associate with the incident

See :py:meth:`create_incident <msticpy.context.azure.sentinel_core.MicrosoftSentinel.create_incident>`

.. code:: ipython3

    sentinel.create_incident(title="A custom incident", severity="High"

)
