Entity Graph
============

This describes the use of the
:py:class:`DataViewer<msticpy.vis.entity_graph_tools.EntityGraph>`
control.

The purpose of this feature is to allow a user to create a graph of Incidents, Alerts, and other eneities during the course of an investigation.
A graph can be initially created using any of the above entity options, with additional entities, and links between these entities added as an investigation progresses.
As well as creating a graph object this feature allows for the plotting of the graph, allowing for interactive exploration of the entities and thier links.

.. note: this feature provides similar funcitonality to `msticpy.nbtools.security_alert_graph`, however it is expanded to include support for additional entity types and incidents.
    You can pass `EntityGraph` a SecurtyAlert in the same way you can with security_alert_graph and will produce a very similar graph.


Creating a Graph from an Incident or Alert
------------------------------------------
Incidents and Alerts often have a set of assocaited entities (and other alerts), graphing these relationships is useful function and a common way to start the creation of a graph.
`EntityGraph` can accept `Incident`, `Alert` and `SecurityAlert` entities and will extract each entity & assocaited alert in the entity, add them to the graph and create the connections between them.
This is done by instantiating an EntityGraph object and passing in the incident or alert entity:

.. code:: ipython3


Creating from a DataFrame
^^^^^^^^^^^^^^^^^^^^^^^^^

Plotting a Graph
^^^^^^^^^^^^^^^^



Adding & Removing Entities
--------------------------

Adding & Removing Links
-----------------------

Adding Notes
------------

Exporting Nodes to a DataFrame
------------------------------

Plotting with a Timeline
------------------------



