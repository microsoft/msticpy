Microsoft Sentinel Threat Intelligence
======================================

MSTICPy integrates with Microsoft Sentinel's Threat Intelligence features, allowing users to list, create
and update indicators in the Threat Intelligence Feature.
https://learn.microsoft.com/azure/sentinel/work-with-threat-indicators

Get Indicators
--------------

It is possible to return details of all indicators within a workspace.
Whilst it is possible to access these indicator details via the indicator table in the Workspace, you can also interact
with them via the Microsoft Sentinel APIs which are utilized in these functions.

See :py:meth:`get_all_indicators <msticpy.context.azure_sentinel_core.MicrosoftSentinel.get_all_indicators>`

.. code:: ipython3

    sentinel = MicrosoftSentinel()
    sentinel.get_all_indicators()

This returns a DataFrame with details of all indicators. It is possible to limit the number of indicators
returned with the `limit` keyword, and sort results by a column with the `orderby` keyword.
Whilst it is possible to return all indicators you may get a timeout error returning more than 100,000 at once.

To get details of a single indicator you can call `.get_indicator` and pass the ID of an indicator.
This ID can be found in the name column of the DataFrame returned by `.get_all_indicators` and appears in the form of a GUID.

See :py:meth:`get_indicator <msticpy.context.azure.sentinel_core.MicrosoftSentinel.get_indicator>`

.. code:: ipython3

    sentinel.get_indicator(indicator_id = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c")


If you want to return a subset of indicators you can use `.query_indicators` and pass in a number of
query parameters to get the required indicators. The supported query parameters are:

- includeDisabled: Parameter to include/exclude disabled indicators.
- keywords: Keyword for searching threat intelligence indicators - use this to search for specific indicator values.
- maxConfidence: Maximum confidence.
- maxValidUntil: End time for ValidUntil filter.
- minConfidence: Minimum confidence.
- minValidUntil: Start time for ValidUntil filter.
- patternTypes: A list of IoC types to include.
- sortBy: Columns to sort by and sorting order as `[{"itemKey": COLUMN_NAME, "sortOrder": ascending/descending}]`
- sources: A list of indicator sources to include
- threatTypes: A list of Threat types to include

.. note:: You can also query indicator values via KQL with the ThreatIntelligenceIndicator table.

See :py:meth:`query_indicators <msticpy.context.azure.sentinel_core.MicrosoftSentinel.query_indicators>`

.. code:: ipython3

    sentinel.query_indicators(keywords = "ACTINIUM", minConfidence=50, patternTypes=["ipv4-addr", "ipv6-addr"])

Update Indicator
----------------

Via the Microsoft Sentinel API it is possible to update indicators, this includes updating details such as Confidence,
as well as adding tags to an indicator.

To interact with an indicator use `.update_indicator` or `.add_tag`.

To update the indicator's features you need to pass `.update_indicator` a set of keyword parameters with
the properties you want to update. Supported keywords are:

- name: A common name to give to the indicator default is 'TI Indicator'
- confidence: A score between 0-100 of the confidence in the indicator
- description: An description of the indicator
- labels: -A list of string object labels to associate with the indicator
- kill_chain_phases: A list of string objects relating to the kill chain phases
- threat_types: A list of threat types associated with the indicator (list of string objects)
- external_references: A list of URLs that provide an external reference for the indicator
- valid_from: A datetime from which the indicator is valid from, defaults to now
- valid_to: A datetime to which the indicator is valid until

See :py:meth:`update_indicator <msticpy.context.azure_sentinel.MicrosoftSentinel.update_indicator>`

.. code:: ipython3

    sentinel.update_indicator(indicator_d = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c",
                confidence = 75, threat_types=["Turla"]
                )

If you just want to add a new tag to an indicator you can use the `.add_tag` function. This appends the
new tag to any existing tags.

See :py:meth:`add_tag <msticpy.context.azure_sentinel.MicrosoftSentinel.post_comment>`

.. code:: ipython3

    sentinel.add_tag(indicator_id = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c",
                tag = "ACTINIUM",
                )


Create indicators
-----------------

As well as interacting with existing indicators you can create them from scratch with `create_indicator`.
With this function you need to specify a number of elements about the indicator including:

- indicator: Tne indicator object to add
- ioc_type: The type of indicator being added, this can be:

    - domain-name
    - url
    - file
    - ipv4-addr
    - ipv6_addr

Optionally you can also provide the following details:

- confidence: A score between 0-100 of the confidence in the indicator, default is 0
- description: An description of the indicator
- labels: A list of string object labels to associate with the indicator
- kill_chain_phases: A list of string objects relating to the kill chain phase
- threat_types: A list of threat types associated with the indicator (list of string objects)
- external_references: A list of URLs that provide an external reference for the indicator
- valid_from: A datetime from which the indicator is valid from, defaults to now
- valid_to: A datetime to which the indicator is valid until

See :py:meth:`create_indicator <msticpy.context.azure.sentinel_core.MicrosoftSentinel.create_indicator>`

.. code:: ipython3

    sentinel.create_indicator(indicator="1.1.1.1", ioc_type="ipv4-addr")

If you are looking to create a large number of indicators at once you can use `.bulk_create_indicators`
Pass in a dataframe and specify which columns contain the indictor, the ioc type, and optionally the confidence.
It is only possible to bulk create indicators with these items - if you need to add additional items
you will need to use `.create_indicator`.

See :py:meth:`bulk_create_indicators <msticpy.context.azure.sentinel_core.MicrosoftSentinel.bulk_create_indicators>`

.. code:: ipython3

    sentinel.bulk_create_indicators(data=ioc_df, indicator_column="iocs", indicator_type_column="type")

Delete Indicator
----------------

It is possible to delete indicators with `.delete_indicator`. All that is needed is the ID of the
indicator to delete. This can be found in the name column of data returned by `.get_all_indicators`.

See :py:meth:`delete_indicator <msticpy.context.azure.sentinel_core.MicrosoftSentinel.delete_indicator>`

.. code:: ipython3

    sentinel.delete_indicator(indicator_d = "875409ee-9e1e-40f6-b0b8-a38aa64a1d1c")
