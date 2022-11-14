Microsoft Sentinel Analytics
============================

List Analytics
--------------

To return a dataframe detailing all configured alert/analytics rules configured with Microsoft Sentinel
call `list_alert_rules`. The returned dataframe includes scheduled queries, as well as Fusion based detections.
The returned dataframe include details of the rule configuration as well as the query run (where applicable).

See :py:meth:`list_alert_rules <msticpy.context.azure.sentinel_core.MicrosoftSentinel.list_alert_rules>`

.. code:: ipython3

    sentinel.list_alert_rules()

You can also return details of avaliable Analytic templates with `list_analytic_templates`.

See :py:meth:`list_analytic_templates <msticpy.context.azure.sentinel_core.MicrosoftSentinel.list_analytic_templates>`

.. code:: ipython3

    sentinel.list_analytic_templates()

Create Analytics
----------------

To create a new analytic rule you can use `create_analytic_rule`. With this function you can create
a new analytic rule either from an analytic template or with a set of custom details.

See :py:meth:`create_analytic_rule <msticpy.context.azure.sentinel_core.MicrosoftSentinel.create_analytic_rule>`

When creating an analytic if a template name or ID is provided details from that template will be used.

.. code:: ipython3

    sentinel.create_analytic_rule(template="f817f062-320c-4c18-891c-7c5cc64da6ee")

Otherwise you can specify details manually. Request elements include:
-name: The name to give the analytic.
-query: The KQL query string to use in the anlaytic.
-query_frequency: How often the query should run in ISO8601 format, by default "PT5H" (or 5 hours)
-query_period: How far back the query should look in ISO8601 format, by default "PT5H" (or 5 hours)
-severity: The severity to raise incidents as, by default "Medium" but options include "Informational", "Low", "Medium", "High"
-suppression_duration: How long to suppress duplicate alerts in ISO8601 format, by default "PT1H" (or 1 hour)
-suppression_enabled: Whether you want to suppress duplicates, by default False
-trigger_operator: The operator for the trigger, by default "GreaterThan". Options include "Equal", "NotEqual", "LessThan", "GreaterThan"
-trigger_threshold: The threshold of events required to create the incident, by default 0
-description: A description of the analytic, by default None
-tactics: A list of MITRE ATT&CK tactics related to the analytic, by default None
-enabled: Whether you want the analytic to be enabled once deployed, by default True

.. code:: ipython3

    sentinel.create_analytic_rule(name="Custom Analytic", query="SecurityAlert | take 10")

Delete Analytics
----------------

You can also delete analytic rules by passing the name or ID of the analytic rules to `delete_analytic_rule`.

See :py:meth:`delete_analytic_rule <msticpy.context.azure.sentinel_core.MicrosoftSentinel.delete_analytic_rule>`

.. code:: ipython3

    sentinel.delete_analytic_rule(analytic_rule="a91d4cfa-1854-431b-a432-8742f9aa0d59")

