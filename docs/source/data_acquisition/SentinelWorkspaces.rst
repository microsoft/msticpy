Microsoft Sentinel Workspaces
=============================

This is a class to help you search and resolve Microsoft Sentinel workspace details.

The methods of this class are class methods - so, unlike other Sentinel APIs,
you do not need to instantiate the class with a specific workspace.

Many of the methods of this class require authenticated access to the
Azure Resource Graph, so you should run ``az_connect()``
before trying to use them.

Getting Workspace details from portal URL
-----------------------------------------

You can copy a URL from the Sentinel portal and use
:py:meth:`get_workspace_details_from_url <msticpy.context.azure.MicrosoftSentinel.get_workspace_details_from_url>`
to retrieve full details of the workspace.

These are returned in a format that you can add to your Microsoft Sentinel
settings.

.. code:: python

    import msticpy
    from msticpy.context.azure import MicrosoftSentinel
    from msticpy.auth.azure_auth import az_connect

    az_connect()

    portal_url = "https://ms.portal.azure.com/#blade/Microsoft_Azure_Secu..."
    MicrosoftSentinel.get_workspace_details_from_url(portal_url)

.. parsed-literal::

    {'contoso55': {'WorkspaceId': '4cf452c0-b9ac-4b2f-a8b4-f83d13c07a5b',
        'TenantId': '72f988bf-86f1-41af-91ab-2d7cd011db47',
        'SubscriptionId': '3c1bb38c-82e3-4f8d-a115-a7110ba70d05',
        'ResourceGroup': 'contoso55-eus',
        'WorkspaceName': 'contoso55',
        'WorkspaceTenantId': '72f988bf-86f1-41af-91ab-2d7cd011db47'}}

Get workspace resource id from URL
----------------------------------

:py:meth:`get_resource_id_from_url <msticpy.context.azure.MicrosoftSentinel.get_resource_id_from_url>`

.. code:: python

    MicrosoftSentinel.get_resource_id_from_url(portal_url)

.. parsed-literal::

    '/subscriptions/3c1bb38c-82e3-4f8d-a115-a7110ba70d05/resourcegroups/contoso55-eus/providers/Microsoft.OperationalInsights/workspaces/contoso55'

Get workspace ID from a Workspace Name
--------------------------------------

:py:meth:`get_workspace_id <msticpy.context.azure.MicrosoftSentinel.get_workspace_id>`

.. code:: python

    MicrosoftSentinel.get_workspace_id(workspace_name="asihuntomsworkspacev4")

.. parsed-literal::

    '52b1ab41-869e-4138-9e40-2a4457f09bf0'

If you have access to multiple workspaces you may see the warning shown above.
Add a ``subscription_id`` or ``resource_group name`` to disambiguate this

.. parsed-literal::

    '52b1ab41-869e-4138-9e40-2a4457f09bf0'
    Warning: query returned multiple results. Specify subscription_id and/or resource_group for more accurate results.
    '4cf452c0-b9ac-4b2f-a8b4-f83d13c07a5b'

Get workspace name from a Workspace or Resource ID
--------------------------------------------------

:py:meth:`get_workspace_name <msticpy.context.azure.MicrosoftSentinel.get_workspace_name>`

.. code:: python

    MicrosoftSentinel.get_workspace_name('52b1ab41-869e-4138-9e40-2a4457f09bf0')

.. parsed-literal::

    'ASIHuntOMSWorkspaceV4'


Get Workspace settings from Workspace name or ID
------------------------------------------------

:py:meth:`get_workspace_settings <msticpy.context.azure.MicrosoftSentinel.get_workspace_settings>`

.. code:: python

    MicrosoftSentinel.get_workspace_settings('52b1ab41-869e-4138-9e40-2a4457f09bf0')

.. parsed-literal::

    {'ASIHuntOMSWorkspaceV4': {'WorkspaceId': '52b1ab41-869e-4138-9e40-2a4457f09bf0',
        'TenantId': '72f988bf-86f1-41af-91ab-2d7cd011db47',
        'SubscriptionId': '40dcc8bf-0478-4f3b-b275-ed0a94f2c013',
        'ResourceGroup': 'asihuntomsworkspacerg',
        'WorkspaceName': 'ASIHuntOMSWorkspaceV4',
        'WorkspaceTenantId': '72f988bf-86f1-41af-91ab-2d7cd011db47'}}


:py:meth:`get_workspace_settings_by_name <msticpy.context.azure.MicrosoftSentinel.get_workspace_settings_by_name>`

.. code:: python

    MicrosoftSentinel.get_workspace_settings_by_name('ASIHuntOMSWorkspaceV4')

.. parsed-literal::

    {'ASIHuntOMSWorkspaceV4': {'WorkspaceId': '52b1ab41-869e-4138-9e40-2a4457f09bf0',
        'TenantId': '72f988bf-86f1-41af-91ab-2d7cd011db47',
        'SubscriptionId': '40dcc8bf-0478-4f3b-b275-ed0a94f2c013',
        'ResourceGroup': 'asihuntomsworkspacerg',
        'WorkspaceName': 'ASIHuntOMSWorkspaceV4',
        'WorkspaceTenantId': '72f988bf-86f1-41af-91ab-2d7cd011db47'}}

Again, when search for workspace details by name, you may need to
provide the Subscription ID and or Resource Group name to disambiguate
multiple workspaces with the same name.

These functions are used in the MSTICPy configuration tools
:py:class:`MpConfigEdit <msticpy.config.mp_config_edit.MpConfigEdit>`
and
:py:class:`MpConfigFile <msticpy.config.mp_config_file.MpConfigFile>`
to help resolve workspace settings details for configuration.
