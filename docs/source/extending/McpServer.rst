MSTICPy MCP Server
==================

The MSTICPy MCP server exposes MSTICPy's :doc:`pivot functions <PivotFunctions>`
to AI agent hosts through the `Model Context Protocol
<https://modelcontextprotocol.io>`__ (MCP). Any MCP-capable host - such as
GitHub Copilot in VS Code, Claude Desktop, or a custom agent - can use it to
enrich and query security entities (IP addresses, hosts, accounts, files,
URLs, domains) with MSTICPy, without writing any host-specific integration
code.

The server exposes a compact, generic set of seven tools that work across all
entity types and all pivot functions, rather than one tool per pivot.


Installation
------------

Install MSTICPy with the ``mcp`` extra:

.. code:: bash

    pip install "msticpy[mcp]"

This adds the `MCP Python SDK <https://pypi.org/project/mcp/>`__ and installs
the ``msticpy-mcp`` console command.


Configuration
-------------

The server uses your standard ``msticpyconfig.yaml`` for TI provider keys,
GeoIP, VirusTotal and data-provider settings. Set the ``MSTICPYCONFIG``
environment variable to point at the file, or run the server from a directory
that contains it.

To load and connect query providers (Microsoft Sentinel, Kusto, Splunk, etc.)
at startup, add an ``MpMcpServer`` section to ``msticpyconfig.yaml``:

.. code:: yaml

    MpMcpServer:
      QueryProviders:
        sentinel:
          DataEnvironment: MSSentinel
          Connect: true
          ConnectArgs:
            workspace: MyWorkspace
            auth_methods: ['msi', 'env', 'cli']
      ResultStore:
        max_results: 50
        max_rows_per_result: 100000
      Defaults:
        timespan_days: 7
        sample_rows: 25

``QueryProviders``
    A mapping of provider name to configuration. ``DataEnvironment`` is the
    MSTICPy environment name; ``InitArgs`` and ``ConnectArgs`` are passed to the
    provider constructor and ``connect()`` respectively.

``ResultStore``
    Bounds for the in-memory result store: ``max_results`` (LRU eviction) and
    ``max_rows_per_result``.

``Defaults``
    ``timespan_days`` sets the initial query look-back window; ``sample_rows``
    is the default number of inline sample rows returned by ``run_pivot``.

.. warning::

    **Non-interactive authentication only.** The server must not trigger an
    interactive browser or device-code prompt while running. Configure query
    providers to use non-interactive auth methods - managed identity (``msi``),
    environment/service-principal credentials (``env``), or pre-populated Azure
    CLI cached tokens (``cli``). If you use ``cli``, run ``az login`` **before**
    starting the server.

    If a provider cannot connect non-interactively at startup it is marked as
    disconnected (visible in ``server_status``) and its query pivots return a
    clear error when called, rather than blocking the server.


Running the server
-------------------

Run with the default ``stdio`` transport (used by local hosts such as VS Code
and Claude Desktop):

.. code:: bash

    msticpy-mcp

Or run it as a module:

.. code:: bash

    python -m msticpy.mcp

Options:

* ``--transport {stdio,sse,streamable-http}`` - transport to use
  (default ``stdio``). Use ``streamable-http`` for remote hosting.
* ``--log-level LEVEL`` - logging level (default ``WARNING``).


Configuring an MCP host
-----------------------

Add the server to your MCP host configuration. For example, for a host that
uses the common ``mcpServers`` JSON format:

.. code:: json

    {
      "mcpServers": {
        "msticpy": {
          "command": "msticpy-mcp",
          "env": {
            "MSTICPYCONFIG": "C:/path/to/msticpyconfig.yaml"
          }
        }
      }
    }


Tools
-----

The server exposes seven tools. A typical agent flow is
``list_entities`` -> ``list_pivots`` -> ``describe_pivot`` -> ``run_pivot`` ->
``get_result``.

``list_entities()``
    List entity types that have pivots, with per-entity pivot counts.

``list_pivots(entity=None, search=None, kind="all")``
    List available pivots, filterable by entity (name or alias), a free-text
    ``search`` string, and ``kind`` (``query``, ``enrichment`` or ``all``).

``describe_pivot(entity, pivot_path)``
    Full detail for one pivot: description, kind, parameters (name, type,
    description, required, default), the primary input parameter, and - for
    query pivots - the backing provider and whether it uses the session
    timespan.

``run_pivot(entity, pivot_path, value, params=None, max_rows=25)``
    Execute a pivot. ``value`` is the entity value(s) to look up or query;
    ``params`` supplies any additional keyword arguments (see
    ``describe_pivot``). Returns a summary (row count, column schema and a
    capped ``max_rows`` sample) plus a ``result_id``. The full result is stored
    for retrieval with ``get_result``.

``get_result(result_id, offset=0, limit=100, columns=None, format="records")``
    Page through or reshape a stored result. ``format`` is ``records`` (list of
    dicts) or ``markdown`` (a table string).

``set_timespan(start=None, end=None, days=None)``
    Set the query timespan used by query pivots. Provide ISO 8601 ``start``/
    ``end``, or a relative ``days`` look-back window.

``server_status()``
    Diagnostics: loaded config path, current timespan, connected query
    providers and their state, loaded TI providers, pivot/entity counts and
    result-store usage. Use this to check that authentication succeeded.

Each tool returns a JSON-serializable object. Errors are returned as
``{"ok": false, "error": "...", "detail": "..."}`` so the agent can recover
rather than the call failing.


Example interaction
-------------------

An agent asked "who owns the IP 8.8.8.8?" might:

1. Call ``list_pivots`` with ``entity="ip"`` and ``search="whois"`` to find the
   ``whois`` pivot.
2. Call ``run_pivot`` with ``entity="ip"``, ``pivot_path="whois"`` and
   ``value="8.8.8.8"``.
3. Read the inline sample from the result (and, if needed, page through the
   full result with ``get_result``).


Security considerations
-----------------------

* **No arbitrary code execution.** ``run_pivot`` can only dispatch to pivot
  functions present in the catalog (discovered from the registered entities);
  it never evaluates arbitrary attributes or code.
* **Read-only.** Only lookup and query pivots are exposed - no mutation
  operations.
* **Secret-safe.** Tool output never includes configuration secrets;
  ``server_status`` reports provider names and connection state only.
* **Bounded memory.** The result store is capped by result count and row count
  with LRU eviction.
* **External cost.** Some TI and VirusTotal lookups call external APIs that may
  incur cost or rate limits.
