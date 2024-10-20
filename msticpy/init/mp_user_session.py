# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICPy User Session loader.

This loads user-specific configuration settings from a YAML file.

Example YAML file:

.. code-block:: yaml

    QueryProviders:
        qry_prov_sent:
            DataEnvironment: MSSentinel
            InitArgs:
                debug: True
            Connect: True
            ConnectArgs:
                workspace: MySoc
                auth_methods: ['cli', 'device_code']
        qry_prov_md:
            DataEnvironment: M365D
        qry_kusto_mde:
            DataEnvironment: Kusto
            Connect: True
            ConnectArgs:
                cluster: MDEData
        qry_kusto_mstic:
            DataEnvironment: Kusto
            Connect: True
            ConnectArgs:
                cluster: MSTIC
    Components:
        mssentinel:
            Module: msticpy.context.azure
            Class: MicrosoftSentinel
            InitArgs:
            Connect: True
            ConnectArgs:
                workspace: CyberSecuritySoc
                auth_methods: ['cli', 'device_code']

Example usage:

.. code-block:: python

    import msticpy as mp
    mp.init_notebook()
    mp.mp_user_session.load_user_config()

"""

from __future__ import annotations

import importlib
import sys
from pathlib import Path
from typing import Any

import yaml
from IPython import get_ipython

from .._version import VERSION
from ..data.core.data_providers import QueryProvider

__version__ = VERSION
__author__ = "Ian Hellen"


def load_user_session(
    session_file: str | Path | None = None, namespace: dict[str, Any] | None = None
):
    """
    Load user session configuration.

    Parameters
    ----------
    session_file : str, Path, optional
        Path to the user configuration file, by default None.
        If no file is provided, the function will look in the current directory
        for an `mp_user_session.yaml` file.
    namespace : dict[str, Any], optional
        Namespace to load the configuration into, by default None.
        If no namespace is provided, the configuration will be loaded into the
        global namespace of the caller (usually the notebook).

    """
    if session_file is None:
        session_file = Path.cwd() / "mp_user_session.yaml"
    session_file = Path(session_file)
    if not session_file.exists():
        print(f"Session file {session_file} not found.")
        return

    if namespace is None:
        ipython = get_ipython()
        if ipython is None:
            # pylint: disable=protected-access
            namespace = sys._getframe(1).f_locals
        else:
            namespace = ipython.user_ns

    user_config = yaml.safe_load(session_file.read_text(encoding="utf-8"))
    _load_query_providers(user_config, namespace)
    _load_mp_components(user_config, namespace)


def _load_query_providers(user_config, namespace):
    """Process the query provider settings."""
    for qry_prov_name, qry_prov_settings in user_config.get(
        "QueryProviders", {}
    ).items():
        qry_prov = _initialize_component(
            qry_prov_name, qry_prov_settings, QueryProvider
        )
        if qry_prov:
            namespace[qry_prov_name] = qry_prov


def _load_mp_components(user_config, namespace):
    """Initialize non-query components."""
    for comp_name, comp_settings in user_config.get("Components", {}).items():
        module = importlib.import_module(comp_settings.get("Module"), package="msticpy")
        comp_class = getattr(module, comp_settings.get("Class"))
        comp_instance = _initialize_component(comp_name, comp_settings, comp_class)
        if comp_instance:
            namespace[comp_name] = comp_instance


def _initialize_component(name, settings, cls):
    """
    Initialize a component or query provider.

    Parameters
    ----------
    name : str
        The name of the component or query provider.
    settings : dict
        The settings for the component or query provider.
    cls : type
        The class of the component or query provider.

    Returns
    -------
    object
        The initialized component or query provider instance, or None if initialization failed.

    """
    try:
        init_args = settings.get("InitArgs", {})
        if init_args is None:
            init_args = {}
        if data_env := settings.get("DataEnvironment"):
            init_args["data_environment"] = data_env
        instance = cls(**init_args)
    # pylint: disable=broad-except
    except Exception as err:
        print(f"Failed to create {name}: {err}")
        return None

    if settings.get("Connect"):
        print(f"Connecting to {name}")
        try:
            connect_args = settings.get("ConnectArgs", {})
            if connect_args is None:
                connect_args = {}
            instance.connect(**connect_args)
        # pylint: disable=broad-except
        except Exception as err:
            print(f"Failed to connect to {name}: {err}")
            return None
    else:
        print(f"Not connecting to {name}")

    return instance
