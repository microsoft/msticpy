# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Data sub-package.

This sub-package contains data-access related modules and sub-packages.

It is split into the following sub-packages:

- azure - Azure and MS Sentinel API related modules
- browsers - notebook interactive data browsers for various components
  such as Threat Intel, Data Queries
- context - context provider modules such as TI providers, geoip providers
- core - core data query provider components
- drivers - query provider interface drivers for external data sources.
- queries - queries for core data providers
- uploaders - loaders for some data services.

"""
from .._version import VERSION
from ..common.exceptions import MsticpyImportExtraError

# flake8: noqa: F403
from .core.data_providers import QueryProvider
from .core.query_defns import DataEnvironment, DataFamily

__version__ = VERSION
