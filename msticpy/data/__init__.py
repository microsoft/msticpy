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

# from ..common.exceptions import MsticpyImportExtraError
from ..lazy_importer import lazy_import

__version__ = VERSION

_LAZY_IMPORTS = {
    "msticpy.data.core.data_providers.QueryProvider",
    "msticpy.data.core.query_defns.DataEnvironment",
    "msticpy.data.core.query_defns.DataFamily",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
