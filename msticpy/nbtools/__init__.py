# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
nbtools module - Notebook Security Tools.

This is a collection of modules with functionality (mostly) specific to
notebooks. It also houses some visualization modules that will migrate
to the vis sub-package.

- nbinit - notebook initialization
- azure_ml_tools - configuration and helpers for AML workspaces
- nbwidgets - ipywidgets-based UI components for infosec notebooks
- nbdisplay - miscellaneous display functions TBM to vis

"""
# flake8: noqa: F403
# pylint: disable=W0401
# import importlib
# from typing import Any

# from .. import nbwidgets
from .._version import VERSION
from ..lazy_importer import lazy_import

# from ..common import utility as utils
# from ..common.wsconfig import WorkspaceConfig
# from ..vis import nbdisplay
# from .security_alert import SecurityAlert

# try:
#     from IPython import get_ipython

#     from ..init import nbmagics
# except ImportError as err:
#     pass

# pylint: enable=W0401

__version__ = VERSION

# _DEFAULT_IMPORTS = {"nbinit": "msticpy.init.nbinit"}

_LAZY_IMPORTS = {
    "msticpy.init.nbinit",
    "msticpy.common.utility as utils",
    "msticpy.common.wsconfig.WorkspaceConfig",
    "msticpy.nbtools.security_alert.SecurityAlert",
    "msticpy.nbwidgets",
    "msticpy.vis.nbdisplay",
}

# def __getattr__(attrib: str) -> Any:
#     """
#     Import and return an attribute of nbtools.

#     Parameters
#     ----------
#     attrib : str
#         The attribute name

#     Returns
#     -------
#     Any
#         The attribute value.

#     Raises
#     ------
#     AttributeError
#         No attribute found.

#     """
#     if attrib in _DEFAULT_IMPORTS:
#         module = importlib.import_module(_DEFAULT_IMPORTS[attrib])
#         return module
#     raise AttributeError(f"msticpy has no attribute {attrib}")

# from .vtlookupv3 import VT3_AVAILABLE

# vtlookupv3: Any
# if VT3_AVAILABLE:
#     from .vtlookupv3 import vtlookupv3
# else:
#     # vtlookup3 will not load if vt package not installed
#     vtlookupv3 = ImportPlaceholder(  # type: ignore
#         "vtlookupv3", ["vt-py", "vt-graph-api", "nest_asyncio"]
#     )

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
