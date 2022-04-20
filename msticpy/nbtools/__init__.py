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
from .. import nbwidgets
from .._version import VERSION
from ..common import utility as utils
from ..common.wsconfig import WorkspaceConfig
from . import nbdisplay

try:
    from IPython import get_ipython

    from . import nbmagics
except ImportError as err:
    pass

# pylint: enable=W0401


__version__ = VERSION
