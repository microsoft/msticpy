# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Deprecated - module process_tree.py has moved.

See :py:mod:`msticpy.vis.process_tree`
"""
import warnings

from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"

# flake8: noqa: F403, F401
# pylint: disable=wildcard-import, unused-wildcard-import, unused-import
from ..transform.process_tree_utils import get_ancestors  # noqa F401
from ..transform.process_tree_utils import (
    get_children,
    get_descendents,
    get_parent,
    get_process,
    get_process_key,
    get_root,
    get_root_tree,
    get_roots,
    get_siblings,
    get_summary_info,
    get_tree_depth,
)
from ..vis.process_tree import *
from ..vis.process_tree import build_process_tree, infer_schema

WARN_MSSG = (
    "This module has moved to msticpy.vis.process_tree\n"
    "Please change your import to reflect this new location."
    "This will be removed in MSTICPy v2.2.0"
)
warnings.warn(WARN_MSSG, category=DeprecationWarning)
