# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Text to DataFrame IPython magic."""
from IPython.core.magic import needs_local_scope, register_cell_magic
from .pivot_magic_core import run_txt2df

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@register_cell_magic
@needs_local_scope
def txt2df(line, cell, local_ns):
    """Convert cell text to pandas DataFrame."""
    return run_txt2df(line, cell, local_ns)
