# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICPY Config modules.

This sub-package contains settings UI management tools
for interactively managing settings in Jupyter notebooks.

It use the ipywidgets package.

"""
from ..common.utility.package import init_dir, init_getattr

_STATIC_ATTRIBS = list(locals().keys())
_DEFAULT_IMPORTS = {
    "MpConfigControls": "msticpy.config.mp_config_control",
    "MpConfigEdit": "msticpy.config.mp_config_edit",
    "MpConfigFile": "msticpy.config.mp_config_file",
}


def __getattr__(attrib: str):
    """Import and a dynamic attribute of module."""
    return init_getattr(__name__, _DEFAULT_IMPORTS, attrib)


def __dir__():
    """Return attribute list."""
    return init_dir(_STATIC_ATTRIBS, _DEFAULT_IMPORTS)
