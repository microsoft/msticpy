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
from ..lazy_importer import lazy_import

_LAZY_IMPORTS = {
    "msticpy.config.mp_config_control.MpConfigControls",
    "msticpy.config.mp_config_edit.MpConfigEdit",
    "msticpy.config.mp_config_file.MpConfigFile",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
