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

from .mp_config_control import MpConfigControls

# flake8: noqa: F403
from .mp_config_edit import MpConfigEdit
from .mp_config_file import MpConfigFile
