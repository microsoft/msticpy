# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Widgets sub-package."""

from .._version import VERSION
from ..lazy_importer import lazy_import

__version__ = VERSION
__author__ = "Ian Hellen"

_LAZY_IMPORTS = {
    "msticpy.nbwidgets.core.IPyDisplayMixin",
    "msticpy.nbwidgets.core.RegisteredWidget",
    "msticpy.nbwidgets.get_environment_key.GetEnvironmentKey",
    "msticpy.nbwidgets.get_text.GetText",
    "msticpy.nbwidgets.lookback.Lookback",
    "msticpy.nbwidgets.option_buttons.OptionButtons",
    "msticpy.nbwidgets.progress.Progress",
    "msticpy.nbwidgets.query_time.QueryTime",
    "msticpy.nbwidgets.select_alert.SelectAlert",
    "msticpy.nbwidgets.select_item.SelectItem",
    "msticpy.nbwidgets.select_subset.SelectSubset",
}

module, __getattr__, __dir__ = lazy_import(__name__, _LAZY_IMPORTS)
