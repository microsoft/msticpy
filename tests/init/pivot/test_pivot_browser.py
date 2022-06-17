# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot pipeline browser UI."""
import pytest_check as check

try:
    import pyperclip

    _ENABLE_CLIP = True
except ImportError:
    _ENABLE_CLIP = False

# pylint: disable=redefined-outer-name, protected-access, unused-import
# pylint: disable=unused-argument
from msticpy.init.pivot import Pivot
from msticpy.init.pivot_core.pivot_browser import PivotBrowser

from .pivot_fixtures import create_data_providers, create_pivot, data_providers

__author__ = "Ian Hellen"


def test_pivot_browser(create_pivot):
    """Test pivot browser."""
    browser = PivotBrowser()

    check.is_in("File", browser._select["entities"].options)
    check.is_in("Dns", browser._select["entities"].options)

    browser._select["entities"].value = "Dns"
    check.is_in("util.dns_components", browser._select["pivot_funcs"].options)

    browser._select["pivot_funcs"].value = "util.dns_components"
    check.is_in(
        "Return&nbsp;components&nbsp;of&nbsp;domain", browser._html["func_help"].value
    )

    browser._text["search_txt"].value = "dns"
    check.is_in("tilookup_dns", browser._html["search_res"].value)

    browser._btn["copy"].click()

    if _ENABLE_CLIP:
        try:
            cb_content = pyperclip.paste()
            check.equal("Dns.util.dns_components()", cb_content)
        except pyperclip.PyperclipException:
            print("Pyperclip not operational on this OS.")
