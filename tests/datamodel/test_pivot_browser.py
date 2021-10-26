# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot pipeline browser UI."""
import warnings

import pytest
import pytest_check as check

try:
    import pyperclip

    _ENABLE_CLIP = True
except ImportError:
    _ENABLE_CLIP = False

from msticpy.datamodel.pivot import Pivot
from msticpy.datamodel.pivot_browser import PivotBrowser

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


@pytest.fixture(scope="session")
def _create_pivot():
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", category=UserWarning)
        return Pivot()


def test_pivot_browser(_create_pivot):
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

    browser._text["search_txt"].value = "Vir"
    check.is_in("ti.lookup_file_hash_VirusTotal", browser._html["search_res"].value)

    browser._btn["copy"].click()

    if _ENABLE_CLIP:
        try:
            cb_content = pyperclip.paste()
            check.equal("entities.Dns.util.dns_components()", cb_content)
        except pyperclip.PyperclipException:
            print("Pyperclip not operational on this OS.")
