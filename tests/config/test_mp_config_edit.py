# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from pathlib import Path
from datetime import datetime
import pytest
import pytest_check as check

from msticpy.config.mp_config_edit import MpConfigEdit
from msticpy.config.comp_edit import (
    CESimpleBase,
    CEItemsBase,
    CompEditItems,
    CompEditStatusMixin,
)


__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def mp_edit():
    """Get instantiated editor."""
    CompEditStatusMixin.testing = True
    return MpConfigEdit()


def test_mp_edit_load(mp_edit):
    """Creating instance of MpConfigEdit."""
    for idx, (title, tab) in enumerate(mp_edit.controls.items()):

        check.equal(mp_edit.tab_ctrl.tab.get_title(idx), title)

        check.is_instance(tab, (CEItemsBase, CESimpleBase))

        check.is_true(hasattr(tab, "layout"))
        check.is_true(hasattr(tab, "help"))
        check.is_true(hasattr(tab, "status"))
        check.is_true(hasattr(tab, "edit_frame"))

        if isinstance(tab, CompEditItems):
            check.is_true(hasattr(tab, "items_frame"))
            check.is_true(hasattr(tab, "edit_buttons"))
            check.is_true(hasattr(tab, "select_item"))
        else:
            check.is_true(hasattr(tab, "btn_save"))

        check.is_true(tab.help.html_help.value)
        check.is_in(
            "https://msticpy.readthedocs.io/en/latest", tab.help.html_help.value
        )

        mp_edit.btn_validate.click()
        mp_edit.btn_reload.click()

        test_file = f"./temp_config{datetime.now().strftime('%H%M%S')}.yaml"
        mp_edit.txt_current_file.value = test_file
        mp_edit.btn_save.click()
        check.is_true(Path(test_file).is_file())
        Path(test_file).unlink()
