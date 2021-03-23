# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from copy import deepcopy
from datetime import datetime
from pathlib import Path

import pytest
import pytest_check as check
import yaml
from msticpy.config.comp_edit import (
    CEItemsBase,
    CESimpleBase,
    CompEditItems,
    CompEditStatusMixin,
)
from msticpy.config.mp_config_edit import MpConfigEdit
from msticpy.config.mp_config_file import MpConfigFile

from ..unit_test_lib import TEST_DATA_PATH, custom_mp_config

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

        test_file = f"./temp_config{datetime.now().strftime('%H%M%S')}.yaml"
        mp_edit.txt_current_file.value = test_file
        mp_edit.btn_save.click()
        check.is_true(Path(test_file).is_file())
        Path(test_file).unlink()


def test_mp_edit_load_params():
    """Test different startup params for MpConfigEdit."""
    config_path = Path(TEST_DATA_PATH).joinpath("msticpyconfig.yaml")
    with open(config_path, "r") as conf_fh:
        settings = yaml.safe_load(conf_fh)

    orig_settings = deepcopy(settings)
    orig_resgroup = orig_settings["AzureSentinel"]["Workspaces"]["Default"].get(
        "ResourceGroup"
    )

    # edit the settings so we know that we're checking the same data
    # - not the default
    settings["AzureSentinel"]["Workspaces"]["Default"]["ResourceGroup"] = "TestMarker"

    test_path = "AzureSentinel.Workspaces.Default.ResourceGroup"

    # pass MpConfigFile instance
    mpc_file = MpConfigFile(settings=settings)
    mp_conf = MpConfigEdit(settings=mpc_file)
    check.equal(mp_conf.mp_controls.mp_config, settings, "MpConfigFile")
    check.equal(mp_conf.mp_controls.get_value(test_path), "TestMarker", "File path")

    # pass settings dict
    mp_conf = MpConfigEdit(settings=mpc_file.settings)
    check.equal(mp_conf.mp_controls.mp_config, settings, "Settings dict")
    check.equal(mp_conf.mp_controls.get_value(test_path), "TestMarker", "File path")

    # In these last tests we can't check for dict equality since MpConfigEdit
    # adds blank values for top level keys

    # pass file_path
    mp_conf = MpConfigEdit(settings=str(config_path))
    check.is_false(
        orig_settings.keys() - mp_conf.mp_controls.mp_config.keys(), "File path"
    )
    check.equal(mp_conf.mp_controls.get_value(test_path), orig_resgroup, "Default")
    for key in orig_settings.keys():
        check.equal(orig_settings[key], mp_conf.mp_controls.mp_config[key])

    with custom_mp_config(str(config_path)):
        mp_conf = MpConfigEdit()
        check.equal(mp_conf.mp_controls.get_value(test_path), orig_resgroup, "Default")
        check.is_false(
            orig_settings.keys() - mp_conf.mp_controls.mp_config.keys(), "Default"
        )
        for key in orig_settings.keys():
            check.equal(orig_settings[key], mp_conf.mp_controls.mp_config[key])
