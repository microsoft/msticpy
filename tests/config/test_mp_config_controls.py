# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
from pathlib import Path

import pytest
import pytest_check as check
import yaml
from msticpy.config.comp_edit import SettingsControl
from msticpy.config.mp_config_control import (
    MpConfigControls,
    get_defn_or_default,
    get_mpconfig_definitions,
    get_or_create_mpc_section,
)

from ..unit_test_lib import TEST_DATA_PATH
from ..nbtools.test_user_config import CONFIG_TEXT

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture(scope="module")
def mp_conf_ctrl():
    """Create MPConfigControl instance."""
    mp_defn = get_mpconfig_definitions()
    config_path = Path(TEST_DATA_PATH).joinpath("msticpyconfig.yaml")
    conf_settings = {}
    with open(config_path, "r") as conf_h:
        conf_settings = yaml.safe_load(conf_h)

    # modify the LocalData to be AzureSentinel
    user_defaults = yaml.safe_load(CONFIG_TEXT)

    nb_settings = user_defaults["UserDefaults"]["LoadComponents"]["Notebooklets"]
    q_prov = nb_settings["query_provider"]["LocalData"]
    del nb_settings["query_provider"]["LocalData"]
    nb_settings["query_provider"]["AzureSentinel"] = q_prov
    conf_settings.update(user_defaults)

    return MpConfigControls(mp_config_def=mp_defn, mp_config=conf_settings)


def test_mp_config_controls_load(mp_conf_ctrl: MpConfigControls):
    """Loading MpConfigControls."""
    check.is_not_none(mp_conf_ctrl.mp_config)
    check.is_not_none(mp_conf_ctrl.config_defn)
    check.is_not_none(mp_conf_ctrl.controls)

    results = mp_conf_ctrl.validate_all_settings()
    # We have one bad setting due to our test mpconfig
    check.equal(len(results), 1)
    check.equal(
        results[0][1], "Validation failed for path 'TIProviders.AzureSentinel.Provider'"
    )

    get_or_create_mpc_section(mp_conf_ctrl, "TestSettings", "TestSubkey")
    val = mp_conf_ctrl.get_value("TestSettings")
    check.is_not_none(val)
    check.is_in("TestSubkey", val)


NEW_WS = {
    "WorkspaceId": "a927809c-8142-43e1-96b3-4ad87cfe95a3",
    "TenantId": "69d28fd7-42a5-48bc-a619-af56397b9f28",
}


def test_mp_config_controls_values(mp_conf_ctrl: MpConfigControls):
    """Loading MpConfigControls."""
    val = mp_conf_ctrl.get_value("AzureSentinel.Workspaces")
    check.is_instance(val, dict)
    check.equal(len(val), 2)

    mp_conf_ctrl.set_value("AzureSentinel.Workspaces.TestWS", NEW_WS)
    check.equal(len(mp_conf_ctrl.get_value("AzureSentinel.Workspaces")), 3)

    mp_conf_ctrl.rename_path(
        "AzureSentinel.Workspaces.TestWS", "AzureSentinel.Workspaces.TestWS_ren"
    )
    val = mp_conf_ctrl.get_value("AzureSentinel.Workspaces.TestWS_ren")
    check.is_instance(val, dict)
    check.equal(len(val), 2)

    mp_conf_ctrl.del_value("AzureSentinel.Workspaces.TestWS_ren")


class TestCtrl(SettingsControl):
    """Test control."""

    __test__ = False

    def __init__(self, value):
        """Initialize control."""
        self._val = value

    @property
    def value(self):
        """Return value."""
        return self._val

    @value.setter
    def value(self, value):
        """Set value."""
        self._val = value


TEST_PATH = "AzureSentinel.Workspaces.TestWS"
NEW_WS = {
    "WorkspaceId": "a927809c-8142-43e1-96b3-4ad87cfe95a3",
    "TenantId": "69d28fd7-42a5-48bc-a619-af56397b9f28",
}


def test_mp_config_controls_ctrls(mp_conf_ctrl: MpConfigControls):
    """Loading MpConfigControls."""
    val = mp_conf_ctrl.get_value("AzureSentinel.Workspaces")
    check.is_instance(val, dict)
    check.equal(len(val), 2)

    mp_conf_ctrl.set_value(TEST_PATH, NEW_WS)

    ctrl = TestCtrl(value=NEW_WS)

    mp_conf_ctrl.set_control(TEST_PATH, ctrl)
    ctrl2 = mp_conf_ctrl.get_control(TEST_PATH)
    check.equal(ctrl, ctrl2)

    # Change the value of the dict and set the control value to this
    NEW_WS["TenantId"] = "TestGUID---1"
    ctrl.value = NEW_WS
    ctrl2 = mp_conf_ctrl.get_control(TEST_PATH)
    check.equal(ctrl2.value, NEW_WS)
    # Test saving control value
    mp_conf_ctrl.save_ctrl_values(TEST_PATH)
    check.equal(mp_conf_ctrl.get_value(TEST_PATH), NEW_WS)

    # Change dict and set the settings value
    NEW_WS["TenantId"] = "TestGUID---2"
    mp_conf_ctrl.set_value(TEST_PATH, NEW_WS)

    mp_conf_ctrl.populate_ctrl_values(TEST_PATH)
    ctrl2 = mp_conf_ctrl.get_control(TEST_PATH)
    check.equal(ctrl2.value, NEW_WS)


TEST_PATH = "TIProviders.XForce"


def test_mp_config_controls_defn(mp_conf_ctrl):
    """Test misc functions."""
    defn = mp_conf_ctrl.get_defn(TEST_PATH)
    for name, item_def in defn.items():
        if name == "Args":
            check.is_instance(item_def, dict)
            for s_name, s_item_def in item_def.items():
                print(s_name, s_item_def)
                check.is_in(s_name, ("ApiID", "AuthKey"))
                i_type, i_opts = get_defn_or_default(s_item_def)
                check.equal(i_type, "cred_key")
                ck_defn = i_opts.get("defn")
                check.is_in("one_of", ck_defn)
                for one_of in ck_defn["one_of"]:
                    oo_name, oo_defn = next(iter(one_of.items()))
                    check.is_in(oo_name, ("str", "EnvironmentVar", "KeyVault"))
                    check.is_true(oo_defn.startswith("str"))

        if name == "Provider":
            check.is_instance(item_def, str)
        if name == "Primary":
            i_type, i_opts = get_defn_or_default(item_def)
            check.equal(i_type, "bool")
            check.is_true(i_opts["default"])
