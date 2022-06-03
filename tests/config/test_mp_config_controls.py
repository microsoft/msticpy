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

from msticpy.config.ce_common import get_defn_or_default, get_or_create_mpc_section
from msticpy.config.comp_edit import SettingsControl
from msticpy.config.mp_config_control import MpConfigControls, get_mpconfig_definitions

from ..init.test_user_config import CONFIG_TEXT
from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture
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


TEST_PATH_WS = "AzureSentinel.Workspaces.TestWS"
NEW_WS = {
    "WorkspaceId": "a927809c-8142-43e1-96b3-4ad87cfe95a3",
    "TenantId": "69d28fd7-42a5-48bc-a619-af56397b9f28",
}


def test_mp_config_controls_ctrls(mp_conf_ctrl: MpConfigControls):
    """Loading MpConfigControls."""
    val = mp_conf_ctrl.get_value("AzureSentinel.Workspaces")
    check.is_instance(val, dict)
    check.equal(len(val), 2)

    mp_conf_ctrl.set_value(TEST_PATH_WS, NEW_WS)

    ctrl = TestCtrl(value=NEW_WS)

    mp_conf_ctrl.set_control(TEST_PATH_WS, ctrl)
    ctrl2 = mp_conf_ctrl.get_control(TEST_PATH_WS)
    check.equal(ctrl, ctrl2)

    # Change the value of the dict and set the control value to this
    NEW_WS["TenantId"] = "TestGUID---1"
    ctrl.value = NEW_WS
    ctrl2 = mp_conf_ctrl.get_control(TEST_PATH_WS)
    check.equal(ctrl2.value, NEW_WS)
    # Test saving control value
    mp_conf_ctrl.save_ctrl_values(TEST_PATH_WS)
    check.equal(mp_conf_ctrl.get_value(TEST_PATH_WS), NEW_WS)

    # Change dict and set the settings value
    NEW_WS["TenantId"] = "TestGUID---2"
    mp_conf_ctrl.set_value(TEST_PATH_WS, NEW_WS)

    mp_conf_ctrl.populate_ctrl_values(TEST_PATH_WS)
    ctrl2 = mp_conf_ctrl.get_control(TEST_PATH_WS)
    check.equal(ctrl2.value, NEW_WS)


TEST_PATH_XF = "TIProviders.XForce"


def test_mp_config_controls_defn(mp_conf_ctrl):
    """Test misc functions."""
    defn = mp_conf_ctrl.get_defn(TEST_PATH_XF)
    for name, item_def in defn.items():
        if name == "Args":
            check.is_instance(item_def, dict)
            for s_name, s_item_def in item_def.items():
                print(s_name, s_item_def)
                check.is_in(s_name, ("ApiID", "AuthKey", "UseVT3PrivateAPI"))
                if s_name == "UseVT3PrivateAPI":
                    continue
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
            check.is_in(i_opts["default"], (True, False))


TEST_PATH_WS3 = "AzureSentinel.Workspaces.TestWS2"
NEW_WS2 = {
    "WorkspaceId": "a927809c-8142-43e1-96b3-4ad87cfe95a3",
    "TenantId": "69d28fd7-42a5-48bc-a619-af56397b9f28",
}
_VALIDATION_CASES = [
    pytest.param((TEST_PATH_WS3, NEW_WS2), 0, id="Workspace OK"),
    pytest.param(
        (TEST_PATH_WS3, {"WorkspaceId": NEW_WS2["WorkspaceId"]}),
        1,
        id="WS missing tenantID",
    ),
    pytest.param(
        (TEST_PATH_WS3, {"WorkspaceId": "not_a_uuid"}), 2, id="WS WorkspaceID not UUID"
    ),
    pytest.param(("TIProviders.OTX.Args.AuthKey", "KeyValue"), 0, id="OTX OK"),
    pytest.param(
        ("TIProviders.OTX.Args.AuthKey", {"InvalidKey": "KeyValue"}),
        1,
        id="OTX invalid credkey",
    ),
    pytest.param(
        ("DataProviders.AzureCLI.Args.auth_methods", ["cli"]),
        0,
        id="AzCLI OK - meth list",
    ),
    pytest.param(
        ("DataProviders.AzureCLI.Args.auth_methods", None), 0, id="AzCLI OK - no meths"
    ),
    pytest.param(
        ("DataProviders.AzureCLI.Args.auth_methods", ["cli", "badmeth"]),
        1,
        id="AzCLI bad enum",
    ),
    pytest.param(("DataProviders.Splunk.Args.sharing", None), 0, id="Splunk OK"),
    pytest.param(
        ("DataProviders.Splunk.Args.sharing", "global"), 0, id="Splunk OK def value"
    ),
    pytest.param(
        ("DataProviders.Splunk.Args.sharing", "badval"), 1, id="Splunk bad enum"
    ),
    pytest.param(
        ("DataProviders.LocalData.data_paths", None), 0, id="LocalData OK - None"
    ),
    pytest.param(
        ("DataProviders.LocalData.data_paths", ["/path1", "e:\\path\\path2"]),
        0,
        id="LocalData OK - List",
    ),
    pytest.param(
        ("DataProviders.LocalData.data_paths", "badval"), 1, id="LocalData str"
    ),
    pytest.param(
        ("DataProviders.Mordor.save_folder", None), 0, id="Mordor save_folder OK - None"
    ),
    pytest.param(
        ("DataProviders.Mordor.save_folder", "e:\\path\\path2"),
        0,
        id="Mordor save_folder OK - path",
    ),
    pytest.param(
        ("DataProviders.Mordor.save_folder", True), 1, id="Mordor save_folder - bool"
    ),
    pytest.param(
        ("DataProviders.LocalData.data_paths", "badval"), 1, id="LocalData str"
    ),
    pytest.param(
        (
            "UserDefaults.LoadComponents.Notebooklets.query_provider.AzureSentinel",
            {"workspace": "Default"},
        ),
        0,
        id="Load Notebooklets OK",
    ),
    pytest.param(
        ("UserDefaults.LoadComponents.Notebooklets.query_provider.AzureSentinel", None),
        1,
        id="Load Notebooklets None",
    ),
    pytest.param(
        (
            "UserDefaults.LoadComponents.Notebooklets.query_provider.AzureSentinel",
            "not_a_kv_pair",
        ),
        1,
        id="Load Notebooklets bad kv pair",
    ),
]


@pytest.mark.parametrize(["test_case", "expected"], _VALIDATION_CASES)
def test_mp_config_controls_validation(mp_conf_ctrl, test_case, expected):
    """Test validation functions."""
    path, test_value = test_case
    curr_val = mp_conf_ctrl.get_value(path)
    mp_conf_ctrl.set_value(path, test_value)
    if path == TEST_PATH_WS3:
        wksp_stem = path.rsplit(".", maxsplit=1)[0]
        results = mp_conf_ctrl.validate_setting(path, f"{wksp_stem}.Default")
    else:
        results = mp_conf_ctrl.validate_setting(path)

    check.equal(len(results), expected)
    print(results)
    mp_conf_ctrl.set_value(path, curr_val)
