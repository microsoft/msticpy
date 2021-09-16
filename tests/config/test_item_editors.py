# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Config settings Items editors."""
import os
from pathlib import Path
from unittest.mock import patch

import pytest
import pytest_check as check
import yaml
from msticpy.config.comp_edit import CompEditStatusMixin
from msticpy.config.ce_azure import CEAzure
from msticpy.config.ce_azure_sentinel import CEAzureSentinel, _validate_ws
from msticpy.config.ce_common import get_def_tenant_id
from msticpy.config.ce_data_providers import CEDataProviders
from msticpy.config.ce_keyvault import CEKeyVault
from msticpy.config.ce_other_providers import CEOtherProviders
from msticpy.config.ce_ti_providers import CETIProviders
from msticpy.config.ce_user_defaults import CEAutoLoadComps, CEAutoLoadQProvs
from msticpy.config.compound_ctrls import ArgControl
from msticpy.config.mp_config_control import MpConfigControls, get_mpconfig_definitions

from ..nbtools.test_user_config import CONFIG_TEXT
from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name


@pytest.fixture
def mp_conf_ctrl():
    """Create MPConfigControl instance."""
    CompEditStatusMixin.testing = True
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


_EDITORS = [
    pytest.param((CEAzureSentinel, ["Placeholder"]), id="CEAzureSentinel"),
    pytest.param(
        (CEDataProviders, ["Splunk", "AzureCLI", "LocalData", "Mordor"]),
        id="CEDataProviders",
    ),
    pytest.param(
        (CETIProviders, ["OTX", "VirusTotal", "AzureSentinel", "TorExitNodes"]),
        id="CETIProviders",
    ),
    pytest.param((CEOtherProviders, ["GeoIPLite", "IPStack"]), id="CEOtherProviders"),
    pytest.param(
        (
            CEAutoLoadComps,
            [
                "TILookup",
                "GeoIpLookup",
                "Notebooklets",
                "Pivot",
                "AzureData",
                "AzureSentinelAPI",
            ],
        ),
        id="CEAutoLoadComps",
    ),
    pytest.param(
        (
            CEAutoLoadQProvs,
            [
                "AzureSentinel.Default",
                "Mordor",
                "LocalData",
                "AzureSecurityCenter",
                "Splunk",
                "MDE",
                "SecurityGraph",
            ],
        ),
        id="CEAutoLoadQProvs",
    ),
]


def _get_select_labels(control):
    return (opt[0] if isinstance(opt, tuple) else opt for opt in control.options)


def _is_current_option(option, control):
    return option in _get_select_labels(control)


# pylint: disable=protected-access
@pytest.mark.parametrize("editor_test", _EDITORS)
def test_item_editor_controls(editor_test, mp_conf_ctrl):
    """Items edit controls."""
    editor, cases = editor_test
    edit_comp = editor(mp_controls=mp_conf_ctrl)
    _execute_item_editor_test(edit_comp, cases)


def _execute_item_editor_test(edit_comp, cases):
    check.is_not_none(edit_comp.help.html_help.value)
    check.is_not_none(edit_comp._DESCRIPTION)
    check.is_not_none(edit_comp._COMP_PATH)
    check.greater_equal(len(edit_comp._HELP_URI), 1)

    for test_opt in cases:
        print(f"Testing {edit_comp.__class__.__name__}, {test_opt}")
        opts = edit_comp.select_item.options
        n_opts = len(opts)

        # If this control has an options list - select the first of these
        prov_opts = getattr(edit_comp, "prov_options", None)
        if prov_opts and prov_opts.options:
            edit_comp.prov_options.value = test_opt

        # If there is an existing item, delete this
        if _is_current_option(test_opt, edit_comp.select_item):
            edit_comp.select_item.label = test_opt
            edit_comp.edit_buttons.btn_del.click()
            n_opts -= 1

        # Add a new one
        edit_comp.edit_buttons.btn_add.click()
        # Save the current item
        edit_comp.edit_buttons.btn_save.click()
        check.equal(len(edit_comp.select_item.options), n_opts + 1, "Item added")

        if isinstance(edit_comp, CEAzureSentinel):
            if _is_current_option("Default", edit_comp.select_item):
                edit_comp.select_item.label = "Default"
                edit_comp.edit_buttons.btn_del.click()
                n_opts -= 1

            edit_comp.btn_set_default.click()
            edit_comp.edit_buttons.btn_save.click()
            n_opts += 1
            check.equal(
                len(edit_comp.select_item.options), n_opts + 1, "AzSent default added"
            )

        if prov_opts and prov_opts.options:
            edit_comp.prov_options.value = test_opt
            edit_comp.edit_buttons.btn_add.click()
            # check that we didn't add a duplicate
            check.equal(
                len(edit_comp.select_item.options), n_opts + 1, "Dup item not added"
            )

        # delete whatever we've just added
        edit_comp.edit_buttons.btn_del.click()
        check.equal(len(edit_comp.select_item.options), n_opts, "New item deleted")


@pytest.mark.parametrize("editor_test", _EDITORS)
def test_item_editor_controls_empty(editor_test, mp_conf_ctrl):
    """Items edit controls."""
    conf_ctrl = MpConfigControls(mp_config_def=mp_conf_ctrl.config_defn, mp_config={})
    editor, cases = editor_test
    edit_comp = editor(mp_controls=conf_ctrl)
    if isinstance(edit_comp, CEAutoLoadQProvs) and "AzureSentinel.Default" in cases:
        cases.remove("AzureSentinel.Default")
    _execute_item_editor_test(edit_comp, cases)


def test_item_editor_load_comp_notebooklets(mp_conf_ctrl):
    """Items edit controls."""
    edit_comp = CEAutoLoadComps(mp_controls=mp_conf_ctrl)

    prev_val = mp_conf_ctrl.get_value("UserDefaults.LoadComponents.Notebooklets")
    edit_comp.select_item.label = "Notebooklets"
    edit_comp.edit_buttons.btn_save.click()
    new_val = mp_conf_ctrl.get_value("UserDefaults.LoadComponents.Notebooklets")
    check.equal(prev_val, new_val)


STORE_TEXT = "Text"
STORE_ENV_VAR = "EnvironmentVar"
STORE_KEYVAULT = "KeyVault"


def test_arg_controls(mp_conf_ctrl):
    """Argcontrol is a sub-component for editing Args."""
    edit_comp = CETIProviders(mp_controls=mp_conf_ctrl)
    edit_comp.select_item.label = "VirusTotal"
    provider = edit_comp.select_item.label
    # get the control for this provider
    ctrl_path = f"TIProviders.{provider}.Args.AuthKey"
    arg_ctrl = mp_conf_ctrl.get_control(ctrl_path)
    store_type = arg_ctrl.rb_store_type.value
    check.is_in(store_type, (STORE_TEXT, STORE_KEYVAULT, STORE_ENV_VAR))

    arg_ctrl.rb_store_type.value = STORE_TEXT
    arg_ctrl.txt_val.value = "test_value"
    edit_comp.edit_buttons.btn_save.click()
    saved_val = mp_conf_ctrl.get_value(ctrl_path)
    check.equal(saved_val, "test_value")
    check.equal(arg_ctrl.cb_kv_def.layout.visibility, "hidden")
    check.equal(arg_ctrl.btn_add_kv_secret.layout.visibility, "visible")

    arg_ctrl.rb_store_type.value = STORE_ENV_VAR
    edit_comp.edit_buttons.btn_save.click()
    saved_val = mp_conf_ctrl.get_value(ctrl_path)
    check.equal(saved_val, {STORE_ENV_VAR: "test_value"})
    check.equal(arg_ctrl.cb_kv_def.layout.visibility, "hidden")
    check.equal(arg_ctrl.btn_add_kv_secret.layout.visibility, "visible")

    arg_ctrl.rb_store_type.value = STORE_KEYVAULT
    arg_ctrl.cb_kv_def.value = True
    edit_comp.edit_buttons.btn_save.click()
    saved_val = mp_conf_ctrl.get_value(ctrl_path)
    check.equal(saved_val, {STORE_KEYVAULT: None})
    check.equal(arg_ctrl.cb_kv_def.layout.visibility, "visible")
    check.equal(arg_ctrl.btn_add_kv_secret.layout.visibility, "hidden")
    check.is_true(arg_ctrl.txt_val.disabled)
    check.equal(arg_ctrl.txt_val.value, "")

    arg_ctrl.cb_kv_def.value = False
    check.is_false(arg_ctrl.txt_val.disabled)


KV_SEC_CLIENT_PATCH = ArgControl.__module__ + ".BHKeyVaultClient"


@patch(KV_SEC_CLIENT_PATCH)
def test_tiproviders_editor(kv_sec, mp_conf_ctrl):
    """TI Providers item editor."""
    edit_comp = CETIProviders(mp_controls=mp_conf_ctrl)
    edit_comp.select_item.label = "VirusTotal"
    provider = edit_comp.select_item.label
    # get the control for this provider
    ctrl_path = f"TIProviders.{provider}.Args.AuthKey"
    arg_ctrl = mp_conf_ctrl.get_control(ctrl_path)

    arg_ctrl.rb_store_type.value = STORE_ENV_VAR
    arg_ctrl.txt_val.value = "test_var"
    os.environ["test_var"] = "test_value"

    arg_ctrl.btn_add_kv_secret.click()
    check.is_true(arg_ctrl.txt_val.disabled)
    check.equal(arg_ctrl.txt_val.value, "")
    set_secret, ss_args, _ = kv_sec.mock_calls[1]
    check.equal(set_secret, "().set_secret")
    check.equal(ss_args[0], "TIProviders-VirusTotal-Args-AuthKey")
    check.equal(ss_args[1], "test_value")
    check.equal(arg_ctrl.rb_store_type.value, STORE_KEYVAULT)

    arg_ctrl.rb_store_type.value = STORE_TEXT
    arg_ctrl.txt_val.value = "test_value2"
    arg_ctrl.btn_add_kv_secret.click()
    check.is_true(arg_ctrl.txt_val.disabled)
    check.equal(arg_ctrl.txt_val.value, "")
    check.equal(arg_ctrl.rb_store_type.value, STORE_KEYVAULT)
    set_secret, ss_args, _ = kv_sec.mock_calls[3]
    check.equal(set_secret, "().set_secret")
    check.equal(ss_args[0], "TIProviders-VirusTotal-Args-AuthKey")
    check.equal(ss_args[1], "test_value2")

    kv_call_count = len(kv_sec.mock_calls)
    arg_ctrl.rb_store_type.value = STORE_TEXT
    arg_ctrl.txt_val.value = ""
    arg_ctrl.btn_add_kv_secret.click()
    # verify we didn't call KV with blank value
    check.equal(len(kv_sec.mock_calls), kv_call_count)


def test_get_tenant_id():
    """Test get tenantID function."""
    tenantid = get_def_tenant_id("40dcc8bf-0478-4f3b-b275-ed0a94f2c013")
    check.equal(tenantid.casefold(), "72f988bf-86f1-41af-91ab-2d7cd011db47".casefold())


def test_azure_sentinel_editor(mp_conf_ctrl):
    """Azure Sentinel edit controls."""
    edit_comp = CEAzureSentinel(mp_controls=mp_conf_ctrl)

    n_opts = len(edit_comp.select_item.options)
    edit_comp.edit_buttons.btn_add.click()
    check.equal(n_opts + 1, len(edit_comp.select_item.options))
    new_ws = edit_comp.current_workspace
    result, _ = _validate_ws(new_ws, mp_conf_ctrl, edit_comp._COMP_PATH)
    check.is_false(result)

    edit_comp.edit_ctrls.children[1].value = "40dcc8bf-0478-4f3b-b275-ed0a94f2c013"
    edit_comp.edit_ctrls.children[2].value = "40dcc8bf-0478-4f3b-b275-ed0a94f2c013"
    edit_comp.edit_buttons.btn_save.click()
    result, _ = _validate_ws(new_ws, mp_conf_ctrl, edit_comp._COMP_PATH)
    check.is_true(result)

    # Save the current item
    edit_comp.edit_buttons.btn_save.click()
    check.is_not_none(mp_conf_ctrl.get_value(f"{edit_comp._COMP_PATH}.{new_ws}"))

    # Rename
    edit_comp.edit_ctrls.children[0].value = "TestWS"
    edit_comp.edit_buttons.btn_save.click()
    ren_workspace_settings = mp_conf_ctrl.get_value(f"{edit_comp._COMP_PATH}.TestWS")
    check.is_not_none(ren_workspace_settings)

    edit_comp.btn_set_default.click()
    def_ws = mp_conf_ctrl.get_value(f"{edit_comp._COMP_PATH}.Default")
    check.equal(def_ws, ren_workspace_settings)


def test_key_vault_editor(mp_conf_ctrl):
    """KeyVault edit controls."""
    edit_comp = CEKeyVault(mp_controls=mp_conf_ctrl)

    check.is_not_none(edit_comp.help.html_help.value)
    check.is_not_none(edit_comp._DESCRIPTION)
    check.is_not_none(edit_comp._COMP_PATH)
    check.greater_equal(len(edit_comp._HELP_URI), 1)

    edit_comp.controls["TenantId"].value = "tenant"  # invalid UUID
    edit_comp.controls["SubscriptionId"].value = "sub"  # invalid UUID
    edit_comp.controls["ResourceGroup"].value = ""  # OK to have empty
    edit_comp.controls["AzureRegion"].value = ""  # OK to have empty
    edit_comp.controls["VaultName"].value = ""  # invalid to have empty
    edit_comp.btn_save.click()

    results = mp_conf_ctrl.validate_setting(f"{edit_comp._COMP_PATH}")
    check.equal(len(results), 3)

    edit_comp.controls["TenantId"].value = "40dcc8bf-0478-4f3b-b275-ed0a94f2c013"
    edit_comp.controls["SubscriptionId"].value = "40dcc8bf-0478-4f3b-b275-ed0a94f2c013"
    edit_comp.controls["ResourceGroup"].value = "resgroup"
    edit_comp.controls["AzureRegion"].value = "Europe"
    edit_comp.controls["VaultName"].value = "MyVault"
    edit_comp.controls["Authority"].value = "global"
    edit_comp.btn_save.click()

    results = mp_conf_ctrl.validate_setting(f"{edit_comp._COMP_PATH}")
    check.equal(len(results), 0)


def test_azure_editor(mp_conf_ctrl):
    """Azure settings editor."""
    edit_comp = CEAzure(mp_controls=mp_conf_ctrl)

    check.is_not_none(edit_comp.help.html_help.value)
    check.is_not_none(edit_comp._DESCRIPTION)
    check.is_not_none(edit_comp._COMP_PATH)
    check.greater_equal(len(edit_comp._HELP_URI), 1)

    with pytest.raises(Exception) as err:
        edit_comp.controls["cloud"].value = ["no-cloud"]  # invalid item
    check.equal(err.typename, "TraitError")
    with pytest.raises(Exception) as err:
        edit_comp.controls["auth_methods"].value = ["invalid"]  # invalid item
    check.equal(err.typename, "TraitError")
    edit_comp.btn_save.click()

    results = mp_conf_ctrl.validate_setting(f"{edit_comp._COMP_PATH}")
    check.equal(len(results), 0)

    edit_comp.controls["cloud"].value = "usgov"
    edit_comp.controls["auth_methods"].value = ["cli", "interactive"]
    edit_comp.btn_save.click()

    results = mp_conf_ctrl.validate_setting(f"{edit_comp._COMP_PATH}")
    check.equal(len(results), 0)

    new_settings = edit_comp.mp_controls.get_value("Azure")
    check.equal(new_settings["cloud"], "usgov")
    check.equal(new_settings["auth_methods"], ["cli", "interactive"])


@patch(KV_SEC_CLIENT_PATCH)
def test_otherproviders_editor(kv_sec, mp_conf_ctrl):
    """Other providers item edit."""
    edit_comp = CEOtherProviders(mp_controls=mp_conf_ctrl)
    edit_comp.select_item.label = "GeoIPLite"
    provider = edit_comp.select_item.label
    # get the control for this provider
    ctrl_path = f"OtherProviders.{provider}.Args.AuthKey"
    arg_ctrl = mp_conf_ctrl.get_control(ctrl_path)

    arg_ctrl.rb_store_type.value = STORE_ENV_VAR
    arg_ctrl.txt_val.value = "test_var"
    os.environ["test_var"] = "test_value"

    edit_comp.edit_buttons.btn_save.click()
    args_settings = edit_comp.settings["GeoIPLite"]["Args"]["AuthKey"]
    check.is_in("EnvironmentVar", args_settings)
    check.equal("test_var", args_settings["EnvironmentVar"])

    arg_ctrl.btn_add_kv_secret.click()
    check.is_true(arg_ctrl.txt_val.disabled)
    check.equal(arg_ctrl.txt_val.value, "")
    set_secret, ss_args, _ = kv_sec.mock_calls[1]
    check.equal(set_secret, "().set_secret")
    check.equal(ss_args[0], f"OtherProviders-{provider}-Args-AuthKey")
    check.equal(ss_args[1], "test_value")
    check.equal(arg_ctrl.rb_store_type.value, STORE_KEYVAULT)

    edit_comp.edit_buttons.btn_save.click()
    args_settings = edit_comp.settings["GeoIPLite"]["Args"]["AuthKey"]
    check.is_in("KeyVault", args_settings)
    check.is_none(args_settings["KeyVault"])
