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
from msticpy.config.ce_azure_sentinel import CEAzureSentinel
from msticpy.config.ce_common import get_def_tenant_id
from msticpy.config.ce_data_providers import CEDataProviders
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
    CEAzureSentinel,
    CEDataProviders,
    CETIProviders,
    CEOtherProviders,
    CEAutoLoadComps,
    CEAutoLoadQProvs,
]


# pylint: disable=protected-access
@pytest.mark.parametrize("editor", _EDITORS)
def test_item_editor_controls(editor, mp_conf_ctrl):
    """Items edit controls."""
    edit_comp = editor(mp_controls=mp_conf_ctrl)

    check.is_not_none(edit_comp.help.html_help.value)
    check.is_not_none(edit_comp._DESCRIPTION)
    check.is_not_none(edit_comp._COMP_PATH)
    check.greater_equal(len(edit_comp._HELP_URI), 1)

    opts = edit_comp.select_item.options
    n_opts = len(opts)

    # If this control has an options list - select the first of these
    if hasattr(edit_comp, "prov_options"):
        edit_comp.prov_options.value = edit_comp.prov_options.options[0]
        select_label = edit_comp.prov_options.value
    else:
        select_label = opts[1][0]

    # If there are no existing items, add one
    if n_opts == 0:
        if isinstance(edit_comp, CEAutoLoadComps):
            edit_comp.prov_options.value = "AzureData"
            edit_comp.edit_buttons.btn_add.click()
            edit_comp.edit_buttons.btn_del.click()
            edit_comp.prov_options.value = "Notebooklets"
            edit_comp.edit_buttons.btn_add.click()
        elif isinstance(edit_comp, CEAutoLoadQProvs):
            edit_comp.prov_options.value = "Splunk"
            edit_comp.edit_buttons.btn_add.click()
            edit_comp.edit_buttons.btn_del.click()
            edit_comp.prov_options.value = "AzureSentinel.Default"
            edit_comp.edit_buttons.btn_add.click()
        check.greater(len(edit_comp.select_item.options), n_opts, "Item added")
        n_opts += 1
    else:
        # otherwise, select this one and delete it, then add a new one
        edit_comp.select_item.label = select_label
        edit_comp.edit_buttons.btn_del.click()
        check.less(len(edit_comp.select_item.options), n_opts, "Item deleted")
        edit_comp.edit_buttons.btn_add.click()
        check.equal(len(edit_comp.select_item.options), n_opts, "Item added")

    # Save the current item
    edit_comp.edit_buttons.btn_save.click()

    # delete whatever we've just added
    edit_comp.edit_buttons.btn_del.click()
    check.less(len(edit_comp.select_item.options), n_opts, "Item deleted")


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
def test_arg_controls_kv(kv_sec, mp_conf_ctrl):
    """Argcontrol is a sub-component for editing Args."""
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
