# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import json
from pathlib import Path
from unittest.mock import PropertyMock, patch

import pytest
import pytest_check as check

from msticpy.config.comp_edit import CompEditStatusMixin
from msticpy.config.mp_config_file import MpConfigFile

from ..unit_test_lib import TEST_DATA_PATH

__author__ = "Ian Hellen"


CompEditStatusMixin.testing = True


def test_mp_config_file_init():
    """Test MpConfigFile init."""
    mpc_file = MpConfigFile()
    check.is_true(mpc_file.settings)

    # Test loading with empty settings
    mpc_file = MpConfigFile(settings={})
    check.is_false(mpc_file.settings)

    mpc_file = MpConfigFile()
    # load file
    with pytest.raises(FileNotFoundError):
        mpc_file.load_from_file("non-existent-file.txt")

    config_path = Path(TEST_DATA_PATH).joinpath("msticpyconfig.yaml")
    mpc_file = MpConfigFile(file=config_path)
    check.is_true(mpc_file.settings)


def test_mp_config_file_load_default():
    """Test load_default."""
    mpc_file = MpConfigFile()
    mpc_file.load_default()
    check.is_true(mpc_file.settings)

    mpc_file.settings = None
    mpc_file.buttons["load_def"].click()
    check.is_true(mpc_file.settings)


def test_mp_config_file_validate():
    """Test validate."""
    mpc_file = MpConfigFile()
    mpc_file.validate_settings()
    check.is_in(mpc_file.txt_viewer, mpc_file.viewer.children)
    check.is_true(len(mpc_file.txt_viewer.value) > 0)


def test_mp_config_file_view_settings():
    """Test view settings."""
    mpc_file = MpConfigFile()
    mpc_file.txt_viewer.value = ""
    mpc_file.view_settings()
    check.greater(len(mpc_file.txt_viewer.value), 0)
    mpc_file.buttons["view"].click()
    mpc_file.btn_close.click()


def test_mp_config_file_browse():
    """Test browse."""
    mpc_file = MpConfigFile()
    mpc_file.browse_for_file()
    check.is_in(mpc_file.file_browser.layout, mpc_file.viewer.children)
    mpc_file.btn_close.click()


def test_mp_config_file_save():
    """Test save."""
    mpc_file = MpConfigFile()
    mpc_file.load_default()
    tgt_file = "./msticpy_test.yaml"
    mpc_file.save_to_file(tgt_file)
    check.is_true(Path(tgt_file).is_file())
    mpc_file.save_to_file(tgt_file)

    check.is_true(Path().glob(f"{tgt_file}.save_*"))
    Path(tgt_file).unlink()
    for test_file in Path().glob(f"{tgt_file}.save_*"):
        test_file.unlink()

    mpc_file.txt_current_config_path.value = tgt_file
    mpc_file.buttons["save"].click()  # same thing but via button click
    check.is_true(Path(tgt_file).is_file())
    Path(tgt_file).unlink()


def test_mp_config_file_convert():
    """Test convert."""
    # config json convert
    config_json = Path(TEST_DATA_PATH).joinpath("config.json")
    mpc_file = MpConfigFile(file=config_json)
    mp_equiv = mpc_file.map_json_to_mp_ws().get("AzureSentinel", {}).get("Workspaces")

    j_text = config_json.read_text()
    config_j = json.loads(j_text)
    workspace = config_j.get("workspace_name", "Default")
    check.equal(config_j["resource_group"], mp_equiv[workspace]["ResourceGroup"])
    check.equal(config_j["subscription_id"], mp_equiv[workspace]["SubscriptionId"])
    check.equal(config_j["tenant_id"], mp_equiv[workspace]["TenantId"])
    check.equal(config_j["workspace_id"], mp_equiv[workspace]["WorkspaceId"])

    # config json convert in-place
    config_json = Path(TEST_DATA_PATH).joinpath("config.json")
    mpc_file = MpConfigFile(file=config_json)
    mpc_file.buttons["convert"].click()
    check.is_in("MSSentinel", mpc_file.settings)


_KV_SECS = {"url/Item1": "Value1", "url/Item2": "Value2"}
KV_SEC_CLIENT_PATCH = MpConfigFile.__module__ + ".BHKeyVaultClient"


@patch(KV_SEC_CLIENT_PATCH)
def test_mp_config_file_show_kv(kv_client):
    """Test view secrets."""
    del kv_client
    mpc_file = MpConfigFile()

    mpc_file.show_kv_secrets()
    check.is_not_none(mpc_file.kv_client)

    # set up mocked kv_client
    sec_list = list(_KV_SECS.keys())
    sec_list.append("url/MissingValue")
    type(mpc_file.kv_client).secrets = PropertyMock(return_value=sec_list)
    mpc_file.kv_client.get_secret = lambda sec: _KV_SECS[f"url/{sec}"]

    # run show secrets again with mocked client
    mpc_file.show_kv_secrets()

    for name, val in _KV_SECS.items():
        s_name = name.split("/")[-1]
        check.is_in(s_name, mpc_file.txt_viewer.value)
        check.is_in(val, mpc_file.txt_viewer.value)

    check.is_in("MissingValue", mpc_file.txt_viewer.value)
    check.is_in("Value: Could not display secret", mpc_file.txt_viewer.value)
