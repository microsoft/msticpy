# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure Synapse tools unit test."""
import os
import re
from collections import namedtuple
from pathlib import Path
from unittest.mock import Mock

import pytest
import pytest_check as check
import respx

from msticpy.common import pkg_config
from msticpy.init import azure_synapse_tools as synapse

from ..unit_test_lib import custom_mp_config, get_test_data_path
from .mssparkutils_fixtures import LINKED_SERVICES_RESP, Jwt, MSSparkUtils

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


SparkUtilsTest = namedtuple("SparkUtilsTest", "mp_spark, msspark_mock")


@pytest.fixture
@respx.mock
def mpsparkutils(monkeypatch):
    url_pattern = re.compile(
        r"https://.*dev\.azuresynapse\.net/linkedservices\?api-version=2020-12-01"
    )
    respx.get(url_pattern).respond(status_code=200, json=LINKED_SERVICES_RESP)

    spark_data = {
        "mount": True,
        "jobId": "2",
        "workspace": "test-workspace",
        "secrets": {
            "special-sec": "the-value",
            "vault1": {"special-sec": "the-value2"},
            "clientid": "[PLACEHOLDER]",
            "clientsecret": "[PLACEHOLDER]",
        },
    }
    spark_mock = MSSparkUtils(data=spark_data)
    monkeypatch.setattr(synapse, "mssparkutils", spark_mock)
    monkeypatch.setattr(synapse, "jwt", Jwt())

    return SparkUtilsTest(synapse.MPSparkUtils(), spark_mock)


def test_create_mp_spark_utils_instance(mpsparkutils):
    """Function_docstring."""
    mp_spark = mpsparkutils.mp_spark
    check.equal(len(mp_spark.linked_services), 5)
    svc_types = [ls.svc_type for ls in mp_spark.linked_services]
    check.is_in("AzureBlobFS", svc_types)
    check.is_in("AzureSqlDW", svc_types)
    check.is_in("AzureKeyVault", svc_types)

    check.is_false(mp_spark.fs_mounts)
    check.equal(mp_spark.job_id, "2")
    check.equal(mp_spark.config_path, Path(f"/synfs/{mp_spark.job_id}/msticpy"))
    check.equal(mp_spark.application_id, "App-ID")
    check.equal(mp_spark.object_id, "App-OID")
    check.equal(mp_spark.tenant_id, "AAD-GUID")
    check.equal(mp_spark.mount_point, "msticpy")
    check.equal(mp_spark.container, "sentinelfiles")

    storage_svc = mp_spark.get_storage_service()
    check.equal(storage_svc.name, "my-synapse1-WorkspaceDefaultStorage")
    check.equal(
        mp_spark.get_storage_service("my-synapse1-WorkspaceDefaultStorage").name,
        "my-synapse1-WorkspaceDefaultStorage",
    )
    check.equal(mp_spark.get_service_of_type("AzureKeyVault").name, "KeyVault1")
    check.equal(
        mp_spark.get_service_of_type("AzureSqlDW").name,
        "my-synapse1-WorkspaceDefaultSqlServer",
    )
    check.is_none(mp_spark.get_service_of_type("InvalidType"))

    check.equal(len(mp_spark.get_all_services_of_type("AzureBlobFS")), 2)

    check.equal(
        mp_spark.get_ws_default_storage().name, "my-synapse1-WorkspaceDefaultStorage"
    )
    check.equal(mp_spark.get_service("KeyVault1").svc_type, "AzureKeyVault")
    check.is_none(mp_spark.get_service("InvalidName"))


def test_linked_service_attribs(mpsparkutils):
    """Test getattr functionality of LinkedService class."""
    mp_spark = mpsparkutils.mp_spark
    storage_svc = mp_spark.get_storage_service()
    check.equal(storage_svc.name, "my-synapse1-WorkspaceDefaultStorage")

    check.equal(storage_svc.azure_name, "mynsynapse")
    check.is_in(
        "linkedservices/my-synapse1-WorkspaceDefaultStorage", storage_svc.res_id
    )
    check.equal(storage_svc.svc_type, "AzureBlobFS")
    # check can retrieve subkeys of properties via direct attrib name
    check.is_instance(storage_svc.typeProperties, dict)
    # check can retrieve subkeys of typeProperties via direct attrib name
    check.equal(storage_svc.url, "https://mynsynapse.dfs.core.windows.net")

    with pytest.raises(AttributeError):
        test = storage_svc.unknown_property

    check.is_in(
        "LinkedService(name='my-synapse1-WorkspaceDefaultStorage'", repr(storage_svc)
    )


def test_mp_spark_utils_get_secret(mpsparkutils):
    """Test get_kv_secret function."""
    mp_spark = mpsparkutils.mp_spark

    check.equal(mp_spark.job_id, "2")
    check.equal(mp_spark.get_kv_secret("special-sec"), "the-value")


def test_mp_spark_utils_mount(mpsparkutils):
    """Test get_kv_secret function."""
    mp_spark = mpsparkutils.mp_spark
    mpsparkutils.msspark_mock.fs.add_mount(
        "testf", "storage_url", linkedService="my_dls_hs"
    )
    check.equal(len(mp_spark.fs_mounts), 1)
    storage_svc = mp_spark.get_storage_service()

    print("Mounting storage...", end=" ")
    stor_acct_match = re.match(synapse._AZ_NAME_PATTERN, storage_svc.url)
    mount_success = synapse.mount_container(
        store_acct_name=stor_acct_match["name"] if stor_acct_match else "",
        container=mp_spark.container,
        mount_path=mp_spark.mount_point,
        linked_service=storage_svc.name,
    )
    check.equal(len(mp_spark.fs_mounts), 2)
    check.is_true(mount_success)

    # try to mount again - should not add new mount point
    mount_success = synapse.mount_container(
        store_acct_name=stor_acct_match["name"] if stor_acct_match else "",
        container=mp_spark.container,
        mount_path=mp_spark.mount_point,
        linked_service=storage_svc.name,
    )
    check.equal(len(mp_spark.fs_mounts), 2)
    check.is_true(mount_success)

    # try to mount same point with different storage - should not add new mount point
    mount_success = synapse.mount_container(
        store_acct_name=stor_acct_match["name"] if stor_acct_match else "",
        container="newcontainer",
        mount_path=mp_spark.mount_point,
        linked_service=storage_svc.name,
    )
    check.equal(len(mp_spark.fs_mounts), 2)
    check.is_false(mount_success)

    # try to mount same point with different storage - should not add new mount point
    mount_success = synapse.mount_container(
        store_acct_name=stor_acct_match["name"] if stor_acct_match else "",
        container="newcontainer",
        mount_path="newmount",
        linked_service=storage_svc.name,
    )
    check.equal(len(mp_spark.fs_mounts), 3)
    check.is_true(mount_success)


def test_synapse_name():
    """Test the Synapse Name class."""
    syn_name = synapse.SynapseName(workspace_id="9999-99999-1234567")
    check.equal(syn_name.storage_account_prefix, "adlsforsentinel")
    check.equal(syn_name.key_vault_name_prefix, "kvforsentinel")
    check.equal(syn_name.kv_linked_service, "Akvlink")
    check.equal(syn_name.sp_client_id_name, "clientid")
    check.equal(syn_name.sp_client_sec_name, "clientsecret")
    check.equal(syn_name.container, "sentinelfiles")

    check.equal(syn_name.storage_account, "adlsforsentinel1234567")
    check.equal(syn_name.key_vault, "kvforsentinel1234567")

    syn_name = synapse.SynapseName(workspace_id="4567")
    check.equal(syn_name.storage_account, "adlsforsentinel4567")
    check.equal(syn_name.key_vault, "kvforsentinel4567")


def test_set_azure_env_creds(mpsparkutils):
    """Test setting environment variables for env credential."""
    mp_spark = mpsparkutils.mp_spark
    az_cred = synapse.AzureCredEnvNames
    synapse._set_azure_env_creds(mp_spark)
    # save any existing variables
    saved_env = {
        az_cred.AZURE_TENANT_ID: os.environ.get(az_cred.AZURE_TENANT_ID),
        az_cred.AZURE_CLIENT_ID: os.environ.get(az_cred.AZURE_CLIENT_ID),
        az_cred.AZURE_CLIENT_SECRET: os.environ.get(az_cred.AZURE_CLIENT_SECRET),
    }
    try:
        check.equal(os.environ[az_cred.AZURE_TENANT_ID], mp_spark.tenant_id)
        check.equal(os.environ[az_cred.AZURE_CLIENT_ID], "[PLACEHOLDER]")
        check.equal(
            os.environ[az_cred.AZURE_CLIENT_SECRET],
            "[PLACEHOLDER]",
        )
    finally:
        # restore any changed variables
        for env, val in saved_env.items():
            if val:
                os.environ[env] = val


def test_set_azure_msi_creds(mpsparkutils):
    """Test setting environment variables for env credential."""
    mp_spark = mpsparkutils.mp_spark
    az_cred = synapse.AzureCredEnvNames
    synapse._set_msi_client_id(mp_spark)
    # save any existing variables
    saved_env = {
        az_cred.AZURE_TENANT_ID: os.environ.get(az_cred.AZURE_TENANT_ID),
        az_cred.AZURE_CLIENT_ID: os.environ.get(az_cred.AZURE_CLIENT_ID),
    }
    try:
        check.equal(os.environ[az_cred.AZURE_TENANT_ID], mp_spark.tenant_id)
        check.equal(os.environ[az_cred.AZURE_CLIENT_ID], "App-ID")
    finally:
        # restore any changed variables
        for env, val in saved_env.items():
            if val:
                os.environ[env] = val


def test_set_mp_azure_settings():
    """Test setting msticpy settings."""
    mp_config = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(mp_config):
        prev_auth_methods = pkg_config.get_config("Azure").get("auth_methods").copy()
        synapse._set_mp_azure_settings("env")
        auth_methods = pkg_config.get_config("Azure").get("auth_methods")
        check.equal(auth_methods[0], "env")
        check.not_equal(auth_methods, prev_auth_methods)

    with custom_mp_config(mp_config):
        prev_auth_methods = pkg_config.get_config("Azure").get("auth_methods").copy()
        del pkg_config._settings["Azure"]
        synapse._set_mp_azure_settings("env")
        auth_methods = pkg_config.get_config("Azure").get("auth_methods")
        check.equal(auth_methods[0], "env")
        check.not_equal(auth_methods, prev_auth_methods)

    with custom_mp_config(mp_config):
        prev_auth_methods = pkg_config.get_config("Azure").get("auth_methods").copy()
        synapse._set_mp_azure_settings(prev_auth_methods[-1])
        auth_methods = pkg_config.get_config("Azure").get("auth_methods")
        check.equal(auth_methods[0], prev_auth_methods[-1])
        check.not_equal(auth_methods, prev_auth_methods)


@respx.mock
def test_init_synapse(mpsparkutils, monkeypatch):
    """Test the init_synapse function."""
    url_pattern = re.compile(
        r"https://.*dev\.azuresynapse\.net/linkedservices\?api-version=2020-12-01"
    )
    respx.get(url_pattern).respond(status_code=200, json=LINKED_SERVICES_RESP)

    # We need to patch out some of the functions to
    # avoid trying to authenticate and connect to Key Vault.
    config_mp = Mock(return_value=None)
    monkeypatch.setattr(synapse, "_configure_mp_settings", config_mp)
    az_connect = Mock(return_value="creds")
    monkeypatch.setattr(synapse, "az_connect", az_connect)
    check_kv = Mock(return_value=True)
    monkeypatch.setattr(synapse, "_check_kv_key_retrieval", check_kv)
    set_az_settings = Mock(return_value=None)
    monkeypatch.setattr(synapse, "_set_mp_azure_settings", set_az_settings)
    set_msi_client = Mock(return_value=None)
    monkeypatch.setattr(synapse, "_set_msi_client_id", set_msi_client)
    set_azure_env_creds = Mock(return_value=None)
    monkeypatch.setattr(synapse, "_set_azure_env_creds", set_azure_env_creds)
    is_in_synapse = Mock(return_value=True)
    monkeypatch.setattr(synapse, "is_in_synapse", is_in_synapse)

    synapse.init_synapse(identity_type="managed")

    check.is_instance(config_mp.mock_calls[0].args[0], synapse.MPSparkUtils)
    check.equal(len(az_connect.mock_calls), 1)
    check.equal(len(az_connect.mock_calls[0].args), 0)
    check.equal(len(check_kv.mock_calls), 1)
    check.equal(len(check_kv.mock_calls[0].args), 0)
    check.equal(len(set_az_settings.mock_calls), 1)
    check.equal(len(set_msi_client.mock_calls), 1)
    check.is_instance(
        set_msi_client.mock_calls[0].kwargs["mp_spark"], synapse.MPSparkUtils
    )
    check.is_instance(
        set_msi_client.mock_calls[0].kwargs["tenant_id"], (str, type(None))
    )
    check.equal(len(set_azure_env_creds.mock_calls), 0)
    check.equal(set_az_settings.mock_calls[0].kwargs["auth_method"], "msi")
    check.equal(set_az_settings.mock_calls[0].kwargs["cloud"], "global")

    synapse.init_synapse(identity_type="service_principal")
    check.is_instance(config_mp.mock_calls[1].args[0], synapse.MPSparkUtils)
    check.equal(len(az_connect.mock_calls), 2)
    check.equal(len(set_msi_client.mock_calls), 1)
    check.equal(len(set_azure_env_creds.mock_calls), 1)
    check.is_instance(
        set_azure_env_creds.mock_calls[0].kwargs["mp_spark"], synapse.MPSparkUtils
    )
    check.is_instance(
        set_azure_env_creds.mock_calls[0].kwargs["tenant_id"], (str, type(None))
    )
    check.equal(set_az_settings.mock_calls[1].kwargs["auth_method"], "env")
    check.equal(set_az_settings.mock_calls[0].kwargs["cloud"], "global")

    az_connect.return_value = None
    with pytest.raises(RuntimeError) as no_connect_err:
        synapse.init_synapse(identity_type="service_principal")
        check.equal(no_connect_err.value.args[0], "Could not authenticate to Azure.")

    az_connect.return_value = "creds"
    check_kv.return_value = False
    with pytest.raises(RuntimeError) as kv_err:
        synapse.init_synapse(identity_type="service_principal")
        check.equal(kv_err.value.args[0], "Could not retrieve secret from Key Vault.")

    prev_calls = len(config_mp.mock_calls)
    # This should cause early exit from function
    is_in_synapse.return_value = False
    synapse.init_synapse(identity_type="service_principal")
    check.equal(len(config_mp.mock_calls), prev_calls)


def test_is_in_synapse(monkeypatch):
    """Test check for environment variable."""
    check.is_false(synapse.is_in_synapse())

    monkeypatch.setenv("MMLSPARK_PLATFORM_INFO", "synapse")
    check.is_true(synapse.is_in_synapse())
