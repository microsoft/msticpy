# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""datq query test class."""
from copy import deepcopy
from collections import namedtuple
import unittest
from unittest.mock import patch, MagicMock
import os
from pathlib import Path
import warnings

import keyring

from azure.core.exceptions import ResourceNotFoundError

from msticpy.common import secret_settings
from msticpy.common.keyvault_client import (
    # AuthClient,
    # KeyringAuthClient,
    BHKeyVaultClient,
    BHKeyVaultMgmtClient,
    KeyVaultSettings,
    MsticpyKeyVaultConfigError,
    MsticpyKeyVaultMissingSecretError,
)
from msticpy.common import pkg_config
from msticpy.common.provider_settings import get_provider_settings
from msticpy.common.utility import set_unit_testing

from ..unit_test_lib import get_test_data_path, custom_mp_config

_TEST_DATA = get_test_data_path()

# set a flag to indicate we're in a unit test
set_unit_testing(True)

# pylint: disable=invalid-name, no-member, attribute-defined-outside-init
# pylint: disable=protected-access, unused-argument, super-init-not-called
# flake8: noqa

# Unit test mock patches
az_connect_core_patch = BHKeyVaultMgmtClient.__module__ + ".az_connect_core"
sec_client_patch = BHKeyVaultMgmtClient.__module__ + ".SecretClient"
is_ipython_patch = BHKeyVaultMgmtClient.__module__ + ".is_ipython"
display_patch = BHKeyVaultMgmtClient.__module__ + ".display"
HTML_patch = BHKeyVaultMgmtClient.__module__ + ".HTML"
basic_tok_auth_patch = BHKeyVaultMgmtClient.__module__ + ".BasicTokenAuthentication"
kv_mgmt_client_patch = BHKeyVaultMgmtClient.__module__ + ".KeyVaultManagementClient"


# Test classes used in unit tests
class _KeyringTestBackend(keyring.backend.KeyringBackend):
    """TestKeyring that returns mocked passwords."""

    priority = -1

    def __init__(self):  # noqa
        self._secrets = {}
        self._secrets.update(KV_SECRETS)
        super().__init__()

    def get_password(self, service, username):

        return self._secrets.get(username, None)

    def set_password(self, service, username, password=None):
        self._secrets[username] = password

    def delete_password(self, service, username):
        self._secrets.pop(username, None)


class _SecretClientTest:
    """TestKeyring that returns mocked passwords."""

    priority = -1

    def __init__(self):
        self._secrets = {}
        self._secrets.update(KV_SECRETS)
        self._sec_props = {name: _KVTestSec(name) for name in KV_SECRETS}

    def get_secret(self, name, *args, **kwargs):
        del args, kwargs
        if name not in self._secrets:
            raise ResourceNotFoundError(f"Missing secret {name}")
        sec_bundle = _KVTestSec(obj_id=name)
        sec_bundle.value = self._secrets[name]
        return sec_bundle

    def set_secret(self, name, value, *args, **kwargs):
        del args, kwargs
        self._secrets[name] = value
        sec_bundle = _KVTestSec(obj_id=name)
        sec_bundle.value = self._secrets[name]

        self._sec_props[name] = sec_bundle
        return sec_bundle

    def list_properties_of_secrets(self):
        return self._sec_props.values()


class _KVTestSec:
    URI = "https://myvault.vault.azure.net/secrets/{name}"

    def __init__(self, obj_id):
        self.name = obj_id
        self.id = self.URI.format(name=obj_id)


class _KeyVaultMgmtMock:
    def __init__(self):
        self.vaults = _KeyVaultVaultsMock()


class _KeyVaultVaultsMock:
    def __init__(self):
        self.vaults = {}

    def list(self):
        return self.vaults.values()

    def get(self, res_group, vault_name):
        return self.vaults.get(vault_name, None)

    def create_or_update(self, res_group, vault_name, params):
        vault = _KeyVaultVaultMock(vault_name)
        vault.params = params
        vault.resource_group = res_group
        self.vaults[vault_name] = vault
        return vault


class _KeyVaultPropsMock:
    pass


class _KeyVaultVaultMock:
    URI_TEMPLT = "https://{vault}.vault.azure.net"

    def __init__(self, name):
        self.name = name
        props = _KeyVaultPropsMock()
        props.vault_uri = self.URI_TEMPLT.format(vault=name)
        self.properties = props

    def result(self):
        return self


# The actual unit test class
class TestSecretsConfig(unittest.TestCase):
    """Unit test class."""

    def setUp(self):
        """Create keyring for tests."""
        keyring.set_keyring(_KeyringTestBackend())

    def test_keyring_client(self):
        """Test keyring client."""
        kr_client = secret_settings.KeyringClient()

        for sec_name, pwd in KV_SECRETS.items():
            self.assertEqual(pwd, kr_client.get_secret(sec_name))

        self.assertIsNone(kr_client.get_secret("DoesntExist"))
        kr_client.set_secret("new_secret", "secret_value")
        self.assertEqual("secret_value", kr_client.get_secret("new_secret"))

        self.assertEqual("secret_value", kr_client["new_secret"])
        # pylint: disable=pointless-statement
        with self.assertRaises(KeyError):
            kr_client["DoesntExist"]

    def test_config_load(self):
        """Test loading configuration from msticpyconfig."""
        expected = {
            "TenantId": "72f988bf-86f1-41af-91ab-2d7cd011db47",
            "SubscriptionId": "40dcc8bf-0478-4f3b-b275-ed0a94f2c013",
            "ResourceGroup": "ASIHuntOMSWorkspaceRG",
            "AzureRegion": "East US",
            "VaultName": "mstic-ianhelle",
            "UseKeyring": True,
            "Authority": "global",
        }

        kv_settings = get_kv_settings("msticpyconfig-kv.yaml")
        self.assertEqual(kv_settings.get("TenantId"), expected["TenantId"])
        self.assertEqual(kv_settings["TenantId"], expected["TenantId"])
        self.assertIn("TenantId", kv_settings)
        self.assertIsNone(kv_settings.get("NotATenantId"))

        self.assertEqual(kv_settings.authority_uri, "https://login.microsoftonline.com")
        self.assertEqual(
            kv_settings.get_tenant_authority_uri(),
            "https://login.microsoftonline.com/72f988bf-86f1-41af-91ab-2d7cd011db47",
        )
        self.assertEqual(
            kv_settings.get_tenant_authority_uri(tenant="myorg.com"),
            "https://login.microsoftonline.com/myorg.com",
        )
        self.assertEqual(
            kv_settings.get_tenant_authority_host(),
            "login.microsoftonline.com/72f988bf-86f1-41af-91ab-2d7cd011db47",
        )
        self.assertEqual(
            kv_settings.get_tenant_authority_host(tenant="myorg.com"),
            "login.microsoftonline.com/myorg.com",
        )

        for attrib in expected:
            self.assertEqual(kv_settings[attrib], expected[attrib])

        kv_settings.authority = "usgov"
        self.assertEqual(kv_settings.authority_uri, "https://login.microsoftonline.us")
        self.assertEqual(
            kv_settings.keyvault_uri, "https://{vault}.vault.usgovcloudapi.net"
        )
        self.assertEqual(kv_settings.mgmt_uri, "https://management.usgovcloudapi.net/")

        kv_settings.authority = "de"
        self.assertEqual(kv_settings.authority_uri, "https://login.microsoftonline.de")

        kv_settings.authority = "cn"
        self.assertEqual(kv_settings.authority_uri, "https://login.chinacloudapi.cn")

    @patch(sec_client_patch)
    @patch(az_connect_core_patch)
    def test_keyvault_client(
        self,
        az_connect_core,
        sec_client,
    ):
        kv_sec_client = _SecretClientTest()
        sec_client_obj = MagicMock()
        sec_client_obj.list_properties_of_secrets = (
            kv_sec_client.list_properties_of_secrets
        )
        sec_client_obj.get_secret = kv_sec_client.get_secret
        sec_client_obj.set_secret = kv_sec_client.set_secret
        sec_client.return_value = sec_client_obj
        # call_prompt = lambda client_id, authority, prompt_callback: _prompt_for_code(
        #    DEV_CODE
        # )
        # az_connect_core_patch.side_effect = call_prompt
        kv_settings = get_kv_settings("msticpyconfig-kv.yaml")

        # Check both vault params
        BHKeyVaultClient(
            tenant_id=kv_settings.tenantid,
            vault_uri="https://myvault.vault.azure.net",
            debug=True,
        )
        BHKeyVaultClient(
            tenant_id=kv_settings.tenantid, vault_name="myvault", debug=True
        )

        # Check missing tenantid
        no_tenant_id = deepcopy(kv_settings)
        no_tenant_id.tenantid = None
        with self.assertRaises(MsticpyKeyVaultConfigError):
            BHKeyVaultClient(settings=no_tenant_id, debug=True)

        keyvault_client = BHKeyVaultClient(debug=True)

        # Check secret methods
        for sec_id in keyvault_client.secrets:
            sec_name = sec_id.split("/")[-1]
            self.assertIn(sec_name, KV_SECRETS)

        for sec, val in KV_SECRETS.items():
            kv_val = keyvault_client.get_secret(sec)
            self.assertEqual(val, kv_val)

        with self.assertRaises(MsticpyKeyVaultMissingSecretError):
            keyvault_client.get_secret("DoesntExist")

        kv_sec_client.set_secret("NoSecret", "")
        with self.assertRaises(MsticpyKeyVaultMissingSecretError):
            keyvault_client.get_secret("NoSecret")

        kv_sec_client.set_secret("MyTestSecret", "TheActualValue")
        self.assertEqual(keyvault_client.get_secret("MyTestSecret"), "TheActualValue")

    @patch(kv_mgmt_client_patch)
    @patch(az_connect_core_patch)
    def test_kv_mgmt_client(self, az_core, kv_mgmt):
        AzCredentials = namedtuple("AzCredentials", ["legacy", "modern"])
        LegacyCreds = namedtuple("legacycreds", ["token"])
        az_core.return_value = AzCredentials(LegacyCreds(ACC_TOKEN), "cred")
        # expiry_time = datetime.now() + timedelta(1)
        # auth_context.return_value = mock_auth_context_methods(expiry_time)
        kv_mgmt.return_value = _KeyVaultMgmtMock()
        # kv_sec_client = _SecretClientTest()

        kv_settings = get_kv_settings("msticpyconfig-kv.yaml")
        vault_mgmt = BHKeyVaultMgmtClient(
            tenant_id=kv_settings.tenantid,
            subscription_id=kv_settings.subscriptionid,
            resource_group=kv_settings.resourcegroup,
            azure_region=kv_settings.azureregion,
        )

        vault_mgmt.create_vault("mynewvault")
        vault_mgmt.create_vault("myothervault")
        self.assertIn("mynewvault", vault_mgmt.list_vaults())
        self.assertIn("myothervault", vault_mgmt.list_vaults())

        self.assertEqual(
            vault_mgmt.get_vault_uri("mynewvault"), "https://mynewvault.vault.azure.net"
        )

        kv_settings = get_kv_settings("msticpyconfig-kv.yaml")
        kv_settings["azureregion"] = None
        with self.assertRaises(MsticpyKeyVaultConfigError):
            nr_vault_mgmt = BHKeyVaultMgmtClient(
                tenant_id=kv_settings.tenantid,
                subscription_id=kv_settings.subscriptionid,
                resource_group=kv_settings.resourcegroup,
                settings=kv_settings,
            )
            nr_vault_mgmt.create_vault("mynewvault")

    @patch(sec_client_patch)
    def test_secret_settings(
        self,
        sec_client,
    ):
        kv_sec_client = _SecretClientTest()
        sec_client_obj = MagicMock()
        sec_client_obj.list_properties_of_secrets = (
            kv_sec_client.list_properties_of_secrets
        )
        sec_client_obj.get_secret = kv_sec_client.get_secret
        sec_client_obj.set_secret = kv_sec_client.set_secret
        sec_client.return_value = sec_client_obj

        # Check single value
        get_kv_settings("msticpyconfig-kv.yaml")
        sec_settings = secret_settings.SecretsClient()
        kv_entry_name = "TIProviders-VirusTotal-Args-AuthKey"
        conf_path = kv_entry_name.replace("-", ".")
        setting_func = sec_settings.get_secret_accessor(conf_path)
        self.assertTrue(callable(setting_func))
        sec_value = sec_settings.read_secret(setting_func)
        self.assertEqual(KV_SECRETS[kv_entry_name], sec_value)

        # Check all TIProvider settings
        self._check_provider_settings(sec_settings)

        # Reload without using keyring cache
        sec_settings = secret_settings.SecretsClient(use_keyring=False)
        self._check_provider_settings(sec_settings)

    def _check_provider_settings(self, sec_settings):
        prov_settings = get_provider_settings()
        for p_name, p_settings in prov_settings.items():
            args = p_settings.args
            if p_name == "OTX":
                sec_value = sec_settings.read_secret(args["AuthKey"])
                self.assertEqual(KV_SECRETS["OTX-AuthKey"], sec_value)
            elif p_name == "VirusTotal":
                sec_value = sec_settings.read_secret(args["AuthKey"])
                self.assertEqual(
                    KV_SECRETS["TIProviders-VirusTotal-Args-AuthKey"], sec_value
                )
            elif p_name == "XForce":
                sec_value = sec_settings.read_secret(args["AuthKey"])
                self.assertEqual(KV_SECRETS["XForce-AuthKey"], sec_value)
                sec_value = sec_settings.read_secret(args["ApiID"])
                self.assertEqual(KV_SECRETS["XForce-ApiID"], sec_value)


def test_keyring_client():
    secret_settings.KeyringClient.is_keyring_available()


# Helper functions
def get_kv_settings(config_file):
    test_config = Path(_TEST_DATA).joinpath(config_file)
    os.environ[pkg_config._CONFIG_ENV_VAR] = str(test_config)
    with warnings.catch_warnings():
        # We want to ignore warnings from missing config
        warnings.simplefilter("ignore", category=UserWarning)
        with custom_mp_config(test_config):
            return KeyVaultSettings()


def mock_auth_context_methods(expiry_time):
    context_obj = MagicMock()
    context_obj.acquire_user_code = MagicMock(return_value=DEV_CODE)
    context_obj.acquire_token_with_device_code = MagicMock(return_value=TEST_TOKEN)
    refresh_token = deepcopy(TEST_TOKEN)

    refresh_token["expiresOn"] = expiry_time.strftime("%Y-%m-%d %H:%M:%S.%f")
    # acquire_with_refresh.return_value = refresh_token
    context_obj.acquire_token_with_refresh_token = MagicMock(return_value=refresh_token)
    return context_obj


# Constants
ACC_TOKEN_LINES = [
    "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsIng1dCI6Ik1ERXlNelExTmpjNE9UQXhNak0w",
    "XzZ0X3RERSIsImtpZCI6Ik1ERXlNelExTmpjNE9UQXhNak0wXzZ0X3RERSJ9.eyJhdWQiOiJ",
    "odHRwczovL21hbmFnZW1lbnQuYXp1cmUuY29tIiwiaXNzIjoiaHR0cHM6Ly9zdHMud2luZG9",
    "3cy5uZXQvZmZmZmZmZmYtODg4OC00NDQ0LWFhYWEtMmQyZDJkMmQyZDJkLyIsImlhdCI6MTU",
    "4MzM3NTcyNiwibmJmIjoxNTgzMzc1NzI2LCJleHAiOjE1ODMzNzk2MjYsIl9jbGFpbV9uYW1",
    "lcyI6eyJncm91cHMiOiJzcmMxIn0sIl9jbGFpbV9zb3VyY2VzIjp7InNyYzEiOnsiZW5kcG9",
    "pbnQiOiJodHRwczovL2dyYXBoLndpbmRvd3MubmV0L2ZmZmZmZmZmLTg4ODgtNDQ0NC1hYWF",
    "hLTJkMmQyZDJkMmQyZC91c2Vycy9lYWVhZWFlYS03Nzc3LTAwMDAtZWVlZS1lN2U3ZTdlN2U",
    "3ZTcvZ2V0TWVtYmVyT2JqZWN0cyJ9fSwiYWNyIjoiMSIsImFpbyI6Ik1ERXlNelExTmpjNE9",
    "UQXhNak0wTlRZM09Ea3dNVEl6TkRVMk56ZzVNREV5TXpRMU5qYzRPVEF4TWpNME5UWTNPRGt",
    "3TVRJek1ERXlNelExTmpjNE9UQXhNak0wTlRZM09Ea3dNVEl6TkRVMk56ZzVNREV5TXpRMSI",
    "sImFtciI6WyJwd2QiLCJtZmEiXSwiYXBwaWQiOiIwNGIwNzc5NS04ZGRiLTQ2MWEtYmJlZS0",
    "wMmY5ZTFiZjdiNDYiLCJhcHBpZGFjciI6IjAiLCJmYW1pbHlfbmFtZSI6IlVzZXIiLCJnaXZ",
    "lbl9uYW1lIjoiVGVzdCIsImlwYWRkciI6IjE5Mi4xNjguMS4yMiIsIm5hbWUiOiJUZXN0IFV",
    "zZXIiLCJvaWQiOiJlYWVhZWFlYS03Nzc3LTAwMDAtZWVlZS1lN2U3ZTdlN2U3ZTciLCJvbnB",
    "yZW1fc2lkIjoiUy0xLTUtMjEtMDEyMzQ1Njc4OS0wMTIzNDU2Nzg5LTAxMjM0NTY3ODktMDE",
    "yMzQ1NiIsInB1aWQiOiIxMDAwMDAwMDAwMDAwMDAwIiwic2NwIjoidXNlcl9pbXBlcnNvbmF",
    "0aW9uIiwic3ViIjoiTURFeU16UTFOamM0T1RBeE1qTTBOVFkzT0Rrd01USXpORFUyTnpnNU1",
    "ERXkiLCJ0aWQiOiJmZmZmZmZmZi04ODg4LTQ0NDQtYWFhYS0yZDJkMmQyZDJkMmQiLCJ1bml",
    "xdWVfbmFtZSI6InRlc3R1c2VyQG1pY3Jvc29mdC5jb20iLCJ1cG4iOiJ0ZXN0dXNlckBtaWN",
    "yb3NvZnQuY29tIiwidXRpIjoiTURFeU16UV9NREV5TXpRTURFeU16USIsInZlciI6IjEuMCJ",
    "9.MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMT",
    "Iz.NDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0N",
    "TY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4O",
    "TAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyM",
    "zQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2N",
    "zg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwM",
    "TIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0N",
    "TY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4O",
    "TAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyM",
    "zQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2N",
    "zg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwM",
    "TIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0N",
    "TY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4O",
    "TAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyM",
    "zQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2N",
    "zg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OQ==",
]


ACC_TOKEN = "".join(ACC_TOKEN_LINES)


REF_TOKEN = """
MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIz
NDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3
ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAx
MjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1
Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5
MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIz
NDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3
ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAx
MjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1
Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5
MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIz
NDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3
ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAx
MjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1
Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5
MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OQ==
"""

TEST_TOKEN = {
    "tokenType": "Bearer",
    "expiresIn": 3598,
    "expiresOn": "2020-03-04 19:40:25.132471",
    "resource": "https://management.azure.com",
    "accessToken": ACC_TOKEN,
    "refreshToken": REF_TOKEN,
    "familyName": "User",
    "givenName": "Test",
    "oid": "eaeaeaea-7777-0000-eeee-e7e7e7e7e7e7",
    "tenantId": "ffffffff-8888-4444-aaaa-2d2d2d2d2d2d",
    "userId": "testuser@microsoft.com",
    "isUserIdDisplayable": True,
    "isMRRT": True,
    "_clientId": "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    "_authority": "https://login.microsoftonline.com/ffffffff-8888-4444-aaaa-2d2d2d2d2d2d",
}

DEV_CODE = {
    "user_code": "BHNDTAGMG",
    "device_code": "eadef8f0-7799-4087-9eb7-c1e7a055616d",
    "verification_url": "https://microsoft.com/devicelogin",
    "expires_in": 900,
    "interval": 5,
    "message": "To sign in, use a web browser to open the page https://microsoft.com/devicelogin and enter the code BHNDTAGMG to authenticate.",
    "correlation_id": "42b6684f-2d31-4673-a1c4-f0a35f80ad82",
}

# These are the first two sections of the ACC_TOKEN above
# Take the first two "." separated sections of the section
# and base64-decode the two strings.
# The third section of the token is a series of b64 encoded
# elements separated by "_" and "-"
j_token_header = {
    "typ": "JWT",
    "alg": "RS256",
    "x5t": "MDEyMzQ1Njc4OTAxMjM0_6t_tDE",
    "kid": "MDEyMzQ1Njc4OTAxMjM0_6t_tDE",
}

j_acc_token = {
    "aud": "https://management.azure.com",
    "iss": "https://sts.windows.net/ffffffff-8888-4444-aaaa-2d2d2d2d2d2d/",
    "iat": 1583375726,
    "nbf": 1583375726,
    "exp": 1583379626,
    "_claim_names": {"groups": "src1"},
    "_claim_sources": {
        "src1": {
            "endpoint": "https://graph.windows.net/ffffffff-8888-4444-aaaa-2d2d2d2d2d2d/users/eaeaeaea-7777-0000-eeee-e7e7e7e7e7e7/getMemberObjects"
        }
    },
    "acr": "1",
    "aio": "MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzMDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1",
    "amr": ["pwd", "mfa"],
    "appid": "04b07795-8ddb-461a-bbee-02f9e1bf7b46",
    "appidacr": "0",
    "family_name": "User",
    "given_name": "Test",
    "ipaddr": "192.168.1.22",
    "name": "Test User",
    "oid": "eaeaeaea-7777-0000-eeee-e7e7e7e7e7e7",
    "onprem_sid": "S-1-5-21-0424246789-0424246789-0424246789-0424246",
    "puid": "1000000000000000",
    "scp": "user_impersonation",
    "sub": "MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEy",
    "tid": "ffffffff-8888-4444-aaaa-2d2d2d2d2d2d",
    "unique_name": "testuser@microsoft.com",
    "upn": "testuser@microsoft.com",
    "uti": "MDEyMzQ_MDEyMzQMDEyMzQ",
    "ver": "1.0",
}


KV_SECRETS = {
    "OTX-AuthKey": "OTX_AuthKey",
    "TIProviders-VirusTotal-Args-AuthKey": "42424678",
    "VirusTotal-AuthKey": "VirusTotal_AuthKey",
    "XForce-ApiID": "OTX_ApiID",
    "XForce-AuthKey": "XForce_AuthKey",
}
