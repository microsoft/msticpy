# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Azure authentication handling."""
import os
from collections import namedtuple
import logging
import sys

from msrest.authentication import BasicTokenAuthentication
from azure.core.pipeline.policies import BearerTokenCredentialPolicy
from azure.core.pipeline import PipelineRequest, PipelineContext
from azure.core.pipeline.transport import HttpRequest
from azure.mgmt.subscription import SubscriptionClient
from azure.common.exceptions import CloudError
from azure.identity import (
    DefaultAzureCredential,
    InteractiveBrowserCredential,
    ChainedTokenCredential,
    EnvironmentCredential,
    AzureCliCredential,
    ManagedIdentityCredential,
)

from azure.identity import CredentialUnavailableError


# uncomment once circular dependencies fixed
# from .provider_settings import get_provider_settings

from .exceptions import MsticpyAzureConfigError


from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"


# Class to extract v1 authentication token from DefaultAzureCredential object.
# Credit - https://gist.github.com/lmazuel/cc683d82ea1d7b40208de7c9fc8de59d
class CredentialWrapper(BasicTokenAuthentication):
    """Class for handling legacy auth conversion."""

    def __init__(
        self,
        credential=None,
        resource_id="https://management.azure.com/.default",
        **kwargs,
    ):
        """
        Wrap any azure-identity credential to work with SDK that needs azure.common.credentials.

        Parameters
        ----------
        credential : [type], optional
            Any azure-identity credential, by default DefaultAzureCredential
        resource_id : str, optional
            The scope to use to get the token, by default "https://management.azure.com/.default"

        """
        super(CredentialWrapper, self).__init__(None)
        if credential is None:
            credential = DefaultAzureCredential()
        self._policy = BearerTokenCredentialPolicy(credential, resource_id, **kwargs)

    def set_token(self):
        """
        Ask the azure-core BearerTokenCredentialPolicy policy to get a token.

        Using the policy gives us for free the caching system of azure-core.
        We could make this code simpler by using private method, but by definition
        I can't assure they will be there forever, so mocking a fake call to the policy
        to extract the token, using 100% public API.

        """
        request = _make_request()
        self._policy.on_request(request)
        # Read Authorization, and get the second part after Bearer
        token = request.http_request.headers["Authorization"].split(" ", 1)[1]
        print(token)
        self.token = {"access_token": token}

    def signed_session(self, session=None):
        """Wrap signed session object."""
        self.set_token()
        return super(CredentialWrapper, self).signed_session(session)


def _make_request():
    """Make mocked request to get token."""
    return PipelineRequest(
        HttpRequest("CredentialWrapper", "https://fakeurl"), PipelineContext(None)
    )


def az_connect(client_id: str = None, tenant_id: str = None, secret: str = None):
    """Authenticate with the SDK."""
    # Use details of msticpyyaml if not provided
    #if client_id is None and tenant_id is None and secret is None:
        #data_provs = get_provider_settings(config_section="DataProviders")
        #az_cli_config = data_provs.get("AzureCLI")
        # az_cli_config = config.settings.get("AzureCLI")
        # if not az_cli_config:
        #    raise MsticpyAzureConfigError(
        #        "No AzureCLI section found in configuration settings.",
        #        title="no AzureCLI settings available.",
        #    )
        #try:
        #    config_items = az_cli_config.args
        #    os.environ["AZURE_CLIENT_ID"] = config_items["clientId"]
        #    os.environ["AZURE_TENANT_ID"] = config_items["tenantId"]
        #    os.environ["AZURE_CLIENT_SECRET"] = config_items["clientSecret"]
        #except:
        #    pass
        # except KeyError as key_err:
        #    key_name = key_err.args[0]
        #    raise MsticpyAzureConfigError(
        #        f"{key_name} is missing from AzureCLI section in your",
        #        "configuration.",
        #        title="missing f{key_name} settings for AzureCLI.",
        #   ) from key_err
    # Create credentials and connect to the subscription client to validate
    env = EnvironmentCredential()
    cli = AzureCliCredential()
    interactive = InteractiveBrowserCredential()
    mi = ManagedIdentityCredential
    handler = logging.StreamHandler(sys.stdout)
    handler.addFilter(filter_credential_warning)
    logging.basicConfig(level=logging.WARNING, handlers=[handler])
    creds = ChainedTokenCredential(env, cli, mi, interactive)
    legacy_creds = CredentialWrapper(creds)
    if not creds:
        raise CloudError("Could not obtain credentials.")
    sub_client = SubscriptionClient(creds)
    if not sub_client:
        raise CloudError("Could not create a Subscription client.")

    credentials = namedtuple("credentials", ["legacy", "modern"])

    return credentials(legacy_creds, creds)


def filter_credential_warning(record):
    if record.name.startswith("azure.identity") and record.levelno == logging.WARNING: 
        message = record.getMessage()
        if ".get_token" in message:
            if message.startswith("EnvironmentCredential"):
                print("Attempting to sign-in with environment variable credentials...")
            if message.startswith("AzureCliCredential"):
                print("Attempting to sign-in with Azure CLI credentials...")
                #print("Using interactive logon...")
                print("Attempting MSI...")
        return not message
    return True
