# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Credential wrapper to expose ADAL and MSAL credentials."""
from msrest.authentication import BasicTokenAuthentication
from azure.core.pipeline.policies import BearerTokenCredentialPolicy
from azure.core.pipeline import PipelineRequest, PipelineContext
from azure.core.pipeline.transport import HttpRequest
from azure.identity import DefaultAzureCredential

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
            The scope to use to get the token, by default
            "https://management.azure.com/.default"

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
