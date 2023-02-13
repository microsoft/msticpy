# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Sentinel test fixtures."""
from unittest.mock import patch

import pytest

from msticpy import VERSION
from msticpy.context.azure import MicrosoftSentinel

__version__ = VERSION
__author__ = "Ian Hellen"

# pylint: disable=protected-access


def _set_default_workspace(self, sub_id, workspace=None):
    """Mock set_default_workspace for MSSentinel."""
    del sub_id, workspace
    self._default_workspace = (
        "WSName",
        "/subscriptions/cd928da3-dcde-42a3-aad7-d2a1268c2f48/"
        "resourceGroups/RG/providers/"
        "Microsoft.OperationalInsights/workspaces/WSName",
    )


@pytest.fixture
@patch(f"{MicrosoftSentinel.__module__}.get_token")
@patch(f"{MicrosoftSentinel.__module__}.AzureData.connect")
def sent_loader(mock_creds, get_token, monkeypatch):
    """Generate MicrosoftSentinel instance for testing."""
    monkeypatch.setattr(
        MicrosoftSentinel, "set_default_workspace", _set_default_workspace
    )
    mock_creds.return_value = None
    get_token.return_value = "fd09863b-5cec-4833-ab9c-330ad07b0c1a"
    sent = MicrosoftSentinel(
        sub_id="fd09863b-5cec-4833-ab9c-330ad07b0c1a", res_grp="RG", ws_name="WSName"
    )
    sent.connect()
    sent.connected = True
    sent.token = "fd09863b-5cec-4833-ab9c-330ad07b0c1a"
    return sent
