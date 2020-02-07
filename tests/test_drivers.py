# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
from ..msticpy.data.drivers import MDATPDriver, SecurityGraphDriver


def test_MDATP():
    mdatp = MDATPDriver()
    assert type(mdatp) == MDATPDriver


def test_SecurityGraph():
    sec_graph = SecurityGraphDriver()
    assert type(sec_graph) == SecurityGraphDriver
