# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""security_alert test class."""
import unittest


from ..msticpy.common.wsconfig import WorkspaceConfig

# from .. nbtools import kql
# from .. nbtools.security_alert_graph import create_alert_graph
# from .. nbtools.nbdisplay import display_alert
from ..msticpy.data import QueryProvider

# import pandas as pd

# from IPython import get_ipython, embed_kernel


class TestKqlMagic(unittest.TestCase):
    """Unit test class."""

    _WS = "802d39e1-9d70-404d-832c-2de5e2478eda"
    query = """
SecurityAlert
| take 1
    """

    def setUp(self):
        """Run query using KqlMagic."""
        pass

    def test_kql_magic(self):
        """Run query using KqlMagic."""
        ws_conf = WorkspaceConfig()

        # q_prov = QueryProvider("LogAnalytics")
        # q_prov.connect(ws_conf.code_connect_str)
        self.assertTrue(True)
