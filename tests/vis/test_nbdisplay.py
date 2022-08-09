# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os
import unittest
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.vis.nbdisplay import display_logon_data

from ..unit_test_lib import exec_notebook, get_test_data_path

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "EventClustering.ipynb"
_MP_CONFIG_PATH = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")


class Testnbdisplay(unittest.TestCase):
    """Unit test class."""

    @pytest.mark.skipif(
        not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
    )
    def test_clustering_nbdisplay_notebook(self):
        """Test a notebook that uses nbdisplay."""
        nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
        exec_notebook(nb_path=nb_path, mp_config=_MP_CONFIG_PATH)


def test_display_logon():
    """Test Logon display code."""
    win_logon_df = pd.read_csv(
        str(get_test_data_path().joinpath("host_logons.csv")),
        index_col=0,
        parse_dates=["TimeGenerated"],
    )

    check.is_not_none(win_logon_df)
    display_logon_data(win_logon_df)
    display_logon_data(win_logon_df.iloc[0])
