# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""morph_charts test class."""
import os
from pathlib import Path
from unittest.mock import patch, call

import pytest
import IPython
import pandas as pd

from msticpy.common.exceptions import MsticpyException
from msticpy.nbtools.morph_charts import MorphCharts


_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"

test_morph = MorphCharts()


class TestMorph:
    """Pytest test class."""

    @patch("builtins.print")
    def test_chart_details(self, mocked_print):
        with pytest.raises(KeyError):
            assert test_morph.get_chart_details("xxx")
        test_morph.get_chart_details("SigninsChart")
        assert mocked_print.mock_calls == [
            call(
                "SigninsChart",
                ":",
                "\n",
                "Charts for visualizing Azure AD Signin Logs.",
                "\n",
                "Query: ",
                "Azure.list_all_signins_geo",
            )
        ]

    @patch("builtins.print")
    def test_list_charts(self, mocked_print):
        test_morph.list_charts()
        assert mocked_print.mock_calls == [call("SigninsChart")]

    @patch("builtins.print")
    def test_search_charts_f(self, mocked_print):
        test_morph.search_charts("testing")
        assert mocked_print.mock_calls == [call("No matching charts found")]

    @patch("builtins.print")
    def test_search_charts_s(self, mocked_print):
        test_morph.search_charts("signinLogs")
        assert mocked_print.mock_calls == [
            call(
                "SigninsChart",
                ":",
                "\n",
                "Charts for visualizing Azure AD Signin Logs.",
            )
        ]

    def test_display(self):
        test_file = Path(_TEST_DATA).joinpath("morph_test.csv")
        test_data = pd.read_csv(test_file, index_col=0)
        output = test_morph.display(data=test_data, chart_name="SigninsChart")
        assert isinstance(output, IPython.lib.display.IFrame)
        assert os.path.isdir(Path.cwd().joinpath("morphchart_package")) is True
        assert (
            os.path.isfile(
                Path.cwd().joinpath(*["morphchart_package", "description.json"])
            )
            is True
        )
        assert (
            os.path.isfile(
                Path.cwd().joinpath(*["morphchart_package", "query_data.csv"])
            )
            is True
        )
        with pytest.raises(MsticpyException):
            assert test_morph.display(data=test_data, chart_name="test")
            assert test_morph.display(data="test_data", chart_name="SigninsChart")
