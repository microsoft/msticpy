# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""morph_charts test class."""
import os
from pathlib import Path
from unittest.mock import call, patch

import IPython
import pandas as pd
import pytest

from msticpy.common.exceptions import MsticpyException
from msticpy.vis.morph_charts import MorphCharts

from ..unit_test_lib import get_test_data_path

_TEST_DATA = get_test_data_path()


@pytest.fixture
def test_morph():
    """Create MorphCharts objcet."""
    return MorphCharts()


@patch("builtins.print")
def test_chart_details(mocked_print, test_morph):
    """Test case."""
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


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@patch("builtins.print")
def test_list_charts(mocked_print, test_morph):
    """Test case."""
    test_morph.list_charts()
    assert mocked_print.mock_calls == [call("SigninsChart")]


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@patch("builtins.print")
def test_search_charts_f(mocked_print, test_morph):
    """Test case."""
    test_morph.search_charts("testing")
    assert mocked_print.mock_calls == [call("No matching charts found")]


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
@patch("builtins.print")
def test_search_charts_s(mocked_print, test_morph):
    """Test case."""
    test_morph.search_charts("signinLogs")
    assert mocked_print.mock_calls == [
        call(
            "SigninsChart",
            ":",
            "\n",
            "Charts for visualizing Azure AD Signin Logs.",
        )
    ]


@pytest.mark.filterwarnings("ignore::DeprecationWarning")
def test_display(test_morph):
    """Test case."""
    test_file = Path(_TEST_DATA).joinpath("morph_test.csv")
    test_data = pd.read_csv(test_file, index_col=0)
    output = test_morph.display(data=test_data, chart_name="SigninsChart")
    assert isinstance(output, IPython.lib.display.IFrame)
    assert os.path.isdir(Path.cwd().joinpath("morphchart_package")) is True
    assert (
        os.path.isfile(Path.cwd().joinpath(*["morphchart_package", "description.json"]))
        is True
    )
    assert (
        os.path.isfile(Path.cwd().joinpath(*["morphchart_package", "query_data.csv"]))
        is True
    )
    with pytest.raises(MsticpyException):
        assert test_morph.display(data=test_data, chart_name="test")
        assert test_morph.display(data="test_data", chart_name="SigninsChart")
