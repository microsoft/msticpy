# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test Text to DF magic."""
import pandas as pd
import pytest_check as check

from msticpy.init.pivot_core.pivot_magic_core import run_txt2df

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name

_INPUT_TEST = """
file, lines1, lines2, lines3, percent
msticpy___init__.py, 24, 12, 0, 50%
msticpy__version.py, 1, 0, 0, 100%
msticpy_analysis___init__.py, 3, 0, 0, 100%
msticpy_analysis_anomalous_sequence___init__.py, 3, 0, 0, 100%
msticpy_analysis_anomalous_sequence_anomalous.py, 34, 26, 0, 24%
msticpy_analysis_anomalous_sequence_model.py, 222, 196, 0, 12%
msticpy_analysis_anomalous_sequence_sessionize.py, 59, 52, 0, 12%
msticpy_analysis_anomalous_sequence_utils___init__.py, 3, 0, 0, 100%
msticpy_analysis_anomalous_sequence_utils_cmds_only.py, 76, 63, 0, 17%
msticpy_analysis_anomalous_sequence_utils_cmds_params_only.py, 105, 91, 0, 13%
msticpy_analysis_anomalous_sequence_utils_cmds_params_values.py, 126, 111, 0, 12%
msticpy_analysis_anomalous_sequence_utils_data_structures.py, 27, 16, 0, 41%
msticpy_analysis_anomalous_sequence_utils_laplace_smooth.py, 34, 28, 0, 18%
msticpy_analysis_anomalous_sequence_utils_probabilities.py, 42, 35, 0, 17%
"""

# Magic args
# "--sep",
# "--name",
# "--headers",
# "--keepna",


def test_txt2df():
    """Test txt2df magic function."""
    res_df = run_txt2df(line="", cell=_INPUT_TEST, local_ns=None)
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (15, 5))

    # headers
    res_df = run_txt2df(line="--headers", cell=_INPUT_TEST, local_ns=None)
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (14, 5))
    for col in _INPUT_TEST.split("\n")[1].split(","):
        check.is_in(col.strip(), list(res_df.columns))

    # separator
    res_df = run_txt2df(
        line="--headers --sep=\t", cell=_INPUT_TEST.replace(",", "\t"), local_ns=None
    )
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (14, 5))

    # some malformed lines
    cell_input = []
    for idx, line in enumerate(_INPUT_TEST.split("\n")):
        if line and idx % 5 != 3:
            cell_input.append(line + ",")
        else:
            cell_input.append(line)
    res_df = run_txt2df(line="--headers", cell="\n".join(cell_input), local_ns=None)
    # expect output with dropped columns
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (14, 5))

    res_df = run_txt2df(
        line="--headers --keepna", cell="\n".join(cell_input), local_ns=None
    )
    # expect output with no dropped columns
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (14, 6))

    # add extra delimiters for 2 empty columns
    cell_input = [line + ", ," for line in _INPUT_TEST.split("\n") if line]
    res_df = run_txt2df(
        line="--headers --keepna", cell="\n".join(cell_input), local_ns=None
    )
    # expect output with cols following header row
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (14, 7))
    for col in ("Unnamed: 5", "Unnamed: 6"):
        check.is_in(col.strip(), list(res_df.columns))

    # keepna should force blank columns to remain
    res_df = run_txt2df(line="--keepna", cell="\n".join(cell_input), local_ns=None)
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (15, 7))

    # name
    namespace = {}
    res_df = run_txt2df(
        line="--headers --name=my_df", cell=_INPUT_TEST, local_ns=namespace
    )
    check.is_instance(res_df, pd.DataFrame)
    check.equal(res_df.shape, (14, 5))
    check.is_in("my_df", namespace)
    check.is_instance(namespace["my_df"], pd.DataFrame)
    check.equal(namespace["my_df"].shape, (14, 5))
