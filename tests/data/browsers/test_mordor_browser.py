# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mordor browser notebook test."""
import os
from pathlib import Path

import nbformat
import pytest
from nbconvert.preprocessors import CellExecutionError, ExecutePreprocessor

__author__ = "Ian Hellen"

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "MordorData.ipynb"


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_mordor_browser():
    """Mordor browser UI Test."""
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    abs_path = Path(_NB_FOLDER).absolute()
    with open(nb_path) as f_hdl:
        nbk = nbformat.read(f_hdl, as_version=4)
    nb_exec = ExecutePreprocessor(timeout=600, kernel_name="python3")

    try:
        nb_exec.preprocess(nbk, {"metadata": {"path": abs_path}})
    except CellExecutionError:
        nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
        msg = f"Error executing the notebook '{nb_path}'.\n"
        msg += f"See notebook '{nb_err}' for the traceback."
        print(msg)
        with open(nb_err, mode="w", encoding="utf-8") as f:
            nbformat.write(nbk, f)
        raise
