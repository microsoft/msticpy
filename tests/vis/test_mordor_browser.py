# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mordor browser notebook test."""
import os
from pathlib import Path

import pytest

from ..unit_test_lib import exec_notebook, get_test_data_path

__author__ = "Ian Hellen"

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "MordorData.ipynb"
_MP_CONFIG_PATH = get_test_data_path().parent.joinpath("msticpyconfig-test.yaml")


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_mordor_browser():
    """Mordor browser UI Test."""
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    abs_path = Path(_NB_FOLDER).absolute()

    ex_json = list(abs_path.glob("**/*.json"))
    ex_zip = list(abs_path.glob("**/*.zip"))

    try:
        exec_notebook(nb_path=nb_path, mp_config=_MP_CONFIG_PATH)
    finally:
        # Data file cleanup
        for j_file in abs_path.glob("**/*.json"):
            if j_file not in ex_json and j_file.is_file():
                j_file.unlink()
        for z_file in abs_path.glob("**/*.zip"):
            if z_file not in ex_zip and z_file.is_file():
                z_file.unlink()
        for file in abs_path.joinpath("mordor").glob("*"):
            file.unlink()
        # pylint: disable=broad-except
        try:
            abs_path.joinpath("mordor").rmdir()
        except Exception:  # nosec
            pass
