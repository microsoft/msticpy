# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
import os
import warnings
from pathlib import Path

import nbformat
import notebook
import pytest
import pytest_check as check

from nbconvert.preprocessors import CellExecutionError, ExecutePreprocessor
from msticpy.sectools.geoip import GeoLiteLookup, IPStackLookup

_NB_FOLDER = "docs/notebooks"
_NB_NAME = "GeoIPLookups.ipynb"


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_geoip_notebook():
    nb_path = Path(_NB_FOLDER).joinpath(_NB_NAME)
    abs_path = Path(_NB_FOLDER).absolute()

    warnings.warn(
        "Test needs to be renabled after IPStack intermittent problems resolved"
    )

    with open(nb_path, "rb") as f:
        nb_bytes = f.read()
    nb_text = nb_bytes.decode("utf-8")
    nb = nbformat.reads(nb_text, as_version=4)
    # ep = ExecutePreprocessor(timeout=600, kernel_name="python3")

    try:
        # ep.preprocess(nb, {"metadata": {"path": abs_path}})
        print("GeoIP NB test skipped")
    except CellExecutionError:
        nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
        msg = f"Error executing the notebook '{nb_path}'.\n"
        msg += f"See notebook '{nb_err}' for the traceback."
        print(msg)
        with open(nb_err, mode="w", encoding="utf-8") as f:
            nbformat.write(nb, f)
        raise


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_geoiplite_download():
    """Test forced download of GeoIPLite DB."""
    test_folder = "test_geolite_data"
    tgt_folder = Path(test_folder).resolve()
    try:
        tgt_folder.mkdir(exist_ok=True)
        with pytest.warns(None) as warning_record:
            iplocation = GeoLiteLookup(db_folder=str(tgt_folder), force_update=True)
            iplocation.close()
        check.equal(len(warning_record), 0)
    finally:
        if tgt_folder.exists():
            for file in tgt_folder.glob("*"):
                file.unlink()
            tgt_folder.rmdir()
