# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test nb_init links."""
from collections import namedtuple
from enum import Enum
import datetime
from datetime import timedelta
from pathlib import Path
import os
import subprocess  # nosec

import pandas as pd
import pytest
import pytest_check as check

from msticpy.nbtools.nbinit import init_notebook, _check_config, _imp_module_all

from ..unit_test_lib import TEST_DATA_PATH, custom_mp_config


def test_nbinit_no_params():
    """Test init_notebook defaults."""
    ns_dict = {}
    init_notebook(
        namespace=ns_dict,
        def_imports="nb",
        verbose=True,
    )

    check.is_in("pd", ns_dict)
    check.is_in("get_ipython", ns_dict)
    check.is_in("Path", ns_dict)
    check.is_in("np", ns_dict)

    print(ns_dict.keys())
    # Note - msticpy imports throw when exec'd from unit test
    # e.g. check.is_in("QueryProvider", ns_dict) fails

    check.is_in("WIDGET_DEFAULTS", ns_dict)

    check.equal(ns_dict["pd"].__name__, "pandas")
    check.equal(ns_dict["np"].__name__, "numpy")

    check.equal(pd.get_option("display.max_columns"), 50)


def test_nbinit_imports():
    """Test custom imports."""
    ns_dict = {}
    init_notebook(
        namespace=ns_dict,
        extra_imports=["pathlib", "datetime, time", "datetime, timedelta, tdelta"],
        def_imports="nb",
        verbose=True,
    )
    print(ns_dict.keys())

    check.is_in("pathlib", ns_dict)
    check.is_in("time", ns_dict)
    check.is_in("tdelta", ns_dict)
    check.is_in("np", ns_dict)

    check.equal(timedelta, ns_dict["tdelta"])
    check.equal(datetime.time, ns_dict["time"])


def test_import_all():
    """Test import all function."""
    ns_dict = {}
    _imp_module_all(ns_dict, module_name="datetime")

    for imp in ["date", "datetime", "time", "timedelta", "timezone", "tzinfo"]:
        check.is_in(imp, ns_dict)


class TestSubdir(Enum):
    """Test enumeration for config folder."""

    NONE = 0
    MAIN_ENV_PTR = 1
    SAME_DIR = 2
    SEARCH = 3


ConfExpd = namedtuple("ConfExpd", "res, errs, wrns")


_CONFIG_TESTS = [
    (("missing_file", TestSubdir.NONE), ConfExpd(True, 0, 1)),
    (
        (TEST_DATA_PATH + "/msticpyconfig.yaml", TestSubdir.MAIN_ENV_PTR),
        ConfExpd(True, 3, 0),
    ),
    (
        (
            TEST_DATA_PATH + "/msticpyconfig-noAzSentSettings.yaml",
            TestSubdir.MAIN_ENV_PTR,
        ),
        ConfExpd(False, 4, 0),
    ),
    (
        (TEST_DATA_PATH + "/msticpyconfig-no-settings.yaml", TestSubdir.MAIN_ENV_PTR),
        ConfExpd(False, 4, 0),
    ),
    (
        (TEST_DATA_PATH + "/msticpyconfig.yaml", TestSubdir.SAME_DIR),
        ConfExpd(True, 3, 0),
    ),
    (
        (TEST_DATA_PATH + "/msticpyconfig-noAzSentSettings.yaml", TestSubdir.SAME_DIR),
        ConfExpd(False, 4, 0),
    ),
    (
        (TEST_DATA_PATH + "/msticpyconfig-no-settings.yaml", TestSubdir.SAME_DIR),
        ConfExpd(False, 4, 0),
    ),
    (
        (TEST_DATA_PATH + "/config.json", TestSubdir.SAME_DIR),
        ConfExpd(True, 1, 2),
    ),
    (
        (TEST_DATA_PATH + "/config-no-settings.json", TestSubdir.SAME_DIR),
        ConfExpd(False, 2, 2),
    ),
    (
        (TEST_DATA_PATH + "/config.json", TestSubdir.SEARCH),
        ConfExpd(True, 0, 1),
    ),
    (
        (TEST_DATA_PATH + "/config-no-settings.json", TestSubdir.SEARCH),
        ConfExpd(False, 1, 1),
    ),
    (
        (TEST_DATA_PATH + "/msticpyconfig.yaml", TestSubdir.SEARCH),
        ConfExpd(True, 0, 1),
    ),
    (
        (TEST_DATA_PATH + "/msticpyconfig-no-settings.yaml", TestSubdir.SEARCH),
        ConfExpd(False, 1, 1),
    ),
]

_test_ids = [f"{test[0][0]}-{test[0][1].name}" for test in _CONFIG_TESTS]


@pytest.mark.parametrize("conf_file, expected", _CONFIG_TESTS, ids=_test_ids)
def test_check_config(conf_file, expected, tmpdir, capsys):
    """Test config check."""
    conf_file, mp_location = conf_file
    settings_file = conf_file
    init_cwd = str(Path(".").absolute())
    try:
        # If we want to test against config files in isolated directory
        if mp_location != TestSubdir.NONE:
            # Read contents of source file
            tgt_file = Path(conf_file).name
            file_txt = Path(conf_file).read_text()

            dest_file = (
                "config.json" if tgt_file.endswith(".json") else "msticpyconfig.yaml"
            )
            # write the file to the folder
            tmpdir.join(dest_file).write(file_txt)
            cwd_path = tmpdir
            # If sub-dir, change to the directory, so WorkspaceConfig has to search.
            if mp_location in (TestSubdir.MAIN_ENV_PTR, TestSubdir.SEARCH):
                cwd_path = tmpdir.mkdir("sub_folder")
            os.chdir(cwd_path)
            if mp_location == TestSubdir.SEARCH:
                # Pass non-existing file to custom_mp_config to bypass default settings
                settings_file = "missing_file"
            else:
                settings_file = Path(str(tmpdir)).joinpath(dest_file)
        with custom_mp_config(settings_file, path_check=False):
            if mp_location in (TestSubdir.SEARCH, TestSubdir.NONE):
                with pytest.warns(UserWarning):
                    result, (errs, warns) = _check_config()
            else:
                result, (errs, warns) = _check_config()

            check.equal(result, expected.res, "Result")
            check.equal(0 if not errs else len(errs), expected.errs, "Num errors")
            check.equal(0 if not warns else len(warns), expected.wrns, "Num warnings")
    finally:
        os.chdir(init_cwd)


def test_install_pkgs():
    """Test installing and importing a package."""
    test_pkg = "pip_install_test"
    test_imp = "pip_install_test, , test_pkg_import"

    # Uninstall package if it is already there
    subprocess.run(["pip", "uninstall", "-y", test_pkg], check=True)  # nosec

    ns_dict = {}
    init_notebook(
        namespace=ns_dict,
        additional_packages=[test_pkg],
        def_imports="nb",
        extra_imports=test_imp,
        verbose=True,
    )

    for name, obj in ns_dict.items():
        print(name, type(obj))
    check.is_in("test_pkg_import", ns_dict)
    print(ns_dict)

    subprocess.run(["pip", "uninstall", "-y", test_pkg], check=True)  # nosec
