# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test nb_init links."""
import datetime
from datetime import timedelta
import os
import subprocess  # nosec

import pandas as pd
import pytest_check as check

from msticpy.nbtools.nbinit import init_notebook, _check_config, _imp_module_all

from ..unit_test_lib import TEST_DATA_PATH


def test_nbinit_no_params():
    """Test init_notebook defaults."""
    ns_dict = {}
    init_notebook(namespace=ns_dict, def_imports="nb")

    check.is_in("pd", ns_dict)
    check.is_in("get_ipython", ns_dict)
    check.is_in("Path", ns_dict)
    check.is_in("np", ns_dict)

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
    )

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


def test_check_config():
    """Test config check."""
    mp_var = os.environ.get("MSTICPYCONFIG")
    mp_file = TEST_DATA_PATH + "/msticpyconfig.yaml"
    os.environ["MSTICPYCONFIG"] = mp_file
    result, err_warn = _check_config()
    if not result:
        # If failed - err_warn should be set
        # and item 0 should be populated with errors
        check.is_not_none(err_warn)
        check.is_true(err_warn[0])
    else:
        # otherwise we have no errors or warnings or
        # just warnings
        if err_warn:
            check.is_false(err_warn[0])
            check.is_true(err_warn[1])
    os.environ["MSTICPYCONFIG"] = mp_var


def test_install_pkgs():
    """Test installing and importing a package."""
    test_pkg = "pip-install-test"

    # Uninstall package if it is already there
    subprocess.run(["pip", "uninstall", "-y", test_pkg], check=True)  # nosec

    ns_dict = {}
    init_notebook(
        namespace=ns_dict,
        additional_packages=[test_pkg],
        extra_imports=["pip_install_test"],
        def_imports="nb",
    )

    for name, obj in ns_dict.items():
        print(name, type(obj))
    check.is_in("pip_install_test", ns_dict)

    subprocess.run(["pip", "uninstall", "-y", test_pkg], check=True)  # nosec
