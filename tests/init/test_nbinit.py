# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test nb_init links."""
import datetime
import os
import subprocess  # nosec
from datetime import timedelta
from enum import Enum
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.init import azure_ml_tools, nbinit
from msticpy.init.nbinit import _get_or_create_config, _imp_module_all, init_notebook

from ..unit_test_lib import TEST_DATA_PATH, custom_mp_config


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_nbinit_no_params():
    """Test init_notebook defaults."""
    ns_dict = {}
    init_notebook(
        namespace=ns_dict,
        def_imports="nb",
        verbose=True,
    )

    check.is_in("get_ipython", ns_dict)
    check.is_in("Path", ns_dict)

    print(ns_dict.keys())
    # Note - msticpy imports throw when exec'd from unit test
    # e.g. check.is_in("QueryProvider", ns_dict) fails

    check.is_in("WIDGET_DEFAULTS", ns_dict)


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

    check.equal(timedelta, ns_dict["tdelta"])
    check.equal(datetime.time, ns_dict["time"])


def test_import_all():
    """Test import all function."""
    ns_dict = {}
    _imp_module_all(ns_dict, module_name="datetime")

    for imp in ["date", "datetime", "time", "timedelta", "timezone", "tzinfo"]:
        check.is_in(imp, ns_dict)


class SubDirCase(Enum):
    """Test enumeration for config folder."""

    NONE = 0
    MAIN_ENV_PTR = 1
    SAME_DIR = 2
    SEARCH = 3


_CONFIG_TESTS = [
    (("missing_file", None, SubDirCase.NONE), False),
    (
        ("msticpyconfig.yaml", None, SubDirCase.MAIN_ENV_PTR),
        True,
    ),
    (
        (
            "msticpyconfig-noAzSentSettings.yaml",
            None,
            SubDirCase.MAIN_ENV_PTR,
        ),
        False,
    ),
    (
        ("msticpyconfig-no-settings.yaml", None, SubDirCase.MAIN_ENV_PTR),
        False,
    ),
    (
        ("msticpyconfig.yaml", None, SubDirCase.SAME_DIR),
        True,
    ),
    (
        ("msticpyconfig-noAzSentSettings.yaml", None, SubDirCase.SAME_DIR),
        False,
    ),
    (
        ("msticpyconfig-no-settings.yaml", None, SubDirCase.SAME_DIR),
        False,
    ),
    (
        (None, "config.json", SubDirCase.SAME_DIR),
        True,
    ),
    (
        (None, "config.json", SubDirCase.SEARCH),
        True,
    ),
    (
        ("msticpyconfig.yaml", None, SubDirCase.SEARCH),
        False,
    ),
    (
        ("msticpyconfig-no-settings.yaml", None, SubDirCase.SEARCH),
        False,
    ),
    (
        (
            "msticpyconfig-noAzSentSettings.yaml",
            "config.json",
            SubDirCase.MAIN_ENV_PTR,
        ),
        True,
    ),
    (
        ("msticpyconfig-no-settings.yaml", "config.json", SubDirCase.MAIN_ENV_PTR),
        True,
    ),
    (
        (
            "msticpyconfig-noAzSentSettings.yaml",
            "config.json",
            SubDirCase.SAME_DIR,
        ),
        True,
    ),
    (
        ("msticpyconfig-no-settings.yaml", "config.json", SubDirCase.SAME_DIR),
        True,
    ),
    (
        (
            "msticpyconfig-noAzSentSettings.yaml",
            "config.json",
            SubDirCase.SEARCH,
        ),
        True,
    ),
    (
        ("msticpyconfig-no-settings.yaml", "config.json", SubDirCase.SEARCH),
        True,
    ),
]

_test_ids = [
    f"{test[0][0]}/{test[0][1]}-{test[0][2].name} => {'Success' if test[1] else 'Fail'}"
    for test in _CONFIG_TESTS
]


@pytest.mark.filterwarnings("ignore::UserWarning")
@pytest.mark.parametrize("conf_file, expected", _CONFIG_TESTS, ids=_test_ids)
def test_check_config(conf_file, expected, tmp_path, monkeypatch):
    """Test config check."""
    mpconf_file, conf_json, mp_location = conf_file
    init_cwd = str(Path(".").absolute())
    settings_file = "missing_file"
    for file in tmp_path.parent.glob("config.json"):
        file.unlink()
    for file in tmp_path.parent.glob("msticpyconfig.yaml"):
        file.unlink()
    try:
        # If we want to test against config files in isolated directory
        if mp_location != SubDirCase.NONE:
            # Read contents of source file
            for file in (mpconf_file, conf_json):
                if file is None:
                    continue
                tgt_file = Path(TEST_DATA_PATH).joinpath(file).name
                file_txt = (
                    Path(TEST_DATA_PATH).joinpath(file).read_text(encoding="utf-8")
                )

                dest_file = (
                    "config.json"
                    if tgt_file.endswith(".json")
                    else "msticpyconfig.yaml"
                )
                # write the file to the folder
                tmp_path.joinpath(dest_file).write_text(file_txt)
            cwd_path = str(tmp_path)
            # If sub-dir, change to the directory, so WorkspaceConfig has to search.
            if mp_location in (SubDirCase.MAIN_ENV_PTR, SubDirCase.SEARCH):
                cwd_path = tmp_path.joinpath("sub_folder")
                cwd_path.mkdir(parents=True, exist_ok=True)
            os.chdir(str(cwd_path))
            if mp_location == SubDirCase.SEARCH or mpconf_file is None:
                # Pass non-existing file to custom_mp_config to bypass default settings
                settings_file = "missing_file"
            else:
                settings_file = tmp_path.joinpath("msticpyconfig.yaml")
        else:
            os.chdir(str(tmp_path))

        # with custom_mp_config(settings_file, path_check=False):
        monkeypatch.setenv("MSTICPYCONFIG", str(settings_file))
        monkeypatch.setattr(nbinit, "current_config_path", lambda: None)
        monkeypatch.setattr(nbinit, "is_in_aml", lambda: True)
        monkeypatch.setattr(azure_ml_tools, "get_aml_user_folder", lambda: tmp_path)
        result = _get_or_create_config()

        print("result=", result)
        check.equal(result, expected, "Result")

    finally:
        os.chdir(init_cwd)


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
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
