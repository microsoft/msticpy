# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test package imports."""
import importlib
import re
import sys
from pathlib import Path

import pytest
import pytest_check as check

from tools.toollib.import_analyzer import analyze_imports

PKG_ROOT = "."
PKG_NAME = "msticpy"
REQS_FILE = "requirements.txt"
REQS_OP_RGX = r"[=<>~!\s]+"

EXTRAS_EXCEPTIONS = {"vt", "vt_graph_api", "bs4", "seaborn"}
CONDA_PKG_EXCEPTIONS = {"vt-py", "vt-graph-api", "nest_asyncio"}


@pytest.fixture(scope="module")
def extras_from_setup():
    """Read extras packages from setup.py."""
    setup_py = Path(PKG_ROOT) / "setup.py"

    setup_txt = None
    with open(setup_py, "+r") as f_handle:
        setup_txt = f_handle.read()

    srch_txt = "setuptools.setup("
    repl_txt = [
        "def fake_setup(*args, **kwargs):" "    pass",
        "",
        "fake_setup(",
    ]
    setup_txt = setup_txt.replace(srch_txt, "\n".join(repl_txt))

    neut_setup_py = Path(PKG_ROOT) / "msticpy/neut_setup.py"
    try:
        with open(neut_setup_py, "+w") as f_handle:
            f_handle.writelines(setup_txt)

        setup_mod = importlib.import_module("msticpy.neut_setup", "msticpy")
        return getattr(setup_mod, "EXTRAS").get("all")
    finally:
        neut_setup_py.unlink()


def test_missing_pkgs_req(extras_from_setup):
    """Check for packages used in code but not in requirements.txt."""
    extras = extras_from_setup
    mod_imports = analyze_imports(
        package_root=PKG_ROOT, package_name=PKG_NAME, req_file=REQS_FILE, extras=extras
    )
    import_errs = {v for s in mod_imports.values() for v in s.unknown}
    print("re module path:", re.__file__)
    print("Import errors:\n", import_errs)
    paths = {str(Path(p).resolve()) for p in sys.path}
    stdlib_paths = {
        p
        for p in paths
        if p.lower().startswith(sys.prefix.lower()) and "site-packages" not in p
    }
    print("sys.path", sys.path)
    print("paths", paths)
    print("sys.prefix", sys.prefix)
    print("Stdlib paths:\b", stdlib_paths)

    missing_reqs = {v for s in mod_imports.values() for v in s.missing_reqs}
    missing_reqs = missing_reqs - EXTRAS_EXCEPTIONS
    if missing_reqs:
        print("Missing packages:\n", "\n".join(missing_reqs))
    check.is_false(missing_reqs)


def test_conda_reqs(extras_from_setup):
    """Test conda requirements files match main file."""
    main_reqs_file = Path(PKG_ROOT) / REQS_FILE
    conda_reqs_file = Path(PKG_ROOT) / "conda/conda-reqs.txt"
    conda_reqs_pip_file = Path(PKG_ROOT) / "conda/conda-reqs-pip.txt"

    main_reqs_dict = {}
    with open(str(main_reqs_file), "r") as f_hdl:
        reqs = f_hdl.readlines()
        lines = [line for line in reqs if not line.strip().startswith("#")]
        for item in [re.split(REQS_OP_RGX, line) for line in lines]:
            main_reqs_dict[item[0].strip()] = item[1].strip() if len(item) > 1 else None
    # Add extras
    for item in [re.split(REQS_OP_RGX, line) for line in extras_from_setup]:
        main_reqs_dict[item[0].strip()] = item[1].strip() if len(item) > 1 else None

    conda_reqs_dict = {}
    with open(str(conda_reqs_file), "r") as f_hdl:
        reqs = f_hdl.readlines()
        lines = [line for line in reqs if not line.strip().startswith("#")]
        for item in [re.split(REQS_OP_RGX, line) for line in lines]:
            conda_reqs_dict[item[0].strip()] = (
                item[1].strip() if len(item) > 1 else None
            )

    conda_reqs_pip_dict = {}
    with open(str(conda_reqs_pip_file), "r") as f_hdl:
        reqs = f_hdl.readlines()
        lines = [line for line in reqs if not line.strip().startswith("#")]
        for item in [re.split(REQS_OP_RGX, line) for line in lines]:
            conda_reqs_pip_dict[item[0].strip()] = (
                item[1].strip() if len(item) > 1 else None
            )

    for key, val in main_reqs_dict.items():
        print(f"Checking {key} in conda-reqs.txt", bool(key in conda_reqs_dict))
        print(f"Checking {key} in conda-reqs-pip.txt", bool(key in conda_reqs_pip_dict))

        check.is_true(
            key in conda_reqs_dict
            or key in conda_reqs_pip_dict
            or key in CONDA_PKG_EXCEPTIONS,
        )
        if not (
            key in conda_reqs_dict
            or key in conda_reqs_pip_dict
            or key in CONDA_PKG_EXCEPTIONS,
        ):
            print("Test Error - no conda package equiv for {key}=={val}")
        if key in conda_reqs_dict:
            if conda_reqs_dict[key]:
                if val != conda_reqs_dict[key]:
                    print(
                        f"{key} version mismatch - setup: {val}: {conda_reqs_dict[key]}",
                        "in conda-reqs.txt",
                    )
                check.equal(val, conda_reqs_dict[key], f"{key} in condas reqs")
            conda_reqs_dict.pop(key)
        if key in conda_reqs_pip_dict:
            if conda_reqs_pip_dict[key]:
                if val != conda_reqs_pip_dict[key]:
                    print(
                        f"{key} version mismatch - setup: {val}: {conda_reqs_pip_dict[key]}",
                        "in conda-reqs-pip.txt",
                    )
                check.equal(val, conda_reqs_pip_dict[key], f"{key} in condas pip reqs")
            conda_reqs_pip_dict.pop(key)

    if conda_reqs_dict:
        print("Extra items found in conda-reqs.txt", conda_reqs_pip_dict)
    check.is_false(conda_reqs_dict, "no extra items in conda-reqs.txt")
    if conda_reqs_pip_dict:
        print("Extra items found in conda-reqs-pip.txt", conda_reqs_dict)
    check.is_false(conda_reqs_pip_dict, "no extra items in conda-reqs-pip.txt")
