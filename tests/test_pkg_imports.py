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

from tools.toollib.import_analyzer import analyze_imports, get_extras_from_setup

PKG_ROOT = "."
PKG_NAME = "msticpy"
REQS_FILE = "requirements.txt"
REQS_OP_RGX = r"[=<>~!\s]+"

EXTRAS_EXCEPTIONS = {
    "vt",
    "vt_graph_api",
    "bs4",
    "seaborn",
    "msticpy",
    "msticnb",
    "pyperclip",
}
CONDA_PKG_EXCEPTIONS = {
    "vt-py",
    "vt-graph-api",
    "nest_asyncio",
    "KqlmagicCustom[jupyter-extended]",
    "sumologic-sdk",
    "openpyxl",
}


@pytest.fixture(scope="module")
def extras_from_setup():
    """Read extras packages from setup.py."""
    return get_extras_from_setup(PKG_ROOT, extra="all")


def test_missing_pkgs_req():
    """Check for packages used in code but not in requirements.txt."""
    mod_imports = analyze_imports(
        package_root=PKG_ROOT, package_name=PKG_NAME, req_file=REQS_FILE
    )
    import_errs = {v for s in mod_imports.values() for v in s.unknown}
    print("re module path:", re.__file__)
    print("Import errors:\n", import_errs)
    stdlib_paths = {
        p
        for p in sys.path
        if p.lower().startswith(sys.prefix.lower()) and "site-packages" not in p
    }
    print("sys.path", sys.path)
    print("sys.prefix", sys.prefix)
    print("Stdlib paths:\b", stdlib_paths)

    missing_req_mod = {
        f"{req}:{mod}" for mod, reqs in mod_imports.items() for req in reqs.missing_reqs
    }
    missing_reqs = {
        req.strip() for reqs in mod_imports.values() for req in reqs.missing_reqs
    }
    missing_reqs = missing_reqs - EXTRAS_EXCEPTIONS
    if missing_reqs:
        print(
            "Missing packages:\n",
            "\n".join(
                req for req in missing_req_mod if req.split(":")[0] in missing_reqs
            ),
        )
    check.is_false(missing_reqs)


def test_conda_reqs(extras_from_setup):
    """Test conda requirements files match main file."""
    main_reqs_file = Path(PKG_ROOT) / REQS_FILE
    conda_reqs_file = Path(PKG_ROOT) / "conda/conda-reqs.txt"
    conda_reqs_pip_file = Path(PKG_ROOT) / "conda/conda-reqs-pip.txt"

    main_reqs_dict = _get_reqs_from_file(main_reqs_file)
    # Add extras
    for item in [re.split(REQS_OP_RGX, line) for line in extras_from_setup]:
        main_reqs_dict[item[0].strip()] = item[1].strip() if len(item) > 1 else None

    conda_reqs_dict = _get_reqs_from_file(conda_reqs_file)
    conda_reqs_pip_dict = _get_reqs_from_file(conda_reqs_pip_file)

    for key, val in main_reqs_dict.items():
        # print(f"Checking {key} in conda-reqs.txt", bool(key in conda_reqs_dict))
        # print(f"Checking {key} in conda-reqs-pip.txt", bool(key in conda_reqs_pip_dict))

        if (
            key not in conda_reqs_dict
            and key not in conda_reqs_pip_dict
            and key not in CONDA_PKG_EXCEPTIONS
        ):
            print(f"Test Error - no conda package equiv for {key}=={val}")
        check.is_true(
            key in conda_reqs_dict
            or key in conda_reqs_pip_dict
            or key in CONDA_PKG_EXCEPTIONS,
        )
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


def _get_reqs_from_file(reqs_file):
    conda_reqs_dict = {}
    with open(str(reqs_file), "r") as f_hdl:
        reqs = f_hdl.readlines()
        lines = [line for line in reqs if not line.strip().startswith("#")]
        for item in [re.split(REQS_OP_RGX, line) for line in lines]:
            conda_reqs_dict[item[0].strip()] = (
                item[1].strip() if len(item) > 1 else None
            )
    return conda_reqs_dict
