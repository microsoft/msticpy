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
import pkg_resources

import pytest
import pytest_check as check

from tools.toollib.import_analyzer import analyze_imports, get_setup_reqs

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
    return get_setup_reqs(PKG_ROOT, skip_setup=False)[1]


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
    conda_reqs_file = Path(PKG_ROOT) / "conda/conda-reqs.txt"
    conda_reqs_pip_file = Path(PKG_ROOT) / "conda/conda-reqs-pip.txt"

    # main_reqs_dict = _get_reqs_from_file(main_reqs_file)
    # # Add extras
    # for item in [re.split(REQS_OP_RGX, line) for line in extras_from_setup]:
    #     main_reqs_dict[item[0].strip()] = item[1].strip() if len(item) > 1 else None

    conda_reqs_dict = _get_reqs_from_file(conda_reqs_file)
    conda_reqs_pip_dict = _get_reqs_from_file(conda_reqs_pip_file)

    for pkg_name, pkg_specifier in extras_from_setup.items():
        # print(f"Checking {key} in conda-reqs.txt", bool(key in conda_reqs_dict))
        # print(f"Checking {key} in conda-reqs-pip.txt", bool(key in conda_reqs_pip_dict))

        if (
            pkg_name not in conda_reqs_dict
            and pkg_name not in conda_reqs_pip_dict
            and pkg_name not in CONDA_PKG_EXCEPTIONS
        ):
            print(
                f"Test Error - no conda package equiv for {pkg_name}=={pkg_specifier}"
            )
        check.is_true(
            pkg_name in conda_reqs_dict
            or pkg_name in conda_reqs_pip_dict
            or pkg_name in CONDA_PKG_EXCEPTIONS,
        )
        if pkg_name in conda_reqs_dict:
            if conda_reqs_dict[pkg_name]:
                if pkg_specifier != conda_reqs_dict[pkg_name]:
                    print(
                        f"{pkg_name} version mismatch - setup: {pkg_specifier}: {conda_reqs_dict[pkg_name]}",
                        "in conda-reqs.txt",
                    )
                check.equal(
                    pkg_specifier,
                    conda_reqs_dict[pkg_name],
                    f"{pkg_name} in condas reqs",
                )
            conda_reqs_dict.pop(pkg_name)
        if pkg_name in conda_reqs_pip_dict:
            if conda_reqs_pip_dict[pkg_name]:
                if pkg_specifier != conda_reqs_pip_dict[pkg_name]:
                    print(
                        f"{pkg_name} version mismatch - setup: {pkg_specifier}: {conda_reqs_pip_dict[pkg_name]}",
                        "in conda-reqs-pip.txt",
                    )
                check.equal(
                    pkg_specifier,
                    conda_reqs_pip_dict[pkg_name],
                    f"{pkg_name} in condas pip reqs",
                )
            conda_reqs_pip_dict.pop(pkg_name)

    if conda_reqs_dict:
        print("Extra items found in conda-reqs.txt", conda_reqs_pip_dict)
    check.is_false(conda_reqs_dict, "no extra items in conda-reqs.txt")
    if conda_reqs_pip_dict:
        print("Extra items found in conda-reqs-pip.txt", conda_reqs_dict)
    check.is_false(conda_reqs_pip_dict, "no extra items in conda-reqs-pip.txt")


def _get_reqs_from_file(reqs_file):
    with open(str(reqs_file), "r") as f_hdl:
        reqs_lines = f_hdl.readlines()
    reqs = [
        pkg_resources.Requirement.parse(req)
        for req in reqs_lines
        if req.strip() and not req.strip().startswith("#")
    ]
    return {req.name.casefold(): req.specifier for req in reqs}
