# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test package imports."""
import importlib.util
import sys
import os
import re
from pathlib import Path

import pandas as pd

from tools.toollib.import_analyzer import analyze_imports, PKG_TOKENS, _get_setup_reqs

PKG_ROOT = "."
PKG_NAME = "msticpy"
REQS_FILE = "requirements.txt"
REQS_OP_RGX = r"[=<>~!]+"


def test_missing_pkgs_req():

    mod_imports = analyze_imports(
        package_root=PKG_ROOT, package_name=PKG_NAME, req_file=REQS_FILE
    )
    import_errs = set([v for s in mod_imports.values() for v in s.unknown])
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

    missing_reqs = set([v for s in mod_imports.values() for v in s.missing_reqs])
    if missing_reqs:
        print("Missing packages:\n", "\n".join(missing_reqs))
    assert not missing_reqs


def test_conda_reqs():
    main_reqs_file = file_path = Path(PKG_ROOT) / REQS_FILE
    conda_reqs_file = file_path = Path(PKG_ROOT) / "conda/conda-reqs.txt"
    conda_reqs_pip_file = file_path = Path(PKG_ROOT) / "conda/conda-reqs-pip.txt"

    main_reqs_dict = {}
    with open(str(main_reqs_file), "r") as f:
        reqs = f.readlines()
        for item in [re.split(REQS_OP_RGX, line) for line in reqs]:
            main_reqs_dict[item[0].strip()] = item[1].strip() if len(item) > 1 else None

    conda_reqs_dict = {}
    with open(str(conda_reqs_file), "r") as f:
        reqs = f.readlines()
        for item in [re.split(REQS_OP_RGX, line) for line in reqs]:
            conda_reqs_dict[item[0].strip()] = (
                item[1].strip() if len(item) > 1 else None
            )

    conda_reqs_pip_dict = {}
    with open(str(conda_reqs_pip_file), "r") as f:
        reqs = f.readlines()
        for item in [re.split(REQS_OP_RGX, line) for line in reqs]:
            conda_reqs_pip_dict[item[0].strip()] = (
                item[1].strip() if len(item) > 1 else None
            )

    for key, val in main_reqs_dict.items():
        print(f"Checking {key} in conda-reqs.txt", bool(key in conda_reqs_dict))
        print(f"Checking {key} in conda-reqs-pip.txt", bool(key in conda_reqs_pip_dict))
        assert key in conda_reqs_dict or key in conda_reqs_pip_dict
        if key in conda_reqs_dict:
            if conda_reqs_dict[key]:
                print(
                    f"Checking version {val} in conda-reqs.txt matches {conda_reqs_dict[key]}"
                )
                assert val == conda_reqs_dict[key]
            conda_reqs_dict.pop(key)
        if key in conda_reqs_pip_dict:
            if conda_reqs_pip_dict[key]:
                print(
                    f"Checking version {val} in conda-reqs-pip.txt matches {conda_reqs_pip_dict[key]}"
                )
                assert val == conda_reqs_pip_dict[key]
            conda_reqs_pip_dict.pop(key)

    print(f"Checking version no extra items in conda-reqs-pip.txt", conda_reqs_dict)
    assert not conda_reqs_dict
    print(f"Checking version no extra items in conda-reqs.txt", conda_reqs_pip_dict)
    assert not conda_reqs_pip_dict
