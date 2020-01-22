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

from ..tools.toollib.import_analyzer import analyze_imports, PKG_TOKENS, _get_setup_reqs

PKG_ROOT = "."
PKG_NAME = "msticpy"
REQS_FILE = "requirements.txt"


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


def test_missing_pkgs_setup():

    mod_imports = analyze_imports(
        package_root=PKG_ROOT, package_name=PKG_NAME, req_file=REQS_FILE
    )
    reqs = set([v for s in mod_imports.values() for v in s.setup_reqs])
    assert reqs
    install_requires = _get_setup_module()
    setup_pkgs = {re.match(PKG_TOKENS, item).groups() for item in install_requires}
    setup_reqs = {key[0].lower(): key for key in setup_pkgs}
    missing_reqs = reqs - setup_reqs.keys()
    assert not missing_reqs


def test_reqs_match_setup():
    install_requires = _get_setup_module()

    setup_pkgs = {re.match(PKG_TOKENS, item).groups() for item in install_requires}
    setup_reqs = {key[0].lower(): key for key in setup_pkgs}

    reqtxt_reqs, reqtxt_ver = _get_setup_reqs(PKG_ROOT, req_file=REQS_FILE)

    setup_missing = setup_reqs.keys() - reqtxt_ver.keys()
    if setup_missing:
        print(f"Packages in REQS_FILE missing from setup.py")
    reqs_missing = reqtxt_ver.keys() - setup_reqs.keys()
    if setup_missing:
        print(f"Packages in setup.py missing from {REQS_FILE}")

    matched_pkgs = reqtxt_ver.keys() & setup_reqs.keys()
    if setup_reqs.values() != reqtxt_ver.values():
        print("Version mismatch for some packages:")
    for pkg in matched_pkgs:

        if setup_reqs[pkg] != reqtxt_ver[pkg]:
            print(
                setup_reqs[pkg],
                "(setup.py) does not match",
                reqtxt_ver[pkg],
                "(requirements)",
            )
            if setup_reqs[pkg][2] > reqtxt_ver[pkg][2]:
                print(
                    f" => Update requirements.txt with \"{''.join(setup_reqs[pkg])}\""
                )
            elif reqtxt_ver[pkg][2] > setup_reqs[pkg][2]:
                print(f" => Update setup.py with \"{''.join(reqtxt_ver[pkg])}\"")
            else:
                print("Other mismatch")
    assert not setup_missing
    assert not reqs_missing
    assert set(setup_reqs.values()) == set(reqtxt_ver.values())


def _get_setup_module():
    file_path = Path(PKG_ROOT) / "setup.py"

    module_name = "setup"

    spec = importlib.util.spec_from_file_location(module_name, file_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    # spec.loader.exec_module(module)
    try:
        spec.loader.exec_module(module)
    except:
        pass

    return module.INSTALL_REQUIRES
