# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test package imports."""

from __future__ import annotations

import pytest_check as check

from tools.toollib.import_analyzer import analyze_imports

PKG_ROOT = "."
PKG_NAME = "msticpy"
REQS_FILE = "requirements.txt"

EXTRAS_EXCEPTIONS = {
    "vt",
    "vt_graph_api",
    "bs4",
    "seaborn",
    "msticpy",
    "msticnb",
    "pygeohash",
    "pyperclip",
    "autogen",
    "importlib_resources",
    "importlib_metadata",
    "notebookutils",
    "distutils",
}


def test_missing_pkgs_req():
    """Check for packages used in code but not in requirements.txt."""
    mod_imports = analyze_imports(
        package_root=PKG_ROOT, package_name=PKG_NAME, req_file=REQS_FILE
    )
    import_errs = {v for s in mod_imports.values() for v in s.unknown}
    print("Import errors:\n", import_errs)

    missing_req_mod = {
        f"{req}:{mod}" for mod, reqs in mod_imports.items() for req in reqs.missing_reqs
    }
    missing_reqs = {req.strip() for reqs in mod_imports.values() for req in reqs.missing_reqs}
    # Remove any missing modules that part of an extra
    missing_reqs = {
        req for req in missing_reqs if not any(req.startswith(p) for p in EXTRAS_EXCEPTIONS)
    }
    if missing_reqs:
        print(
            "Missing packages:\n",
            "\n".join(req for req in missing_req_mod if req.split(":")[0] in missing_reqs),
        )
    check.is_false(missing_reqs)
