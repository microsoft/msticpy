# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
import argparse
from pathlib import Path
import re
import sys
from contextlib import contextmanager
from importlib import import_module
from typing import Dict

import networkx as nx

from .ast_parser import analyze
from ..msticpy._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# TODO parameter
PACKAGE_ROOT = "E:/src/microsoft/msticpy"
PACKAGE_NAME = "msticpy"
PKG_TOKENS = r"([^=><]+)([=><]+)(.+)"


class ModuleImports:
    def __init__(self):
        self.internal = []
        self.external = []
        self.standard = []
        self.setup_reqs = []
        self.missing_reqs = []


_PKG_RENAME_NAME = {
    "attr": "attrs",
    "dns": "dnspython",
    "pkg_resources": "setuptools",
    "sklearn": "scikit_learn",
    "yaml": "pyyaml",
    "azure.common": "azure-common",
}


def _get_setup_reqs(package_root: str, req_file="requirements.txt"):
    with open(Path(package_root).joinpath(req_file), "r") as req_f:
        req_list = req_f.readlines()

    setup_pkgs = {
        re.match(PKG_TOKENS, item).groups()
        for item in req_list
        if re.match(PKG_TOKENS, item)
    }
    setup_versions = {key[0].lower(): key for key in setup_pkgs}
    setup_reqs = {key[0].lower(): key[0] for key in setup_pkgs}

    # for packages that do not match top-level names
    # add the mapping
    # create a dictionary so that we can re-map some names
    for src, tgt in _PKG_RENAME_NAME.items():
        if tgt in setup_reqs:
            setup_reqs.pop(tgt)
            setup_reqs[src] = tgt
    # Rename Azure packages replace "." with "-"
    for pkg in setup_reqs:
        if pkg.startswith("azure-mgmt"):
            setup_reqs.pop(pkg)
            setup_reqs[pkg.replace("-", ".")] = pkg
    return setup_reqs, setup_versions


def _get_pkg_from_path(pkg_file: str, pkg_root: str):
    module = ""
    py_file = Path(pkg_file)
    rel_path = py_file.relative_to(pkg_root)

    for p_elem in reversed(rel_path.parts):
        if p_elem.endswith(".py"):
            p_elem = p_elem.replace(".py", "")
        if module:
            module = p_elem + "." + module
        else:
            module = p_elem
        yield module


# Code from miki725 on stackoverflow (url split over 3 lines)
# https://stackoverflow.com/questions/22195382/how-to-check-
# if-a-module-library-package-is-part-of-the-python-standard-library
# /25646050#25646050
@contextmanager
def _ignore_site_packages_paths():
    paths = sys.path
    # remove all third-party paths
    # so that only stdlib imports will succeed
    sys.path = list(filter(None, filter(lambda i: "site-packages" not in i, sys.path)))
    yield
    sys.path = paths


def _is_std_lib(module):
    if module in sys.builtin_module_names:
        return True

    with _ignore_site_packages_paths():
        imported_module = sys.modules.pop(module, None)
        try:
            import_module(module)
        except (ImportError, RuntimeError):
            return False
        else:
            return True
        finally:
            if imported_module:
                sys.modules[module] = imported_module


# Get the list of all modules from file paths
def _get_pkg_modules(pkg_root):
    pkg_modules = set()
    for py_file in pkg_root.glob("**/*.py"):
        pkg_modules.update(list(_get_pkg_from_path(py_file, pkg_root)))
        if py_file.name == "__init__.py":
            pkg_modules.update(list(_get_pkg_from_path(py_file.parent, pkg_root)))
    return pkg_modules


def _is_std_library(ext_imports):
    std_lib = []
    other_lib = []
    for imp_lib in ext_imports:
        if _is_std_lib(imp_lib):
            std_lib.append(imp_lib)
        else:
            other_lib.append(imp_lib)
    return std_lib, other_lib


def _match_pkg_to_reqs(imports, setup_reqs):
    req_libs = set()
    req_missing = set()
    for imp in imports:
        imp = imp.lower()
        if imp in setup_reqs:
            req_libs.add(setup_reqs[imp])
            continue
        imp_parts = imp.split(".")
        for i in range(1, len(imp_parts)):
            imp_name = ".".join(imp_parts[0:i])

            if imp_name.lower() in setup_reqs:
                req_libs.add(setup_reqs[imp_name])
                break
        else:
            req_missing.add(setup_reqs.get(imp, imp))
    return req_libs, req_missing


def analyze_imports(
    package_root: str, package_name: str, req_file: str = "requirements.txt"
) -> Dict[str, ModuleImports]:
    """
    Analyze imports for package.

    Parameters
    ----------
    package_root : str
        The path containing the package and requirements.txt
    package_name : str
        The name of the package (subfolder name)
    req_file : str, optional
        Name of the requirements file,
        by default "requirements.txt"

    Returns
    -------
    Dict[str, ModuleImports]
        A dictionary of modules and imports

    """
    setup_reqs, _ = _get_setup_reqs(package_root, req_file)
    pkg_root = Path(package_root) / package_name
    all_mod_imports: Dict[str, ModuleImports] = {}
    pkg_modules = _get_pkg_modules(pkg_root)

    pkg_py_files = list(pkg_root.glob("**/*.py"))
    print(f"processing {len(pkg_py_files)}")
    for py_file in pkg_py_files:
        rel_path = py_file.relative_to(pkg_root)
        file_analysis = analyze(py_file)

        # create a set of all imports
        all_imports = set(
            file_analysis["imports"] + list(file_analysis["imports_from"].keys())
        )
        if None in all_imports:
            all_imports.remove(None)  # type: ignore

        module_imports = ModuleImports()
        module_imports.internal = set(all_imports) & pkg_modules
        # remove known modules from the current package
        # to get the list of external imports
        ext_imports = set(all_imports) - pkg_modules
        module_imports.standard, module_imports.external = _is_std_library(ext_imports)

        mod_name = ".".join(rel_path.parts)

        module_imports.setup_reqs, module_imports.missing_reqs = _match_pkg_to_reqs(
            module_imports.external, setup_reqs
        )

        # add the external imports for the module
        all_mod_imports[mod_name] = module_imports
    return all_mod_imports


def print_module_imports(modules: Dict[str, ModuleImports], imp_type="setup_reqs"):
    """
    Print module imports of type.

    Parameters
    ----------
    modules : Dict[str, ModuleImports]
        Dictionary of module imports
    imp_type : str, optional
        import type, by default "setup_reqs"

    """
    for py_mod in modules:
        print(py_mod, getattr(modules[py_mod], imp_type))


def build_import_graph(modules: Dict[str, ModuleImports]) -> nx.Graph:
    """
    Build Networkx graph of imports

    Parameters
    ----------
    modules : Dict[str, ModuleImports]
        Dictionary of module imports

    Returns
    -------
    nx.Graph
        Networkx DiGraph

    """
    req_imports = {mod: attribs.setup_reqs for mod, attribs in modules.items()}
    import_graph = nx.DiGraph()
    for py_mod, mod_imps in req_imports.items():
        for imp in mod_imps:
            import_graph.add_node(py_mod, n_type="module", degree=len(imps))
            import_graph.add_node(imp, n_type="import")
            import_graph.add_edge(py_mod, imp)

    for node, attr in import_graph.nodes(data=True):
        if attr["n_type"] == "import":
            imp_nbrs = len(list(import_graph.predecessors(node)))
            import_graph.add_node(node, n_type="import", degree=imp_nbrs)

    return import_graph


def _add_script_args():
    parser = argparse.ArgumentParser(description="Package imports analyer.")
    parser.add_argument(
        "--path",
        "-p",
        default=".",
        required=True,
        help="Path to folder containing package",
    )
    parser.add_argument(
        "--package", "-k", required=True, help="Name of package (subfolder of --path)"
    )
    parser.add_argument(
        "--req_file",
        "-r",
        default="requirements.txt",
        help="Name of requirements.txt file",
    )
    parser.add_argument(
        "--missing",
        action="store_true",
        default=True,
        help="Show missing imports for modules",
    )
    parser.add_argument(
        "--stdlib",
        action="store_true",
        default=False,
        help="Show standard library imports for modules",
    )
    parser.add_argument(
        "--reqs",
        action="store_true",
        default=False,
        help="Show imports listed in requirements.txt",
    )
    parser.add_argument(
        "--internal",
        action="store_true",
        default=False,
        help="Show missing imports for modules",
    )
    parser.add_argument(
        "--modules", action="store_true", default=False, help="Show imports by module."
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    mod_imports = analyze_imports(args.path, args.package, req_file=args.req_file)
    if args.modules:
        for mod, imps in mod_imports.items():
            print(mod)
            if args.internal:
                print("internal imports:", end=" ")
                print(imps.internal if imps.internal else "none")
            if args.stdlib:
                print("std lib imports:", end=" ")
                print(imps.standard if imps.standard else "none")
            if args.reqs:
                print(f"external imports listed in {args.req_file}:", end=" ")
                print(imps.setup_reqs if imps.setup_reqs else "none")
            if args.missing:
                print("missing imports:", end=" ")
                print(imps.missing_reqs if imps.missing_reqs else "none")
    else:
        if args.internal:
            print("internal imports:", end=" ")
            print(sorted({v for s in mod_imports.values() for v in s.internal}))
        if args.stdlib:
            print("std lib imports:", end=" ")
            print(sorted({v for s in mod_imports.values() for v in s.standard}))
        if args.reqs:
            print(f"external imports listed in {args.req_file}:", end=" ")
            print(sorted({v for s in mod_imports.values() for v in s.setup_reqs}))
        if args.missing:
            print("missing imports:", end=" ")
            print(sorted({v for s in mod_imports.values() for v in s.missing_reqs}))
    # print("\nReq_imports",  {v for s in mod_imports.values() for v in s.setup_reqs})
    # print("\nReq_missing imports", {v for s in mod_imports.values() for v in s.missing_reqs})
