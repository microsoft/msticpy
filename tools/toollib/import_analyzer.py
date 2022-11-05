# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
import sys
from collections import defaultdict
from importlib import import_module
from pathlib import Path
from typing import Any, DefaultDict, Dict, Generator, List, Optional, Set, Tuple

import networkx as nx
import pandas as pd
import pkg_resources

from . import VERSION

# pylint: disable=relative-beyond-top-level
from .ast_parser import analyze

__version__ = VERSION
__author__ = "Ian Hellen"


PKG_TOKENS = r"([^#=><\[]+)(?:\[[^\]]+\])?([~=><]+)(.+)"
ImportDict = Dict[str, Dict[str, List[str]]]


# pylint: disable=too-few-public-methods
class ModuleImports:
    """Container class for module analysis."""

    def __init__(self):
        """Initialize class."""
        self.internal: Set[str] = set()
        self.external: Set[str] = set()
        self.standard: Set[str] = set()
        self.setup_reqs: Set[str] = set()
        self.missing_reqs: Set[str] = set()
        self.unknown: Set[str] = set()


_PKG_RENAME_NAME = {
    "attr": "attrs",
    "dns": "dnspython",
    "pkg_resources": "setuptools",
    "sklearn": "scikit-learn",
    "yaml": "pyyaml",
    "bs4": "beautifulsoup4",
    "dateutil": "python-dateutil",
    "splunklib": "splunk-sdk",
    "sumologic": "sumologic-sdk",
    "vt": "vt-py",
    "kqlmagic": "KqlmagicCustom",
    "jwt": "pyjwt",
}


def analyze_imports(
    package_root: str,
    package_name: str,
    req_file: str = "requirements.txt",
    extras: Optional[List[str]] = None,
    process_setup_py: bool = True,
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
    extras : List[str]
        A list of extras not specified in requirements file.
    process_setup_py : bool, optional
        If True try to parse setup.py for extras.

    Returns
    -------
    Dict[str, ModuleImports]
        A dictionary of modules and imports

    """
    setup_reqs, _ = get_setup_reqs(
        package_root, req_file, extras, skip_setup=(not process_setup_py)
    )
    pkg_root = Path(package_root) / package_name
    all_mod_imports: Dict[str, ModuleImports] = {}
    pkg_modules = _get_pkg_modules(pkg_root)

    pkg_py_files = list(pkg_root.glob("**/*.py"))
    print(f"processing {len(pkg_py_files)} modules")
    for py_file in pkg_py_files:
        module_imports = _analyze_module_imports(py_file, pkg_modules, setup_reqs)
        # add the external imports for the module
        mod_name = ".".join(py_file.relative_to(pkg_root).parts)
        all_mod_imports[mod_name] = module_imports

    return all_mod_imports


def analyze_local_imports(
    package_root: str,
    package_name: str,
    filter_common: bool = True,
) -> pd.DataFrame:
    """
    Analyze imports for package.

    Parameters
    ----------
    package_root : str
        The path containing the package and requirements.txt
    package_name : str
        The name of the package (subfolder name)
    filter_common : bool
        Filter common imports.

    Returns
    -------
    DataFrame of internal module dependencies.

    """
    pkg_root = Path(package_root) / package_name
    all_mod_imports: ImportDict = {}
    # get all the module names in the package
    pkg_module_names = _get_pkg_module_paths(pkg_root)
    # generate relative variants - e.g. a.b.c -> {a.b.c, b.c, c}
    pkg_modules: Set[str] = set()
    for mod_variants in pkg_module_names.values():
        pkg_modules.update(mod_variants)

    pkg_py_files = list(pkg_root.glob("**/*.py"))
    print(f"processing {len(pkg_py_files)} modules")
    for py_file in pkg_py_files:
        # get all of the imports from the module
        module_imports = _analyze_module_local_imports(py_file, pkg_modules)
        # create the relative module name
        mod_name = ".".join(py_file.relative_to(pkg_root).parts)
        all_mod_imports[mod_name] = module_imports

    # filter out imports of modules without a dot (same folder imports)
    all_mod_imports = _filter_none_local_imports(all_mod_imports)
    # remove noisy imports
    if filter_common:
        all_mod_imports = _filter_exclusions(all_mod_imports, ["common", "_version"])
    # create a reverse mapping from partial module paths to full paths.
    mod_name_mapping = _create_module_reverse_mapping(pkg_module_names)
    # and remap the targets to full module names.
    all_mod_imports = _remap_partial_module_names(all_mod_imports, mod_name_mapping)
    return pd.DataFrame(all_mod_imports)


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
    for py_mod_name, py_mod in modules.items():
        print(py_mod_name, getattr(py_mod, imp_type))


def build_import_graph(modules: Dict[str, ModuleImports]) -> nx.Graph:
    """
    Build Networkx graph of imports.

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
            import_graph.add_node(py_mod, n_type="module", degree=len(mod_imps))
            import_graph.add_node(imp, n_type="import")
            import_graph.add_edge(py_mod, imp)

    for node, attr in import_graph.nodes(data=True):
        if attr["n_type"] == "import":
            imp_nbrs = len(list(import_graph.predecessors(node)))
            import_graph.add_node(node, n_type="import", degree=imp_nbrs)

    return import_graph


def get_setup_reqs(
    package_root: str,
    req_file="requirements.txt",
    extras: Optional[List[str]] = None,
    skip_setup=True,
) -> Tuple[Dict[str, Any], Dict[str, str]]:
    """
    Return list of extras from setup.py.

    Parameters
    ----------
    package_root : str
        The root folder of the package
    req_file
        Requirements file
    extras : Optional[List[str]], optional
        Optional list of extras to process in place of reading
        from setup CFG
    skip_setup : bool, optional
        If True, process the setup file, by default True

    Returns
    -------
    Tuple[Dict[str, SpecifierSet], Dict[str, str]]
        Tuple of Dict[pkg_name_lower, pkg_name], Dict[pkg_name, version_spec]
        of package requirements.

    """
    with open(Path(package_root).joinpath(req_file), "r", encoding="utf-8") as req_f:
        req_list = req_f.readlines()

    setup_pkgs = _extract_pkg_specs(req_list)
    if not skip_setup and not extras:
        try:
            extras = get_extras_from_setup(extra="all")
            extra_pkgs = _extract_pkg_specs(extras)
            setup_pkgs = setup_pkgs | extra_pkgs
        except ImportError:
            print("Could not process modifed 'setup.py'")
    setup_versions = {
        req.name.casefold(): req.specifier
        for req in setup_pkgs
        if req.marker is None or req.marker.evaluate()
    }
    setup_reqs = {
        req.name.casefold(): req.name
        for req in setup_pkgs
        if req.marker is None or req.marker.evaluate()
    }

    # for packages that do not match top-level names
    # add the mapping
    # create a dictionary so that we can re-map some names
    for src, tgt in _PKG_RENAME_NAME.items():
        if tgt.casefold() in setup_reqs:
            setup_reqs.pop(tgt.casefold())
            setup_reqs[src] = tgt.casefold()
    # Rename Azure packages replace "." with "-"
    az_mgmt_reqs = {
        pkg.replace("-", "."): pkg for pkg in setup_reqs if pkg.startswith("azure-")
    }
    for key, pkg in az_mgmt_reqs.items():
        setup_reqs.pop(pkg)
        setup_reqs[key] = pkg

    # remap packages with "-" in the name with "_" to be valid Python identifiers
    setup_reqs = {key.replace("-", "_"): value for key, value in setup_reqs.items()}
    return setup_reqs, setup_versions


def get_extras_from_setup(
    extra: str = "all",
    include_base: bool = False,
) -> List[str]:
    """
    Return list of extras from setup.py.

    Parameters
    ----------
    extra : str, optional
        The name of the extra to return, by default "all"
    include_base : bool, optional
        If True include install_requires, by default False

    Returns
    -------
    List[str]
        List of package requirements.

    """
    setup_mod = import_module("setup")
    extras = getattr(setup_mod, "EXTRAS").get(extra)
    if include_base:
        base_install = getattr(setup_mod, "INSTALL_REQUIRES")
        extras.extend(
            [req.strip() for req in base_install if not req.strip().startswith("#")]
        )
    return sorted(list(set(extras)), key=str.casefold)


def _analyze_module_imports(py_file, pkg_modules, setup_reqs):
    file_analysis = analyze(py_file)

    # create a set of all imports
    all_imports = {file.strip() for file in file_analysis["imports"] if file}
    all_imports.update(
        file.strip() for file in file_analysis["imports_from"].keys() if file
    )

    if None in all_imports:
        all_imports.remove(None)  # type: ignore

    module_imports = ModuleImports()
    module_imports.internal = set(all_imports) & pkg_modules
    # remove known modules from the current package
    # to get the list of external imports
    ext_imports = set(all_imports) - pkg_modules
    (
        module_imports.standard,
        module_imports.external,
        module_imports.unknown,
    ) = _check_std_lib(ext_imports)

    module_imports.setup_reqs, module_imports.missing_reqs = _match_pkg_to_reqs(
        module_imports.external, setup_reqs
    )
    return module_imports


# Local imports analysis functions
def _analyze_module_local_imports(py_file, pkg_modules):
    file_analysis = analyze(py_file)

    # create a set of all imports
    all_imports = {module.strip(): [] for module in file_analysis["imports"] if module}
    all_imports.update(
        {
            module.strip(): imports
            for module, imports in file_analysis["imports_from"].items()
            if module
        }
    )

    if None in all_imports:
        all_imports.remove(None)  # type: ignore

    return {
        imp_mod: imports
        for imp_mod, imports in all_imports.items()
        if imp_mod in pkg_modules
    }


def _create_module_reverse_mapping(pkg_mod_mapping):
    """Return dict of partial module names to full names."""
    pkg_mod_mapping = {
        par.replace(".__init__", ""): variants
        for par, variants in pkg_mod_mapping.items()
    }
    return {mapping: key for key, val in pkg_mod_mapping.items() for mapping in val}


def _filter_none_local_imports(module_imports: ImportDict) -> ImportDict:
    """Remove all imports without dotted names."""
    return {
        mod: {imp: tgts for imp, tgts in imports.items() if "." in imp}
        for mod, imports in module_imports.items()
        if any(imp for imp in imports if "." in imp)
    }


def _filter_exclusions(
    module_imports: ImportDict,
    excl_prefixes: List[str],
) -> ImportDict:
    """Remove any imports that start with items in `excl_prefixes`."""
    return {
        mod: {
            imp: tgts
            for imp, tgts in imports.items()
            if not any(exc for exc in excl_prefixes if imp.startswith(exc))
        }
        for mod, imports in module_imports.items()
        if any(
            imp
            for imp in imports
            if not any(exc for exc in excl_prefixes if imp.startswith(exc))
        )
    }


def _remap_partial_module_names(
    module_imports: ImportDict, name_mapping: Dict[str, str]
) -> ImportDict:
    """Map partial names of module imports to full names."""
    remapped: ImportDict = {}
    for importer, imported in module_imports.items():
        remapped[importer] = {}
        for imp_path, targets in imported.items():
            if imp_path in name_mapping:
                remapped[importer][name_mapping[imp_path]] = targets
            else:
                remapped[importer][imp_path] = targets
    return remapped


def _extract_pkg_specs(pkg_specs: List[str]) -> pkg_resources.Requirement:
    return {
        pkg_resources.Requirement.parse(req)  # type: ignore
        for req in pkg_specs
        if (req and not req.strip().startswith("#"))
    }


# Adapted from code on stackoverflow (url split over 3 lines)
# https://stackoverflow.com/questions/22195382/how-to-check-
# if-a-module-library-package-is-part-of-the-python-standard-library
# /25646050#25646050
def _check_std_lib(modules):
    external = set()
    std_libs = set()
    imp_errors = set()
    paths = {p.casefold() for p in sys.path}
    paths.update({str(Path(p).resolve()).casefold() for p in sys.path})
    stdlib_paths = {
        p
        for p in paths
        if p.startswith(sys.prefix.casefold()) and "site-packages" not in p
    }
    for mod_name in modules:
        if mod_name not in sys.modules:
            try:
                import_module(mod_name)
            except ImportError:
                imp_errors.add(mod_name)
                continue
            except Exception as err:  # pylint: disable=broad-except
                print(f"Unexpected exception importing {mod_name}")
                print(err)
                imp_errors.add(mod_name)
                continue
        module = sys.modules[mod_name]

        stdlib_module = _check_stdlib_path(module, mod_name, stdlib_paths)
        if stdlib_module:
            std_libs.add(mod_name)
            continue

        parts = mod_name.split(".")
        for i, part in enumerate(parts):
            partial = ".".join(parts[:i] + [part])
            if partial in external or partial in std_libs:
                # already listed or exempted
                break
            if partial in sys.modules and sys.modules[partial]:
                # if match, add as external import
                external.add(mod_name)
                break
    return std_libs, external, imp_errors


def _check_stdlib_path(module, mod_name, stdlib_paths):
    if (
        not module
        or mod_name in sys.builtin_module_names
        or not hasattr(module, "__file__")
    ):
        # an import sentinel, built-in module or not a real module, really
        return mod_name
    # Test the path
    fname = module.__file__
    if fname.endswith(("__init__.py", "__init__.pyc", "__init__.pyo")):
        fname = Path(fname).parent

    if "site-packages" in str(fname).casefold():
        return None

    # step up the module path
    while Path(fname) != Path(fname).parent:
        if str(fname).casefold() in stdlib_paths:
            # stdlib path, skip
            return mod_name
        fname = Path(fname).parent
    return None


def _get_pkg_from_path(pkg_file: str, pkg_root: str) -> Generator[str, None, None]:
    module = ""
    py_file = Path(pkg_file)
    rel_path = py_file.relative_to(pkg_root)
    for p_elem in reversed(rel_path.parts):
        if p_elem.endswith(".py"):
            p_elem = p_elem.replace(".py", "")
        module = f"{p_elem}.{module}" if module else p_elem
        yield module


def _get_pkg_modules(pkg_root) -> Set[str]:
    """Get the list of all modules from file paths."""
    pkg_modules = set()
    for py_file in pkg_root.glob("**/*.py"):
        pkg_modules.update(list(_get_pkg_from_path(py_file, pkg_root)))
        if py_file.name == "__init__.py":
            pkg_modules.update(list(_get_pkg_from_path(py_file.parent, pkg_root)))
    return pkg_modules


def _get_pkg_module_paths(pkg_root) -> DefaultDict[str, Set[str]]:
    """Get the list of all modules from file paths."""
    pkg_modules = defaultdict(set)
    for py_file in pkg_root.glob("**/*.py"):
        module_name = ".".join(py_file.relative_to(pkg_root).parts).replace(".py", "")
        pkg_modules[module_name].update(list(_get_pkg_from_path(py_file, pkg_root)))
        if py_file.name == "__init__.py":
            pkg_modules[module_name].update(
                list(_get_pkg_from_path(py_file.parent, pkg_root))
            )
    return pkg_modules


def _match_pkg_to_reqs(imports, setup_reqs):
    req_libs = set()
    req_missing = set()
    for imp in imports:
        imp = imp.casefold()
        if imp in setup_reqs:
            req_libs.add(setup_reqs[imp])
            continue
        imp_parts = imp.split(".")
        for i in range(1, len(imp_parts)):
            imp_name = ".".join(imp_parts[:i])

            if imp_name.casefold() in setup_reqs:
                req_libs.add(setup_reqs[imp_name])
                break
        else:
            req_missing.add(setup_reqs.get(imp, imp))
    return req_libs, req_missing
