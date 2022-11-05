# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Packaging utility functions."""
import os
import re
import subprocess  # nosec
import sys
import warnings
from pathlib import Path
from platform import python_version
from typing import Dict, List, Optional, Tuple, Union

import pkg_resources
from IPython import get_ipython
from IPython.display import HTML, display
from tqdm.auto import tqdm
from tqdm.notebook import tqdm as tqdm_notebook

from ... import _version
from ..._version import VERSION
from .ipython import is_ipython
from .types import export

__version__ = VERSION
__author__ = "Ian Hellen"


@export
def resolve_pkg_path(part_path: str):
    """
    Resolve a path relative to the package.

    Parameters
    ----------
    part_path : str
        Absolute or relative path to resolve.

    """
    if Path(part_path).is_absolute():
        return part_path

    resolved_path = str(Path(_version.__file__).resolve().parent.joinpath(part_path))
    if Path(resolved_path).exists():
        return resolved_path

    searched_paths = list(
        {
            str(path)
            for path in Path(_version.__file__)
            .resolve()
            .parent.glob(str(Path("**").joinpath(part_path)))
            if ".mypy_cache" not in str(path)
        }
    )

    if not searched_paths or len(searched_paths) > 1:
        warnings.warn(f"No path or ambiguous match for {part_path} not found")
        return None
    return str(searched_paths[0])


@export
def check_py_version(min_ver: Tuple = (3, 6)):
    """
    Check that the current python version is not less than `min_ver`.

    Parameters
    ----------
    min_ver : Tuple, optional
        Minimum required version, by default (3,6)

    """
    if isinstance(min_ver, (float, str)):
        min_ver_list = str(min_ver).split(".")
        min_ver = (int(min_ver_list[0]), int(min_ver_list[1]))
    if sys.version_info < min_ver:
        print("Check the Kernel->Change Kernel menu and ensure that Python 3.6")
        print("or later is selected as the active kernel.")
        raise SystemExit(f"Python {min_ver[0]}.{min_ver[1]} or later is required.\n")


# pylint: disable=not-an-iterable, too-many-branches
@export  # noqa: MC0001
def check_and_install_missing_packages(  # noqa: MC0001
    required_packages: List[str],
    force_notebook: bool = False,
    user: bool = False,
    upgrade: bool = False,
) -> bool:
    """
    Check and install missing packages from provided list of packages.

    Parameters
    ----------
    required_packages : List[str]
        List of packages to check and install in a current environment
        Note you can add package version constraints by appending them to
        the package name, e.g. `pandas>=1.01`
    force_notebook : bool, optional
        Boolean value to force notebook version of progress bar,
        by default False (autodetect)
    user : bool, optional
        Boolean value to toggle user flag while installing pip packages,
        by default False
    upgrade: bool, option
        If true supply `--upgrade` flag to pip to install the latest
        version (applies to all package in `required_packages`)

    Returns
    -------
    bool :
        True if successful, else False

    """
    missing_packages = []
    if isinstance(required_packages, str):
        if "," in required_packages:
            required_packages = [
                req.strip() for req in required_packages.split(",") if req.strip()
            ]
        else:
            required_packages = [required_packages]
    # Check package requirements against installed set
    for req in required_packages:
        pkg_req = pkg_resources.Requirement.parse(req)
        try:
            found_pkg = pkg_resources.working_set.find(pkg_req)
        except pkg_resources.VersionConflict:
            found_pkg = None
        if found_pkg is None:
            missing_packages.append(req)

    if not missing_packages:
        print("All packages are already installed")
        return True

    print("Missing packages to be installed: ", *missing_packages, sep=" ")
    if is_ipython() or force_notebook:
        pkgbar = tqdm_notebook(missing_packages, desc="Installing...", unit="bytes")
    else:
        pkgbar = tqdm(missing_packages, desc="Installing...", unit="bytes")

    pkg_command = ["install"] if is_ipython() else ["python", "-m", "pip", "install"]
    if user:
        pkg_command.append("--user")
    if upgrade:
        pkg_command.append("--upgrade")
    pkg_success = True
    for package in pkgbar:
        if is_ipython():
            get_ipython().run_line_magic("pip", " ".join(pkg_command + [package]))
        else:
            try:
                subprocess.run(  # nosec
                    pkg_command + [package],
                    check=True,
                    stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE,
                )
            except subprocess.CalledProcessError as proc_err:
                print(f"An Error has occurred while installing {package}.")
                print(f"Output: {proc_err.stdout}")
                print(f"Errs: {proc_err.stderr}")
                pkg_success = False
        print(f"{package} installed.")

    return pkg_success


# pylint: enable=not-an-iterable, too-many-branches


def _get_mp_ua():
    """Build a MSTICPy specific User Agent string."""
    return f"MSTICPy{VERSION}-Python{python_version()}-{sys.platform}"


# User Agent string for MSTICPy
_MSTICPY_USER_AGENT = _get_mp_ua()


@export
def mp_ua_header() -> Dict[str, str]:
    """Return headers dict for MSTICPy User Agent."""
    return {"UserAgent": _get_mp_ua()}


@export
def search_for_file(
    pattern: str, paths: List[Union[str, Path]] = None
) -> Optional[str]:
    """Search `paths` for file `pattern`."""
    paths = paths or [".", ".."]
    for start_path in paths:
        found_files = list(Path(start_path).glob(pattern)) if start_path else []
        if found_files:
            return str(found_files[0])
    return None


@export
def search_module(pattern: str) -> Dict[str, str]:
    """
    Return MSTICPy modules that match `pattern`.

    Parameters
    ----------
    pattern : str
        Substring or regular expression

    Returns
    -------
    Dict[str, str]
        Dict of module name and help URI of matching modules

    """
    pkg_path = Path(resolve_pkg_path(""))
    deprec_pattern = "Deprecated - .* has moved"
    html_tmplt = "https://msticpy.readthedocs.io/en/latest/api/{mod_name}.html"

    def _format_module(path, root):
        stem = str(path).replace(str(root), "")
        stem = stem[1:] if stem[0] in ("\\", "/") else stem
        return stem.replace(".py", "").replace("/", ".").replace("\\", ".")

    module_list = {
        _format_module(file, pkg_path.parent)
        for file in Path(pkg_path).rglob("*.py")
        if not str(file).endswith("__init__.py")
        and re.search(pattern, str(file))
        and not re.search(deprec_pattern, file.read_text())
    }
    return {module: html_tmplt.format(mod_name=module) for module in module_list}


@export
def search_name(pattern: str) -> None:
    """
    Display matching modules as list or HTML table.

    Parameters
    ----------
    pattern : str
        Substring or regular expression

    """
    mod_uris = search_module(pattern)
    if not is_ipython(notebook=True):
        print("\n".join(f"{mod}:\t{uri}" for mod, uri in mod_uris.items()))
        return

    table_template = """
    <style>
        .table_mod {{border-collapse: collapse; width: 50%;}}
        .cell_mod {{border: 1px solid #ddd !important;
            text-align: left !important; padding: 15px !important;}}
    </style>
    <h4>Modules matching '{pattern}'</h4>
    <table class='table_mod'>
    <tr class='cell_mod'><th>Module</th><th>Help</th></tr>
    {rows}
    </table>
    """
    rows = [
        (
            f"<tr class='cell_mod'><td>{mod}</td><td>"
            f"<a href={uri} target='_blank'>{mod}</a></td></tr>"
        )
        for mod, uri in mod_uris.items()
    ]
    display(HTML(table_template.format(rows="\n".join(rows), pattern=pattern)))


_U_TEST_ENV = "PYTEST_CURRENT_TEST"


@export
def unit_testing() -> bool:
    """
    Return True if in unit testing.

    Returns
    -------
    bool
        True if in unit testing

    """
    return _U_TEST_ENV in os.environ


# pylint: disable=invalid-name
@export
def set_unit_testing(on: bool = True):
    """
    Set flag env var to indicated that code is being unit-tested.

    Parameters
    ----------
    on : bool, optional
        Turn unit testing flag on or off, by default True

    """
    if on:
        os.environ[_U_TEST_ENV] = "True"
    else:
        os.environ.pop(_U_TEST_ENV, None)
