# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Initialization for Jupyter Notebooks."""
import os
import importlib
import sys
import warnings
from pathlib import Path
from typing import Any, List, Optional, Tuple, Dict

from IPython.display import display, HTML
import ipywidgets as widgets
from matplotlib import MatplotlibDeprecationWarning
import pandas as pd
import seaborn as sns

from ..common.utility import (
    check_and_install_missing_packages,
    MsticpyException,
    unit_testing,
)
from ..common.pkg_config import validate_config
from ..common.wsconfig import WorkspaceConfig
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


_IMPORT_ERR_MSSG = """
<h2><font color='red'>One or more missing packages detected</h2>
Please correct these by installing the required packages, restart
the kernel and re-run the notebook.</font>
<i>Package error: {err}</i><br>
"""

_MISSING_PKG_WARN = """
<h3><font color='orange'>Warning {package} is not installed or has an
incorrect version</h3></font>
"""

_MISSING_MPCONFIG_ERR = """
<h3><font color='orange'>Warning: no <i>msticpyconfig.yaml</i> found</h3></font>
Some functionality (such as Threat Intel lookups) will not function without
valid configuration settings.
Please go to the <a href="#Configuration">Configuration section</a>
follow the instructions there.
"""

_PANDAS_REQ_VERSION = (0, 25, 0)


def init_notebook(
    namespace: Dict[str, Any],
    additional_packages: List[str] = None,
    extra_imports: List[str] = None,
):
    print("Processing imports....")
    imp_ok = _global_imports(namespace, additional_packages, extra_imports)

    print("Checking configuration....")
    conf_ok = _check_config

    print("Setting options....")
    _set_nb_options()

    if not imp_ok or not conf_ok:
        display(HTML("<font color='red'><h3>Notebook setup failed</h3>"))
        return False
    display(HTML("<h3>Notebook setup complete</h3>"))
    return True


def _global_imports(
    namespace: Dict[str, Any],
    additional_packages: List[str] = None,
    extra_imports: List[str] = None,
):
    try:

        _imp_from_package(
            namespace=namespace, package_name="IPython", target_name="get_ipython"
        )
        _imp_from_package(
            namespace=namespace, package_name="ipywidgets", alias="widgets"
        )
        _imp_from_package(
            namespace=namespace, package_name="pathlib", target_name="Path"
        )
        _imp_from_package(
            namespace=namespace, package_name="matplotlib.pyplot", alias="plt"
        )
        _imp_from_package(
            namespace=namespace,
            package_name="matplotlib",
            target_name="MatplotlibDeprecationWarning",
        )
        _imp_from_package(namespace=namespace, package_name="seaborn", alias="sns")
        _imp_from_package(namespace=namespace, package_name="numpy", alias="np")
        _imp_from_package(namespace=namespace, package_name="pandas", alias="pd")

        _check_and_reload_pkg(namespace, pd, _PANDAS_REQ_VERSION, "pd")

        _imp_from_package(
            namespace=namespace,
            package_name="msticpy.data",
            target_name="QueryProvider",
        )
        _imp_module_all(namespace=namespace, module_name="msticpy.nbtools")
        _imp_module_all(namespace=namespace, module_name="msticpy.sectools")
        #     from msticpy.data import QueryProvider
        #     from msticpy.nbtools import *
        #     from msticpy.sectools import *
        #     from msticpy.nbtools.foliummap import FoliumMap
        #     from msticpy.nbtools.utility import md, md_warn
        #     from msticpy.nbtools.wsconfig import WorkspaceConfig
        _imp_from_package(
            namespace=namespace,
            package_name="msticpy.nbtools.foliummap",
            target_name="FoliumMap",
        )
        _imp_from_package(
            namespace=namespace,
            package_name="msticpy.nbtools.utility",
            target_name="md",
        )
        _imp_from_package(
            namespace=namespace,
            package_name="msticpy.nbtools.utility",
            target_name="md_warn",
        )
        _imp_from_package(
            namespace=namespace,
            package_name="msticpy.nbtools.wsconfig",
            target_name="WorkspaceConfig",
        )

        if additional_packages:
            check_and_install_missing_packages(additional_packages)
        if extra_imports:
            for imp_spec in extra_imports:
                params: List[Optional[str]] = [None, None, None]
                for idx, param in enumerate(imp_spec.split(",")):
                    params[idx] = param.strip() or None

                if params[0] is None:
                    raise ValueError(
                        f"First parameter in extra_imports is mandatory: {imp_spec}"
                    )
                _imp_from_package(
                    namespace=namespace,
                    package_name=params[0],
                    target_name=params[1],
                    alias=params[2],
                )
        return True
    except ImportError as imp_err:
        display(HTML(_IMPORT_ERR_MSSG.format(err=imp_err)))
        return False


def _check_config():
    config_ok = True
    mp_path = os.environ.get("MSTICPYCONFIG", "./msticpyconfig.yaml")
    if not Path(mp_path).exists():
        display(HTML(_MISSING_MPCONFIG_ERR))
    else:
        err_warn = validate_config()
        if err_warn and err_warn[0]:
            print("Errors found in msticpy configuration.")
            config_ok = False
        if err_warn and err_warn[1]:
            print("Warnings found in msticpy configuration.")

    ws_config = WorkspaceConfig()
    if not ws_config.config_loaded:
        print("No valid configuration for Azure Sentinel found.")
        config_ok = False
    return config_ok


def _set_nb_options():
    globals()["WIDGET_DEFAULTS"] = {
        "layout": widgets.Layout(width="95%"),
        "style": {"description_width": "initial"},
    }

    # Some of our dependencies (networkx) still use deprecated Matplotlib
    # APIs - we can't do anything about it, so suppress them from view
    warnings.simplefilter("ignore", category=MatplotlibDeprecationWarning)
    warnings.filterwarnings("ignore", category=DeprecationWarning)
    sns.set()
    pd.set_option("display.max_rows", 100)
    pd.set_option("display.max_columns", 50)
    pd.set_option("display.max_colwidth", 100)
    os.environ["KQLMAGIC_LOAD_MODE"] = "silent"


def _imp_module(namespace: Dict[str, Any], module_name: str, alias: str = None):
    """Import named module and assign to global alias."""
    mod = importlib.import_module(module_name)
    if alias:
        namespace[alias] = mod
    else:
        namespace[module_name] = mod
    print(f"{module_name} imported (alias={alias})")
    return mod


def _imp_module_all(namespace: Dict[str, Any], module_name):
    """Import all from named module add to globals."""
    imported_mod = importlib.import_module(module_name)
    for item in dir(imported_mod):
        if item.startswith("_"):
            continue
        namespace[item] = getattr(imported_mod, item)
    print(f"All items imported from {module_name}")


def _imp_from_package(
    namespace: Dict[str, Any],
    package_name: str,
    target_name: str = None,
    alias: str = None,
):
    """Import object or submodule from `package_name`."""
    if not target_name:
        return _imp_module(namespace=namespace, module_name=package_name, alias=alias)
    try:
        # target could be a module
        obj = importlib.import_module(f".{target_name}", package_name)
    except (ImportError, ModuleNotFoundError):
        # if not, it must be an attribute (class, func, etc.)
        mod = importlib.import_module(package_name)
        obj = getattr(mod, target_name)
    if alias:
        namespace[alias] = obj
    else:
        namespace[target_name] = obj
    print(f"{target_name} imported from {package_name} (alias={alias})")
    return obj


def _check_and_reload_pkg(
    namespace: Dict[str, Any], pkg: Any, req_version: Tuple[int, ...], alias: str = None
):
    """Check package version matches required version and reload."""
    warn_mssg = []
    pkg_name = pkg.__name__
    if not hasattr(pkg, "__version__"):
        raise MsticpyException(f"Package {pkg_name} has no version data.")
    pkg_version = tuple([int(v) for v in pkg.__version__.split(".")])
    if pkg_version < req_version:
        display(HTML(_MISSING_PKG_WARN.format(package=pkg_name)))
        if not unit_testing():
            resp = input("Install the package now? (y/n)")
        else:
            resp = "y"
        if resp.casefold().startswith("y"):
            warn_mssg.append(f"{pkg_name} was installed or upgraded.")
            pip_ver = ".".join([str(elem) for elem in req_version])
            pkg_spec = f"{pkg_name}>={pip_ver}"
            check_and_install_missing_packages(required_packages=[pkg_spec], user=True)

            if pkg_name in sys.modules:
                importlib.reload(pkg)
            else:
                _imp_module(namespace, pkg_name, alias=alias)
    print(f"{pkg_name} imported version {pkg.__version__}")
    return warn_mssg
