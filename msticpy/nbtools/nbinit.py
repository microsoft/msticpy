# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Initialization for Jupyter Notebooks."""
import os
from functools import wraps
import importlib
import sys
import warnings
from pathlib import Path
from typing import Any, List, Optional, Tuple, Dict, Callable

from IPython.core.interactiveshell import InteractiveShell
from IPython.display import display, HTML
import ipywidgets as widgets
from matplotlib import MatplotlibDeprecationWarning
import pandas as pd
import seaborn as sns

from ..common.exceptions import MsticpyUserError, MsticpyException
from ..common.utility import check_and_install_missing_packages, unit_testing
from ..common.pkg_config import validate_config, get_config
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

_IMPORT_MODULE_MSSG = """
<font color='red'>Error import module {module}</font>
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


def _get_verbose_setting() -> Callable[[Optional[bool]], bool]:
    """Closure for holding trace setting."""
    _verbose_enabled = False

    def _verbose(verbose: Optional[bool] = None) -> bool:
        nonlocal _verbose_enabled
        if verbose is not None:
            _verbose_enabled = verbose
        return _verbose_enabled

    return _verbose


_VERBOSE = _get_verbose_setting()

_NB_IMPORTS = [
    dict(pkg="pandas", alias="pd"),
    dict(pkg="IPython", tgt="get_ipython"),
    dict(pkg="IPython.display", tgt="display"),
    dict(pkg="IPython.display", tgt="HTML"),
    dict(pkg="IPython.display", tgt="Markdown"),
    dict(pkg="ipywidgets", alias="widgets"),
    dict(pkg="pathlib", tgt="Path"),
    dict(pkg="matplotlib.pyplot", alias="plt"),
    dict(pkg="matplotlib", tgt="MatplotlibDeprecationWarning"),
    dict(pkg="seaborn", alias="sns"),
    dict(pkg="numpy", alias="np"),
]
_MP_IMPORTS = [
    dict(pkg="msticpy.data", tgt="QueryProvider"),
    dict(pkg="msticpy.nbtools.foliummap", tgt="FoliumMap"),
    dict(pkg="msticpy.common.utility", tgt="md"),
    dict(pkg="msticpy.common.utility", tgt="md_warn"),
    dict(pkg="msticpy.common.wsconfig", tgt="WorkspaceConfig"),
]
_MP_IMPORT_ALL = [
    dict(module_name="msticpy.nbtools"),
    dict(module_name="msticpy.sectools"),
]


def init_notebook(
    namespace: Dict[str, Any],
    def_imports: str = "all",
    additional_packages: List[str] = None,
    user_install: bool = False,
    extra_imports: List[str] = None,
    friendly_exceptions: Optional[bool] = None,
    verbose: bool = False,
) -> bool:
    """
    Initialize the notebook environment.

    Parameters
    ----------
    namespace : Dict[str, Any]
        Namespace (usually globals()) into which imports
        are to be populated.
    def_imports : str, optional
        Import default packages. By default "all".
        Possible values are:
        - "all" - import all packages
        - "nb" - import common notebook packages
        - "msticpy" - import msticpy packages
        - "none" (or any other value) don't load any default packages.
    additional_packages : List[str], optional
        Additional packages to be pip installed,
        by default None.
        Packages are specified by name only or version
        specification (e.g. "pandas>=0.25")
    user_install : bool, optional
        Install packages in the "user" rather than system site-packages.
        Use this option if you cannot or do not want to update the system
        packages.
        You should usually avoid using this option with standard Conda environments.
    extra_imports : List[str], optional
        Additional import definitions, by default None.
        Imports are specified as up to 3 comma-delimited values
        in a string:
        "{source_pkg}, [{import_tgt}], [{alias}]"
        `source_pkg` is mandatory - equivalent to a simple "import xyz"
        statement.
        `{import_tgt}` specifies an object to import from the package
        equivalent to "from source_pkg import import_tgt"
        `alias` allows renaming of the imported object - equivent to
        the "as alias" part of the import statement.
        If you want to provide just `source_pkg` and `alias` include
        an additional placeholder comma: e.g. "pandas, , pd"
    friendly_exceptions : Optional[bool]
        Setting this to True causes msticpy to hook the notebook
        exception hander. Any exceptions derived from MsticpyUserException
        are displayed but do not produce a stack trace, etc.
        Defaults to system/user settings if no value is supplied.
    verbose : bool, optional
        Display more verbose status, by default False

    Returns
    -------
    bool
        True if successful

    Raises
    ------
    MsticpyException
        If extra_imports data format is incorrect.
        If package with required version check has no version
        information.

    """
    _VERBOSE(verbose)

    print("Processing imports....")
    imp_ok = _global_imports(
        namespace, additional_packages, user_install, extra_imports, def_imports
    )

    print("Checking configuration....")
    conf_ok, _ = _check_config()

    print("Setting options....")
    _set_nb_options(namespace)

    if friendly_exceptions is None:
        friendly_exceptions = get_config("msticpy.FriendlyExceptions")
    if friendly_exceptions:
        if verbose:
            print("Friendly exceptions enabled.")
        InteractiveShell.showtraceback = _hook_ipython_exceptions(
            InteractiveShell.showtraceback
        )
    if not imp_ok or not conf_ok:
        display(HTML("<font color='red'><h3>Notebook setup failed</h3>"))
        return False
    display(HTML("<h3>Notebook setup complete</h3>"))
    return True


def _global_imports(  # noqa: MC0001
    namespace: Dict[str, Any],
    additional_packages: List[str] = None,
    user_install: bool = False,
    extra_imports: List[str] = None,
    def_imports: str = "all",
):
    try:
        if def_imports.casefold() in ["all", "nb"]:
            for imp_pkg in _NB_IMPORTS:
                _imp_from_package(nm_spc=namespace, **imp_pkg)

            _check_and_reload_pkg(namespace, pd, _PANDAS_REQ_VERSION, "pd")

        if def_imports.casefold() in ["all", "msticpy"]:
            for imp_pkg in _MP_IMPORTS:
                _imp_from_package(nm_spc=namespace, **imp_pkg)
            for imp_pkg in _MP_IMPORT_ALL:
                _imp_module_all(nm_spc=namespace, **imp_pkg)

        if additional_packages:
            pkg_success = check_and_install_missing_packages(
                additional_packages, user=user_install
            )
            if not pkg_success:
                print("One or more packages failed to install.")
                print(
                    "Please re-run init_notebook() with the parameter user_install=True."
                )
        if extra_imports:
            _import_extras(nm_spc=namespace, extra_imports=extra_imports)
        return True
    except ImportError as imp_err:
        display(HTML(_IMPORT_ERR_MSSG.format(err=imp_err)))
        return False


def _check_config() -> Tuple[bool, Optional[Tuple[List[str], List[str]]]]:
    config_ok = True
    err_warn = None
    mp_path = os.environ.get("MSTICPYCONFIG", "./msticpyconfig.yaml")
    if not Path(mp_path).exists():
        display(HTML(_MISSING_MPCONFIG_ERR))
    else:
        err_warn = validate_config(config_file=mp_path)
        if err_warn and err_warn[0]:
            config_ok = False

    ws_config = WorkspaceConfig()
    if not ws_config.config_loaded:
        print("No valid configuration for Azure Sentinel found.")
        config_ok = False
    return config_ok, err_warn


def _set_nb_options(namespace):
    namespace["WIDGET_DEFAULTS"] = {
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


def _import_extras(nm_spc: Dict[str, Any], extra_imports: List[str]):
    for imp_spec in extra_imports:
        params: List[Optional[str]] = [None, None, None]
        for idx, param in enumerate(imp_spec.split(",")):
            params[idx] = param.strip() or None

        if params[0] is None:
            raise MsticpyException(
                f"First parameter in extra_imports is mandatory: {imp_spec}"
            )
        _imp_from_package(nm_spc=nm_spc, pkg=params[0], tgt=params[1], alias=params[2])


def _imp_module(nm_spc: Dict[str, Any], module_name: str, alias: str = None):
    """Import named module and assign to global alias."""
    try:
        mod = importlib.import_module(module_name)
    except ImportError:
        display(HTML(_IMPORT_MODULE_MSSG.format(module=module_name)))
        raise
    if alias:
        nm_spc[alias] = mod
    else:
        nm_spc[module_name] = mod
    if _VERBOSE():  # type: ignore
        print(f"{module_name} imported (alias={alias})")
    return mod


def _imp_module_all(nm_spc: Dict[str, Any], module_name):
    """Import all from named module add to globals."""
    try:
        imported_mod = importlib.import_module(module_name)
    except ImportError:
        display(HTML(_IMPORT_MODULE_MSSG.format(module=module_name)))
        raise
    for item in dir(imported_mod):
        if item.startswith("_"):
            continue
        nm_spc[item] = getattr(imported_mod, item)
    if _VERBOSE():  # type: ignore
        print(f"All items imported from {module_name}")


def _imp_from_package(
    nm_spc: Dict[str, Any], pkg: str, tgt: str = None, alias: str = None
):
    """Import object or submodule from `pkg`."""
    if not tgt:
        return _imp_module(nm_spc=nm_spc, module_name=pkg, alias=alias)
    try:
        # target could be a module
        obj = importlib.import_module(f".{tgt}", pkg)
    except (ImportError, ModuleNotFoundError):
        # if not, it must be an attribute (class, func, etc.)
        try:
            mod = importlib.import_module(pkg)
        except ImportError:
            display(HTML(_IMPORT_MODULE_MSSG.format(module=pkg)))
            raise
        obj = getattr(mod, tgt)
    if alias:
        nm_spc[alias] = obj
    else:
        nm_spc[tgt] = obj
    if _VERBOSE():  # type: ignore
        print(f"{tgt} imported from {pkg} (alias={alias})")
    return obj


def _check_and_reload_pkg(
    nm_spc: Dict[str, Any], pkg: Any, req_version: Tuple[int, ...], alias: str = None
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
            resp = input("Install the package now? (y/n)")  # nosec
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
                _imp_module(nm_spc, pkg_name, alias=alias)
    if _VERBOSE():  # type: ignore
        print(f"{pkg_name} imported version {pkg.__version__}")
    return warn_mssg


def _hook_ipython_exceptions(func):
    """Hooks the `func` and bypasses it if exception is MsticpyUserException."""

    @wraps(func)
    def showtraceback(*args, **kwargs):
        """Replace IPython showtraceback."""
        # extract exception type, value and traceback
        e_type, _, _ = sys.exc_info()
        if e_type is not None and issubclass(e_type, MsticpyUserError):
            return None
        # otherwise run the original hook
        value = func(*args, **kwargs)
        return value

    return showtraceback
