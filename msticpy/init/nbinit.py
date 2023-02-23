# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Initialization for Jupyter Notebooks.

This module performs several steps to initialize MSTICPy:
- imports a number of standard packages (e.g. pandas) into the notebook
- imports a number of modules and functions from msticpy
- checks the version of MSTICPy
- checks for presence and validates msticpyconfig.yaml

In the Azure ML and Azure Synapse environments, some additional
initialization and checks are performed.

One frequent problem in bootstrapping MSTICPy is obtaining or
creating a valid `msticpyconfig.yaml`. This is needed for many
configuration settings such as MS Sentinel workspaces, Threat
Intelligence (TI) providers, Azure authentication, Key Vault
settings and more.

If you are having trouble with this, you should check out the
following resources:

The basic structure of msticpyconfig.yaml

https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html

Using the msticpy settings editor to create or modify
msticpyconfig.yaml

https://msticpy.readthedocs.io/en/latest/getting_started/SettingsEditor.html

MS Sentinel and Azure ML resources:

Getting started notebook
https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/A%20Getting%20Started%20Guide%20For%20Azure%20Sentinel%20ML%20Notebooks.ipynb

Configuring your environment notebook
https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/ConfiguringNotebookEnvironment.ipynb

"""
import importlib
import io
import os
import sys
import traceback
import warnings
from contextlib import redirect_stdout
from functools import wraps
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Tuple

import ipywidgets as widgets
import pandas as pd
from IPython import get_ipython
from IPython.core.interactiveshell import InteractiveShell
from IPython.display import HTML, display
from matplotlib import MatplotlibDeprecationWarning

try:
    import seaborn as sns
except ImportError:
    sns = None

from .._version import VERSION
from ..auth.azure_auth_core import AzureCliStatus, check_cli_credentials
from ..common.check_version import check_version
from ..common.exceptions import MsticpyException, MsticpyUserError
from ..common.pkg_config import _HOME_PATH, get_config, refresh_config, validate_config
from ..common.utility import (
    check_and_install_missing_packages,
    check_kwargs,
    is_ipython,
    md,
    search_for_file,
    unit_testing,
)
from .azure_ml_tools import check_versions as check_versions_aml
from .azure_ml_tools import is_in_aml, populate_config_to_mp_config
from .azure_synapse_tools import init_synapse, is_in_synapse
from .pivot import Pivot
from .user_config import load_user_defaults

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

_HELP_URIS = (
    (
        '<li><a href="https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/'
        'A%20Getting%20Started%20Guide%20For%20Azure%20Sentinel%20ML%20Notebooks.ipynb"'
        'target="_blank" rel="noopener noreferrer">'
        "Getting Started (notebook)</a></li>"
    ),
    (
        '<li><a href="https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/'
        'ConfiguringNotebookEnvironment.ipynb"'
        'target="_blank" rel="noopener noreferrer">'
        "Configuring your Notebook environment (notebook)</a></li>"
    ),
    (
        '<li><a href="https://msticpy.readthedocs.io/en/latest/getting_started/'
        'msticpyconfig.html"'
        'target="_blank" rel="noopener noreferrer">'
        "Configuring MSTICPy settings (doc)</a></li>"
    ),
    (
        '<li><a href="https://msticpy.readthedocs.io/en/latest/getting_started/'
        'SettingsEditor.html"'
        'target="_blank" rel="noopener noreferrer">'
        "MSTICPy settings editor (doc)</a></li>"
    ),
    (
        '<li><a href="https://github.com/Azure/Azure-Sentinel-Notebooks/blob/'
        'master/TroubleShootingNotebooks.ipynb"'
        'target="_blank" rel="noopener noreferrer">'
        "Trouble-Shooting Notebooks (notebook)</a></li>"
    ),
)

_MISSING_MPCONFIG_ENV_ERR = f"""
<h3><font color='orange'>Warning: no <i>msticpyconfig.yaml</i> found</h3></font>
The MSTICPYCONFIG environment variable is set but does not point
to a valid file.<br>
Some functionality (such as Threat Intel lookups) will not function without
valid configuration settings.<br>
The following resources will help you set up your configuration:
<ul>{"".join(_HELP_URIS)}</ul>
<br>The first two of these are notebooks from the <b>Microsoft Sentinel
Notebooks</b> repo.
You can download and run these to set up your configuration in
another tab.
"""


_PANDAS_REQ_VERSION = (0, 25, 0)


def _get_verbosity_setting() -> Callable[[Optional[int]], int]:
    """Closure for holding trace setting."""
    _verbosity = 1

    def _verbose(verbosity: Optional[int] = None) -> int:
        nonlocal _verbosity
        if verbosity is not None:
            _verbosity = verbosity
        return _verbosity

    return _verbose


_VERBOSITY: Callable[[Optional[int]], int] = _get_verbosity_setting()

# pylint: disable=use-dict-literal
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
    dict(pkg="numpy", alias="np"),
]
if sns is not None:
    _NB_IMPORTS.append(dict(pkg="seaborn", alias="sns"))

_MP_IMPORTS = [
    dict(pkg="msticpy"),
    dict(pkg="msticpy.data", tgt="QueryProvider"),
    dict(pkg="msticpy.vis.foliummap", tgt="FoliumMap"),
    dict(pkg="msticpy.context", tgt="TILookup"),
    dict(pkg="msticpy.context", tgt="GeoLiteLookup"),
    dict(pkg="msticpy.context", tgt="IPStackLookup"),
    dict(pkg="msticpy.transform", tgt="IoCExtract"),
    dict(pkg="msticpy.common.utility", tgt="md"),
    dict(pkg="msticpy.common.utility", tgt="md_warn"),
    dict(pkg="msticpy.common.wsconfig", tgt="WorkspaceConfig"),
    dict(pkg="msticpy.init.pivot", tgt="Pivot"),
    dict(pkg="msticpy.datamodel", tgt="entities"),
    dict(pkg="msticpy.init", tgt="nbmagics"),
    dict(pkg="msticpy.nbtools", tgt="SecurityAlert"),
    dict(pkg="msticpy.vis", tgt="mp_pandas_plot"),
    dict(pkg="msticpy.vis", tgt="nbdisplay"),
    dict(pkg="msticpy.init", tgt="mp_pandas_accessors"),
    dict(pkg="msticpy", tgt="nbwidgets"),
]

_MP_IMPORT_ALL: List[Dict[str, str]] = [
    dict(module_name="msticpy.datamodel.entities"),
    dict(module_name="msticpy.nbtools"),
    dict(module_name="msticpy.sectools"),
]
# pylint: enable=use-dict-literal

_CONF_URI = (
    "https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html"
)

_AZNB_GUIDE = (
    "Please run the <i>Getting Started Guide for Azure Sentinel "
    + "ML Notebooks</i> notebook."
)
_AZ_CLI_WIKI_URI = (
    "https://github.com/Azure/Azure-Sentinel-Notebooks/wiki/"
    "Caching-credentials-with-Azure-CLI"
)
_CLI_WIKI_MSSG_GEN = (
    f"For more information see <a href='{_AZ_CLI_WIKI_URI}'>"
    "Caching credentials with Azure CLI</>"
)
_CLI_WIKI_MSSG_SHORT = (
    f"see <a href='{_AZ_CLI_WIKI_URI}'>Caching credentials with Azure CLI</>"
)

current_providers: Dict[str, Any] = {}  # pylint: disable=invalid-name

_SYNAPSE_KWARGS = ["identity_type", "storage_svc_name", "tenant_id", "cloud"]


def _pr_output(*args):
    """Output to IPython display or print."""
    if not _VERBOSITY():
        return
    if is_ipython():
        display(HTML(" ".join([*args, "<br>"]).replace("\n", "<br>")))
    else:
        print(*args)


def _err_output(*args):
    """Output to IPython display or print - always output regardless of verbosity."""
    if is_ipython():
        display(HTML(" ".join([*args, "<br>"]).replace("\n", "<br>")))
        display(
            HTML(
                "For more info and options run:"
                "<pre>import msticpy as mp\nhelp(mp.nbinit)</pre>"
            )
        )
    else:
        print(*args)
        print(
            "\nFor more info and options run:",
            "\n    import msticpy as mp",
            "\n    help(mp.nbinit)",
        )


# pylint: disable=too-many-statements
def init_notebook(
    namespace: Optional[Dict[str, Any]] = None,
    def_imports: str = "all",
    additional_packages: List[str] = None,
    extra_imports: List[str] = None,
    **kwargs,
):
    """
    Initialize the notebook environment.

    Parameters
    ----------
    namespace : Dict[str, Any], optional
        Namespace (usually globals()) into which imports
        are to be populated.
        By default, it will use the ipython `user_global_ns`.
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
        `alias` allows renaming of the imported object - equivalent to
        the "as alias" part of the import statement.
        If you want to provide just `source_pkg` and `alias` include
        an additional placeholder comma: e.g. "pandas, , pd"
    friendly_exceptions : Optional[bool]
        Setting this to True causes msticpy to hook the notebook
        exception handler. Any exceptions derived from MsticpyUserException
        are displayed but do not produce a stack trace, etc.
        Defaults to system/user settings if no value is supplied.
    verbose : Union[int, bool], optional
        Controls amount if status output, by default 1
        0 = No output
        1 or False = Brief output (default)
        2 or True = Detailed output
    config : Optional[str]
        Use this path to load a msticpyconfig.yaml.
        Defaults are MSTICPYCONFIG env variable, home folder (~/.msticpy),
        current working directory.
    no_config_check : bool, optional
        Skip the check for valid configuration. Default is False.
    verbosity : int, optional

    Raises
    ------
    MsticpyException
        If extra_imports data format is incorrect.
        If package with required version check has no version
        information.

    Notes
    -----
    This module performs several steps to initialize MSTICPy:
    - imports a number of standard packages (e.g. pandas) into the notebook
    - imports a number of modules and functions from msticpy
    - checks the version of MSTICPy
    - checks for presence and validates msticpyconfig.yaml

    In the Azure ML and Azure Synapse environments, some additional
    initialization and checks are performed.

    One frequent problem in bootstrapping MSTICPy is obtaining or
    creating a valid `msticpyconfig.yaml`. This is needed for many
    configuration settings such as MS Sentinel workspaces, Threat
    Intelligence (TI) providers, Azure authentication, Key Vault
    settings and more.

    If you are having trouble with this, you should do the following:
    1. Run `init_notebook` again using the `verbosity=2` parameter
       This will print out additional status and debugging information

    2. Run mp.MpConfigEdit() to edit (or create) a msticpyconfig file.

    3. Check out the following resources:

    The basic structure of msticpyconfig.yaml

    https://msticpy.readthedocs.io/en/latest/getting_started/msticpyconfig.html

    Using the msticpy settings editor to create or modify
    msticpyconfig.yaml

    https://msticpy.readthedocs.io/en/latest/getting_started/SettingsEditor.html

    MS Sentinel and Azure ML resources:

    Getting started notebook
    https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/A%20Getting%20Started%20Guide%20For%20Azure%20Sentinel%20ML%20Notebooks.ipynb

    Configuring your environment notebook
    https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/ConfiguringNotebookEnvironment.ipynb

    """
    global current_providers  # pylint: disable=global-statement, invalid-name

    if namespace is None and get_ipython():
        namespace = get_ipython().user_global_ns
    else:
        namespace = namespace if (namespace is not None) else {}
    check_kwargs(
        kwargs,
        [
            "user_install",
            "friendly_exceptions",
            "no_config_check",
            "verbosity",
            "verbose",
            "config",
            *_SYNAPSE_KWARGS,
        ],
    )
    user_install: bool = kwargs.pop("user_install", False)
    friendly_exceptions: Optional[bool] = kwargs.pop("friendly_exceptions", None)
    no_config_check: bool = kwargs.pop("no_config_check", False)
    if "config" in kwargs:
        _use_custom_config(kwargs.pop("config", None))

    _set_verbosity(**kwargs)

    _pr_output("<hr><h4>Starting Notebook initialization...</h4>")
    # Check Azure ML environment
    if is_in_aml():
        check_versions_aml(*_get_aml_globals(namespace))
    else:
        # If not in AML check and print version status
        stdout_cap = io.StringIO()
        with redirect_stdout(stdout_cap):
            check_version()
            _pr_output(stdout_cap.getvalue())

    if is_in_synapse():
        synapse_params = {
            key: val for key, val in kwargs.items() if key in _SYNAPSE_KWARGS
        }
        init_synapse(**synapse_params)

    # Handle required packages and imports
    _pr_output("Processing imports....")
    stdout_cap = io.StringIO()
    with redirect_stdout(stdout_cap):
        imp_ok = _global_imports(
            namespace, additional_packages, user_install, extra_imports, def_imports
        )
        _pr_output(stdout_cap.getvalue())

    # Configuration check
    if no_config_check:
        conf_ok = True
    else:
        _pr_output("Checking configuration....")
        conf_ok = _get_or_create_config()
        _check_azure_cli_status()

    # Notebook options
    _pr_output("Setting notebook options....")
    _set_nb_options(namespace)

    # Set friendly exceptions
    if friendly_exceptions is None:
        friendly_exceptions = get_config("msticpy.FriendlyExceptions")
    if friendly_exceptions:
        if _VERBOSITY() == 2:  # type: ignore
            _pr_output("Friendly exceptions enabled.")
        InteractiveShell.showtraceback = _hook_ipython_exceptions(  # type: ignore
            InteractiveShell.showtraceback
        )

    # load pivots
    stdout_cap = io.StringIO()
    with redirect_stdout(stdout_cap):
        _pr_output("Loading pivots.")
        _load_pivots(namespace=namespace)
        _pr_output(stdout_cap.getvalue())

    # User defaults
    stdout_cap = io.StringIO()
    with redirect_stdout(stdout_cap):
        _pr_output("Loading user defaults.")
        prov_dict = load_user_defaults()
        _pr_output(stdout_cap.getvalue())

    if prov_dict:
        namespace.update(prov_dict)
        current_providers = prov_dict
        _pr_output("Auto-loaded components:", ", ".join(prov_dict.keys()))

    # show any warnings
    _show_init_warnings(imp_ok, conf_ok)
    _pr_output("<h4>Notebook initialization complete</h4>")


def _show_init_warnings(imp_ok, conf_ok):
    if imp_ok and conf_ok:
        return True
    md("<font color='orange'><h3>Notebook setup completed with some warnings.</h3>")
    if not imp_ok:
        md("One or more libraries did not import successfully.")
        md(_AZNB_GUIDE)
    if not conf_ok:
        md("One or more configuration items were missing or set incorrectly.")
        md(
            _AZNB_GUIDE
            + f" and the <a href='{_CONF_URI}'>msticpy configuration guide</a>."
        )
    md("This notebook may still run but with reduced functionality.")
    return False


def _set_verbosity(**kwargs):
    """Set verbosity of output from boolean or int `verbose` param."""
    verbosity = 0
    verb_param = kwargs.pop("verbose", kwargs.pop("verbosity", 0))
    if isinstance(verb_param, bool):
        verbosity = 2 if verb_param else 0
    elif isinstance(verb_param, int):
        verbosity = min(2, max(0, verb_param))
    _VERBOSITY(verbosity)


def list_default_imports():
    """List the default imports for `init_notebook`."""
    for imp_group in (_NB_IMPORTS, _MP_IMPORTS):
        for imp_item in imp_group:
            if "tgt" in imp_item:
                import_line = f"from {imp_item['pkg']} import {imp_item['tgt']}"
            else:
                import_line = f"import {imp_item['pkg']}"
            if "alias" in imp_item:
                import_line += f" as {imp_item['alias']}"
            _pr_output(import_line)
    for imp_item in _MP_IMPORT_ALL:
        _pr_output(f"from {imp_item['module_name']} import *")


def _extract_pkg_name(
    imp_pkg: Optional[Dict[str, str]] = None,
    pkg: str = None,
    tgt: str = None,
    alias: str = None,
) -> str:
    """Return string representation of package import."""
    if imp_pkg:
        pkg = imp_pkg.get("pkg")
        tgt = imp_pkg.get("tgt")
        alias = imp_pkg.get("alias")
    import_item = f"{pkg}.{tgt}" if tgt else pkg
    if alias:
        import_item = f"{alias} ({import_item})"
    return import_item  # type: ignore


PY_VER_VAR = "REQ_PYTHON_VER"
MP_VER_VAR = "REQ_MSTICPY_VER"
MP_EXTRAS = "REQ_MP_EXTRAS"


def _get_aml_globals(namespace: Dict[str, Any]):
    """Return global values if found."""
    py_ver = namespace.get(PY_VER_VAR, "3.8")
    mp_ver = namespace.get(MP_VER_VAR, __version__)
    extras = namespace.get(MP_EXTRAS)
    return py_ver, mp_ver, extras


def _global_imports(
    namespace: Dict[str, Any],
    additional_packages: List[str] = None,
    user_install: bool = False,
    extra_imports: List[str] = None,
    def_imports: str = "all",
):
    import_list = []
    imports, imports_all = _build_import_list(def_imports)

    try:
        for imp_pkg in imports:
            _imp_from_package(nm_spc=namespace, **imp_pkg)
            import_list.append(_extract_pkg_name(imp_pkg))
        for imp_pkg in imports_all:
            _imp_module_all(nm_spc=namespace, **imp_pkg)
            import_list.append(_extract_pkg_name(imp_pkg))
        _check_and_reload_pkg(namespace, pd, _PANDAS_REQ_VERSION, "pd")

        if additional_packages:
            pkg_success = check_and_install_missing_packages(
                additional_packages, user=user_install
            )
            if not pkg_success:
                _err_output("One or more packages failed to install.")
                _err_output(
                    "Please re-run init_notebook() with the parameter user_install=True."
                )
            # We want to force import lib to see anything that we've
            # just installed.
            importlib.invalidate_caches()
        if extra_imports:
            import_list.extend(
                _import_extras(nm_spc=namespace, extra_imports=extra_imports)
            )

        _pr_output("Imported:", ", ".join(imp for imp in import_list if imp))
        return True
    except ImportError as imp_err:
        display(HTML(_IMPORT_ERR_MSSG.format(err=imp_err)))
        return False


def _build_import_list(
    def_imports: str,
) -> Tuple[List[Dict[str, str]], List[Dict[str, str]]]:
    imports = []
    imports_all = []
    if def_imports.casefold() in {"all", "nb"}:
        imports.extend(_NB_IMPORTS)
    if def_imports.casefold() in {"all", "msticpy"}:
        imports.extend(_MP_IMPORTS)
        imports_all.extend(_MP_IMPORT_ALL)
    return imports, imports_all


_AZ_SENT_ERRS = [
    "Missing or empty 'AzureSentinel' section",
    "Missing or empty 'Workspaces' key in 'AzureSentinel' section",
]


def _verify_no_azs_errors(errs):
    """Verify none of the Microsoft Sentinel errors appear in `errs`."""
    return all(az_err not in errs for az_err in _AZ_SENT_ERRS)


def _use_custom_config(config_file: str):
    """Use the config file supplied as a command line parameter."""
    if not config_file:
        return
    if not Path(config_file).is_file():
        raise ValueError(f"Configuration file {config_file} not found")
    os.environ["MSTICPYCONFIG"] = config_file
    refresh_config()


def _get_or_create_config() -> bool:
    # Cases
    # 1. Env var set and mpconfig exists -> goto 4
    # 2. Env var set and mpconfig file not exists - warn and continue
    # 3. search_for_file finds mpconfig -> goto 4
    # 4. if file and check_file_contents -> return ok
    # 5. search_for_file(config.json)
    # 6. If aml user try to import config.json into mpconfig and save
    # 7. Error - no Microsoft Sentinel config
    mp_path = os.environ.get("MSTICPYCONFIG")
    if mp_path and not Path(mp_path).is_file():
        _err_output(_MISSING_MPCONFIG_ENV_ERR)
    if not mp_path or not Path(mp_path).is_file():
        mp_path = search_for_file("msticpyconfig.yaml", paths=[".", _HOME_PATH])

    if mp_path:
        errs: List[str] = []
        try:
            std_out_cap = io.StringIO()
            with redirect_stdout(std_out_cap):
                errs, _ = validate_config(config_file=mp_path)
            if errs:
                _pr_output(std_out_cap.getvalue())
            if _verify_no_azs_errors(errs):
                # If the mpconfig has a Microsoft Sentinel config, return here
                return True
        # pylint: disable=broad-except
        except Exception as err:
            errs.append(f"Exception while checking configuration:\n{err}")
            _pr_output(f"Exception while checking configuration:\n{type(err)} - {err}")
            _pr_output("\n".join(traceback.format_tb(err.__traceback__)))
            _pr_output("Please report this to msticpy@microsoft.com")
        # pylint: enable=broad-except

    _pr_output("Could not find msticpyconfig.yaml in standard search.")
    if is_in_aml():
        _pr_output(
            "AML environment detected.",
            "Attempting to import settings from config.json to msticpyconfig.yaml.",
        )
        status = populate_config_to_mp_config(mp_path)
        if status:
            _pr_output(status)
            return True
    _pr_output("No valid configuration for Microsoft Sentinel found.")
    return False


def _set_nb_options(namespace):
    namespace["WIDGET_DEFAULTS"] = {
        "layout": widgets.Layout(width="95%"),
        "style": {"description_width": "initial"},
    }

    # Some of our dependencies (networkx) still use deprecated Matplotlib
    # APIs - we can't do anything about it, so suppress them from view
    warnings.simplefilter("ignore", category=MatplotlibDeprecationWarning)
    warnings.filterwarnings("ignore", category=DeprecationWarning)
    if sns:
        sns.set()
    pd.set_option("display.max_rows", 100)
    pd.set_option("display.max_columns", 50)
    pd.set_option("display.max_colwidth", 100)

    os.environ["KQLMAGIC_LOAD_MODE"] = "silent"
    # Kqlmagic config will use AZ CLI login if available
    kql_config = os.environ.get("KQLMAGIC_CONFIGURATION", "")
    if "try_azcli_login" not in kql_config:
        kql_config = ";".join([kql_config, "try_azcli_login=True"])
        os.environ["KQLMAGIC_CONFIGURATION"] = kql_config


def _load_pivots(namespace):
    """Load pivot functions."""
    # pylint: disable=no-member
    if not Pivot.current():
        pivot = Pivot()
        pivot.reload_pivots()
        namespace["pivot"] = pivot
        # pylint: disable=import-outside-toplevel, cyclic-import
        import msticpy

        setattr(msticpy, "pivot", pivot)


def _import_extras(nm_spc: Dict[str, Any], extra_imports: List[str]):
    added_imports = []
    if isinstance(extra_imports, str):
        extra_imports = [extra_imports]
    for imp_spec in extra_imports:
        params: List[Optional[str]] = [None, None, None]
        for idx, param in enumerate(imp_spec.split(",")):
            params[idx] = param.strip() or None

        if params[0] is None:
            raise MsticpyException(
                f"First parameter in extra_imports is mandatory: {imp_spec}"
            )
        _imp_from_package(nm_spc=nm_spc, pkg=params[0], tgt=params[1], alias=params[2])
        added_imports.append(
            _extract_pkg_name(pkg=params[0], tgt=params[1], alias=params[2])
        )
    return added_imports


def _imp_module(nm_spc: Dict[str, Any], module_name: str, alias: str = None):
    """Import named module and assign to global alias."""
    try:
        mod = importlib.import_module(module_name)
    except ImportError:
        _err_output(_IMPORT_MODULE_MSSG.format(module=module_name))
        return None
    if alias:
        nm_spc[alias] = mod
    else:
        nm_spc[module_name] = mod
    if _VERBOSITY() == 2:  # type: ignore
        _pr_output(f"{module_name} imported (alias={alias})")
    return mod


def _imp_module_all(nm_spc: Dict[str, Any], module_name):
    """Import all from named module add to globals."""
    try:
        imported_mod = importlib.import_module(module_name)
    except ImportError:
        _err_output(_IMPORT_MODULE_MSSG.format(module=module_name))
        return
    for item in dir(imported_mod):
        if item.startswith("_"):
            continue
        nm_spc[item] = getattr(imported_mod, item)
    if _VERBOSITY() == 2:  # type: ignore
        _pr_output(f"All items imported from {module_name}")


def _imp_from_package(
    nm_spc: Dict[str, Any], pkg: str, tgt: str = None, alias: str = None
):
    """Import object or submodule from `pkg`."""
    if not tgt:
        return _imp_module(nm_spc=nm_spc, module_name=pkg, alias=alias)
    try:
        # target could be a module
        obj = importlib.import_module(f".{tgt}", pkg)
    except ImportError:
        # if not, it must be an attribute (class, func, etc.)
        try:
            mod = importlib.import_module(pkg)
        except ImportError:
            _err_output(_IMPORT_MODULE_MSSG.format(module=pkg))
            return None
        obj = getattr(mod, tgt)
    if alias:
        nm_spc[alias] = obj
    else:
        nm_spc[tgt] = obj
    if _VERBOSITY() == 2:  # type: ignore
        _pr_output(f"{tgt} imported from {pkg} (alias={alias})")
    return obj


def _check_and_reload_pkg(
    nm_spc: Dict[str, Any], pkg: Any, req_version: Tuple[int, ...], alias: str = None
):
    """Check package version matches required version and reload."""
    warn_mssg = []
    pkg_name = pkg.__name__
    if not hasattr(pkg, "__version__"):
        raise MsticpyException(f"Package {pkg_name} has no version data.")
    pkg_version = tuple(int(v) for v in pkg.__version__.split("."))
    if pkg_version < req_version:
        _err_output(_MISSING_PKG_WARN.format(package=pkg_name))
        # sourcery skip: swap-if-expression
        resp = (
            input("Install the package now? (y/n)") if not unit_testing() else "y"
        )  # nosec
        if resp.casefold().startswith("y"):
            warn_mssg.append(f"{pkg_name} was installed or upgraded.")
            pip_ver = ".".join(str(elem) for elem in req_version)
            pkg_spec = f"{pkg_name}>={pip_ver}"
            check_and_install_missing_packages(required_packages=[pkg_spec], user=True)

            if pkg_name in sys.modules:
                importlib.reload(pkg)
            else:
                _imp_module(nm_spc, pkg_name, alias=alias)
    if _VERBOSITY() == 2:  # type: ignore
        _pr_output(f"{pkg_name} imported version {pkg.__version__}")
    return warn_mssg


def _hook_ipython_exceptions(func):
    """Hooks the `func` and bypasses it if exception is MsticpyUserException."""
    # if already wrapped, don't do it again
    if hasattr(InteractiveShell.showtraceback, "__wrapped__"):
        return InteractiveShell.showtraceback

    @wraps(func)
    def showtraceback(*args, **kwargs):
        """Replace IPython showtraceback."""
        # extract exception type, value and traceback
        e_type, exception, _ = sys.exc_info()
        if e_type is not None and issubclass(e_type, MsticpyUserError):
            exception.display_exception()
            return None
        # otherwise run the original hook
        return func(*args, **kwargs)

    return showtraceback


def reset_ipython_exception_handler():
    """Remove MSTICPy custom exception handler."""
    if hasattr(InteractiveShell.showtraceback, "__wrapped__"):
        InteractiveShell.showtraceback = InteractiveShell.showtraceback.__wrapped__


def _check_azure_cli_status():
    """Check for Azure CLI credentials."""
    if not unit_testing():
        status, message = check_cli_credentials()
        if status == AzureCliStatus.CLI_OK:
            _pr_output(message)
        elif status == AzureCliStatus.CLI_NOT_INSTALLED:
            _pr_output(
                "Azure CLI credentials not detected." f" ({_CLI_WIKI_MSSG_SHORT})"
            )
        elif message:
            _pr_output("\n".join([message, _CLI_WIKI_MSSG_GEN]))
