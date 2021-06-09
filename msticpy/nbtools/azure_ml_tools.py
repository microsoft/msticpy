# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Checker functions for Azure ML notebooks."""
import json
import os
import re
import socket
import sys
import urllib
from pathlib import Path
from typing import Any, List, Mapping, Optional, Tuple, Union
from urllib import request

from IPython import get_ipython
from IPython.display import HTML, display
from pkg_resources import parse_version

from .._version import VERSION
from ..common.pkg_config import refresh_config


__version__ = VERSION

AZ_GET_STARTED = (
    "https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/A%20Getting"
    "%20Started%20Guide%20For%20Azure%20Sentinel%20ML%20Notebooks.ipynb"
)
TROUBLE_SHOOTING = (
    "https://github.com/Azure/Azure-Sentinel-Notebooks/blob/master/"
    "TroubleShootingNotebooks.ipynb"
)
MISSING_PKG_ERR = """
    <h4><font color='orange'>The package '<b>{package}</b>' is not
    installed or has an unsupported version (installed version = '{inst_ver}')</font></h4>
    Please install or upgrade before continuing: required version is {package}>={req_ver}
    """
MP_INSTALL_FAILED = """
    <h4><font color='red'>The notebook may not run correctly without
    the correct version of '<b>{pkg}</b>' ({ver} or later).</font></h4>
    Please see the <a href="{nbk_uri}">
    Getting Started Guide For Azure Sentinel ML Notebooks</a></b>
    for more information<br><hr>
"""
RELOAD_MP = """
    <h4><font color='orange'>Kernel restart needed</h4>
    An error was detected trying to load the updated version of MSTICPy.<br>
    Please restart the notebook kernel and re-run this cell - it should
    run without error.
    """

MIN_PYTHON_VER_DEF = "3.6"
MSTICPY_REQ_VERSION = __version__

VER_RGX = r"(?P<maj>\d+)\.(?P<min>\d+).(?P<pnt>\d+)(?P<suff>.*)"
MP_ENV_VAR = "MSTICPYCONFIG"
MP_FILE = "msticpyconfig.yaml"
NB_CHECK_URI = (
    "https://raw.githubusercontent.com/Azure/Azure-Sentinel-"
    "Notebooks/master/utils/nb_check.py"
)


def is_in_aml():
    """Return True if running in Azure Machine Learning."""
    return os.environ.get("APPSETTING_WEBSITE_SITE_NAME") == "AMLComputeInstance"


def check_versions(
    min_py_ver: Union[str, Tuple] = MIN_PYTHON_VER_DEF,
    min_mp_ver: Union[str, Tuple] = MSTICPY_REQ_VERSION,
    extras: Optional[List[str]] = None,
    mp_release: Optional[str] = None,
    **kwargs,
):
    """
    Check the current versions of the Python kernel and MSTICPy.

    Parameters
    ----------
    min_py_ver : Union[Tuple[int, int], str]
        Minimum Python version
    min_mp_ver : Union[Tuple[int, int], str]
        Minimum MSTICPy version
    extras : Optional[List[str]], optional
        A list of extras required for MSTICPy
    mp_release : Optional[str], optional
        Override the MSTICPy release version. This
        can also be specified in the environment variable 'MP_TEST_VER'

    Raises
    ------
    RuntimeError
        If the Python version does not support the notebook.
        If the MSTICPy version does not support the notebook
        and the user chose not to upgrade

    """
    del kwargs
    _disp_html("<h4>Starting notebook pre-checks...</h4>")
    if isinstance(min_py_ver, str):
        min_py_ver = _get_pkg_version(min_py_ver).release
    check_python_ver(min_py_ver=min_py_ver)

    if is_in_aml() and not _check_nb_check_ver():
        _disp_html("Check stopped because nb_check has been updated.")
        return
    _check_mp_install(min_mp_ver, mp_release, extras)
    _check_kql_prereqs()
    _set_kql_env_vars(extras)
    _run_user_settings()
    _set_mpconfig_var()
    _disp_html("<h4>Notebook pre-checks complete.</h4>")


def check_python_ver(min_py_ver: Union[str, Tuple] = MIN_PYTHON_VER_DEF):
    """
    Check the current version of the Python kernel.

    Parameters
    ----------
    min_py_ver : Tuple[int, int]
        Minimum Python version

    Raises
    ------
    RuntimeError
        If the Python version does not support the notebook.

    """
    min_py_ver = _get_pkg_version(min_py_ver)
    sys_ver = _get_pkg_version(sys.version_info[:3])
    _disp_html("Checking Python kernel version...")
    if sys_ver < min_py_ver:
        # Bandit SQL inject error found here
        _disp_html(
            f"""
            <h4><font color='red'>This notebook requires a later
            (Python) kernel version.</h4></font>
            Select a kernel from the notebook toolbar (above), that is Python
            {min_py_ver} or later (Python 3.8 recommended)<br>
            """  # nosec
        )
        _disp_html(
            f"""
            Please see the <a href="{TROUBLE_SHOOTING}">TroubleShootingNotebooks</a>
            for more information<br><br><hr>
            """
        )
        # Bandit SQL inject error found here
        raise RuntimeError(f"Python {min_py_ver} or later kernel is required.")  # nosec

    if sys_ver < _get_pkg_version("3.8"):
        _disp_html(
            "Recommended: switch to using the 'Python 3.8 - AzureML' notebook kernel"
            " if this is available."
        )
    _disp_html(f"Info: Python kernel version {sys_ver} OK<br>")


def _check_mp_install(
    min_mp_ver: Union[str, Tuple],
    mp_release: Optional[str],
    extras: Optional[List[str]],
):
    """Check for and try to install required MSTICPy version."""
    # Use the release ver specified in params, in the environment or
    # the notebook default.
    pkg_version = _get_pkg_version(min_mp_ver)
    mp_install_version = mp_release or os.environ.get("MP_TEST_VER") or str(pkg_version)

    check_mp_ver(min_msticpy_ver=mp_install_version, extras=extras)


def check_mp_ver(min_msticpy_ver: Union[str, Tuple], extras: Optional[List[str]]):
    """
    Check and optionally update the current version of msticpy.

    Parameters
    ----------
    min_msticpy_ver : Tuple[int, int]
        Minimum MSTICPy version
    extras : Optional[List[str]], optional
        A list of extras required for MSTICPy

    Raises
    ------
    ImportError
        If MSTICPy version is insufficient and we need to upgrade

    """
    mp_min_pkg_ver = _get_pkg_version(min_msticpy_ver)

    _disp_html("Checking msticpy version...<br>")

    inst_version = _get_pkg_version(__version__)
    if inst_version < mp_min_pkg_ver:
        _disp_html(
            MISSING_PKG_ERR.format(
                package="msticpy",
                inst_ver=inst_version,
                req_ver=mp_min_pkg_ver,
            )
        )
        mp_pkg_spec = f"msticpy[{','.join(extras)}]" if extras else "msticpy"
        mp_pkg_spec = f"{mp_pkg_spec}>={min_msticpy_ver}"

        _disp_html(
            f"Please run the following command to upgrade MSTICPy<br>"
            f"<pre>!{mp_pkg_spec}</pre><br>"
        )
        raise ImportError(
            "Unsupported version of MSTICPy installed",
            f"Installed version: {inst_version}",
            f"Required version: {mp_min_pkg_ver}",
        )

    _disp_html(f"Info: msticpy version {mp_min_pkg_ver} OK<br>")


def _set_kql_env_vars(extras: Optional[List[str]]):
    """Set environment variables for Kqlmagic based on MP extras."""
    jp_extended = ("azsentinel", "azuresentinel", "kql")
    if extras and any(extra for extra in extras if extra in jp_extended):
        os.environ["KQLMAGIC_EXTRAS_REQUIRE"] = "jupyter-extended"
    else:
        os.environ["KQLMAGIC_EXTRAS_REQUIRE"] = "jupyter-basic"
    if is_in_aml():
        os.environ["KQLMAGIC_AZUREML_COMPUTE"] = _get_vm_fqdn()


def _get_pkg_version(version: Union[str, Tuple]) -> Any:
    """Return pkg_resources parsed version from string or tuple."""
    if isinstance(version, str):
        return parse_version(version)
    if isinstance(version, tuple):
        return parse_version(".".join(str(ver) for ver in version))
    raise TypeError(f"Unparseable type version {version}")


def _disp_html(text: str):
    """Display the HTML text."""
    display(HTML(text))


def get_aml_user_folder() -> Optional[Path]:
    """Return the root of the user folder."""
    path_parts = Path(".").absolute().parts
    if "users" not in path_parts:
        return None
    # find the index of the last occurence of "users"
    users_idx = len(path_parts) - path_parts[::-1].index("users")
    # the user folder is one item below this
    if len(path_parts) < users_idx + 1:
        return None
    return Path("/".join(path_parts[: users_idx + 1]))


# pylint: disable=import-outside-toplevel, unused-import, import-error
def _run_user_settings():
    """Import nbuser_settings.py, if it exists."""
    user_folder = get_aml_user_folder()
    if user_folder.joinpath("nbuser_settings.py").is_file():
        sys.path.append(str(user_folder))
        import nbuser_settings  # noqa: F401


# pylint: enable=import-outside-toplevel, unused-import, import-error


def _set_mpconfig_var():
    """Set MSTICPYCONFIG to file in user directory if no other found."""
    mp_path_val = os.environ.get(MP_ENV_VAR)
    if (
        # If a valid MSTICPYCONFIG value is found - return
        (mp_path_val and Path(mp_path_val).is_file())
        # Or if there is a msticpconfig in the current folder.
        or Path(".").joinpath(MP_FILE).is_file()
    ):
        return
    # Otherwise check the user's root folder
    user_dir = get_aml_user_folder()
    mp_path = Path(user_dir).joinpath(MP_FILE)
    if mp_path.is_file():
        # If there's a file there, set the env variable to that.
        os.environ[MP_ENV_VAR] = str(mp_path)
        # Since we have already imported msticpy to check the version
        # it will have already configured settings so we need to refresh.
        refresh_config()
        _disp_html(
            f"<br>No {MP_FILE} found. Will use {MP_FILE} in user folder {user_dir}<br>"
        )


def _get_vm_metadata() -> Mapping[str, Any]:
    """Use local request to get VM metadata."""
    vm_uri = "http://169.254.169.254/metadata/instance?api-version=2017-08-01"
    req = urllib.request.Request(vm_uri)
    req.add_header("Metadata", "true")

    # Bandit warning on urlopen - Fixed private URL
    with urllib.request.urlopen(req) as resp:  # nosec
        metadata = json.loads(resp.read())
    return metadata if isinstance(metadata, dict) else {}


def _get_vm_fqdn() -> str:
    """Get the FQDN of the host."""
    az_region = _get_vm_metadata().get("compute", {}).get("location")
    return ".".join(
        [
            socket.gethostname(),
            az_region,
            "instances.azureml.ms",
        ]
        if az_region
        else ""
    )


def _check_kql_prereqs():
    """
    Check and install packages for Kqlmagic/msal_extensions.

    Notes
    -----
    Kqlmagic may trigger warnings about a missing PyGObject package
    and some system library dependencies. To fix this do the
    following:<br>
    From a notebook run:

        %pip uninstall enum34
        !sudo apt-get --yes install libgirepository1.0-dev
        !sudo apt-get --yes install gir1.2-secret-1
        %pip install pygobject

    You can also do this from a terminal - but ensure that you've
    activated the environment corresponding to the kernel you are
    using prior to running the pip commands.

        # Install the libgi dependency
        sudo apt install libgirepository1.0-dev
        sudo apt install gir1.2-secret-1

        # activate the environment
        # conda activate azureml_py38
        # source ./env_path/scripts/activate

        # Uninstall enum34
        python -m pip uninstall enum34
        # Install pygobject
        python -m install pygobject

    """
    if not is_in_aml():
        return
    try:
        # If this successfully imports, we are ok
        # pylint: disable=import-outside-toplevel
        import gi

        # pylint: enable=import-outside-toplevel
        del gi
    except ImportError:
        # Check for system packages
        ip_shell = get_ipython()
        if not ip_shell:
            return
        apt_list = ip_shell.run_line_magic("sx", "apt list")
        apt_list = [apt.split("/", maxsplit=1)[0] for apt in apt_list]
        missing_lx_pkg = [
            apt_pkg
            for apt_pkg in ("libgirepository1.0-dev", "gir1.2-secret-1")
            if apt_pkg not in apt_list
        ]
        if missing_lx_pkg:
            _disp_html(
                "Kqlmagic/msal-extensions pre-requisite PyGObject not installed."
            )
            _disp_html(
                "To prevent warnings when loading the Kqlmagic data provider,"
                " Please run the following commands:<br>"
                f"<pre>!sudo apt-get --yes install {' '.join(missing_lx_pkg)}</pre><br>"
                "<pre>!pip uninstall -y enum34</pre><br>"
                "<pre>!pip install PyGObject</pre>"
            )


# pylint: disable=broad-except
def _check_nb_check_ver():
    """Check the version of nb_check and optionally update."""
    nb_check_path = "utils/nb_check.py"
    gh_file = ""
    curr_file = ""
    try:
        # Bandit warning - fixed https URL
        with request.urlopen(NB_CHECK_URI) as gh_fh:  # nosec
            gh_file = gh_fh.read().decode("utf-8")
    except Exception:
        _disp_html(f"Warning could not check version of {NB_CHECK_URI}")
        return True
    nbc_path = get_aml_user_folder().joinpath(nb_check_path)
    if nbc_path.is_file():
        try:
            curr_file = nbc_path.read_text()
        except Exception:
            _disp_html(f"Warning could not check version local {nb_check_path}")

    if _get_file_ver(gh_file) == _get_file_ver(curr_file):
        return True

    _disp_html("Updating local {nb_check_path}...")
    bk_up = get_aml_user_folder().joinpath(f"{nb_check_path}._save_")
    if bk_up.is_file():
        bk_up.unlink()
    nbc_path.replace(bk_up)
    try:
        with open(nbc_path, "w") as repl_fh:
            repl_fh.write(gh_file)
    except Exception:
        bk_up.replace(nbc_path)

    _disp_html(
        "<h4><font color='orange'>"
        f"Important: The version of {nb_check_path} has been updated.<br>"
        "Please re-run this cell to run the new version."
        "</font></h4>"
    )
    return False


def _get_file_ver(file_text) -> Optional[str]:
    if not file_text:
        return None
    f_match = re.search(r"__version__\s*=\s*\"([\d.]+)\"", file_text)
    if f_match:
        return f_match.groups()[0]
    return None
