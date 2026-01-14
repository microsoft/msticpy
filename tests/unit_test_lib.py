# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit test common utilities."""

from __future__ import annotations

import os
import sys
import tempfile
from contextlib import contextmanager
from os import chdir, getcwd
from pathlib import Path

from typing import Any, Callable, Generator, Iterable

import nbformat
import yaml
from filelock import FileLock
from nbconvert.preprocessors import CellExecutionError, ExecutePreprocessor

from msticpy.common import pkg_config
from msticpy.common.pkg_config import SettingsDict

__author__ = "Ian Hellen"


def get_test_data_path() -> Path:
    """Get path to testdata folder."""
    return Path(__file__).parent.joinpath("testdata")


def get_queries_schema() -> Any:
    """Get queries schema."""
    queries_schema_path = (
        Path(__file__).parent.parent.joinpath(".schemas").joinpath("queries.json")
    )
    with queries_schema_path.open(mode="r", encoding="utf-8") as queries_schema:
        return yaml.safe_load(queries_schema)


TEST_DATA_PATH = str(get_test_data_path())


# pylint: disable=protected-access, broad-except
@contextmanager
def custom_mp_config(
    mp_path: str | Path,
    path_check: bool = True,
) -> Generator[SettingsDict, None, None]:
    """
    Context manager to temporarily set MSTICPYCONFIG path.

    Parameters
    ----------
    mp_path : str | Path
        Path to msticpy config yaml
    path_check : bool
        If False, skip check for existing file

    Yields
    ------
    SettingsDict
        Custom settings.

    Raises
    ------
    FileNotFoundError
        If mp_path does not exist.

    """
    current_path = os.environ.get(pkg_config._CONFIG_ENV_VAR)
    if path_check and not Path(mp_path).is_file():
        raise FileNotFoundError(f"Setting MSTICPYCONFIG to non-existent file {mp_path}")

    # Use temp directory for lock file with Python version and PID
    # Each test process gets its own lock since os.environ and pkg_config._settings
    # are per-process globals that don't conflict across processes
    # The lock only prevents conflicts between tests within the same process
    python_version = f"{sys.version_info.major}_{sys.version_info.minor}"
    pid = os.getpid()
    _lock_file_path = (
        Path(tempfile.gettempdir())
        / f"msticpy_test_settings_{python_version}_{pid}.lock"
    )

    try:
        with FileLock(_lock_file_path, timeout=30):
            try:
                # We need to lock the settings since these are global
                # Otherwise the tests interfere with each other.
                os.environ[pkg_config._CONFIG_ENV_VAR] = str(mp_path)
                pkg_config.refresh_config()
                yield pkg_config._settings
            finally:
                if not current_path:
                    del os.environ[pkg_config._CONFIG_ENV_VAR]
                else:
                    os.environ[pkg_config._CONFIG_ENV_VAR] = current_path
                pkg_config.refresh_config()
    finally:
        try:
            _lock_file_path.unlink()
        except FileNotFoundError:
            # Lock file already removed by another process - this is fine
            pass
        except Exception as err:
            print(f"Warning: Could not remove lock file {_lock_file_path}: {err}")


@contextmanager
def change_directory(path: str | Path) -> Generator[None, None, None]:
    """Change the current working directory temporarily."""
    path = Path(path).expanduser()
    prev_path = Path(getcwd())
    # Use temp directory for lock file with Python version and PID
    # Each test process gets its own lock since working directory changes
    # are per-process and don't conflict across processes
    python_version = f"{sys.version_info.major}_{sys.version_info.minor}"
    pid = os.getpid()
    cwd_lock = (
        Path(tempfile.gettempdir()) / f"msticpy_test_cwd_{python_version}_{pid}.lock"
    )
    try:
        with FileLock(cwd_lock, timeout=30):
            chdir(str(path))
            yield
    finally:
        chdir(str(prev_path))
        try:
            cwd_lock.unlink()
        except FileNotFoundError:
            # Lock file already removed by another process - this is fine
            pass
        except Exception as err:
            print(f"Warning: Could not remove lock file {cwd_lock}: {err}")


def exec_notebook(
    nb_path: str | Path,
    out_dir: str | Path | None = None,
    mp_config: str | Path | None = None,
    kernel: str = "python3",
) -> None:
    """
    Run a notebook.

    Parameters
    ----------
    nb_path : str | Path
        Path to the notebook to run
    out_dir : str | Path | None, optional
        output directory, defaults to folder containing notebook.
    mp_config : str | Path | None, optional
        If specified, use a custom msticpyconfig.yaml file.
    kernel : str, optional
        Name of the IPython kernel to use, defaults to 'python3'

    Raises
    ------
    CellExecutionError:
        If notebook execution fails.

    """
    nb_folder = Path(nb_path).parent
    with open(nb_path, "rb") as file_handle:
        nb_bytes = file_handle.read()
    nb_text = nb_bytes.decode("utf-8")
    nb_content = nbformat.reads(nb_text, as_version=4)
    exec_proc = ExecutePreprocessor(timeout=600, kernel_name=kernel)

    try:
        if mp_config:
            with custom_mp_config(mp_config):
                exec_proc.preprocess(nb_content, {"metadata": {"path": nb_folder}})
        else:
            exec_proc.preprocess(nb_content, {"metadata": {"path": nb_folder}})

    except CellExecutionError:
        nb_err = str(nb_path).replace(".ipynb", "-err.ipynb")
        msg = (
            f"Error executing the notebook '{nb_path}'.\n"
            f"See notebook '{nb_err}' for the traceback."
        )
        print(msg)
        out_dir = out_dir or nb_folder
        with open(nb_err, mode="w", encoding="utf-8") as file_handle:  # type: ignore
            nbformat.write(nb_content, file_handle)
        raise


_DEFAULT_SENTINEL = object()


def create_get_config(settings: dict[str, Any]) -> Callable[[str | None, Any], Any]:
    """Return a get_config function with settings set to settings."""

    def get_config(
        setting_path: str | None = None, default: Any = _DEFAULT_SENTINEL
    ) -> Any:
        """Get mocked setting item for path."""
        if setting_path is None:
            return settings
        try:
            return _get_config(setting_path, settings)
        except KeyError:
            if default != _DEFAULT_SENTINEL:
                return default
            raise

    return get_config


def _get_config(setting_path: str, settings_dict: dict[str, Any]) -> Any:
    """Return value from setting_path."""
    path_elems = setting_path.split(".")
    cur_node = settings_dict
    for elem in path_elems:
        cur_node = cur_node.get(elem, None)
        if cur_node is None:
            raise KeyError(f"{elem} value of {setting_path} is not a valid path")
    return cur_node


@contextmanager
def custom_get_config(
    monkeypatch: Any,
    add_modules: Iterable[str] | None = None,
    settings: dict[str, Any] | None = None,
    mp_path: str | Path | None = None,
) -> Generator[dict[str, Any], None, None]:
    """
    Context manager to temporarily set MSTICPYCONFIG path.

    Parameters
    ----------
    monkeypatch : Any
        Pytest monkeypatch fixture
    add_modules : Iterable[str] | None
        Additional modules to patch get_config for.
    settings : dict[str, Any] | None
        The mocked settings to use.
    mp_path : str | Path | None
        Path to load msticpyconfig.yaml settings from.

    Yields
    ------
    dict[str, Any]
        Custom settings.

    Raises
    ------
    FileNotFoundError
        If mp_path does not exist.

    """
    if mp_path:
        if not Path(mp_path).is_file():
            raise FileNotFoundError(
                f"Setting MSTICPYCONFIG to non-existent file {mp_path}"
            )
        mp_text = Path(mp_path).read_text(encoding="utf-8")
        settings = yaml.safe_load(mp_text)

    if settings:
        core_modules = ["msticpy.common.pkg_config", "msticpy.common.settings"]
        patched_get_config = create_get_config(settings=settings)
        for module_name in core_modules + (list(add_modules or [])):
            patched_module = sys.modules[module_name]
            monkeypatch.setattr(patched_module, "get_config", patched_get_config)
            print(f"using patched get_config for {module_name}")
        yield settings
    else:
        raise ValueError("No settings specified")
