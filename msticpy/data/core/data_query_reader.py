# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data query definition reader."""
from pathlib import Path
from typing import Any, Dict, Iterable, Tuple

import yaml

from ..._version import VERSION
from .query_defns import DataEnvironment

__version__ = VERSION
__author__ = "Ian Hellen"


def find_yaml_files(source_path: str, recursive: bool = True) -> Iterable[Path]:
    """
    Return iterable of yaml files found in `source_path`.

    Parameters
    ----------
    source_path : str
        The source path to search in.
    recursive : bool, optional
        Whether to recurse through subfolders.
        By default False

    Returns
    -------
    Iterable[str]
        File paths of yanl files found.

    """
    recurse_pfx = "**/" if recursive else ""
    file_glob = Path(source_path).glob(f"{recurse_pfx}*.yaml")
    for file_path in file_glob:
        if not file_path.is_file():
            continue
        yield file_path


def read_query_def_file(query_file: str) -> Tuple[Dict, Dict, Dict]:
    """
    Read a yaml data query definition file.

    Parameters
    ----------
    query_file : str
        Path to yaml query defintion file

    Returns
    -------
    Tuple[Dict, Dict, Dict]
        Tuple of dictionaries.
        sources - dictionary of query definitions
        defaults - the default parameters from the file
        metadata - the global metadata from the file

    """
    data_map = None
    with open(query_file, "r", encoding="utf-8") as f_handle:
        # use safe_load instead load
        data_map = yaml.safe_load(f_handle)

    validate_query_defs(query_def_dict=data_map)

    defaults = data_map.get("defaults", {})
    sources = data_map.get("sources", {})
    metadata = data_map.get("metadata", {})

    return sources, defaults, metadata


def validate_query_defs(query_def_dict: Dict[str, Any]) -> bool:
    """
    Validate content of query definition.

    Parameters
    ----------
    query_def_dict : dict
        Dictionary of query definition yaml file contents.

    Returns
    -------
    bool
        True if validation succeeds.

    Raises
    ------
    ValueError
        The validation failure reason is returned in the
        exception message (arg[0])

    """
    # verify that sources and metadata are in the data dict
    if "sources" not in query_def_dict or not query_def_dict["sources"]:
        raise ValueError("Imported file has no sources defined")
    if "metadata" not in query_def_dict or not query_def_dict["metadata"]:
        raise ValueError("Imported file has no metadata defined")

    # data_environments and data_families must be defined at with at least
    # one value
    _validate_data_categories(query_def_dict)

    return True


def _validate_data_categories(query_def_dict: Dict):
    if (
        "data_environments" not in query_def_dict["metadata"]
        or not query_def_dict["metadata"]["data_environments"]
    ):
        raise ValueError("Imported file has no data_environments defined")

    for env in query_def_dict["metadata"]["data_environments"]:
        if not DataEnvironment.parse(env):
            raise ValueError(
                f"Unknown data environment {env} in metadata. ",
                "Valid values are\n",
                ", ".join(e.name for e in DataEnvironment),
            )

    if (
        "data_families" not in query_def_dict["metadata"]
        or not query_def_dict["metadata"]["data_families"]
    ):
        raise ValueError("Imported file has no data families defined")
