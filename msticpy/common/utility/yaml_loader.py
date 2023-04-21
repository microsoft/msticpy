# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Yaml loader extension for including external files.

Based on https://gist.github.com/joshbode/569627ced3076931b02f

"""
import json
from pathlib import Path
from typing import IO, Any, cast

import yaml

from ..._version import VERSION

__version__ = VERSION
__author__ = "Josh Robe"


# pylint: disable=too-few-public-methods, too-many-ancestors
class IncludeLoader(yaml.SafeLoader):
    """YAML Loader with `!include` constructor."""

    def __init__(self, stream: IO) -> None:
        """Initialise Loader."""
        self.curr_dir = Path(stream.name).absolute().parent
        super().__init__(stream)


def construct_include(loader: IncludeLoader, node: yaml.Node) -> Any:
    """Include file referenced at node."""
    node_str = cast(str, loader.construct_scalar(cast(yaml.ScalarNode, node)))
    file_name = Path(loader.curr_dir).joinpath(node_str).absolute()
    extension = file_name.suffix

    with open(file_name, "r", encoding="utf-8") as f_handle:
        if extension in (".yaml", ".yml"):
            # IncludeLoader derives from SafeLoader
            return yaml.load(f_handle, IncludeLoader)  # nosec
        if extension in ("json",):
            return json.load(f_handle)
        return "".join(f_handle.readlines())


# Register the constructor
yaml.add_constructor("!include", construct_include, IncludeLoader)
