# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
MSTICPy plugin loader.

To load MSTICPy plugins you must have modules with classes
defined from one of the supported plugin types.

To specify the locations to look for plugins specify
the folder(s) as absolute paths or relative to the current
directory

msticpyconfig.yaml
------------------
PluginFolders:
  - tests/testdata/dp_plugins
  - tests/testdata/ti_plugins
  - tests/testdata/mixed_plugins

"""


import contextlib
import sys
from importlib import import_module
from inspect import getmembers, isabstract, isclass
from pathlib import Path
from types import ModuleType
from typing import Iterable, NamedTuple, Optional, Union
from warnings import warn

from .._version import VERSION
from ..common.pkg_config import get_config
from ..context.contextlookup import ContextLookup
from ..context.contextproviders.context_provider_base import ContextProvider
from ..context.tilookup import TILookup
from ..context.tiproviders.ti_provider_base import TIProvider
from ..data import drivers
from ..data.drivers import DriverBase

__version__ = VERSION
__author__ = "Ian Hellen"

_PLUGIN_KEY = "PluginFolders"


class PluginReg(NamedTuple):
    """Plugin registration tuple."""

    reg_dest: Union[type, ModuleType]  # class or module containing CUSTOM_PROVIDERS
    name_property: Optional[str]  # Custom name(s) for provider


# This dictionary maps the class of the plugin to
# the module or class where the CUSTOM_PROVIDERS dictionary
# for that provider type is defined.
# This dictionary maps the class of the plugin to
# the module or class where the CUSTOM_PROVIDERS dictionary
# for that provider type is defined.
_PLUGIN_TYPES = {
    DriverBase: PluginReg(drivers, "DATA_ENVIRONMENTS"),
    TIProvider: PluginReg(TILookup, "PROVIDER_NAME"),
    ContextProvider: PluginReg(ContextLookup, "PROVIDER_NAME"),
}


def read_plugins(plugin_paths: Union[str, Iterable[str]]):
    """Load plugins from folders specified in msticpyconfig.yaml."""
    plugin_config = [plugin_paths] if isinstance(plugin_paths, str) else plugin_paths
    if not plugin_config:
        with contextlib.suppress(KeyError):
            plugin_config = get_config(_PLUGIN_KEY)
    if not plugin_config:
        return
    for plugin_path in plugin_config:
        load_plugins_from_path(plugin_path=plugin_path)


def load_plugins_from_path(plugin_path: Union[str, Path]):
    """Load all compatible plugins found in plugin_path."""
    sys.path.append(str(plugin_path))
    for module_file in Path(plugin_path).glob("*.py"):
        try:
            module = import_module(module_file.stem)
        except ImportError:
            warn(f"Unable to import plugin {module_file} from {plugin_path}")
        for name, obj in getmembers(module, isclass):
            if not isinstance(obj, type):
                continue
            if isabstract(obj):
                continue
            plugin_type, reg_object = _get_plugin_type(obj)
            if plugin_type:
                # if no specified registration, use the root class
                reg_dest = reg_object.reg_dest or plugin_type
                plugin_names = getattr(obj, reg_object.name_property, name)
                if not isinstance(plugin_names, (list, tuple)):
                    plugin_names = (plugin_names,)
                for plugin_name in plugin_names:
                    reg_dest.CUSTOM_PROVIDERS[plugin_name] = obj


def _get_plugin_type(plugin_class):
    """Return matching parent class for plugin_class."""
    return next(
        (
            (parent_class, plugin_reg)
            for parent_class, plugin_reg in _PLUGIN_TYPES.items()
            if issubclass(plugin_class, parent_class) and plugin_class != parent_class
        ),
        (None, None),
    )
