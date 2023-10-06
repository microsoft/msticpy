# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lazy importer for msticpy sub-packages."""
import importlib
from types import ModuleType
from typing import Callable, Iterable, Tuple

from ._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def lazy_import(
    importer_name: str, import_list: Iterable[str]
) -> Tuple[ModuleType, Callable, Callable]:
    """
    Return the importing module and a callable for lazy importing.

    Parameters
    ----------
    importer_name: str
        The module performing the import to help facilitate resolving
        relative imports.
    import_list:  Iterable[str]
        Iterable of the modules to be potentially imported (absolute
        or relative).
        The basic form is "[mod_path.]import_item". This will import "import_item"
        from the module specified by "mod_path".
        The `as` form of importing is also supported,
        e.g. "pkg.mod.func as spam_func".

    Returns
    -------
    Tuple[module, Callable]
        This function returns a tuple of two items. The first is the importer
        module for easy reference within itself. The second item is a callable to be
        set to `__getattr__` of the calling module.

    Notes
    -----
    Code modified (slightly) from example by Brett Cannon.
    https://snarky.ca/lazy-importing-in-python-3-7/

    """
    module = importlib.import_module(importer_name)
    static_attribs = set(dir(module))
    import_mapping = {}
    for name in import_list:
        importing, _, binding = name.partition(" as ")
        if not binding:
            _, _, binding = importing.rpartition(".")
        import_mapping[binding] = importing

    def __getattr__(name: str):
        """Return the imported module or module member."""
        if name not in import_mapping:
            message = f"module {importer_name!r} has no attribute {name!r}"
            raise AttributeError(message)
        importing = import_mapping[name]
        mod_name, _, attrib_name = importing.rpartition(".")
        if mod_name == importer_name:
            # avoid infinite recursion
            raise AttributeError(
                f"Recursive import of name '[{mod_name}].{name}' from '{importer_name}'."
            )
        # importlib.import_module() implicitly sets submodules on this module as
        # appropriate for direct imports.
        try:
            imported = importlib.import_module(
                mod_name, module.__spec__.parent  # type: ignore
            )
        except ImportError as imp_err:
            message = f"cannot import name '{mod_name}' from '{importer_name}'"
            raise ImportError(message) from imp_err
        mod_attrib = getattr(imported, attrib_name, None)
        setattr(module, name, mod_attrib)
        return mod_attrib

    def __dir__():
        """Return module attribute list combining static and dynamic attribs."""
        return sorted(set(import_mapping).union(static_attribs))

    return module, __getattr__, __dir__
