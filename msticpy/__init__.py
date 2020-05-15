"""Placeholder for top level folder."""
# flake8: noqa: F403
from . import sectools
from . import nbtools
from . import data

from ._version import VERSION
from .common import pkg_config as settings
from .nbtools.nbinit import init_notebook

__version__ = VERSION
