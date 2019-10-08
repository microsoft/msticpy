"""Placeholder for top level folder."""
# flake8: noqa: F403
from . import sectools
from . import nbtools
from . import data

from ._version import VERSION
from .nbtools import pkg_config as settings

__version__ = VERSION
