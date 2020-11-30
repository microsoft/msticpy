"""Data sub-package."""
# flake8: noqa: F403
from .data_providers import QueryProvider
from .azure_data import AzureData
from .query_defns import DataEnvironment, DataFamily

from .._version import VERSION

__version__ = VERSION
