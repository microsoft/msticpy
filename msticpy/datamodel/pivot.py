# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pivot functions main module."""
from typing import Any, Dict

from .query_functions import PivotQueryFunctions
from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


class Pivot:
    """Pivot environment loader."""

    def __init__(self):
        """Instantiate a Pivot environment."""
        # acquire current providers

        # create QueryTimes object

        # load and assign functions
        # queries
        query_providers = []
        q_funcs = {
            prov.environment: PivotQueryFunctions(prov) for prov in query_providers
        }

        for func in q_funcs:
            pass

    def get_providers(self) -> Dict[str, Any]:
        """
        Return the current list of loaded providers.

        Returns
        -------
        Dict[str, Any]
            provider_name, provider_instance

        """
        pass

    def get_provider(self, name: str) -> Any:
        """
        Get a provider by type name.

        Parameters
        ----------
        name : str
            The name of the provider type.

        Returns
        -------
        Any
            An instance of the provider or None
            if the Pivot environment does not have one.
        """
        pass
