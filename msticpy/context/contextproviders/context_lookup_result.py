# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lookup Result and Status classes."""
import attr

from ..._version import VERSION
from ..lookup_result import LookupResult, LookupStatus

__version__ = VERSION
__author__ = "Ian Hellen"

ContextLookupStatus = LookupStatus


@attr.s(auto_attribs=True)
class ContextLookupResult(LookupResult):
    """Lookup result for Observables."""

    observable: str = attr.ib(default="")
    observable_type: str = attr.ib(default="")

    @property
    def item(self) -> str:
        """
        Return Observable value.

        Returns
        -------
        str
            Observable.

        """
        return self.observable

    @item.setter
    def item(self, value: str):
        self.observable = value

    @property
    def item_type(self) -> str:
        """
        Return Observable type.

        Returns
        -------
        str
            Observable type.

        """
        return self.observable_type

    @item_type.setter
    def item_type(self, value: str):
        self.observable_type = value

    @property
    def safe_observable(self) -> str:
        """Return sanitized value."""
        return self.sanitized_value
