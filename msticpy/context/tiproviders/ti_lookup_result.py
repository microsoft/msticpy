# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lookup Result and Status classes."""
from typing import Any

import attr

from ..._version import VERSION
from .result_severity import ResultSeverity
from ..lookup_result import LookupResult, LookupStatus

__version__ = VERSION
__author__ = "Ian Hellen"

TILookupStatus = LookupStatus


# pylint: enable=comparison-with-callable
@attr.s(auto_attribs=True)
class TILookupResult(LookupResult):
    """Lookup result for IoCs."""

    ioc: str = attr.ib(default="")
    ioc_type: str = attr.ib(default="")
    severity: int = attr.ib(default=0)

    @property
    def item(self) -> str:
        """
        Return IoC value.

        Returns
        -------
        str
            IoC.

        """
        return self.ioc

    @item.setter
    def item(self, value: str):
        self.ioc = value

    @property
    def item_type(self) -> str:
        """
        Return IoC type.

        Returns
        -------
        str
            IoC.

        """
        return self.ioc_type

    @item_type.setter
    def item_type(self, value: str):
        self.ioc_type = value

    @severity.validator
    def _check_severity(self, attribute, value):
        del attribute
        if isinstance(value, ResultSeverity):
            self.severity = value.name
            return
        self.severity = ResultSeverity.parse(value).name

    @property
    def severity_name(self) -> str:
        """
        Return text description of severity score.

        Returns
        -------
        str
            Severity description.

        """
        try:
            return ResultSeverity(self.severity).name
        except ValueError:
            return ResultSeverity.unknown.name

    def set_severity(self, value: Any):
        """
        Set the severity from enum, int or string.

        Parameters
        ----------
        value : Any
            The severity value to set

        """
        self._check_severity(None, value)

    @property
    def safe_ioc(self) -> str:
        """Return sanitized value."""
        return self.sanitized_value
