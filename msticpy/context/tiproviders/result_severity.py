# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Result Severity enumeration."""
from enum import Enum
from functools import total_ordering

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


@total_ordering
class ResultSeverity(Enum):
    """Item report severity."""

    # pylint: disable=invalid-name
    unknown = -1
    information = 0
    warning = 1
    high = 2

    # pylint: enable=invalid-name

    @classmethod
    def parse(cls, value) -> "ResultSeverity":
        """
        Parse string or numeric value to ResultSeverity.

        Parameters
        ----------
        value : Any
            ResultSeverity, str or int

        Returns
        -------
        ResultSeverity
            ResultSeverity instance.

        """
        if isinstance(value, ResultSeverity):
            return value
        if isinstance(value, str) and value.lower() in cls.__members__:
            return cls[value.lower()]
        if isinstance(value, int) and value in [
            v.value for v in cls.__members__.values()
        ]:
            return cls(value)
        return ResultSeverity.unknown

    def __eq__(self, other) -> bool:
        """
        Return True if severities are equal.

        Parameters
        ----------
        other : Any
            ResultSeverity to compare to.
            Can be a numeric value or name of ResultSeverity value.

        Returns
        -------
        bool
            If severities are equal

        """
        other_sev = ResultSeverity.parse(other)
        return self.value == other_sev.value

    def __gt__(self, other) -> bool:
        """
        Return True self is greater than other.

        Parameters
        ----------
        other : Any
            ResultSeverity to compare to.
            Can be a numeric value or name of ResultSeverity value.

        Returns
        -------
        bool
            If severities are equal

        """
        other_sev = ResultSeverity.parse(other)
        return self.value > other_sev.value
