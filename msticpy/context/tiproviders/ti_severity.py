# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TI Severity enumeration."""
from collections import namedtuple
from enum import Enum
from functools import total_ordering

from ..._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


SanitizedObservable = namedtuple("SanitizedObservable", ["observable", "status"])


# pylint: disable=too-few-public-methods
@total_ordering
class TISeverity(Enum):
    """Threat intelligence report severity."""

    # pylint: disable=invalid-name
    unknown = -1
    information = 0
    warning = 1
    high = 2

    # pylint: enable=invalid-name

    # pylint: disable=unsupported-membership-test, no-member
    @classmethod
    def parse(cls, value) -> "TISeverity":
        """
        Parse string or numeric value to TISeverity.

        Parameters
        ----------
        value : Any
            TISeverity, str or int

        Returns
        -------
        TISeverity
            TISeverity instance.

        """
        if isinstance(value, TISeverity):
            return value
        if isinstance(value, str) and value.lower() in cls.__members__:
            return cls[value.lower()]
        if isinstance(value, int) and value in [
            v.value for v in cls.__members__.values()
        ]:
            return cls(value)
        return TISeverity.unknown

    # pylint: enable=unsupported-membership-test, no-member

    # pylint: disable=comparison-with-callable
    def __eq__(self, other) -> bool:
        """
        Return True if severities are equal.

        Parameters
        ----------
        other : Any
            TISeverity to compare to.
            Can be a numeric value or name of TISeverity value.

        Returns
        -------
        bool
            If severities are equal

        """
        other_sev = TISeverity.parse(other)
        return self.value == other_sev.value

    def __gt__(self, other) -> bool:
        """
        Return True self is greater than other.

        Parameters
        ----------
        other : Any
            TISeverity to compare to.
            Can be a numeric value or name of TISeverity value.

        Returns
        -------
        bool
            If severities are equal

        """
        other_sev = TISeverity.parse(other)
        return self.value > other_sev.value
