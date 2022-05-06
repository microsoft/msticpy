# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lookup Result and Status classes."""
import pprint
from collections import namedtuple
from enum import Enum
from typing import Any, Optional, Union

import attr

from ..._version import VERSION
from .result_severity import ResultSeverity

__version__ = VERSION
__author__ = "Ian Hellen"


SanitizedObservable = namedtuple("SanitizedObservable", ["observable", "status"])


class LookupStatus(Enum):
    """Threat intelligence lookup status."""

    OK = 0
    NOT_SUPPORTED = 1
    BAD_FORMAT = 2
    QUERY_FAILED = 3
    NO_DATA = 4
    OTHER = 10


# pylint: enable=comparison-with-callable
@attr.s(auto_attribs=True)
class LookupResult:
    """Lookup result for IoCs."""

    ioc: str
    ioc_type: str
    sanitized_value: str = ""
    query_subtype: Optional[str] = None
    provider: Optional[str] = None
    result: bool = False
    severity: int = attr.ib(default=0)
    details: Any = None
    raw_result: Optional[Union[str, dict]] = None
    reference: Optional[str] = None
    status: int = LookupStatus.OK.value

    @severity.validator
    def _check_severity(self, attribute, value):
        del attribute
        if isinstance(value, ResultSeverity):
            self.severity = value.name
            return
        self.severity = ResultSeverity.parse(value).name

    @property
    def summary(self):
        """Print a summary of the Lookup Result."""
        p_pr = pprint.PrettyPrinter(indent=4)
        print("value:", self.ioc, "(", self.ioc_type, ")")
        print("result:", self.result)
        # print("severity:", self.severity)
        p_pr.pprint(self.details)
        print("reference: ", self.reference)

    @property
    def raw_result_fmtd(self):
        """Print raw results of the Lookup Result."""
        p_pr = pprint.PrettyPrinter(indent=4)
        p_pr.pprint(self.raw_result)

    @property
    def value(self) -> str:
        """Return lookup value."""
        return self.ioc

    @value.setter
    def value(self, value: str):
        """Set lookup value."""
        self.ioc = value

    @property
    def value_type(self) -> str:
        """Return lookup value type."""
        return self.ioc_type

    @value_type.setter
    def value_type(self, value: str):
        """Set lookup value type."""
        self.ioc_type = value

    @property
    def safe_ioc(self) -> str:
        """Return sanitized value."""
        return self.sanitized_value

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

    @classmethod
    def column_map(cls):
        """Return a dictionary that maps fields to DF Names."""
        col_mapping = {}
        for name in attr.fields_dict(cls):
            out_name = "".join(part.capitalize() for part in name.split("_"))
            col_mapping[name] = out_name
        return col_mapping
