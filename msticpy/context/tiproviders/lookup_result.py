# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TI Result class."""
import pprint
from collections import namedtuple
from typing import Any, Optional, Union

import attr

from ..._version import VERSION
from .ti_severity import TISeverity

__version__ = VERSION
__author__ = "Ian Hellen"


SanitizedObservable = namedtuple("SanitizedObservable", ["observable", "status"])


# pylint: enable=comparison-with-callable
# pylint: disable=too-many-instance-attributes
@attr.s(auto_attribs=True)
class LookupResult:
    """Lookup result for IoCs."""

    ioc: str
    ioc_type: str
    safe_ioc: str = ""
    query_subtype: Optional[str] = None
    provider: Optional[str] = None
    result: bool = False
    severity: int = attr.ib(default=0)
    details: Any = None
    raw_result: Optional[Union[str, dict]] = None
    reference: Optional[str] = None
    status: int = 0

    @severity.validator
    def _check_severity(self, attribute, value):
        del attribute
        if isinstance(value, TISeverity):
            self.severity = value.name
            return
        self.severity = TISeverity.parse(value).name

    @property
    def summary(self):
        """Print a summary of the Lookup Result."""
        p_pr = pprint.PrettyPrinter(indent=4)
        print("ioc:", self.ioc, "(", self.ioc_type, ")")
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
    def severity_name(self) -> str:
        """
        Return text description of severity score.

        Returns
        -------
        str
            Severity description.

        """
        try:
            return TISeverity(self.severity).name
        except ValueError:
            return TISeverity.unknown.name

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
