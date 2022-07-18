# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Lookup Result and Status classes."""
from abc import abstractmethod
import pprint
from collections import namedtuple
from enum import Enum
from typing import Any, Optional, Union

import attr

from .._version import VERSION

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
    """Lookup results."""

    sanitized_value: str = ""
    query_subtype: Optional[str] = None
    provider: Optional[str] = None
    result: bool = False
    details: Any = None
    raw_result: Optional[Union[str, dict]] = None
    reference: Optional[str] = None
    status: int = LookupStatus.OK.value

    @property
    @abstractmethod
    def item(self) -> str:
        """Item to lookup."""
        return self.item

    @item.setter
    def item(self, value):
        """Set Item to lookup."""
        self.item = value

    @property
    @abstractmethod
    def item_type(self) -> str:
        """Item Type to lookup."""
        return self.item_type

    @item_type.setter
    def item_type(self, value):
        """Set Item Type to lookup."""
        self.item_type = value

    @property
    def summary(self):
        """Print a summary of the Lookup Result."""
        p_pr = pprint.PrettyPrinter(indent=4)
        print("value:", self.item, "(", self.item_type, ")")
        print("result:", self.result)
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
        return self.item

    @value.setter
    def value(self, value: str):
        """Set lookup value."""
        self.item = value

    @property
    def value_type(self) -> str:
        """Return lookup value type."""
        return self.item_type

    @value_type.setter
    def value_type(self, value: str):
        """Set lookup value type."""
        self.item_type = value

    @property
    def safe_item(self) -> str:
        """Return sanitized value."""
        return self.sanitized_value

    @classmethod
    def column_map(cls):
        """Return a dictionary that maps fields to DF Names."""
        col_mapping = {}
        for name in attr.fields_dict(cls):
            out_name = "".join(part.capitalize() for part in name.split("_"))
            col_mapping[name] = out_name
        return col_mapping
