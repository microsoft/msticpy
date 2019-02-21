# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for SecurityEvent class."""
import pandas as pd

from . entityschema import Host, Process, Account, IpAddress
from . security_base import SecurityBase
from . utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


@export
class SecurityEvent(SecurityBase):
    """SecurityEvent class."""

    def __init__(self, src_row: pd.Series = None):
        """
        Instantiate new instance of SecurityEvent.

            :param src_row: Pandas series containing single security event
        """
        self._source_data = src_row
        self._entity_set = []

        super().__init__(src_row=src_row)

        self._extract_entities(src_row)
        self._find_os_family()

    # Properties
    @property
    def entities(self):
        """Return the list of entities extracted from the event."""
        return list(self._entity_set)

    @property
    def query_params(self):
        """
        Query parameters derived from alert.

        Returns:
            dict(str, str) -- Dictionary of parameter names

        """
        return super().query_params

    # index operator override
    def __getattr__(self, name):
        """Return the value of the named property 'name'."""
        if name is not None and name in self._source_data:
            return self._source_data[name]
        return None

    def _extract_entities(self, src_row):
        if 'EventID' in src_row:
            self._entities.append(Host(src_event=src_row))
            event_id = str(src_row['EventID'])
            if event_id == '4688':
                self._entities.append(Process(src_row, role='new'))

            if event_id == '4624' or event_id == '4625':
                self._entities.append(Account(src_event=src_row, role='subject'))
                self._entities.append(Account(src_event=src_row, role='target'))
                self._entities.append(IpAddress(src_event=src_row))
