# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for SecurityEvent class."""
from typing import Any, Dict, List

import pandas as pd
from deprecated.sphinx import deprecated

from .._version import VERSION
from ..common.utility import export
from ..datamodel.entities import (
    Account,
    Entity,
    Host,
    HostLogonSession,
    IpAddress,
    Process,
)
from .security_base import SecurityBase

__version__ = VERSION
__author__ = "Ian Hellen"


@deprecated(reason="Replaced by datamodel entities", version="1.7.0")
@export
class SecurityEvent(SecurityBase):
    """SecurityEvent class."""

    def __init__(self, src_row: pd.Series = None):
        """
        Instantiate new instance of SecurityEvent.

            :param src_row: Pandas series containing single security event
        """
        self._source_data = src_row

        super().__init__(src_row=src_row)

        self._extract_entities(src_row)
        self._find_os_family()

    # Properties
    @property
    def entities(self) -> List[Entity]:
        """
        Return the list of entities extracted from the event.

        Returns
        -------
        List[Entity]
            The list of entities extracted from the event.

        """
        return list(self._entities)

    @property
    def query_params(self) -> Dict[str, Any]:
        """
        Query parameters derived from alert.

        Returns
        -------
            Dict[str, Any]
                Dictionary of parameter names

        """
        return super().query_params

    # index operator override
    def __getattr__(self, name):
        """Return the value of the named property 'name'."""
        if name is not None and name in self._source_data:
            return self._source_data[name]
        return None

    def _extract_entities(self, src_row):
        if "EventID" not in src_row:
            return
        host = Host(src_event=src_row)
        self._entities.append(host)
        event_id = str(src_row["EventID"])
        if event_id == "4688":
            event_proc = Process(src_event=src_row, role="new")
            self._entities.append(event_proc)
            event_proc["Host"] = host
            if "ParentProcess" in event_proc:
                self._entities.append(event_proc.ParentProcess)
                if "ImageFile" in event_proc.ParentProcess:
                    self._entities.append(event_proc.ParentProcess.ImageFile)
            logon_session = HostLogonSession(src_event=src_row)
            logon_session.Host = host
            if "Account" in event_proc:
                logon_session.Account = event_proc.Account
                event_proc.Account.Host = host
                self._entities.append(event_proc.Account)
            self._entities.append(logon_session)
            if "ImageFile" in event_proc:
                self._entities.append(event_proc.ImageFile)

        if event_id in ("4624", "4625"):
            subj_account = Account(src_event=src_row, role="subject")
            subj_account.Host = host
            self._entities.append(subj_account)
            tgt_account = Account(src_event=src_row, role="target")
            tgt_account.Host = host
            self._entities.append(tgt_account)
            self._entities.append(IpAddress(src_event=src_row))
