# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import pandas as pd
import pytest
import pytest_check as check

from msticpy.data.core.data_providers import QueryProvider
from msticpy.data.drivers.local_velociraptor_driver import VelociraptorLogDriver

from ...unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, protected-access


# change this for actual data

_VR_LOG_PATH = "velociraptor"

_EXPECTED_TABLES = [
    "Windows_Forensics_Lnk",
    "Windows_Forensics_ProcessInfo",
    "Windows_Forensics_Usn",
    "Windows_Memory_Acquisition",
    "Windows_Network_ArpCache",
    "Windows_Network_InterfaceAddresses",
    "Windows_Network_ListeningPorts",
    "Windows_Network_Netstat",
    "Windows_Sys_Users",
    "Windows_Sysinternals_Autoruns",
    "Windows_System_DNSCache",
    "Windows_System_Pslist",
]


def test_read_log_files():
    """Test reading Velociraptor logs."""
    query_path = str(get_test_data_path().joinpath(_VR_LOG_PATH))
    vr_driver = VelociraptorLogDriver(data_paths=[query_path])

    vr_driver.connect()
    check.equal(len(vr_driver.data_files), len(_EXPECTED_TABLES))
    check.equal(len(vr_driver.schema), len(_EXPECTED_TABLES))


def test_vr_query():
    """Test loading and querying velociraptor data."""
    os_query_path = str(get_test_data_path().joinpath(_VR_LOG_PATH))
    qry_prov = QueryProvider("Velociraptor", data_paths=[os_query_path])
    qry_prov.connect()
    check.equal(len(qry_prov.velociraptor), len(_EXPECTED_TABLES))
    check.equal(len(qry_prov.velociraptor.Windows_Forensics_Lnk()), 10)
    check.equal(len(qry_prov.velociraptor.Windows_Forensics_ProcessInfo()), 10)
    check.equal(len(qry_prov.velociraptor.Windows_Sys_Users()), 7)
