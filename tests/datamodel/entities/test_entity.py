# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module docstring."""
import json
from datetime import datetime
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.datamodel import entities
from msticpy.datamodel.entities import Alert, Host, IpAddress, OSFamily, Url
from msticpy.datamodel.soc.sentinel_alert import SentinelAlert
from msticpy.init.pivot import Pivot

from ...unit_test_lib import get_test_data_path

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name, broad-except


def test_entity_merge():
    """Entity comparison and merging."""
    host1 = Host(
        HostName="host1",
        DnsDomain="contoso.com",
        OSFamily=OSFamily.Windows,
        TimeGenerated=datetime(2022, 1, 24, 23, 5, 5, 728510),
    )
    host2 = Host(
        HostName="host1",
        DnsDomain="contoso.com",
        IsDomainJoined=True,
        TimeGenerated=datetime(2022, 1, 24, 23, 5, 5, 728510),
    )
    host3 = Host(HostName="host3", DnsDomain="contoso.com")

    check.not_equal(host1, host2)
    check.not_equal(host1, host3)

    check.is_true(host1.is_equivalent(host2))
    check.is_false(host1.is_equivalent(host3))
    check.is_false(host2.is_equivalent(host3))

    check.is_true(host1.can_merge(host2))
    check.is_false(host1.can_merge(host3))

    host4 = host1.merge(host2)
    check.equal(host4.HostName, "host1")
    check.equal(host4.OSFamily, OSFamily.Windows)
    check.equal(host4.DnsDomain, "contoso.com")
    check.is_true(host4.IsDomainJoined)


def test_url():
    """Test URL get_componennts."""
    URL = "https://www.contoso.com/path#frag?query=xxx"
    url = Url(Url=URL)
    check.equal(url.Url, URL)
    check.equal(url.host, "www.contoso.com")
    check.equal(url.scheme, "https")
    check.equal(url.path, "/path")
    check.equal(url.fragment, "frag?query=xxx")

    url.Url = "https://www.contoso2.com/path2#frag2?query=xxx"
    check.equal(url.host, "www.contoso2.com")
    check.equal(url.scheme, "https")
    check.equal(url.path, "/path2")
    check.equal(url.fragment, "frag2?query=xxx")


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_pivot_shortcuts():
    """Test pivot function shortcut creation and deletion."""
    pivot = Pivot()
    pivot.reload_pivots()

    check.is_true(hasattr(IpAddress, "util"))
    util_ctnr = getattr(IpAddress, "util")
    func = getattr(util_ctnr, "ip_type")

    IpAddress.make_pivot_shortcut("util.ip_type", "test_iptype")
    check.is_true(hasattr(IpAddress, "test_iptype"))
    check.equal(func, IpAddress.test_iptype)

    ip_addr = IpAddress(Address="192.168.1.2")

    ip_df = ip_addr.test_iptype()
    check.is_instance(ip_df, pd.DataFrame)

    with pytest.raises(AttributeError):
        IpAddress.make_pivot_shortcut("util.not_defined", "test_iptype")

    with pytest.raises(TypeError):
        IpAddress.make_pivot_shortcut("properties", "test_iptype")

    with pytest.raises(AttributeError):
        IpAddress.make_pivot_shortcut("util.ip_type", "test_iptype")

    IpAddress.make_pivot_shortcut("util.ip_type", "test_iptype", overwrite=True)
    check.is_true(hasattr(IpAddress, "test_iptype"))
    check.equal(func, IpAddress.test_iptype)

    IpAddress.del_pivot_shortcut("test_iptype")
    check.is_false(hasattr(IpAddress, "test_iptype"))
    with pytest.raises(AttributeError):
        IpAddress.del_pivot_shortcut("test_iptype")

    with pytest.raises(TypeError):
        IpAddress.del_pivot_shortcut("properties")


def test_entity_instantiation():
    """Test that we can instantiate all entities."""
    for attrib in dir(entities):
        attr_cls = getattr(entities, attrib)
        if (
            isinstance(attr_cls, type)
            and issubclass(attr_cls, entities.Entity)
            and attr_cls != entities.Entity
        ):
            ent_obj = attr_cls()
            check.greater(len(ent_obj.properties), 0)
            # Check that we can access properties without incident
            for attr in (attr for attr in dir(ent_obj) if not attr.startswith("_")):
                getattr(ent_obj, attr)


def test_entity_creation():
    """Test creating entities from raw data."""

    input_file = get_test_data_path().joinpath("entities.json")
    with open(input_file, "r") as file_handle:
        txt = file_handle.read()
        entity_dict = json.loads(txt)

    parsed_entities = []
    for _, entity in entity_dict.items():
        entity = entities.Entity.instantiate_entity(entity)

        check.is_instance(entity, entities.Entity)

        if entity["Type"] == "account":
            check.is_instance(entity, entities.Account)
            check.is_true("Name" in entity)
            check.greater(len(entity.Name), 0)
        elif entity["Type"] == "host":
            check.is_instance(entity, entities.Host)
            check.is_true("HostName" in entity)
            check.greater(len(entity.HostName), 0)
        elif entity["Type"] == "process":
            check.is_instance(entity, entities.Process)
            check.is_true("ProcessId" in entity)
            check.greater(len(entity.ProcessId), 0)
        elif entity["Type"] == "file":
            check.is_instance(entity, entities.File)
            check.is_true("Name" in entity)
            check.greater(len(entity.Name), 0)
        elif entity["Type"] == "ipaddress":
            check.is_instance(entity, entities.IpAddress)
            check.is_true("Address" in entity)
            check.greater(len(entity.Address), 0)

        parsed_entities.append(entity)

    check.greater_equal(len(parsed_entities), 7)


def test_alert_entity_creation():
    """Test creation of Alert entity."""
    data_path = Path(get_test_data_path()) / "localdata"
    alert_df = pd.read_pickle(Path(data_path).joinpath("alerts_list.pkl"))
    alert_entity = Alert(src_event=alert_df.iloc[0])
    assert len(alert_entity.properties) == 15
    assert alert_entity.SystemAlertIds == "f1ce87ca-8863-4a66-a0bd-a4d3776a7c64"


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_sentinel_entity_creation():
    """Test creation of Sentinel Alert entity."""
    data_path = Path(get_test_data_path()) / "localdata"
    sent_alert_df = pd.read_pickle(Path(data_path).joinpath("alerts_list.pkl"))
    sent_alert_entity = SentinelAlert(src_event=sent_alert_df.iloc[0])
    assert len(sent_alert_entity.properties) == 29
    assert "Search Query Results Overall Count" in sent_alert_entity.analytic
    assert sent_alert_entity.is_in_log_analytics
