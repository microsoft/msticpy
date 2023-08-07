# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data provider sub-package."""
import importlib
from functools import singledispatch
from typing import Dict, Union

from ..._version import VERSION
from ..core.query_defns import DataEnvironment

# flake8: noqa: F403
from .driver_base import DriverBase

__version__ = VERSION

_ENVIRONMENT_DRIVERS = {
    DataEnvironment.LogAnalytics: ("azure_monitor_driver", "AzureMonitorDriver"),
    DataEnvironment.SecurityGraph: ("security_graph_driver", "SecurityGraphDriver"),
    DataEnvironment.Kusto: ("azure_kusto_driver", "AzureKustoDriver"),
    DataEnvironment.MDATP: ("mdatp_driver", "MDATPDriver"),
    DataEnvironment.MDE: ("mdatp_driver", "MDATPDriver"),
    DataEnvironment.LocalData: ("local_data_driver", "LocalDataDriver"),
    DataEnvironment.OSQueryLogs: ("local_osquery_driver", "OSQueryLogDriver"),
    DataEnvironment.Splunk: ("splunk_driver", "SplunkDriver"),
    DataEnvironment.Mordor: ("mordor_driver", "MordorDriver"),
    DataEnvironment.Sumologic: ("sumologic_driver", "SumologicDriver"),
    DataEnvironment.ResourceGraph: ("resource_graph_driver", "ResourceGraphDriver"),
    DataEnvironment.M365D: ("mdatp_driver", "MDATPDriver"),
    DataEnvironment.Cybereason: ("cybereason_driver", "CybereasonDriver"),
    DataEnvironment.Elastic: ("elastic_driver", "ElasticDriver"),
    DataEnvironment.MSSentinel_New: ("azure_monitor_driver", "AzureMonitorDriver"),
    DataEnvironment.Kusto_New: ("azure_kusto_driver", "AzureKustoDriver"),
    DataEnvironment.VelociraptorLogs: (
        "local_velociraptor_driver",
        "VelociraptorLogDriver",
    ),
    DataEnvironment.MSSentinel_Legacy: ("kql_driver", "KqlDriver"),
    DataEnvironment.Kusto_Legacy: ("kusto_driver", "KustoDriver"),
}

CUSTOM_PROVIDERS: Dict[str, type] = {}


@singledispatch
def import_driver(data_environment) -> type:
    """Unsupported type for environment."""
    raise TypeError(
        "'data_environment' must be a str or DataEnvironment type.",
        f"Called with type: {type(data_environment)}",
    )


@import_driver.register
def _(data_environment: DataEnvironment) -> type:
    """Import driver class for a data environment."""
    mod_name, cls_name = _ENVIRONMENT_DRIVERS.get(data_environment, (None, None))

    if not (mod_name and cls_name):
        raise ValueError(
            f"No driver available for environment {data_environment.name}.",
            "Possible values are:",
            ", ".join(env.name for env in _ENVIRONMENT_DRIVERS),
        )

    imp_module = importlib.import_module(
        f"msticpy.data.drivers.{mod_name}", package="msticpy"
    )
    return getattr(imp_module, cls_name)


@import_driver.register
def _(data_environment: str) -> type:
    """Import custom driver class for a data environment."""
    if plugin_cls := CUSTOM_PROVIDERS.get(data_environment):
        return plugin_cls

    raise ValueError(
        f"No driver available for environment {data_environment}.",
        "Possible values are:",
        ", ".join(CUSTOM_PROVIDERS),
    )
