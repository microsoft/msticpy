# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
User configuration functions.

Loads providers based on user_defaults section in msticpyconfig.yaml

UserDefaults:
  QueryProviders:
    - AzureSentinel:
      - Default: asi
      - CyberSecuritySoc:
        alias: soc
        connect: false
    - Splunk:
        connect: false
    - LocalData: local
  LoadComponents:
    - TILookup
    - GeoIpLookup: GeoLiteLookup
    - Notebooklets:
        query_provider:
          AzureSentinel: Centrica
    - Pivot
    - AzureData:
      auth_methods=['cli','interactive']
    - AzureSentinelAPI

"""
import textwrap
from contextlib import redirect_stdout
from io import StringIO
from typing import Any, Dict, Tuple, Union

import msticpy
from .._version import VERSION
from ..common.wsconfig import WorkspaceConfig
from ..common.pkg_config import settings
from ..data.data_providers import QueryProvider
from ..sectools.geoip import GeoIpLookup

__version__ = VERSION
__author__ = "Ian Hellen"


def load_user_defaults() -> Dict[str, object]:
    """
    Load providers from user defaults in msticpyconfig.yaml.

    Returns
    -------
    Dict[str, object]
        Dict of object name and provider instances.

    """
    user_defaults = settings.get("UserDefaults")
    if not user_defaults:
        return {}
    prov_dict = _load_query_providers(user_defaults)
    prov_dict.update(_load_other_providers(user_defaults, prov_dict))

    setattr(msticpy, "current_providers", prov_dict)
    print("done")
    return prov_dict


def _load_query_providers(user_defaults):
    prov_dict = {}
    if "QueryProviders" in user_defaults:
        for qry_prov_entry in user_defaults.get("QueryProviders"):
            if isinstance(qry_prov_entry, str):
                obj_name, prov_obj = _load_provider(qry_prov_entry)
                prov_dict[obj_name] = prov_obj

            if isinstance(qry_prov_entry, dict):
                prov_name, _ = next(iter(qry_prov_entry.items()))
                if prov_name == "AzureSentinel":
                    provs = _load_az_workspaces(qry_prov_entry)
                    prov_dict.update(provs)
                else:
                    obj_name, prov_obj = _load_provider(qry_prov_entry)
                    prov_dict[obj_name] = prov_obj
    return prov_dict


def _load_other_providers(user_defaults, namespace=None):
    prov_dict = {}
    if "LoadComponents" not in user_defaults:
        return prov_dict
    comps_to_load = {}
    for component in user_defaults.get("LoadComponents", []):
        if isinstance(component, dict):
            comp_name, p_settings = next(iter(component.items()))
        else:
            comp_name = component
            p_settings = None
        comps_to_load[comp_name] = p_settings

    for comp in COMP_LOADERS:
        load_comp_func = COMP_LOADERS.get(comp)
        if comp not in comps_to_load or not load_comp_func:
            continue
        p_settings = comps_to_load.get(comp)

        if load_comp_func:
            print(f"Loading *{comp}*. ", end="")
            comp_out = StringIO()
            with redirect_stdout(comp_out):
                obj_name, comp_obj = load_comp_func(
                    p_settings, local_ns=prov_dict, global_ns=namespace
                )

            prov_dict[obj_name] = comp_obj
            print(f"- as '{obj_name}'")
            if comp_out.getvalue():
                print("   Component output:")
                print(
                    "\n".join(
                        textwrap.wrap(
                            comp_out.getvalue(),
                            replace_whitespace=True,
                            initial_indent="    ",
                            subsequent_indent="    ",
                        )
                    )
                )
        print()
    return prov_dict


def _load_az_workspaces(azsent_prov_entry: Dict[str, Any]) -> Dict[str, Any]:
    az_provs = {}
    for qp_name, workspaces in azsent_prov_entry.items():
        print(f"Loading {qp_name}")
        for ws_entry in workspaces:
            ws_name, alias, connect = _extract_qprov_entry(ws_entry)
            obj_name = f"qry_{alias.lower()}"
            print(
                f"Workspace *{ws_name}* query provider loaded as '{obj_name}'. ", end=""
            )
            prov_obj = QueryProvider(qp_name)

            if connect:
                ws_params = {}
                if ws_name != "Default":
                    ws_params = {"workspace": ws_name}
                ws_config = WorkspaceConfig(**ws_params)
                print("Connected.")
                prov_obj.connect(ws_config.code_connect_str)
            az_provs[obj_name] = prov_obj
    return az_provs


def _load_provider(qry_prov_entry: Union[Dict[str, Any], str]) -> Tuple[str, Any]:
    prov_name, alias, connect = _extract_qprov_entry(qry_prov_entry)
    obj_name = f"qry_{alias.lower()}"

    prov_obj = QueryProvider(prov_name)
    print(f"Loaded *{prov_name}* as '{obj_name}'. ", end="")
    if connect:
        prov_obj.connect()
        print("Connected.")
    return obj_name, prov_obj


def _extract_qprov_entry(
    q_prov_settings: Union[Dict[str, Any], str]
) -> Tuple[str, str, bool]:
    connect = True
    if isinstance(q_prov_settings, dict):
        name, p_settings = next(iter(q_prov_settings.items()))
        if isinstance(p_settings, dict):
            alias = p_settings.get("alias", name)
            connect = p_settings.get("connect", True)
        else:
            alias = p_settings
    else:
        name = alias = q_prov_settings
    return name, alias, connect


# pylint: disable=import-outside-toplevel
def _load_ti_lookup(p_settings=None, **kwargs):
    del p_settings, kwargs
    from msticpy.sectools.tilookup import TILookup

    return "ti_lookup", TILookup()


def _load_geoip_lookup(p_settings=None, **kwargs):
    del kwargs
    if p_settings == "GeoLiteLookup":
        from msticpy.sectools.geoip import GeoLiteLookup

        return "geoip", GeoLiteLookup()
    if p_settings == "IpStackLookup":
        from msticpy.sectools.geoip import IPStackLookup

        return "geoip", IPStackLookup()
    return None, None


def _load_notebooklets(p_settings=None, **kwargs):
    nbinit_params = {}
    if p_settings and isinstance(p_settings, dict):
        prov_name, wk_space = next(iter(p_settings.get("query_provider", {}).items()))
        nbinit_params = {
            "query_provider": prov_name,
            f"{prov_name}_workspace": wk_space,
        }
    cur_provs = kwargs.pop("global_ns", {})
    cur_provs.update(kwargs.pop("local_ns", {}))
    providers = _get_provider_names(cur_provs)
    nbinit_params.update({"providers": providers})
    try:
        import msticnb

        msticnb.init(**nbinit_params)
        return "nb", msticnb
    except ImportError:
        print("Cannot load MSTIC notebooklets package.")
        print("Please install - 'pip install msticnb'")

    return None, None


def _load_pivot(p_settings=None, **kwargs):
    del p_settings
    from msticpy.datamodel.pivot import Pivot

    namespace = kwargs.get("global_ns", {}).copy()
    namespace.update(kwargs.get("local_ns", {}))
    piv_kwargs = {"namespace": namespace}
    pivot = Pivot(**piv_kwargs)
    return "pivot", pivot


def _get_az_connect_args(p_settings):
    connect = True
    connect_args = {}
    if isinstance(p_settings, dict):
        connect = p_settings.pop("connect", True)
        connect_args = p_settings
    return connect, connect_args


def _load_azure_data(p_settings=None, **kwargs):
    del kwargs
    from msticpy.data.azure_data import AzureData

    az_data = AzureData()
    connect, connect_args = _get_az_connect_args(p_settings)
    if connect:
        az_data.connect(**connect_args)
        print("Connected. ", end="")
    return "az_data", az_data


def _load_azsent_api(p_settings=None, **kwargs):
    del kwargs
    from msticpy.data.azure_sentinel import AzureSentinel

    az_sent = AzureSentinel()
    connect, connect_args = _get_az_connect_args(p_settings)
    if connect:
        az_sent.connect(**connect_args)
        print("Connected. ", end="")
    return "azs_api", az_sent


# providers loaded in order specified
COMP_LOADERS = {
    "TILookup": _load_ti_lookup,
    "GeoIpLookup": _load_geoip_lookup,
    "Pivot": _load_pivot,
    "AzureData": _load_azure_data,
    "AzureSentinelAPI": _load_azsent_api,
    "Notebooklets": _load_notebooklets,
}


def _get_provider_names(prov_dict):
    providers = []
    for _, obj in prov_dict.items():
        if isinstance(obj, QueryProvider) and obj.connected:
            providers.append(obj.environment)
        else:
            cls_name = obj.__class__.__name__
            if cls_name in COMP_LOADERS or isinstance(obj, GeoIpLookup):
                providers.append(cls_name)
    return providers
