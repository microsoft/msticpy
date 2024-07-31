# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Utility Modules related to AI agents used in MSTICpy."""

import os
from typing import Callable, Dict, List, TypeAlias, Union

from azure.identity import DefaultAzureCredential, get_bearer_token_provider

from ..common.exceptions import MsticpyUserConfigError
from ..common.pkg_config import get_config

ConfigWithTokenProvider: TypeAlias = Dict[str, Union[str | Callable]]

token_provider = get_bearer_token_provider(
    DefaultAzureCredential(), "https://cognitiveservices.azure.com/.default"
)


def inject_token_provider_callable(config: Dict[str, str]) -> ConfigWithTokenProvider:
    """Replace autogen configuration `azure_ad_token_provider` with a token provider callable.

    Parameters
    ----------
    config : Dict[str, str]
        Autogen LLM configuration.

    Returns
    -------
    ConfigWithTokenProvider
        Autogen LLM configuration with the token provider callable.
    """
    if "azure_ad_token_provider" in config:
        config["azure_ad_token_provider"] = token_provider

    return config


def inject_environment_variable(config: Dict[str, str]) -> Dict[str, str]:
    """Replace autogen configuration `api_key` with the value of an environment variable.

    Parameters
    ----------
    config : Dict[str, str]
        Autogen LLM configuration.

    Returns
    -------
    Dict[str, str]
        Autogen LLM configuration with the environment variable value.
    """
    if "api_key" in config:
        api_key = os.environ.get(config["api_key"], None)
        if not api_key:
            raise MsticpyUserConfigError(
                f"Environment variable {config['api_key']} specified, but not found!"
            )
        config["api_key"] = api_key

    return config


def get_autogen_config_from_msticpyconfig() -> Dict[str, Union[str, float, List]]:
    """Get Autogen configuration from msticpyconfig.yaml.

    See `https://microsoft.github.io/autogen/docs/topics/llm_configuration`
    for more information on the structure of the LLM configuration object.

    Please note that a configuration list is required, rather than a single configuration.

    Returns
    -------
    Dict[str, Union[str, float, List]]
        Autogen configuration.

    Raises
    ------
    MsticpyUserConfigError
        Autogen settings not found in msticpyconfig.yaml configuration
    MsticpyUserConfigError
        Autogen.config_list setting not found in msticpyconfig.yaml configuration
    """
    autogen_config = get_config("Autogen", None)
    if not autogen_config:
        raise MsticpyUserConfigError(
            "Autogen settings not found in msticpyconfig.yaml configuration!"
        )

    if "config_list" not in autogen_config:
        raise MsticpyUserConfigError(
            "Autogen.config_list setting not found in msticpyconfig.yaml configuration!"
        )

    injectors = [
        inject_token_provider_callable,
        inject_environment_variable,
    ]

    config_list = []
    for config in autogen_config["config_list"]:
        for injector in injectors:
            config = injector(config)
        config_list.append(config)

    return {
        **autogen_config,
        "config_list": config_list,
    }
