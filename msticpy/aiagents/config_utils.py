"""Utility Modules related to AI agents used in MSTICpy."""

from typing import Callable, Dict, TypeAlias, Union

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


def get_autogen_config_from_msticpyconfig():
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

    return {
        **autogen_config,
        "config_list": [
            inject_token_provider_callable(config)
            for config in autogen_config["config_list"]
        ],
    }
