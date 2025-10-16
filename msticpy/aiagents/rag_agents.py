# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Module for MSTICpy documentation utilities and retrieval agent configuration.

Includes functions to find documentation files and to set up retrieval
agents that assist security analysts by answering questions based on MSTICpy documentation.
"""
from __future__ import annotations

import sys
from pathlib import Path
from typing import Any

try:
    # pylint: disable=import-error
    from autogen_agentchat.agents import AssistantAgent
    from autogen_agentchat.task import MaxMessageTermination
    from autogen_agentchat.teams import RoundRobinGroupChat
    from autogen_ext.models.openai import OpenAIChatCompletionClient

    AUTOGEN_AVAILABLE = True
except ImportError:
    AUTOGEN_AVAILABLE = False

from ..common.exceptions import MsticpyImportExtraError, MsticpyUserConfigError
from .config_utils import get_autogen_config_from_msticpyconfig

if sys.version_info < (3, 9):
    import importlib_resources as pkg_resources
else:
    import importlib.resources as pkg_resources


def find_rst_files() -> list[str]:
    """
    Find all .rst files in the docs/source directory of 'msticpy' package.

    Returns
    -------
    list of str
        List of paths to .rst files in the docs/source directory.

    """
    # Get the path to the docs/source directory of the package
    mp_resources = pkg_resources.files("msticpy")
    docs_path = Path(mp_resources).parent / "docs" / "source"  # type: ignore[arg-type]

    # Find all .rst files in the docs/source directory
    rst_files = list(str(fp) for fp in docs_path.rglob("*.rst"))

    return rst_files


def get_retrieval_assistant_agent(system_message: str = "") -> "AssistantAgent":
    """
    Create and return an AssistantAgent.

    Parameters
    ----------
    system_message : str, optional
        Custom system message for the assistant.

    Returns
    -------
    AssistantAgent
        Configured AssistantAgent instance.

    Raises
    ------
    MsticpyImportExtraError
        If autogen packages are not installed.

    """
    if not AUTOGEN_AVAILABLE:
        raise MsticpyImportExtraError(
            "Autogen packages not installed. Only supported on Python 3.10 or later."
            "Install with 'pip install msticpy[aiagents]' or "
            "'pip install autogen-agentchat autogen-ext[retrievechat]'",
            title="Error importing autogen packages",
            extra="aiagents",
        )

    if not system_message:
        system_message = (
            "You are a helpful assistant to security analysts using MSTICpy."
        )

    autogen_config = get_autogen_config_from_msticpyconfig()
    model_client = _create_model_client(autogen_config)

    return AssistantAgent(
        name="assistant",
        model_client=model_client,
        system_message=system_message,
    )


def _create_model_client(
    autogen_config: dict[str, Any]
) -> "OpenAIChatCompletionClient":
    """
    Create an OpenAI model client from autogen configuration.

    Parameters
    ----------
    autogen_config : dict[str, Any]
        Autogen configuration dictionary.

    Returns
    -------
    OpenAIChatCompletionClient
        Configured model client.

    Raises
    ------
    MsticpyUserConfigError
        If configuration is invalid.

    """
    if not AUTOGEN_AVAILABLE:
        raise MsticpyImportExtraError(
            "Autogen packages not installed. "
            "Install with 'pip install msticpy[aiagents]' or "
            "'pip install autogen-agentchat autogen-ext[retrievechat]'",
            title="Error importing autogen packages",
            extra="aiagents",
        )
    if "config_list" not in autogen_config or not isinstance(
        autogen_config["config_list"], list
    ):
        raise MsticpyUserConfigError(
            "Invalid Autogen configuration: 'config_list' not found or not a list!"
        )

    if not autogen_config["config_list"]:
        raise MsticpyUserConfigError(
            "Invalid Autogen configuration: 'config_list' is empty!"
        )

    default_config = autogen_config["config_list"][0]

    if "model" not in default_config:
        raise MsticpyUserConfigError(
            "Invalid Autogen configuration: 'model' not found in config!"
        )

    model = default_config["model"]
    api_key = default_config.get("api_key")
    base_url = default_config.get("base_url")

    return OpenAIChatCompletionClient(
        model=model,
        api_key=api_key,
        base_url=base_url,
    )


def get_retrieval_user_proxy_agent(
    customized_prompt: str | None = None,
) -> "AssistantAgent":
    """
    Create and return an AssistantAgent configured for RAG.

    Note: In autogen 0.4+, RAG functionality is handled through
    extensions and requires additional setup with vector databases.
    This function returns a basic AssistantAgent.

    Parameters
    ----------
    customized_prompt : str | None, optional
        Custom prompt for the assistant agent, by default None.

    Returns
    -------
    AssistantAgent
        Configured AssistantAgent instance.

    Raises
    ------
    MsticpyUserConfigError
        Autogen settings not found in msticpyconfig.yaml configuration
    MsticpyImportExtraError
        If autogen packages are not installed.

    """
    if not AUTOGEN_AVAILABLE:
        raise MsticpyImportExtraError(
            "Autogen packages not installed. "
            "Install with 'pip install msticpy[aiagents]' or "
            "'pip install autogen-agentchat autogen-ext[retrievechat]'",
            title="Error importing autogen packages",
            extra="aiagents",
        )
    autogen_config = get_autogen_config_from_msticpyconfig()
    model_client = _create_model_client(autogen_config)

    system_message = customized_prompt or (
        "You are a helpful assistant with access to MSTICpy documentation. "
        "Answer questions about MSTICpy based on the provided context."
    )

    return AssistantAgent(
        name="ragproxyagent",
        model_client=model_client,
        system_message=system_message,
    )


async def ask_question(
    assistant_agent: "AssistantAgent",
    user_proxy_agent: "AssistantAgent",
    question: str,
) -> str:
    """
    Ask a question using the assistant and user proxy agents.

    Parameters
    ----------
    assistant_agent : AssistantAgent
        The assistant agent to use.
    user_proxy_agent : AssistantAgent
        The user proxy agent to use.
    question : str
        The question to ask.

    Returns
    -------
    str
        The response from the assistant.

    Raises
    ------
    MsticpyImportExtraError
        If autogen packages are not installed.

    """
    if not AUTOGEN_AVAILABLE:
        raise MsticpyImportExtraError(
            "Autogen packages not installed. "
            "Install with 'pip install msticpy[aiagents]' or "
            "'pip install autogen-agentchat autogen-ext[retrievechat]'",
            title="Error importing autogen packages",
            extra="aiagents",
        )
    # Create a team with both agents
    team = RoundRobinGroupChat(
        [user_proxy_agent, assistant_agent],
        termination_condition=MaxMessageTermination(max_messages=10),
    )

    # Run the team
    result = await team.run(task=question)

    # Return the last message content
    if result.messages:
        return result.messages[-1].content  # type: ignore[union-attr]
    return "No response generated."
