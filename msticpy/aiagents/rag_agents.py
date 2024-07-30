# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
# pylint: disable=too-few-public-methods
"""Implements the RAG (Retrieval-Augmented Generation) agent for MSTICpy."""

import importlib.resources as pkg_resources
from pathlib import Path
from typing import Optional

from autogen.agentchat.chat import ChatResult
from autogen.agentchat.contrib.retrieve_assistant_agent import RetrieveAssistantAgent
from autogen.agentchat.contrib.retrieve_user_proxy_agent import RetrieveUserProxyAgent

from .._version import VERSION
from .config_utils import get_autogen_config_from_msticpyconfig


def find_rst_files():
    """Find all .rst files in the docs/source directory of 'msticpy' package."""
    # Get the path to the docs/source directory of the package
    docs_path = Path(pkg_resources.files("msticpy")).parent / "docs" / "source"

    # Find all .rst files in the docs/source directory
    rst_files = list(str(fp) for fp in docs_path.rglob("*.rst"))

    return rst_files


def get_retrieval_assistant_agent(system_message: str = "") -> RetrieveAssistantAgent:
    """_summary_

    Parameters
    ----------
    system_message : str, optional
        _description_, by default ""

    Returns
    -------
    RetrieveAssistantAgent
        _description_
    """
    if not system_message:
        system_message = (
            "You are a helpful assistant to security analysts using MSTICpy."
        )
    return RetrieveAssistantAgent(
        name="assistant",
        system_message=system_message,
        llm_config=get_autogen_config_from_msticpyconfig(),
    )


def get_retrieval_user_proxy_agent(
    max_consecutive_auto_reply: int = 1,
) -> RetrieveUserProxyAgent:
    """_summary_

    Parameters
    ----------
    max_consecutive_auto_reply : int, optional
        _description_, by default 1

    Returns
    -------
    RetrieveUserProxyAgent
        _description_
    """
    rst_files = find_rst_files()
    autogen_config = get_autogen_config_from_msticpyconfig()
    return RetrieveUserProxyAgent(
        name="ragproxyagent",
        human_input_mode="NEVER",
        max_consecutive_auto_reply=max_consecutive_auto_reply,
        retrieve_config={
            "task": "default",
            "docs_path": rst_files,
            "chunk_token_size": 2000,
            "model": autogen_config["config_list"][0]["model"],
            "vector_db": "chroma",
            "collection_name": f"MSTICpy_Docs_{VERSION}",
            "get_or_create": True,
        },
        code_execution_config=False,  # set to False if you don't want to execute the code
    )


def ask_question(
    assistant_agent: RetrieveAssistantAgent,
    user_proxy_agent: RetrieveUserProxyAgent,
    question: str,
    agent_prompt: Optional[str] = None,
) -> ChatResult:
    """_summary_

    Parameters
    ----------
    assistant_agent : RetrieveAssistantAgent
        _description_
    user_proxy_agent : RetrieveUserProxyAgent
        _description_
    question : str
        _description_
    agent_prompt : Optional[str], optional
        _description_, by default None

    Returns
    -------
    ChatResult
        _description_
    """
    assistant_agent.reset()
    if agent_prompt:
        assistant_agent.customized_prompt = agent_prompt
    return user_proxy_agent.initiate_chat(
        assistant_agent, message=user_proxy_agent.message_generator, problem=question
    )
