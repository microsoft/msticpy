# pylint: disable=too-few-public-methods
"""Implements the RAG (Retrieval-Augmented Generation) agent for MSTICpy."""

import importlib.resources as pkg_resources
from pathlib import Path

from autogen.agentchat.contrib.retrieve_user_proxy_agent import RetrieveUserProxyAgent

from . import config


def find_rst_files():
    """Find all .rst files in the docs/source directory of 'msticpy' package."""
    # Get the path to the docs/source directory of the package
    docs_path = Path(pkg_resources.files("msticpy")).parent / "docs" / "source"

    # # Find all .rst files in the docs/source directory
    rst_files = list(docs_path.rglob("*.rst"))

    return rst_files


class RagAgent:
    """
    Implement a RAG (Retrieval-Augmented Generation) agent.

    Attributes
    ----------
    ragproxyagent : RetrieveUserProxyAgent
        An instance of the RetrieveUserProxyAgent class.
    """

    def __init__(self):
        """Initialize the rag_agent with a RetrieveUserProxyAgent instance."""
        rst_files = find_rst_files()
        self.ragproxyagent = RetrieveUserProxyAgent(
            name="ragproxyagent",
            human_input_mode="NEVER",
            max_consecutive_auto_reply=3,
            retrieve_config={
                "task": "qa",
                "docs_path": rst_files,
                "chunk_token_size": 2000,
                "model": config["model"],
                "vector_db": "chroma",
                "collection_name": "MSTICpy_Docs",
                "overwrite": True,  # set to True if you want to overwrite an existing collection
            },
            code_execution_config=False,  # set to False if you don't want to execute the code
        )
