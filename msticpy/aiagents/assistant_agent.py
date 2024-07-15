# pylint: disable=too-few-public-methods
"""Implements the assistant agent for MSTICpy."""
from autogen.agentchat.contrib.retrieve_assistant_agent import RetrieveAssistantAgent

from . import config


class AssistantAgent:
    """
    Implement an assistant agent for MSTICpy using the RetrieveAssistantAgent class.

    The assistant agent is initialized with a specific
    configuration and can be used to perform various tasks.
    """

    def __init__(self):
        """Initialize the assistant agent with a RetrieveAssistantAgent instance."""
        self.assistant = RetrieveAssistantAgent(
            name="assistant",
            system_message="You are a helpful assistant to security analysts using MSTICpy.",
            llm_config=config,
        )
