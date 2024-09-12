# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Integrates MSTICpy with IPython custom magic commands for retrieval-augmented generation."""

import io
from contextlib import redirect_stdout

from IPython.core.magic import Magics, cell_magic, magics_class
from IPython.display import display_markdown

from .rag_agents import (
    ask_question,
    get_retrieval_assistant_agent,
    get_retrieval_user_proxy_agent,
)


@magics_class
class DocsRagMagic(Magics):
    """Implement a class to provide RAG Magic functionalities for MSTICpy."""

    def __init__(self, shell):
        super().__init__(shell)
        self.assistant_agent = get_retrieval_assistant_agent()
        self.user_proxy_agent = get_retrieval_user_proxy_agent()

    @cell_magic
    def ask(self, _, cell: str):
        """
        Enable the user to ask a question to the RAG agent using a cell magic function.

        It calls the ask_magic method with the cell content as the question.

        Parameters
        ----------
        cell : str
            The content of the cell. This is used as the question to ask the RAG agent.

        Example Usage
        -------------
        To ask a question to the RAG agent, use the cell magic
        command followed by the question in the cell.
        For example:

        %%ask
        Which msticpy module contains the code related to visualizing network graphs?
        """
        question = cell.strip()
        output = io.StringIO()
        with redirect_stdout(output):
            response = ask_question(
                self.assistant_agent,
                self.user_proxy_agent,
                question=question,
            )

        answer = response.summary

        display_markdown(f"\n**Question**: {question}", raw=True)
        display_markdown(f"\n**Answer**: {answer}", raw=True)


# Register the magic class with IPython
def load_ipython_extension(ipython):
    """Register the magic class with IPython."""
    ipython.register_magics(DocsRagMagic)
