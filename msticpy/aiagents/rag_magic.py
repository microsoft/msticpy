# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Provides RAG Magic functionalities for MSTICpy."""

import io
from contextlib import redirect_stdout

from IPython.core.magic import Magics, cell_magic, magics_class

from msticpy.aiagents.assistant_agent import AssistantAgent
from msticpy.aiagents.rag_agent import RagAgent

CUSTOM_SYSTEM_MESSAGE = """
    Answer the question. If you don't know the answer, just say that you don't know, don't try to make
    up an answer.

    Question: <<<{question}>>>

    Additional instructions:
    1. When answering give preference to functions that are exposed
    via a pandas extension method (e.g. `df.mp_plot.<func_name>()`) or
    a pivot function (e.g. `IpAddress.<func_name>()`) over a standard function.

    2. Supply any relevant code examples and include these with your answer
    (ensure you delimit the code with triple backticks and add the language name).

    3. Examine your answer and ensure that you are not using functions imported
    from a deprecated location. Your examples must not include any functions or
    modules imported from the paths 'msticpy.sectools' or 'msticpy.nbtools'.

    4. The URL reference to the source document is included in the "url" property
    of the retrieved document metadata. Try to include a URL reference
    to the source documents.

    5. Do not infer the existence of msticpy methods or classes that you have not seen in your context. If you are
    asked about something and you cannot find an exact match for the term that you being are asked for,
    clarify this with the user. Say that you could not find "<unmatched-term>". If you have some close matches add those
    to the response as alternatives that the user may be searching for (e.g. "Did you mean "<alternative_1>" or "<alternative_2>)"

    Return the answer in Markdown format.
"""


@magics_class
class AutogenMagic(Magics):
    """Implement a class to provide RAG Magic functionalities for MSTICpy."""

    def __init__(self, shell):
        super().__init__(shell)
        self.assistant_instance = AssistantAgent()
        self.rag_instance = RagAgent()

    # Queries the RAG agent and processes response with Assistant agent
    def ask_magic(self, question: str, verbose: bool = False) -> str:
        """Query the RAG agent and process the response with the Assistant agent."""
        self.assistant_instance.assistant.reset()
        output = io.StringIO()
        # with redirect_stdout(output):
        chosen_message = CUSTOM_SYSTEM_MESSAGE.format(question=question)

        self.rag_instance.ragproxyagent.customized_prompt = chosen_message
        self.assistant_instance.assistant.customized_prompt = chosen_message

        rag_response = self.rag_instance.ragproxyagent.initiate_chat(
            self.assistant_instance.assistant,
            message=self.rag_instance.ragproxyagent.message_generator,
            problem=question,
            # summary_method="reflection_with_llm",
        )

        print(f"\nQuestion: {question}")
        print(f"\nAnswer: {rag_response.summary}")

        return rag_response

    @cell_magic
    def ask(self, line: str, cell: str):
        """
        Enable the user to ask a question to the RAG agent using a cell magic function.

        It parses the line to check for the --v flag and calls the ask_magic method
        with the cell content as the question.

        Parameters
        ----------
        line : str
            The line of code following the cell magic command. Used to check for the --v flag.
        cell : str
            The content of the cell. This is used as the question to ask the RAG agent.

        Example Usage
        -------------
        To ask a question to the RAG agent, use the cell magic
        command followed by the question in the cell.
        For example:

        %%ask
        Which msticpy module contains the code related to visualizing network graphs?

        If you want the RAG agent to provide a verbose response, include the --v flag. For example:

        %%ask --v
        Which msticpy module contains the code related to visualizing network graphs?
        """
        args = line.split()
        verbose_flag = "--v" in args

        self.ask_magic(cell)


# Register the magic class with IPython
def load_ipython_extension(ipython):
    """Register the magic class with IPython."""
    ipython.register_magics(AutogenMagic)
