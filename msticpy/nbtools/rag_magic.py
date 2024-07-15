"""Provides RAG Magic functionalities for MSTICpy."""

from IPython.core.magic import Magics, cell_magic, magics_class

from msticpy.aiagents.assistant_agent import AssistantAgent
from msticpy.aiagents.rag_agent import RagAgent

# Define string constants
CUSTOM_SYSTEM_MESSAGE = """You're a retrieve augmented chatbot for MSTICpy.
You answer user's questions based on your own knowledge and the context provided by the user.
If you can't answer the question with or without the current context, you should reply exactly
`I am unable to find relevant context to answer your question.`.
You must give as short an answer as possible. Do not output any reasoning. Only return your best response, which may not be the most recent.
Following your best response, list your sources, including file title and hyperlink if available.
Terminate immediately once you have your best response.
Do not allow the ragproxyagent to reply again if not needed.

User's Question is: {}"""

VERBOSE_CUSTOM_SYSTEM_MESSAGE = """You're a retrieve augmented chatbot for MSTICpy.
You answer user's questions based on your own knowledge and the context provided by the user.
If you can't answer the question with or without the current context, you should reply exactly
`I am unable to find relevant context to answer your question.`.

User's Question is: {}"""


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
        print(f"Question: {question}")

        # Toggle between custom messages depending on flag
        chosen_message = (
            VERBOSE_CUSTOM_SYSTEM_MESSAGE.format(question)
            if verbose
            else CUSTOM_SYSTEM_MESSAGE.format(question)
        )

        self.rag_instance.ragproxyagent.customized_prompt = chosen_message
        self.assistant_instance.assistant.customized_prompt = chosen_message

        rag_response = self.rag_instance.ragproxyagent.initiate_chat(
            self.assistant_instance.assistant,
            message=self.rag_instance.ragproxyagent.message_generator,
            problem=question,
            summary_method="reflection_with_llm",
        )
        return rag_response

    @cell_magic
    def ask(self, line: str, cell: str):
        """Parse the line to check for the --v flag and call the ask_magic method."""
        args = line.split()
        verbose_flag = "--v" in args

        self.ask_magic(cell, verbose=verbose_flag)


# Register the magic class with IPython
def load_ipython_extension(ipython):
    """Register the magic class with IPython."""
    ipython.register_magics(AutogenMagic)


# Example Usage:
# %%ask
# Which msticpy module contains the code related to visualizing network graphs?

# Example Verbose Usage:
# %%ask --v
# Which msticpy module contains the code related to visualizing network graphs?
