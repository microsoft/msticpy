# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Code cleanup functions to re-format obfuscated code."""
import re

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


def format_powershell(code: str) -> str:
    """
    Cleanup obfuscated powershell.

    Parameters
    ----------
    code : str
        Powershell code segment.

    Returns
    -------
    str
       Reformatted code.

    Notes
    -----
    This function is purely for displaying intentionally obfuscated
    code, it will likely not return executable code.

    See Also
    --------
    msticpy.vis.code_view

    """
    # pre-process: change to lowercase and remove redundant string concat
    cleaned_str = code.casefold().replace("'+'", "")

    # Use regex for more complex replacements
    # regex patterns to replace
    patterns = {
        ";": "\n",
        "{": "\n{\n",
        "}": "\n}\n",
        "`": "",
    }
    # replace only if not in a quoted string
    search_pattern = r"(\"|')[^\"']+\"'|(\{find})"

    # iterate through patterns replacing each one.
    for pattern, repl in patterns.items():
        cleaned_str = re.sub(search_pattern.format(find=pattern), repl, cleaned_str)

    # re-process lines
    cleaned_lines = cleaned_str.splitlines()
    indent = 0
    output = []
    for line in cleaned_lines:
        # add indents if line starts with "{" and de-indent on "}"
        if not line:
            continue  # skip blank lines
        if line.startswith("}"):
            indent -= 4
        output.append(" " * indent + line)
        if line.startswith("{"):
            indent += 4

    return "\n".join(output)
