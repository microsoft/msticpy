# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Tools lib init."""
import re
from pathlib import Path

# pylint: disable=invalid-name
VER_PATH = "../msticpy/_version.py"

for check_path in Path(__file__).absolute().parents:
    if check_path.joinpath(VER_PATH).is_file():
        ver_text = check_path.joinpath(VER_PATH).read_text()
        break
else:
    ver_text = ""

v_match = re.search(r'^VERSION\s*=\s*[\'"]([^\'"]*)[\'"]', ver_text, re.MULTILINE)
__version__ = v_match.group(1) if v_match else "no version"

VERSION = __version__
