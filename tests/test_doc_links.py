# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test documentation links."""
import sys
import os
from pathlib import Path

import pytest

from ..tools.toollib.url_checker import check_md_document, check_html_docs

DOC_ROOT = "docs."
HTML_PATH = "build/html"


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_readme_md():
    readme_file = "README.md"
    if Path(".").absolute().joinpath(readme_file).is_file:
        readme_file = Path(".").absolute().joinpath(readme_file)
    else:
        for check_path in Path(__file__).absolute().parents:
            if check_path.joinpath(readme_file).is_file():
                readme_file = check_path.joinpath(readme_file)
                break
        else:
            assert Path(".").absolute().joinpath(readme_file).is_file

    results = check_md_document(str(readme_file))
    page_not_found = [p for p in results.values() if p.status == 404]
    if page_not_found:
        print("Please fix the following 404 Errors:")
        for page in page_not_found:
            print(page)
    assert not page_not_found


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_html_doc_links():

    html_doc_root = Path(DOC_ROOT) / Path(HTML_PATH)

    results = check_html_docs(str(html_doc_root), recurse=True)
    all_errs = []
    for doc, result in results.items():
        page_errs = [p for p in result.values() if p.status == 404]
        if page_errs:
            print(f"Document {doc} has 404 errors:")
            for page_err in page_errs:
                print(page_err)
            all_errs = all_errs + page_errs
    assert not all_errs
