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

from tools.toollib.url_checker import check_md_document, check_html_docs
from tools.toollib.url_checker_async import check_docs

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
def test_doc_pages_doc_links():
    results = check_docs("./docs", recurse=False)
    page_errors = []
    for page, result_dict in results.items():
        for result in result_dict.values():
            if result.status == 404:
                page_errors.append(f"{result.status} - {result.url}")
    if page_errors:
        print("Please fix the following 404 Errors:")
        for page in page_errors:
            print(page)
    assert not page_errors
