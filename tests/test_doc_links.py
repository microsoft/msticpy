# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""test documentation links."""
import os
from pathlib import Path

import pytest
from tools.toollib.url_checker import check_md_document
from tools.toollib.url_checker_async import check_docs

from .unit_test_lib import get_test_data_path

DOC_ROOT = "docs."
HTML_PATH = "build/html"


@pytest.fixture(scope="session")
def ignored_urls():
    tests_path = get_test_data_path().parent
    content = tests_path.joinpath("ignored_uri_links.txt").read_text(encoding="utf-8")
    return [line for line in content.split("\n") if line and not line.startswith("#")]


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
        print(f"File: {readme_file}")
        print("Please fix the following 404 Errors:")
        for page in page_not_found:
            print(page)
    assert not page_not_found


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
def test_doc_pages_doc_links(ignored_urls):
    path_rewrite = {
        "build\\html": "source",
        "build/html": "source",
        ".html": ".rst",
    }
    results = check_docs(
        "./docs/build/html",
        recurse=True,
        ignore_uris=ignored_urls,
    )
    page_errors = []
    for page, result_dict in results.items():
        page_errors.extend(
            f"{result.status} - {result.url}"
            for result in result_dict.values()
            if result.status == 404
        )

    if page_errors:
        print("Please fix the following 404 Errors:")
        for page in page_errors:
            print(page)
    assert not page_errors
