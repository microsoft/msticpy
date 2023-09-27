# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query files test class."""
from pathlib import Path

import pytest_check as check
import yaml
from jsonschema import Draft7Validator

from ...unit_test_lib import get_queries_schema, get_test_data_path

_QUERIES_SCHEMA = get_queries_schema()


def validate_queries_file_structure(query_file: Path, expected: bool = True):
    """Test if query files have a valid structure."""
    with query_file.open(mode="r", encoding="utf-8") as queries:
        queries_yaml = yaml.safe_load(queries)
        if expected:
            check.is_true(
                Draft7Validator(_QUERIES_SCHEMA).is_valid(queries_yaml),
                msg=f"File {query_file} is not a valid query file",
            )
        else:
            check.is_false(
                Draft7Validator(_QUERIES_SCHEMA).is_valid(queries_yaml),
                msg=f"File {query_file} was expected to be invalid but is a valid query file",
            )


def test_valid_queries():
    """Test load default settings."""
    valid_queries_path = get_test_data_path() / "queries" / "valid"
    for valid_query in valid_queries_path.rglob("*yaml"):
        validate_queries_file_structure(valid_query)


def test_invalid_queries():
    """Test load default settings."""
    valid_queries_path = get_test_data_path() / "queries" / "invalid"
    for valid_query in valid_queries_path.rglob("*yaml"):
        validate_queries_file_structure(valid_query, expected=False)
