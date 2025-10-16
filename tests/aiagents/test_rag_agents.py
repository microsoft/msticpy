"""Tests for aiagents."""

import sys
from pathlib import Path

import pytest

if sys.version_info >= (3, 10):
    from msticpy.aiagents.rag_agents import find_rst_files


@pytest.mark.skipif(
    sys.version_info <= (3, 10), reason="Older versions of Python not supported."
)
def test_find_rst_files_not_empty():
    """Test for presence of rst doc files."""
    rst_files = find_rst_files()

    # Assert that the list of .rst files is not empty
    assert rst_files, "The list of .rst files is empty."


@pytest.mark.skipif(
    sys.version_info <= (3, 10), reason="Older versions of Python not supported."
)
def test_find_specific_rst_file():
    """Look for a specific doc file that should be present."""
    # Optionally, we can also check for specific .rst files
    rst_files = find_rst_files()

    rst_file_names = [Path(file).name for file in rst_files]
    assert "DataAcquisition.rst" in rst_file_names, "The specific file is not found."
