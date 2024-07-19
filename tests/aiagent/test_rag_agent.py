from pathlib import Path

from msticpy.aiagents.rag_agent import find_rst_files


def test_find_rst_files_not_empty():
    rst_files = find_rst_files()

    # Assert that the list of .rst files is not empty
    assert rst_files, "The list of .rst files is empty."


def test_find_specific_rst_file():
    # Optionally, we can also check for specific .rst files
    rst_files = find_rst_files()

    rst_file_names = [Path(file).name for file in rst_files]
    assert "DataAcquisition.rst" in rst_file_names, "The specific file is not found."
