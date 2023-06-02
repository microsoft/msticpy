# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test QueryEditor docstring."""
import os
from pathlib import Path

import pytest
from pydantic import ValidationError

from msticpy.config.query_editor import (
    _PARAM_OPTIONS,
    MetadataEditWidget,
    QueryEditor,
    QueryEditWidget,
    QueryParameterEditWidget,
    load_queries_from_yaml,
)
from msticpy.data import queries
from msticpy.data.core.query_template import (
    Query,
    QueryArgs,
    QueryCollection,
    QueryDefaults,
    QueryMetadata,
)

__author__ = "Ian Hellen"

_QUERY_PATH = Path(queries.__path__[0])

_TEST_FILES = [
    str(Path(_QUERY_PATH, "mssentinel/kql_sent_az_dns.yaml")),
    str(Path(_QUERY_PATH, "mssentinel/kql_sent_net.yaml")),
    str(Path(_QUERY_PATH, "mssentinel/kql_sent_o365.yaml")),
    str(Path(_QUERY_PATH, "mssentinel/kql_sent_winevent_proc.yaml")),
]

# pylint: disable=redefined-outer-name


@pytest.fixture
def query():
    """Return a Query object."""
    return Query(
        description="A test query",
        metadata={},
        args=QueryArgs("SELECT * FROM test_table WHERE test_column = ?"),
        parameters={},
    )


@pytest.fixture
def query_defaults():
    """Return a QueryDefaults object."""
    return QueryDefaults(
        metadata={},
        parameters={},
    )


def _add_test_param(editor):
    editor.parameter_name_widget.value = "test_param"
    editor.description_widget.value = "A test parameter"
    editor.type_widget.value = _PARAM_OPTIONS[0]
    editor.default_widget.value = "test_value"
    editor.default_reqd_widget.value = True


def _add_test_parameter2(editor):
    editor.parameter_name_widget.value = "test_param2"
    editor.description_widget.value = "A test parameter2"
    editor.type_widget.value = _PARAM_OPTIONS[1]
    editor.default_widget.value = "test_value2"
    editor.default_reqd_widget.value = False


def test_add_parameter(query):
    """Test adding a parameter."""
    editor = QueryParameterEditWidget(query)
    editor.add_parameter(None)
    _add_test_param(editor)
    editor.save_parameter(None)

    assert len(editor.param_container.parameters) == 1
    assert (
        editor.param_container.parameters["test_param"].description
        == "A test parameter"
    )
    assert editor.param_container.parameters["test_param"].datatype == _PARAM_OPTIONS[0]
    assert editor.param_container.parameters["test_param"].default == "test_value"


def test_select_parameter(query):
    """Test selecting a parameter."""
    editor = QueryParameterEditWidget(query)
    editor.add_parameter(None)
    _add_test_param(editor)
    editor.save_parameter(None)
    editor.add_parameter(None)
    _add_test_parameter2(editor)
    editor.save_parameter(None)

    assert len(editor.param_container.parameters) == 2
    editor.parameter_dropdown.value = "test_param"

    # assert widgets are populated with initial parameter values
    assert editor.parameter_name_widget.value == "test_param"
    assert editor.description_widget.value == "A test parameter"
    assert editor.type_widget.value == _PARAM_OPTIONS[0]
    assert editor.default_widget.value == "test_value"


def test_remove_parameter(query):
    """Test removing a parameter."""
    editor = QueryParameterEditWidget(query)
    editor.add_parameter(None)
    _add_test_param(editor)
    editor.save_parameter(None)
    assert len(editor.param_container.parameters) == 1
    editor.delete_parameter(None)
    assert len(editor.param_container.parameters) == 0


def test_edit_parameter(query):
    """Test editing a parameter."""
    editor = QueryParameterEditWidget(query)
    editor.add_parameter(None)
    _add_test_param(editor)
    editor.save_parameter(None)
    assert len(editor.param_container.parameters) == 1

    editor.description_widget.value = "A test parameter2"
    editor.type_widget.value = _PARAM_OPTIONS[1]
    editor.default_widget.value = "test_value2"
    editor.default_reqd_widget.value = False
    editor.save_parameter(None)
    assert len(editor.param_container.parameters) == 1
    assert (
        editor.param_container.parameters["test_param"].description
        == "A test parameter2"
    )
    assert editor.param_container.parameters["test_param"].datatype == _PARAM_OPTIONS[1]
    assert editor.param_container.parameters["test_param"].default is None


@pytest.fixture
def metadata_editor():
    return MetadataEditWidget()


@pytest.fixture
def query_metadata():
    return QueryMetadata(
        version=1,
        description="Test description",
        data_environments=["AzureSentinel"],
        data_families=["WindowsSecurity"],
        cluster="https://contoso.com",
        database="test_db",
        clusters=["https://kusto.contoso.com", "https://kusto2.contoso.com"],
        cluster_groups=["Contoso"],
        tags=["test", "test2"],
        data_source="Azure Sentinel",
    )


def test_metadata_editor_edit(metadata_editor, query_metadata):
    """Test editing metadata."""
    metadata_editor.version_widget.value = query_metadata.version
    metadata_editor.description_widget.value = query_metadata.description
    metadata_editor.data_env_widget.value = query_metadata.data_environments
    metadata_editor.data_families_widget.value = ",".join(query_metadata.data_families)
    metadata_editor.cluster_widget.value = query_metadata.cluster
    metadata_editor.database_widget.value = query_metadata.database
    metadata_editor.clusters_widget.value = "\n".join(query_metadata.clusters)
    metadata_editor.cluster_groups_widget.value = "\n".join(
        query_metadata.cluster_groups
    )
    metadata_editor.tags_widget.value = ", ".join(query_metadata.tags)
    metadata_editor.data_source_widget.value = query_metadata.data_source
    metadata_editor.save_metadata(None)
    assert metadata_editor.metadata.version == query_metadata.version
    assert metadata_editor.metadata.description == query_metadata.description
    assert (
        metadata_editor.metadata.data_environments == query_metadata.data_environments
    )
    assert metadata_editor.metadata.data_families == query_metadata.data_families
    assert metadata_editor.metadata.cluster == query_metadata.cluster
    assert metadata_editor.metadata.database == query_metadata.database
    assert metadata_editor.metadata.clusters == query_metadata.clusters
    assert metadata_editor.metadata.cluster_groups == query_metadata.cluster_groups
    assert metadata_editor.metadata.tags == query_metadata.tags
    assert metadata_editor.metadata.data_source == query_metadata.data_source


@pytest.fixture
def query_collection(query_metadata):
    """Return a QueryCollection instance."""
    return QueryCollection(
        file_name="test_file",
        metadata=query_metadata,
    )


@pytest.fixture
def query_editor(query_collection):
    """Return a QueryEditor instance."""
    return QueryEditWidget(query_collection)


def _add_new_query(query_editor, arg1, arg2):
    query_editor.add_query(None)
    query_editor.name_widget.value = arg1
    query_editor.description_widget.value = arg2
    query_editor.save_query(None)


def test_add_query(query_editor):
    """Test adding a query."""
    _add_new_query(query_editor, "test_query", "This is a test query")
    # query_editor.query_select_widget.value = "New Query"

    assert "test_query" in query_editor.query_collection.sources


def test_select_query(query_editor):
    """Test selecting a query."""
    _add_new_query(query_editor, "test_query", "This is a test query")
    _add_new_query(query_editor, "test_query2", "This is a test query2")
    query_editor.query_select_widget.value = "test_query"

    assert query_editor.description_widget.value == "This is a test query"


def test_delete_query(query_editor):
    """Test deleting a query."""
    _add_new_query(query_editor, "test_query", "This is a test query")
    _add_new_query(query_editor, "test_query2", "This is a test query2")
    query_editor.query_select_widget.value = "test_query"
    query_editor.delete_query(None)
    assert len(query_editor.query_collection.sources) == 1
    assert query_editor.query_select_widget.value == "test_query2"


def test_query_file_editor_init():
    """Test the query template editor."""
    editor = QueryEditor(_TEST_FILES[0])

    assert editor.filename_widget.value == _TEST_FILES[0]
    assert len(editor.query_collection.sources) == 3

    query_collection = load_queries_from_yaml(_TEST_FILES[0])
    editor = QueryEditor(query_collection)
    assert len(editor.query_collection.sources) == 3


# pylint: disable=protected-access
def test_query_file_editor_file_load_and_save(tmp_path):
    """Test the query template editor."""
    editor = QueryEditor()
    editor.filename_widget.value = _TEST_FILES[0]
    editor._open_file(None)

    dest_path = tmp_path / "test.yaml"
    editor.filename_widget.value = str(dest_path)
    editor._save_file(None)
    assert dest_path.is_file()
    query_coll_orig = load_queries_from_yaml(_TEST_FILES[0])
    query_coll_dest = load_queries_from_yaml(str(dest_path))
    query_coll_orig.file_name = "test"
    query_coll_dest.file_name = "test"
    assert query_coll_orig == query_coll_dest


def test_query_editor_unsaved_changes(tmp_path):
    """Test the query template editor."""
    editor = QueryEditor()
    editor.filename_widget.value = _TEST_FILES[0]
    editor._open_file(None)

    # make a change
    editor.metadata_editor.version_widget.value = 2
    editor.metadata_editor.save_metadata(None)
    assert editor._unsaved_changes()

    editor = QueryEditor()
    editor.filename_widget.value = _TEST_FILES[0]
    editor._open_file(None)

    editor.query_editor.add_query(None)
    _add_new_query(editor.query_editor, "test_query", "This is a test query")
    editor.query_editor.save_query(None)
    assert editor._unsaved_changes()

    editor = QueryEditor()
    editor.filename_widget.value = _TEST_FILES[0]
    editor._open_file(None)

    editor.default_param_editor.add_parameter(None)
    _add_test_param(editor.default_param_editor)
    editor.default_param_editor.save_parameter(None)
    assert editor._unsaved_changes()


def test_query_editor_block_unsaved_changes(tmp_path):
    """Test the query template editor."""
    editor = QueryEditor()
    editor.filename_widget.value = _TEST_FILES[0]
    editor._open_file(None)

    query_collection = editor.query_collection
    # make a change
    editor.metadata_editor.version_widget.value = 2
    editor.metadata_editor.save_metadata(None)
    assert editor._unsaved_changes()

    editor.filename_widget.value = _TEST_FILES[1]
    editor._open_file(None)
    assert editor._unsaved_changes()
    assert editor.query_collection == query_collection

    # allow changes to be abandoned
    editor.ignore_changes.value = True
    editor.filename_widget.value = _TEST_FILES[1]
    editor._open_file(None)
    assert not editor._unsaved_changes()
    assert editor.query_collection != query_collection


def _get_query_files():
    return [(file.name, file) for file in _QUERY_PATH.rglob("*.yaml")]


@pytest.mark.skipif(
    not os.environ.get("MSTICPY_TEST_NOSKIP"), reason="Skipped for local tests."
)
@pytest.mark.parametrize(
    "file_name, query_file",
    _get_query_files(),
    ids=[name for name, _ in _get_query_files()],
)
def test_query_file_schema(file_name, query_file):
    """Test the query schemas for all checked-in files."""
    folder = query_file.parent.name
    try:
        load_queries_from_yaml(query_file)
    except ValidationError as valid_err:
        print(f"{folder}/{file_name}")
        print(valid_err)
        raise
