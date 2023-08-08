# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Query Editor."""

from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any, Dict, Optional, Union, cast

import ipywidgets as widgets
import yaml
from IPython.display import display

from .._version import VERSION
from ..data.core.query_defns import DataEnvironment, DataFamily
from ..data.core.query_template import (
    Query,
    QueryArgs,
    QueryCollection,
    QueryDefaults,
    QueryMetadata,
    QueryParameter,
)

__version__ = VERSION
__author__ = "Ian Hellen"

# pylint: disable=too-many-instance-attributes, too-many-lines


class IPyDisplayMixin:
    """IPython display mixin class."""

    def display(self):
        """Display the interactive widgets."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()


_PARAM_OPTIONS = ("str", "datetime", "list", "int")
_DESC_WIDTH = "120px"


def txt_fmt(width: str = "70%") -> Dict:
    """Return a dictionary with layout and style for a text widget.

    Parameters
    ----------
    width : str, optional
        The width of the widget, by default "70%"
    height : str, optional
        The height of the widget, by default "20px"

    Returns
    -------
    Dict
        A dictionary with layout and style for a text widget.

    """
    return {
        "layout": widgets.Layout(width=width),
        "style": {"description_width": _DESC_WIDTH},
    }


def txtarea_fmt(width: str = "70%", height: str = "150px") -> Dict:
    """Return a dictionary with layout and style for a textarea widget.

    Parameters
    ----------
    width : str, optional
        The width of the widget, by default "70%"
    height : str, optional
        The height of the widget, by default "150px"

    Returns
    -------
    Dict
        A dictionary with layout and style for a textarea widget.

    """
    return {
        "layout": widgets.Layout(width=width, height=height),
        "style": {"description_width": _DESC_WIDTH},
    }


def sel_fmt(width: str = "40%", height: str = "150px") -> Dict:
    """Return a dictionary with layout and style for a select widget.

    Parameters
    ----------
    width : str, optional
        The width of the widget, by default "40%"
    height : str, optional
        The height of the widget, by default "150px"

    Returns
    -------
    Dict
        A dictionary with layout and style for a select widget.

    """
    return {
        "layout": widgets.Layout(width=width, height=height),
        "style": {"description_width": _DESC_WIDTH},
    }


def box_layout():
    """Return a dictionary with layout and style for a box widget.

    Returns
    -------
    Dict
        A dictionary with layout and style for a select widget.

    """
    return {
        "layout": widgets.Layout(
            border="1px solid black",
            margin="5px",
            padding="5px",
            style={"padding": "5px", "background-color": "red"},
        )
    }


@dataclass
class CustomChange:
    """Dummy ipywidgets change event."""

    new: Any


class QueryParameterEditWidget(IPyDisplayMixin):
    """Class to manage editing of query parameters.

    This class provides a graphical user interface for editing query parameters.
    It allows users to add, modify, and delete parameters, as well as specify
    their names, descriptions, types, default values, and whether they are mandatory.

    Attributes
    ----------
    _changed_data : bool
        A flag indicating whether the data has been changed.
    param_container : Union[Query, QueryDefaults]
        The container for the query parameters.
    parameter_dropdown : ipywidgets.Select
        A widget for selecting a parameter to edit.
    parameter_name_widget : ipywidgets.Text
        A widget for editing the name of a parameter.
    description_widget : ipywidgets.Text
        A widget for editing the description of a parameter.
    type_widget : ipywidgets.Dropdown
        A widget for selecting the type of a parameter.
    default_reqd_widget : ipywidgets.Checkbox
        A widget for indicating whether a default value is required for a parameter.

    """

    def __init__(self, container: Union[Query, QueryDefaults]):
        """Initialize the class."""
        self._changed_data = False
        self.param_container = container
        self.parameter_dropdown = widgets.Select(
            description="Parameters",
            size=5,
            options=list(
                self.param_container.parameters.keys()
                if self.param_container.parameters
                else []
            ),
            **sel_fmt(height="100px"),
        )
        # Create widgets for the Parameter fields
        self.parameter_name_widget = widgets.Text(description="Name", **txt_fmt())
        self.description_widget = widgets.Text(description="Description", **txt_fmt())
        self.type_widget = widgets.Dropdown(
            description="Type", options=_PARAM_OPTIONS, **sel_fmt(height="30px")
        )
        self.default_reqd_widget = widgets.Checkbox(description="Use a default value")
        self.default_widget = widgets.Text(description="Default Value", **txt_fmt())

        # Create buttons
        self.add_parameter_button = widgets.Button(description="New Parameter")
        self.save_parameter_button = widgets.Button(description="Save Parameter")
        self.delete_parameter_button = widgets.Button(description="Delete Parameter")

        # Attach the functions to buttons
        self.add_parameter_button.on_click(self.add_parameter)
        self.save_parameter_button.on_click(self.save_parameter)
        self.delete_parameter_button.on_click(self.delete_parameter)
        self.parameter_dropdown.observe(self.populate_widgets, names="value")

        # Create a widget for adding, editing, and deleting Parameters
        self.layout = widgets.VBox(
            [
                widgets.HBox(
                    [
                        self.parameter_dropdown,
                        widgets.VBox(
                            [
                                self.add_parameter_button,
                                self.delete_parameter_button,
                            ]
                        ),
                    ]
                ),
                widgets.VBox(
                    children=[
                        self.parameter_name_widget,
                        self.description_widget,
                        self.type_widget,
                        widgets.HBox([self.default_reqd_widget, self.default_widget]),
                        self.save_parameter_button,
                    ],
                    **box_layout(),
                ),
            ]
        )
        if self.param_container and self.param_container.parameters:
            init_change = CustomChange(new=next(iter(self.param_container.parameters)))
            self.populate_widgets(init_change)

    @property
    def changed_data(self):
        """Return True if data has changed."""
        return self._changed_data

    def reset_changed_data(self):
        """Reset changed data flag."""
        self._changed_data = False

    def set_param_container(self, container: Union[Query, QueryDefaults]):
        """Set the parameter container."""
        self.param_container = container
        if self.param_container and self.param_container.parameters:
            self.parameter_dropdown.options = list(
                self.param_container.parameters.keys()
            )
            init_change = CustomChange(new=next(iter(self.param_container.parameters)))
            self.populate_widgets(init_change)
        else:
            self.parameter_dropdown.options = []
            self._blank_parameter()

    # Define a function to add a new Parameter to the selected Query
    def add_parameter(self, button):
        """Add a new parameter."""
        del button
        # Clear the input widgets
        self._blank_parameter()
        self.parameter_name_widget.value = "new_parameter"

    def _blank_parameter(self):
        """Clear the parameter widgets."""
        self.parameter_name_widget.value = ""
        self.description_widget.value = ""
        self.type_widget.value = _PARAM_OPTIONS[0]
        self.default_widget.value = ""
        self.default_reqd_widget.value = False

    # Define a function to populate the Parameter widgets with the values of the selected Parameter
    def populate_widgets(self, change):
        """Populate parameter value in widgets."""
        parameter = self.param_container.parameters[change.new]
        self.parameter_name_widget.value = change.new
        self.description_widget.value = parameter.description
        self.type_widget.value = parameter.datatype
        self.default_reqd_widget.value = parameter.default is not None
        self.default_widget.value = parameter.default or ""

    # Define a function to edit the selected Parameter with the values from the widgets
    def save_parameter(self, button):
        """Save currently edited parameter."""
        del button
        if not self.parameter_name_widget.value:
            return
        param_name = self.parameter_name_widget.value
        parameter = QueryParameter(
            description=self.description_widget.value,
            datatype=self.type_widget.value or _PARAM_OPTIONS[0],
        )
        parameter.default = (
            self.default_widget.value if self.default_reqd_widget.value else None
        )
        self.param_container.parameters[param_name] = parameter
        self.parameter_dropdown.options = list(self.param_container.parameters.keys())
        self.parameter_dropdown.value = param_name
        self._changed_data = True

    # Define a function to delete the selected Parameter from the selected Query
    def delete_parameter(self, button):
        """Delete parameter item."""
        del button
        del self.param_container.parameters[self.parameter_dropdown.value]
        # Clear the input widgets
        self._blank_parameter()
        self._changed_data = True


def replace_in_query(
    param_name: str, param: QueryParameter, src_name: str, query: str
) -> str:
    """
    Replace a parameter in a query string with a formatted string.

    Parameters
    ----------
    param_name : str
        The name of the parameter to replace.
    param : Parameter
        The parameter object containing the datatype of the parameter.
    src_name : str
        The name of the source to replace in the query string.
    query : str
        The query string to replace the source in.

    Returns
    -------
    str
        The query string with the source replaced by the formatted parameter string.

    """
    if param.datatype == "datetime":  # type: ignore
        repl_str = f"datetime({{{param_name}}})"
    elif param.datatype == "str":
        repl_str = f'"{{{param_name}}}"'
    else:
        repl_str = f"{{{param_name}}}"
    return query.replace(src_name, repl_str)


class QueryEditWidget(IPyDisplayMixin):
    """A class for editing queries."""

    def __init__(self, query_collection: QueryCollection):
        """
        Initialize a QueryEditWidget object.

        Parameters
        ----------
        query_collection : QueryCollection
            A collection of queries.

        """
        self._changed_data = False
        self.query_collection = query_collection
        self.query_select_widget = widgets.Select(
            description="Queries",
            options=list(self.query_collection.sources.keys()),
            **sel_fmt(),
        )
        self.name_widget = widgets.Text(
            description="Name", placeholder="Python-compatible query name", **txt_fmt()
        )
        self.description_widget = widgets.Text(
            description="Description",
            placeholder="Description of query",
            **txt_fmt("90%"),
        )
        self.metadata_widget = widgets.Textarea(
            description="Query metadata", **txtarea_fmt("90%", height="70px")
        )
        self.parameters: Optional[QueryParameterEditWidget] = None
        self.query_text_widget = widgets.Textarea(
            description="Query",
            placeholder="query text with {parameter}a",
            **txtarea_fmt("90%"),
        )
        self.query_opts_widget = widgets.Accordion(
            children=[
                widgets.Label(value="No parameters"),
                self.metadata_widget,
            ],
            titles=["Query parameters", "Query metadata"],
            layout=widgets.Layout(margin="5px"),
        )
        self.query_opts_widget.set_title(0, "Query parameters")
        self.query_opts_widget.set_title(1, "Query metadata")
        self.query_opts_widget.selected_index = None
        self.add_query_button = widgets.Button(description="New Query")
        self.save_query_button = widgets.Button(description="Save Query")
        self.delete_query_button = widgets.Button(description="Delete Query")
        self.queries_widget = widgets.VBox(
            children=[
                widgets.Label(value="Query"),
                self.name_widget,
                self.description_widget,
                self.query_text_widget,
                self.query_opts_widget,
                self.save_query_button,
            ],
            **box_layout(),
        )
        self.layout = widgets.VBox(
            [
                widgets.Label(value="Queries"),
                widgets.HBox(
                    children=[
                        self.query_select_widget,
                        widgets.VBox(
                            children=[
                                self.add_query_button,
                                self.delete_query_button,
                            ],
                        ),
                    ],
                    **box_layout(),
                ),
                self.queries_widget,
            ]
        )
        self.add_query_button.on_click(self.add_query)
        self.query_select_widget.observe(self.populate_widgets, names="value")
        self.save_query_button.on_click(self.save_query)
        self.delete_query_button.on_click(self.delete_query)
        self.populate_query_dropdown(None)
        if self.query_collection.sources:
            init_change = CustomChange(new=next(iter(self.query_collection.sources)))
            self.populate_widgets(init_change)

    @property
    def changed_data(self):
        """Return True if data has changed."""
        return self._changed_data or (
            self.parameters.changed_data if self.parameters else False
        )

    def reset_changed_data(self):
        """Reset changed data flag."""
        self._changed_data = False
        if self.parameters:
            self.parameters.reset_changed_data()

    def set_query_collection(self, query_collection: QueryCollection):
        """Set the query collection."""
        self.query_collection = query_collection
        self.populate_query_dropdown(None)
        if self.query_collection.sources:
            init_change = CustomChange(new=next(iter(self.query_collection.sources)))
            self.populate_widgets(init_change)

    def populate_query_dropdown(self, change: Any) -> None:
        """
        Populate the query dropdown widget.

        Parameters
        ----------
        change : Any
            The change event.

        """
        del change
        self.query_select_widget.options = list(self.query_collection.sources.keys())

    def _populate_qry_metadata(self, query: Query) -> str:
        """
        Populate the metadata widget with the metadata of a query.

        Parameters
        ----------
        query : Query
            The query object.

        """
        return yaml.safe_dump(query.metadata) if query.metadata else ""

    def _save_qry_metadata(self, metadata: str) -> Dict[str, Any]:
        """
        Save the metadata of a query.

        Parameters
        ----------
        metadata : str
            The metadata string.

        """
        return yaml.safe_load(metadata) if metadata else {}

    def populate_widgets(self, change):
        """
        Populate the query widgets with the data of a query.

        Parameters
        ----------
        change : Any
            The change event.

        """
        query = self.query_collection.sources[change.new]
        self.name_widget.value = change.new
        self.description_widget.value = query.description
        self.metadata_widget.value = self._populate_qry_metadata(query)
        self.query_text_widget.value = self._fmt_query(query.args.query)
        self.parameters = QueryParameterEditWidget(query)
        self.query_opts_widget.children = [
            self.parameters.layout,
            self.metadata_widget,
        ]

    @staticmethod
    def _fmt_query(query: str) -> str:
        """
        Format a query string.

        Parameters
        ----------
        query : str
            The query string.

        Returns
        -------
        str
            The formatted query string.

        """
        return "\n|".join(query.strip().split("|"))

    def add_query(self, button):
        """
        Add a new query.

        Parameters
        ----------
        button : Button
            The button object.

        """
        del button
        self.name_widget.value = ""
        self.description_widget.value = ""
        self.metadata_widget.value = ""
        self.query_text_widget.value = ""

    def save_query(self, button):
        """
        Save the data of a query.

        Parameters
        ----------
        button : Button
            The button object.

        """
        del button
        if self.name_widget.value not in self.query_collection.sources:
            self.query_collection.sources[self.name_widget.value] = Query(
                description=self.description_widget.value,
                metadata=self._save_qry_metadata(self.metadata_widget.value),
                args=QueryArgs(query=self.query_text_widget.value),
                parameters={},
            )
            self.populate_query_dropdown(None)
        else:
            query = self.query_collection.sources[self.name_widget.value]
            query = self.query_collection.sources[self.name_widget.value]
            query.description = self.description_widget.value
            query.metadata = self._save_qry_metadata(self.metadata_widget.value)
            query.args = QueryArgs(query=self.query_text_widget.value)
        self._changed_data = True

    def delete_query(self, button):
        """
        Delete a query.

        Parameters
        ----------
        button : Button
            The button object.

        """
        del button
        del self.query_collection.sources[self.query_select_widget.value]
        self.name_widget.value = ""
        self.description_widget.value = ""
        self.metadata_widget.value = ""
        self.query_text_widget.value = ""
        self.populate_query_dropdown(None)
        self._changed_data = True


class MetadataEditWidget(IPyDisplayMixin):
    """A class for editing Metadata properties."""

    def __init__(self, metadata: Optional[QueryMetadata] = None):
        """
        Initialize a MetadataEditWidget object.

        Parameters
        ----------
        metadata : Metadata
            A Metadata object.

        """
        self._changed_data = False
        self.metadata = metadata or self._new_metadata()
        self.version_widget = widgets.IntText(description="Version", **txt_fmt())
        self.description_widget = widgets.Text(description="Description", **txt_fmt())
        self.data_env_widget = widgets.SelectMultiple(
            options=list(DataEnvironment.__members__),
            description="Data Environments",
            **sel_fmt(),
        )
        self.data_families_widget = widgets.Textarea(
            description="Data Families",
            **sel_fmt(),
        )
        self.database_widget = widgets.Text(
            description="Kusto Database", placeholder="Database name", **txt_fmt()
        )
        self.cluster_widget = widgets.Text(
            description="Kusto Cluster",
            placeholder="URI, hostname or alias",
            **txt_fmt(),
        )
        self.clusters_widget = widgets.Textarea(
            description="Kusto Clusters",
            placeholder="URI, hostname or alias (one per line)",
            **sel_fmt(width="70%", height="75px"),
        )
        self.cluster_groups_widget = widgets.Textarea(
            description="Kusto Clusters",
            placeholder="MSTICPy cluster group (one per line)",
            **sel_fmt(width="70%", height="75px"),
        )
        self.tags_widget = widgets.Text(
            description="Tags", placeholder="(optional)", **txt_fmt()
        )
        self.data_source_widget = widgets.Text(
            description="Data Source", placeholder="(optional)", **txt_fmt()
        )
        self.save_metadata_widget = widgets.Button(description="Save metadata")
        self.save_metadata_widget.on_click(self.save_metadata)

        self.layout = widgets.VBox(
            [
                self.version_widget,
                self.description_widget,
                self.data_env_widget,
                self.data_families_widget,
                self.database_widget,
                widgets.Label(
                    "Specify the cluster, clusters or cluster groups for the query.",
                ),
                self.cluster_widget,
                self.clusters_widget,
                self.cluster_groups_widget,
                widgets.Label(
                    "Other items",
                ),
                self.tags_widget,
                self.data_source_widget,
                self.save_metadata_widget,
            ]
        )
        self.populate_widgets()

    @property
    def changed_data(self):
        """Return True if data has changed."""
        return self._changed_data

    def reset_changed_data(self):
        """Reset changed data flag."""
        self._changed_data = False

    def set_metadata(self, metadata: QueryMetadata):
        """Set the metadata object."""
        self.metadata = metadata
        self.populate_widgets()

    def populate_widgets(self):
        """Populate the widgets with the metadata of the Metadata object."""
        self.version_widget.value = self.metadata.version or ""
        self.description_widget.value = self.metadata.description or ""
        self.data_env_widget.value = (
            tuple(self.metadata.data_environments)
            if self.metadata.data_environments
            else ()
        )
        self.data_families_widget.value = (
            ", ".join(self.metadata.data_families)
            if self.metadata.data_families
            else ""
        )
        self.database_widget.value = self.metadata.database or ""
        self.cluster_widget.value = self.metadata.cluster or ""
        self.clusters_widget.value = "\n".join(self.metadata.clusters or [])
        self.cluster_groups_widget.value = "\n".join(self.metadata.cluster_groups or [])
        self.tags_widget.value = (
            ", ".join(self.metadata.tags) if self.metadata.tags else ""
        )
        self.data_source_widget.value = self.metadata.data_source or ""

    def save_metadata(self, button):
        """Save the values to the Metadata object."""
        del button
        self.metadata.version = self.version_widget.value
        self.metadata.description = self.description_widget.value
        self.metadata.data_environments = list(self.data_env_widget.value)
        self.metadata.data_families = [
            fam.strip()
            for fam in self.data_families_widget.value.split(",")
            if fam.strip()
        ]
        self.metadata.database = self.database_widget.value
        self.metadata.cluster = self.cluster_widget.value
        self.metadata.clusters = [
            cluster.strip() for cluster in self.clusters_widget.value.split("\n")
        ]
        self.metadata.cluster_groups = [
            cluster_grp.strip()
            for cluster_grp in self.cluster_groups_widget.value.split("\n")
        ]
        self.metadata.tags = [
            tag.strip() for tag in self.tags_widget.value.split(",") if tag.strip()
        ]
        self.metadata.data_source = self.data_source_widget.value
        self._changed_data = True

    def display(self):
        """Display the GUI widget."""
        display(self.layout)

    def _new_metadata(self):
        return QueryMetadata(
            version=1,
            description="Query collection description",
            data_environments=[DataEnvironment.Unknown.name],
            data_families=[DataFamily.Unknown.name],
        )


_DEF_FILENAME = "new_query_file.yaml"


class QueryEditor(IPyDisplayMixin):
    """Query template editor."""

    def __init__(
        self,
        query_file: Union[QueryCollection, Path, str, None] = None,
    ):
        """
        Initialize the QueryEditor.

        Parameters
        ----------
        query_file : Union[QueryCollection, Path, str, None], optional
            The query collection to edit, can be a string or pathlib Path
            object for a file to load, or a QueryCollection object.
            By default None

        """
        self.filename_widget = widgets.Text(
            description="Current file", layout=widgets.Layout(width="70%")
        )
        if isinstance(query_file, (Path, str)):
            self.filename_widget.value = str(query_file)
            self._open_initial_file()
        else:
            self.query_collection = query_file or self._new_collection()
            self.filename_widget.value = (
                self.query_collection.file_name or _DEF_FILENAME
            )
        self.query_editor = QueryEditWidget(self.query_collection)
        self.metadata_editor = MetadataEditWidget(self.query_collection.metadata)
        if not self.query_collection.defaults:
            self.query_collection.defaults = QueryDefaults(
                metadata={}, parameters={}  # type: ignore[call-arg]
            )
        self.default_param_editor = QueryParameterEditWidget(
            self.query_collection.defaults
        )

        self.ignore_changes = widgets.Checkbox(
            description="Ignore changes", value=False
        )
        self.open_button = widgets.Button(description="Open File")
        self.save_button = widgets.Button(description="Save File")
        self.new_button = widgets.Button(description="New file")
        self.collection_accordion = widgets.Accordion(
            children=[self.metadata_editor.layout, self.default_param_editor.layout],
            titles=["File metadata", "Default parameters"],
        )
        self.collection_accordion.set_title(0, "File metadata")
        self.collection_accordion.set_title(1, "Default parameters")
        self.collection_accordion.selected_index = None
        self.layout = widgets.VBox(
            [
                widgets.HTML("<h1 style='text-indent: 15px'>MSTICPy Query Editor</h1>"),
                self.collection_accordion,
                self.query_editor.layout,
                self._create_file_widget(),
            ]
        )
        self.open_button.on_click(self._open_file)
        self.save_button.on_click(self._save_file)
        self.new_button.on_click(self._new_file)

    @property
    def current_file(self) -> str:
        """Return the current file name."""
        return cast(str, self.filename_widget.value) or cast(str, _DEF_FILENAME)

    def _create_file_widget(self):
        """Create the file widget."""
        return widgets.HBox(
            children=[
                self.filename_widget,
                widgets.VBox(
                    [
                        self.new_button,
                        self.open_button,
                        self.save_button,
                        self.ignore_changes,
                    ],
                    layout=widgets.Layout(width="30%"),
                ),
            ]
        )

    def _new_collection(self):
        """Create a new query collection."""
        return QueryCollection(
            file_name=_DEF_FILENAME,
            metadata=QueryMetadata(
                version=1, description="new", data_families=[], data_environments=[]
            ),
            defaults=QueryDefaults(),
        )

    def _update_query_collection(self, query_collection: QueryCollection):
        """Update the sub-editors with the current query collection."""
        self.query_collection = query_collection
        self.query_editor.set_query_collection(self.query_collection)
        if not self.query_collection.defaults:
            self.query_collection.defaults = QueryDefaults(
                metadata={}, parameters={}  # type: ignore[call-arg]
            )
        self.default_param_editor.set_param_container(self.query_collection.defaults)
        self.metadata_editor.set_metadata(self.query_collection.metadata)

    def _save_file(self, button):
        """Save the current query collection."""
        del button
        save_queries_to_yaml(self.query_collection, self.current_file)

    def _open_file(self, button):
        """Open a new query collection."""
        del button
        if self._unsaved_changes() and not self.ignore_changes.value:
            print(
                "Please save or check 'Ignore changes' before opening a different file."
            )
            return
        self._reset_change_state()
        self.query_collection = load_queries_from_yaml(self.current_file)
        self._update_query_collection(self.query_collection)

    def _open_initial_file(self):
        """Open the initial file."""
        self.query_collection = load_queries_from_yaml(self.current_file)

    def _new_file(self, button):
        """Create a new query collection."""
        del button
        if self._unsaved_changes() and not self.ignore_changes.value:
            print("Please save or check 'Ignore changes' before creating a new file.")
            return
        self._reset_change_state()
        self.query_collection = QueryCollection(
            file_name=_DEF_FILENAME,
            metadata=QueryMetadata(
                version=1, description="new", data_families=[], data_environments=[]
            ),
            defaults=QueryDefaults(),
            sources={"new_query": Query("New query")},
        )
        self.filename_widget.value = _DEF_FILENAME
        self._update_query_collection(self.query_collection)

    def _reset_change_state(self):
        """Reset the change state of the sub-editors."""
        self.default_param_editor.reset_changed_data()
        self.metadata_editor.reset_changed_data()
        self.query_editor.reset_changed_data()

    def _unsaved_changes(self):
        """Check if there are unsaved changes."""
        return (
            self.default_param_editor.changed_data
            or self.metadata_editor.changed_data
            or self.query_editor.changed_data
        )


# Read and write the yaml file
# Define a function to load a YAML file into a QueryCollection
def load_queries_from_yaml(yaml_file: Union[str, Path]):
    """
    Load a YAML file into a QueryCollection.

    Parameters
    ----------
    yaml_file : Union[str, Path]
        The path to the YAML file to load.

    Returns
    -------
    QueryCollection
        A QueryCollection object containing the loaded queries.

    """
    with open(yaml_file, "r", encoding="utf-8") as f_handle:
        yaml_data = yaml.safe_load(f_handle)

    metadata = QueryMetadata(**yaml_data.get("metadata", {}))
    defaults = _create_query_defaults(yaml_data.get("defaults", {}))
    queries_dict = yaml_data.get("sources", {})
    queries = {name: _create_query(query) for name, query in queries_dict.items()}
    return QueryCollection(  # type: ignore[call-arg]
        file_name=str(yaml_file), metadata=metadata, defaults=defaults, sources=queries
    )


def save_queries_to_yaml(
    query_collection: QueryCollection, yaml_file: Union[str, Path]
):
    """
    Save a QueryCollection to a YAML file.

    Parameters
    ----------
    query_collection : QueryCollection
        The QueryCollection object to save to YAML.
    yaml_file : Union[str, Path]
        The path to the YAML file to save.

    """
    query_dict = asdict(query_collection)
    # ordered_dict =
    if "file_name" in query_dict:
        del query_dict["file_name"]
    _rename_data_type(query_dict)
    yaml_data = yaml.safe_dump(_remove_none_values(query_dict), sort_keys=False)
    Path(yaml_file).write_text(yaml_data, encoding="utf-8")


def _create_query_defaults(defaults):
    """Create a QueryDefaults object."""
    def_metadata = defaults["metadata"] if "metadata" in defaults else {}
    def_params = (
        {
            name: _create_parameter(param)
            for name, param in defaults["parameters"].items()
        }
        if "parameters" in defaults and defaults["parameters"]
        else {}
    )
    return QueryDefaults(metadata=def_metadata, parameters=def_params)


def _create_query(query_data):
    """Create a Query object."""
    parameters = query_data.get("parameters", {})
    if parameters:
        parameters = {
            name: _create_parameter(param) for name, param in parameters.items()
        }
    return Query(
        description=query_data.get("description", ""),
        metadata=query_data.get("metadata", {}),
        args=QueryArgs(query=query_data.get("args", {}).get("query", "")),
        parameters=parameters,
    )


def _create_parameter(param_data):
    """Create a Parameter object."""
    return QueryParameter(
        description=param_data.get("description", ""),
        datatype=param_data.get("type", "str"),
        default=param_data.get("default"),
    )


def _remove_none_values(source_obj):
    """Recursively remove any item with a None value from a nested dictionary."""
    if isinstance(source_obj, dict):
        return {
            key: _remove_none_values(val)
            for key, val in source_obj.items()
            if val is not None or (isinstance(val, (list, dict)) and len(val) > 0)
        }
    if isinstance(source_obj, (list, tuple)):
        return type(source_obj)(
            _remove_none_values(val) for val in source_obj if val is not None
        )
    return source_obj


def _rename_data_type(source_obj):
    """Recursively rename the 'datatype' key to 'type' in a nested dictionary."""
    if isinstance(source_obj, dict):
        if "datatype" in source_obj:
            val = source_obj["datatype"]
            del source_obj["datatype"]
            source_obj["type"] = val
        for value in source_obj.values():
            _rename_data_type(value)
    if isinstance(source_obj, (list, tuple)):
        for value in source_obj:
            _rename_data_type(value)
