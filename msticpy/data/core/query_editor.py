# %%
from datetime import datetime
from dataclasses import asdict
from dataclasses import dataclass, field, asdict
from enum import Enum
from pathlib import Path
from typing import Any, List, Optional, Union, Dict

from IPython.display import display
import ipywidgets as widgets
import yaml


__version__ = "1.0.0"
__author__ = "Ian Hellen"


# %% [markdown]
# ## DataEnvironment and DataFamily Enum classes

# %%
# pylint: disable=invalid-name
@export
class DataFamily(Enum):
    """
    Enumeration of data families.

    Used to identify which queries are relevant for which
    data sources.
    """

    Unknown = 0
    WindowsSecurity = 1
    LinuxSecurity = 2
    SecurityAlert = 3
    SecurityGraphAlert = 4
    LinuxSyslog = 5
    AzureNetwork = 6
    MDATP = 7
    Splunk = 8
    ResourceGraph = 9
    Sumologic = 10
    Cybereason = 11
    Elastic = 14

    @classmethod
    def parse(cls, value: Union[str, int]) -> "DataFamily":
        """
        Convert string or int to enum.

        Parameters
        ----------
        value : Union[str, int]
            value to parse

        """
        if isinstance(value, cls):
            return value

        parsed_enum = cls.Unknown
        if isinstance(value, str):
            try:
                parsed_enum = cls[value]
            except KeyError:
                # match to value if case is incorrect
                # pylint: disable=no-member
                return next(
                    (
                        e_val
                        for e_name, e_val in cls.__members__.items()
                        if e_name.upper() == value.upper()
                    ),
                    cls.Unknown,
                )
                # pylint: enable=no-member
        if isinstance(value, int):
            try:
                parsed_enum = cls(value)
            except ValueError:
                parsed_enum = cls.Unknown
        return parsed_enum


@export
class DataEnvironment(Enum):
    """
    Enumeration of data environments.

    Used to identify which queries are relevant for which
    data sources.
    """

    Unknown = 0
    MSSentinel = 1
    AzureSentinel = 1  # alias of LogAnalytics
    LogAnalytics = 1
    Kusto = 2
    AzureSecurityCenter = 3
    MSGraph = 4
    SecurityGraph = 4
    MDE = 5
    MDATP = 5  # alias of MDE
    LocalData = 6
    Splunk = 7
    OTRF = 8
    Mordor = 8
    ResourceGraph = 9
    Sumologic = 10
    M365D = 11
    Cybereason = 12
    Elastic = 14
    OSQueryLogs = 15

    @classmethod
    def parse(cls, value: Union[str, int]) -> "DataEnvironment":
        """
        Convert string or int to enum.

        Parameters
        ----------
        value : Union[str, int]
            value to parse

        """
        if isinstance(value, cls):
            return value

        parsed_enum = cls.Unknown
        if isinstance(value, str):
            try:
                parsed_enum = cls[value]
            except KeyError:
                pass
        if isinstance(value, int):
            parsed_enum = cls(value)
        return parsed_enum


class IPyDisplayMixin:
    """IPython display mixin class."""

    def display(self):
        """Display the interactive widgets."""
        display(self.layout)

    def _ipython_display_(self):
        """Display in IPython."""
        self.display()

# %% [markdown]
# ## DataQuery classes

# %%
@dataclass
class Metadata:
    """Metadata for query definitions."""

    version: int
    description: str
    data_environments: List[str]
    data_families: List[str]
    database: Optional[str] = None
    cluster: Optional[str] = None
    clusters: Optional[List[str]] = None
    cluster_groups: Optional[List[str]] = None
    tags: List[str] = field(default_factory=list)
    data_source: Optional[str] = None



@dataclass
class Parameter:
    """Query parameter."""

    description: str
    datatype: str
    default: Any = None
    required: Optional[bool] = None


@dataclass
class Defaults:
    """Default values for query definitions."""

    metadata: Optional[Dict[str, Any]] = None
    parameters: Dict[str, Parameter] = field(default_factory=dict)


@dataclass
class Args:
    """Query arguments."""

    query: str = ""


@dataclass
class Query:
    """A Query definition."""

    description: str
    metadata: Dict[str, Any] = field(default_factory=dict)
    args: Args = field(default_factory=Args)
    parameters: Dict[str, Parameter] = field(default_factory=dict)


@dataclass
class QueryCollection:
    """Query Collection class - a query template."""

    file_name: str
    metadata: Metadata
    defaults: Optional[Defaults] = None
    sources: Dict[str, Query] = field(default_factory=dict)



# %% [markdown]
# ## Parameter editing

# %%


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
        "style": {"description_width": _DESC_WIDTH}
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
        "style": {"description_width": _DESC_WIDTH}
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
        "style": {"description_width": _DESC_WIDTH}
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
            border='1px solid black',
            margin="5px",
            padding="5px",
            style={"padding": "5px", "background-color": "red"},
        )
    }

@dataclass
class CustomChange:

    new: Any


class QueryParameters:
    """Class to manage Query parameters."""

    def __init__(self, container: Union[Query, Defaults]):
        """Initialize the class."""
        self._changed_data = False
        self.param_container = container
        self.parameter_dropdown = widgets.Select(
            description='Parameters',
            size=5,
            options=list(self.param_container.parameters.keys()),
            **sel_fmt(height="100px")
        )
        # Create widgets for the Parameter fields
        self.parameter_name_widget = widgets.Text(description='Name', **txt_fmt())
        self.description_widget = widgets.Text(description='Description', **txt_fmt())
        self.type_widget = widgets.Dropdown(description='Type', options=_PARAM_OPTIONS, **sel_fmt(height="30px"))
        self.default_reqd_widget = widgets.Checkbox(description="Use a default value")
        self.default_widget = widgets.Text(description='Default Value', **txt_fmt())
        self.required_widget = widgets.Checkbox(description='Mandatory Parameter')

        # Create buttons
        self.add_parameter_button = widgets.Button(description='New Parameter')
        self.save_parameter_button = widgets.Button(description='Save Parameter')
        self.delete_parameter_button = widgets.Button(description='Delete Parameter')

        # Attach the functions to buttons
        self.add_parameter_button.on_click(self.add_parameter)
        self.save_parameter_button.on_click(self.save_parameter)
        self.delete_parameter_button.on_click(self.delete_parameter)
        self.parameter_dropdown.observe(self.populate_parameter_widgets, names='value')

        # Create a widget for adding, editing, and deleting Parameters
        self.layout = widgets.VBox([
            widgets.HBox([
                self.parameter_dropdown,
                widgets.VBox([
                    self.add_parameter_button,
                    self.delete_parameter_button,
                ]),
            ]),
            widgets.VBox(children=[
                self.parameter_name_widget,
                self.description_widget,
                self.type_widget,
                widgets.HBox([self.default_reqd_widget, self.default_widget]),
                self.required_widget,
                self.save_parameter_button,
                ],
                **box_layout()
            )
        ])
        if self.param_container and self.param_container.parameters:
            init_change = CustomChange(new=next(iter(self.param_container.parameters)))
            self.populate_parameter_widgets(init_change)

    @property
    def changed_data(self):
        """Return True if data has changed."""
        return self._changed_data

    def reset_changed_data(self):
        """Reset changed data flag."""
        self._changed_data = False

    # Define a function to add a new Parameter to the selected Query
    def add_parameter(self, button):
        """Add a new parameter."""
        # Clear the input widgets
        self._blank_parameter()
        self.parameter_name_widget.value = "new_parameter"

    def _blank_parameter(self):
        """Clear the parameter widgets"""
        self.parameter_name_widget.value = ""
        self.description_widget.value = ''
        self.type_widget.value = ''
        self.default_widget.value = ''
        self.default_reqd_widget.value = False
        self.required_widget.value = False

    # Define a function to populate the Parameter widgets with the values of the selected Parameter
    def populate_parameter_widgets(self, change):
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
        parameter = self.param_container.parameters[self.parameter_dropdown.value]
        param_name = self.parameter_name_widget.value
        parameter.description = self.description_widget.value
        parameter.datatype = self.type_widget.value
        parameter.default = self.default_widget.value if self.default_reqd_widget.value else None
        parameter.required = True if self.required_widget.value else None
        self._changed_data = True

        self.collection[param_name] = parameter

    # Define a function to delete the selected Parameter from the selected Query
    def delete_parameter(self, button):
        """Delete parameter item."""
        del button
        del self.param_container.parameters[self.parameter_dropdown.value]
        # Clear the input widgets
        self._blank_parameter()
        self._changed_data = True



# qp = QueryParameters(query_collection.defaults)

# Display the GUI widget
# display(qp.layout)

# %% [markdown]
# ## Replace parameter name

# %%

def replace_in_query(param_name: str, param: Parameter, src_name: str, query: str) -> str:
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
    if param.datatype == "datetime":
        repl_str = f"datetime({{{param_name}}})"
    elif param.datatype == "str":
        repl_str = f"\"{{{param_name}}}\""
    else:
        repl_str = f"{{{param_name}}}"
    return query.replace(src_name, repl_str)

# query_collection.sources
src_name = "TimeGenerated"
query = query_collection.sources["dns_lookups_for_ip"].args.query
for dt in ("str", "datetime", "list", "int"):
    param = Parameter(
        description="test",
        datatype=dt
    )
    print(dt, replace_in_query("TEST_PARAM", param, src_name, query))

# %% [markdown]
# # Add/Edit Query objects

# %%


_DESC_WIDTH = "120px"


def txt_fmt(width: str = "70%") -> Dict:
    """Return a dictionary with layout and style for a text widget.

    Parameters
    ----------
    width : str, optional
        The width of the widget, by default "70%"

    Returns
    -------
    Dict
        A dictionary with layout and style for a text widget.
    """
    return {
        "layout": widgets.Layout(width=width),
        "style": {"description_width": _DESC_WIDTH}
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
        "style": {"description_width": _DESC_WIDTH}
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
        "style": {"description_width": _DESC_WIDTH}
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
            border='1px solid black',
            margin="5px",
            padding="5px",
            style={"padding": "5px", "background-color": "red"},
        )
    }

class QueryEditor:
    """A class for editing queries."""

    def __init__(self, query_collection: QueryCollection):
        """
        Initialize a QueryEditor object.

        Parameters
        ----------
        query_collection : QueryCollection
            A collection of queries.
        """
        self._changed_data = False
        self.query_collection = query_collection
        self.query_select_widget = widgets.Select(
            description='Queries',
            options=list(self.query_collection.sources.keys()),
            **sel_fmt()
        )
        self.name_widget = widgets.Text(
            description='Name', placeholder="Python-compatible query name", **txt_fmt())
        self.description_widget = widgets.Text(
            description='Description',
            placeholder="Description of query",
            **txt_fmt("90%")
        )
        self.metadata_widget = widgets.Textarea(
            description='Query metadata', **txtarea_fmt("90%", height="70px")
        )
        self.parameters: Optional[QueryParameters] = None
        self.query_text_widget = widgets.Textarea(
            description='Query',
            placeholder="query text with {parameter}a",
            **txtarea_fmt("90%")
        )
        self.query_opts_widget = widgets.Accordion(
            children=[
                widgets.Label(value="No parameters"),
                self.metadata_widget,
            ],
            titles=["Query parameters","Query metadata"],
            layout=widgets.Layout(margin="5px"),
        )
        self.query_opts_widget.set_title(0, "Query parameters")
        self.query_opts_widget.set_title(1, "Query metadata")
        self.query_opts_widget.selected_index = None
        self.add_query_button = widgets.Button(description='New Query')
        self.save_query_button = widgets.Button(description='Save Query')
        self.delete_query_button = widgets.Button(description='Delete Query')
        self.queries_widget = widgets.VBox(
            children=[
                widgets.Label(value='Query'),
                self.name_widget,
                self.description_widget,
                self.query_text_widget,
                self.query_opts_widget,
                self.save_query_button
            ],
            **box_layout()
        )
        self.layout = widgets.VBox([
            widgets.Label(value='Queries'),
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
                **box_layout()
            ),
            self.queries_widget,
        ])
        self.add_query_button.on_click(self.add_query)
        self.query_select_widget.observe(self.populate_query_widgets, names='value')
        self.save_query_button.on_click(self.save_query)
        self.delete_query_button.on_click(self.delete_query)
        self.populate_query_dropdown(None)
        if self.query_collection.sources:
            init_change = CustomChange(new=next(iter(self.query_collection.sources)))
            print("first item", init_change)
            self.populate_query_widgets(init_change)

    @property
    def changed_data(self):
        """Return True if data has changed."""
        return self._changed_data or self.parameters.changed_data

    def reset_changed_data(self):
        """Reset changed data flag."""
        self._changed_data = False
        self.parameters.reset_changed_data()

    def populate_query_dropdown(self, change: Optional[Dict[str, Any]]) -> None:
        """
        Populate the query dropdown widget.

        Parameters
        ----------
        change : Any
            The change event.
        """
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

    def _save_qry_metadata(self, metadata: str) -> Optional[Dict[str, Any]]:
        """
        Save the metadata of a query.

        Parameters
        ----------
        metadata : str
            The metadata string.
        """
        return yaml.safe_load(metadata) if metadata else None

    def populate_query_widgets(self, change):
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
        self.parameters = QueryParameters(query)
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
        self.name_widget.value = ''
        self.description_widget.value = ''
        self.metadata_widget.value = ''
        self.query_text_widget.value = ''


    def save_query(self, button):
        """
        Save the data of a query.

        Parameters
        ----------
        button : Button
            The button object.
        """
        if self.name_widget.value not in self.query_collection.sources:
            self.query_collection.sources[self.name_widget.value] = Query(
                description=self.description_widget.value,
                metadata=self._save_qry_metadata(self.metadata_widget.value),
                args=Args(query=self.query_text_widget.value),
                parameters={}
            )
            self.populate_query_dropdown(None)
        else:
            query = self.query_collection.sources[self.name_widget.value]
            query = self.query_collection.sources[self.name_widget.value]
            query.description = self.description_widget.value
            query.metadata = self._save_qry_metadata(self.metadata_widget.value)
            query.args = Args(query=self.query_text_widget.value)
        self._changed_data = True

    def delete_query(self, button):
        """
        Delete a query.

        Parameters
        ----------
        button : Button
            The button object.
        """
        del self.query_collection.sources[self.query_select_widget.value]
        self.name_widget.value = ''
        self.description_widget.value = ''
        self.metadata_widget.value = ''
        self.query_text_widget.value = ''
        self.populate_query_dropdown(None)
        self._changed_data = True

    def display(self):
        """Display the GUI widget."""
        display(self.layout)




## %% [markdown]
## ## Metadata
# %%


@dataclass
class Metadata:
    """Metadata for query definitions."""

    version: int
    description: str
    data_environments: List[str]
    data_families: List[str]
    database: Optional[str] = None
    cluster: Optional[str] = None
    clusters: Optional[List[str]] = None
    cluster_groups: Optional[List[str]] = None
    tags: List[str] = field(default_factory=list)
    data_source: Optional[str] = None



class MetadataEditor:
    """A class for editing Metadata properties."""

    def __init__(self, metadata: Optional[Metadata] = None):
        """
        Initialize a MetadataEditor object.

        Parameters
        ----------
        metadata : Metadata
            A Metadata object.
        """
        self._changed_data = False
        self.metadata = metadata or self._new_metadata()
        self.version_widget = widgets.IntText(description='Version', **txt_fmt())
        self.description_widget = widgets.Text(description='Description', **txt_fmt())
        self.data_env_widget = widgets.SelectMultiple(
            options=list(DataEnvironment.__members__),
            description='Data Environments',
            **sel_fmt()
        )
        self.data_families_widget = widgets.SelectMultiple(
            options=list(DataFamily.__members__),
            description='Data Families',
            **sel_fmt()
        )
        self.database_widget = widgets.Text(
            description='Kusto Database', placeholder="Database name", **txt_fmt()
        )
        self.cluster_widget = widgets.Text(
            description='Kusto Cluster', placeholder="URI, hostname or alias", **txt_fmt()
        )
        self.clusters_widget = widgets.Textarea(
            description='Kusto Clusters',
            placeholder="URI, hostname or alias (one per line)",
            **sel_fmt(width="70%", height="75px")
        )
        self.cluster_groups_widget = widgets.Textarea(
            description='Kusto Clusters',
             placeholder="MSTICPy cluster group (one per line)",
             **sel_fmt(width="70%", height="75px")
        )
        self.tags_widget = widgets.Text(description='Tags', placeholder="(optional)", **txt_fmt())
        self.data_source_widget = widgets.Text(description='Data Source', placeholder="(optional)", **txt_fmt())
        self.save_metadata_widget = widgets.Button(description="Save metadata")
        self.save_metadata_widget.on_click(self.save_metadata)

        self.layout = widgets.VBox([
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
            self.save_metadata_widget
        ])
        self._populate_widgets()

    @property
    def changed_data(self):
        """Return True if data has changed."""
        return self._changed_data

    def reset_changed_data(self):
        """Reset changed data flag."""
        self._changed_data = False

    def _populate_widgets(self):
        """Populate the widgets with the metadata of the Metadata object."""
        self.version_widget.value = self.metadata.version or ""
        self.description_widget.value = self.metadata.description or ""
        self.data_env_widget.value = tuple(self.metadata.data_environments) if self.metadata.data_environments else ()
        self.data_families_widget.value = tuple(self.metadata.data_families) if self.metadata.data_families else ()
        self.database_widget.value = self.metadata.database or ""
        self.cluster_widget.value = self.metadata.cluster or ""
        self.clusters_widget.value = "\n".join(self.metadata.clusters or [])
        self.cluster_groups_widget.value = "\n".join(self.metadata.cluster_groups or [])
        self.tags_widget.value = ', '.join(self.metadata.tags) if self.metadata.tags else ""
        self.data_source_widget.value = self.metadata.data_source or ""

    def save_metadata(self, button):
        """
        Save the values to the Metadata object.

        Parameters
        ----------
        change : dict
            The change dictionary.
        """
        self.metadata.version = self.version_widget.value
        self.metadata.description = self.description_widget.value
        self.metadata.data_environments = self.data_env_widget.value
        self.metadata.data_families = self.data_families_widget.value
        self.metadata.database = self.database_widget.value
        self.metadata.cluster = self.cluster_widget.value
        self.metadata.clusters = [cluster.strip for cluster in self.clusters_widget.value.split("\n")]
        self.metadata.cluster_groups = [cluster_grp.strip for cluster_grp in self.cluster_groups_widget.value.split("\n")]
        self.metadata.tags = self.tags_widget.value
        self.metadata.data_source = self.data_source_widget.value
        self._changed_data = True

    def display(self):
        """Display the GUI widget."""
        display(self.layout)

    def _new_metadata(self):
        return Metadata(
            version=1,
            description="Query collection description",
            data_environments=[DataEnvironment.Unknown.name],
            data_families=[DataFamily.Unknown.name],
        )

# me = MetadataEditor()
# me.display()

# %%


class QueryTemplateEditor(IPyDisplayMixin):

    def __init__(self, query_collection: Optional[QueryCollection] = None, path: Union[Path, str, None] = None):
        self.query_file_list = widgets.Select(description="QueryFiles")
        self.query_path = widgets.Text(
            description="Folder",
            value=str(path) or ".",
            layout=widgets.Layout(width="70%")
        )

        self.query_collection = query_collection or self._new_collection()
        self.query_editor = QueryEditor(self.query_collection)
        # self.parameter_editor = QueryParameters(self.query_collection)
        self.metadata_editor = MetadataEditor(self.query_collection.metadata)
        self.default_param_editor = QueryParameters(self.query_collection.defaults)
        self.filename_widget = widgets.Text(
            description="Current file",
            value=self.query_collection.file_name,
            layout=widgets.Layout(width="70%")
        )
        self.open_button = widgets.Button(description="Read File")
        self.save_button = widgets.Button(description="Save File")
        self.new_button = widgets.Button(description="New file")
        self.revert_button = widgets.Button(description="Revert to saved")
        self.collection_accordion = widgets.Accordion(
            children=[
                self.metadata_editor.layout,
                self.default_param_editor.layout
            ],
            titles=["File metadata", "Default parameters"],
        )
        self.collection_accordion.set_title(0, "File metadata")
        self.collection_accordion.set_title(1, "Default parameters")
        self.collection_accordion.selected_index = None
        self.layout = widgets.VBox([
            widgets.HTML("<h1 style='text-indent: 15px'>MSTICPy Query Editor</h1>"),
            widgets.HBox(
                children=[
                    self.filename_widget,
                    widgets.VBox([self.open_button, self.save_button], layout=widgets.Layout(width="30%")),
                ]
            ),
            self.collection_accordion,
            self.query_editor.layout,
        ])
        self.open_button.on_click(self._open_file)
        self.save_button.on_click(self._save_file)
        self.new_button.on_click(self._new_file)

    def _new_collection(self):
        return QueryCollection(
            file_name="new_file.yaml",
            metadata=Metadata(version=1, description="new", data_families=[], data_environments=[]),
            defaults=Defaults(),
        )

    def _get_current_files(self, path):
        folder = Path(path)
        if not folder.is_dir():
            print("Invalid path")
        files = [file.name for file in folder.glob("*.yaml")] + [file.name for file in folder.glob("*.yml")]
        self.query_file_list.options = files

    def _save_file(self):
        save_queries_to_yaml(self.query_collection, self.filename_widget.value)

    def _open_file(self):
        if self._unsaved_changes():
            print("Please save or revert changes before opening a different file.")
        self.query_collection = load_queries_from_yaml(self.filename_widget.value)

    def _new_file(self):
        if self._unsaved_changes():
            print("Please save or revert changes before creating a new file.")
        self.query_collection = QueryCollection(
            file_name="new_query_file.yaml",
            metadata=Metadata(version=1, description="new", data_families=[], data_environments=[]),
            defaults=Defaults(),
            sources={"new_query", Query("New query")}
        )

    def _unsaved_changes(self):
        return (
            self.default_param_editor.changed_data
            or self.metadata_editor.changed_data
            or self.query_editor.changed_data
        )


# Read and write the yaml file
# Define a function to load a YAML file into a QueryCollection
def load_queries_from_yaml(yaml_file):
    with open(yaml_file, 'r') as f:
        yaml_data = yaml.safe_load(f)

    metadata = Metadata(**yaml_data.get('metadata', {}))
    defaults = _create_defaults(yaml_data.get('defaults', {}))
    queries_dict = yaml_data.get('sources', {})
    queries = {name:_create_query(query) for name, query in queries_dict.items()}
    return QueryCollection(file_name=yaml_file, metadata=metadata, defaults=defaults, sources=queries)


def _create_defaults(defaults):
    def_metadata = {}
    def_params = {}
    if "metadata" in defaults:
        def_metadata = defaults["metadata"]
    if "parameters" in defaults:
        def_params = {name: _create_parameter(param) for name, param in defaults['parameters'].items()}
    return Defaults(metadata=def_metadata, parameters=def_params)

def _create_query(query_data):
    return Query(
        description=query_data.get('description', ''),
        metadata=query_data.get('metadata', {}),
        args=Args(query=query_data.get('args', {}).get('query', '')),
        parameters={name: _create_parameter(param) for name, param in query_data.get('parameters', {}).items()}
    )


def _create_parameter(param_data):
    print(param_data)
    return Parameter(
        description=param_data.get('description', ''),
        datatype=param_data.get('type', 'str'),
        default=param_data.get('default'),
        required=param_data.get('required')
    )


# Define a function to save a QueryCollection to a YAML file
def save_queries_to_yaml(query_collection, yaml_file):
    query_dict = asdict(query_collection)
    # ordered_dict =
    if 'file_name' in query_dict:
        del query_dict["file_name"]
    rename_data_type(query_dict)
    yaml_data = yaml.safe_dump(remove_none_values(query_dict), sort_keys=False)
    Path(yaml_file).write_text(yaml_data, encoding='utf-8')


def remove_none_values(source_obj):
    """Recursively remove any item with a None value from a nested dictionary."""
    if isinstance(source_obj, dict):
        return {
            key: remove_none_values(val) for key, val in source_obj.items()
            if val is not None or (isinstance(val, (list, dict)) and len(val) > 0)
        }
    elif isinstance(source_obj, (list, tuple)):
        return type(source_obj)(remove_none_values(val) for val in source_obj if val is not None)
    else:
        return source_obj

def rename_data_type(source_obj):
    if isinstance(source_obj, dict):
        if "datatype" in source_obj:
            val = source_obj["datatype"]
            del source_obj["datatype"]
            source_obj["type"] = val
        for value in source_obj.values():
            rename_data_type(value)
    if isinstance(source_obj, (list, tuple)):
        for value in source_obj:
            rename_data_type(value)


# qt = QueryTemplateEditor(query_collection=query_collection)
# qt.layout
_TEST_FILES = [
    "e:/src/msticpy/msticpy/data/queries/mssentinel/kql_sent_az_dns.yaml",
    "e:/src/msticpy/msticpy/data/queries/mssentinel/kql_sent_az_net.yaml",
    "e:/src/msticpy/msticpy/data/queries/mssentinel/kql_sent_o365.yaml",
    "e:/src/msticpy/msticpy/data/queries/mssentinel/kql_sent_winevent_proc.yaml",
]
# Load the queries from the YAML file
query_collection = load_queries_from_yaml(_TEST_FILES[0])


# %% [markdown]
# # Read/write yaml files

# %%

# Read and write the yaml file
# Define a function to load a YAML file into a QueryCollection
def load_queries_from_yaml(yaml_file):
    with open(yaml_file, 'r') as f:
        yaml_data = yaml.safe_load(f)

    metadata = Metadata(**yaml_data.get('metadata', {}))
    defaults = _create_defaults(yaml_data.get('defaults', {}))
    queries_dict = yaml_data.get('sources', {})
    queries = {name:_create_query(query) for name, query in queries_dict.items()}
    return QueryCollection(file_name=yaml_file, metadata=metadata, defaults=defaults, sources=queries)


def _create_defaults(defaults):
    def_metadata = {}
    def_params = {}
    if "metadata" in defaults:
        def_metadata = defaults["metadata"]
    if "parameters" in defaults:
        def_params = {name: _create_parameter(param) for name, param in defaults['parameters'].items()}
    return Defaults(metadata=def_metadata, parameters=def_params)

def _create_query(query_data):
    return Query(
        description=query_data.get('description', ''),
        metadata=query_data.get('metadata', {}),
        args=Args(query=query_data.get('args', {}).get('query', '')),
        parameters={name: _create_parameter(param) for name, param in query_data.get('parameters', {}).items()}
    )


def _create_parameter(param_data):
    print(param_data)
    return Parameter(
        description=param_data.get('description', ''),
        datatype=param_data.get('type', 'str'),
        default=param_data.get('default'),
        required=param_data.get('required')
    )


# Define a function to save a QueryCollection to a YAML file
def save_queries_to_yaml(query_collection, yaml_file):
    query_dict = asdict(query_collection)
    # ordered_dict =
    if 'file_name' in query_dict:
        del query_dict["file_name"]
    rename_data_type(query_dict)
    yaml_data = yaml.safe_dump(remove_none_values(query_dict), sort_keys=False)
    Path(yaml_file).write_text(yaml_data, encoding='utf-8')


# Load the queries from the YAML file
query_collection = load_queries_from_yaml('e:/src/msticpy/msticpy/data/queries/mssentinel/kql_sent_az_dns.yaml')

def remove_none_values(source_obj):
    """Recursively remove any item with a None value from a nested dictionary."""
    if isinstance(source_obj, dict):
        return {
            key: remove_none_values(val) for key, val in source_obj.items()
            if val is not None or (isinstance(val, (list, dict)) and len(val) > 0)
        }
    elif isinstance(source_obj, (list, tuple)):
        return type(source_obj)(remove_none_values(val) for val in source_obj if val is not None)
    else:
        return source_obj

def rename_data_type(source_obj):
    if isinstance(source_obj, dict):
        if "datatype" in source_obj:
            val = source_obj["datatype"]
            del source_obj["datatype"]
            source_obj["type"] = val
        for value in source_obj.values():
            rename_data_type(value)
    if isinstance(source_obj, (list, tuple)):
        for value in source_obj:
            rename_data_type(value)

# # Modify the queries as needed
# query_collection.sources['example_query'].description = 'This is an example query'

from pprint import pprint
# # Save the queries to a new YAML file
def test_save_queries_to_yaml(query_collection, yaml_file):
    save_queries_to_yaml(query_collection, yaml_file)

    pprint(asdict(query_collection))
    print(remove_none_values(asdict(query_collection)))
    print(yaml.safe_dump(remove_none_values(asdict(query_collection)), sort_keys=False))


# test_save_queries_to_yaml(query_collection, 'test_queries.yaml')
# rea
