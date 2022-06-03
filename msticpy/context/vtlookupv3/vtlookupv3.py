"""VirusTotal v3 API."""
import asyncio
from enum import Enum
from typing import Any, Dict, List, Optional, Set, Tuple

import pandas as pd
from IPython.display import HTML, display

from ...common.exceptions import MsticpyImportExtraError
from ...common.provider_settings import get_provider_settings
from ...common.utility import is_ipython

try:
    import nest_asyncio
    import vt

    # Removing dependency temporarily due to build break
    from vt_graph_api import VTGraph
    from vt_graph_api import errors as vt_graph_errs

    from .vtfile_behavior import VTFileBehavior
except ImportError as imp_err:
    raise MsticpyImportExtraError(
        "Cannot use this feature without vt-py, vt-graph-api and",
        "nest_asyncio packages installed.",
        title="Error importing VirusTotal modules.",
        extra="vt3",
    ) from imp_err


# pylint: disable=too-many-lines


class MsticpyVTNoDataError(Exception):
    """No data returned from VT API."""


class MsticpyVTGraphSaveGraphError(Exception):
    """Could not save VT Graph."""


class VTEntityType(Enum):
    """VTEntityType: Enum class for VirusTotal entity types."""

    FILE = "file"
    DOMAIN = "domain"
    IP_ADDRESS = "ip_address"
    URL = "url"


class ColumnNames(Enum):
    """Column name enum for DataFrame output."""

    ID = "id"
    TYPE = "type"
    DETECTIONS = "detections"
    SCANS = "scans"
    SOURCE = "source"
    TARGET = "target"
    RELATIONSHIP_TYPE = "relationship_type"
    SOURCE_TYPE = "source_type"
    TARGET_TYPE = "target_type"


class VTObjectProperties(Enum):
    """Enum for VT Object properties."""

    ATTRIBUTES = "attributes"
    RELATIONSHIPS = "relationship"
    LAST_ANALYSIS_STATS = "last_analysis_stats"
    MALICIOUS = "malicious"


def _make_sync(future):
    """Wait for an async call, making it sync."""
    try:
        event_loop = asyncio.get_event_loop()
    except RuntimeError:
        # Generate an event loop if there isn't any.
        event_loop = asyncio.new_event_loop()
        asyncio.set_event_loop(event_loop)
    return event_loop.run_until_complete(future)


VT_API_NOT_FOUND = "NotFoundError"


class VTLookupV3:
    """VTLookupV3: VirusTotal lookup of IoC reports."""

    _SUPPORTED_VT_TYPES: Set[VTEntityType] = {
        VTEntityType.FILE,
        VTEntityType.URL,
        VTEntityType.IP_ADDRESS,
        VTEntityType.DOMAIN,
    }

    _MAPPING_TYPES_ENDPOINT: Dict[VTEntityType, str] = {
        VTEntityType.FILE: "files",
        VTEntityType.URL: "urls",
        VTEntityType.IP_ADDRESS: "ip_addresses",
        VTEntityType.DOMAIN: "domains",
    }

    _BASIC_PROPERTIES_PER_TYPE: Dict[VTEntityType, Set[str]] = {
        VTEntityType.FILE: {
            "type_description",
            "size",
            "first_submission_date",
            "last_submission_date",
            "times_submitted",
            "meaningful_name",
        },
        VTEntityType.URL: {
            "first_submission_date",
            "last_submission_date",
            "times_submitted",
        },
        VTEntityType.IP_ADDRESS: {"date", "country", "asn", "as_owner"},
        VTEntityType.DOMAIN: {"id", "creation_date", "last_update_date", "country"},
    }

    @property
    def supported_vt_types(self) -> List[str]:
        """
        Return list of VirusTotal supported IoC type names.

        Returns
        -------
        List[str]:
            List of VirusTotal supported IoC type names.

        """
        return [str(i_type) for i_type in self._SUPPORTED_VT_TYPES]

    @classmethod
    def _get_endpoint_name(cls, vt_type: str) -> str:
        if VTEntityType(vt_type) not in cls._SUPPORTED_VT_TYPES:
            raise KeyError(f"Property type {vt_type} not supported")

        return cls._MAPPING_TYPES_ENDPOINT[VTEntityType(vt_type)]

    @classmethod
    def _parse_vt_object(
        cls, vt_object: vt.object.Object, all_props: bool = False
    ) -> pd.DataFrame:
        obj_dict = vt_object.to_dict()
        add_columns_df = pd.DataFrame()
        if VTObjectProperties.ATTRIBUTES.value in obj_dict:
            attributes = obj_dict[VTObjectProperties.ATTRIBUTES.value]
            vt_type = VTEntityType(vt_object.type)
            if vt_type not in cls._SUPPORTED_VT_TYPES:
                raise KeyError(f"Property type {vt_type} not supported")
            if not all_props:
                obj = {
                    key: attributes[key]
                    for key in cls._BASIC_PROPERTIES_PER_TYPE[vt_type]
                    if key in attributes
                }
            else:
                obj = attributes
            vt_df = pd.json_normalize(data=[obj])
            last_analysis_stats = attributes.get(
                VTObjectProperties.LAST_ANALYSIS_STATS.value
            )
            if last_analysis_stats:
                add_columns_df = pd.DataFrame(
                    {
                        ColumnNames.DETECTIONS.value: [
                            last_analysis_stats[VTObjectProperties.MALICIOUS.value]
                        ],
                        ColumnNames.SCANS.value: [sum(last_analysis_stats.values())],
                    }
                )
            # Format dates for pandas
            vt_df = timestamps_to_utcdate(vt_df)
        elif obj_dict:
            vt_df = pd.json_normalize([obj_dict])
        else:
            vt_df = cls._item_not_found_df(
                vt_type=vt_object.type, observable=vt_object.id
            )

        # Inject ID and Type columns
        if ColumnNames.ID.value not in vt_df.columns:
            add_columns_df[ColumnNames.ID.value] = [vt_object.id]
        if ColumnNames.TYPE.value not in vt_df.columns:
            add_columns_df[ColumnNames.TYPE.value] = [vt_object.type]
        return pd.concat(
            [vt_df, add_columns_df], axis="columns"
        )  # .set_index([ColumnNames.ID.value])

    def __init__(self, vt_key: Optional[str] = None):
        """
        Create a new instance of VTLookupV3 class.

        Parameters
        ----------
        vt_key: str, optional
            VirusTotal API key, if not supplied, this is read from
            user configuration.

        """
        self._vt_key = vt_key or _get_vt_api_key()
        self._vt_client = vt.Client(apikey=self._vt_key)
        if is_ipython():
            nest_asyncio.apply()

    async def _lookup_ioc_async(
        self, observable: str, vt_type: str, all_props: bool = False
    ) -> pd.DataFrame:
        """
        Look up and single IoC observable.

        Parameters
        ----------
        observable: str
            The observable value
        vt_type: str
            The VT entity type
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Attributes Pandas DataFrame with the properties of the entity

        Raises
        ------
        KeyError
            Unknown vt_type

        """
        if VTEntityType(vt_type) not in self._SUPPORTED_VT_TYPES:
            raise KeyError(
                f"Property type {vt_type} not supported",
                "Valid types are",
                ", ".join(x.value for x in VTEntityType.__members__.values()),
            )

        endpoint_name = self._get_endpoint_name(vt_type)
        try:
            response = self._vt_client.get_object(f"/{endpoint_name}/{observable}")
            return self._parse_vt_object(response, all_props=all_props)
        except vt.APIError as err:
            if err.args and err.args[0] == VT_API_NOT_FOUND:
                return self._item_not_found_df(vt_type, observable)
            raise MsticpyVTNoDataError(
                "An error occurred requesting data from VirusTotal"
            ) from err

    def lookup_ioc(
        self, observable: str, vt_type: str, all_props: bool = False
    ) -> pd.DataFrame:
        """
        Look up and single IoC observable.

        Parameters
        ----------
        observable: str
            The observable value
        vt_type: str
            The VT entity type
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Attributes Pandas DataFrame with the properties of the entity

        Raises
        ------
        KeyError
            Unknown vt_type

        """
        try:
            return _make_sync(
                self._lookup_ioc_async(observable, vt_type, all_props=all_props)
            )
        finally:
            self._vt_client.close()

    async def _lookup_iocs_async(
        self,
        observables_df: pd.DataFrame,
        observable_column: str = ColumnNames.TARGET.value,
        observable_type_column: str = ColumnNames.TARGET_TYPE.value,
        all_props: bool = False,
    ):
        """
        Look up and multiple IoC observables.

        Parameters
        ----------
        observables_df: pd.DataFrame
            A Pandas DataFrame, where each row is an observable
        observable_column:
            ID column of each observable
        observable_type_column:
            Type column of each observable
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Future Attributes Pandas DataFrame with the properties of the entities

        Raises
        ------
        KeyError
            Column not found in observables_df

        """
        _observables_df = observables_df.reset_index()

        for column in [observable_column, observable_type_column]:
            if column not in _observables_df.columns:
                raise KeyError(f"Column {column} not found in observables_df")

        observables_list = _observables_df[observable_column]
        types_list = _observables_df[observable_type_column]
        dfs_futures = []
        for observable, observable_type in zip(observables_list, types_list):
            try:
                ioc_df_future = self._lookup_ioc_async(
                    observable, observable_type, all_props=all_props
                )
                dfs_futures.append(ioc_df_future)
            except KeyError:
                print(
                    "ERROR\t It was not possible to obtain results for",
                    f"{observable_type} {observable}",
                )
        dfs = await asyncio.gather(*dfs_futures)
        return (
            pd.concat(dfs)
            if dfs
            else self._item_not_found_df(
                vt_type=", ".join(types_list),
                observable=", ".join(observables_list),
            )
        )

    def lookup_iocs(
        self,
        observables_df: pd.DataFrame,
        observable_column: str = ColumnNames.TARGET.value,
        observable_type_column: str = ColumnNames.TARGET_TYPE.value,
        all_props: bool = False,
    ):
        """
        Look up and multiple IoC observables.

        Parameters
        ----------
        observables_df: pd.DataFrame
            A Pandas DataFrame, where each row is an observable
        observable_column:
            ID column of each observable
        observable_type_column:
            Type column of each observable
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Attributes Pandas DataFrame with the properties of the entities

        """
        try:
            return _make_sync(
                self._lookup_iocs_async(
                    observables_df,
                    observable_column,
                    observable_type_column,
                    all_props=all_props,
                )
            )
        finally:
            self._vt_client.close()

    # pylint: disable=too-many-locals
    async def _lookup_ioc_relationships_async(
        self,
        observable: str,
        vt_type: str,
        relationship: str,
        limit: int = None,
        all_props: bool = False,
        full_objects: bool = False,
    ):
        """
        Look up and single IoC observable relationships.

        Parameters
        ----------
        observable: str
            The observable value
        vt_type: str
            The VT entity type
        relationship: str
            Desired relationship
        limit: int
            Relations limit
        all_props : bool, optional
            If True, return all properties, by default False
        full_objects : bool, optional
            If True, return the full object rather than just ID links.

        Returns
        -------
            Future Relationship Pandas DataFrame with the relationships of the entity

        Raises
        ------
        KeyError
            Unknown vt_type

        """
        if VTEntityType(vt_type) not in self._SUPPORTED_VT_TYPES:
            raise KeyError(f"Property type {vt_type} not supported")

        endpoint_name = self._get_endpoint_name(vt_type)
        response: vt.object.Object

        if limit is None:
            try:
                response = self._vt_client.get_object(
                    f"/{endpoint_name}/{observable}?relationship_counters=true"
                )
                relationships = response.relationships
                limit = (
                    relationships[relationship]["meta"]["count"]
                    if relationship in relationships
                    else 0
                )
            except KeyError:
                print(
                    f"ERROR: Could not obtain relationship limit for {vt_type} {observable}"
                )
                return self._item_not_found_df(vt_type=vt_type, observable=observable)

        if limit == 0 or limit is None:
            return self._item_not_found_df(vt_type=vt_type, observable=observable)

        obj_relation_type = (
            f"{relationship}" if full_objects else f"relationships/{relationship}"
        )
        try:
            # print(f"Obtaining {limit} relationships for {vt_type} {observable}")
            response = self._vt_client.iterator(
                f"/{endpoint_name}/{observable}/{obj_relation_type}",
                batch_size=40,
                limit=limit,
            )
            vt_objects = [
                self._parse_vt_object(r, all_props=all_props) for r in response
            ]
            result_df = (
                pd.concat(vt_objects, ignore_index=True)
                if vt_objects
                else self._relation_not_found_df(
                    vt_type=vt_type, observable=observable, relationship=relationship
                )
            )

            if vt_objects:
                # Add additional columns (with same size as DF)
                rows = len(result_df)
                add_columns = pd.DataFrame(
                    {
                        ColumnNames.SOURCE.value: [observable] * rows,
                        ColumnNames.SOURCE_TYPE.value: [VTEntityType(vt_type).value]
                        * rows,
                        ColumnNames.RELATIONSHIP_TYPE.value: [relationship] * rows,
                    }
                )

                result_df = (
                    pd.concat([result_df, add_columns], axis="columns")
                    .rename(
                        columns={
                            ColumnNames.ID.value: ColumnNames.TARGET.value,
                            ColumnNames.TYPE.value: ColumnNames.TARGET_TYPE.value,
                        },
                    )
                    .reset_index()
                )
                # result_df.set_index(
                #     [ColumnNames.SOURCE.value, ColumnNames.TARGET.value], inplace=True
                # )
                # if "index" in result_df.columns:
                #     result_df.drop(columns=["index"], inplace=True)
        except vt.APIError as err:
            if err.args and err.args[0] == VT_API_NOT_FOUND:
                return self._relation_not_found_df(vt_type, observable, relationship)
            raise MsticpyVTNoDataError(
                "An error occurred requesting data from VirusTotal"
            ) from err

        return result_df

    def lookup_ioc_relationships(
        self,
        observable: str,
        vt_type: str,
        relationship: str,
        limit: int = None,
        all_props: bool = False,
    ) -> pd.DataFrame:
        """
        Look up single IoC observable relationship links.

        Parameters
        ----------
        observable: str
            The observable value
        vt_type: str
            The VT entity type
        relationship: str
            Desired relationship
        limit: int
            Relations limit
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Relationship Pandas DataFrame with the relationships of the entity

        Notes
        -----
        This method returns relationship links rather than whole objects.
        That is, it will return the IDs of related items in the specified
        `relationship`, if any.

        See Also
        --------
        lookup_ioc_related : return the full related objects.

        """
        try:
            return _make_sync(
                self._lookup_ioc_relationships_async(
                    observable, vt_type, relationship, limit, all_props=all_props
                )
            )
        finally:
            self._vt_client.close()

    def lookup_ioc_related(
        self, observable: str, vt_type: str, relationship: str, limit: int = None
    ) -> pd.DataFrame:
        """
        Look single IoC observable related items.

        Parameters
        ----------
        observable: str
            The observable value
        vt_type: str
            The VT entity type
        relationship: str
            Desired relationship
        limit: int
            Relations limit

        Returns
        -------
        pd.DataFrame
            Any objects with specified `relationship` to the entity

        Notes
        -----
        This method returns full related objects rather than ID links.
        It is less efficient than looking up ID links only.

        See Also
        --------
        lookup_ioc_relationships : return the related IDs.

        """
        try:
            return _make_sync(
                self._lookup_ioc_relationships_async(
                    observable,
                    vt_type,
                    relationship,
                    limit,
                    all_props=True,
                    full_objects=True,
                )
            )
        finally:
            self._vt_client.close()

    async def _lookup_iocs_relationships_async(
        self,
        observables_df: pd.DataFrame,
        relationship: str,
        observable_column: str = ColumnNames.TARGET.value,
        observable_type_column: str = ColumnNames.TARGET_TYPE.value,
        limit: int = None,
        all_props: bool = False,
    ) -> pd.DataFrame:
        """
        Look up and single IoC observable relationships.

        Parameters
        ----------
        observables_df: pd.DataFrame
            A Pandas DataFrame, where each row is an observable
        relationship: str
            Desired relationship
        observable_column:
            ID column of each observable
        observable_type_column:
            Type column of each observable.
        limit: int
            Relations limit
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Future Relationship Pandas DataFrame with the relationships of each observable.

        Raises
        ------
        KeyError
            Column not found in observables_df

        """
        _observables_df = observables_df.reset_index()

        for column in [observable_column, observable_type_column]:
            if column not in _observables_df.columns:
                raise KeyError(f"Column {column} not found in observables df")

        observables_list = _observables_df[observable_column]
        types_list = _observables_df[observable_type_column]
        dfs_futures = []

        for observable, observable_type in zip(observables_list, types_list):
            try:
                dfs_futures.append(
                    self._lookup_ioc_relationships_async(
                        observable,
                        observable_type,
                        relationship,
                        limit,
                        all_props=all_props,
                    )
                )
            except KeyError:
                print(
                    "ERROR:\t It was not possible to get the data for",
                    f"{observable_type} {observable}",
                )
        dfs = await asyncio.gather(*dfs_futures)
        return (
            pd.concat(dfs)
            if len(dfs) > 0
            else self._relation_not_found_df(
                vt_type=", ".join(types_list),
                observable=", ".join(observables_list),
                relationship=relationship,
            )
        )

    def lookup_iocs_relationships(
        self,
        observables_df: pd.DataFrame,
        relationship: str,
        observable_column: str = ColumnNames.TARGET.value,
        observable_type_column: str = ColumnNames.TARGET_TYPE.value,
        limit: int = None,
        all_props: bool = False,
    ) -> pd.DataFrame:
        """
        Look up and single IoC observable relationships.

        Parameters
        ----------
        observables_df: pd.DataFrame
            A Pandas DataFrame, where each row is an observable
        relationship: str
            Desired relationship
        observable_column:
            ID column of each observable
        observable_type_column:
            Type column of each observable.
        limit: int
            Relations limit
        all_props : bool, optional
            If True, return all properties, by default False

        Returns
        -------
            Relationship Pandas DataFrame with the relationships of each observable.

        """
        try:
            return _make_sync(
                self._lookup_iocs_relationships_async(
                    observables_df,
                    relationship,
                    observable_column,
                    observable_type_column,
                    limit,
                    all_props=all_props,
                )
            )

        finally:
            self._vt_client.close()

    def create_vt_graph(
        self, relationship_dfs: List[pd.DataFrame], name: str, private: bool
    ) -> str:
        """
        Create a VirusTotal Graph with a set of Relationship DataFrames.

        Parameters
        ----------
        relationship_dfs:
            List of Relationship DataFrames
        name:
            New graph name
        private
            Indicates if the Graph is private or not.

        Returns
        -------
            Graph ID

        Raises
        ------
            ValueError when private is not indicated.
            ValueError when there are no relationship DataFrames
            MsticpyVTGraphSaveGraphError when Graph can not be saved

        """
        if not isinstance(private, bool):
            raise ValueError("Please indicate if Graph is private or not")

        nodes, edges = self.relationships_to_graph(relationship_dfs)
        graph = VTGraph(self._vt_key, name=name, private=private)
        graph.add_nodes(nodes)

        for edge in edges:
            graph.add_link(**edge)

        try:
            graph.save_graph()
        except vt_graph_errs.SaveGraphError as graph_err:
            graph_mssg = (
                [
                    "Please check you have Private Graph premium feature enabled in",
                    "your subscription. It is possible to create public Graphs",
                    "with 'private=False' input argument",
                ]
                if private
                else [""]
            )

            raise MsticpyVTGraphSaveGraphError(
                "Could not save Graph.", *graph_mssg
            ) from graph_err

        return graph.graph_id

    def get_object(self, vt_id: str, vt_type: str) -> pd.DataFrame:
        """
        Return the full VT object as a DataFrame.

        Parameters
        ----------
        vt_id : str
            The ID of the object
        vt_type : str
            The type of object to query.

        Returns
        -------
        pd.DataFrame
            Single column DataFrame with attribute names as
            index and values as data column.

        Raises
        ------
        KeyError
            Unrecognized VT Type
        MsticpyVTNoDataError
            Error requesting data from VT.

        Notes
        -----
        This calls the underlying VT get_object API directly and
        returns all attributes for the object - hence a very wide
        DataFrame.

        """
        if VTEntityType(vt_type) not in self._SUPPORTED_VT_TYPES:
            # pylint: disable=no-member
            raise KeyError(
                f"Property type {vt_type} not supported",
                "Valid types are",
                ", ".join(x.value for x in VTEntityType.__members__.values()),
            )
            # pylint: enable=no-member

        endpoint_name = self._get_endpoint_name(vt_type)
        try:
            response: vt.object.Object = self._vt_client.get_object(
                f"/{endpoint_name}/{vt_id}"
            )
            result_df = pd.DataFrame(
                {
                    "id": [response.id],
                    "type": [response.type],
                }
            )
            attribs = pd.json_normalize(response.to_dict()["attributes"])
            result_df = pd.concat([result_df, attribs], axis=1)
            result_df["context_attributes"] = response.to_dict().get(
                "context_attributes"
            )
            return timestamps_to_utcdate(result_df)
        except vt.APIError as err:
            if err.args and err.args[0] == VT_API_NOT_FOUND:
                return self._item_not_found_df(vt_type, vt_id)
            raise MsticpyVTNoDataError(
                "An error occurred requesting data from VirusTotal"
            ) from err
        finally:
            self._vt_client.close()

    def get_file_behavior(
        self,
        file_id: Optional[str] = None,
        file_summary: Optional[Dict[str, Any]] = None,
        sandbox: Optional[str] = None,
    ) -> VTFileBehavior:
        """
        Return a VTFileBehavior object with file detonation results.

        Parameters
        ----------
        file_id : Optional[str], optional
            The ID of the file to look up, by default None
        file_summary : Optional[Dict[str, Any]], optional
            VT file summary object dictionary, by default None
        sandbox : str, optional
            Name of specific sandbox to retrieve, by default None
            If None, it will retrieve the behavior summary.

        Returns
        -------
        VTFileBehavior

        """
        vt_behavior = VTFileBehavior(
            self._vt_key, file_id=file_id, file_summary=file_summary
        )
        vt_behavior.get_file_behavior(sandbox=sandbox)
        return vt_behavior

    @staticmethod
    def relationships_to_graph(
        relationship_dfs: List[pd.DataFrame],
    ) -> Tuple[List[Dict[str, Any]], List[Dict[str, Any]]]:
        """
        Generate nodes and edges from relationships.

        Parameters
        ----------
        relationship_dfs : List[pd.DataFrame]
            List of relationship DataFrames

        Returns
        -------
        Tuple[List[Dict[str, Any]], List[Dict[str, Any]]]
            List of nodes (node_id, node_type)
            List of edges (source_node, target_node, connection_type)

        Raises
        ------
        ValueError
            If an empty list is supplied.

        """
        if not relationship_dfs:
            raise ValueError("There are no relationship DataFrames")

        # Set indexes to source-target so that pd.concat aligns on these indexes
        relationship_dfs = [
            rel_df.set_index([ColumnNames.SOURCE.value, ColumnNames.TARGET.value])
            for rel_df in relationship_dfs
        ]
        concatenated_df = pd.concat(relationship_dfs).reset_index()

        # Create nodes DF, with source and target
        sources_df = (
            concatenated_df.groupby(ColumnNames.SOURCE.value)[
                ColumnNames.SOURCE_TYPE.value
            ]
            .first()
            .reset_index()
            .rename(
                columns={
                    ColumnNames.SOURCE.value: ColumnNames.ID.value,
                    ColumnNames.SOURCE_TYPE.value: ColumnNames.TYPE.value,
                }
            )
        )

        target_df = (
            concatenated_df.groupby(ColumnNames.TARGET.value)[
                ColumnNames.TARGET_TYPE.value
            ]
            .first()
            .reset_index()
            .rename(
                columns={
                    ColumnNames.TARGET.value: ColumnNames.ID.value,
                    ColumnNames.TARGET_TYPE.value: ColumnNames.TYPE.value,
                }
            )
        )

        nodes_df = pd.concat([sources_df, target_df])
        nodes = [
            {
                "node_id": row[ColumnNames.ID.value],
                "node_type": row[ColumnNames.TYPE.value],
            }
            for row in nodes_df.itertuples(index=False)
        ]

        edges = [
            {
                "source_node": row[ColumnNames.SOURCE.value],
                "target_node": row[ColumnNames.TARGET.value],
                "connection_type": row[ColumnNames.RELATIONSHIP_TYPE.value],
            }
            for row in concatenated_df.itertuples(index=False)
        ]
        return nodes, edges

    @staticmethod
    def render_vt_graph(graph_id: str, width: int = 800, height: int = 600):
        """
        Display a VTGraph in a Jupyter Notebook.

        Parameters
        ----------
        graph_id:
            Graph ID
        width
            Graph width.
        height
            Graph height

        """
        display(
            HTML(
                f"""
              <iframe
                src="https://www.virustotal.com/graph/embed/{graph_id}"
                width="{width}"
                height="{height}">
              </iframe>

            """
            )
        )

    @classmethod
    def _item_not_found_df(cls, vt_type: str, observable: str):
        not_found_dict = {
            ColumnNames.ID.value: observable,
            ColumnNames.TYPE.value: vt_type,
        }
        vte_type = VTEntityType(vt_type)
        if vte_type not in cls._SUPPORTED_VT_TYPES:
            not_found_dict["status"] = "Unsupported type"
        else:
            not_found_dict.update(
                {key: "Not found" for key in cls._BASIC_PROPERTIES_PER_TYPE[vte_type]}
            )
        return pd.DataFrame([not_found_dict])

    @classmethod
    def _relation_not_found_df(cls, vt_type: str, observable: str, relationship: str):
        not_found_dict = {
            ColumnNames.SOURCE.value: observable,
            ColumnNames.SOURCE_TYPE.value: vt_type,
            ColumnNames.RELATIONSHIP_TYPE.value: relationship,
            ColumnNames.TARGET.value: "Not found",
            ColumnNames.TARGET_TYPE.value: "Not found",
        }
        return pd.DataFrame([not_found_dict])


def _get_vt_api_key() -> Optional[str]:
    """Retrieve the VT key from settings."""
    prov_settings = get_provider_settings("TIProviders")
    vt_settings = prov_settings.get("VirusTotal")
    if vt_settings:
        return vt_settings.args.get("AuthKey")
    return None


def timestamps_to_utcdate(data: pd.DataFrame):
    """Replace Unix timestamps in VT data with Py/pandas Timestamp."""
    columns = data.columns
    for date_col in (col for col in columns if col.endswith("_date")):
        data = (
            data.assign(pd_data=pd.to_datetime(data[date_col], unit="s", utc=True))
            .drop(columns=date_col)
            .rename(columns={"pd_data": date_col})
        )
    return data[columns]
