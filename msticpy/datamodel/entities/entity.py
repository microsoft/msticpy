# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Entity Entity class."""
import json
import pprint
import typing
from abc import ABC
from copy import deepcopy
from datetime import datetime
from typing import Any, Dict, List, Mapping, Optional, Type, Union

import networkx as nx

from ..._version import VERSION
from ...common.utility import export, valid_pyname
from .entity_enums import ENTITY_ENUMS
from .entity_graph import Node

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name, too-few-public-methods


@export
class ContextObject:
    """Information object attached to entity but is not an Entity."""


# pylint: enable=too-few-public-methods


class _EntityJSONEncoder(json.JSONEncoder):
    """Encode entity to JSON."""

    def default(self, o):
        if isinstance(o, Entity):
            return {
                name: value
                for name, value in o.properties.items()
                if value and name != "edges"
            }
        # Let the base class default method raise the TypeError
        return json.JSONEncoder.default(self, o)


# Future: replace setting entity properties in __dict__ with
# setattr (to support attributes implemented as properties)
@export
class Entity(ABC, Node):
    """
    Entity abstract base class.

    Implements common methods for Entity classes
    """

    ENTITY_NAME_MAP: Dict[str, type] = {}
    _entity_schema: Dict[str, Any] = {}
    ID_PROPERTIES: List[str] = []
    JSONEncoder = _EntityJSONEncoder

    def __init__(self, src_entity: Mapping[str, Any] = None, **kwargs):
        """
        Create a new instance of an entity.

        Parameters
        ----------
        src_entity : Mapping[str, Any], optional
            If src_entity is supplied it attempts to extract common
            properties from the source entity and assign them to
            the new instance. (the default is None)

        Other Parameters
        ----------------
        kwargs : Dict[str, Any]
            Supply the entity properties as a set of
            kw arguments.

        """
        super().__init__()
        self.TimeGenerated = datetime.utcnow()
        self.Type = self._get_entity_type_name(type(self))
        # If we have an unknown entity see if we a type passed in
        if self.Type == "unknown" and "Type" in kwargs:
            self.Type = kwargs["Type"]
        # Make sure Type is in the class schema dictionary
        self._entity_schema["Type"] = None

        # if we didn't populate AdditionalData, add an empty dict in case it's
        # needed
        if "AdditionalData" not in self:
            self.AdditionalData = {}

        if src_entity is not None:
            self._extract_src_entity(src_entity)
            # add AdditionalData dictionary if it's populated
            if "AdditionalData" in src_entity:
                self.AdditionalData = src_entity["AdditionalData"]
            if "TimeGenerated" in src_entity:
                self.TimeGenerated = src_entity["TimeGenerated"]

        if kwargs:
            self.__dict__.update(kwargs)

    @classmethod
    def create(cls, src_entity: Mapping[str, Any] = None, **kwargs) -> "Entity":
        """
        Create an entity from a mapping type (e.g. pd.Series) or dict or kwargs.

        Returns
        -------
        Entity
            Instantiated entity

        Notes
        -----
        The entity type should be specified as "Type", in either a key of `src_entity`
        or as a keyword argument.

        """
        ent_type = (
            src_entity.get("Type") or src_entity.get("type")
            if src_entity
            else kwargs.get("Type") or kwargs.get("type")
        )
        if not ent_type:
            ent_type = "unknown"
        ent_cls = cls.ENTITY_NAME_MAP.get(ent_type)
        if not ent_cls:
            ent_cls = cls.ENTITY_NAME_MAP["unknown"]
        return ent_cls(src_entity, **kwargs)

    def _extract_src_entity(self, src_entity: Mapping[str, Any]):
        """
        Extract source entity properties.

        Parameters
        ----------
        src_entity : Mapping[str, Any]
            The source mappable object from which to
            extract entity properties.

        """
        schema_dict = self._entity_schema.copy()
        schema_dict["Type"] = None
        for attr, val in schema_dict.items():
            if attr not in src_entity:
                continue
            self[attr] = src_entity[attr]

            if val is None:
                continue
            try:
                # If the property is an enum
                if val in ENTITY_ENUMS.values():
                    self[attr] = val[src_entity[attr]]
                elif val in ENTITY_ENUMS:
                    self[attr] = ENTITY_ENUMS[val][src_entity[attr]]
                    continue
            except KeyError:
                # Catch key errors from invalid enum values
                self[attr] = None

            if isinstance(val, tuple):
                self._instantiate_from_value(attr, val, src_entity)
            else:
                self._instantiate_from_entity(attr, val, src_entity)

    def _instantiate_from_value(self, attr, val, src_entity):
        # if the property is a collection
        entity_type = None
        if isinstance(val[1], (type)) and issubclass(val[1], Entity):
            entity_type = val[1]
        entity_list = [
            Entity.instantiate_entity(col_entity, entity_type=entity_type)
            for col_entity in src_entity[attr]
        ]

        self[attr] = entity_list
        for child_entity in entity_list:
            if isinstance(child_entity, Entity):
                self.add_edge(child_entity, edge_attrs={"name": attr})

    def _instantiate_from_entity(self, attr, val, src_entity):
        # else try to instantiate an entity
        entity_type = None
        if isinstance(val, type) and issubclass(val, Entity):
            entity_type = val
        self[attr] = Entity.instantiate_entity(
            src_entity[attr], entity_type=entity_type
        )
        if isinstance(self[attr], Entity):
            self.add_edge(self[attr], edge_attrs={"name": attr})

    def __getitem__(self, key: str):
        """Allow property get using dictionary key syntax."""
        if key in self.__dict__:
            return self.__dict__[key]
        if key in self._entity_schema:
            return None
        raise KeyError

    def __setitem__(self, key: str, value: Any):
        """Allow property set using dictionary key syntax."""
        self.__dict__[key] = value

    def __contains__(self, key: str):
        """Allow property in test."""
        # In operator overload
        return key in self.__dict__

    def __getattr__(self, name: str):
        """Return the value of the named property 'name'."""
        props = ["name_str", "description_str"]
        if name in self._entity_schema or name in props:
            return None
        raise AttributeError(f"{name} is not a valid attribute.")

    def __iter__(self):
        """Iterate over entity_properties."""
        return iter(self.properties)

    def __len__(self) -> int:
        """Return length/number of entity_properties."""
        return len(self.properties)

    def __str__(self) -> str:
        """Return string representation of entity."""
        return pprint.pformat(self._to_dict(), indent=2, width=100)

    def __repr__(self) -> str:
        """Return repr of entity."""
        params = ", ".join(
            f"{name}={val}" for name, val in self.properties.items() if val
        )
        if self.edges:
            params = f"{params}, edges={'. '.join(str(edge) for edge in self.edges)}"

        if len(params) > 80:
            params = f"{params[:80]}..."
        return f"{self.__class__.__name__}({params})"

    def _to_dict(self) -> dict:
        """Return as simple nested dictionary."""
        # pylint: disable=protected-access
        return {
            prop: val._to_dict() if isinstance(val, Entity) else val
            for prop, val in self.properties.items()
            if val is not None
        }
        # pylint: enable=protected-access

    def _repr_html_(self) -> str:
        """
        Display entity in IPython/Notebook.

        Returns
        -------
        HTML
            IPython HTML object

        """
        return self.to_html()

    def to_html(self) -> str:
        """
        Return HTML representation of entity.

        Returns
        -------
        str
            HTML representation of entity

        """
        e_text = str(self)
        e_type = self.Type
        e_text = e_text.replace("\n", "<br>").replace(" ", "&nbsp;")
        return f"<h3>{e_type}</h3>{e_text}"

    def to_json(self):  # noqa: N802
        """Return object as a JSON string."""
        return json.dumps(self, cls=self.JSONEncoder)

    def __eq__(self, other: Any) -> bool:
        """
        Return True if the entities have the same properties/values.

        Parameters
        ----------
        other : Any
            The entity (object) to compare

        Returns
        -------
        bool
            True if the two objects have the same property values

        """
        if self.__class__ != other.__class__ or not isinstance(other, Entity):
            return False
        return self.properties == other.properties

    def __hash__(self) -> int:
        """Return the hash of the entity based on non-empty property values."""
        return hash(
            " ".join(
                f"{prop}:{val}" for prop, val in self.properties.items() if str(val)
            )
        )

    def is_equivalent(self, other: Any) -> bool:
        """
        Return True if the entities are equivalent.

        Parameters
        ----------
        other : Any
            The entity to check

        Returns
        -------
        bool
            True if equivalent.

        Notes
        -----
        This method checks that the compared entities do not have
        any property values with conflicting values. E.g.
        self.A == other.A
        self.B == "xyz" and other.B == None
        self.C == [] and other.C == [1, 2, 3]

        """
        if self == other:
            return True
        if not isinstance(other, Entity):
            return False
        return not any(
            self.__dict__[prop] != other.__dict__[prop]
            and self.__dict__[prop]
            and other.__dict__[prop]
            for prop in self.__dict__  # pylint: disable=consider-using-dict-items
        )

    def merge(self, other: Any) -> "Entity":
        """
        Merge with other entity to create new entity.

        Returns
        -------
        Entity
            Merged entity.

        Raises
        ------
        AttributeError
            If the entities cannot be merged.

        """
        if self == other:
            return self
        if not self.can_merge(other):
            raise AttributeError("Entities cannot be merged.")
        merged = deepcopy(self)
        for prop, value in other.properties.items():
            if not value:
                continue
            if not self.__dict__[prop]:
                setattr(merged, prop, value)
            # Future (ianhelle) - cannot merge ID field
        if other.edges:
            self.edges.update(other.edges)
        return merged

    def can_merge(self, other: Any) -> bool:
        """
        Return True if the entities can be merged.

        Parameters
        ----------
        other : Any
            The other entity (object) to check

        Returns
        -------
        bool
            True if other has no conflicting properties.

        """
        if self.__class__ != other.__class__ or not isinstance(other, Entity):
            return False

        other_id_props = {
            prop: getattr(other, prop, None)
            for prop in other.ID_PROPERTIES
            if getattr(other, prop, None) is not None
        }
        self_id_props = {
            prop: getattr(self, prop, None)
            for prop in self.ID_PROPERTIES
            if getattr(self, prop, None) is not None
        }
        # Return True if there is no overlap
        overlap = self_id_props.keys() | other_id_props.keys()
        if not overlap:
            return True
        return all(getattr(self, prop) == getattr(other, prop) for prop in overlap)

    @property
    def properties(self) -> dict:
        """
        Return dictionary properties of entity.

        Returns
        -------
        dict
            Entity properties.

        """
        return {
            name: value
            for name, value in self.__dict__.items()
            if not name.startswith("_") and name != "edges" and value
        }

    @property
    def description_str(self) -> str:
        """
        Return Entity Description.

        Returns
        -------
        str
            Entity description (optional). If not overridden
            by the Entity instance type, it will return the
            Type string.

        """
        return self.Type

    @property
    def name_str(self) -> str:
        """
        Return Name Description.

        Returns
        -------
        str
            Entity Name (optional). If not overridden
            by the Entity instance type, it will return the
            class name string.

        """
        return self.__class__.__name__

    @classmethod
    def instantiate_entity(
        cls, raw_entity: Mapping[str, Any], entity_type: Optional[Type] = None
    ) -> Union["Entity", Mapping[str, Any]]:
        """
        Class factory to return entity from raw dictionary representation.

        Parameters
        ----------
        raw_entity : Mapping[str, Any]
            A mapping object (e.g. dictionary or pandas Series)
            that contains the properties of the entity.
        entity_type : Optional[Type]
            The entity type to create, by default None.

        Returns
        -------
        Entity
            The instantiated entity

        """
        if "Type" not in raw_entity and entity_type is None:
            return raw_entity

        entity_type_name = raw_entity.get("Type")

        if not entity_type_name and entity_type:
            entity_type_name = cls._get_entity_type_name(entity_type)

        if entity_type:
            return entity_type(raw_entity)
        if entity_type_name and entity_type_name.lower() in cls.ENTITY_NAME_MAP:
            return cls.ENTITY_NAME_MAP[entity_type_name.lower()](raw_entity)

        raise TypeError(f"Could not find a suitable type for {entity_type}")

    @classmethod
    def _get_entity_type_name(cls, entity_type: Type) -> str:
        """
        Get V3 entity name for an entity.

        Parameters
        ----------
        entity_type : Type
            The Entity class

        Returns
        -------
        str
            The V3 serialized name.

        """
        try:
            name = next(
                iter(
                    (
                        key
                        for key, val in cls.ENTITY_NAME_MAP.items()
                        if val == entity_type
                    )
                )
            )
        except StopIteration:
            name = "unknown"
        return name

    @property
    def node_properties(self) -> Dict[str, Any]:
        """
        Return all public properties that are not entities.

        Returns
        -------
        Dict[str, Any]
            Dictionary of name, value properties.

        """
        props = {
            name: str(value)
            for name, value in self.properties.items()
            if not isinstance(value, (Entity, list)) and name != "edges"
        }
        props["Description"] = self.description_str
        props["Name"] = self.name_str
        return props

    def to_networkx(self, graph: nx.Graph = None) -> nx.Graph:
        """
        Return networkx graph of entities.

        Parameters
        ----------
        graph : nx.Graph, optional
            Graph to add entities to. If not supplied the function
            creates and returns a new graph.
            By default None

        Returns
        -------
        nx.Graph
            Graph with entity and any connected entities.

        """
        graph = graph or nx.Graph()
        if not graph.has_node(self):
            graph.add_node(self.name_str, **self.node_properties)
        for edge in self.edges:
            if not isinstance(edge.source, Entity) or not isinstance(
                edge.target, Entity
            ):
                continue
            if graph.has_edge(edge.source.name_str, edge.target.name_str):
                continue
            graph.add_edge(edge.source.name_str, edge.target.name_str, **edge.attrs)

            for node in (edge.source, edge.target):
                # If this node has edges that are not in our graph
                # call to_networkx recursively on that node.
                if any(
                    edge
                    for edge in node.edges
                    if isinstance(edge.source, Entity)
                    and isinstance(edge.target, Entity)
                    and not graph.has_edge(edge.source.name_str, edge.target.name_str)
                ):
                    ent_node = typing.cast(Entity, node)
                    ent_node.to_networkx(graph)
        return graph

    @classmethod
    def get_pivot_list(cls, search_str: Optional[str] = None) -> List[str]:
        """
        Return list of current pivot functions.

        Returns
        -------
        List[str]
            List of pivot functions assigned to entity.

        """
        pivots = []
        for prop in dir(cls):
            attr = getattr(cls, prop)
            if hasattr(attr, "pivot_properties"):
                pivots.append(prop)
                continue
            if attr.__class__.__name__ != "PivotContainer":
                continue
            pivots.extend(
                f"{prop}.{name}"
                for name, qt_attr in attr
                if hasattr(qt_attr, "pivot_properties")
            )

        return sorted(
            pivot
            for pivot in pivots
            if search_str is None or search_str.casefold() in pivot.casefold()
        )

    # alias for get_pivot_list
    pivots = get_pivot_list

    def list_pivot_funcs(self):
        """Print list of pivot functions assigned to entity."""
        print("\n".join(self.get_pivot_list()))

    @classmethod
    def make_pivot_shortcut(cls, func_name: str, target: str, overwrite: bool = False):
        """
        Add a shortcut to a pivot function to the class.

        Parameters
        ----------
        func_name : str
            The name of source pivot function.
        target : str
            The shortcut name (this will be a member function of the class)
        overwrite : bool, optional
            Force overwrite an existing pivot function, by default False

        Raises
        ------
        AttributeError
            The source function does not exist
        TypeError
            The source function is not a pivot function.
        TypeError
            The target attribute exists and is not a pivot function
        AttributeError
            The target function exists and 'overwrite=True' was not specified.

        """
        func_path = func_name.split(".") if "." in func_name else [func_name]
        curr_attr: Optional[Any] = cls
        for path in func_path:
            curr_attr = getattr(curr_attr, path, None)  # type: ignore
            if not curr_attr:
                raise AttributeError(f"No function found for {func_name}")
        if not hasattr(curr_attr, "pivot_properties"):
            raise TypeError(f"Function {func_name} is not a Pivot function")
        tgt_name = valid_pyname(target)
        if tgt_name != target:
            print(f"{target} rename to valid Python identifier {tgt_name}")

        existing_attr = getattr(cls, tgt_name, None)
        if existing_attr:
            if not hasattr(existing_attr, "pivot_properties"):
                raise TypeError(
                    f"Cannot overwrite existing an attribute {tgt_name}.",
                    "This is not a pivot function.",
                )
            if not overwrite:
                raise AttributeError(
                    f"{cls.__name__} already has an attribute {tgt_name}",
                    "Use 'overwrite' parameter to force.",
                )
        setattr(cls, tgt_name, curr_attr)

    @classmethod
    def del_pivot_shortcut(cls, func_name: str):
        """
        Remove a pivot shortcut.

        Parameters
        ----------
        func_name : str
            The name of the shortcut function.

        Raises
        ------
        AttributeError
            The class does not have an attribute `func_name`
        TypeError
            The attribute to delete is not a pivot shortcut.

        """
        existing_attr = getattr(cls, func_name, None)
        if not existing_attr:
            raise AttributeError(
                f"{cls.__name__} has no attribute {func_name}",
            )
        if not hasattr(existing_attr, "pivot_properties"):
            raise TypeError(
                f"Cannot delete an attribute {func_name} that isn't a pivot function.",
                "This is not a pivot function.",
            )
        delattr(cls, func_name)


def camelcase_property_names(input_ent: Dict[str, Any]) -> Dict[str, Any]:
    """Change initial letter Microsoft Sentinel API entity properties to upper case."""
    return {key[0].upper() + key[1:]: input_ent[key] for key in input_ent}
