# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Entity Entity class."""
import pprint
import typing
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Mapping, Optional, Type, Union

import networkx as nx

from ..._version import VERSION
from ...common.utility import export
from .entity_enums import ENTITY_ENUMS
from .entity_graph import Edge, Node

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=invalid-name


@export
class Entity(ABC, Node):
    """
    Entity abstract base class.

    Implements common methods for Entity classes
    """

    ENTITY_NAME_MAP: Dict[str, type] = {}
    _entity_schema: Dict[str, Any] = {}

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
        self.Type = self._get_entity_type_name(type(self))
        # If we have an unknown entity see if we a type passed in
        if self.Type == "unknownentity" and "Type" in kwargs:
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

        if kwargs:
            self.__dict__.update(kwargs)

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
                        self.add_edge(child_entity, attrs={"name": attr})
            else:
                # else try to instantiate an entity
                entity_type = None
                if isinstance(val, type) and issubclass(val, Entity):
                    entity_type = val
                self[attr] = Entity.instantiate_entity(
                    src_entity[attr], entity_type=entity_type
                )
                if isinstance(self[attr], Entity):
                    self.add_edge(self[attr], attrs={"name": attr})

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
        if name in self._entity_schema:
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
        return pprint.pformat(self._to_dict(self), indent=2, width=100)

    def __repr__(self) -> str:
        """Return repr of entity."""
        params = ", ".join(
            [f"{name}={val}" for name, val in self.properties.items() if val]
        )
        if len(params) > 80:
            params = params[:80] + "..."
        return f"{self.__class__.__name__}({params})"

    def _to_dict(self, entity) -> dict:
        """Return as simple nested dictionary."""
        return {
            prop: self._to_dict(val) if isinstance(val, Entity) else val
            for prop, val in entity.properties.items()
            if val is not None
        }

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
            if not name.startswith("_")
        }

    @property
    @abstractmethod
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
        if entity_type_name in cls.ENTITY_NAME_MAP:
            return cls.ENTITY_NAME_MAP[entity_type_name](raw_entity)

        raise TypeError("Could not find a suitable type for {}".format(entity_type))

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
        name = next(
            iter(
                (key for key, val in cls.ENTITY_NAME_MAP.items() if val == entity_type)
            )
        )
        return name or "unknown"

    @property
    def node_properties(self) -> Dict[str, Any]:
        """
        Return all public properties that are not entities.

        Returns
        -------
        Dict[str, Any]
            Dictionary of name, value properties.

        """
        return {
            name: value
            for name, value in self.properties.items()
            if not isinstance(value, (Entity, list))
        }

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
            graph.add_node(self, **self.node_properties)
        for edge in self.edges:
            if graph.has_edge(edge.source, edge.target):
                continue
            graph.add_edge(edge.source, edge.target, **edge.attrs)

            for node in (edge.source, edge.target):
                # If this node has edges that are not in our graph
                # call to_networkx recursively on that node.
                if any(
                    edge
                    for edge in node.edges
                    if not graph.has_edge(edge.source, edge.target)
                ):
                    ent_node = typing.cast(Entity, node)
                    ent_node.to_networkx(graph)
        return graph
