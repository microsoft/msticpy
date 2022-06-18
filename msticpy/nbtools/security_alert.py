# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for SecurityAlert class."""
import json
from json import JSONDecodeError
from typing import Any, Dict, List

import pandas as pd
from deprecated.sphinx import deprecated

from .._version import VERSION
from ..common.utility import export
from ..datamodel.entities import Entity, UnknownEntity
from .security_base import SecurityBase

__version__ = VERSION
__author__ = "Ian Hellen"


@deprecated(reason="Replaced by Alert entity in datamodel", version="1.7.0")
@export
class SecurityAlert(SecurityBase):
    """
    Security Alert Class.

    Instantiates a security alert from a pandas Series and provides
    convenience access methods to retrieve properties.
    """

    def __init__(self, src_row: pd.Series = None):
        """Instantiate a security alert from a pandas Series."""
        super().__init__(src_row=src_row)

        # add entities to dictionary to remove dups
        self._src_entities: Dict[int, Entity] = {}

        self.extended_properties: Dict[str, Any] = {}
        if src_row is not None:
            if "Entities" in src_row:
                self._extract_entities(src_row)

            if "ExtendedProperties" in src_row:
                if isinstance(src_row.ExtendedProperties, dict):
                    self.extended_properties = src_row.ExtendedProperties
                elif isinstance(src_row.ExtendedProperties, str):
                    try:
                        self.extended_properties = json.loads(
                            src_row.ExtendedProperties
                        )
                    except JSONDecodeError:
                        pass
        self._find_os_family()

    @property
    def entities(self) -> List[Entity]:
        """Return a list of the Security Alert entities."""
        return list(self._src_entities.values())

    @property
    def query_params(self) -> Dict[str, Any]:
        """
        Query parameters derived from alert.

        Returns
        -------
            Dict[str, Any]
                Dictionary of parameter names/value

        """
        params_dict = super().query_params
        if (
            "system_alert_id" not in params_dict
            or params_dict["system_alert_id"] is None
        ):
            params_dict["system_alert_id"] = self._ids["SystemAlertId"]
        return params_dict

    def to_html(self, show_entities=False) -> str:
        """Return the item as HTML string."""
        if self.properties:
            start = self.properties.get(
                "StartTimeUtc", self.properties.get("StartTime", "no timestamp")
            )
            name = self.properties.get(
                "AlertDisplayName",
                self.properties.get("DisplayName", "no alert name"),
            )
            entity = self.properties.get("CompromisedEntity", "unknown")
            title = f"""
            <h3>Alert: '{name}'</h3>
            <b>Alert_time:</b> {start},
            <b>Compr_entity:</b> {entity},
            <b>Alert_id:</b> self.properties['SystemAlertId']
            """
        else:
            title = "Alert has no data."
        return title + super().to_html(show_entities)

    # Public methods
    def __str__(self):
        """Print the alert contents to stdout."""
        alert_props = str(super().__str__())

        if self.extended_properties:
            str_rep = [
                f"ExtProp: {prop}: {val}"
                for prop, val in self.extended_properties.items()
            ]
            alert_props = alert_props + "\n" + "\n".join(str_rep)

        return alert_props

    # Private methods
    def _resolve_entity_refs(self):
        """
        Resolve and replace entity properties that are '$ref' type.

        When serialized the nested entities can be references to other
        referenced objects in the collection. This iterates through
        the raw entities and replaces referenced items with the actual
        object reference. If the Id referenced by this property exists
        in the entities dictionary we replace the original property
        with a reference to the entity in the dictionary.

        """
        for _, entity in self._src_entities.items():
            if not isinstance(entity, Entity):
                continue
            # Resolve all the simple references
            ref_props = {
                name: prop
                for name, prop in entity.properties.items()
                if isinstance(prop, dict) and "$ref" in prop
            }
            for prop_name, prop_val in ref_props.items():
                entity_id = prop_val["$ref"]
                if entity_id in self._src_entities:
                    entity[prop_name] = self._src_entities[entity_id]
                    entity.add_edge(entity[prop_name], edge_attrs={"name": prop_name})
            # Resolve all the lists of references
            ref_props_multi = {
                name: prop
                for name, prop in entity.properties.items()
                if isinstance(prop, list)
                and any(elem for elem in prop if "$ref" in elem)
            }
            for prop_name, prop_val in ref_props_multi.items():
                for idx, elem in enumerate(prop_val):
                    if not isinstance(elem, dict):
                        continue
                    entity_id = elem["$ref"]
                    if entity_id in self._src_entities:
                        entity[prop_name][idx] = self._src_entities[entity_id]
                        entity.add_edge(
                            self._src_entities[entity_id],
                            edge_attrs={"name": prop_name},
                        )

    def _extract_entities(self, src_row):  # noqa: MC0001
        input_entities = []

        if isinstance(src_row.ExtendedProperties, str):
            try:
                ext_props = json.loads(src_row["ExtendedProperties"])
                for ent, val in ext_props.items():
                    if ent in ["IpAddress", "Username"]:
                        input_entities.append({"Entity": val, "Type": ent})
            except json.JSONDecodeError:
                pass

        if isinstance(src_row.Entities, str):
            try:
                input_entities += json.loads(src_row["Entities"])
            except json.JSONDecodeError:
                pass
        elif isinstance(src_row.Entities, list):
            input_entities += src_row.Entities

        for ent in input_entities:
            try:
                entity = Entity.instantiate_entity(ent)
            except TypeError:
                # if we didn't instantiate a known entity
                # just add it as it is
                entity = UnknownEntity(**ent)
            if "$id" in ent:
                self._src_entities[ent["$id"]] = entity
            self._extract_child_entities(ent)
        self._resolve_entity_refs()

    def _extract_child_entities(self, src_entity):
        for prop in src_entity.values():
            if prop is None:
                continue
            if (
                isinstance(prop, dict)
                and "$id" in prop
                and prop["$id"] not in self._src_entities
            ):
                self._extract_child_entities(prop)
                try:
                    entity = Entity.instantiate_entity(prop)
                except TypeError:
                    # if we didn't instantiate a known entity
                    # just add it as it is
                    entity = UnknownEntity(**prop)
                self._src_entities[prop["$id"]] = entity
