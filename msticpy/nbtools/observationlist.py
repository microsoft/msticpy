# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Observation summary collector."""
from collections import OrderedDict
from typing import Mapping, Any, Optional, List, Dict

import attr
from attr import Factory
from IPython.display import display, Markdown

from .._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


# Query definition
@attr.s(auto_attribs=True)
class Observation:
    """
    Observation definition.

    Attributes
    ----------
    caption : str
        The title and index of the observation. Must
        be unique in the observation set.
    description : Optional[str]
        Text description of the observation.
        (default is None)
    data : Any
        The data to be stored for the observation (e.g.
        a pandas DataFrame). The object should implement
        a useable __repr__ to display correctly.
    data_type : Optional[str]
        The data type of the `data` property
    link : Optional[str]
        Link (usually a document-local link) to the
        originating section of the notebook.
        (default is None)
    score : int
        The risk score associated with the observation.
        (default is 0)
    tags : List[str]
        Optional list of tags.
    additional_properties Dict[str, Any]
        Additional properties not covered
        by core properties.

    """

    caption: str
    data: Any
    description: Optional[str] = None
    data_type: Optional[str] = None
    link: Optional[str] = None
    score: int = 0
    tags: List[str] = Factory(list)
    additional_properties: Dict[str, Any] = Factory(dict)

    @classmethod
    def required_fields(cls) -> List[str]:
        """
        Return required fields for Observation instance.

        Returns
        -------
        List[str]
            List of field names.

        """
        return ["caption", "data"]

    @classmethod
    def all_fields(cls) -> List[str]:
        """
        Return all fields of Observation class.

        Returns
        -------
        List[str]
            List of all field names.

        """
        return [field.name for field in attr.fields(cls)]


class Observations:
    """Class to collect and display investigation observations."""

    def __init__(self, observationlist: "Observations" = None):
        """
        Create an observation list.

        Parameters
        ----------
        observationlist : Observations, optional
            Initialize from an existing Observations list
            (the default is None)

        """
        self.observation_list: Dict[str, Observation] = OrderedDict()
        if observationlist is not None:
            self.observation_list.update(observationlist.observations)

    @property
    def observations(self) -> Mapping[str, Observation]:
        """
        Return the current list of Observations.

        Returns
        -------
        Mapping[str, Observation]
            The current ordered dictionary of Observations

        """
        return self.observation_list

    def display_observations(self):
        """Display the current observations using IPython.display."""
        for observation in self.observation_list.values():
            display(Markdown(f"### {observation.caption}"))
            display(Markdown(observation.description))
            display(Markdown(f"Score: {observation.score}"))
            if observation.link:
                display(Markdown(f"[Go to details](#{observation.link})"))
            if observation.tags:
                display(Markdown(f'tags: {", ".join(observation.tags)}'))
            display(observation.data)
            if observation.additional_properties:
                display(Markdown("### Additional Properties"))
                for key, val in observation.additional_properties.items():
                    display(Markdown(f"**{key}**: {val}"))

    def add_observation(self, observation: Observation = None, **kwargs):
        """
        Add an observation.

        Add an observation as an Observation instance or as a
        set of keyword parameters (see Observation class for
        acceptable values). Any keyword parameters that are not
        properties of Observation will be stored in the
        Observation.additional_properties dictionary

        Parameters
        ----------
        observation : Observation
            An observation instance.

        Other Parameters
        ----------------
        kwargs : str, Any
            List of key value pairs of the property names
            and values of the Observation to be stored.

        """
        if observation is not None:
            self.observation_list[observation.caption] = observation
        else:
            req_fields = set(Observation.required_fields())
            missing_fields = req_fields.difference(kwargs.keys())
            if missing_fields:
                raise ValueError(
                    f"The following fields are required",
                    f"in an Observation: {missing_fields}",
                )

            core_fields = {
                k: v for k, v in kwargs.items() if k in Observation.all_fields()
            }
            new_observation = Observation(**core_fields)
            addl_fields = {
                k: v for k, v in kwargs.items() if k not in Observation.all_fields()
            }
            # pylint: disable=no-member
            new_observation.additional_properties.update(addl_fields)
            self.observation_list[new_observation.caption] = new_observation
