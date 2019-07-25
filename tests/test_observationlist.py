# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Unit test class for Observations class."""
# imports
import unittest
import os

from ..msticpy.nbtools.observationlist import Observation, Observations
from ..msticpy._version import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"

_test_data_folders = [
    d for d, _, _ in os.walk(os.getcwd()) if d.endswith("/tests/testdata")
]
if len(_test_data_folders) == 1:
    _TEST_DATA = _test_data_folders[0]
else:
    _TEST_DATA = "./tests/testdata"


_TEST_FIELDS = {
    "caption",
    "description",
    "data",
    "data_type",
    "link",
    "score",
    "tags",
    "additional_properties",
}


class TestObservations(unittest.TestCase):
    """Unit test class."""

    def test_create_observation_class(self):

        self.assertIn("caption", Observation.required_fields())
        self.assertIn("data", Observation.required_fields())

        fields_set = set(Observation.all_fields())

        self.assertEqual(len(fields_set.symmetric_difference(_TEST_FIELDS)), 0)

        # Shouldn't be able to create an observation without required
        # properties
        with self.assertRaises(TypeError):
            Observation()

        obs = Observation(caption="My caption", data=fields_set)

        ob1 = Observation(
            caption="O1",
            description="Description1",
            data=obs,
            data_type="object",
            tags=["t1", "t2", "t3"],
        )
        ob1.additional_properties["foo_field"] = "xyz"
        self.assertEqual(ob1.caption, "O1")
        self.assertEqual(ob1.description, "Description1")
        self.assertEqual(ob1.data, obs)
        self.assertEqual(ob1.data_type, "object")
        self.assertEqual(ob1.tags, ["t1", "t2", "t3"])
        self.assertIn("foo_field", ob1.additional_properties)
        self.assertEqual("xyz", ob1.additional_properties["foo_field"])

    def test_create_observations(self):
        obs_list = Observations()

        obs = Observation(caption="My caption", data=_TEST_FIELDS)

        ob1 = Observation(
            caption="O1",
            description="Description1",
            data=_TEST_FIELDS,
            data_type="object",
            tags=["t1", "t2", "t3"],
        )
        obs_list.add_observation(obs)
        obs_list.add_observation(ob1)
        self.assertEqual(len(obs_list.observations), 2)

        # adding another with same caption should overwrite
        ob2 = Observation(
            caption="O1",
            description="Description2",
            data=_TEST_FIELDS,
            data_type="object",
            tags=["t1", "t2", "t3"],
        )
        obs_list.add_observation(ob2)
        self.assertEqual(len(obs_list.observations), 2)
        self.assertEqual(obs_list.observations["O1"].description, "Description2")
        self.assertEqual(obs_list.observations["O1"], ob2)

        # Add observation with keywords
        obs_list.add_observation(
            caption="O3",
            description="Description1",
            data=_TEST_FIELDS,
            data_type="object",
            tags=["t1", "t2", "t3"],
            foo="some extra data",
        )
        self.assertEqual(len(obs_list.observations), 3)
        self.assertEqual(
            obs_list.observations["O3"].additional_properties["foo"], "some extra data"
        )

        obs_list.display_observations()
