# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""query_schema test class."""
import unittest
import json

# from .. nbtools import query_builtin_queries as queries
# from .. nbtools.query_mgr import replace_query_params, add_query
# from .. nbtools.query_defns import DataFamily, DataEnvironment, KqlQuery
from .. nbtools.entityschema import Entity, Account, Host, Process, File, IpAddress


class Testnbtools(unittest.TestCase):
    """Unit test class."""

    def entity_creation(self):
        try:
            file = './msticpy/tests/testdata/entities.json'
            with open(file, 'r') as file_handle:
                txt = file_handle.read()
                entity_dict = json.loads(txt)

            parsed_entities = []
            for _, entity in entity_dict.items():
                e = Entity.instantiate_entity(entity)
                self.assertIsInstance(e, Entity)

                if e['Type'] == 'account':
                    self.assertIsInstance(e, Account)
                    self.assertTrue('Name' in e)
                    self.assertGreater(len(e.Name), 0)
                elif e['Type'] == 'host':
                    self.assertIsInstance(e, Host)
                    self.assertTrue('HostName' in e)
                    self.assertGreater(len(e.HostName), 0)
                elif e['Type'] == 'process':
                    self.assertIsInstance(e, Process)
                    self.assertTrue('ProcessId' in e)
                    self.assertGreater(len(e.ProcessId), 0)
                elif e['Type'] == 'file':
                    self.assertIsInstance(e, File)
                    self.assertTrue('Name' in e)
                    self.assertGreater(len(e.Name), 0)
                elif e['Type'] == 'ipaddress':
                    self.assertIsInstance(e, IpAddress)
                    self.assertTrue('Address' in e)
                    self.assertGreater(len(e.Address), 0)

                parsed_entities.append(e)

            self.assertGreaterEqual(len(parsed_entities), 7)

        except Exception as ex:
            self.fail(msg='Exception {}'.format(str(ex)))


if __name__ == '__main__':
    unittest.main()
    print('bye')
