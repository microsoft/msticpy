# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Useful helper data structure classes for modelling sessions."""

from collections import defaultdict
from typing import Union

from ....common.exceptions import MsticpyException


class StateMatrix(dict):
    """Class for storing trained counts/probabilities."""

    def __init__(self, states: Union[dict, defaultdict], unk_token: str):
        """
        Containr for dict of counts/probs or dict of dicts of cond counts/probs.

        If you try and retrieve the count/probability for an unseen
        command/param/value from the resulting object, it will return
        the value associated with the `unk_token` key.

        Parameters
        ----------
        states: Union[dict, defaultdict]
            Either a dict representing counts or probabilities.
            Or a dict of dicts representing
            conditional counts or conditional probabilities.
            E.g.::

                {'Set-Mailbox': 20,'##UNK##': 1}

            or::

                {'Set-Mailbox': {'Set-Mailbox': 5, '##UNK##': 1},
                '##UNK##': {'Set-Mailbox': 1, '##UNK##': 1}}

        unk_token: str
            dummy token to signify an unseen command (e.g. "##UNK##").
            This token should be present in the `states` keys. And if `states`
            is a dict of dicts, then the `unk_token` should be present in
            the keys of the outer dict and all the inner dicts.

        """
        super().__init__(states)
        if unk_token not in states:
            raise MsticpyException("`unk_token` should be a key in `states`")
        self.states = dict(states)
        self.unk_token = unk_token
        for key, val in self.states.items():
            if isinstance(val, dict):
                self.states[key] = StateMatrix(self.states[key], unk_token)

    def __getitem__(self, item):
        """
        Get value or self.unk_token for unseen cmds/params/values.

        Parameters
        ----------
        item:
            desired key to retrieve value for from the dictionary

        Returns
        -------
        Value associated with the key `item` if it exists, else the
        value associated with the `unk_token`

        """
        if item not in self.states:
            return self.states[self.unk_token]
        return self.states[item]


class Cmd:
    """Class to store commands with accompanying params (and optionally values)."""

    def __init__(self, name: str, params: Union[set, dict]):
        """
        Instantiate the Cmd class.

        Parameters
        ----------
        name: str
            name of the command. e.g. for Exchange online: "Set-Mailbox"
        params: Union[set, dict]
            set of accompanying params or dict of accompanying params and values.
            e.g.::

                {'Identity', 'ForwardingEmailAddress'}

            or::

                {'Identity': 'some identity', 'ForwardingEmailAddress':
                 'an_email@email.com'}


        """
        self.name = name
        self.params = params

    def __str__(self):
        """
        Make string representation more pleasing to the eye.

        Returns
        -------
        New string representation of the object

        """
        return f"Cmd(name='{self.name}', params={self.params})"

    def __repr__(self):
        """
        Make object representation more pleasing to the eye.

        Returns
        -------
        New string representation of the object

        """
        return str(self)
