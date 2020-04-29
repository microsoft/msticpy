from collections import defaultdict
from typing import Union


class StateMatrix(dict):
    def __init__(self, states: Union[dict, defaultdict], unk_token: str):
        """
        Takes in dict or dict of dicts of prior counts/probs or transition counts/probs respectively.
        If you try and retrieve the count/probability for an unseen command from the result, it will return the value
        associated with the `unk_token` key.

        Parameters
        ----------
        states: Union[dict, defaultdict]
            Either a dict representing prior counts or prior probabilities. Or a dict of dicts
            representing transition counts or transitions probabilities.
            E.g.
                {'Set-Mailbox': 20,'##UNK##': 1}
            or
                {'Set-Mailbox': {'Set-Mailbox': 5, '##UNK##': 1}, '##UNK##': {'Set-Mailbox': 1, '##UNK##': 1}}
        unk_token: str
            dummy token to signify an unseen command (e.g. "##UNK##").
            This token should be present in the `states` keys. And if `states` is a dict of dicts, then the `unk_token`
            should be present in the keys of the outer dict and all the inner dicts.
        """
        super().__init__(states)
        assert unk_token in states
        self.states = dict(states)
        self.unk_token = unk_token
        for key, val in self.states.items():
            if isinstance(val, dict):
                self.states[key] = StateMatrix(self.states[key], unk_token)

    def __getitem__(self, item):
        if item not in self.states:
            return self.states[self.unk_token]
        else:
            return self.states[item]


class Cmd:
    def __init__(self, name: str, params: Union[set, dict]):
        """
        This class should be used to store commands which have params and optionally values

        Parameters
        ----------
        name: str
            name of the command. e.g. for Exchange online: "Set-Mailbox"
        params: Union[set, dict]
            set of accompanying params or dict of accompanying params and values.
            e.g.
                {'Identity', 'ForwardingEmailAddress'}
            of
                {'Identity': 'some identity', 'ForwardingEmailAddress': 'an_email@email.com'}
        """
        self.name = name
        self.params = params

    def __str__(self):
        rep = "Cmd(name='{}', params={})".format(self.name, self.params)
        return rep

    def __repr__(self):
        return str(self)
