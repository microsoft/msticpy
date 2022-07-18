# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
HTTP Context Provider base.

HTTP implementation of a ContextProvider.
It is used to interface with HTTP API providing additional contexts.
It inherits from ContextProvider and HttpProvider

"""
import attr
from ..._version import VERSION
from .context_provider_base import ContextProvider
from .context_lookup_result import ContextLookupResult, ContextLookupStatus
from ..http_lookup import HttpLookupProvider, APILookupParams

__version__ = VERSION
__author__ = "Florian Bracq"


# Create alias for simplicity
ContextLookupParams = APILookupParams


class HttpContextProvider(HttpLookupProvider, ContextProvider):
    """HTTP Context Provider base class."""

    def lookup_observable(  # type: ignore
        self,
        observable: str,
        obs_type: str = None,
        query_type: str = None,
        **kwargs,
    ) -> ContextLookupResult:
        """
        Lookup a single item.

        Parameters
        ----------
        observable : str
            Observable value to lookup
        obs_type : str, optional
            The Type of the value to lookup, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item_value
            will be returned.

        Returns
        -------
        ContextLookupResult
            The context lookup result:
            result - Positive/Negative,
            details - Lookup Details (or status if failure),
            raw_result - Raw Response
            reference - URL of the item

        Raises
        ------
        NotImplementedError
            If attempting to use an HTTP method or authentication
            protocol that is not supported.

        Notes
        -----
        Note: this method uses memorization (lru_cache) to cache results
        for a particular observable to try avoid repeated network calls for
        the same item.

        """
        result = self.lookup_item(
            observable, item_type=obs_type, query_type=query_type, **kwargs
        )
        item_dict = attr.asdict(result)
        obs_dict = {
            key.replace("item", "observable"): value for key, value in item_dict.items()
        }
        obs_result = ContextLookupResult(**obs_dict)
        if obs_result.status == ContextLookupStatus.OK.value:
            obs_result.result, obs_result.details = self.parse_results(obs_result)

        return obs_result
