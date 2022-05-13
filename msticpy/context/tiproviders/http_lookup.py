# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
HTTP TI Provider base.

Input can be a single IoC observable or a pandas DataFrame containing
multiple observables. Processing may require a an API key and
processing performance may be limited to a specific number of
requests per minute for the account type that you have.

"""
import abc
from typing import Dict, Iterable, List, Union

import attr
import pandas as pd
from attr import Factory

from ..._version import VERSION
from .http_provider import HttpTIProvider
from .ti_provider_base import LookupResult

__version__ = VERSION
__author__ = "Ian Hellen"


# pylint: disable=too-few-public-methods
@attr.s(auto_attribs=True)
class APILookupParams:
    """IoC HTTP Lookup Params definition."""

    path: str = ""
    verb: str = "GET"
    full_url: bool = False
    headers: Dict[str, str] = Factory(dict)
    params: Dict[str, str] = Factory(dict)
    data: Dict[str, str] = Factory(dict)
    auth_type: str = ""
    auth_str: List[str] = Factory(list)
    sub_type: str = ""


class HttpLookupProvider(HttpTIProvider, abc.ABC):
    """
    HTTP Generic lookup provider base class.

    For subclasses:

    Define Base URL of the service

    ``_BASE_URL = "https://my.api.org/"``

    Define query parameters for different item types (keys)

    ``_API_QUERIES: Dict[str, APILookupParams] = {}``

    E.g.
    .. code:: python

        _IOC_QUERIES = {
        # Community API
        "ipv4": IoCLookupParams(
            path="/v3/community/{observable}",
            headers={"key": "{API_KEY}"},
        ),
        # Enterprise API Quick Lookup
        "ipv4-quick": IoCLookupParams(
            ...


    Define list of required __init__ params
    _REQUIRED_PARAMS: List[str] = []

    .. code:: python

        _REQUIRED_PARAMS = ["API_KEY"]

    In __init__

    Be sure to call ``super().__init__(**kwargs)``

    Supply any additional checkers/preprocessors
    with
    ```
    self._preprocessors.add_check(type, check_func)

    See Also
    --------
    PreProcessor
    HttpTIProvider

    """

    # Base URL of the service
    _BASE_URL = ""

    # Define query parameters for different item types (keys)
    _API_QUERIES: Dict[str, APILookupParams] = {}

    # List of required __init__ params
    _REQUIRED_PARAMS: List[str] = []

    def __init__(self, **kwargs):
        """Initialize the class."""
        super().__init__(**kwargs)
        self.__class__._IOC_QUERIES = self._API_QUERIES

        # In __init__ you might want to
        # supply additional checkers/preprocessors
        # with
        # self._preprocessors.add_check(type, check_func)
        # or replace the default PreProcessors
        # self._preprocessors = MyPreProcessor()

    def lookup_item(  # type: ignore
        self, value: str, item_type: str = None, query_type: str = None, **kwargs
    ) -> LookupResult:
        """
        Lookup a single item.

        Parameters
        ----------
        value : str
            Item value to lookup
        item_type : str, optional
            The Type of the value to lookup, by default None (type will be inferred)
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the item_value
            will be returned.

        Returns
        -------
        LookupResult
            The lookup result:
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
        Note: this method uses memoization (lru_cache) to cache results
        for a particular observable to try avoid repeated network calls for
        the same item.

        """
        return self.lookup_ioc(
            ioc=value, ioc_type=item_type, query_type=query_type, **kwargs
        )

    def lookup_items(
        self,
        data: Union[pd.DataFrame, Dict[str, str], Iterable[str]],
        obs_col: str = None,
        item_type_col: str = None,
        query_type: str = None,
        **kwargs,
    ) -> pd.DataFrame:
        """
        Lookup collection of IoC observables.

        Parameters
        ----------
        data : Union[pd.DataFrame, Dict[str, str], Iterable[str]]
            Data input in one of three formats:
            1. Pandas dataframe (you must supply the column name in
            `obs_col` parameter)
            2. Dict of observable, IoCType
            3. Iterable of observables - IoCTypes will be inferred
        obs_col : str, optional
            DataFrame column to use for observables, by default None
        item_type_col : str, optional
            DataFrame column to use for IoCTypes, by default None
        query_type : str, optional
            Specify the data subtype to be queried, by default None.
            If not specified the default record type for the IoC type
            will be returned.

        Returns
        -------
        pd.DataFrame
            DataFrame of results.

        """
        return self.lookup_iocs(
            data=data,
            obs_col=obs_col,
            ioc_type_col=item_type_col,
            query_type=query_type,
            **kwargs,
        )
