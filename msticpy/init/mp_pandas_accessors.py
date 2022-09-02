# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""MSTICPy core pandas accessor methods."""
from typing import Any, Dict, List, Mapping, Union

import pandas as pd

from .._version import VERSION
from ..context.ip_utils import get_whois_df
from ..data.data_obfus import mask_df
from ..transform.base64unpack import unpack_df
from ..transform.iocextract import IoCExtract
from ..transform.network import df_to_networkx
from ..transform.proc_tree_builder import ProcSchema, build_process_tree

__version__ = VERSION
__author__ = "Ian Hellen"


@pd.api.extensions.register_dataframe_accessor("mp")
class MsticpyCoreAccessor:
    """Msticpy pandas accessor for core functions."""

    def __init__(self, pandas_obj):
        """Initialize the extension."""
        self._df = pandas_obj
        self._ioc = IoCExtract()

    def b64extract(self, column: str, **kwargs) -> pd.DataFrame:
        """
        Base64-decode strings taken from a pandas dataframe.

        Parameters
        ----------
        data : pd.DataFrame
            dataframe containing column to decode
        column : str
            Name of dataframe text column
        trace : bool, optional
            Show additional status (the default is None)
        utf16 : bool, optional
            Attempt to decode UTF16 byte strings

        Returns
        -------
        pd.DataFrame
            Decoded string and additional metadata in dataframe

        Notes
        -----
        Items that decode to utf-8 or utf-16 strings will be returned as decoded
        strings replaced in the original string. If the encoded string is a
        known binary type it will identify the file type and return the hashes
        of the file. If any binary types are known archives (zip, tar, gzip) it
        will unpack the contents of the archive.
        For any binary it will return the decoded file as a byte array, and as a
        printable list of byte values.

        The columns of the output DataFrame are:

        - decoded string: this is the input string with any decoded sections
          replaced by the results of the decoding
        - reference : this is an index that matches an index number in the
          decoded string (e.g. <<encoded binary type=pdf index=1.2').
        - original_string : the string prior to decoding - file_type : the type
          of file if this could be determined
        - file_hashes : a dictionary of hashes (the md5, sha1 and sha256 hashes
          are broken out into separate columns)
        - input_bytes : the binary image as a byte array
        - decoded_string : printable form of the decoded string (either string
          or list of hex byte values)
        - encoding_type : utf-8, utf-16 or binary
        - md5, sha1, sha256 : the respective hashes of the binary file_type,
          file_hashes, input_bytes, md5, sha1, sha256 will be null if this item is
          decoded to a string
        - src_index - the index of the source row in the input
          frame.

        """
        return unpack_df(data=self._df, column=column, **kwargs)

    def ioc_extract(self, columns: List[str], **kwargs) -> pd.DataFrame:
        """
        Extract IoCs from either a pandas DataFrame.

        Parameters
        ----------
        columns : list
            The list of columns to use as source strings,

        Other Parameters
        ----------------
        ioc_types : list, optional
            Restrict matching to just specified types.
            (default is all types)
        include_paths : bool, optional
            Whether to include path matches (which can be noisy)
            (the default is false - excludes 'windows_path'
            and 'linux_path'). If `ioc_types` is specified
            this parameter is ignored.

        Returns
        -------
        pd.DataFrame
            DataFrame of observables

        Notes
        -----
        Extract takes a pandas DataFrame as input.
        The results will be returned as a new
        DataFrame with the following columns:
        - IoCType: the mnemonic used to distinguish different IoC Types
        - Observable: the actual value of the observable
        - SourceIndex: the index of the row in the input DataFrame from
        which the source for the IoC observable was extracted.

        IoCType Pattern selection
        The default list is:  ['ipv4', 'ipv6', 'dns', 'url',
        'md5_hash', 'sha1_hash', 'sha256_hash'] plus any
        user-defined types.
        'windows_path', 'linux_path' are excluded unless `include_paths`
        is True or explicitly included in `ioc_paths`.

        """
        return self._ioc.extract_df(data=self._df, columns=columns, **kwargs)

    def build_process_tree(
        self,
        schema: Union[ProcSchema, Dict[str, Any]] = None,
        show_summary: bool = False,
        debug: bool = False,
    ) -> pd.DataFrame:
        """
        Build process trees from the process events.

        Parameters
        ----------
        schema : Union[ProcSchema, Dict[str, Any]], optional
            The column schema to use, by default None.
            If supplied as a dict it must include definitions for the
            required fields in the ProcSchema class
            If None, then the schema is inferred
        show_summary : bool
            Shows summary of the built tree, default is False.
        debug : bool
            If True produces extra debugging output,
            by default False

        Returns
        -------
        pd.DataFrame
            Process tree dataframe.

        See Also
        --------
        ProcSchema

        """
        return build_process_tree(
            procs=self._df, schema=schema, show_summary=show_summary, debug=debug
        )

    def to_graph(self, **kwargs):
        """
        Create a networkx graph from a DataFrame.

        Parameters
        ----------
        source_col : str
            Column for source nodes.
        target_col : str
            Column for target nodes.
        source_attrs : Optional[List[str]], optional
            Optional list of columns to use as source node attributes, by default None
        target_attrs : Optional[List[str]], optional
            Optional list of columns to use as target node attributes, by default None
        edge_attrs : Optional[List[str]], optional
            Optional list of columns to use as edge node attributes, by default None
        graph_type : str
            "graph" or "digraph" (for nx.DiGraph)

        Returns
        -------
        nx.Graph
            The networkx graph object

        """
        return df_to_networkx(self._df, **kwargs)

    def mask(
        self, column_map: Mapping[str, Any] = None, use_default: bool = True
    ) -> pd.DataFrame:
        """
        Obfuscate the data in columns of a pandas dataframe.

        Parameters
        ----------
        data : pd.DataFrame
            dataframe containing column to obfuscate
        column_map : Mapping[str, Any], optional
            Custom column mapping, by default None
        use_default: bool
            If True use the built-in map (adding any custom
            mappings to this dictionary)

        Returns
        -------
        pd.DataFrame
            Obfuscated dataframe

        """
        return mask_df(data=self._df, column_map=column_map, use_default=use_default)

    def whois(self, ip_column, **kwargs):
        """
        Extract IoCs from either a pandas DataFrame.

        Parameters
        ----------
        ip_column : str
            Column name of IP Address to look up.

        Other Parameters
        ----------------
        asn_col : str, optional
            Name of the output column for ASN description,
            by default "ASNDescription"
        ip_column : str, optional
            Name of the output column for full whois data,
            by default "WhoIsData"
        show_progress : bool, optional
            Show progress for each query, by default False

        Returns
        -------
        pd.DataFrame
            Output DataFrame with results in added columns.

        """
        return get_whois_df(data=self._df, ip_column=ip_column, **kwargs)
