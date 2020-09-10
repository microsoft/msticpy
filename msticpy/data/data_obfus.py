# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data obfuscation functions."""
import hashlib
import re
import uuid
import warnings
from functools import lru_cache
import pkgutil
from typing import Any, Callable, Dict, List, Mapping, Union, Optional, Tuple

import numpy as np
import pandas as pd
import yaml

OBFUS_COL_MAP: Dict[str, str] = {}
_MAP_FILE = "resources/obfuscation_cols.yaml"
_obfus_map_file = pkgutil.get_data("msticpy", _MAP_FILE)
if not _obfus_map_file:
    warnings.warn(f"Could not find obfuscation column map {_MAP_FILE}")
else:
    _obfus_dicts = yaml.safe_load(_obfus_map_file)
    for data_col_map in _obfus_dicts.values():
        OBFUS_COL_MAP.update(data_col_map)

# SHA256 instance for hashing
sha_func = hashlib.sha256()

# Create a random map for shuffling IP address components
rng = np.random.default_rng()
ip_list = [str(n) for n in np.arange(256)]
rand_list = ip_list.copy()
rng.shuffle(rand_list)
ip_map = dict(zip(ip_list, rand_list))


def hash_string(input_str: str) -> str:
    """
    Hash a simple string.

    Parameters
    ----------
    input_str : str
        The input string

    Returns
    -------
    str
        The obfuscated output string

    """
    if not input_str:
        return input_str
    if not isinstance(input_str, str):
        input_str = str(input_str)
    str_dig = hashlib.sha256(bytes(input_str, "utf-8")).hexdigest()
    factor = int(len(input_str) / len(str_dig))
    out_str = (str_dig * max(factor, 1))[: len(input_str)]
    if not input_str.isnumeric():
        return "".join([chr(int(c) + 105) if c.isdigit() else c for c in out_str])
    # Convert entirely numeric strings to numbers
    return "".join([c if c.isdigit() else str(ord(c) - 97) for c in out_str])


@lru_cache(maxsize=1024)
def hash_item(input_item: str, delim: str = None) -> str:
    """
    Hash a simple string.

    Parameters
    ----------
    input_item : str
        The input string
    delim: str, optional
        A string of delimiters to use to split the input string
        prior to hashing.

    Returns
    -------
    str
        The obfuscated output string

    """
    if not input_item or not isinstance(input_item, str):
        return input_item
    if not delim:
        return hash_string(input_item)
    if len(delim) == 1:
        return delim.join([hash_string(elem) for elem in input_item.split(delim)])

    out_str = input_item
    delim_char = delim[0]
    delim_rest = delim[1:]
    for str_item in out_str.split(delim_char):
        out_str = out_str.replace(str_item, hash_item(str_item, delim_rest))
    return out_str


@lru_cache(maxsize=1024)
def _hash_ip_item(ip_addr: str) -> str:
    """
    Hash IP address.

    Parameters
    ----------
    ip_addr : str
        IP address string

    Returns
    -------
    str
        Hashed IP Address.

    """
    if not ip_addr or not isinstance(ip_addr, str):
        return ip_addr
    if "." in ip_addr:
        return ".".join([ip_map.get(byte, "1") for byte in ip_addr.split(".")])
    if ":" in ip_addr:
        ip_out = []
        for part in ip_addr.split(":"):
            enc = hashlib.sha256(bytes(part, "utf-8")).hexdigest()[: len(part)]
            ip_out.append(enc)
        return ":".join(ip_out)
    return hashlib.sha256(bytes(ip_addr, "utf-8")).hexdigest()[: len(ip_addr)]


def hash_ip(input_item: Union[List[str], str]) -> Union[List[str], str]:
    """
    Hash IP address or list of IP addresses.

    Parameters
    ----------
    input_item : Union[List[str], str]
        List of IP addresses or single IP address.

    Returns
    -------
    Union[List[str], str]
        List of hashed addresses or single address.
        (depending on input)

    """
    if not input_item:
        return input_item
    if isinstance(input_item, list):
        return [_hash_ip_item(elem) for elem in input_item]
    return _hash_ip_item(input_item)


def hash_list(item_list: List[str]) -> List[str]:
    """
    Hash list of strings.

    Parameters
    ----------
    item_list : List[str]
        Input list

    Returns
    -------
    List[str]
        Hashed list

    """
    out_list = []
    for val in item_list:
        if isinstance(val, dict):
            hash_val = hash_dict(val)
        elif isinstance(val, list):
            hash_val = hash_list(val)
        else:
            hash_val = hash_string(val)
        out_list.append(hash_val)
    return out_list


def hash_dict(
    item_dict: Dict[str, Union[Dict[str, Any], List[Any], str]]
) -> Dict[str, Any]:
    """
    Hash dictionary values.

    Parameters
    ----------
    item_dict : Dict[str, Union[Dict[str, Any], List[Any], str]]
        Input item can be a Dict of strings, lists or other
        dictionaries.

    Returns
    -------
    Dict[str, Any]
        Dictionary with hashed values.

    """
    out_dict = {}
    for key, val in item_dict.items():
        if isinstance(val, dict):
            hash_val = hash_dict(val)
        elif isinstance(val, list):
            hash_val = hash_list(val)  # type: ignore
        else:
            hash_val = hash_string(val)  # type: ignore
        out_dict[key] = hash_val
    return out_dict


WK_SID_PATTERN = re.compile(r"S(-\d+){3}$")
SID_PATTERN = re.compile(r"(S(?:-\d+){3})((?:-\d+){3})(-\d+)$")


@lru_cache(maxsize=1024)
def hash_sid(sid: str) -> str:
    """
    Hash a SID preserving well-known SIDs and the RID.

    Parameters
    ----------
    sid : str
        SID string

    Returns
    -------
    str
        Hashed SID

    """
    if re.match(WK_SID_PATTERN, sid):
        return sid
    usr_sid = re.match(SID_PATTERN, sid)
    if usr_sid:
        return (
            f"{usr_sid.groups()[0]}{hash_item(usr_sid.groups()[1], delim='-')}"
            + f"{usr_sid.groups()[2]}"
        )
    return sid


def _guid_replacer() -> Callable[[str], str]:
    """
    Closure for replace_guid.

    Returns
    -------
    Callable[[str], str]
        replace_guid function

    """
    guid_map: Dict[str, str] = {}

    def _replace_guid(guid: str) -> str:
        """
        Replace GUID/UUID with mapped random UUID.

        Parameters
        ----------
        guid : str
            Input UUID.

        Returns
        -------
        str
            Mapped UUID

        """
        if not guid or not isinstance(guid, str):
            return guid

        if guid in guid_map:
            return guid_map[guid]
        new_guid = str(uuid.uuid4())
        guid_map[guid] = new_guid
        return new_guid

    return _replace_guid


replace_guid = _guid_replacer()


# DataFrame obfuscation functions

# Map codes to functions
MAP_FUNCS: Dict[str, Union[str, Callable]] = {
    "uuid": replace_guid,
    "ip": hash_ip,
    "str": hash_string,
    "dict": hash_dict,
    "list": hash_list,
    "sid": hash_sid,
    "null": "null",
}


def obfuscate_df(
    data: pd.DataFrame, column_map: Mapping[str, Any] = None, use_default: bool = True
) -> pd.DataFrame:
    """
    Obfuscate columns of a DataFrame.

    Parameters
    ----------
    data : pd.DataFrame
        Input dataframe
    column_map : Mapping[str, Any], optional
        Custom column mapping, by default None
    use_default: bool
        If True use the built-in map (adding any custom
        mappings to this dictionary)

    Returns
    -------
    pd.DataFrame
        Obfuscated dataframe.

    """
    col_map = OBFUS_COL_MAP if use_default else {}
    if column_map is not None:
        col_map.update(column_map)

    out_df = data.copy()
    print("obfuscating columns:")
    for col in data.columns:
        if col not in col_map:
            continue
        col_type = col_map.get(col, "str")
        print(col, end=", ")
        map_func = MAP_FUNCS.get(col_type)
        try:
            if map_func == "null":
                data[col] = None
# pylint: disable=cell-var-from-loop
            elif map_func is not None and callable(map_func):
                out_df[col] = out_df.apply(lambda x: map_func(x[col]), axis=1)
            else:
                out_df[col] = out_df.apply(
                    lambda x: hash_item(x[col], col_type), axis=1
                )
# pylint: enable=cell-var-from-loop
        except Exception as err:
            print(col, str(err))
            raise

    print("\ndone")
    return out_df


def check_obfuscation(
    data: pd.DataFrame, orig_data: pd.DataFrame, index: int = 0, silent=True
) -> Optional[Tuple[List[str], List[str]]]:
    """
    Check the obfuscation results for a row.

    Parameters
    ----------
    data : pd.DataFrame
        Obfuscated DataFrame
    orig_data : pd.DataFrame
        Original DataFrame
    index : int, optional
        The row to check, by default 0
    silent: bool
        If False the function returns no output and
        returns lists of changed and unchanged columns.
        By default, True

    Returns
    -------
    Optional[Tuple[List[str], List[str]]] :
        If silent is True returns a tuple of unchanged, changed
        items. If False, returns None.

    """
    unchanged = []
    obfuscated = []
    for col in sorted(data.columns):
        if data.iloc[index][col] == orig_data.iloc[index][col]:
            unchanged.append(f"{col}: {data.iloc[index][col]}")
        else:
            obfuscated.append(
                f"{col}:   {orig_data.iloc[index][col]} ----> {data.iloc[index][col]}"
            )
    if not silent:
        print("===== Start Check ====")
        print("Unchanged columns:")
        print("------------------")
        print("\n".join(unchanged))
        print("\nObfuscated columns:")
        print("--------------------")
        print("\n".join(obfuscated))
        print("====== End Check =====")
        return None

    return unchanged, obfuscated


@pd.api.extensions.register_dataframe_accessor("mp_obf")
class ObfuscationAccessor:
    """Base64 Unpack pandas extension."""

    def __init__(self, pandas_obj):
        """Initialize the extension."""
        self._df = pandas_obj

    def obfuscate(
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
        return obfuscate_df(
            data=self._df, column_map=column_map, use_default=use_default
        )
