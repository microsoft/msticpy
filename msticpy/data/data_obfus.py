# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Data obfuscation functions."""
import hashlib
import pkgutil
import re
import uuid
import warnings
from functools import lru_cache
from typing import Any, Callable, Dict, List, Mapping, Optional, Tuple, Union

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


# Create a random map for shuffling IP address components
ip_map: List[Dict[str, str]] = []
for _ in range(4):
    rng = np.random.default_rng()
    ip_list = [str(n) for n in np.arange(256)]
    rand_list = ip_list.copy()
    rng.shuffle(rand_list)
    ip_map.append(dict(zip(ip_list, rand_list)))


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
        return _map_ip4_address(ip_addr)
    if ":" in ip_addr:
        if ip_addr.strip() == "::1":
            # Localhost
            return ip_addr
        ip_out = []
        for part in ip_addr.split(":"):
            enc = hashlib.sha256(bytes(part, "utf-8")).hexdigest()[: len(part)]
            ip_out.append(enc)
        return ":".join(ip_out)
    return hashlib.sha256(bytes(ip_addr, "utf-8")).hexdigest()[: len(ip_addr)]


_WK_IPV4 = set(["0.0.0.0", "127.0.0.1", "255.255.255.255"])  # nosec


def _map_ip4_address(ip_addr: str) -> str:
    try:
        ip_bytes = [int(byte) for byte in ip_addr.split(".")]
    except ValueError:
        return hash_string(ip_addr)
    if ".".join(str(byte) for byte in ip_bytes) in _WK_IPV4:
        # Well-known address
        return ip_addr
    if ip_bytes[0] == 10:
        # class A res private
        ls_bytes = ".".join(
            [
                ip_map[idx].get(byte, "1")
                for idx, byte in enumerate(ip_addr.split(".")[1:])
            ]
        )
        return f"10.{ls_bytes}"
    if ip_bytes[0] == 17 and (16 <= ip_bytes[1] <= 31):
        # class B res private
        ls_bytes = ".".join(
            [
                ip_map[idx].get(byte, "1")
                for idx, byte in enumerate(ip_addr.split(".")[2:])
            ]
        )
        return f"{ip_bytes[0]}.{ip_bytes[1]}.{ls_bytes}"
    if ip_bytes[0] == 192 and ip_bytes[1] == 168:
        # class C res private
        ls_bytes = ".".join(
            [
                ip_map[idx].get(byte, "1")
                for idx, byte in enumerate(ip_addr.split(".")[2:])
            ]
        )
        return f"192.168.{ls_bytes}"
    # by default, remap all
    return ".".join(
        [ip_map[idx].get(byte, "1") for idx, byte in enumerate(ip_addr.split("."))]
    )


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


def hash_list(item_list: List[str]) -> List[Any]:
    """
    Hash list of strings.

    Parameters
    ----------
    item_list : List[str]
        Input list

    Returns
    -------
    List[Any]
        Hashed list

    """
    out_list: List[Union[Dict[str, Any], List[Any], str]] = []
    hash_val: Union[str, Dict[str, Any], List[str]]
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


_WK_ACCOUNTS = set(
    [
        "administrator",
        "guest",
        "system",
        "local service",
        "network service",
        "root",
        "crontab",
        "nt authority",
    ]
)


@lru_cache(maxsize=1024)
def hash_account(account: str) -> str:
    """
    Hash an Account to something recognizable.

    Parameters
    ----------
    account : str
        Account name (UPN, NT or simple name)

    Returns
    -------
    str
        Hashed Account

    """
    if "@" in account:
        acct_type = "UPN"
        user, domain = account.split("@")
    elif "/" in account:
        acct_type = "NT"
        domain, user = account.split("/")
    else:
        acct_type = "NO DOM"
        user, domain = account, ""

    if user.lower() not in _WK_ACCOUNTS:
        user_hash = hashlib.sha256(bytes(user, "utf-8")).digest()
        user_num = sum(user_hash[:16]) * sum(user_hash[16:]) // 199
        user = f"account-#{user_num}"
    if domain.lower() not in _WK_ACCOUNTS:
        domain = hash_item(domain, ".")

    if acct_type == "UPN":
        return f"{user}@{domain}"
    if acct_type == "NT":
        return f"{domain}/{user}"
    return user


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
    "acct": hash_account,
    "null": "null",
}


def mask_df(  # noqa: MC0001
    data: pd.DataFrame,
    column_map: Mapping[str, Any] = None,
    use_default: bool = True,
    silent: bool = True,
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
    silent: bool
        If False the function returns progress output,
        by default True.

    Returns
    -------
    pd.DataFrame
        Obfuscated dataframe.

    """
    col_map = OBFUS_COL_MAP.copy() if use_default else {}
    if column_map is not None:
        col_map.update(column_map)

    out_df = data.copy()
    if not silent:
        print("obfuscating columns:")
    for col_name in data.columns:
        if col_name not in col_map:
            continue
        col_type = col_map.get(col_name, "str")
        if not silent:
            print(col_name, end=", ")
        map_func = MAP_FUNCS.get(col_type)
        try:
            if map_func == "null":
                data[col_name] = None
            elif map_func is not None and callable(map_func):
                out_df[col_name] = out_df.apply(
                    lambda x, col=col_name, func=map_func: func(x[col]), axis=1
                )
            else:
                out_df[col_name] = out_df.apply(
                    lambda x, col=col_name, c_type=col_type: hash_item(x[col], c_type),
                    axis=1,
                )
        except Exception as err:
            print(col_name, err)
            raise

    if not silent:
        print("\ndone")
    return out_df


def check_masking(
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


# alertnative names for backward compat
obfuscate_df = mask_df
check_obfuscation = check_masking


@pd.api.extensions.register_dataframe_accessor("mp_mask")
class ObfuscationAccessor:
    """Base64 Unpack pandas extension."""

    def __init__(self, pandas_obj):
        """Initialize the extension."""
        self._df = pandas_obj

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
        warn_message = (
            "This accessor method has been deprecated.\n"
            "Please use df.mp.mask() method instead."
            "This will be removed in MSTICPy v2.2.0"
        )
        warnings.warn(warn_message, category=DeprecationWarning)
        return mask_df(data=self._df, column_map=column_map, use_default=use_default)
