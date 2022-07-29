# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Mordor/OTRF Security datasets driver."""
import json
import pickle  # nosec
import zipfile
from collections import defaultdict
from datetime import datetime
from pathlib import Path
from typing import Any, Dict, Generator, Iterable, List, Optional, Set, Tuple, Union
from zipfile import BadZipFile, ZipFile

import attr
import httpx
import pandas as pd
import yaml
from tqdm.auto import tqdm

from ..._version import VERSION
from ...common.exceptions import MsticpyUserError
from ...common.pkg_config import settings
from ...common.utility import mp_ua_header
from ..core.query_source import QuerySource
from .driver_base import DriverBase

__version__ = VERSION
__author__ = "Ian Hellen"


_MORDOR_TREE_URI = (
    "https://api.github.com/repos/OTRF/Security-Datasets/git/trees/master?recursive=1"
)

_MITRE_JSON_URL = (
    "https://raw.githubusercontent.com/mitre/cti/"
    "master/enterprise-attack/enterprise-attack.json"
)
_MTR_TAC_CAT_URI = "https://attack.mitre.org/tactics/{cat}/"
_MTR_TECH_CAT_URI = "https://attack.mitre.org/techniques/{cat}/"

MITRE_TECHNIQUES: pd.DataFrame = None
MITRE_TACTICS: pd.DataFrame = None

_MITRE_TECH_CACHE = "mitre_tech_cache.pkl"
_MITRE_TACTICS_CACHE = "mitre_tact_cache.pkl"
_MORDOR_CACHE = "mordor_cache.json"

_HTTP_TIMEOUT = DriverBase.get_http_timeout()


# pylint: disable=too-many-instance-attributes
class MordorDriver(DriverBase):
    """Mordor data driver."""

    def __init__(self, **kwargs):
        """Initialize the Mordor driver."""
        super().__init__(**kwargs)
        self.use_query_paths = False
        self.has_driver_queries = True
        self.mitre_techniques: pd.DataFrame
        self.mitre_tactics: pd.DataFrame
        self.mordor_data: Dict[str, MordorEntry]
        self.mdr_idx_tech: Dict[str, Set[str]]
        self.mdr_idx_tact: Dict[str, Set[str]]
        self._driver_queries: List[Dict[str, Any]] = []

        mdr_settings = settings.get("DataProviders", {}).get("Mordor", {})
        self.use_cached = kwargs.pop(
            "used_cached", mdr_settings.get("used_cached", True)
        )
        self.save_folder = kwargs.pop(
            "save_folder", mdr_settings.get("save_folder", ".")
        )
        self.save_folder = _resolve_cache_folder(self.save_folder)
        self.silent = kwargs.pop("silent", False)

        self._loaded = True

    # pylint: disable=global-statement

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : Optional[str]
            Connect to a data source

        """
        global MITRE_TECHNIQUES, MITRE_TACTICS
        cache_folder = self.save_folder if self.use_cached else None
        print("Retrieving Mitre data...")

        if MITRE_TECHNIQUES is None or MITRE_TACTICS is None:
            MITRE_TECHNIQUES, MITRE_TACTICS = _get_mitre_categories(
                cache_folder=cache_folder
            )
        self.mitre_techniques = MITRE_TECHNIQUES
        self.mitre_tactics = MITRE_TACTICS

        print("Retrieving Mordor data...")

        self.mordor_data = _GET_MORDOR_METADATA(cache_folder=cache_folder)
        self.mdr_idx_tech, self.mdr_idx_tact = _build_mdr_indexes(self.mordor_data)

        self._connected = True
        self.public_attribs = {
            "mitre_techniques": self.mitre_techniques,
            "mitre_tactics": self.mitre_tactics,
            "driver_queries": self.driver_queries,
            "search_queries": self.search_queries,
        }

    # pylint: enable=global-statement

    def query(
        self, query: str, query_source: QuerySource = None, **kwargs
    ) -> Union[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame of results.

        Parameters
        ----------
        query : str
            The query to execute
        query_source : QuerySource
            The query definition object

        Other Parameters
        ----------------
        kwargs :
            Are passed to the underlying provider query method,
            if supported.

        Returns
        -------
        Union[pd.DataFrame, Any]
            A DataFrame (if successfull) or
            the underlying provider result if an error.

        """
        del query_source
        if not self._connected:
            raise self._create_not_connected_err("OTRF Datasets")
        use_cached = kwargs.pop("used_cached", self.use_cached)
        save_folder = kwargs.pop("save_folder", self.save_folder)
        save_folder = _resolve_cache_folder(save_folder)
        silent = kwargs.pop("silent", self.silent)
        result_df = download_mdr_file(
            file_uri=query,
            use_cached=use_cached,
            save_folder=save_folder,
            silent=silent,
        )
        if not isinstance(result_df, pd.DataFrame) or result_df.empty:
            return "Could not convert result to a DataFrame."
        return result_df

    def query_with_results(self, query: str, **kwargs) -> Tuple[pd.DataFrame, Any]:
        """
        Execute query string and return DataFrame plus native results.

        Parameters
        ----------
        query : str
            The query to execute

        Returns
        -------
        Tuple[pd.DataFrame,Any]
            A DataFrame and native results.

        """
        result = self.query(query, **kwargs)
        if isinstance(result, pd.DataFrame):
            return result, "OK"
        return pd.DataFrame, result

    @property
    def driver_queries(self) -> Iterable[Dict[str, Any]]:
        """
        Return generator of Mordor query definitions.

        Yields
        ------
        Iterable[Dict[str, Any]]
            Iterable of Dictionaries containing query definitions.

        """
        if not self._connected:
            raise self._create_not_connected_err("OTRF Datasets")
        if not self._driver_queries:
            self._driver_queries = list(self._get_driver_queries())
        return self._driver_queries

    def _get_driver_queries(self):
        """Generate iterable of Mordor queries."""
        for mdr_item in self.mordor_data.values():
            for file_path in mdr_item.get_file_paths():
                mitre_data = mdr_item.get_attacks()
                techniques = ", ".join(
                    f"{att.technique}: {att.technique_name}" for att in mitre_data
                )

                tactics = ", ".join(
                    f"{tac[0]}: {tac[1]}"
                    for att in mitre_data
                    for tac in att.tactics_full
                )

                doc_string: List[str] = [
                    f"{mdr_item.title}",
                    "",
                    "Notes",
                    "-----",
                    f"Mordor ID: {mdr_item.id}",
                    mdr_item.description or "",
                    "",
                    f"Mitre Techniques: {techniques}",
                    f"Mitre Tactics: {tactics}",
                ]
                q_container, _, full_name = file_path["qry_path"].partition(".")
                short_name = file_path["qry_path"].split(".")[-1]
                yield {
                    "name": full_name,
                    "description": "\n".join(doc_string),
                    "query_name": short_name,
                    "query": file_path["file_path"],
                    "query_container": q_container,
                    "metadata": {},
                }

    def search_queries(self, search: str) -> Iterable[str]:
        """
        Search queries for matching attributes.

        Parameters
        ----------
        search : str
            Search string. Substrings separated by commas will
            be treated as OR terms - e.g. "a, b" == "a" or "b".
            Substrings separated by "+" will be treated as AND
            terms - e.g. "a + b" == "a" and "b"

        Returns
        -------
        Iterable[str]
            Iterable of matching query names.


        """
        if not self._connected:
            raise self._create_not_connected_err("OTRF Datasets")
        matches = []
        for mdr_id in search_mdr_data(self.mordor_data, terms=search):
            for file_path in self.mordor_data[mdr_id].get_file_paths():
                matches.append(
                    f"{file_path['qry_path']} ({self.mordor_data[mdr_id].title})"
                )
        return matches


def _resolve_cache_folder(cache_path: str):
    """Expand and optionally creates cache folder."""
    cache_folder = Path(cache_path).expanduser()
    if not cache_folder.is_dir():
        cache_folder.mkdir(parents=True, exist_ok=True)
    return str(cache_folder)


# pylint: enable=too-many-instance-attributes


class MitreAttack:
    """MitreAttack container for techniques and tactics."""

    MTR_TECH_URI = "https://attack.mitre.org/techniques/{technique_id}/"
    MTR_TAC_URI = "https://attack.mitre.org/tactics/{tactic_id}/"

    def __init__(
        self,
        attack: Dict[str, Any] = None,
        technique: str = None,
        sub_technique: str = None,
        tactics: List[str] = None,
    ):
        """
        Create instance of MitreAttack.

        Parameters
        ----------
        attack : Dict[str, Any], optional
            attack data as dictionary, by default None
        technique : str, optional
            technique ID, by default None
        sub_technique : str, optional
            sub-technique ID, by default None
        tactics : List[str], optional
            List of associated tactics, by default None

        """
        if attack is None and (technique is None and tactics is None):
            raise TypeError(
                "Either 'attack' or 'technique' and 'tactics' must be specified."
            )
        self.technique = attack.get("technique") if attack else technique
        self.sub_technique = attack.get("sub-technique") if attack else sub_technique
        self.tactics = attack.get("tactics") if attack else tactics  # type: ignore

        self._technique_name = None
        self._technique_desc = None
        self._technique_uri = None
        self._tactics_full: List[Tuple[str, str, str, str]] = []

    def __repr__(self) -> str:
        """
        Return repr of MitreAttack object.

        Returns
        -------
        str
            The repr of the object.

        """
        return "".join(
            [
                f"MitreAttack(technique={self.technique}), ",
                f"sub_technique={self.sub_technique}, ",
                f"tactics={repr(self.tactics)}",
            ]
        )

    @property
    def technique_name(self) -> Optional[str]:
        """
        Return Mitre Technique full name.

        Returns
        -------
        Optional[str]
            Name of the Mitre technique

        """
        if not self._technique_name and self.technique in MITRE_TECHNIQUES.index:
            self._technique_name = MITRE_TECHNIQUES.loc[self.technique].Name
        return self._technique_name

    @property
    def technique_desc(self) -> Optional[str]:
        """
        Return Mitre technique description.

        Returns
        -------
        Optional[str]
            Technique description

        """
        if not self._technique_desc and self.technique in MITRE_TECHNIQUES.index:
            self._technique_desc = MITRE_TECHNIQUES.loc[self.technique].Description
        return self._technique_desc

    @property
    def technique_uri(self) -> str:
        """
        Return Mitre Technique URI.

        Returns
        -------
        Optional[str]
            URI of the Mitre technique

        """
        return self.MTR_TECH_URI.format(technique_id=self.technique)

    @property
    def tactics_full(self) -> List[Tuple[str, str, str, str]]:
        """
        Return full listing of Mitre tactics.

        Returns
        -------
        List[Tuple[str, str, str, str]]
            List of tuples of:
            (ID, Name, Description, URI)

        """
        if not self._tactics_full and self.tactics:
            for tactic in self.tactics:
                tactic_name = tactic_desc = "unknown"
                if tactic in MITRE_TACTICS.index:
                    tactic_name = MITRE_TACTICS.loc[tactic].Name
                    tactic_desc = MITRE_TACTICS.loc[tactic].Description
                tactic_uri = self.MTR_TAC_URI.format(tactic_id=tactic)
                self._tactics_full.append(
                    (tactic, tactic_name, tactic_desc, tactic_uri)
                )
        return self._tactics_full


def _to_datetime(date_val) -> datetime:
    """
    Return datetime from parsed date string.

    Parameters
    ----------
    date_val : datetime
        The datetime or datetime string.

    Returns
    -------
    datetime
        Parse datetime.

    """
    if isinstance(date_val, datetime):
        return date_val
    try:
        return pd.to_datetime(date_val)
    except TypeError:
        return datetime.min


DS_PREFIX = "https://raw.githubusercontent.com/OTRF/Security-Datasets/master/datasets/"


# pylint: disable=not-an-iterable, no-member


@attr.s(auto_attribs=True)
class MordorEntry:
    """Mordor data set metadata."""

    title: str
    id: str
    type: str
    creation_date: datetime = attr.ib(converter=_to_datetime)
    modification_date: datetime = attr.ib(converter=_to_datetime)
    contributors: List[str] = attr.Factory(list)
    author: Optional[str] = None
    platform: Optional[str] = None
    description: Optional[str] = None
    tags: List[str] = attr.Factory(list)
    files: List[Dict[str, Any]] = attr.Factory(list)
    datasets: List[Dict[str, Any]] = attr.Factory(list)
    attack_mappings: List[Dict[str, Any]] = attr.Factory(list)
    notebooks: List[Dict[str, str]] = attr.Factory(list)
    simulation: Dict[str, Any] = attr.Factory(dict)
    references: List[Any] = attr.Factory(list)
    _rel_file_paths: List[Dict[str, Any]] = attr.Factory(list)

    def get_notebooks(self) -> List[Tuple[str, str, str]]:
        """
        Return the list of notebooks for the dataset.

        Returns
        -------
        List[Tuple[str, str, str]]
            Tuples of (name, project, link)

        """
        return [
            (nbk.get("name", ""), nbk.get("project", ""), nbk.get("link", ""))
            for nbk in self.notebooks
        ]

    def get_attacks(self) -> List[MitreAttack]:
        """
        Return list of Mitre attack classifications.

        Returns
        -------
        List[MitreAttack]
            List of MitreAttack definitions.

        """
        return [MitreAttack(attack=attack) for attack in self.attack_mappings]

    def get_file_paths(self) -> List[Dict[str, str]]:
        """
        Return list of data file links.

        Returns
        -------
        List[Dict[str, str]]
            list of dictionaries describing files.
            Each entry has key/values for:
            - file_type
            - file_path
            - relative_path
            - qry_path

        """
        if not self._rel_file_paths:
            for file in self.files:
                f_path = file.get("link")
                if not f_path:
                    continue
                f_rel_path = f_path.replace(DS_PREFIX, "")
                query_path = ".".join(Path(f_rel_path).parts).replace(
                    Path(f_rel_path).suffix, ""
                )
                self._rel_file_paths.append(
                    {
                        "file_type": file.get("type"),
                        "file_path": f_path,
                        "relative_path": f_rel_path,
                        "qry_path": query_path,
                    }
                )
        return self._rel_file_paths


# pylint: disable=not-an-iterable, no-member


def get_mdr_data_paths(item_type="metadata") -> Generator[str, None, None]:
    """
    Generate Mordor data sets from GitHub repo.

    Parameters
    ----------
    item_type : str, optional
        The type of item required, by default "metadata"
        Other values are "large", "small.

    Yields
    ------
    str
        Iterable of paths

    """
    md_tree = _GET_MORDOR_TREE(_MORDOR_TREE_URI)
    yield from (
        t_item["path"]
        for t_item in md_tree.get("tree")
        if t_item["type"] == "blob"
        and t_item["path"].startswith("datasets")
        and item_type in t_item["path"]
    )


def _get_mdr_github_tree():
    """Closure to wrap fetching Mordor tree from GitHub."""
    mordor_tree = None

    def _get_mdr_tree(uri):
        nonlocal mordor_tree
        if mordor_tree is None:
            resp = httpx.get(uri, timeout=_HTTP_TIMEOUT, headers=mp_ua_header())
            mordor_tree = resp.json()
        return mordor_tree

    return _get_mdr_tree


# Create closure
_GET_MORDOR_TREE = _get_mdr_github_tree()


def _get_mdr_file(gh_file):
    """Fetch a file from Mordor repo."""
    file_blob_uri = (
        f"https://raw.githubusercontent.com/OTRF/Security-Datasets/master/{gh_file}"
    )
    file_resp = httpx.get(file_blob_uri, timeout=_HTTP_TIMEOUT, headers=mp_ua_header())
    return file_resp.content


def _create_mdr_metadata_cache():
    md_metadata: Dict[str, MordorEntry] = {}

    def _get_mdr_metadata(cache_folder: Optional[str] = None):
        nonlocal md_metadata
        if not md_metadata:
            md_metadata = _fetch_mdr_metadata(cache_folder=cache_folder)
        return md_metadata

    return _get_mdr_metadata


# Create closure
_GET_MORDOR_METADATA = _create_mdr_metadata_cache()

_LAST_UPDATE_KEY = "mp_last_updated"
_DEFAULT_TS = pd.Timestamp(pd.Timestamp.utcnow() - pd.Timedelta(days=60))


# pylint: disable=global-statement
def _fetch_mdr_metadata(cache_folder: Optional[str] = None) -> Dict[str, MordorEntry]:
    """
    Return full metadata for Mordor datasets.

    Parameters
    ----------
    cache_folder : Optional[str]
        Folder to search for mordor cache, by default None

    Returns
    -------
    Dict[str, MordorEntry]:
        Mordor data set metadata keyed by MordorID

    """
    global MITRE_TECHNIQUES, MITRE_TACTICS

    if MITRE_TECHNIQUES is None or MITRE_TACTICS is None:
        MITRE_TECHNIQUES, MITRE_TACTICS = _get_mitre_categories()
    md_metadata: Dict[str, MordorEntry] = {}

    md_cached_metadata = _read_mordor_cache(cache_folder)
    mdr_md_paths = list(get_mdr_data_paths("metadata"))
    for filename in tqdm(
        mdr_md_paths, unit=" files", desc="Downloading Mordor metadata"
    ):
        cache_valid = False
        if filename in md_cached_metadata:
            metadata_doc = md_cached_metadata[filename]
            last_timestamp = pd.Timestamp(
                metadata_doc.get(_LAST_UPDATE_KEY, _DEFAULT_TS)
            )
            cache_valid = (pd.Timestamp.utcnow() - last_timestamp).days < 30

        if not cache_valid:
            gh_file_content = _get_mdr_file(filename)
            try:
                metadata_doc = yaml.safe_load(gh_file_content)
            except yaml.error.YAMLError:
                continue
            metadata_doc[_LAST_UPDATE_KEY] = pd.Timestamp.utcnow().isoformat()
            md_cached_metadata[filename] = metadata_doc
        doc_id = metadata_doc.get("id")
        mdr_entry = metadata_doc.copy()
        mdr_entry.pop(_LAST_UPDATE_KEY, None)
        md_metadata[doc_id] = MordorEntry(**mdr_entry)

    _write_mordor_cache(md_cached_metadata, cache_folder)
    return md_metadata


# pylint: enable=global-statement


def _read_mordor_cache(cache_folder) -> Dict[str, Any]:
    """Return dictionary of cached metadata if cached_folder is a valid path."""
    md_cached_metadata: Dict[str, Any] = {}
    mordor_cache = Path(cache_folder).joinpath(_MORDOR_CACHE)
    if _valid_cache(mordor_cache):
        try:
            md_json = Path(mordor_cache).read_text(encoding="utf-8")
            md_cached_metadata = json.loads(md_json)
        except json.JSONDecodeError:
            pass
    return md_cached_metadata


def _write_mordor_cache(md_cached_metadata, cache_folder):
    """Write dictionary of cached metadata if cached_folder is a valid path."""
    mordor_cache = Path(cache_folder).joinpath(_MORDOR_CACHE)
    json_text = json.dumps(md_cached_metadata, indent=4)
    Path(mordor_cache).write_text(json_text, encoding="utf-8")


def _build_mdr_indexes(
    mdr_metadata: Dict[str, MordorEntry]
) -> Tuple[Dict[str, Set[str]], Dict[str, Set[str]]]:
    """
    Return dictionaries mapping Mitre items to Mordor datasets.

    Parameters
    ----------
    mdr_metadata : Dict[str, MordorEntry]
        Dictionary of mordor dataset metadata.

    Returns
    -------
    Tuple[Dict[str, Set[str]], Dict[str, Set[str]]]
        Mordor dataset IDs keyed by Mitre Technique and Mitre Tactic.

    """
    md_idx_techniques = defaultdict(set)
    md_idx_tactics = defaultdict(set)

    for md_id, md_file in mdr_metadata.items():
        for attack in md_file.get_attacks():
            md_idx_techniques[attack.technique].add(md_id)
            if not attack.tactics:
                continue
            for tactic in attack.tactics:
                md_idx_tactics[tactic].add(md_id)

    return md_idx_techniques, md_idx_tactics  # type: ignore


def download_mdr_file(
    file_uri: str, use_cached: bool = True, save_folder: str = ".", silent: bool = False
) -> pd.DataFrame:
    """
    Download data file from Mordor.

    Parameters
    ----------
    file_uri : str
        The URI of the file to download.
    use_cached : bool, optional
        Try to use locally saved file first, by default True
    save_folder : str, optional
        Path to output folder, by default "."
    silent : bool
        If True, suppress feedback. By default, False.

    Returns
    -------
    pd.DataFrame
        DataFrame of Dataset

    """
    if not silent:
        print(file_uri)
    if not file_uri.lower().endswith("zip"):
        raise TypeError(f"File type not supported {file_uri}")
    if not Path(save_folder).is_dir():
        Path(save_folder).mkdir(parents=True, exist_ok=True)
    save_path = "-".join(Path(file_uri.replace(DS_PREFIX, "")).parts)
    save_file = Path(save_folder).joinpath(save_path)
    if not use_cached or not save_file.is_file():
        # streamed download
        with open(str(save_file), "wb") as fdesc:
            with httpx.stream(
                "GET", file_uri, timeout=_HTTP_TIMEOUT, headers=mp_ua_header()
            ) as resp:
                for chunk in resp.iter_bytes(chunk_size=1024):
                    fdesc.write(chunk)

    try:
        with zipfile.ZipFile(str(save_file)) as zip_file:
            file_names = zip_file.namelist()
            d_frames = {
                file_name: _extract_zip_file_to_df(
                    zip_file, file_name, use_cached, save_folder, silent
                )
                for file_name in file_names
            }
            return pd.concat(d_frames.values())
    except BadZipFile as bad_zip_err:
        raise MsticpyUserError(
            f"Could not extract zip file for {file_uri}.",
            "File does not exist or is corrupt.",
            title="Mordor download error",
            help_uri="https://msticpy.readthedocs.io/en/latest/data_acquisition/MordorData.html",
        ) from bad_zip_err


def _extract_zip_file_to_df(  # noqa: MC0001
    zip_file: ZipFile,
    file_name: str,
    use_cached: bool = True,
    save_folder: str = ".",
    silent: bool = False,
) -> pd.DataFrame:
    """
    Extract from zip and parse json file to DataFrame.

    Parameters
    ----------
    zip_file : ZipFile
        ZipFile object containing the file
    file_name : str
        File name to extract
    use_cached : bool, optional
        Try to use locally saved file first, by default True
    save_folder : str, optional
        Path to output folder, by default "."
    silent : bool
        If False, suppress feedback. By default, True.

    Returns
    -------
    pd.DataFrame
        Extracted DataFrame

    """
    if not silent:
        print("Extracting", file_name)

    file_path = Path(save_folder).joinpath(file_name)
    if not use_cached or not file_path.is_file():
        zip_file.extract(file_name, path=save_folder)

    out_df = pd.DataFrame()
    if file_path.suffix.lower() == ".json":
        out_df = pd.read_json(file_path, lines=True)
    if file_path.suffix.lower() == ".csv":
        out_df = pd.read_csv(file_path)
    if file_path.suffix.lower() not in (".json", ".csv"):
        print(f"Cannot process files of type {file_path.suffix.lower()}")
    if not use_cached:
        Path(file_name).unlink()
    return out_df


def search_mdr_data(
    mdr_data: Dict[str, MordorEntry], terms: str = None, subset: Iterable[str] = None
) -> Set[str]:
    """
    Return IDs for items matching terms.

    Parameters
    ----------
    mdr_data : Dict[str, MordorEntry]
        Mordor dataset
    terms : str, optional
        Search terms, by default None
        (comma-separated values are treated as OR terms
        plus-separated values are treated as AND terms)
    subset : Iterable[str], optional
        A subset of IDs over which to search, by default None

    Returns
    -------
    Set[str]
        The set of matching IDs.

    """
    if terms is None:
        return set(subset or mdr_data.keys())
    logic = "OR"
    if "," in terms:
        search_terms = terms.split(",")
    elif "+" in terms:
        search_terms = terms.split("+")
        logic = "AND"
    else:
        search_terms = [terms]
    results: Set[str] = set()
    for search_idx, term in enumerate(search_terms):
        item_results = set()
        for md_id, item in mdr_data.items():
            if subset is not None and md_id not in subset:
                continue
            if term.strip() in str(item):
                item_results.add(md_id)
        if logic == "OR":
            results = results | item_results
        else:
            # Don't AND if search_idx == 0 (and-ing against empty results)
            results = results & item_results if search_idx else item_results
    return results


def _extract_mitre_refs(ext_refs):
    ref_dict = [
        ref
        for ref in ext_refs
        if "source_name" in ref and ref["source_name"] == "mitre-attack"
    ]
    if ref_dict:
        return ref_dict[0]
    return {}


def _reshape_mitre_df(data):
    col_rename = {"external_id": "ID", "name": "Name", "description": "Description"}
    return (
        data.join(
            data.apply(
                lambda x: _extract_mitre_refs(x.external_references),
                axis=1,
                result_type="expand",
            )
        )
        .assign(MitreGroup="Enterprise")[["external_id", "name", "description", "url"]]
        .rename(columns=col_rename)
        .sort_values("ID")
        .set_index("ID")
    )


def _get_mitre_categories(
    cache_folder: Optional[str] = None,
) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Download and return Mitre techniques and tactics.

    Parameters
    ----------
    cache_folder : Optional[str]
        Folder to search for mordor cache, by default None

    Returns
    -------
    Tuple[pd.DataFrame, pd.DataFrame]
        Tuple of techniques, tactics with IDs, names,
        descriptions.

    """
    if cache_folder:
        tech_cache = Path(cache_folder).joinpath(_MITRE_TECH_CACHE)
        tactics_cache = Path(cache_folder).joinpath(_MITRE_TACTICS_CACHE)
        if _valid_cache(tech_cache) and _valid_cache(tactics_cache):
            try:
                tech_df = pd.read_pickle(tech_cache)
                tactics_df = pd.read_pickle(tactics_cache)
                return tech_df, tactics_df
            except pickle.PickleError:
                pass
    resp = httpx.get(_MITRE_JSON_URL, timeout=_HTTP_TIMEOUT, headers=mp_ua_header())
    mitre = pd.json_normalize(resp.json()["objects"])

    # remove deprecated items
    mitre["x_mitre_deprecated"].fillna(False, inplace=True)
    mitre = mitre[~(mitre["x_mitre_deprecated"])]

    tech_df = _reshape_mitre_df(mitre[mitre.type == "attack-pattern"])
    tactics_df = _reshape_mitre_df(mitre[mitre.type == "x-mitre-tactic"])
    if cache_folder:
        tech_df.to_pickle(tech_cache)
        tactics_df.to_pickle(tactics_cache)

    return tech_df, tactics_df


def _valid_cache(path: Path, expired_days=30):
    """Return True if the file exists and is younger than `expired_days`."""
    if not path.is_file():
        return False
    days_old = (datetime.now() - datetime.fromtimestamp(path.stat().st_mtime)).days
    return days_old < expired_days
