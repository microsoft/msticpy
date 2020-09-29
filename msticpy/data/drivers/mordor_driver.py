# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""."""
import io
import json
import re
import zipfile
from collections import defaultdict
from datetime import datetime
from json import JSONDecodeError
from pathlib import Path
from typing import Any, Dict, Generator, Iterable, List, Optional, Set, Tuple, Union
from zipfile import ZipFile

import attr
import pandas as pd
import requests
import yaml
from tqdm.auto import tqdm

from ..._version import VERSION
from ..query_source import QuerySource
from .driver_base import DriverBase

__version__ = VERSION
__author__ = "Ian Hellen"


_MORDOR_TREE_URI = (
    "https://api.github.com/repos/OTRF/mordor/git/trees/master?recursive=1"
)

_MTR_TAC_CAT_URI = "https://attack.mitre.org/tactics/{cat}/"
_MTR_TECH_CAT_URI = "https://attack.mitre.org/techniques/{cat}/"

MITRE_TECHNIQUES: pd.DataFrame = None
MITRE_TACTICS: pd.DataFrame = None


class MordorDriver(DriverBase):
    """Mordor data driver."""

    # pylint: disable=global-statement
    def __init__(self, **kwargs):
        """Initialize the Morder driver."""
        global MITRE_TECHNIQUES, MITRE_TACTICS
        super().__init__(**kwargs)

        self.mitre_techniques = get_mitre_categories(_MTR_TECH_CAT_URI)
        self.mitre_tactics = get_mitre_categories(_MTR_TAC_CAT_URI)
        if MITRE_TECHNIQUES is None:
            MITRE_TECHNIQUES = self.mitre_techniques
        if MITRE_TACTICS is None:
            MITRE_TACTICS = self.mitre_tactics

        self.public_attribs = {
            "mitre_techniques": self.mitre_techniques,
            "mitre_tactics": self.mitre_tactics,
            "driver_queries": self.driver_queries,
        }

        self.mordor_data = get_mdr_metadata()
        self.mdr_idx_tech, self.mdr_idx_tact = build_mdr_indexes(self.mordor_data)

        self._loaded = True
        self._connected = True

    # pylint: enable=global-statement

    def connect(self, connection_str: Optional[str] = None, **kwargs):
        """
        Connect to data source.

        Parameters
        ----------
        connection_str : Optional[str]
            Connect to a data source

        """

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
        result_dfs = download_mdr_file(file_uri=query, use_cached=True)
        return pd.concat(result_dfs.values())

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

    @property
    def driver_queries(self) -> List[Dict[str, str]]:
        """
        Return list of Mordor query definitions.

        Returns
        -------
        List[Dict[str, str]]
            List of query definitions

        """
        return list(self._get_driver_queries())

    def _get_driver_queries(self) -> Generator[Dict[str, Union[str, Dict]], None, None]:
        """
        Return generator of Mordor query definitions.

        Yields
        ------
        Generator[Dict[str, str], None, None]
            Iterable of Dictionaries containing query definition

        """
        for mdr_item in self.mordor_data.values():
            for file_path in mdr_item.get_file_paths():
                mitre_data = mdr_item.get_attacks()
                techniques = ", ".join([att.technique for att in mitre_data])
                tactics = ", ".join({tac for tactics in mitre_data for tac in tactics})
                doc_string = [
                    f"{mdr_item.title}",
                    "",
                    "Notes",
                    "-----",
                    f"Mordor ID: {mdr_item.id}",
                    mdr_item.description,
                    "",
                    f"Mitre Techniques: {techniques}",
                    f"Mitre Tactics: {tactics}",
                ]
                q_container = file_path["qry_path"].split(".")[:-1]
                q_name = file_path["qry_path"].split(".")[-1]
                yield {
                    "query_name": q_name,
                    "query_text": file_path["file_path"],
                    "query_container": q_container,
                    "doc_string": "\n".join(doc_string),
                    "metadata": {},
                }


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
        if attack is None or (technique is None and tactics is None):
            raise TypeError(
                "Either 'attack' or 'technique' and 'tactics' must be specified."
            )
        self.technique = attack.get("technique") if attack else technique
        self.sub_technique = attack.get("sub-technique") if attack else sub_technique
        self.tactics: List[str] = attack.get(
            "tactics"
        ) if attack else tactics  # type: ignore

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
        if not self._tactics_full:
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


DS_PREFIX = "https://raw.githubusercontent.com/OTRF/mordor/master/datasets/"


# pylint: disable=not-an-iterable, no-member


@attr.s(auto_attribs=True)
class MordorEntry:
    """Mordor data set metadata."""

    title: str
    id: str
    author: str
    creation_date: datetime = attr.ib(converter=_to_datetime)
    modification_date: datetime = attr.ib(converter=_to_datetime)
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
            for file in [*self.files, *self.datasets]:
                f_path = file.get("link")
                if not f_path:
                    continue
                f_rel_path = f_path.replace(DS_PREFIX, "")
                self._rel_file_paths.append(
                    {
                        "file_type": file.get("type"),
                        "file_path": f_path,
                        "relative_path": f_rel_path,
                        "qry_path": ".".join(Path(f_rel_path).parts),
                    }
                )
        return self._rel_file_paths


# pylint: disable=not-an-iterable, no-member


def get_mdr_data_paths(item_type="metadata"):
    """
    Generate Mordor data sets from GitHub repo.

    Parameters
    ----------
    item_type : str, optional
        The type of item required, by default "metadata"
        Other values are "large", "small.

    Yields
    ------
    [type]
        [description]
    """
    md_tree = _GET_MORDOR_TREE(_MORDOR_TREE_URI)
    prefix = f"datasets/{item_type}"
    yield from (
        t_item["path"]
        for t_item in md_tree.get("tree")
        if t_item["type"] == "blob" and t_item["path"].startswith(prefix)
    )


def get_mdr_github_tree():
    """Closure to wrap fetching Mordor tree from GitHub."""
    mordor_tree = None

    def _get_mdr_tree(uri):
        nonlocal mordor_tree
        if mordor_tree is None:
            resp = requests.get(uri)
            mordor_tree = resp.json()
        return mordor_tree

    return _get_mdr_tree


# Create closure
_GET_MORDOR_TREE = get_mdr_github_tree()


def get_mdr_file(gh_file):
    """Fetch a file from Mordor repo."""
    file_blob_uri = f"https://raw.githubusercontent.com/OTRF/mordor/master/{gh_file}"
    file_resp = requests.get(file_blob_uri)
    return file_resp.content


# pylint: disable=global-statement
def get_mdr_metadata() -> Dict[str, MordorEntry]:
    """
    Return full metadata for Mordor datasets.

    Returns
    -------
    Dict[str, MordorEntry]:
        Mordor data set metadata keyed by MordorID

    """
    global MITRE_TECHNIQUES, MITRE_TACTICS

    if MITRE_TECHNIQUES is None:
        MITRE_TECHNIQUES = get_mitre_categories(_MTR_TECH_CAT_URI)
    if MITRE_TACTICS is None:
        MITRE_TACTICS = get_mitre_categories(_MTR_TAC_CAT_URI)

    md_metadata: Dict[str, MordorEntry] = {}
    for y_file in tqdm(
        get_mdr_data_paths("metadata"), unit=" files", desc="Downloading Mordor files"
    ):
        gh_file_content = get_mdr_file(y_file)
        yaml_doc = yaml.safe_load(gh_file_content)
        doc_id = yaml_doc.get("id")
        md_metadata[doc_id] = MordorEntry(**yaml_doc)
    return md_metadata


# pylint: enable=global-statement


def build_mdr_indexes(
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
            for tactic in attack.tactics:
                md_idx_tactics[tactic].add(md_id)

    return md_idx_techniques, md_idx_tactics  # type: ignore


def download_mdr_file(
    file_uri: str, use_cached: bool = True, save_folder: str = "."
) -> Dict[str, pd.DataFrame]:
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

    Returns
    -------
    Dict[str, pd.DataFrame]
        Dictionary of DataFrames

    """
    print(file_uri)
    if file_uri.lower().endswith("zip"):
        raise TypeError(f"File type not supported {file_uri}")
    save_path = file_uri.split("/")[-1]
    save_file = Path(save_folder).joinpath(save_path)
    if not use_cached or not save_file.is_file():
        # streamed download
        resp = requests.get(file_uri, stream=True)
        with open(str(save_file), "wb") as fdesc:
            for chunk in resp.iter_content(chunk_size=1024):
                fdesc.write(chunk)

    zip_file = zipfile.ZipFile(str(save_file))
    file_names = zip_file.namelist()
    d_frames = {}
    for file_name in file_names:
        d_frames[file_name] = extract_zip_file_to_df(
            zip_file, file_name, use_cached, save_folder
        )

    if len(d_frames) == 1:
        return {name: df for name, df in next(iter(d_frames.items()))}
    return d_frames


def extract_zip_file_to_df(
    zip_file: ZipFile, file_name: str, use_cached: bool = True, save_folder: str = "."
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

    Returns
    -------
    pd.DataFrame
        Extracted DataFrame

    """
    print("Extracting", file_name)

    file_path = Path(save_folder).joinpath(file_name)
    if not use_cached or not file_path.is_file():
        zip_file.extract(file_name, path=save_folder)

    out_df = pd.DataFrame()
    if file_path.suffix == ".json":
        errs = []
        with open(str(file_path), "r") as j_file:
            j_text = j_file.read()
            df_list = []
            for line_num, line in tqdm(enumerate(j_text.split("\n")), "lines"):
                if not line:
                    continue
                try:
                    df_list.append(json.loads(line))
                except JSONDecodeError:
                    errs.append(f"Could not parse #{line_num}: '{line}'")
            out_df = pd.DataFrame(df_list)
        print(f"{len(errs)} errors detected", errs)
    if Path(file_name).suffix == ".csv":
        out_df = pd.read_csv(file_name)

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
        return subset or mdr_data.keys()
    logic = "OR"
    if "," in terms:
        terms = terms.split(",")
    elif "+" in terms:
        terms = terms.split("+")
        logic = "AND"
    if not isinstance(terms, list):
        terms = [terms]
    results = set()
    for search_idx, term in enumerate(terms):
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


def get_mitre_categories(uri_template: str) -> pd.DataFrame:
    """
    Download and return Mitre techniques and tactics.

    Parameters
    ----------
    uri_template : str
        URI to fetch MITRE category from.

    Returns
    -------
    Tuple[pd.DataFrame, pd.DataFrame]
        Tuple of techniques, tactics with IDs, names,
        descriptions.

    """
    categories = {"pre": "PreAttack", "enterprise": "Enterprise", "mobile": "Mobile"}

    mitre_data = pd.concat(
        [
            pd.read_html(uri_template.format(cat=cat))[0].assign(MitreGroup=cat_title)
            for cat, cat_title in categories.items()
        ]
    ).set_index("ID")
    if "ID.1" in mitre_data.columns:
        mitre_data.drop(columns=["ID.1"])

    return mitre_data
