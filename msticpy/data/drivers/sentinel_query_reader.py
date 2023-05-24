# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Github Sentinel Query repo import class and helpers."""

import logging
import os
import re
import warnings
import zipfile
from datetime import datetime
from pathlib import Path
from typing import Optional

import attr
import httpx
import yaml
from attr import attrs
from tqdm.notebook import tqdm

from ..._version import VERSION

__version__ = VERSION
__author__ = "Jannie Li"

# pylint: disable=too-many-instance-attributes
# pylint: disable=consider-using-with
# pylint: disable=too-many-locals
# pylint: disable=unspecified-encoding


QUERY_METADATA_SECTION = [
    "name",
    "severity",
    "tags",
    "required_data_connectors",
    "query_frequency",
    "query_period",
    "trigger_operator",
    "trigger_threshold",
    "tactics",
    "relevant_techniques",
    "entity_mappings",
    "custom_details",
    "alert_details_override",
    "version",
    "kind",
    "folder_name",
    "source_file_name",
    "query_type",
]


QUERY_DEFAULT_PARAMETER_SECTION = {
    "add_query_items": {
        "description": "Additional query clauses",
        "type": "str",
        "default": "",
    },
    "start": {"description": "Query start time", "type": "datetime"},
    "end": {"description": "Query end time", "type": "datetime"},
}


@attrs(auto_attribs=True)
class SentinelQuery:
    """Attrs class that represents a Sentinel Query yaml file."""

    query_id: str = attr.Factory(str)
    name: str = attr.Factory(str)
    description: str = attr.Factory(str)
    severity: str = attr.Factory(str)
    query_frequency: str = attr.Factory(str)
    query_period: str = attr.Factory(str)
    trigger_operator: str = attr.Factory(str)
    trigger_threshold: str = attr.Factory(str)
    version: str = attr.Factory(str)
    kind: str = attr.Factory(str)
    folder_name: str = attr.Factory(str)
    source_file_name: str = attr.Factory(str)
    query_type: str = attr.Factory(str)
    tactics: list = attr.Factory(list)
    relevant_techniques: list = attr.Factory(list)
    query: str = attr.Factory(str)
    entity_mappings: dict = attr.Factory(dict)
    custom_details: dict = attr.Factory(dict)
    alert_details_override: dict = attr.Factory(dict)
    tags: list = attr.Factory(list)
    required_data_connectors: dict = attr.Factory(dict)


def get_sentinel_queries_from_github(
    git_url: Optional[
        str
    ] = "https://github.com/Azure/Azure-Sentinel/archive/master.zip",
    outputdir: Optional[str] = None,
) -> bool:
    r"""
    Download Microsoft Sentinel Github archive and extract detection and hunting queries.

    Parameters
    ----------
    git_url : str, optional
        URL of the GIT Repository to be downloaded, by default
        "https://github.com/Azure/Azure-Sentinel/archive/master.zip"
    outputdir : str, optional
        Provide absolute path to the output folder to save downloaded archive
        (e.g. '/usr/home' or 'C:\downloads').
        If no path provided, it will download to .msticpy dir under Azure-Sentinel directory.

    """
    if outputdir is None:
        outputdir = str(
            Path.joinpath(Path("~").expanduser(), ".msticpy", "Azure-Sentinel")
        )

    try:
        with httpx.stream("GET", git_url, follow_redirects=True) as response:  # type: ignore
            progress_bar = tqdm(
                desc="Downloading from Microsoft Sentinel Github",
                initial=0,
                unit="iB",
                unit_scale=True,
            )
            repo_zip = Path.joinpath(Path(outputdir), "Azure-Sentinel.zip")  # type: ignore
            with open(repo_zip, "wb") as file:
                for data in response.iter_bytes(chunk_size=10000):
                    progress_bar.update(len(data))
                    file.write(data)
            progress_bar.close()

            archive = zipfile.ZipFile(repo_zip, mode="r")

        # Only extract Detections and Hunting Queries Folder
        for file in archive.namelist():  # type: ignore
            if file.startswith(  # type: ignore
                (
                    "Azure-Sentinel-master/Detections/",
                    "Azure-Sentinel-master/Hunting Queries/",
                )
            ):
                archive.extract(file, path=outputdir)  # type: ignore
        print("Downloaded and Extracted Files successfully")
        return True

    except httpx.HTTPError as http_err:
        warnings.warn(f"HTTP error occurred trying to download from Github: {http_err}")
        return False


def read_yaml_files(parent_dir: str, child_dir: str) -> dict:
    """
    Create dictionary mapping query file paths with the yaml file text each contains.

    Parameters
    ----------
    parent_dir : str
        Directory storing the Hunting and Detections directories
    child_dir : str
        Either "Hunting Queries" or "Detections" or otherwise named query category

    Returns
    -------
    dict
       Dictionary mapping query file paths to corresponding yaml file text in the
       parent_dir/child_dir specified. Only identifies .yaml files.

    """
    # enumerate the files and read the yaml
    yaml_queries = list(Path(parent_dir, child_dir).rglob("*.yaml"))
    yaml_queries_list = [str(Path(q)) for q in yaml_queries]
    parsed_query_dict = {}

    for query in yaml_queries_list:
        with open(query, encoding="utf8", errors="ignore") as opened_query_file:
            parsed_query_dict[query] = opened_query_file.read()

    return parsed_query_dict


def import_sentinel_queries(yaml_files: dict, query_type: str) -> list:
    """
    Create list of SentinelQuery attr objects.

    Parameters
    ----------
    yaml_files : dict
        Dictionary mapping query file addresses to yaml file text created by read_yaml_files
    query_type : str
        Either "Hunting Queries" or "Detections" or otherwise named query category

    Returns
    -------
    list
        Returns a list of SentinelQuery attr objects from a dict of yaml files and query type given

    """
    return [
        _import_sentinel_query(yaml_path, yaml_text, query_type)
        for yaml_path, yaml_text in yaml_files.items()
    ]


def _import_sentinel_query(
    yaml_path: str, yaml_text: str, query_type: str
) -> SentinelQuery:
    """
    Create a SentinelQuery attr object for a given yaml query.

    Parameters
    ----------
    yaml_path : str
        File path to a YAML Sentinel query
    yaml_text : str
        YAML text (Sentinel query) associated with the yaml_path
    query_type : str
        Either "Hunting Queries" or "Detections" or otherwise named query category

    Returns
    -------
    SentinelQuery
        Returns an attrs object called SentinelQuery with all the YAML query information

    """
    logger = logging.getLogger(__name__)
    try:
        parsed_yaml_dict = yaml.load(yaml_text, Loader=yaml.SafeLoader)
    except yaml.YAMLError as error:
        logger.warning("Failed to parse yaml for %s", yaml_path)
        logger.warning(error)
        return SentinelQuery()

    new_query = SentinelQuery(
        name=parsed_yaml_dict.get("name"),
        query_id=parsed_yaml_dict.get("id", ""),
        description=parsed_yaml_dict.get("description", ""),
        severity=parsed_yaml_dict.get("severity", ""),
        tags=parsed_yaml_dict.get("tags_entry", []),
        required_data_connectors=parsed_yaml_dict.get("requiredDataConnectors", {}),
        query_frequency=parsed_yaml_dict.get("queryFrequency", ""),
        query_period=parsed_yaml_dict.get("queryPeriod", ""),
        trigger_operator=parsed_yaml_dict.get("triggerOperator", ""),
        trigger_threshold=parsed_yaml_dict.get("triggerThreshold", ""),
        tactics=parsed_yaml_dict.get("tactics", ""),
        relevant_techniques=parsed_yaml_dict.get("relevantTechniques", []),
        query=parsed_yaml_dict.get("query", ""),
        version=parsed_yaml_dict.get("version", ""),
        kind=parsed_yaml_dict.get("kind", ""),
        folder_name=yaml_path.replace("\\", "/").split("/")[-2],
        source_file_name=yaml_path,
        query_type=query_type,
    )
    return new_query


def _format_query_name(qname: str) -> str:
    """
    Format the inputted query name for use as a file name.

    Parameters
    ----------
    qname : str
        Name of a Sentinel Query as read from the corresponding YAML file

    Returns
    -------
    str
        Returns the inputted string with non-alphanumeric characters removed and
        spaces replaced with an underscore

    """
    del_chars_pattern = r"""[%*\*'\"\\.()[\]{}-]"""
    repl_str = re.sub(del_chars_pattern, "", qname)
    repl_str = repl_str.replace(" ", "_").replace("__", "_")
    return repl_str


def _organize_query_list_by_folder(query_list: list) -> dict:
    """
    Create a dictionary mapping each folder name with related SentinelQuery objects.

    Parameters
    ----------
    query_list : list
        List of SentinelQuery objects returned by import_sentinel_queries()

    Returns
    -------
    dict
        Returns a dictionary mapping each folder name with the SentinelQuery objects associated
        with it

    """
    queries_by_folder = {}
    for query in query_list:
        if query.folder_name == "":
            warnings.warn(f"query {query} has no folder_name")
        if query.folder_name not in queries_by_folder:
            queries_by_folder[query.folder_name] = [query]
        else:
            queries_by_folder[query.folder_name].append(query)

    return queries_by_folder


def _create_queryfile_metadata(folder_name: str) -> dict:  # type: ignore
    """
    Generate metadata section of the YAML file for the given folder_name.

    Parameters
    ----------
    folder_name : str
        Either "Hunting Queries" or "Detections" or otherwise named query category

    Returns
    -------
    dict
        Returns a generated metadata section for the YAML files in the given folder_name

    """
    dict_to_write = {"metadata": {}, "defaults": {}, "sources": {}}  # type: ignore
    dict_to_write["metadata"]["version"] = 1  # write update version functionality
    dict_to_write["metadata"]["description"] = "Sentinel Alert Queries - " + folder_name
    dict_to_write["metadata"]["data_environments"] = [
        "MSSentinel"
    ]  # write how to get this and what type this should be
    dict_to_write["metadata"]["data_families"] = folder_name
    dict_to_write["metadata"]["last_updated"] = str(datetime.now())
    return dict_to_write


def _create_yaml_source_sec(cur_query: dict) -> dict:
    """
    Create the metadata section of the YAML for the current query.

    Parameters
    ----------
    cur_query : SentinelQuery
        The current SentinelQuery attrs object that a metadata section will be generated for.

    Returns
    -------
    dict
        Returns generated metadata section of the YAML for the given individual query.

    """
    source_dict = {}
    source_dict["description"] = cur_query["description"]
    source_dict["metadata"] = {}
    source_dict["metadata"]["sentinel"] = {"query_id": cur_query["query_id"]}
    for section in QUERY_METADATA_SECTION:
        source_dict["metadata"][section] = cur_query[section]
    source_dict["metadata"]["args"] = {}
    source_dict["metadata"]["args"]["query"] = cur_query["query"]
    source_dict["metadata"]["parameters"] = {}
    return source_dict


def write_to_yaml(query_list: list, query_type: str, output_folder: str) -> bool:
    """
    Write out generated YAML files of the given query_list into the given output_folder.

    Parameters
    ----------
    query_list : list
        List of SentinelQuery attr objects generated by import_sentinel_queries()
    query_type : str
        Either "Hunting Queries" or "Detections" or otherwise named query category
    output_folder : str
        The name of the folder you want the written YAML files to be stored in

    Returns
    -------
    bool
        True if succeeded; False if an error occurred

    """
    logger = logging.getLogger(__name__)
    query_dict_by_folder = _organize_query_list_by_folder(query_list)
    all_folders = query_dict_by_folder.keys()

    for source_folder in all_folders:
        logger.info("now writing files from %s", source_folder)
        dict_to_write = _create_queryfile_metadata(source_folder)

        if query_type == "Detections":
            dict_to_write["defaults"]["parameters"] = QUERY_DEFAULT_PARAMETER_SECTION

        for cur_query in query_dict_by_folder[source_folder]:
            # skipping instances where there is no name but should probably have a better solution
            try:
                formatted_qname = _format_query_name(cur_query.name)
                dict_to_write["sources"][formatted_qname] = _create_yaml_source_sec(
                    attr.asdict(cur_query)
                )

            except TypeError as err:
                logger.warning(
                    """Query name is most likely None at %s for
                    current query %r""",
                    source_folder,
                    cur_query,
                )
                print(err)

        try:
            query_text = yaml.safe_dump(
                dict_to_write, encoding="utf-8", sort_keys=False
            )
        except yaml.YAMLError as error:
            print(error)
            return False

        try:
            def_path = Path.joinpath(Path(os.getcwd()))
            path_main = Path(def_path, output_folder)
            path_type = Path(output_folder, query_type)
            if not path_main.is_dir():
                path = Path(def_path, output_folder)
                os.mkdir(path)
            if not path_type.is_dir():
                path = Path(output_folder, query_type)
                os.mkdir(path)
            Path(output_folder, query_type, source_folder).write_text(
                query_text.decode("utf-8")
            )
        except OSError as error:
            print(error)
            return False
    logger.info("done writing files")
    return True


def download_and_write_sentinel_queries(
    query_type: str, yaml_output_folder: str, github_outputdir: Optional[str] = None
):
    """
    Download queries from GitHub and write out YAML files for the given query type.

    Parameters
    ----------
    query_type : str
        Either "Hunting Queries" or "Detections" or otherwise named query category
    yaml_output_folder : str
        Path to the folder you want the new generated YAML files to be stored in
    github_outputdir : Optional[str]
        Path to the directory you want the Github download to be stored in
    """
    print("Downloading files from GitHub")
    get_sentinel_queries_from_github(outputdir=github_outputdir)
    print("Reading yaml_files")
    if github_outputdir is None:
        github_outputdir = str(
            Path.joinpath(Path("~").expanduser(), ".msticpy", "Azure-Sentinel")
        )
    base_dir = str(Path(github_outputdir, "/Azure-Sentinel-master"))
    yaml_files = read_yaml_files(parent_dir=base_dir, child_dir=query_type)
    print("Generating a list of queries")
    query_list = import_sentinel_queries(yaml_files, query_type=query_type)
    query_list = [
        query for query in query_list if query.query_id != ""
    ]  # may need better solution for failed query definitions
    print("Writing to YAML output folder")
    write_to_yaml(query_list, query_type, yaml_output_folder)
