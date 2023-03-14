# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Github Sentinel Query repo import class and helpers."""

import glob
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
from attr import attrib, attrs
from tqdm.notebook import tqdm


@attrs
class SentinelQuery:
    """Attrs class that represents a Sentinel Query yaml file."""

    # pylint: disable=too-many-instance-attributes

    query_id: str = attrib(factory=str)
    name: str = attrib(factory=str)
    description: str = attrib(factory=str)
    severity: str = attrib(factory=str)
    tags: dict = attrib(factory=list)
    required_data_connectors: dict = attrib(factory=dict)
    query_frequency: str = attrib(factory=str)
    query_period: str = attrib(factory=str)
    trigger_operator: str = attrib(factory=str)
    trigger_threshold: str = attrib(factory=str)
    tactics: list = attrib(factory=list)
    relevant_techniques: list = attrib(factory=list)
    query: str = attrib(factory=str)
    entity_mappings: dict = attrib(factory=dict)
    custom_details: dict = attrib(factory=dict)
    alert_details_override: dict = attrib(factory=dict)
    version: str = attrib(factory=str)
    kind: str = attrib(factory=str)
    folder_name: str = attrib(factory=str)
    source_file_name: str = attrib(factory=str)
    query_type: str = attrib(factory=str)


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
        outputdir = Path.joinpath(Path("~").expanduser(), ".msticpy", "Azure-Sentinel")

    try:
        with httpx.stream("GET", git_url, follow_redirects=True) as response:
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
        for file in archive.namelist():
            if file.startswith(
                (
                    "Azure-Sentinel-master/Detections/",
                    "Azure-Sentinel-master/Hunting Queries/",
                )
            ):
                archive.extract(file, path=outputdir)
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
    yaml_queries = glob.glob(f"{parent_dir}/{child_dir}/**/*.yaml", recursive=True)
    yaml_queries = [str(Path(q)) for q in yaml_queries]
    parsed_query_dict = {}

    for query in yaml_queries:
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
    try:
        parsed_yaml_dict = yaml.load(yaml_text, Loader=yaml.SafeLoader)
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
        if new_query is None:
            print("No info for query - query is None")
            print(yaml_path)
            print(yaml_text)
        return new_query

    except Exception as error:
        print(
            """Failed - either YAML error or issue with creating attrs class. See error, path to the file, 
            and text below."""
        )
        print(error)
        print("path:", yaml_path)
        print("text:", yaml_text)
        return None  # better alternative for this?


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
    queries_by_folder = {}  #:Dict = {} #import
    for query in query_list:
        if query.folder_name == "":
            print(query)
        if query.folder_name not in queries_by_folder:
            queries_by_folder[query.folder_name] = [query]
        else:
            queries_by_folder[query.folder_name].append(query)

    return queries_by_folder


def _create_queryfile_metadata(folder_name: str) -> dict:
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
    dict_to_write = {"metadata": {}, "defaults": {}, "sources": {}}
    dict_to_write["metadata"]["version"] = 1  # write update version functionality
    dict_to_write["metadata"]["description"] = "Sentinel Alert Queries - " + folder_name
    dict_to_write["metadata"]["data_environments"] = [
        "MSSentinel"
    ]  # write how to get this and what type this should be
    dict_to_write["metadata"]["data_families"] = folder_name
    dict_to_write["metadata"]["last_updated"] = str(datetime.now())
    return dict_to_write


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
    query_dict_by_folder = _organize_query_list_by_folder(query_list)
    all_folders = query_dict_by_folder.keys()

    for source_folder in all_folders:
        print("now writing files from folder " + source_folder)
        dict_to_write = _create_queryfile_metadata(source_folder)

        if query_type == "Detections":
            dict_to_write["defaults"]["parameters"] = {
                "add_query_items": {
                    "description": "Additional query clauses",
                    "type": "str",
                    "default": "",
                },
                "start": {"description": "Query start time", "type": "datetime"},
                "end": {"description": "Query end time", "type": "datetime"},
            }  # what are these

        for cur_query in query_dict_by_folder[source_folder]:
            # skipping instances where there is no name but should probably have a better solution
            try:
                formatted_qname = _format_query_name(cur_query.name)
                dict_to_write["sources"][formatted_qname] = {}
                dict_to_write["sources"][formatted_qname][
                    "description"
                ] = cur_query.description
                dict_to_write["sources"][formatted_qname]["metadata"] = {}
                dict_to_write["sources"][formatted_qname]["metadata"]["sentinel"] = {
                    "query_id": cur_query.query_id
                }
                metadata_sections = [
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
                cur_query_dict = attr.asdict(cur_query)
                for section in metadata_sections:
                    dict_to_write["sources"][formatted_qname]["metadata"][
                        section
                    ] = cur_query_dict[section]
                dict_to_write["sources"][formatted_qname]["metadata"]["args"] = {}
                dict_to_write["sources"][formatted_qname]["metadata"]["args"][
                    "query"
                ] = cur_query.query
                dict_to_write["sources"][formatted_qname]["metadata"]["parameters"] = {}
            except Exception as format_error:
                print(
                    """Failed to format query name - see error and source folder as well as 
                    current_query below"""
                )
                print(format_error)
                print("source_folder", source_folder)
                print(cur_query)
                print()

        try:
            query_text = yaml.safe_dump(
                dict_to_write, encoding="utf-8", sort_keys=False
            )
        except yaml.YAMLError as error:
            print(error)
            return False

        try:  # Path(def_path).joinpath("save_queries")
            #             Path(path).mkdir(parents=True, errors=False)
            def_path = Path.joinpath(Path(os.getcwd()))
            path_main = os.path.join(def_path, output_folder + "/")
            path_type = os.path.join(output_folder + "/" + query_type)
            if not os.path.exists(path_main):
                path = os.path.join(def_path, output_folder)
                os.mkdir(path)
            if not os.path.exists(path_type):
                path = os.path.join(output_folder, query_type)
                os.mkdir(path)
            Path(
                output_folder + "/" + query_type + "/" + source_folder + ".yaml"
            ).write_text(query_text.decode("utf-8"))
        except OSError as error:
            print(error)
            return False
    print("done writing query files")
    return True


def import_and_write_sentinel_queries(
    base_dir: str, query_type: str, output_folder: str
):
    """
    Call appropriate functions to write YAML files for the given query_type within
    the base_dir directory into the given output_folder.

    Parameters
    ----------
    base_dir : str
        Path to the directory storing the folders containing the original query YAML files;
        most likely the downloaded Github repo's location
    query_type : str
        Either "Hunting Queries" or "Detections" or otherwise named query category
    output_folder : str
        Path to the folder you want the new generated YAML files to be stored in

    """
    print("read yaml_files")
    yaml_files = read_yaml_files(parent_dir=base_dir, child_dir=query_type)
    print("get query_list")
    query_list = import_sentinel_queries(yaml_files, query_type=query_type)
    query_list = [
        query for query in query_list if query is not None
    ]  # may need better solution for failed query definitions
    print("write out")
    write_to_yaml(query_list, query_type, output_folder)
