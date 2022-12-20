import attr
from attr import attrs, attrib
 
import requests
import zipfile
import io
import re
import glob
import IPython
import pandas as pd
from ipywidgets import widgets as ipywidgets, Layout
from IPython.display import display, HTML
from pathlib import Path
from requests.exceptions import HTTPError
from typing import Callable, List, Optional, Set, Tuple, IO

import yaml
import json
import csv
import os

import matplotlib.pyplot as plt
import sys

from pandas import json_normalize
from tqdm.notebook import tqdm
from datetime import date
from datetime import datetime
from requests_html import HTMLSession
from collections import Counter

@attrs
class SentinelQuery(object): # 
    id: str = attrib(factory=str)
    name: str = attrib(factory=str)
    description: str = attrib(factory=str)
    severity: str = attrib(factory=str)
    tags: dict = attrib(factory=list)
    requiredDataConnectors: dict = attrib(factory=dict)
    queryFrequency: str = attrib(factory=str)
    queryPeriod: str = attrib(factory=str)
    triggerOperator: str = attrib(factory=str)
    triggerThreshold: str = attrib(factory=str)
    tactics: list = attrib(factory=list)
    relevantTechniques: list = attrib(factory=list)
    query: str = attrib(factory=list)
    entityMappings: dict = attrib(factory=dict)
    customDetails: dict = attrib(factory=dict)
    alertDetailsOverride: dict = attrib(factory=dict)
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
    """
    Download Microsoft Sentinel Github archive and extract detection and hunting queries.

    Parameters
    ----------
    git_url : str, optional
        URL of the GIT Repository to be downloaded, by default "https://github.com/Azure/Azure-Sentinel/archive/master.zip"
    outputdir : str, optional
        Provide absolute path to the output folder to save downloaded archive (e.g. '/usr/home' or 'C:\downloads'),
        If no path provided, it will download to .msticpy dir under Azure-Sentinel directory.
    """

    if outputdir is None:
        outputdir = Path.joinpath(Path("~").expanduser(), ".msticpy", "Azure-Sentinel")

    try:
        with requests.get(git_url, stream=True) as response:
            response = requests.get(git_url, stream=True)
            total_size_in_bytes= int(response.headers.get('content-length', 0))
            block_size = 1024
            progress_bar = tqdm(desc="Downloading from Microsoft Sentinel Github" , initial= 0, unit='iB', unit_scale=True)
            response.raise_for_status()
            repo_zip = Path.joinpath(Path(outputdir),"Azure-Sentinel.zip")
            with open(repo_zip, 'wb') as file:
                for data in response.iter_content(chunk_size=10000):
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

    except HTTPError as http_err:
        warnings.warn(f"HTTP error occurred trying to download from Github: {http_err}")


def read_yaml_files(parent_dir, child_dir) -> dict:
    # enumerate the files and read the yaml
    yaml_queries = glob.glob(f"{parent_dir}/{child_dir}/**/*.yaml", recursive=True)
    
    parsed_query_dict = {}
    
    for query in yaml_queries: 
        parsed_query_dict[query] = yaml_text = open(query, encoding="utf8", errors="ignore").read()
        
    return parsed_query_dict


def import_sentinel_queries(yaml_files, query_type) -> list:
     return [_import_sentinel_query(yaml_path, yaml_text, query_type) for yaml_path, yaml_text in yaml_files.items()]


def _import_sentinel_query(yaml_path, yaml_text, query_type) -> SentinelQuery:
    # most of the stuff that's in your current "parse_yaml" function
    try:
        parsed_yaml_dict = yaml.load(yaml_text, Loader=yaml.FullLoader)
        new_query = SentinelQuery(name=parsed_yaml_dict.get("name"), 
                          id=parsed_yaml_dict.get("id", ""), 
                          description=parsed_yaml_dict.get("description", ""),
                          severity=parsed_yaml_dict.get('severity', ""),
                          tags=parsed_yaml_dict.get('tags_entry', []),
                          requiredDataConnectors=parsed_yaml_dict.get('requiredDataConnectors', {}),
                          queryFrequency=parsed_yaml_dict.get('queryFrequency', ""),
                          queryPeriod=parsed_yaml_dict.get('queryPeriod', ""),
                          triggerOperator=parsed_yaml_dict.get('triggerOperator', ""),
                          triggerThreshold=parsed_yaml_dict.get('triggerThreshold', ""),
                          tactics=parsed_yaml_dict.get('tactics', ""),
                          relevantTechniques=parsed_yaml_dict.get('relevantTechniques', []),
                          query=parsed_yaml_dict.get('query', ""),
                          version=parsed_yaml_dict.get('version', ""),
                          kind=parsed_yaml_dict.get('kind', ""),
                          folder_name=yaml_path.split('\\')[-2],
                          source_file_name=yaml_path,
                          query_type=query_type)
        if new_query is None:
            print("found a none")
            print(yaml_path)
            print(yaml_text)
        return new_query
    
    except Exception as e:
        print('failed')
        print(e)
        print("path:", yaml_path)
        print("text:", yaml_text)
        return  # better alternative for this?

        
def format_query_name(qname):
    del_chars_pattern = "[%*\*\'\"\\.()[\]{}-]"
    repl_str = re.sub(del_chars_pattern, "", qname)
    repl_str = repl_str.replace(" ", "_").replace("__", "_")
    return repl_str


def _organize_query_list_by_folder(query_list):
    queries_by_folder = {}
    for query in query_list:
        if query.folder_name == '':
            print(query)
        if query.folder_name not in queries_by_folder.keys():
            queries_by_folder[query.folder_name] = [query]
        else:
            queries_by_folder[query.folder_name].append(query)
        
    return queries_by_folder


def _create_queryfile_metadata(folder_name):
    dict_to_write = {'metadata': dict(), 'defaults': dict(), 'sources': dict()}
    dict_to_write['metadata']['version'] = 1 # write update version functionality 
    dict_to_write['metadata']['description'] = "Sentinel Alert Queries - " + folder_name
    dict_to_write['metadata']['data_environments'] = ["MSSentinel"] # write how to get this and what type this should be
    dict_to_write['metadata']['data_families'] = folder_name
    dict_to_write['metadata']['last_updated'] = str(datetime.now())
    return dict_to_write


def write_to_yaml(query_list, query_type, output_folder):
    query_dict_by_folder = _organize_query_list_by_folder(query_list)
    all_folders = query_dict_by_folder.keys()
        
    for source_folder in all_folders:
        print("now writing files from folder " + source_folder)
        dict_to_write = _create_queryfile_metadata(source_folder)
    
        if query_type == "Detections":
            dict_to_write['defaults']['parameters'] = {'add_query_items': {'description': 'Additional query clauses', 'type': 'str', 'default': ''}, 'start': {'description': 'Query start time', 'type': 'datetime'}, 'end': {'description': 'Query end time', 'type': 'datetime'}} # what are these 

        for cur_query in query_dict_by_folder[source_folder]:
            # skipping instances where there is no name but should probably have a better solution
            try:
                formatted_qname = format_query_name(cur_query.name)
            except:
                print('source_folder', source_folder)
                print(cur_query)
                print()
                pass
            dict_to_write['sources'][formatted_qname] = {}
            dict_to_write['sources'][formatted_qname]['description'] = cur_query.description
            dict_to_write['sources'][formatted_qname]['metadata'] = {}
            dict_to_write['sources'][formatted_qname]['metadata']['sentinel'] = {'id': cur_query.id}
            metadata_sections = ['name', 'severity', 'tags', 'requiredDataConnectors', 'queryFrequency', 'queryPeriod', 'triggerOperator', 'triggerThreshold', 'tactics', 'relevantTechniques','entityMappings', 'customDetails', 'alertDetailsOverride', 'version', 'kind', 'folder_name', 'source_file_name', 'query_type']
            cur_query_dict = attr.asdict(cur_query)
            for section in metadata_sections:
                dict_to_write['sources'][formatted_qname]['metadata'][section] = cur_query_dict[section]
            dict_to_write['sources'][formatted_qname]['metadata']['args'] = {}
            dict_to_write['sources'][formatted_qname]['metadata']['args']['query'] = cur_query.query
            dict_to_write['sources'][formatted_qname]['metadata']['parameters'] = {}

        try:
            query_text = yaml.safe_dump(dict_to_write, encoding="utf-8", sort_keys=False)
        except yaml.YAMLError as e:
            print(e)       

        try: # Path(def_path).joinpath("save_queries")
#             Path(path).mkdir(parents=True, errors=False)
            path_main = os.path.join(def_path, output_folder + "/")
            path_type = os.path.join(output_folder + "/" + query_type)
            if not os.path.exists(path_main):
                path = os.path.join(def_path, output_folder)
                os.mkdir(path)
            if not os.path.exists(path_type):
                path = os.path.join(output_folder, query_type)
                os.mkdir(path)
            Path(output_folder + "/" + query_type + "/" + source_folder + ".yaml").write_text(query_text.decode('utf-8'))
        except OSError as e:
            print(e)
    print("done writing query files")


def import_and_write_sentinel_queries(base_dir, query_type, output_folder):
    print('read yaml_files')
    yaml_files = read_yaml_files(parent_dir=base_dir, child_dir=query_type)
    print('get query_list')
    query_list = import_sentinel_queries(yaml_files, query_type=query_type)
    query_list = [l for l in query_list if l is not None] # may need better solution for failed query definitions
    print('write out')
    write_to_yaml(query_list, query_type, output_folder)