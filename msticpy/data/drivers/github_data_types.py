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
class Query(object):
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


def parse_yaml(parent_dir:str, child_dir:str) -> pd.DataFrame:

    sentinel_repourl = "https://github.com/Azure/Azure-Sentinel/blob/master"

    # Collect list of files recusrively uinder a folder
    yaml_queries = glob.glob(f"{parent_dir}/{child_dir}/**/*.yaml", recursive=True)
    no_qf = 0
    total_ct = 0
    query_attrs = []
    for query in yaml_queries: 
        with open(query, "r", encoding="utf-8", errors="ignore") as f:
#             parsed_yaml_df = json_normalize(yaml.load(f, Loader=yaml.FullLoader))
            parsed_yaml_dict = yaml.load(f, Loader=yaml.FullLoader)

            if 'relevantTechniques' in parsed_yaml_dict.keys():
                relevantTechniques_entry = parsed_yaml_dict['relevantTechniques'] if len( parsed_yaml_dict['relevantTechniques']) else parsed_yaml_dict['relevantTechniques']
            else:
                relevantTechniques_entry = []
            if 'severity' in parsed_yaml_dict.keys():
                sev_entry = parsed_yaml_dict['severity']
            else:
                sev_entry = ""
            if 'tags' in parsed_yaml_dict.keys():
                tags_entry = parsed_yaml_dict['tags']
            else:
                tags_entry = []
            if 'entityMappings' in parsed_yaml_dict.keys():
                entMap_entry = parsed_yaml_dict['entityMappings'] if len(parsed_yaml_dict['entityMappings']) else parsed_yaml_dict['entityMappings']
            else:
                entMap_entry = {}
            if 'customDetails' in parsed_yaml_dict.keys():
                custDetails_entry = parsed_yaml_dict['customDetails']
            else:
                custDetails_entry = {}
            if 'alertDetailsOverride' in parsed_yaml_dict.keys():
                alertDetails_entry = parsed_yaml_dict['alertDetailsOverride']
            else:
                alertDetails_entry = {}
            if 'queryFrequency' in parsed_yaml_dict.keys():
                qf_entry = parsed_yaml_dict['queryFrequency']
            else:
                qf_entry = ""
            if 'queryPeriod' in parsed_yaml_dict.keys():
                qp_entry = parsed_yaml_dict['queryPeriod']
            else:
                qp_entry = ""
            if 'triggerOperator' in parsed_yaml_dict.keys():
                to_entry = parsed_yaml_dict['queryPeriod']
            else:
                to_entry = ""
            if 'triggerThreshold' in parsed_yaml_dict.keys():
                tt_entry = parsed_yaml_dict['triggerThreshold']
            else:
                tt_entry = ""
            if 'tactics' in parsed_yaml_dict.keys():
                tactics_entry = parsed_yaml_dict['tactics']
            else:
                tactics_entry = ""
            if 'requiredDataConnectors' in parsed_yaml_dict.keys():
                reqDataConnectors_entry = parsed_yaml_dict['requiredDataConnectors'] if len(parsed_yaml_dict['requiredDataConnectors']) else parsed_yaml_dict['requiredDataConnectors']
            else:
                reqDataConnectors_entry = {}
            if 'query' in parsed_yaml_dict.keys():
                query_entry = parsed_yaml_dict['query']
            else:
                query_entry = ""
            if 'version' in parsed_yaml_dict.keys():
                version_entry = parsed_yaml_dict['version']
            else:
                version_entry = ""
            if 'kind' in parsed_yaml_dict.keys():
                kind_entry = parsed_yaml_dict['kind']
            else:
                kind_entry = ""
            new_query = Query(id=parsed_yaml_dict['id'], 
                              name=parsed_yaml_dict['name'], 
                              description=parsed_yaml_dict['description'],
                              severity=sev_entry, 
                              tags=tags_entry,
                              requiredDataConnectors=reqDataConnectors_entry,
                              queryFrequency=qf_entry, 
                              queryPeriod=qp_entry,
                              triggerOperator=to_entry,
                              triggerThreshold=tt_entry,
                              tactics=tactics_entry,
                              relevantTechniques=relevantTechniques_entry,
                              query=query_entry,
                              version=version_entry,
                              kind=kind_entry
                             )
            query_attrs.append(new_query)

    return query_attrs

        
def format_query_name(qname):
    characters_to_replace = ["(", ")", "-", "%", "[", "]", "{", "}", "*", "'", '"', "/", ".", "\\"]
    ret = qname.replace(" ", "_")
    for c in characters_to_replace:
        ret = ret.replace(c, "")
    ret = ret.replace("__", "_")
    return ret


def write_to_yaml(query_list, ct_folders, qtype, def_path):
#     ret_dicts = []
    cur_index = 0
    for f in ct_folders:
        print("now writing files from folder " + f)
        dict_to_write = {'metadata': dict(), 'defaults': dict(), 'sources': dict()}
        dict_to_write['metadata']['version'] = 1 # write update version functionality 
        dict_to_write['metadata']['description'] = "Sentinel Alert Queries - " + f
        dict_to_write['metadata']['data_environments'] = ["MSSentinel"] # write how to get this and what type this should be
        dict_to_write['metadata']['data_families'] = f
        dict_to_write['metadata']['last_updated'] = str(datetime.now())
        dict_to_write['defaults']['metadata'] = dict() # what is this
        if qtype == "detection":
            dict_to_write['defaults']['parameters'] = {'add_query_items': {'description': 'Additional query clauses', 'type': 'str', 'default': ''}, 'start': {'description': 'Query start time', 'type': 'datetime'}, 'end': {'description': 'Query end time', 'type': 'datetime'}} # what are these 
        for c in range(ct_folders[f]):
            cur_query = query_list[c+cur_index]
            formatted_qname = format_query_name(cur_query.name)
            sentinel_features = list(attr.asdict(cur_query).keys())
            sentinel_features.remove('description')
            sentinel_features.remove('query')
            dict_to_write['sources'][formatted_qname] = dict()
            dict_to_write['sources'][formatted_qname]['description'] = cur_query.description
            dict_to_write['sources'][formatted_qname]['metadata'] = {"pivot": {"short_name": "interface", "direct_func_entities": ["Host"]}, "sentinel": dict()}
            cur_query_dict = attr.asdict(cur_query)
            for feature in sentinel_features:
                dict_to_write['sources'][formatted_qname]['metadata']['sentinel'][feature] = cur_query_dict[feature]
#             dict_to_write['sources'][format_query_name(cur_query.name)] = attr.asdict(cur_query)
            dict_to_write['sources'][formatted_qname]['args'] = dict()
            dict_to_write['sources'][formatted_qname]['args']['query']= cur_query.query
            if qtype == "detection":
                dict_to_write['sources'][formatted_qname]['parameters'] = dict()
            elif qtype == "hunting":
                dict_to_write['sources'][formatted_qname]['parameters'] = {"start": {"description": "Query start time", "type": "datetime", "aliases": ["StartTimeISO"]}, "end": {"description": "Query end time", "type": "datetime", "aliases": ["EndTimeISO"]}}
        cur_index += ct_folders[f]
#         ret_dicts.append(dict_to_write)
        
        try:
            query_text = yaml.safe_dump(dict_to_write, encoding="utf-8")
        except yaml.YAMLError as e:
            print(e)
        
        try:
            path_main = os.path.join(def_path, "saved_queries/")
            path_type = os.path.join("saved_queries/" + qtype)
            if not os.path.exists(path_main):
                path = os.path.join(def_path, "saved_queries")
                os.mkdir(path)
            if not os.path.exists(path_type):
                path = os.path.join("saved_queries", qtype)
                os.mkdir(path)
            Path("saved_queries/" + qtype + "/" + f + ".yaml").write_text(str(query_text))
        except OSError as e:
            print(e)
    print("done writing query files")


def import_parse_write_queries():
    print('downloading files from github')
    def_path = Path.joinpath(Path(os.getcwd()))
    # Download the Microsoft Sentinel Github repo as ZIP
    azsentinel_git_url = 'https://github.com/Azure/Azure-Sentinel/archive/master.zip'
    get_sentinel_queries_from_github(git_url=azsentinel_git_url, outputdir=def_path)
    base_dir = str(def_path) + "/Azure-Sentinel-master"
    
    print('processing queries')
    detection_queries = parse_yaml(parent_dir=base_dir, child_dir="Detections")
    hunting_queries = parse_yaml(parent_dir=base_dir, child_dir="Hunting Queries")

    d_child_dir = "Detections"
    yaml_queries = glob.glob(f"{base_dir}/{d_child_dir}/**/*.yaml", recursive=True)
    d_folders = [address.split('\\')[-2] for address in yaml_queries]
    d_ct_folders = Counter(d_folders)
    
    h_child_dir = "Hunting Queries"
    yaml_queries = glob.glob(f"{base_dir}/{h_child_dir}/**/*.yaml", recursive=True)
    h_folders = [address.split('\\')[-2] for address in yaml_queries]
    h_ct_folders = Counter(h_folders)

    write_to_yaml(detection_queries, d_ct_folders, "detection", def_path)
    write_to_yaml(hunting_queries, h_ct_folders, "hunting", def_path)