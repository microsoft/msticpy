from ..storage.github_download import get_sentinel_queries_from_github

#imports 
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
from requests_html import HTMLSession

def parse_yaml(parent_dir:str, child_dir:str) -> pd.DataFrame:

    sentinel_repourl = "https://github.com/Azure/Azure-Sentinel/blob/master"

    # Collect list of files recusrively uinder a folder
    yaml_queries = glob.glob(f"{parent_dir}/{child_dir}/**/*.yaml", recursive=True)
    df = pd.DataFrame()

    # Recursively load yaml Files and append to dataframe
    for query in yaml_queries:
        with open(query, "r", encoding="utf-8", errors="ignore") as f:
            parsed_yaml_df = json_normalize(yaml.load(f, Loader=yaml.FullLoader))
            parsed_yaml_df["DetectionURL"] = query.replace(parent_dir, sentinel_repourl)
            df = df.append(parsed_yaml_df, ignore_index=True, sort=True)

    if child_dir == "Detections":
        df["DetectionType"] = "Analytics"
    elif child_dir == "Hunting Queries":
        df["DetectionType"] = "Hunting"

    df["DetectionService"] = "Microsoft Sentinel Community Github"

    return df



#Custom ConnectorId to Platform Mapping
platform_mapping = {
    "AIVectraDetect" : ["Azure", "Windows", "Linux"],
    "AlsidForAD" : ["Azure", "Azure AD"],
    "AWS": ["AWS"],
    "AWSS3": ["AWS", "SaaS"],
    "AzureActiveDirectory": ["Azure", "Azure AD"],
    "AzureActiveDirectoryIdentityProtection": ["Azure", "Azure AD"],
    "AzureActivity": ["Azure", "SaaS"],
    "AzureFirewall" : ["Azure", "Windows", "Linux"],
    "AzureDevOpsAuditing": ["Azure", "SaaS"],
    "AzureMonitor": ["SaaS"],
    "AzureMonitor(IIS)": ["Azure"],
    "AzureMonitor(Keyvault)": ["Azure"],
    "AzureMonitor(Query Audit)": ["Azure"],
    "AzureMonitor(VMInsights)": ["Azure", "Windows", "Linux"],
    "AzureMonitor(WindowsEventLogs)": ["Azure", "Windows"],
    "AzureMonitor(WireData)": ["Azure", "Windows", "Linux"],
    "AzureNetworkWatcher": ["Azure", "Windows", "Linux"],
    "AzureSecurityCenter": ["Azure", "SaaS"],
    "Barracuda": ["Azure", "Windows", "Linux"],
    "BehaviorAnalytics": ["Azure AD", "Azure", "Windows"],
    "CEF": ["Azure", "Windows", "Linux"],
    "CheckPoint": ["Azure", "Windows", "Linux"],
    "CiscoASA": ["Azure", "Windows", "Linux"],
    "CiscoUmbrellaDataConnector": ["Windows", "Linux"],
    "CognniSentinelDataConnector": ["SaaS"],
    "CyberpionSecurityLogs" : ["SaaS"],
    "CustomConnector": ["Unknown"],
    "DNS": ["Azure", "Windows", "Linux"],
    "EsetSMC": ["Azure", "Windows", "Linux"],
    "F5": ["Azure", "Windows", "Linux"],
    "Fortinet": ["Azure", "Windows", "Linux"],
    "GitHub": ["SaaS", "Windows", "Linux"],
    "InfobloxNIOS": ["Azure", "Windows", "Linux"],
    "Microsoft365Defender": ["Azure","Windows"],
    "MicrosoftCloudAppSecurity": ["Azure", "AWS", "GCP", "SaaS"],
    "MicrosoftDefenderAdvancedThreatProtection": ["Windows", "Linux"],
    "MicrosoftThreatProtection": ["Azure","Windows"],
    "Office365": ["Office 365"],
    "OfficeATP": ["Office 365"],
    "OktaSSO": ["Azure AD", "AWS", "GCP", "SaaS"],
    "PaloAltoNetworks": ["Azure", "Windows", "Linux"],
    "ProofpointPOD": ["Office 365"],
    "ProofpointTAP": ["Office 365"],
    "PulseConnectSecure": ["Azure", "Windows", "Linux"],
    "QualysVulnerabilityManagement": ["Azure", "Windows", "Linux", "macOS"],
    "SecurityEvents": ["Windows"],
    "SophosXGFirewall": ["Azure", "Windows", "Linux"],
    "SymantecProxySG": ["Azure", "Windows", "Linux"],
    "SymantecVIP": ["Azure", "Windows", "Linux"],
    "Syslog": ["Linux"],
    "ThreatIntelligence": [
        "Windows",
        "Linux",
        "macOS",
        "Azure",
        "AWS",
        "Azure AD",
        "Office 365",
    ],
    "ThreatIntelligenceTaxii": [
        "Windows",
        "Linux",
        "macOS",
        "Azure",
        "AWS",
        "Azure AD",
        "Office 365",
    ],
    "TeamsLogs": ["Windows", "Linux", "macOS"],
    "TrendMicro": ["Windows", "Linux", "macOS"],
    "TrendMicroXDR": ["Windows", "Linux", "macOS"],
    "VMwareCarbonBlack": ["Windows", "Linux", "macOS"],
    "WAF": ["Azure", "SaaS"],
    "WindowsFirewall": ["Windows"],
    "WindowsSecurityEvents": ["Windows"],
    "Zscaler": ["Azure", "Windows", "Linux"],
    "ZoomLogs": ["SaaS"],
}

def clean_and_preprocess_data(df):

    columns = [
        "DetectionType",
        "DetectionService",
        "id",
        "name",
        "tags",
        "description",
        "query",
        "queryFrequency",
        "queryPeriod",
        "triggerOperator",
        "triggerThreshold",
        "tactics",
        "relevantTechniques",
        "requiredDataConnectors",
        "severity",
        "DetectionURL",
        "IngestedDate",
    ]

    # Reording columns
    df = df[columns]

    # Inserting additional columns to list at specific index for later use
    columns.insert(5, "connectorId")
    columns.insert(6, "dataTypes")

    #Ignoring the records with invalid connector values
    df = df[df.requiredDataConnectors.apply(lambda x: x != [{'connectorId': []}])]
    
    # Handle null values in required data connectors
    isnull = df.requiredDataConnectors.isnull()
    if len(df[isnull]) > 0:
        df.loc[isnull, "requiredDataConnectors"] = [
            [[]] * isnull.sum()
        ]

    no_of_records_with_emptylist_connectors = len(
        df[
            df["requiredDataConnectors"].map(lambda d: len(d)) == 0
        ]
    )

    # Separate Null and Not Null requiredDataConnectors
    not_null_df = (
        df[
            df["requiredDataConnectors"].map(lambda d: len(d)) > 0
        ]
        .reset_index()
        .drop("index", axis=1)
    )
    empty_null_df = (
        df[df.requiredDataConnectors.isnull()]
        .reset_index()
        .drop("index", axis=1)
    )
    null_df = (
        df[
            df["requiredDataConnectors"].map(lambda d: len(d)) == 0
        ]
        .reset_index()
        .drop("index", axis=1)
    )

    # Exploding columns to flatten the table
    columns_to_expand = [
        "tactics",
        "relevantTechniques",
        "requiredDataConnectors"
    ]
    for column in columns_to_expand:
        not_null_df = not_null_df.explode(column).reset_index(drop=True)

    # #Apply Data wrangling to derive columns from Json response
    final_not_null_df = pd.DataFrame(
        not_null_df["requiredDataConnectors"].values.tolist()
    )

    # Concatenate 2 dataframs vertically
    result_not_null_df = pd.concat([not_null_df, final_not_null_df], axis=1)

    # Exploding dataTypes column
    result_not_null_df = result_not_null_df.explode("dataTypes").reset_index(drop=True)
    new_columns = [
        "DetectionType",
        "DetectionService",
        "id",
        "name",
        "tags",
        "description",
        "connectorId",
        "dataTypes",
        "query",
        "queryFrequency",
        "queryPeriod",
        "triggerOperator",
        "triggerThreshold",
        "tactics",
        "relevantTechniques",
        "severity",
        "DetectionURL",
        "IngestedDate",
    ]
    result_not_null_df = result_not_null_df[new_columns]
 

    result_not_null_df["Platform"] = result_not_null_df.connectorId.map(platform_mapping)
    result_not_null_df = result_not_null_df.explode("Platform").reset_index(drop=True)

    # Exploding columns to flatten the table
    columns_to_expand = ["tactics", "relevantTechniques"]
    for column in columns_to_expand:
        null_df = null_df.explode(column).reset_index(drop=True)

    null_df["connectorId"] = "CustomConnector"
    null_df["dataTypes"] = null_df.DetectionURL.apply(
        lambda x: pd.Series(str(x).split("/")[-2] + "_CL")
    )
    null_df["Platform"] = ""

    new_columns = [
        "DetectionType",
        "DetectionService",
        "id",
        "name",
        "tags",
        "description",
        "connectorId",
        "dataTypes",
        "query",
        "queryFrequency",
        "queryPeriod",
        "triggerOperator",
        "triggerThreshold",
        "tactics",
        "relevantTechniques",
        "severity",
        "DetectionURL",
        "IngestedDate",
        "Platform",
    ]

    result_null_df = null_df[new_columns]

    result = pd.concat([result_not_null_df, result_null_df], axis=0)

    return result


def download_queries_return_df(git_url: Optional[
        str
    ] = "https://github.com/Azure/Azure-Sentinel/archive/master.zip",
    outputdir: Optional[str] = None):
    def_path = Path.joinpath(Path(os.getcwd()))
    get_sentinel_queries_from_github(outputdir=def_path)
    base_dir = str(def_path) + "/Azure-Sentinel-master"
    detections_df = parse_yaml(parent_dir=base_dir, child_dir="Detections")
    hunting_df = parse_yaml(parent_dir=base_dir, child_dir="Hunting Queries")
    frames = [detections_df, hunting_df]
    sentinel_github_df = pd.concat(frames).reset_index()
    sentinel_github_df = sentinel_github_df.copy()
    sentinel_github_df["DetectionURL"] = sentinel_github_df["DetectionURL"].str.replace(
        " ", "%20", regex=True
    )
    sentinel_github_df["IngestedDate"] = date.today()
    result = clean_and_preprocess_data(df=sentinel_github_df)
    result_df = (result.groupby(['id', 'name', 'DetectionType', 'DetectionService', 'description', 'query', 'DetectionURL', 'IngestedDate'])
           .agg({'connectorId': lambda x: get_unique_vals(x), 'dataTypes': lambda x: get_unique_vals(x), 'tags': lambda x: get_unique_vals(x), 'Platform': lambda x: get_unique_vals(x), 'relevantTechniques': lambda x: get_unique_vals(x), 'queryFrequency': lambda x: get_unique_vals(x), 'queryPeriod': lambda x: get_unique_vals(x), 'triggerOperator': lambda x: get_unique_vals(x), 'tactics': lambda x: get_unique_vals(x), 'severity': lambda x: get_unique_vals(x)})).reset_index()
    return result_df


def generate_yaml_files():
    return None