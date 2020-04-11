# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Morph Charts  class."""
import json
from pathlib import Path
import yaml
import IPython
import pandas as pd


from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"

_CHART_FOLDER = "morph_charts"


class MorphCharts:
    """Create Morph Charts package data and render Morph Charts site."""

    def __init__(self):
        """Create object and populate charts container."""
        self.charts = _get_charts(_CHART_FOLDER)

    def display(self, data: pd.DataFrame, chart_name: str):
        """
        Prepare package data and display MorphChart in an IFrame.

        Parameters
        ----------
        data: pd.DataFrame:
            A DataFrame of data for the morphchart to plot.

        chart_name: str:
            The name of the Morph Chart to plot.

        """
        # Create description file with length of our data set
        description_dict = self.charts[chart_name]["DescriptionFile"]
        description_dict["tables"][0]["rows"] = len(data)
        # Create output folder for package files
        out_path = Path.cwd().joinpath(*["morphchart_package", "description.json"])
        Path.mkdir(Path.cwd().joinpath("morphchart_package"), exist_ok=True)
        # Write description file
        morph_file = open(out_path, "w")
        json.dump(description_dict, morph_file)
        # Write dataset to query_data csv
        data_out_path = out_path = Path.cwd().joinpath(
            *["morphchart_package", "query_data.csv"]
        )
        data.to_csv(data_out_path, index=False)
        # Display Morph Charts in IFrame with instructions
        print(
            f"Navigate to {Path.cwd().joinpath('morphchart_package')} and upload the files below"
        )
        return IPython.display.IFrame(
            "http://morphcharts.com/designer.html", "100%", "600px"
        )

    def list_charts(self):
        """Get a list of avaliable charts."""
        for key, _ in self.charts.items():
            print(key)

    def get_chart_details(self, chart_name):
        """
        Get description for a chart.

        Parameters
        ----------
        chart_name: str:
            The name of the chart you get description for.

        """
        print(chart_name, ":")
        print(self.charts[chart_name]["Description"])

    def search_charts(self, keyword):
        """
        Search for charts that match a keyword.

        Parameters
        ----------
        keyword: str:
            The keyword to search charts for.

        """
        for key, value in self.charts.items():
            if keyword in value["Tags"]:
                print(key, ":")
                print(value["Description"])
            else:
                print("No matching charts found")


def _get_charts(path: str = "MorphCharts"):
    """
    Return dictionary of yaml files found in the Morph Charts folder.

    Parameters
    ----------
    path : str
        The source path to search in.

    Returns
    -------
    Dict
        Details of the chart files

    """
    full_path = Path(__file__).parent.parent.joinpath("data").joinpath(path)
    file_glob = Path(full_path).glob(f"*.yaml")
    chart_files = [file_path for file_path in file_glob if file_path.is_file()]
    chart_details = {}
    for chart in chart_files:
        chart_data = open(chart, "r")
        details = yaml.safe_load(chart_data)
        chart_details.update(
            {
                details["Name"]: {
                    "Description": details["Description"],
                    "Tags": details["Tags"],
                    "DescriptionFile": details["DescriptionFile"],
                }
            }
        )

    return chart_details
