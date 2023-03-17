# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Morph Charts  class."""
import json
from pathlib import Path

import pandas as pd
import yaml
from deprecated.sphinx import deprecated
from IPython.display import IFrame

from .._version import VERSION
from ..common.exceptions import MsticpyException

__version__ = VERSION
__author__ = "Pete Bryan"

_CHART_FOLDER = "morph_charts"


class MorphCharts:
    """Create Morph Charts package data and render Morph Charts site."""

    @deprecated("Morphchart functionality has been deprecated.", version="2.2.0")
    def __init__(self):
        """Create object and populate charts container."""
        self.charts = _get_charts(_CHART_FOLDER)

    def display(self, data: pd.DataFrame, chart_name: str) -> IFrame:
        """
        Prepare package data and display MorphChart in an IFrame.

        Parameters
        ----------
        data: pd.DataFrame:
            A DataFrame of data for the morphchart to plot.

        chart_name: str:
            The name of the Morph Chart to plot.

        """
        # Check input data is correct format and that the chart being requested exists
        if not isinstance(data, pd.DataFrame):
            raise MsticpyException("Data provided must be in pandas.DataFrame format")

        if chart_name not in self.charts:
            raise MsticpyException(
                f"{chart_name} is not a vaid chart. Run list_charts() to see avaliable charts"  # pylint: disable=line-too-long
            )

        # Create description file with length of our data set
        description_dict = self.charts[chart_name]["DescriptionFile"]
        description_dict["tables"][0]["rows"] = len(data)
        # Create output folder for package files
        out_path = Path.cwd().joinpath(*["morphchart_package", "description.json"])
        Path.mkdir(Path.cwd().joinpath("morphchart_package"), exist_ok=True)
        # Write description file
        with open(out_path, "w", encoding="utf-8") as morph_file:
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
        print("Charts provided by http://morphcharts.com/")
        return IFrame("http://morphcharts.com/designer.html", "100%", "600px")

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
        try:
            print(
                chart_name,
                ":",
                "\n",
                self.charts[chart_name]["Description"],
                "\n",
                "Query: ",
                self.charts[chart_name]["Query"],
            )
        except KeyError as key_err:
            raise KeyError(f"Unknown chart {chart_name}") from key_err

    def search_charts(self, keyword):
        """
        Search for charts that match a keyword.

        Parameters
        ----------
        keyword: str:
            The keyword to search charts for.

        """
        for key, value in self.charts.items():
            if keyword.casefold() in [tag.casefold() for tag in value["Tags"]]:
                print(key, ":", "\n", value["Description"])
            elif keyword.casefold() in [
                word.casefold() for word in value["Description"].split()
            ]:
                print(key, ":", "\n", value["Description"])
            else:
                print("No matching charts found")


def _get_charts(path: str = "morph_charts") -> dict:
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
    full_path = Path(__file__).parent.parent.joinpath("resources").joinpath(path)
    file_glob = Path(full_path).glob("*.yaml")
    chart_files = [file_path for file_path in file_glob if file_path.is_file()]
    chart_details = {}
    for chart in chart_files:
        with open(chart, "r", encoding="utf-8") as chart_data:
            details = yaml.safe_load(chart_data)
        try:
            chart_details.update(
                {
                    details["Name"]: {
                        "Description": details["Description"],
                        "Query": details["Query"],
                        "Tags": details["Tags"],
                        "DescriptionFile": details["DescriptionFile"],
                    }
                }
            )
        except KeyError as key_err:
            raise LookupError(
                f"{chart} description does not appear to be in the correct format."
            ) from key_err

    return chart_details
