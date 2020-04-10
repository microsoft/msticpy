# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Morph Charts  class."""
import json
import yaml

from .._version import VERSION

__version__ = VERSION
__author__ = "Pete Bryan"

_CHART_FOLDER = "morph_charts"

class MorphCharts():
    def __init__():
        self.charts = _get_charts(_CHART_FOLDER)

    def prepare(data: pd.DataFrame, chart_name: str):
        description_dict = self.charts[chart_name]['DescriptionFile']
        description_dict['tables'][0]['rows'] = len(data)
        out_path = Path.cwd().joinpath(*['morphchart_package','description.json'])
        morph_file = description = open(out_path, 'w')
        json.dump(description_dict, morph_file)
        data_out_path = out_path = Path.cwd().joinpath(*['morphchart_package','query_data.csv'])
        data.to_csv(data_out_path, index=False)
        print(f"Navigate to {Path.cwd().joinpath('morphchart_package')} and upload the filed to http://morphcharts.com/designer.html")


    def list_charts():
        for key, _ self.charts.items():
            print(key)

    def get_chart_details(chart_name):
        print(chart_name,":")
        print(self.charts[chart_name]['Description'])

    def search_charts(keyword):
        for k,v in self.charts.items():
            if keyword in v['Tags']:
                print(k,":")
                print(v['Description'])
        else:
            print('No matching charts found')


def _get_charts(path:str = 'MorphCharts'):
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
    full_path = Path(__file__).parent.joinpath(*['data','queries']).joinpath(morph_foler)
    file_glob = Path(full_path).glob(f"*.yaml")
    chart_files = [file_path for file_path in file_glob if file_path.is_file()]
    chart_details = {}
    for chart in chart_files:
        chart_data = open(chart, 'r')
        details = yaml.safe_load(chart_data)
        chart_details.update({details['Name']: {"Description":details['Description'], "Tags":details["Tags"], "DescriptionFile":details['DescriptionFile']}})

    return chart_details
