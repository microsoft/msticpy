# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
Test run notebooks.

The script runs all of the notebooks in the --nb_path folder
(exceptions for notebooks that cannot be run without authentication
"""
import argparse
import os
from collections import namedtuple
from functools import partial
from multiprocessing import Process, Queue
from pathlib import Path
from typing import Any, List, Optional, Tuple

import nbformat
from nbconvert import HTMLExporter
from nbconvert.preprocessors import CellExecutionError, ExecutePreprocessor
from toollib.url_checker_async import check_html

NBError = namedtuple("NBError", "title, errname, errvalue, exception")
_NB_FOLDER = "docs/notebooks"

_IGNORE_NBS = [
    "AnomalousSequence.ipynb",
    "AWS_S3_HoneybucketLogAnalysis.ipynb",
    "AzureSentinelAPIs.ipynb",
    "AzureBlobStorage.ipynb",
    "Data_Queries.ipynb",
    "DataUploader.ipynb",
    "Kusto-Analysis.ipynb",
    "Kusto-Ingest.ipynb",
    "MDATPQuery.ipynb",
    "MicrosoftDefender.ipynb",
    "MSTICpy_Blackhat_Demo_2020.ipynb",
    "ResourceGraphDriver.ipynb",
    "Splunk-DataConnector.ipynb",
    "Sumologic-DataConnector.ipynb",
    "VirusTotalLookup.ipynb",
    "VTLookupV3.ipynb",
]


def test_all_notebooks(
    src_path: str,
    out_path: str,
    kernel: str = "python3",
    run_all: bool = False,
    links: bool = False,
    ignore_urls: List[str] = None,
):
    """
    Run notebooks.

    Parameters
    ----------
    src_path : str
        Source path for notebooks
    out_path : str
        Path to save failed notebooks
    kernel : str, optional
        Notebook kernel name, by default "python3"
    run_all : bool, optional
        Skip notebook exceptions, by default False
    links : bool, optional
        Test URIs in notebook, by default False
    ignore_urls : List[str]
        List of URLs to skip checks on.

    """
    notebook_exec_errors: List[Tuple[str, str, str, str]] = []
    for notebook_file in Path(src_path).glob("*.ipynb"):
        if str(notebook_file).casefold().endswith("-err.ipynb"):
            continue
        if not run_all and any(
            str(notebook_file).casefold().endswith(skip_file.casefold())
            for skip_file in _IGNORE_NBS
        ):
            continue

        print(f"\nTesting notebook {notebook_file.name}")
        print("-" * len(f"Testing notebook {notebook_file.name}"))
        result = test_notebook(notebook_file, out_path, kernel, links, ignore_urls)
        if result:
            notebook_exec_errors.append(result)

    for notebook_err in notebook_exec_errors:
        for item in notebook_err:
            print(item)


def test_all_notebooks_mp(
    src_path: str,
    out_path: str,
    kernel: str = "python3",
    run_all: bool = False,
    links: bool = False,
    ignore_urls: List[str] = None,
):
    """
    Run notebooks.

    Parameters
    ----------
    src_path : str
        Source path for notebooks
    out_path : str
        Path to save failed notebooks
    kernel : str, optional
        Notebook kernel name, by default "python3"
    run_all : bool, optional
        Skip notebook exceptions, by default False
    links : bool, optional
        Test URIs in notebook, by default False
    ignore_urls : List[str]
        List of URLs to skip checks on.

    """
    # notebook_exec_errors: List[Tuple[str, str, str, str]] = []
    result_queue: Any = Queue()
    nbs_to_run = []
    for notebook_file in Path(src_path).glob("*.ipynb"):
        if str(notebook_file).casefold().endswith("-err.ipynb"):
            continue
        if not run_all and any(
            str(notebook_file).casefold().endswith(skip_file.casefold())
            for skip_file in _IGNORE_NBS
        ):
            continue
        nbs_to_run.append(notebook_file)

    test_notebook_task = partial(
        test_notebook_mp,
        queue=result_queue,
        out_path=out_path,
        kernel=kernel,
        links=links,
        ignore_urls=ignore_urls,
    )
    for index in range(0, len(nbs_to_run), 4):
        print(f"Running tasks {index} - {index + 4}")
        for notebook_task in nbs_to_run[index : index + 4]:  # noqa: E203
            Process(
                target=test_notebook_task, kwargs={"notebook_file": notebook_task}
            ).start()

    print("Processing finished, reading queue")
    print(result_queue.qsize())
    while result_queue:
        for item in result_queue.get():
            print(item)
    result_queue.close()


def test_notebook_mp(queue, *args, **kwargs):
    """Wrap test_notebook and add result to queue."""
    queue.put(test_notebook(*args, **kwargs))


# pylint: disable=too-many-locals
def test_notebook(
    notebook_file, out_path, kernel, links, ignore_urls
) -> Optional[NBError]:
    """
    Test single notebook.

    Parameters
    ----------
    notebook_file : str
        Source path for notebook
    out_path : str
        Path to save failed notebooks
    kernel : str, optional
        Notebook kernel name, by default "python3"
    links : bool, optional
        Test URIs in notebook, by default False
    ignore_urls : List[str]
        List of URLs to skip checks on.

    Returns
    -------
    NBError
        Error tuple

    """
    html_exporter = HTMLExporter(template_name="classic")
    if not os.environ.get("MSTICPY_TEST_IPSTACK"):
        os.environ["MSTICPY_SKIP_IPSTACK_TEST"] = "1"

    print(f"\nTesting notebook {notebook_file.name}")
    with open(notebook_file, "rb") as file_handle:
        nb_bytes = file_handle.read()
    nb_text = nb_bytes.decode("utf-8")
    nb_content = nbformat.reads(nb_text, as_version=4)
    exec_processor = ExecutePreprocessor(timeout=600, kernel_name=kernel)

    try:
        print(f"{notebook_file.name} - running notebook: ...")
        exec_processor.preprocess(
            nb_content, {"metadata": {"path": str(notebook_file.parent)}}
        )

    except CellExecutionError as cell_err:
        error_notebook = Path(out_path).joinpath(
            str(notebook_file.name).replace(".ipynb", "-err.ipynb")
        )
        msg = f"Error executing the notebook '{notebook_file.absolute()}'.\n"
        msg += f"See notebook '{error_notebook}' for the traceback."
        with open(error_notebook, mode="w", encoding="utf-8") as file_handle:  # type: ignore
            nbformat.write(nb_content, file_handle)
        return NBError(
            f"Error while running notebook {notebook_file}",
            cell_err.ename,
            cell_err.evalue,
            cell_err.args[0],
        )

    # convert to html if links check
    if links:
        print(f"{notebook_file.name} - checking notebook links: ...")
        html_body, _ = html_exporter.from_notebook_node(nb_content)
        page_errors = _check_notebook_links(
            html_body, ignore_urls, name=notebook_file.name
        )
        if page_errors:
            print(f"Unreachable links in source notebook: {notebook_file.absolute()}")
            for page in page_errors:
                print(page)

    if os.environ.get("MSTICPY_SKIP_IPSTACK_TEST"):
        del os.environ["MSTICPY_SKIP_IPSTACK_TEST"]

    return None


def _check_notebook_links(html_file, ignore_urls, name: str = None):
    """Check html links in the notebook."""
    results = check_html(html_file, ignore_uris=ignore_urls, doc_name=name)
    return _format_page_errors(name, results)


def _format_page_errors(notebook, results):
    page_errors = []
    for result_dict in results.values():
        page_errors.extend(
            NBError(notebook, result.status, result.url, None)
            for result in result_dict.values()
            if result.status == 404
        )
    return page_errors


def _add_script_args(description):
    parser = argparse.ArgumentParser(
        description=description, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--nbpath",
        "-n",
        required=True,
        help="Path to input notebook or folder of notebooks.",
    )
    parser.add_argument(
        "--outdir",
        "-o",
        default=".",
        help="Path to output folder to save error notebooks. Defaults to current directory.",
    )
    parser.add_argument(
        "--config",
        "-c",
        help="Path to a custom msticpyconfig.yaml to use.",
    )
    parser.add_argument(
        "--kernel",
        "-k",
        default="python3",
        help="The name of the IPython kernel to use.",
    )
    parser.add_argument(
        "--links",
        "-l",
        action="store_true",
        default=False,
        help="Test any that links in the notebooks are accessible.",
    )
    parser.add_argument(
        "--all",
        "-a",
        action="store_true",
        default=False,
        help="Include all notebooks - include those that need authentication and keys.",
    )
    parser.add_argument(
        "--mp",
        "-m",
        action="store_true",
        default=False,
        help="Run notebooks in multiprocessing mode.",
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args(description=__doc__)
    script_args = arg_parser.parse_args()

    ignore_uris_file = Path("tests/ignored_uri_links.txt")
    ignore_uris: List[str] = []
    if ignore_uris_file.is_file():
        ignore_uris = [
            line
            for line in ignore_uris_file.read_text(encoding="utf-8").split("\n")
            if line and not line.startswith("#")
        ]

    if Path(script_args.nbpath).is_dir():
        if script_args.mp:
            test_all_notebooks_mp(
                src_path=script_args.nbpath,
                out_path=script_args.outdir,
                kernel=script_args.kernel,
                links=script_args.links,
                ignore_urls=ignore_uris,
            )
        else:
            test_all_notebooks(
                src_path=script_args.nbpath,
                out_path=script_args.outdir,
                kernel=script_args.kernel,
                links=script_args.links,
                ignore_urls=ignore_uris,
            )
    elif Path(script_args.nbpath).is_file():
        test_notebook(
            notebook_file=script_args.nbpath,
            out_path=script_args.outdir,
            kernel=script_args.kernel,
            links=script_args.links,
            ignore_urls=ignore_uris,
        )
