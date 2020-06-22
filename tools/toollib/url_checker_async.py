# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
from collections import defaultdict, namedtuple
from pathlib import Path
from typing import Dict, Set, Optional, Iterable
from urllib import parse

import asyncio
import markdown
from bs4 import BeautifulSoup
from aiohttp import ClientSession, ClientResponseError, ClientConnectionError


# pylint: disable=relative-beyond-top-level
# from . import VERSION

# __version__ = VERSION
__author__ = "Ian Hellen"


UrlResult = namedtuple("UrlResult", "status, history, url, message")


def check_docs(
    doc_path: str, recurse: bool = True, max_threads: int = 10, delay: float = 0
) -> Dict[str, Dict[str, UrlResult]]:
    """
    Check multiple HTML files in `doc_path`.

    Parameters
    ----------
    doc_path : str
        Path
    recurse: bool
        If True, recurse subfolders, default is True
    max_threads: int, optional
        The maximum number of async threads to run
    delay: float, optional
        Seconds delay between requests

    Returns
    -------
    Dict[str, Dict[str, UrlResult]]
        Dictionary of pages checked. Results for each page
        is a dictionary of checked links for the page.

    """
    page_results: Dict[str, Dict[str, UrlResult]] = defaultdict(dict)
    link_results: Dict[str, UrlResult] = {}

    links_to_check = _get_links_from_files(doc_path, recurse)
    print(f"Checking links {len(links_to_check)}...")
    checked_links = check_uris(links_to_check, max_threads, delay)
    print("\ndone")
    for result in checked_links:
        link_results[result.url] = result
        src_pages = links_to_check[result.url]
        for src_page in src_pages:
            page_results[src_page][result.url] = result

    _print_url_results(page_results)
    return page_results


# pyline: disable=broad-except
def _get_links_from_files(doc_path: str, recurse: bool = True) -> Dict[str, Set[str]]:
    links_to_check: Dict[str, Set[str]] = defaultdict(set)

    html_glob_pattern = "**/*.html" if recurse else "*.html"
    all_files = list(Path(doc_path).glob(html_glob_pattern))
    md_glob_pattern = "**/*.md" if recurse else "*.md"
    md_files = list(Path(doc_path).glob(md_glob_pattern))
    all_files.extend(md_files)
    print(f"reading {len(all_files)} files...")
    for file_name in all_files:
        pg_links = _get_doc_links(file_name)
        page = str(file_name.relative_to(Path(doc_path)))
        for link in pg_links:
            links_to_check[link].add(page)

    return links_to_check


def _get_doc_links(doc_path: Path) -> Set[str]:
    """
    Check links in an HTML or Markdown document.

    Parameters
    ----------
    doc_path : str
        Path to the document

    Returns
    -------
    Set[str]
        Set of links

    """
    html_content = None
    try:
        html_content = doc_path.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        html_content = doc_path.read_text(encoding="mbcs")
    if doc_path.suffix.casefold() == ".md":
        html_content = markdown.markdown(html_content)
    soup = BeautifulSoup(html_content, "html.parser")
    links = soup.find_all("a")

    links = {link.get("href") for link in links}
    links = {link for link in links if link.casefold().startswith("http")}
    return links


def _resolve_rel_link(
    url_link: str, all_links: bool, page_url: str, top_root: str
) -> Optional[str]:
    if url_link[0:4] == "http":
        if all_links or (top_root.lower() not in url_link.lower()):
            return url_link
    else:
        if url_link.startswith("#"):
            # don't follow fragments
            return None
        url_link = parse.urljoin(page_url, url_link)
        if all_links:
            return url_link
    return None


def check_uris(
    uris_to_check: Iterable[str], max_threads: int = 10, delay: float = 0
) -> Iterable[UrlResult]:
    """
    Check URIs.

    Parameters
    ----------
    uris_to_check : Iterable[str]
        Iterable of URI strings
    max_threads: int, optional
        The maximum number of async threads to run
    delay: float, optional
        Seconds delay between requests

    Returns
    -------
    Iterable[UrlResult]
        Iterable of UrlResults

    """
    loop = asyncio.get_event_loop()

    future = asyncio.ensure_future(_check_uris_async(uris_to_check, max_threads, delay))
    return loop.run_until_complete(future)


async def _check_url_async(url: str, session: ClientSession) -> UrlResult:
    """
    Connect to URL and return response status.

    Parameters
    ----------
    url : str
        URL to check
    session : ClientSession
        aiohttp client session

    Returns
    -------
    UrlResult
        Tuple of status code, redirect history, requested url,
        status/error message.

    """
    try:
        async with session.get(url) as resp:
            try:
                await resp.read()
                if resp.history:
                    result = UrlResult(
                        resp.status,
                        resp.history,
                        url,
                        "No error. Redirect to " + str(resp.url),
                    )
                elif resp.status == 200:
                    result = UrlResult(
                        resp.status, resp.history, url, "No error. No redirect."
                    )
                else:
                    result = UrlResult(resp.status, resp.history, url, "Error?")
            except ClientResponseError as client_err:
                return UrlResult(client_err.status, [], url, client_err)
    except ClientConnectionError as err:
        result = UrlResult(404, [], url, err)
    return result


async def _check_uri_with_sem_async(sem, url, session) -> Iterable[UrlResult]:
    # Getter function with semaphore.
    async with sem:
        return await _check_url_async(url, session)


async def _check_uris_async(
    links_to_check: Iterable[str], max_threads: int = 10, delay: float = 0
) -> Iterable[UrlResult]:
    tasks = []
    # create instance of Semaphore
    sem = asyncio.Semaphore(max_threads)

    # Create client session that will ensure we dont open new connection
    # per each request.
    async with ClientSession() as session:
        for uri in links_to_check:
            if delay:
                asyncio.sleep(delay)
            # pass Semaphore and session to every GET request
            task = asyncio.ensure_future(_check_uri_with_sem_async(sem, uri, session))
            tasks.append(task)

        results = await asyncio.gather(*tasks)
        return results


def _print_url_results(results: Dict[str, Dict[str, UrlResult]]):
    """
    Print results of any URLs that did not return 200 status.

    Parameters
    ----------
    results : Dict[str, Dict[str, UrlResult]]
        List of URLs checks to print.

    """
    print("\n\nResults")

    # non-200s
    print("\n==========\nERRORS")
    for page, result_dict in results.items():
        page_errors = []
        for result in result_dict.values():
            if result.status != 200:
                page_errors.append(f"{result.status} - {result.url}")
        if page_errors:
            print(f"Document {page}")
            for err in page_errors:
                print(err)


# if __name__ == "__main__":
#     t_results = check_docs("..//..")
