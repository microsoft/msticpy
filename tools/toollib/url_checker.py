# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Python file import analyzer."""
from collections import defaultdict, namedtuple
from pathlib import Path
from typing import Dict, Set, Tuple, Optional, List
from urllib import parse

import markdown
import requests
from bs4 import BeautifulSoup

# pylint: disable=relative-beyond-top-level
from . import VERSION

__version__ = VERSION
__author__ = "Ian Hellen"


UrlResult = namedtuple("UrlResult", "status, history, url, message")


# pylint: disable=broad-except
def check_url(url: str) -> UrlResult:
    """
    Connect to URL and return response status.

    Parameters
    ----------
    url : str
        URL to check

    Returns
    -------
    UrlResult
        Tuple of status code, redirect history, requested url,
        status/error message.

    """
    try:
        resp = requests.get(url)
        report = str(resp.status_code)
        if resp.history:
            history_status_codes = [str(h.status_code) for h in resp.history]
            report += " [HISTORY: " + ", ".join(history_status_codes) + "]"
            result = UrlResult(
                resp.status_code, resp.history, url, "No error. Redirect to " + resp.url
            )
        elif resp.status_code == 200:
            result = UrlResult(
                resp.status_code, resp.history, url, "No error. No redirect."
            )
        else:
            result = UrlResult(resp.status_code, resp.history, url, "Error?")
    except Exception as err:
        result = UrlResult(0, [], url, err)
    return result


# pylint: enable=broad-except


PAGES_TO_SKIP = [
    "readthedocs.io/en/latest/genindex.html",
    "readthedocs.io/en/latest/_images/",
]


# pylint: disable=too-many-locals, too-many-branches
def check_site(  # noqa: MC0001
    page_url: str,
    all_links: bool = False,
    top_root: Optional[str] = None,
    checked_links: Dict[str, UrlResult] = None,
    visited_pages: Dict[str, Set[str]] = None,
) -> Tuple[Dict[str, UrlResult], Dict[str, Set[str]]]:
    """
    Recursively check URL page and child links.

    Parameters
    ----------
    page_url : str
        Url to check
    all_links : bool, optional
        Check all links, by default False - only check
        links to external locations
    top_root : Optional[str], optional
        Top level URL, by default None - page_url is taken
        as the top level URL.
    checked_links : Dict[str, UrlResult], optional
        Dictionary of links checked, by default None.
        This is used in recursive calls to child pages, not usually
        specified in initial call.
    visited_pages : Dict[str, Set[str]], optional
        Dictionary of pages visited, by default None.
        This is used in recursive calls to child pages, not usually
        specified in initial call.

    Returns
    -------
    Tuple[Dict[str, UrlResult], Dict[str, Set[str]]]
        Tuple of:
        1. Dictionary of checked links
        2. Dictionary of visited pages (url, set of referring pages)

    """
    if visited_pages is None:
        visited_pages = defaultdict(set)
    if checked_links is None:
        checked_links = {}
    if not top_root:
        top_root, _ = parse.urldefrag(page_url)
        if not top_root:
            return {}, {}

    print(f"\nPage: {page_url}")
    skip_pages = [pat for pat in PAGES_TO_SKIP if pat in page_url]
    if skip_pages:
        return {}, {}
    if page_url in visited_pages:
        print("already visited")
        return {}, {}

    resp = requests.get(page_url)
    if resp.headers["Content-Type"] != "text/html":
        print(resp.headers["Content-Type"])
        return {}, {}
    html = resp.content

    soup = BeautifulSoup(html, "html.parser")
    links = soup.find_all("a")

    page_links = {link.get("href") for link in links}
    print(
        f"visited_pages = {len(visited_pages)}",
        f"visited_links = {len(checked_links)}",
        f"{len(page_links)} links in current page",
    )
    for url_link in page_links:
        result, skip_next = _check_single_link(
            url_link, all_links, checked_links, page_url, top_root
        )
        if skip_next:
            continue
        if result:
            checked_links[url_link] = result
        if top_root.lower() in url_link.lower():
            # Intra site link - we may need to follow that
            child_page, _ = parse.urldefrag(url_link)
            if child_page not in visited_pages:
                child_results, _ = check_site(
                    page_url=child_page,
                    all_links=all_links,
                    top_root=top_root,
                    visited_pages=visited_pages,
                    checked_links=checked_links,
                )
                # record a reference from the parent page
                visited_pages[child_page].add(page_url)
                checked_links = {**checked_links, **child_results}
            else:
                print("v", end="")
    return (checked_links, visited_pages)


# pylint: enable=too-many-locals, too-many-branches


def _check_single_link(
    url_link: str,
    all_links: bool,
    checked_links: Dict[str, UrlResult],
    page_url: str,
    top_root: str,
) -> Tuple[Optional[UrlResult], bool]:
    result = None
    if url_link[0:4] == "http":
        if url_link in checked_links:
            print("_", end="")
            return result, True
        if all_links or (top_root.lower() not in url_link.lower()):
            print("u", end="")
            result = check_url(url_link)
    else:
        if url_link.startswith("#"):
            # don't check fragments
            print("#", end="")
            return result, True
        url_link = parse.urljoin(page_url, url_link)
        if url_link in checked_links:
            print("_", end="")
            return result, True
        if all_links:
            print("r", end="")
            result = check_url(url_link)
    return result, False


def check_html_doc(
    html_path: str,
    all_links: bool = False,
    top_root: str = None,
    checked_links: Dict[str, UrlResult] = None,
) -> Dict[str, UrlResult]:
    """
    Check links in an html document (file).

    Parameters
    ----------
    html_path : str
        Path to the file
    all_links : bool, optional
        Check all links, by default False - only check
        links to external locations
    top_root : Optional[str], optional
        Top level URL, by default None - page_url is taken
        as the top level URL.
    checked_links : Dict[str, UrlResult], optional
        Dictionary of links checked, by default None.
        This is used in recursive calls to child pages, not usually
        specified in initial call.

    Returns
    -------
    Dict[str, UrlResult]
        Dictionary of checked links from this file

    """
    if checked_links is None:
        checked_links = {}
    result_links: Dict[str, UrlResult] = {}

    print(f"\nPage: {html_path}")
    with open(html_path, "rb") as html_file:
        html = html_file.read()
    soup = BeautifulSoup(html, "html.parser")
    links = soup.find_all("a")

    page_links = {link.get("href") for link in links}
    print(
        f"visited_links = {len(checked_links)}",
        f"{len(page_links)} links in current page",
    )
    for url_link in page_links:
        result = None
        if url_link[0:4] == "http":
            if url_link in checked_links:
                print("_", end="")
                continue
            if all_links or not top_root or (top_root.lower() not in url_link.lower()):
                print("u", end="")
                result = check_url(url_link)
        if result:
            result_links[url_link] = result
    return result_links


def check_md_document(doc_path: str) -> Dict[str, UrlResult]:
    """
    Check links in Markdown document.

    Parameters
    ----------
    doc_path : str
        Path to the document

    Returns
    -------
    Dict[str, UrlResult]
        Dictionary of checked links

    """
    with open(doc_path, "r") as doc_file:
        body_markdown = doc_file.read()
    md_content = markdown.markdown(body_markdown)
    soup = BeautifulSoup(md_content, "html.parser")
    links = soup.find_all("a")

    page_links = {link.get("href") for link in links}

    checked_links: Dict[str, UrlResult] = {}
    print("Checking page...")
    for url_link in page_links:
        if url_link[0:4] == "http":
            if url_link in checked_links:
                print("_", end="")
                continue
            print("a", end="")
            result = check_url(url_link)
            checked_links[url_link] = result
        else:
            print("_", end="")
    print(
        f"visited_links = {len(checked_links)}",
        f"{len(page_links)} links in current page",
    )
    _print_url_results(list(checked_links.values()))
    return checked_links


def _print_url_results(results: List[UrlResult]):
    """
    Print results of any URLs that did not return 200 status.

    Parameters
    ----------
    results : List[UrlResult]
        List of URLs checks to print.

    """
    # Sort by status and then by history length
    results.sort(key=lambda result: (result.status, len(result.history)))

    print("\n\nResults")
    print("=========")
    # 301s - may want to clean up 301s if you have multiple redirects
    print("Redirect 301s")
    i = 0
    for result in results:
        if result.history:
            i += 1
            print(i, end=". ")
            for response in result.history:
                print(">>", response.url, end="\n\t")
            print(">>>>", result[3])

    # non-200s
    print("\n==========\nERRORS")
    for result in results:
        if result.status != 200:
            print(result.status, "-", result.url)


def check_site_links(
    start_url: str, all_links: bool = False
) -> Tuple[Dict[str, UrlResult], Dict[str, Set[str]]]:
    """
    Check a URL and child pages for broken links.

    Parameters
    ----------
    start_url : [type]
        Starting URL
    all_links : bool, optional
        Check all links, by default False - only check
        links to external locations

    Returns
    -------
    Tuple[Dict[str, UrlResult], Dict[str, Set[str]]]
        Tuple of:
        1. Dictionary of checked links
        2. Dictionary of visited pages (url, set of referring pages)

    """
    print(
        "Checks: u = full url, # = fragment (skipped)",
        "r = relative linkv = child page",
        "_ = skipped (duplicate)",
    )
    checked_links, visits = check_site(start_url, all_links=all_links)
    _print_url_results(list(checked_links.values()))
    return checked_links, visits


def check_html_docs(
    doc_path: str, recurse: bool = True
) -> Dict[str, Dict[str, UrlResult]]:
    """
    Check multiple HTML files in `doc_path`.

    Parameters
    ----------
    doc_path : str
        Path
    recurse: bool
        If True, recurse subfolders, default is True

    Returns
    -------
    Dict[str, Dict[str, UrlResult]]
        Dictionary of pages checked. Results for each page
        is a dictionary of checked links for the page.

    """
    if recurse:
        glob_pattern = "**/*.html"
    else:
        glob_pattern = "*.html"
    html_files = list(Path(doc_path).glob(glob_pattern))
    checked_pages: Dict[str, Dict[str, UrlResult]] = {}
    checked_links: Dict[str, UrlResult] = {}
    for html_file in html_files:
        pg_links = check_html_doc(str(html_file), checked_links=checked_links)
        checked_links = {**checked_links, **pg_links}
        page = str(html_file.relative_to(Path(doc_path)))
        checked_pages[page] = pg_links
    _print_url_results(list(checked_links.values()))
    return checked_pages
