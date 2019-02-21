# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for IoCExtract class."""

import re
from collections import namedtuple, defaultdict
from urllib.parse import unquote

import pandas as pd
from .. nbtools.utility import export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'


def _compile_regex(regex):

    return re.compile(regex, re.I | re.X | re.M)

IoCPattern = namedtuple('IoCPattern', ['ioc_type', 'comp_regex', 'priority'])


@export
class IoCExtract(object):
    """
    IoC Extractor - looks for common IoC patterns in input strings.

    The extract() method takes either a string or a pandas DataFrame
    as input. When using the string option as an input extract will
    return a dictionary of results. When using a DataFrame the results
    will be returned as a new DataFrame with the following columns:
    IoCType: the mnemonic used to distinguish different IoC Types
    Observable: the actual value of the observable
    SourceIndex: the index of the row in the input DataFrame from
    which the source for the IoC observable was extracted.

    The class has a number of built-in IoC regex definitions.
    These can be retrieved using the ioc_types attribute.

    Addition IoC definitions can be added using the add_ioc_type
    method.

    Note: due to some ambiguity in the regular expression patterns
    for different types and observable may be returned assigned to
    multiple observable types. E.g. 192.168.0.1 is a also a legal file
    name in both Linux and Windows. Linux file names have a particularly
    large scope in terms of legal characters so it will be quite common
    to see other IoC observables (or parts of them) returned as a
    possible linux path.
    """

    IPV4_REGEX = r'(?P<ipaddress>(?:[0-9]{1,3}\.){3}[0-9]{1,3})'
    IPV6_REGEX = r'(?<![:.\w])(?:[A-F0-9]{1,4}:){7}[A-F0-9]{1,4}(?![:.\w])'
    DNS_REGEX = r'((?=[a-z0-9-]{1,63}\.)[a-z0-9]+(-[a-z0-9]+)*\.){2,}[a-z]{2,63}'
    # dns_regex = '\\b((?=[a-z0-9-]{1,63}\\.)[a-z0-9]+(-[a-z0-9]+)*\\.){2,}[a-z]{2,63}\\b'

    URL_REGEX = r'''
            (?P<protocol>(https?|ftp|telnet|ldap|file)://)
            (?P<userinfo>([a-z0-9-._~!$&\'()*+,;=:]|%[0-9A-F]{2})*@)?
            (?P<host>([a-z0-9-._~!$&\'()*+,;=]|%[0-9A-F]{2})*)
            (:(?P<port>\d*))?
            (/(?P<path>([^?\# ]|%[0-9A-F]{2})*/?))?
            (\?(?P<query>([a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9A-F]{2})*))?
            (\#(?P<fragment>([a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9A-F]{2})*))?'''

    WINPATH_REGEX = r'''
            (?P<root>[a-z]:|\\\\[a-z0-9_.$-]+||[.]+)
            (?P<folder>\\(?:[^\/:*?"\'<>|\r\n]+\\)*)
            (?P<file>[^\\/*?""<>|\r\n ]+)'''
    # Linux simplified - this ignores some legal linux paths avoid matching too much
    # TODO - also matches URLs!
    LXPATH_REGEX = r'''(?P<root>/+||[.]+)
            (?P<folder>/(?:[^\\/:*?<>|\r\n]+/)*)
            (?P<file>[^/\0<>|\r\n ]+)'''

    MD5_REGEX = r'(?:^|[^A-Fa-f0-9])(?P<hash>[A-Fa-f0-9]{32})(?:$|[^A-Fa-f0-9])'
    SHA1_REGEX = r'(?:^|[^A-Fa-f0-9])(?P<hash>[A-Fa-f0-9]{40})(?:$|[^A-Fa-f0-9])'
    SHA256_REGEX = r'(?:^|[^A-Fa-f0-9])(?P<hash>[A-Fa-f0-9]{64})(?:$|[^A-Fa-f0-9])'

    _content_regex = {}

    def __init__(self):
        """Intialize new instance of IoCExtract."""
        # IP Addresses
        self.add_ioc_type('ipv4', self.IPV4_REGEX, 0)
        self.add_ioc_type('ipv6', self.IPV6_REGEX, 0)

        # Dns Domains
        # TODO - This also matches IP addresses
        self.add_ioc_type('dns', self.DNS_REGEX, 1)

        # Http requests
        self.add_ioc_type('url', self.URL_REGEX, 0)

        # File paths
        # Windows
        self.add_ioc_type('windows_path', self.WINPATH_REGEX, 2)

        self.add_ioc_type('linux_path', self.LXPATH_REGEX, 2)

        # MD5, SHA1, SHA256 hashes
        self.add_ioc_type('md5_hash', self.MD5_REGEX, 1)
        self.add_ioc_type('sha1_hash', self.SHA1_REGEX, 1)
        self.add_ioc_type('sha256_hash', self.SHA256_REGEX, 1)

    # Public members

    def add_ioc_type(self, ioc_type: str, ioc_regex: str, priority: int = 0):
        """
        Add an IoC type and regular expression to use to the built-in set.

        Note: adding an ioc_type that exists in the internal set will overwrite that item
        Regular expressions are compiled with re.I | re.X | re.M (Ignore case, Verbose
        and MultiLine)
            :param: ioc_type - a unique name for the IoC type
            :param: ioc_regex - a regular expression used to search for the type
            :type ioc_type: str
            :type ioc_regex: str
        """
        if ioc_type is None or ioc_type.strip() is None:
            raise Exception('No value supplied for ioc_type parameter')
        if ioc_regex is None or ioc_regex.strip() is None:
            raise Exception('No value supplied for ioc_regex parameter')

        self._content_regex[ioc_type] = IoCPattern(ioc_type=ioc_type,
                                                   comp_regex=_compile_regex(regex=ioc_regex),
                                                   priority=priority)

    @property
    def ioc_types(self) -> dict:
        """
        Return the current set of IoC types and regular expressions.

            :rtype: dict of IoC Type names and regular expressions
        """
        return self._content_regex

    def extract(self, src: str = None, data: pd.DataFrame = None,
                columns: list = None, os_family='Windows',
                ioc_types: list = None):
        """
        Extract IoCs from either a string or pandas DataFrame.

        Keyword Arguments:
            src {str} --  source string in which to look for IoC patterns
                (default: {None})
            data {pd.DataFrame} -- input DataFrame from which to read source strings
                 (default: {None})
            columns {list} -- The list of columns to use as source strings,
                if the data parameter is used.  (default: {None})
            os_family {str} -- 'Linux' or 'Windows' (default: {'Windows'})
            ioc_types {list({str})} -- Restrict matching to just specified
                types (default: {None})


        Returns:
            dict of found observables (if input is a string) or
            DataFrame of observables

        Extract takes either a string or a pandas DataFrame as input.
        When using the string option as an input extract will
        return a dictionary of results.
        When using a DataFrame the results will be returned as a new
        DataFrame with the following columns:
        - IoCType: the mnemonic used to distinguish different IoC Types
        - Observable: the actual value of the observable
        - SourceIndex: the index of the row in the input DataFrame from
        which the source for the IoC observable was extracted.

        """
        if src and src.strip():
            return self._scan_for_iocs(src, os_family)

        if data is None:
            raise Exception('No source data was supplied to extract')

        # Handle DataFrame option
        assert isinstance(data, pd.DataFrame)

        if columns is None:
            raise Exception(
                'No values where supplied for the columns parameter')

        col_set = set(columns)
        if not col_set <= set(data.columns):
            missing_cols = [elem for elem in col_set if elem not in data.colums]
            raise Exception('Source column(s) {} not found in supplied DataFrame'
                            .format(', '.join(missing_cols)))

        result_columns = ['IoCType', 'Observable', 'SourceIndex']
        result_frame = pd.DataFrame(columns=result_columns)
        for idx, datarow in data.iterrows():
            for col in columns:
                ioc_results = self._scan_for_iocs(datarow[col], os_family, ioc_types)
                for result_type, result_set in ioc_results.items():
                    if result_set:
                        for observable in result_set:
                            result_row = pd.Series(
                                data=[result_type, observable, idx], index=result_columns)
                            result_frame = result_frame.append(
                                result_row, ignore_index=True)

        return result_frame

    def validate(self, input_str: str, ioc_type: str) -> bool:
        """
        Return true if the input_str matches the corresponding regex.

        Arguments:
            :input_str str: the string to test
            :ioc_type str: the regex pattern to use

        Returns:
            bool - True if match.

        """
        if ioc_type not in self._content_regex:
            raise KeyError('Unknown type {}. Valid types are: {}'
                           .format(ioc_type, list(self._content_regex.keys())))
        rgx = self._content_regex[ioc_type]
        return rgx.comp_regex.fullmatch(input_str) is not None

    # Private methods
    def _scan_for_iocs(self, src: str, os_family: str, ioc_types: list = None) -> dict:
        """Return IoCs found in the string."""
        ioc_results = defaultdict(set)
        iocs_found = {}

        for (ioc_type, rgx_def) in self._content_regex.items():
            if ioc_types and ioc_type not in ioc_types:
                continue

            if os_family == 'Linux' and rgx_def.ioc_type == 'windows_path':
                continue
            elif os_family == 'Windows' and rgx_def.ioc_type == 'linux_path':
                continue

            match_pos = 0
            for rgx_match in rgx_def.comp_regex.finditer(src, match_pos):
                if rgx_match is not None:
                    self._add_highest_pri_match(iocs_found,
                                                rgx_match.group(),
                                                rgx_def)
                    if ioc_type == 'url':
                        decoded_url = unquote(rgx_match.group())
                        for url_match in rgx_def.comp_regex.finditer(decoded_url, match_pos):
                            if url_match is not None:
                                self._add_highest_pri_match(iocs_found,
                                                            url_match.group(),
                                                            rgx_def)
                                self._add_highest_pri_match(iocs_found,
                                                            url_match.groupdict()['host'],
                                                            self._content_regex['dns'])
                    match_pos = rgx_match.end()
                else:
                    break
        for ioc, ioc_result in iocs_found.items():
            ioc_results[ioc_result[0]].add(ioc)

        return ioc_results

    def _add_highest_pri_match(self, iocs_found: dict, current_match: str, current_def: IoCPattern):
        # if we already found a match for this item and the previous
        # ioc type is more specific then don't add this to the results
        if current_match in iocs_found and current_def.priority > iocs_found[current_match][1]:
            return
        else:
            iocs_found[current_match] = (current_def.ioc_type, current_def.priority)
