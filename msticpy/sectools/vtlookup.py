# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Module for VTLookup class."""

import json
from json import JSONDecodeError
import math
import re
from collections import namedtuple, Counter
from ipaddress import IPv4Address, ip_address

import pandas as pd
import requests
from urllib3.exceptions import LocationParseError
from urllib3.util import parse_url

from . iocextract import IoCExtract
from .. nbtools.utility import export, pd_version_23
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

# VirusTotal parameter collection
VTParams = namedtuple('VTParams',
                      ['api_type', 'batch_size', 'batch_delimiter',
                       'http_verb', 'api_var_name', 'headers'])

DuplicateStatus = namedtuple('DuplicateStatus', ['is_dup', 'status'])
PreProcessResult = namedtuple('PreProcessResult', ['observable', 'status'])


@export
class VTLookup:
    """
    VTLookup: VirusTotal lookup of IoC reports.

    Main methods are:
    lookup_iocs() - accepts input of multiple IoCs in a Pandas DataFrame
    lookup_ioc() - looks up a single IoC observable.
    supported_ioc_types - a list of valid target types.
    ioc_vt_type_mapping - a dictionary of mappings to recognized VT Types.
    Types mapped to None will not be submitted to VT.

    For urls a full http request can be submitted, query string and fragments will be
    dropped before submitting.
    For files MD5, SHA1 and SHA256 hashes are supported.
    For IP addresses only dotted IPv4 addresses are supported.
    """

    # Ioc types that we support
    _SUPPORTED_INPUT_TYPES = ['ipv4', 'dns', 'url', 'md5_hash',
                              'sha1_hash', 'sh256_hash']

    # Mapping to to VT Types
    _VT_TYPE_MAP = {'ipv4': 'ip-address',
                    'ipv6': None,
                    'dns': 'domain',
                    'url': 'url',
                    'md5_hash': 'file',
                    'sha1_hash': 'file',
                    'sh256_hash': 'file'}

    # VT API parameters
    _HDR_GZIP = {"Accept-Encoding": "gzip, deflate"}
    _VT_API = 'https://www.virustotal.com/vtapi/v2/{type}/report'
    _VT_API_TYPES = {'url': VTParams('url', 1, '\n', 'post', 'resource', _HDR_GZIP),
                     'file': VTParams('file', 25, ',', 'post', 'resource', _HDR_GZIP),
                     'ip-address': VTParams('ip-address', 1, '', 'get', 'ip', None),
                     'domain': VTParams('domain', 1, '', 'get', 'domain', None)}

    _RESULT_COLUMNS = ['Observable', 'IoCType', 'Status', 'ResponseCode', 'RawResponse',
                       'Resource', 'SourceIndex', 'VerboseMsg', 'Resource', 'ScanId',
                       'Permalink', 'Positives', 'MD5', 'SHA1', 'SHA256',
                       'ResolvedDomains', 'ResolvedIPs', 'DetectedUrls']

    def __init__(self, vtkey, verbosity=1):
        """
        Create a new instance of VTLookup class.

            :param vtkey: VirusTotal API key
            :param verbosity: The level of detail of reporting
                0 = no reporting
                1 = minimal reporting (default)
                2 = verbose reporting
        """
        self._vtkey = vtkey
        self._verbosity = verbosity
        self._ioc_custom_type_map = {}

        self._ioc_extract = IoCExtract()

        # create a data frame to store the results
        self.results = pd.DataFrame(data=None, columns=self._RESULT_COLUMNS)

    @property
    def supported_ioc_types(self) -> list({str}):
        """Return list of supported IoC type internal names."""
        return self._SUPPORTED_INPUT_TYPES

    @property
    def supported_vt_types(self) -> list({str}):
        """Return list of VirusTotal supported IoC type names."""
        return list(self._VT_API_TYPES.keys())

# flake8: noqa: D102
    @property
    def ioc_vt_type_mapping(self) -> dict({str: str}):
        """Return mapping between internal and VirusTotal IoC type names."""
        return self._VT_TYPE_MAP

    def lookup_iocs(self, data: pd.DataFrame, src_col: str = 'Observable',
                    type_col: str = 'IoCType', src_index_col: str = 'SourceIndex',
                    **kwargs) -> pd.DataFrame:
        """
        lookup_iocs: main lookup method.

        Tries to retrieve results for IoC observables in the source dataframe.

            :param data: dataframe containing the observables to search for
            :param src_col: the column name that contains the observable data
                (one item per row)
            :param type_col: the column name containing the observable type
            :param source_index: the name of the column to use as source index. If not
                specified this defaults to 'SourceIndex'. If this (or the supplied value)
                is not in the source dataframe the index of the source dataframe will
                be used. This is retained in the output so that you can join the results
                back to the original data.
            :param kwargs: key/value pairs of additional mappings to supported IoC type names
                e.g. ipv4='ipaddress', url='httprequest'. This allows you to specify custom
                mappings when the source data is tagged with different names.

        Returns:
            pd.DataFrame: VT Results

        See supported_ioc_types attribute for a list of valid target types.
        Not all of these types are supported by VirusTotal. See ioc_vt_type_mapping for
        current mappings. Types mapped to None will not be submitted to VT.

        For urls a full http request can be submitted, query string and fragments will be
        dropped before submitting. Other supported protocols are ftp, telnet, ldap, file
        For files MD5, SHA1 and SHA256 hashes are supported.
        For IP addresses only dotted IPv4 addresses are supported.

        """
        # if the caller has supplied alternative type name mappings add any of these
        # to our lookup dictionary
        for k in self._get_supported_vt_ioc_types():
            self._ioc_custom_type_map[k] = k
        for k, val in kwargs.items():
            if k in self._get_supported_vt_ioc_types():
                self._ioc_custom_type_map[k] = val

        if src_index_col not in data:
            src_index_col = None

        # for each ioc_type, retrieve observables from dataframe
        for ioc_type, mapped_type in self._ioc_custom_type_map.items():
            input_df = data[data[type_col] == mapped_type]
            self._lookup_ioc_type(input_df, ioc_type, src_col, src_index_col)

        self._print_status(
            'Submission complete. {} responses from {} input rows'
            .format(len(self.results), len(data)), 2)

        return self.results

    def lookup_ioc(self, observable: str, ioc_type: str, output: str = 'dict'):
        """
        Look up and single IoC observable.

            :param observable: The observable value
            :param ioc_type: The IoC Type (see 'supported_ioc_types' attribute)
            :param output='dict': Output results as a dictionary (or list of dicts)
                if output is any other value the result will be returned in a
                Pandas DataFrame

            Returns:
                list{dict}: if output == 'dict'
                pd.DataFrame: otherwise

        """
        # Check input
        if (observable is None or observable.strip() is None or
                ioc_type is None or ioc_type.strip() is None):
            raise SyntaxError("Invalid value for observable or ioc_type")

        observable, status = self._preprocess_observable(observable, ioc_type)
        if observable is None:
            raise SyntaxError(
                '{} for observable value {}'.format(status, observable), 1)

        if ioc_type not in self._VT_TYPE_MAP:
            raise LookupError('IoC Type {} not recognized. Valid types are [{}]'.format(
                ioc_type, ', '.join(self.supported_ioc_types)))

        if self._VT_TYPE_MAP[ioc_type] not in self._VT_API_TYPES:
            vt_types = {
                k for k, val in self.ioc_vt_type_mapping if val is not None}
            err = 'IoC Type {} is recognized by VirusTotal. Valid types are [{}]'.format(
                ioc_type, ', '.join(vt_types))
            raise LookupError(err)

        # do the submission
        vt_param = self._VT_API_TYPES[self._VT_TYPE_MAP[ioc_type]]
        results = self._vt_submit_request(observable, vt_param)
        self._parse_vt_results(results, observable, ioc_type)

        # return as a list of dictionaries or a DataFrame
        if output == 'dict':
            list_res = self.results.apply(
                lambda x: x.to_dict(), axis=1).tolist()
            return list_res[0] if len(list_res) == 1 else list_res
        else:
            return self.results

    def _lookup_ioc_type(self, input_frame: pd.DataFrame, ioc_type: str, src_col: str,
                         src_index_col: str):
        """
        Perform the VT submission of a set of IoCs of a given type.

            :param self:
            :param input_frame: the input dataframe
            :param ioc_type: the IoC Type to submit
            :param src_col: The name column in the dataframe
                containing the IoC observables
            :para source_index_col
        """
        assert(ioc_type in self._VT_TYPE_MAP and
               self._VT_TYPE_MAP[ioc_type] in self._VT_API_TYPES)
        vt_param = self._VT_API_TYPES[self._VT_TYPE_MAP[ioc_type]]

        # Some types support batch lookups so we can assemble them into batches
        # for the moment we are only supporting
        source_row_index = {}
        obs_batch = []
        batch_index = 0
        row_num = 0
        row_count = len(input_frame)
        if src_index_col:
            src_cols = [src_col, src_index_col]
        else:
            src_cols = [src_col]

        for idx, row in input_frame[src_cols].iterrows():
            row_num += 1
            observable = row[src_col]

            # Use the user-specified index if possible
            if src_index_col:
                idx = row[src_index_col]

            # validate the observable to avoid sending too much junk to VT
            pp_observable = self._validate_observable(observable, ioc_type, idx)

            # if the observable is valid, add it to the submission batch
            if pp_observable.observable:
                obs_batch.append(pp_observable.observable)
                source_row_index[pp_observable.observable] = idx
                batch_index += 1

            # We want to trigger in the following circumstances
            # 1. if the length of our batch is at the max VT batchsize for this type
            #   (If the batch size is 1 this will fire for every row)
            # 2. Or we have reached the end of our row iteration
            # AND
            # 3. The batch is not empty
            if ((len(obs_batch) == vt_param.batch_size or row_num == row_count) and
                    obs_batch):
                obs_submit = vt_param.batch_delimiter.join(obs_batch)

                self._print_status(
                    'Submitting observables: "{}", type "{}" to VT. (Source index {})'
                    .format(obs_submit, ioc_type, idx), 2)
                # Submit the request
                results, status_code = self._vt_submit_request(
                    obs_submit, vt_param)

                if status_code != 200:
                    # Print status messages and add failure cases to results
                    status = 'Failed submission: http error {}'.format(status_code)
                    for failed_obs in obs_batch:
                        self._add_invalid_input_result(failed_obs, ioc_type, status,
                                                       source_row_index[failed_obs])
                        self._print_status(
                            'Error in response submitting observables: "{}", type "{}" '
                            'http status is {}. Response: {} (Source index {})'
                            .format(obs_submit, ioc_type, status_code, results,
                                    source_row_index[failed_obs]), 1)
                else:
                    # parse the results from the response
                    self._parse_vt_results(
                        results, obs_submit, ioc_type, idx, source_row_index, vt_param)

                # reset index of batch
                batch_index = 0
                obs_batch = []

    def _parse_vt_results(self, vt_results: any, observable: str, ioc_type: str,
                          source_idx: any = 0,
                          source_row_index: any = None, vt_param: VTParams = None):
        """
        Parse VirusTotal results based on IoCType.

            :param vt_results: Raw results from VT
            :param observable: The observable or observable batch
            :param ioc_type: The IoC type of the observables
            :param source_idx: The row index of the source frame
            :param source_row_index: (batch only) Mapping between observable item
                and row index of the source
            :param vt_param: (batch only) the VTParams tuple for this submission
        """
        results_to_parse = []
        if isinstance(vt_results, str):
            try:
                vt_results = json.loads(vt_results, strict=False)
            except (JSONDecodeError, TypeError):
                pass

        if isinstance(vt_results, list) and vt_param.batch_size > 1:
            # multiple results
            results_to_parse = vt_results
        elif isinstance(vt_results, dict):
            # single result
            results_to_parse.append(vt_results)
        else:
            self._print_status('Error parsing response to JSON: "{}", type "{}". (Source index {})'
                               .format(observable, ioc_type, source_idx), 1)

        if vt_param and vt_param.batch_delimiter:
            observables = observable.split(vt_param.batch_delimiter)
        else:
            observables = [observable]

        # pylint: disable=locally-disabled, C0200
        for result_idx in range(0, len(results_to_parse)):
            df_dict_vtresults = self._parse_single_result(results_to_parse[result_idx], ioc_type)

            # Add remaining fields from source
            df_dict_vtresults['IoCType'] = ioc_type
            df_dict_vtresults['Status'] = 'Success'
            df_dict_vtresults['RawResponse'] = json.dumps(results_to_parse[result_idx])
            if len(results_to_parse) == 1 or source_row_index is None or len(source_row_index) == 1:
                df_dict_vtresults['Observable'] = observable
                df_dict_vtresults['SourceIndex'] = source_idx
            else:
                # If we submitted multiple values in a batch
                # we assume (hope) that the ordering of the response is the same
                # as in the request. We try our best to remarry the observable
                # and source index
                if 'resource' in results_to_parse[result_idx]:
                    vt_resource = results_to_parse[result_idx]['resource']
                    df_dict_vtresults['Observable'] = vt_resource
                    if vt_resource in source_row_index:
                        df_dict_vtresults['SourceIndex'] = source_row_index[vt_resource]
                    else:
                        df_dict_vtresults['SourceIndex'] = source_row_index[observables[result_idx]]
                else:
                    df_dict_vtresults['Observable'] = observables[result_idx]
                    df_dict_vtresults['SourceIndex'] = source_row_index[observables[result_idx]]

            if pd_version_23():
                new_results = pd.concat(
                    objs=[self.results, df_dict_vtresults], ignore_index=True, sort=False, axis=0)
            else:
                new_results = pd.concat(
                    objs=[self.results, df_dict_vtresults], ignore_index=True, axis=0)

            self.results = new_results
        # pylint enable=locally-disabled, C0200

    def _parse_single_result(self, results_dict: dict({str: any}), ioc_type: str) -> pd.DataFrame:
        """
        Parse VirusTotal single result based on IoCType.

            :param results_dict: Raw results dictionary from VT
            :param ioc_type: The IoC type of the observables
        """
        # create output frame and parse results to intermediate frame
        df_dict_vtresults = {}

        # Parse returned results to our output dataframe depending
        # on the IoC type
        if ioc_type in ['url', 'md5_hash', 'sha1_hash', 'sha256_hash']:
            df_dict_vtresults['ResponseCode'] = results_dict['response_code']
            df_dict_vtresults['VerboseMsg'] = results_dict['verbose_msg']
            df_dict_vtresults['ScanId'] = results_dict['scan_id']
            df_dict_vtresults['Resource'] = results_dict['resource']
            df_dict_vtresults['Permalink'] = results_dict['permalink']
            df_dict_vtresults['Positives'] = results_dict['positives']
            if ioc_type in ['md5_hash', 'sha1_hash', 'sha256_hash']:
                df_dict_vtresults['MD5'] = results_dict['md5']
                df_dict_vtresults['SHA1'] = results_dict['sha1']
                df_dict_vtresults['SHA256'] = results_dict['sha256']

        if ioc_type == 'ipv4' or ioc_type == 'dns':
            df_dict_vtresults['ResponseCode'] = results_dict['response_code']
            df_dict_vtresults['VerboseMsg'] = results_dict['verbose_msg']
            # dns and ipv4 have multi-valued 'resolutions' and 'detected_urls' lists
            # of dictionaries
            # This leads to a few horrendous-looking list comprehensions
            # These are essentially pulling out the columns that contain these lists.
            # then using a list comprehension to pull out the value, where the key 'k'
            # is of the required value
            if ioc_type == 'ipv4' and 'resolutions' in results_dict:
                item_list = [item['hostname']
                             for item in results_dict['resolutions'] if 'hostname' in item]
                df_dict_vtresults['ResolvedDomains'] = ', '.join(item_list)
            elif ioc_type == 'dns' and 'resolutions' in results_dict:
                item_list = [item['ip_address']
                             for item in results_dict['resolutions'] if 'ip_address' in item]
                df_dict_vtresults['ResolvedIPs'] = ', '.join(item_list)
            if 'detected_urls' in results_dict:
                item_list = [item['url']
                             for item in results_dict['detected_urls'] if 'url' in item]
                df_dict_vtresults['DetectedUrls'] = ', '.join(item_list)
                # positives are listed per detected_url so we need to pull those our and sum them.
                positives = sum([item['positives']
                                 for item in results_dict['detected_urls']
                                 if 'positives' in item])
                df_dict_vtresults['Positives'] = positives

        return pd.DataFrame(data=df_dict_vtresults, columns=self._RESULT_COLUMNS, index=[0])

    def _validate_observable(self, observable: str, ioc_type: str, idx: any):
        """
        Validate observable for format and duplicates of existing results.

        Arguments:
            :param observable:str: The observable to be checked
            :param ioc_type:str: The IoCType of the observable
            :param idx:any: The index of the source row

        Returns:
            PreProcessResult

        """
        if observable is None or observable.strip() is None:
            status = 'Failed: Empty or missing observable value'
            self._add_invalid_input_result(
                observable, ioc_type, status, idx)
            self._print_status(status + ' (Source index {})'.format(idx), 1)
            return PreProcessResult(None, status)

        # Check that observable is of the correct format for this type
        # and do any cleaning up required
        pp_observable = self._preprocess_observable(observable, ioc_type)
        if pp_observable.observable is None:
            self._add_invalid_input_result(observable, ioc_type, pp_observable.status, idx)
            # pylint: disable=locally-disabled, line-too-long
            self._print_status(
                'Invalid observable format: "{}", type "{}", status: {} - skipping. (Source index {})'
                .format(observable, ioc_type, pp_observable.status, idx), 2)
            # pylint: enable=locally-disabled, line-too-long
            return pp_observable

        # Check that we don't already have a result for this
        dup_result = self._check_duplicate_submission(observable, ioc_type, idx)
        if dup_result.is_dup:
            # pylint: disable=locally-disabled, line-too-long
            self._print_status(
                'Duplicate observable value detected: "{}", type "{}" status: {} - skipping. (Source index {})'
                .format(observable, ioc_type, dup_result.status, idx), 2)
            return PreProcessResult(None, dup_result.status)

        return pp_observable

    def _check_duplicate_submission(self, observable, ioc_type, source_index) -> DuplicateStatus:
        """
        Check for a duplicate value in existing results.

        If duplicate found, add a new result row that is a reference to
        the duplicated result row
            :param observable: The IoC observable value
            :param ioc_type: The IoC type
            :param source_idx: The index of the source DataFrame row

        Returns:
            DuplicateStatus{bool, str}

        """
        if self.results is None:
            return DuplicateStatus(False, 'ok')

        # Note duplicate var here can be multiple rows of past results
        duplicate = self.results[self.results['Observable'] == observable].copy()
        # if this is a file hash we should check for previous results in all of the hash
        # columns
        if duplicate.shape[0] == 0 and ioc_type in ['md5_hash', 'sha1_hash', 'sh256_hash']:
            dup_query = 'MD5 == @observable or SHA1 == @observable or SHA256 == @observable'
            duplicate = self.results.query(dup_query).copy()
            # In these cases we want to set the observable to the source value but keep the
            # rest of the results
            if duplicate.shape[0] > 0:
                duplicate['Observable'] = observable

        # if we found a duplicate so add the copies of the duplicated requests
        # to the results
        if duplicate.shape[0] > 0:
            original_indices = [v[0] for v in duplicate[['SourceIndex']].values]
            duplicate['SourceIndex'] = source_index
            duplicate['Status'] = 'Duplicate'
            if pd_version_23():
                new_results = pd.concat(
                    objs=[self.results, duplicate], ignore_index=True, sort=False, axis=0)
            else:
                new_results = pd.concat(
                    objs=[self.results, duplicate], ignore_index=True, axis=0)
            self.results = new_results

            return DuplicateStatus(True, 'Duplicates of {}'.format(original_indices))

        return DuplicateStatus(False, 'ok')

    def _add_invalid_input_result(self, observable, ioc_type, status, source_idx):
        """
        Add a result row to indicate an invalid submission.

            :param observable: The IoC observable value
            :param ioc_type: The IoC type
            :param status: The status - why the item was invalid
            :param source_idx: The index of the source DataFrame row
        """
        new_row = pd.Series(index=self._RESULT_COLUMNS)
        new_row['Observable'] = observable
        new_row['IoCType'] = ioc_type
        new_row['Status'] = status
        new_row['SourceIndex'] = source_idx
        new_results = self.results.append(new_row.to_dict(), ignore_index=True)

        self.results = new_results

    def _vt_submit_request(self, submission_string, vt_param):
        """
        Submit the request to VT.

            :param submission_string: The observable (or observable collection)
            :param vt_param: VT parameters appropriate to this observable type
        """
        params = {'apikey': self._vtkey,
                  vt_param.api_var_name: submission_string}
        submit_url = self._get_vt_api_url(vt_param.api_type)
        headers = {'User-Agent': 'VirusTotal',
                   'Content-Type': 'application/json'}
        if vt_param.headers is not None:
            for hdr, val in vt_param.headers.items():
                headers[hdr] = val

        if vt_param.http_verb == 'post':
            response = requests.post(submit_url, data=params, headers=headers)
        else:
            response = requests.get(submit_url, params=params, headers=headers)
        if response.status_code == 200:
            return response.json(), response.status_code
        else:
            if response:
                try:
                    return response.json(), response.status_code
                except JSONDecodeError:
                    pass
            return None, response.status_code

    def _preprocess_observable(self, observable, ioc_type) -> PreProcessResult:
        """
        Preprocesses and checks validity of observable against declared IoC type.

            :param observable: the value of the IoC
            :param ioc_type: the IoC type
        """
        observable = observable.strip()
        if not self._ioc_extract.validate(observable, ioc_type):
            return PreProcessResult(None,
                                    'Observable does not match expected pattern for ' + ioc_type)
        if ioc_type == 'url':
            return self._preprocess_url(observable)
        if ioc_type == 'ipv4':
            return self._preprocess_ip4(observable)
        if ioc_type == 'dns':
            return self._preprocess_dns(observable)
        if ioc_type in ['md5_hash', 'sha1_hash', 'sha256_hash']:
            return self._preprocess_hash(observable)
        return PreProcessResult(observable, 'ok')

    @classmethod
    def _preprocess_url(cls, url: str) -> PreProcessResult:
        """
        Check that URL can be parsed.

            :param cls: the class
            :param url: the URL to check
        """
        try:
            scheme, _, host, _, _, _, _ = parse_url(url)
            clean_url = url
        except LocationParseError:
            # Try to clean URL and re-check
            clean_url = cls._clean_url(url)
            if clean_url is None:
                return PreProcessResult(None, 'Could not parse as valid URL')
            try:
                scheme, _, host, _, _, _, _ = parse_url(clean_url)
            except LocationParseError:
                return PreProcessResult(None, 'Could not parse as valid URL')

        if scheme is None or host is None:
            return PreProcessResult(None, f'url scheme or host missing from {url}')
        # get rid of some obvious false positives (localhost, local hostnames)
        try:
            addr = ip_address(host)
            if addr.is_private:
                return PreProcessResult(None, 'Host part of URL is a private IP address')
            if addr.is_loopback:
                return PreProcessResult(None, 'Host part of URL is a loopback IP address')
        except ValueError:
            pass

        if '.' not in host:
            return PreProcessResult(None, 'Host is unqualified domain name')

        if scheme.lower() in ['file']:
            return PreProcessResult(None, f'{scheme} URL scheme is not supported')

        return PreProcessResult(clean_url, 'ok')

    @classmethod
    def _clean_url(cls, url: str) -> str:
        """
        Clean URL to remove query params and fragments and any trailing stuff.

            :param cls: the class
            :param url: the URL to check
        """
        # slightly stricter than normal URL regex to exclude '() from host string
        http_strict_regex = r'''
            (?P<protocol>(https?|ftp|telnet|ldap|file)://)
            (?P<userinfo>([a-z0-9-._~!$&*+,;=:]|%[0-9A-F]{2})*@)?
            (?P<host>([a-z0-9-._~!$&\*+,;=]|%[0-9A-F]{2})*)
            (:(?P<port>\d*))?
            (/(?P<path>([^?\#| ]|%[0-9A-F]{2})*))?
            (\?(?P<query>([a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9A-F]{2})*))?
            (\#(?P<fragment>([a-z0-9-._~!$&'()*+,;=:/?@]|%[0-9A-F]{2})*))?\b'''
        if cls._http_strict_rgxc is None:
            cls._http_strict_rgxc = re.compile(
                http_strict_regex, re.I | re.X | re.M)

        # Try to clean URL and re-check
        match_url = cls._http_strict_rgxc.search(url)
        if (match_url.groupdict()['protocol'] is None or
                match_url.groupdict()['host'] is None):
            return None, 'Could not parse as valid URL'

        # build the URL dropping the query string and fragments
        clean_url = match_url.groupdict()['protocol']
        if match_url.groupdict()['userinfo']:
            clean_url += match_url.groupdict()['userinfo']
        clean_url += match_url.groupdict()['host']
        if match_url.groupdict()['port']:
            clean_url += ':' + match_url.groupdict()['port']
        if match_url.groupdict()['path']:
            clean_url += '/' + match_url.groupdict()['path']

        return clean_url

    @classmethod
    def _preprocess_ip4(cls, ipaddress: str):
        """Ensure Ip address is a valid public IPv4 address."""
        try:
            addr = ip_address(ipaddress)
        except ValueError:
            return PreProcessResult(None, 'IP address is invalid format')

        if not isinstance(addr, IPv4Address):
            return PreProcessResult(None, 'Not an IPv4 address')
        if addr.is_global:
            return PreProcessResult(ipaddress, 'ok')
        if addr.is_private:
            return PreProcessResult(None, 'IP is private address')
        if addr.is_loopback:
            return PreProcessResult(None, 'IP is loopback address')
        if addr.is_reserved:
            return PreProcessResult(None, 'IP is reserved address')
        if addr.is_multicast:
            return PreProcessResult(None, 'IP is multicast address')
        return PreProcessResult(None, 'IP address is not global')

    @classmethod
    def _preprocess_dns(cls, domain: str) -> PreProcessResult:
        """Ensure DNS is a valid-looking domain."""
        if '.' not in domain:
            return PreProcessResult(None, 'Domain is unqualified domain name')
        try:
            addr = ip_address(domain)
            del addr
            return PreProcessResult(None, 'Domain is an IP address')
        except ValueError:
            return PreProcessResult(domain, 'ok')

    @classmethod
    def _preprocess_hash(cls, hash_str: str) -> PreProcessResult:
        """Ensure Hash has minimum entropy (rather than a string of 'x')."""
        str_entropy = cls.entropy(hash_str)
        if str_entropy < 3.0:
            return PreProcessResult(None, 'String has too low an entropy to be a hash')
        return PreProcessResult(hash_str, 'ok')

    @classmethod
    def entropy(cls, input_str: str) -> float:
        """Compute entropy of input string."""
        str_len = float(len(input_str))
        return -sum(map(lambda a: (a / str_len) * math.log2(a / str_len),
                        Counter(input_str).values()))

    @classmethod
    def _get_vt_api_url(cls, api_type: str) -> str:
        """
        Return the VirusTotal API URL for the supplied type.

            :param api_type: The IoC type
        """
        if api_type not in cls._VT_API_TYPES:
            raise LookupError('Unknown api type "{}"'.format(api_type))
        return cls._VT_API.format(type=api_type)

    @classmethod
    def _get_supported_vt_ioc_types(cls) -> list({str}):
        """Return the subset of IoC types supported by VT."""
        return [t for t in cls._SUPPORTED_INPUT_TYPES if cls._VT_TYPE_MAP[t] is not None]

    def _print_status(self, message: str, verbosity_level: str):
        """
        Print a status message depending on the current level of verbosity.

            :param message: the string message to print
            :param verbosity_level: at which level the message should be output
        """
        if verbosity_level <= self._verbosity:
            print(message)
