# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""
base64_unpack.

The main function of this module is to decode and unpack
strings that are obfuscated using base64 and/or certain
compression algorithms such as gzip and zip.

It has the following functions:
unpack_items - this is the main entry point and takes either a string or
a pandas dataframe (with specified column) as input. It returns a string
with obfuscated parts replaced by decoded equivalents (unless the decoding
results in an undecodable binary, in which case a placeholder is used).


Other helper functions may also be useful standalone
get_items_from_gzip(binary): Return decompressed gzip content of byte string
get_items_from_zip(binary): Return dictionary of zip contents from byte string
get_items_from_tar(binary): Return dictionary of tar file contents
get_hashes(binary): Return md5, sha1 and sha256 hashes of input byte string
"""

import base64
import binascii
import gzip
import hashlib
import io
import re
import tarfile
import zipfile
from collections import namedtuple

import pandas as pd

from .. nbtools.utility import pd_version_23, export
from .. _version import VERSION

__version__ = VERSION
__author__ = 'Ian Hellen'

_RESULT_FIELDS = ['reference', 'original_string', 'file_name', 'file_type',
                  'input_bytes', 'decoded_string', 'encoding_type',
                  'file_hashes', 'md5', 'sha1', 'sha256', 'printable_bytes']

BinaryRecord = namedtuple('BinaryRecord', _RESULT_FIELDS)

# pylint: disable=locally-disabled, line-too-long
_BASE64_HEADER_TYPES = {
    'TVqQAAMAAAAEAAAA//8AALgAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA+AAAAA4fug': 'exe',
    'TVqQAAMAAAAEAAAA//8AALgAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA8AAAAA4fug': 'dll',
    'TVqQAAMAAAAEAAAA//8AALgAAAAAAAAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA6AAAAA4fug': 'sys',
    'UEsDBBQAAAAIA': 'zip',
    'UEsDBBQAAQAIA': 'zip (passprotected)',
    'H4sI': 'gz',
    'N3q8ryccAAR': '7z',
    'UmFyIRoHAM': 'rar',
    'JVBERi0xLjcNC': 'pdf',
    '0M8R4KGxGuE': 'msi',
    'TVNXSU0AAADQ': 'wim'}
# pylint: enable=locally-disabled, line-too-long
_BASE64_HEADER_OFFSET_TYPES = {
    'DAxMDA3NzcAMDAwMDAwM': 'tar'
}

# Base64 simple regex
_BASE64_REGEX = '(?P<b64>[A-Za-z0-9+/\\n\\r]{30,}={0,2})'
BASE64_REGEX_C = re.compile(_BASE64_REGEX, re.I | re.X)
# '''(?<![A-Za-z0-9+\/])
#             (?:
#                 (?:[A-Za-z0-9+\/]{4})*
#                 (?:
#                     [A-Za-z0-9+\/]{2} [AEIMQUYcgkosw048] = |
#                     [A-Za-z0-9+\/] [AQgw] ==
#                 ) |
#                 (?:[A-Za-z0-9\/]{4})*
#                 (?:
#                     \+ [A-Za-z0-9\/+]{3} |
#                     [A-Za-z0-9\/] \+ [A-Za-z0-9+\/]{2} |
#                     [A-Za-z0-9\/]{2} \+ [A-Za-z0-9+\/] |
#                     [A-Za-z0-9\/]{3} \+
#                 )
#                 (?:[A-Za-z0-9+\/]{4})*
#             )
#             (?![A-Za-z0-9+\/=])'''

# When True prints see more verbose execution
# (set from 'trace' parameter to unpack_items)
# pylint: disable=locally-disabled, C0103
_debug_trace = False
# pylint: enable=locally-disabled, C0103


@export
def unpack_items(input_string=None, data=None, column=None, trace=False):
    """
    Base64 decode an input string or multiple strings taken from a pandas dataframe column.

    If the input is a dataframe you must supply the name of the column to use.

    Items that decode to utf-8 or utf-16 strings will be returned as decoded
    strings replaced in the original string. If the encoded string is a
    known binary type it will identify the file type and return the hashes
    of the file. If any binary types are known archives (zip, tar, gzip) it
    will unpack the contents of the archive.
    For any binary it will return the decoded file as a byte array, and as a
    printable list of byte values. If the input is a string the function
    returns:

    - decoded string: this is the input string with any decoded sections
    replaced by the results of the decoding

    It also returns the data as a Pandas DataFrame with the following columns:
    - reference : this is an index that matches an index number in the
    returned string (e.g. <<encoded binary type=pdf index=1.2').
    - original_string : the string prior to decoding - file_type : the type
    of file if this could be determined
    - file_hashes : a dictionary of hashes (the md5, sha1 and sha256 hashes
    are broken out into separate columns)
    - input_bytes : the binary image as a byte array
    - decoded_string : printable form of the decoded string (either string
    or list of hex byte values)
    - encoding_type : utf-8, utf-16 or binary
    - md5, sha1, sha256 : the respective hashes of the binary file_type,
    file_hashes, input_bytes, md5, sha1, sha256 will be null if this item is
    decoded to a string

    If the input is a dataframe the output dataframe will also include the
    following column: - src_index - the index of the source row in the input
    frame. This allows you to re-join the output data to the input data
    """
    global _debug_trace
    _debug_trace = trace

    if input_string is not None:
        return _decode_b64_string_recursive(input_string)
    if data is not None:
        if not column:
            raise ValueError('column must be supplied if the input is a DataFrame')

        output_df = None
        for input_row in data[[column]].itertuples():
            (decoded_string, output_frame) = _decode_b64_string_recursive(input_row[1])
            output_frame['src_index'] = input_row.Index
            output_frame['full_decoded_string'] = decoded_string
            if output_df is None:
                output_df = output_frame
            else:
                if pd_version_23():
                    output_df = output_df.append(output_frame, ignore_index=True,
                                                 sort=False)
                else:
                    output_df = output_df.append(output_frame, ignore_index=True)
        return output_df
    return None


def _debug_print_trace(*args):
    if _debug_trace:
        for arg in args:
            print(arg, end='')
        print()


# base64 decoding
def _decode_and_format_b64_string(b64encoded_string, item_prefix=None,
                                  current_depth=1, current_index=1):
    """Decode string and return displayable content plus list of decoded artifacts."""
    # Check if we recognize this as a known file type
    (_, f_type) = _is_known_b64_prefix(b64encoded_string)
    _debug_print_trace('Found type: ', f_type)
    output_files = _decode_b64_binary(b64encoded_string, f_type)
    if not output_files:
        return b64encoded_string, None

    if len(output_files) == 1:
        # get the first (only) item
        out_name, out_record = list(output_files.items())[0]
        _debug_print_trace('_decode_b64_binary returned a single record')
        _debug_print_trace('record:', out_record)

        # Build display string
        # If a string, include the decoded item in the output
        if out_record.encoding_type in ['utf-8', 'utf-16']:
            display_string = f'<decoded type=\'string\' name=\'{out_name}\' ' +\
                             f'index=\'{item_prefix}{current_index}\' ' +\
                             f'depth=\'{current_depth}\'>' +\
                             f'{out_record.decoded_string}</decoded>'
            return display_string, [out_record]
        else:
            # if a binary just record its presence
            display_string = f'<decoded value=\'binary\'  name=\'{out_name}\' ' +\
                             f'type=\'{out_record.file_type}\' ' +\
                             f'index=\'{item_prefix}{current_index}\' ' +\
                             f'depth=\'{current_depth}\'/>'
            return display_string, [out_record]
    else:
        # Build header display string
        display_header = f'<decoded value=\'multiple binary\' type=\'multiple\' ' +\
                         f' index=\'{item_prefix}{current_index}\'>'
        child_display_strings = []
        child_index = 1
        child_depth = current_depth + 1
        _debug_print_trace('_decode_b64_binary returned multiple records')

        # Build child display strings
        for child_name, child_rec in output_files.items():
            _debug_print_trace('Child_decode: ', child_rec)
            child_index_string = f'{item_prefix}{current_index}.{child_index}'

            if child_rec.encoding_type in ['utf-8', 'utf-16']:
                # If a string, include the decoded item in the output
                child_display_string = f'<decoded type=\'string\' name=\'{child_name}\' ' +\
                                       f'index=\'{child_index_string}\' ' +\
                                       f'depth=\'{child_depth}\'>' +\
                                       f'{child_rec.decoded_string}</decoded>'
            else:
                # if a binary just record its presence
                child_display_string = f'<decoded type=\'{child_rec.file_type}\' ' +\
                                       f'name=\'{child_name}\' ' +\
                                       f'index=\'{child_index_string}\' ' +\
                                       f'depth=\'{child_depth}\'/>'
            child_display_strings.append(child_display_string)
            child_index += 1

        display_string = display_header + ''.join(child_display_strings) + '</decoded>'
        return display_string, output_files.values()


def _decode_b64_string_recursive(input_string, undecodable_strings=None,
                                 max_recursion=20, current_depth=1, item_prefix=''):
    """
    Recursively decode and unpack an encoded string.

        :rtype: (str, [BinaryRecord]
    """
    _debug_print_trace('_decode_b64_string_recursive: ', max_recursion)
    _debug_print_trace('processing input: ', input_string[:200])

    decoded_string = input_string

    # we use this to store a set of strings that match the B64 regex but
    # that we were unable to decode - so that we don't end up in an
    # infinite loop
    if not undecodable_strings:
        undecodable_strings = set()

    binary_records = pd.DataFrame(columns=_RESULT_FIELDS)
    fragment_index = 0
    match_pos = 0
    something_decoded = False
    while True:
        # search sequentially through the input string for any strings that look like base64
        _debug_print_trace('regex searching ', decoded_string[:200],
                           ' from pos: ', match_pos, ' bin_index ',
                           fragment_index)
        b64match = BASE64_REGEX_C.search(decoded_string, match_pos)

        # _debug_print_trace('groups: ', len(b64match.groupdict()))
        if b64match is not None:
            _debug_print_trace('regex found: ', b64match.groupdict()['b64'])
            # if (in a recursive call) we already know that this string doesn't decode
            # skip this match
            if b64match.groupdict()['b64'] in undecodable_strings:
                _debug_print_trace('previous undecodable string')
                match_pos = b64match.end()
                continue

            # try to decode
            fragment_index += 1
            (decoded_fragment, binary_items) = (
                _decode_and_format_b64_string(b64match.groupdict()['b64'],
                                              item_prefix=item_prefix,
                                              current_index=fragment_index,
                                              current_depth=current_depth))

            # if the string didn't decode we'll have the same output as input
            # so add that to our set of undecodable strings (we need to track this
            # otherwise we will recurse infinitely)
            something_decoded = (decoded_fragment != b64match.groupdict()['b64'])
            if something_decoded:
                # we did decode something so lets put our result this in the output string
                if binary_items:
                    # if there are one or more binary_items from decoding
                    # add them to our output DataFrame
                    for bin_record in binary_items:
                        new_row = bin_record._asdict()
                        new_row['reference'] = f'{item_prefix}{current_depth}.{fragment_index}'
                        new_row['original_string'] = b64match.groupdict()['b64']
                        new_row['md5'] = new_row['file_hashes']['md5']
                        new_row['sha1'] = new_row['file_hashes']['sha1']
                        new_row['sha256'] = new_row['file_hashes']['sha256']

                        if pd_version_23():
                            binary_records = binary_records.append(new_row,
                                                                   ignore_index=True,
                                                                   sort=False)
                        else:
                            binary_records = binary_records.append(new_row,
                                                                   ignore_index=True)

                # replace the decoded fragment in our current results string (decode_string)
                decoded_string = decoded_string.replace(b64match.groupdict()['b64'],
                                                        decoded_fragment)
                _debug_print_trace('Replaced string', decoded_string[match_pos:match_pos + 100])
                match_pos += len(decoded_fragment)
            else:
                undecodable_strings.add(b64match.groupdict()['b64'])
                _debug_print_trace('new undecodable string')
                match_pos = b64match.end()

        else:
            _debug_print_trace('Regex not found: ', match_pos)
            break

        if fragment_index > 50:
            break

    # if we reach our max recursion depth bail out here
    if max_recursion == 0:
        _debug_print_trace('max recursion reached')
        return decoded_string, binary_records

    if something_decoded:
        # stuff that we have already decoded may also contain further base64 encoded strings
        pfx = f'{item_prefix}.{fragment_index}.' if item_prefix else f'{fragment_index}.'
        (next_level_string, child_records) = _decode_b64_string_recursive(
            decoded_string, undecodable_strings, item_prefix=pfx,
            max_recursion=max_recursion - 1, current_depth=(current_depth + 1))
        return next_level_string, binary_records.append(
            child_records, ignore_index=True, sort=False)
    else:
        _debug_print_trace('Nothing left to decode')
        return decoded_string, binary_records


def _print_bytes(bytes_array):
    """Print byte array as string or hex."""
    enc_results = _get_byte_encoding(bytes_array)
    if enc_results.encoding_type != 'binary':
        print(enc_results.decoded_string)

    else:
        print('Could not decode bytes to string. Hashes:')
        print(get_hashes(_binary_to_bytesio(bytes_array).getbuffer()))
        print(enc_results.printable_bytes)


def _as_byte_string(bytes_array):
    return ' '.join(['{0:02x}'.format(b) for b in bytes_array])


def _empty_binary_rec():
    return BinaryRecord(reference=None,
                        original_string=None,
                        decoded_string=None,
                        encoding_type=None,
                        file_name=None,
                        file_type=None,
                        input_bytes=None,
                        file_hashes=None,
                        md5=None,
                        sha1=None,
                        sha256=None,
                        printable_bytes=None)


def _get_byte_encoding(bytes_array):
    """
    Return encoding type and decoded result.

    Decoded result is list of hex bytes in the case that the decoded
    result is not a string
    """
    result_rec = _empty_binary_rec()
    printable_bytes = _as_byte_string(bytes_array)
    try:
        decoded_string = bytes_array.decode('utf-8')
        return result_rec._replace(decoded_string=decoded_string,
                                   encoding_type='utf-8',
                                   printable_bytes=printable_bytes)
    except UnicodeDecodeError:
        pass

    try:
        # Difficult to tell the difference between a real unicode string
        # and a binary string that happens to decode to a utf-16 string
        decoded_string = bytes_array.decode('utf-16')
        return result_rec._replace(decoded_string=decoded_string,
                                   encoding_type='utf-16',
                                   printable_bytes=printable_bytes)
    except UnicodeDecodeError:
        pass
    return result_rec._replace(encoding_type='binary',
                               printable_bytes=printable_bytes)


def _is_known_b64_prefix(input_string):
    """If this is known file type return the prefix and file type."""
    first160chars = input_string[0:160].replace('\n', '').replace('\r', '')
    for prefix, file_type in _BASE64_HEADER_TYPES.items():
        if first160chars.startswith(prefix):
            return prefix, file_type

    for matching_string, file_type in _BASE64_HEADER_OFFSET_TYPES.items():
        regex_comp = re.compile(matching_string, re.I | re.X)
        if regex_comp.search(first160chars):
            return matching_string, file_type
    return None, None


def _decode_b64_binary(input_string, file_type=None):
    """Examine input string for known binaries and decode and unpack."""
    if not file_type:
        (_, f_type) = _is_known_b64_prefix(input_string)
        file_type = f_type

    try:
        decoded_bytes = base64.b64decode(input_string)
        return _unpack_and_hash_b64_binary(decoded_bytes, file_type)
    except binascii.Error:
        # we couldn't decode
        _debug_print_trace('Binascii exception - trying to decode string')
        _debug_print_trace(input_string)
        return None


def _unpack_and_hash_b64_binary(input_bytes, file_type=None):
    """
    If this is a known archive type extract the contents.

    Return a dictionary of (file_type :
        (file_name, file_hashes, input_bytes, decoded_string, encoding_type)
    """
    if not input_bytes:
        return None

    output_files = dict()
    if file_type in ['zip', 'gz', 'tar']:
        # if this is a known archive type - try to extract the contents
        (unpacked_type, file_items) = _get_items_from_archive(input_bytes, file_type)
        if unpacked_type != 'unknown':
            for file_name, extracted_file in file_items.items():
                file_results = _get_hashes_and_printable_string(extracted_file)
                idx = f'[{unpacked_type}] Filename: {file_name}'

                # ToDo - the unpacked type here refers to the archive file type so assigning this
                # to file_type is not exactly the right thing to do. In a future episode we'll
                # try to determine the file type using magic numbers.
                output_files[idx] = file_results._replace(file_name=file_name,
                                                          file_type=unpacked_type,
                                                          input_bytes=extracted_file)
                _debug_print_trace('_unpack_and_hash_b64_binary item (archive): ',
                                   type(file_results.decoded_string),
                                   file_results.decoded_string)

    if not output_files:
        # if this wasn't a known archive type or we failed to unpack anything,
        # just get the hashes and return
        file_results = _get_hashes_and_printable_string(input_bytes)
        idx = f'[{file_type}]'
        output_files[idx] = file_results._replace(file_name='unknown',
                                                  file_type=file_type,
                                                  input_bytes=input_bytes)
        _debug_print_trace('_unpack_and_hash_b64_binary item (other): ',
                           type(file_results.decoded_string),
                           file_results.decoded_string)
    return output_files


def _get_hashes_and_printable_string(extracted_file):
    """
    Get the hashes, encoding type and printable form of binary.

    (either string or list of hex-encoded byte values)
    """
    file_details = _get_byte_encoding(extracted_file)
    file_hashes = get_hashes(extracted_file)
    return file_details._replace(file_hashes=file_hashes)


def _get_items_from_archive(binary, archive_type='zip'):
    """Extract contained files from an archive type."""
    _debug_print_trace('_get_items_from_archive type: ', archive_type)
    if archive_type == 'zip':
        return get_items_from_zip(binary)
    elif archive_type == 'gz':
        return get_items_from_gzip(binary)
    elif archive_type == 'tar':
        return get_items_from_tar(binary)
    else:
        return 'unknown', {archive_type, binary}


@export
def get_items_from_gzip(binary):
    """Return decompressed gzip contents."""
    archive_file = gzip.decompress(binary)
    return 'gz', {'gzip_file': archive_file}


@export
def get_items_from_zip(binary):
    """Return dictionary of zip contents."""
    file_obj = io.BytesIO(binary)
    zip_archive = zipfile.ZipFile(file_obj, mode='r')
    archive_dict = dict()
    for item in zip_archive.namelist():
        archive_file = zip_archive.read(item)
        archive_dict[item] = archive_file
    return 'zip', archive_dict


@export
def get_items_from_tar(binary):
    """Return dictionary of tar file contents."""
    file_obj = io.BytesIO(binary)
    # Open tarfile
    tar = tarfile.open(mode="r", fileobj=file_obj)
    archive_dict = dict()
    # Iterate over every member
    for item in tar.getnames():
        tar_file = tar.extractfile(item)
        archive_file = tar_file.read()
        archive_dict[item] = archive_file
    return 'tar', archive_dict


@export
def get_hashes(binary):
    """Return md5, sha1 and sha256 hashes of input byte string."""
    hash_dict = dict()
    for hash_type in ['md5', 'sha1', 'sha256']:
        if hash_type == 'md5':
            hash_alg = hashlib.md5()
        elif hash_type == 'sha256':
            hash_alg = hashlib.sha256()
        elif hash_type == 'sha1':
            hash_alg = hashlib.sha1()
        hash_alg.update(binary)
        hash_dict[hash_type] = hash_alg.hexdigest()
    return hash_dict


def _binary_to_bytesio(binary):
    if isinstance(binary, io.BytesIO):
        return binary.getbuffer()
    return io.BytesIO(binary).getbuffer()
