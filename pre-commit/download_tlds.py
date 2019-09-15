# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Pre-commit hook to download Top Level Domain seedlist."""
import argparse
from datetime import datetime
import sys
import os
from typing import List, Optional, Sequence
from urllib import request
from urllib.error import HTTPError, URLError
import warnings


_TLD_LIST = "https://data.iana.org/TLD/tlds-alpha-by-domain.txt"


def _get_tlds() -> Optional[List[str]]:
    """
    Return IANA Top Level Domains.

    Returns
    -------
    Set[str]
        Set of top level domains.

    """
    try:
        req = request.Request(_TLD_LIST)
        with request.urlopen(req) as resp:  # nosec - Hard-coded URL
            txt_resp = resp.read().decode("utf-8", "ignore")
            tld_set = set(txt_resp.split("\n")[1:])  # get rid of header
            tld_set.remove("")  # get rid of blank values
            return sorted(tld_set)
    except (HTTPError, URLError) as err:
        warnings.warn(
            "Exception detected trying to retrieve IANA top-level domain list."
            + "Falling back to builtin seed list. "
            + f"{err.args}",
            RuntimeWarning,
        )
    return None


def _is_file_old(seed_file: str) -> bool:
    if not os.path.exists(seed_file):
        return True
    mod_date = datetime.fromtimestamp(os.path.getmtime(seed_file))
    time_diff = datetime.utcnow() - mod_date
    return time_diff.days > 30


def _write_tld_seed_file(seed_file: str, seed_list: List[str]):
    """Write existing TLD list to a text file."""
    with open(seed_file, "w") as file_handle:
        file_handle.write("\n".join(seed_list))


def main(argv: Optional[Sequence[str]] = None) -> int:
    """
    Download and replace tld_seed.txt.

    Parameters
    ----------
    argv : Optional[Sequence[str]], optional
        Arguments, by default None

    Returns
    -------
    int
        Return code - non-zero on fail

    """
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--tld-seed", required=True, help="The path to the TLD seed file"
    )
    parser.add_argument(
        "--skip-age-check", action="store_true", help="Skip the file age check"
    )

    args = parser.parse_args(argv)
    retcode = 0
    if _is_file_old(args.tld_seed) or args.skip_age_check:
        seed_list = _get_tlds()
        if seed_list:
            _write_tld_seed_file(args.tld_seed, seed_list)
        else:
            retcode = 1
    return retcode


if __name__ == "__main__":
    sys.exit(main())
