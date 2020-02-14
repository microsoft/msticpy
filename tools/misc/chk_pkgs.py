# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test script for checking package version alignment."""
import re

KQL_REQUIRES = [
    "ipython>=7.1.1",
    "ipykernel>=5.1.1",
    "plotly>=3.10.0",
    "prettytable>=0.7.2",
    "matplotlib>=3.0.0",
    "pandas>=0.23.4",
    "adal>=1.2.1",
    "Pygments>=2.2.0",
    "seaborn>=0.9.0",
    "requests>=2.21.0",
    "python-dateutil>=2.7.5",
    "traitlets>=4.3.2",
    "psutil>=5.4.7",
    "six>=1.11.0",
    "setuptools>=41.0.1",
    "Markdown>=3.0.1",
    "beautifulsoup4>=4.6.3",
    "lxml>=4.2.5",
    "pytz>=2019.1",
    "pyjwt>=1.7.1",
]


PKG_VER_PATTERN = r"([\w\-_]+)\s*([><=]{2})\s*([\d.]+)"


def extract_pkgs(req_file=None, pkg_reqs=None):
    """Get reqs from requirements.txt."""
    if req_file is not None:
        with open(req_file, "r") as req_fh:
            pkg_reqs = req_fh.readlines()
    if not pkg_reqs:
        return {}
    pkg_dict = {}
    for line in pkg_reqs:
        req_match = re.match(PKG_VER_PATTERN, line)
        if not req_match:
            print(f"Failed on {line}")
        pkg_dict[req_match.groups()[0]] = (req_match.groups()[1], req_match.groups()[2])
    return pkg_dict


BH_PATH = "E:/temp/pkg_comp/bluehound-req.txt"
MP_PATH = "E:/src/microsoft/msticpy/msticpy/requirements.txt"

BH_REQS = extract_pkgs(req_file=BH_PATH)
MP_REQS = extract_pkgs(req_file=MP_PATH)
KM_REQS = extract_pkgs(pkg_reqs=KQL_REQUIRES)


def solve_all(ver1, op1, ver2, op2):
    """Solve conflicts in both directions."""
    ver_ok, problem = solve(ver1, op1, ver2, op2)
    if not ver_ok:
        return ver_ok, problem
    return solve(ver1=ver2, op1=op2, ver2=ver1, op2=op1)


def solve(ver1, op1, ver2, op2):
    """Solve conflicts."""
    ver1_t = tuple(ver1.split("."))
    ver2_t = tuple(ver2.split("."))
    if op1 == "==":
        if op2 == "==":
            return ver1_t == ver2_t, f"{ver1} != {ver2}"
        if op2 == ">=":
            return ver1_t >= ver2_t, f"{ver1} < {ver2}"
    return True, ""


def check_conflicts(src_pkg, dest_pkg):
    """Check conflicts in between packages."""
    conflicts = []
    compats = []
    matches = []
    for pkg, ver in src_pkg.items():
        if pkg in dest_pkg:
            ver2 = dest_pkg[pkg]
            if ver[1] == dest_pkg[pkg][1]:
                matches.append(pkg)
            else:
                ver_ok, mssg = solve_all(ver[1], ver[0], ver2[1], ver2[0])
                if ver_ok:
                    compats.append((pkg, ver, dest_pkg[pkg]))
                else:
                    conflicts.append((pkg, ver, dest_pkg[pkg], mssg))
    print(f"Matched version: {matches}")
    if conflicts:
        print("Conflicts (pkg, ver_pkg1, ver_pkg2, mssg)")
        for conflict in conflicts:
            print(conflict)
    if compats:
        print("Compatible (pkg, ver_pkg1, ver_pkg2)")
        for compat in compats:
            print(compat)


print("msticpy vs. bluehound")
check_conflicts(MP_REQS, BH_REQS)
print("\nmsticpy vs. kqlmagic")
check_conflicts(MP_REQS, KM_REQS)
print("\nbluehound vs. kqlmagic")
check_conflicts(BH_REQS, KM_REQS)
