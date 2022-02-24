"""Test runs for different extras."""
import argparse
from datetime import datetime
import os
import subprocess  # nosec


# pylint: disable=subprocess-run-check

DEF_PKG_LIST = """
apipkg              1.5
argon2-cffi         20.1.0
async-generator     1.10
atomicwrites        1.4.0
attrs               20.3.0
backcall            0.2.0
bleach              3.2.1
cffi                1.14.4
colorama            0.4.4
coverage            5.3
cycler              0.10.0
decorator           4.4.2
defusedxml          0.6.0
entrypoints         0.3
execnet             1.7.1
iniconfig           1.1.1
ipykernel           5.3.4
ipython             7.18.1
ipython-genutils    0.2.0
ipywidgets          7.5.1
jedi                0.17.2
Jinja2              2.11.2
jsonschema          3.2.0
jupyter             1.0.0
jupyter-client      6.1.7
jupyter-console     5.2.0
jupyter-core        4.6.3
jupyterlab-pygments 0.1.2
kiwisolver          1.3.1
MarkupSafe          1.1.1
matplotlib          3.2.0
mistune             0.8.4
nbclient            0.5.1
nbconvert           6.0.7
nbformat            5.0.8
nest-asyncio        1.4.1
notebook            6.1.4
numpy               1.19.3
packaging           20.7
pandas              1.1.5
pandocfilters       1.4.3
parso               0.7.1
pickleshare         0.7.5
pip                 20.1.1
pluggy              0.13.1
prometheus-client   0.9.0
prompt-toolkit      3.0.8
py                  1.9.0
pycparser           2.20
Pygments            2.7.1
pyparsing           2.4.7
pyrsistent          0.17.3
pytest              6.1.2
pytest-check        0.3.9
pytest-cov          2.10.1
pytest-xdist        2.1.0
python-dateutil     2.8.1
pytz                2020.4
pywin32             300
pywinpty            0.5.7
pyzmq               19.0.2
qtconsole           5.0.1
QtPy                1.9.0
respx               0.17.1
Send2Trash          1.5.0
setuptools          47.1.0
six                 1.15.0
terminado           0.9.1
testpath            0.4.4
toml                0.10.2
tornado             6.0.4
traitlets           5.0.5
wcwidth             0.2.5
webencodings        0.5.1
widgetsnbextension  3.5.1
"""


base_pkgs = [pkg.split()[0] for pkg in DEF_PKG_LIST.split("\n") if pkg]


def _install_pkg(app_args):

    extra_spec = f"[{','.join(app_args.extras)}]" if app_args.extras else ""
    sp_run = [
        "python",
        "-m",
        "pip",
        "install",
        "--disable-pip-version-check",
        "--no-cache-dir" if app_args.nocache else "",
        "-f",
        "e:\\src\\microsoft\\msticpy\\dist",
        f"msticpy{extra_spec}==0.9.0a1",
    ]

    start = datetime.now()
    print(f"Install extras {app_args.extras}")
    print("start", start)
    print(sp_run)
    if not test:
        subprocess.run(sp_run, shell=True)  # nosec

    end = datetime.now()
    print("end", end)
    print("duration", end - start)


def _reset_pkgs():
    sp_run = [
        "python",
        "-m",
        "pip",
        "list",
        "--disable-pip-version-check",
    ]
    proc_call = subprocess.run(sp_run, shell=True, capture_output=True)  # nosec
    inst_pkgs = proc_call.stdout.decode("utf-8").split("\n")[2:]
    inst_pkgs = {pkg.split()[0] for pkg in inst_pkgs if pkg and not pkg.startswith("-")}
    print(f"{len(inst_pkgs)} packages installed")
    remove_pkgs = inst_pkgs - set(base_pkgs)

    if remove_pkgs:
        pip_cmd = sp_run.index("list")
        sp_run[pip_cmd] = "uninstall"
        sp_run.extend(["-y", *remove_pkgs])
        print(sp_run)
        if not test:
            subprocess.run(sp_run, shell=True)  # nosec
    else:
        print("No packages to remove")


_MP_SRC = "/src/microsoft/msticpy"


def _run_tests():
    os.environ["MSTICPYCONFIG"] = f"{_MP_SRC}/tests/msticpyconfig-test.yaml"
    os.environ["MAXMIND_AUTH"] = "REDACTED"
    os.environ["IPSTACK_AUTH"] = "REDACTED"
    sp_run = [
        "pytest",
        "-r",
        "fEp",
        "--disable-warnings",
        "--show-capture=no",
        "--tb=no",
        "--continue-on-collection-errors",
        "--cov=msticpy",
    ]
    print(sp_run)
    if test:
        cur_dir = os.getcwd()
        os.chdir(_MP_SRC)
        subprocess.run(sp_run, shell=True)  # nosec
        os.chdir(cur_dir)


def _add_script_args():
    parser = argparse.ArgumentParser(description="Msticpy test installer")
    parser.add_argument(
        "--install",
        "-i",
        action="store_true",
        required=False,
        default=False,
        help="Run install",
    )
    parser.add_argument(
        "--extras",
        "-e",
        nargs="+",
        required=False,
        default=None,
        help="Name of extras",
    )
    parser.add_argument(
        "--nocache",
        "-n",
        action="store_true",
        required=False,
        default=False,
        help="Run pip with --no-cache-dir option",
    )
    parser.add_argument(
        "--reset",
        "-r",
        action="store_true",
        default=False,
        help="Uninstall everying but base",
    )
    parser.add_argument(
        "--test",
        "-t",
        action="store_true",
        default=False,
        help="Run tests",
    )
    parser.add_argument(
        "--check",
        "-c",
        action="store_true",
        default=False,
        help="Run in check mode",
    )
    return parser


# pylint: disable=invalid-name
if __name__ == "__main__":
    arg_parser = _add_script_args()
    args = arg_parser.parse_args()

    test = args.test
    if args.install:
        _install_pkg(args)

    if args.reset:
        _reset_pkgs()

    if args.test:
        _run_tests()
