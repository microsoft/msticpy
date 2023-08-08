# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""TIProviders test class."""
import datetime as dt
import json
import random
import string
import warnings
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import pytest
import pytest_check as check

from msticpy.common import pkg_config
from msticpy.common.provider_settings import get_provider_settings
from msticpy.context.preprocess_observable import _clean_url, preprocess_observable
from msticpy.context.provider_base import generate_items
from msticpy.context.tilookup import TILookup
from msticpy.context.tiproviders.result_severity import ResultSeverity
from msticpy.context.tiproviders.tor_exit_nodes import Tor

from ..unit_test_lib import custom_mp_config, get_test_data_path

_TEST_DATA = get_test_data_path()


# pylint: disable=protected-access, redefined-outer-name
@pytest.fixture
def ti_lookup():
    """Return TILookup instance."""
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            return TILookup()


_IOC_IPS = [
    "185.92.220.35",
    "213.159.214.86",
    "77.222.54.202",
    "91.219.29.81",
    "193.9.28.254",
    "89.108.83.196",
    "91.219.28.44",
    "188.127.231.124",
    "192.42.116.41",
    "91.219.31.18",
    "46.4.239.76",
    "188.166.168.250",
    "195.154.241.208",
    "51.255.172.55",
    "93.170.169.52",
    "104.215.148.63",
    "13.77.161.179",
]
_BENIGN_IPS = ["40.76.4.15", "40.112.72.205", "40.113.200.201"]  # benign


def is_benign_ioc(request_item):
    """Return True if benign."""
    if isinstance(request_item, str):
        return any(item for item in _BENIGN_IPS if item in request_item)
    if isinstance(request_item, dict):
        return any(item for item in _BENIGN_IPS if item in request_item.values())
    return False


# This class will mock httpx.Client()
class HTTPResponse:
    """Class for mocked response."""

    def __init__(self, json_data, status_code):
        """Initialize the class."""
        self.json_data = json_data
        self.status_code = status_code

    def json(self):
        """Return Python representation of json data."""
        return self.json_data


class RequestSession:
    """Mock httpx session."""

    def get(self, *args, **kwargs):
        """Return results of httpx.get."""
        if "url" not in kwargs:
            if args:
                kwargs.update({"url": args[0]})
            else:
                return HTTPResponse(None, 404)
        for url_match, resp_data in _PROVIDER_RESPONSES.items():
            if not kwargs["url"].startswith(url_match):
                continue
            if is_benign_ioc(self._get_ioc_from_query(resp_data, **kwargs)):
                return HTTPResponse(None, 404)
            if "params" in resp_data and isinstance(resp_data["response"], str):
                str_response = self._format_json_response(resp_data, **kwargs)
                check.is_not_none(json.loads(str_response))
                return HTTPResponse(json.loads(str_response), 200)
            elif isinstance(resp_data["response"], dict):
                return HTTPResponse(resp_data["response"], 200)
            elif callable(resp_data["response"]):
                check.is_not_none(
                    resp_data["response"](**(resp_data["params"]), **kwargs)
                )
                return HTTPResponse(
                    resp_data["response"](**(resp_data["params"]), **kwargs), 200
                )
        return HTTPResponse(None, 404)

    @staticmethod
    def _get_ioc_from_query(resp_data, **kwargs):
        """Extract the part of the query containing the observable."""
        path = resp_data.get("ioc_param")
        if path is None:
            return kwargs["url"]
        kwargs_item = kwargs
        for elem in path.split("."):
            kwargs_item = kwargs_item.get(elem)
        return kwargs_item

    @staticmethod
    def _format_json_response(resp_data, **kwargs):
        """Replace dynamic params in the response string."""
        format_params = resp_data["params"].copy()
        if "query" in format_params:
            format_params["query"] = kwargs["params"]["query"]
        if "url" in format_params:
            format_params["url"] = kwargs["url"]
        response_str = resp_data["response"]
        for token, replace in format_params.items():
            # if isinstance(replace, str):
            #     replace = f'"{replace}"'
            if f"{{[[{token}]]}}" in response_str:
                response_str = response_str.replace(f"{{[[{token}]]}}", str(replace))
        return response_str


_TEST_IOCS = {
    "124.5.6.7": ("ipv4", None),
    "124.5.6.8": ("ipv4", "rep"),
    "124.5.6.9": ("ipv4", "malware"),
    "124.5.6.10": ("ipv4", "whois"),
    "office.microsoft.com": ("dns", "info"),
    "https://badplace.net/path1/path2?x=1": ("url", None),
    "7657fcb7d772448a6d8504e4b20168b7": ("file_hash", None),
    "7657fcb7d772448a6d8504e4b20168b8": ("md5_hash", None),
    "www.microsoft.com": ("hostname", "whois"),
}


@dataclass
class TiTestCase:
    """Class for test cases."""

    provider: str
    exp_requests: int = 20
    exp_responses: int = 17


_TI_PROVIDER_TESTS = [
    TiTestCase("XForce"),
    TiTestCase("OTX"),
    TiTestCase("VirusTotal"),
    TiTestCase("GreyNoise"),
    TiTestCase("RiskIQ"),
    TiTestCase("IntSights"),
    TiTestCase("CrowdSec"),
    TiTestCase("AbuseIPDB", exp_responses=20),
]


@pytest.mark.parametrize("provider_test", _TI_PROVIDER_TESTS)
def test_ti_provider(ti_lookup, provider_test):
    """Test individual providers."""
    provider_name = provider_test.provider
    ti_provider = ti_lookup.loaded_providers[provider_name]
    saved_session = ti_provider._httpx_client
    ti_provider._httpx_client = RequestSession()

    # Lookup multiple IoCs
    for ioc, ioc_params in _TEST_IOCS.items():
        result = ti_lookup.lookup_ioc(
            ioc=ioc,
            ioc_type=ioc_params[0],
            ioc_query_type=ioc_params[1],
            providers=[provider_name],
            show_not_supported=True,
        )
        verify_result(result, ti_lookup)

    # Check if lookup works with observable parameter
    for ioc, ioc_params in _TEST_IOCS.items():
        result = ti_lookup.lookup_ioc(
            ioc=None,
            observable=ioc,
            ioc_type=ioc_params[0],
            ioc_query_type=ioc_params[1],
            providers=[provider_name],
            show_not_supported=True,
        )
        verify_result(result, ti_lookup)

    results_df = ti_lookup.lookup_iocs(
        data=(_IOC_IPS + _BENIGN_IPS), providers=[provider_name]
    )
    check.equal(provider_test.exp_requests, len(results_df))
    check.equal(provider_test.exp_responses, len(results_df[results_df["Result"]]))

    # test the sync version of the API
    results_df = ti_lookup.lookup_iocs_sync(
        data=(_IOC_IPS + _BENIGN_IPS), providers=[provider_name]
    )
    check.equal(provider_test.exp_requests, len(results_df))
    check.equal(provider_test.exp_responses, len(results_df[results_df["Result"]]))

    ti_lookup.browse_results(results_df, severities=["information", "warning", "high"])

    ti_provider._httpx_client = saved_session


# pylint: disable=pointless-statement
def verify_result(result, ti_lookup):
    """Verify return results."""
    check.is_not_none(result)
    check.is_instance(result, pd.DataFrame)
    check.is_false(result.empty)
    check.equal(1, len(result))
    for lu_result in result.to_dict(orient="records"):
        check.is_in(
            lu_result["Provider"],
            [
                "OTX",
                "XForce",
                "VirusTotal",
                "GreyNoise",
                "RiskIQ",
                "IntSights",
                "CrowdSec",
                "AbuseIPDB",
            ],
        )
        check.is_not_none(lu_result["Ioc"])
        check.is_not_none(lu_result["IocType"])
        if lu_result["Result"]:
            check.is_not_none(lu_result["Details"])
            check.is_not_none(lu_result["RawResult"])
            check.is_not_none(lu_result["Reference"])
            # exercise summary functions of Lookup class

    # test browser with raw result
    # note something wrong with RiskIQ raw output.
    riskiq_result = result[
        (result.Provider == "RiskIQ") & (result.Severity != ResultSeverity.unknown.name)
    ]
    if not riskiq_result.empty:
        ti_lookup.browse(riskiq_result)
        # test convert to DF
        result_df = ti_lookup.result_to_df(riskiq_result)
        check.is_instance(result_df, pd.DataFrame)


@pytest.mark.filterwarnings("ignore::UserWarning")
def test_ti_config_and_load(ti_lookup):
    """Test loading TI providers."""
    config_path = Path(_TEST_DATA).parent.joinpath("msticpyconfig-test.yaml")
    with custom_mp_config(config_path):
        with warnings.catch_warnings():
            # We want to ignore warnings from missing config
            warnings.simplefilter("ignore", category=UserWarning)
            ti_settings = get_provider_settings()

            check.is_instance(ti_settings, dict)
            check.greater_equal(len(ti_settings), 4)

    # Try to load TIProviders - should throw a warning on
    # missing provider class
    config_path = Path(_TEST_DATA).joinpath(pkg_config._CONFIG_FILE)
    with custom_mp_config(config_path):
        with pytest.warns(UserWarning):
            ti_lookup = TILookup()

    # should have 2 successfully loaded providers
    check.greater_equal(len(ti_lookup.loaded_providers), 3)
    check.greater_equal(len(ti_lookup.provider_status), 3)

    av_provs = ti_lookup.available_providers
    check.greater_equal(len(av_provs), 1)
    ti_lookup.provider_usage()
    ti_lookup.list_available_providers(show_query_types=True)

    ti_lookup.reload_providers()
    check.greater_equal(len(ti_lookup.available_providers), 1)


def test_ti_lookup_enable_disable(ti_lookup):
    """Test enable and disable a provider."""
    check.greater(len(ti_lookup.configured_providers), 4)

    check.is_in("OTX", ti_lookup._providers)
    ti_lookup.disable_provider("OTX")
    check.is_in("OTX", ti_lookup._secondary_providers)
    check.is_not_in("OTX", ti_lookup._providers)
    ti_lookup.enable_provider("OTX")
    check.is_in("OTX", ti_lookup._providers)
    check.is_not_in("OTX", ti_lookup._secondary_providers)

    ti_lookup.set_provider_state({"OTX": False})
    check.is_in("OTX", ti_lookup._secondary_providers)
    check.is_not_in("OTX", ti_lookup._providers)
    ti_lookup.set_provider_state({"OTX": True})
    check.is_in("OTX", ti_lookup._providers)
    check.is_not_in("OTX", ti_lookup._secondary_providers)


def test_opr_single_result(ti_lookup):
    """Test OPR for single result."""
    ti_provider = ti_lookup.loaded_providers["OPR"]
    ti_provider._httpx_client = RequestSession()
    iocs = {
        "google.com": ("dns", None),
        "microsoft.com": ("dns", None),
        "fsdghklfgh.ndfspccpos.net": ("dns", None),
    }

    # Lookup multiple IoCs
    for ioc, ioc_params in iocs.items():
        result = ti_lookup.lookup_ioc(
            ioc=ioc,
            ioc_type=ioc_params[0],
            ioc_query_type=ioc_params[1],
            providers=["OPR"],
            show_not_supported=True,
        )
        check.is_not_none(result)
        for lu_result in result.to_dict(orient="records"):
            check.is_not_none(lu_result["Ioc"])
            check.is_not_none(lu_result["IocType"])
            if lu_result["Severity"] in ["warning", "high"]:
                check.is_true(
                    "rank" in lu_result["Details"]
                    and lu_result["Details"]["rank"] is None
                )
                check.is_true(
                    "error" in lu_result["Details"]
                    and lu_result["Details"]["error"] == "Domain not found"
                )
            else:
                check.is_true(
                    "rank" in lu_result["Details"]
                    and lu_result["Details"]["rank"].isdigit()
                    and int(lu_result["Details"]["rank"]) > 0
                )
                check.is_true(
                    "response" in lu_result["RawResult"]
                    and lu_result["RawResult"]["response"][0]
                    and lu_result["RawResult"]["response"][0]["domain"] == ioc
                )


def test_opr_multi_result(ti_lookup):
    """Test OPR multi-item lookup."""
    ti_provider = ti_lookup.loaded_providers["OPR"]
    ti_provider._httpx_client = RequestSession()

    n_requests = 9
    gen_doms = pd.DataFrame(
        [
            {"domain": _generate_rand_domain(), "ioc_type": "dns"}
            for _ in range(n_requests)
        ]
    )

    results_df = ti_lookup.lookup_iocs(
        data=gen_doms, ioc_col="domain", ioc_type_col="ioc_type", providers=["OPR"]
    )
    check.equal(n_requests, len(results_df))
    check.greater_equal(
        len(results_df[results_df["Severity"].isin(["warning", "high"]) > 0]),
        1,
    )
    check.equal(n_requests, len(results_df[results_df["Result"]]))


def _generate_rand_domain():
    """Return random domain name helper function."""
    dom_suffixes = ["com", "org", "net", "biz"]
    letters = string.ascii_letters
    str_length = 10  # nosec
    dom = "".join(random.choice(letters) for _ in range(str_length))  # nosec
    dom_part = "".join(random.choice(letters) for _ in range(str_length))  # nosec
    suffix = random.choice(dom_suffixes)  # nosec

    return f"{dom}.{dom_part}.{suffix}".casefold()


def test_tor_exit_nodes(ti_lookup, monkeypatch):
    """Test TOR exit nodes."""
    # # ONLINE_TEST - Uncomment these lines and comment out "OFFLINE_TEST" section
    # # Trigger a lookup to load Tor List
    # init_lookup = "104.117.0.237"
    # ti_lookup.lookup_ioc(observable=init_lookup, ioc_type="ipv4", providers=["Tor"])
    # # we can't use a fixed list since this changes all the time
    # # so take a sample from the current list
    # # ONLINE_TEST - End

    # OFFLINE_TEST
    node_dict = {"ExitNode": True, "LastStatus": dt.datetime.now(dt.timezone.utc)}
    nodelist = {node: node_dict for node in _TOR_NODES}
    monkeypatch.setattr(Tor, "_nodelist", nodelist)
    # OFFLINE_TEST - End
    tor_prov = ti_lookup.loaded_providers["Tor"]
    tor_nodes = random.sample(list(tor_prov._nodelist.keys()), 4)  # nosec

    other_ips = [
        "104.117.0.237",
        "13.107.4.50",
        "172.217.10.144",
        "172.217.11.16",
        "172.217.15.112",
    ]

    pos_results = []
    neg_results = []
    for ioc in tor_nodes + other_ips:
        result = ti_lookup.lookup_ioc(
            ioc=ioc,
            providers=["Tor"],
            show_not_supported=True,
        )
        lu_result = result.to_dict(orient="records")[0]
        check.is_true(lu_result["Result"])
        check.is_true(bool(lu_result["Reference"]))
        if lu_result["Severity"] in ["warning", "high"]:
            check.is_true(bool(lu_result["Details"]))
            check.is_true(bool(lu_result["RawResult"]))
            pos_results.append(lu_result)
        else:
            neg_results.append(lu_result)

    check.equal(len(pos_results), 4)
    check.equal(len(neg_results), 5)

    all_ips = tor_nodes + other_ips
    tor_results_df = ti_lookup.lookup_iocs(data=all_ips, providers=["Tor"])
    check.equal(len(all_ips), len(tor_results_df))
    check.equal(
        len(tor_results_df[tor_results_df["Severity"].isin(["warning", "high"])]), 4
    )
    check.equal(len(tor_results_df[tor_results_df["Severity"] == "information"]), 5)


def test_check_ioc_type(ti_lookup):
    """Check IOC types."""
    provider = ti_lookup.loaded_providers["OTX"]
    lu_result = provider._check_ioc_type(ioc="a.b.c.d", ioc_type="ipv4")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="a.b.c.d", ioc_type="ipv6")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="url", ioc_type="ipv4")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="123", ioc_type="dns")
    check.equal(lu_result["Status"], 2)
    lu_result = provider._check_ioc_type(ioc="424246", ioc_type="file_hash")
    check.equal(lu_result["Status"], 2)


def test_result_severity():
    """Test result severities."""
    sev_inf = ResultSeverity.parse("information")
    check.equal(sev_inf, ResultSeverity.information)
    sev_warn = ResultSeverity.parse(1)
    check.equal(sev_warn, ResultSeverity.warning)
    sev_warn2 = ResultSeverity.parse(sev_warn)
    check.equal(sev_warn2, ResultSeverity.warning)

    sev_unknown = ResultSeverity.unknown
    check.is_true(sev_inf == ResultSeverity.information)
    check.is_true(sev_inf <= "information")
    check.is_true(sev_inf < 1)
    check.is_true(sev_warn > ResultSeverity.information)
    check.is_false(sev_unknown > "high")


_OBS_TYPES = [
    ("ipv4", "127.0.0.1", "IP address is not global"),
    (
        "ipv4",
        "not an ip address",
        "Observable does not match expected pattern for ipv4",
    ),
    ("ipv4", "185.92.220.35", "ok"),
    ("ipv4", "185[.]92[.]220[.]35", "ok"),
    ("ipv6", "185.92.220.35", "Observable does not match expected pattern for ipv6"),
    (
        "ipv4",
        "2001:0db8:85a3:0000:0000:8a2e:0370:7334",
        "Observable does not match expected pattern for ipv4",
    ),
    ("ipv6", "2001:0db8:85a3:0000:0000:8a2e:0370:7334", "IP address is not global"),
    ("ipv6", "2345:0425:2CA1:0000:0000:0567:5673:23b5", "ok"),
    ("dns", "www.python.com", "ok"),
    ("dns", "www[.]python[.]com", "ok"),
    ("dns", "localhost", "Observable does not match expected pattern for dns"),
    ("dns", "185.92.220.35", "Observable does not match expected pattern for dns"),
    (
        "md5_hash",
        "AAAAAAAAAAAAAAAA",
        "Observable does not match expected pattern for md5_hash",
    ),
    (
        "md5_hash",
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
        "String has too low an entropy to be a hash",
    ),
    ("md5_hash", "A123008438B9C391A123008438B9C391", "ok"),
    ("other_type", "A123008438B9C391", "ok"),
    ("url", "https://www.python.com", "ok"),
    ("url", "https://www[.]python[.]com", "ok"),
    ("url", "hXXps://www[.]python[.]com", "ok"),
]


@pytest.mark.parametrize("obs_type, obs, expected", _OBS_TYPES)
def test_preprocess_observables(obs_type, obs, expected):
    """Test observable pre-processing."""
    result = preprocess_observable(obs, ioc_type=obs_type)
    check.equal(result.status, expected)


_TEST_URLS = [
    (
        "https://me@www.microsoft.com:443/test1?testparam=x",
        "https://me@www.microsoft.com:443/test1",
    ),
    ("https://www.microsoft.com", "https://www.microsoft.com"),
]


@pytest.mark.parametrize("url, expected", _TEST_URLS)
def test_clean_url(url, expected):
    """Test clean URL function."""
    check.equal(_clean_url(url), expected)


def test_iterable_generator():
    """Test iterable generator."""
    test_df = pd.DataFrame({"col1": _IOC_IPS, "col2": _IOC_IPS})

    # DataFrames
    for ioc, _ in generate_items(test_df, item_col="col1", item_type_col="col2"):
        check.is_in(ioc, _IOC_IPS)

    # Iterables
    for ioc, ioc_type in generate_items(test_df[["col1"]], item_col="col1"):
        check.is_in(ioc, _IOC_IPS)
        check.equal(ioc_type, "ipv4")

    # Used for local testing only
    # def test_interactive(self):
    #     saved_env = os.environ[pkg_config._CONFIG_ENV_VAR]
    #     os.environ[pkg_config._CONFIG_ENV_VAR] = "e:\\src\\microsoft\\msticpyconfig.yaml"
    #     pkg_config.refresh_config()
    #     if "AzureSentinel" in pkg_config.custom_settings["TIProviders"]:
    #         pkg_config.custom_settings["TIProviders"].pop("AzureSentinel")
    #     ti_lookup = TILookup()

    #     result = ti_lookup.lookup_ioc(
    #         observable="www.401k.com", providers=["OPR", "VirusTotal", "XForce"]
    #         )

    #     os.environ[pkg_config._CONFIG_ENV_VAR] = saved_env


# -------------- PROVIDER RESPONSES ------------------


def test_json_responses():
    """Tests any json string test responses for correct formatting."""
    kwargs = {
        "url": "http://foo",
        "params": {"one": "two", "query": "query_str"},
    }
    for url, resp_data in _PROVIDER_RESPONSES.items():
        print(url, resp_data.keys())
        if isinstance(resp_data["response"], str):
            repl_str = RequestSession._format_json_response(resp_data, **kwargs)
            print(url)
            print(repl_str)
            json.loads(repl_str)


def _open_page_rank_response(dom_responses, **kwargs):
    """Generate conditional OPR response (format changes dep on query)."""
    if "params" in kwargs:
        mocked_result = {
            "status_code": 200,
            "response": [dom_responses["unknown.dom"]],
        }
        for domain in dom_responses.keys():
            if domain in kwargs["params"].values():
                mocked_result = {
                    "status_code": 200,
                    "response": [dom_responses[domain]],
                }
                break
        return mocked_result
    else:
        url_param_str = kwargs["url"].split("?", 1)[1]
        url_params = url_param_str.split("&")
        if len(url_params) > 100:
            raise ValueError("Maximum of 100 items in bulk request")
        rand_responses = []
        for param in url_params:
            dom = param.split("=")[1]
            rank = random.randint(1, 10000)
            if bool(rank % 5):
                dom_resp = {
                    "status_code": 404,
                    "error": "Domain not found",
                    "page_rank_integer": 0,
                    "page_rank_decimal": 0,
                    "rank": None,
                    "domain": dom,
                }

            else:
                dom_resp = {
                    "status_code": 200,
                    "error": "",
                    "page_rank_integer": rank,
                    "page_rank_decimal": float(rank),
                    "rank": str(rank),
                    "domain": dom,
                }
            rand_responses.append(dom_resp)
        return {"status_code": 200, "response": rand_responses}


def _get_riskiq_classification():
    """Generate random classification."""
    score = random.randint(0, 100)
    if score == 100:
        return "MALICIOUS"
    elif score >= 70:
        return "SUSPICIOUS"
    else:
        return "UNKNOWN"


_PROVIDER_RESPONSES = {
    "https://otx.alienvault.com": {
        "ioc_param": "url",
        "response": {
            "response": "Found stuff",
            "pulse_info": {
                "pulses": [
                    {
                        "name": ["somename"],
                        "tags": ["bad", "good", "ugly"],
                        "references": ["url1", "url2"],
                    }
                ]
            },
        },
    },
    "https://api.xforce.ibmcloud.com": {
        "ioc_param": "url",
        "response": {
            "score": 1,
            "cats": ["one", "two"],
            "reason": "no reason really",
            "reasonDescription": "what does it mean?",
            "tags": ["bad", "good", "ugly"],
            "malware": {
                "risk": "high",
                "family": {"type": "trojan", "name": "mybadfile"},
            },
            "total_rows": 4,
            "categoryDescriptions": ["desc1", "desc2"],
            "contact": [
                {
                    "type": "registrant",
                    "name": "Domain Administrator",
                    "organization": "Microsoft Corporation",
                    "country": "United States",
                }
            ],
        },
    },
    "https://api.greynoise.io": {
        "ioc_param": "url",
        "response": {
            "ip": "51.91.185.74",
            "noise": True,
            "riot": False,
            "classification": "malicious",
            "name": "unknown",
            "link": "https://viz.greynoise.io/ip/51.91.185.74",
            "last_seen": "2021-03-18",
            "message": "Success",
        },
    },
    "https://www.virustotal.com/": {
        "ioc_param": "params",
        "response": {
            "resource": "ioc",
            "permalink": "https://virustotal.com/report.html",
            "positives": 1,
            "detected_urls": [
                {
                    "url": "http://bad.com/foo",
                    "positives": 1,
                    "scan_date": dt.datetime.strftime(
                        dt.datetime.now(), "%Y-%m-%d %H:%M:%S"
                    ),
                }
            ],
            "verbose_msg": "A long description....",
            "response_code": 1,
        },
    },
    "https://openpagerank.com": {
        "params": {
            "dom_responses": {
                "google.com": {
                    "status_code": 200,
                    "error": "",
                    "page_rank_integer": 10,
                    "page_rank_decimal": 10,
                    "rank": "6",
                    "domain": "google.com",
                },
                "microsoft.com": {
                    "status_code": 200,
                    "error": "",
                    "page_rank_integer": 8,
                    "page_rank_decimal": 7.63,
                    "rank": "40",
                    "domain": "microsoft.com",
                },
                "unknown.dom": {
                    "status_code": 404,
                    "error": "Domain not found",
                    "page_rank_integer": 0,
                    "page_rank_decimal": 0,
                    "rank": None,
                    "domain": "unknowndomain.com",
                },
            }
        },
        "response": _open_page_rank_response,
    },
    "https://api.passivetotal.org/v2/cards/summary": {
        "ioc_param": "params.query",
        "params": {
            "query": "params.query",
            "score": random.randint(0, 100),
            "classification": _get_riskiq_classification(),
        },
        "response": """
            {
                "type": "IP Address",
                "name": "{[[query]]}",
                "link": "https://community.riskiq.com/search/{[[query]]}",
                "netblock": "8.8.8.0/24",
                "os": "n/a",
                "organization": "ISP Solutions",
                "asn": "AS11234 - ISPSOL",
                "hosting_provider": "SuperHosters",
                "data_summary": {
                    "resolutions": {
                        "count": 21,
                        "link": "https://community.riskiq.com/search/{[[query]]}/resolutions"
                    },
                    "certificates": {
                        "count": 34,
                        "link": "https://community.riskiq.com/search/{[[query]]}/domaincertificates"
                    },
                    "hashes": {
                        "count": 1,
                        "link": "https://community.riskiq.com/search/{[[query]]}/hashes"
                    },
                    "projects": {
                        "count": 1,
                        "link": "https://community.riskiq.com/search/{[[query]]}/projects"
                    },
                    "articles": {
                        "count": 5,
                        "link": "https://community.riskiq.com/research/{[[query]]}"
                    },
                    "trackers": {
                        "count": 15,
                        "link": "https://community.riskiq.com/search/{[[query]]}/trackers"
                    },
                    "components": {
                        "count": 7,
                        "link": "https://community.riskiq.com/search/{[[query]]}/components"
                    },
                    "host_pairs": {
                        "count": 9,
                        "link": "https://community.riskiq.com/search/{[[query]]}/hostpairs"
                    },
                    "cookies": {
                        "count": 25,
                        "link": "https://community.riskiq.com/search/{[[query]]}/cookies"
                    }
                }
            }
            """,
    },
    "https://api.passivetotal.org/v2/reputation": {
        "ioc_param": "params.query",
        "params": {
            "query": "params.query",
            "score": random.randint(0, 100),
            "classification": _get_riskiq_classification(),
        },
        "response": """
            {
                "score": {[[score]]},
                "classification": "{[[classification]]}",
                "rules": [
                    {
                        "name": "Blocklist Malware",
                        "description": "Observed malware on this entity",
                        "severity": 5,
                        "link": null
                    },
                    {
                        "name": "Name server",
                        "description": "Domain is using a name server that is more likely to be used by malicious infrastructure",
                        "severity": 4,
                        "link": null
                    },
                    {
                        "name": "ASN",
                        "description": "Infrastructure hosted by this ASN are more likely to be malicious",
                        "severity": 4,
                        "link": null
                    },
                    {
                        "name": "TLD",
                        "description": "Domains in this TLD are more likely to be malicious",
                        "severity": 3,
                        "link": null
                    }
                ]
            }
        """,
    },
    "https://api.passivetotal.org/v2/enrichment/malware": {
        "ioc_param": "params.query",
        "params": {
            "query": "params.query",
            "score": random.randint(0, 100),
            "classification": _get_riskiq_classification(),
        },
        "response": {
            "success": True,
            "results": [
                {
                    "collectionDate": "2021-01-01",
                    "sample": "36190e9c66c801bc393b8189a5aeaf22",
                    "source": "Malsource Info",
                    "sourceUrl": "https://mocked.url/md5/36190e9c66c801bc393b8189a5aeaf22",
                },
                {
                    "collectionDate": "2020-09-25",
                    "sample": "1c1033f184cc33c87cb6aa54a955d034",
                    "source": "Malsource Info",
                    "sourceUrl": "https://mocked.url/md5/1c1033f184cc33c87cb6aa54a955d034",
                },
            ],
        },
    },
    "https://api.ti.insight.rapid7.com": {
        "ioc_param": "params",
        "response": {
            "Value": "124.5.6.7",
            "Type": "IpAddresses",
            "Score": 42,
            "Severity": "Medium",
            "Whitelist": False,
            "FirstSeen": dt.datetime.strftime(
                dt.datetime.now(), "%Y-%m-%dT%H:%M:%S.%fZ"
            ),
            "LastSeen": dt.datetime.strftime(
                dt.datetime.now(), "%Y-%m-%dT%H:%M:%S.%fZ"
            ),
            "LastUpdate": dt.datetime.strftime(
                dt.datetime.now(), "%Y-%m-%dT%H:%M:%S.%fZ"
            ),
            "Sources": [
                {"ConfidenceLevel": 2, "Name": "Source A"},
                {"ConfidenceLevel": 1, "Name": "Source B"},
                {"ConfidenceLevel": 1, "Name": "Source C"},
                {"ConfidenceLevel": 3, "Name": "Source D"},
            ],
            "SystemTags": ["bot", "malware related"],
            "Geolocation": "FR",
            "RelatedMalware": ["malware1"],
            "RelatedCampaigns": ["Campaign A"],
            "RelatedThreatActors": ["Threat Actor 00"],
            "Tags": ["tag"],
        },
    },
    "https://cti.api.crowdsec.net": {
        "response": {
            "ip_range_score": 1,
            "ip": "167.248.133.133",
            "ip_range": "167.248.133.0/24",
            "as_name": "CENSYS-ARIN-03",
            "as_num": 398722,
            "location": {
                "country": "US",
                "city": None,
                "latitude": 1.751,
                "longitude": -97.822,
            },
            "reverse_dns": "scanner-03.ch1.censys-scanner.com",
            "behaviors": [
                {
                    "name": "sip:bruteforce",
                    "label": "SIP Bruteforce",
                    "description": "IP has been reported for performing a SIP (VOIP) brute force attack.",
                },
                {
                    "name": "tcp:scan",
                    "label": "TCP Scan",
                    "description": "IP has been reported for performing TCP port scanning.",
                },
            ],
            "history": {
                "first_seen": f"{dt.datetime.now().isoformat(timespec='seconds')}+00:00",
                "last_seen": f"{dt.datetime.now().isoformat(timespec='seconds')}+00:00",
                "full_age": 490,
                "days_age": 489,
            },
            "classifications": {
                "false_positives": [],
                "classifications": [
                    {
                        "name": "scanner:legit",
                        "label": "Legit scanner",
                        "description": "IP belongs to a company that scans the internet",
                    },
                    {
                        "name": "scanner:censys",
                        "label": "Known Security Company",
                        "description": "IP belongs to a company that scans the internet: Censys.",
                    },
                    {
                        "name": "community-blocklist",
                        "label": "CrowdSec Community Blocklist",
                        "description": "IP belongs to the CrowdSec Community Blocklist",
                    },
                ],
            },
            "attack_details": [
                {
                    "name": "crowdsecurity/opensips-request",
                    "label": "SIP Bruteforce",
                    "description": "Detect brute force on VOIP/SIP services",
                    "references": [],
                },
                {
                    "name": "firewallservices/pf-scan-multi_ports",
                    "label": "Port Scanner",
                    "description": "Detect tcp port scan",
                    "references": [],
                },
            ],
            "target_countries": {
                "DE": 27,
                "FR": 19,
                "US": 16,
                "EE": 16,
                "HK": 5,
                "DK": 2,
                "GB": 2,
                "FI": 2,
                "KR": 2,
                "SG": 2,
            },
            "background_noise_score": 10,
            "scores": {
                "overall": {
                    "aggressiveness": 1,
                    "threat": 4,
                    "trust": 5,
                    "anomaly": 0,
                    "total": 3,
                },
                "last_day": {
                    "aggressiveness": 0,
                    "threat": 0,
                    "trust": 0,
                    "anomaly": 0,
                    "total": 0,
                },
                "last_week": {
                    "aggressiveness": 0,
                    "threat": 4,
                    "trust": 5,
                    "anomaly": 0,
                    "total": 3,
                },
                "last_month": {
                    "aggressiveness": 0,
                    "threat": 4,
                    "trust": 5,
                    "anomaly": 0,
                    "total": 3,
                },
            },
            "references": [],
        },
    },
    "https://api.abuseipdb.com": {
        "response": {
            "data": {
                "ipAddress": "38.75.137.9",
                "isPublic": True,
                "ipVersion": 4,
                "isWhitelisted": None,
                "abuseConfidenceScore": 0,
                "countryCode": "US",
                "usageType": "Data Center/Web Hosting/Transit",
                "isp": "GlobalTeleHost Corp.",
                "domain": "gthost.com",
                "hostnames": ["9-137-75-38.clients.gthost.com"],
                "isTor": False,
                "totalReports": 0,
                "numDistinctUsers": 0,
                "lastReportedAt": None,
            }
        }
    },
}

_TOR_NODES = [
    "163.172.213.212",
    "163.172.41.228",
    "164.132.9.199",
    "164.92.218.139",
    "164.92.79.65",
    "166.70.207.2",
    "167.235.245.76",
    "167.86.70.160",
    "167.86.94.107",
    "167.99.214.205",
    "169.239.128.179",
    "171.22.147.64",
    "171.25.193.20",
    "171.25.193.235",
    "171.25.193.25",
    "2001:0678:0e3c:0000:0000:0000:0000:000a",
    "2001:0678:0e3c:0000:0000:0000:0000:000b",
    "2001:0678:0e3c:0000:0000:0000:0000:000c",
    "2001:0678:0e3c:0000:0000:0000:0000:000d",
    "2001:067c:06ec:0203:0192:0042:0116:0016",
    "2001:067c:06ec:0203:0218:33ff:fe44:5513",
    "2001:067c:06ec:0203:0218:33ff:fe44:5514",
]
