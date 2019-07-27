import unittest

import pandas as pd

# Test code
from ..msticpy.sectools.iocextract import IoCExtract

TEST_CASES = {
    "ipv4_test": r"c:\one\path\or\another\myprocess -ip4:206.123.1.123",
    "ipv6_test": r"""c:\one\path\or\another\myprocess -ip6:(2001:0db8:85a3:0000:0000:8a2e:0370:7334,
    2001:db8:85a3:0:0:8a2e:370:7334,2001:db8:85a3::8a2e:370:7334,::ffff:192.0.2.128)""",
    "url_test": r"""c:\one\path\or\another\myprocess /url:https://some.domain.it/thepath?qry1=abc&qry2=xyz
        /url:https://myuser@some.domain.es:88/thepath?qry1=abc&qry2=xyz"<some other trailing stuff""",
    "windows_path_test": r'c:\one\path\or\another\myprocess -file:"..\another\file" -file:"\\uncpath\file"',
    "linux_path_test": r"/bin/bash --file:./bish --file:/bin/bash --file:../../bosh",
    "md5_hash_test": "00236a2ae558018ed13b5222ef1bd987hash -something-hash=00236a2ae558018ed13b5222ef1bd988hash -something -hash=00236a2ae558018ed13b5222ef1bd989",
    "sha1_hash_test": "00236a2ae558018ed13b5222ef1bd98700000001hash -something -hash=00236a2ae558018ed13b5222ef1bd98700000002hash -something -hash=00236a2ae558018ed13b5222ef1bd98700000003",
    "sha256_hash_test": """00236a2ae558018ed13b5222ef1bd98700000001123456789012345678901234hash -something -hash=00236a2ae558018ed13b5222ef1bd98700000001123456789012345678901235hash -something
-hash=00236a2ae558018ed13b5222ef1bd98700000001123456789012345678901236""",
    "url2_test": "curl 'https://www.virustotal.com/en/ip-address/90.156.201.27/information/'",
    "domain1_test": "some text with a domain.like.uk in it",
    "domain_neg_test": "some text with a bad domain.like.iandom in it",
    "domain_short_test": "some text with a microsoft.com in it",
}


class TestIoCExtractor(unittest.TestCase):
    """Unit test class."""

    def __run_extract(
        self, extractor=None, testcase=None, expected_items=None, os_family="Windows"
    ):
        if extractor is None or testcase is None or expected_items is None:
            raise Exception("One or more required parameters were missing")

        test_input = TEST_CASES[testcase + "_test"]
        results = extractor.extract(test_input, os_family=os_family, include_paths=True)
        for k, v in expected_items.items():
            self.assertEqual(len(results[k]), v, "Unexpected value for " + k)

    def setUp(self):
        self.extractor = IoCExtract()

    def test_ipv4(self):
        self.__run_extract(self.extractor, "ipv4", {"ipv4": 1})

    def test_ipv6(self):
        self.__run_extract(self.extractor, "ipv6", {"ipv6": 2})

    def test_url(self):
        self.__run_extract(self.extractor, "url", {"url": 2, "dns": 2, "ipv4": 0})
        self.__run_extract(self.extractor, "url2", {"url": 1, "dns": 1, "ipv4": 1})

    def test_windows_path(self):
        self.__run_extract(self.extractor, "windows_path", {"windows_path": 3})

    def test_linux_path(self):
        self.__run_extract(
            self.extractor, "linux_path", {"linux_path": 3}, os_family="Linux"
        )

    def test_hashes(self):
        self.__run_extract(self.extractor, "md5_hash", {"md5_hash": 3})
        self.__run_extract(self.extractor, "sha1_hash", {"sha1_hash": 3})
        self.__run_extract(self.extractor, "sha256_hash", {"sha256_hash": 3})

    def test_dns(self):
        self.__run_extract(self.extractor, "domain1", {"dns": 1})
        self.__run_extract(self.extractor, "domain_neg", {"dns": 0})
        self.__run_extract(self.extractor, "domain_short", {"dns": 1})

    def test_dataframe(self):

        input_df = pd.DataFrame.from_dict(
            data=TEST_CASES, orient="index", columns=["input"]
        )
        output_df = self.extractor.extract(
            data=input_df, columns=["input"], os_family="Windows", include_paths=True
        )

        self.assertGreater(output_df.shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv6"].shape[0], 2)
        self.assertEqual(output_df[output_df["IoCType"] == "url"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "windows_path"].shape[0], 6)
        self.assertEqual(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)

        input_df = pd.DataFrame.from_dict(
            data=TEST_CASES, orient="index", columns=["input"]
        )
        output_df = self.extractor.extract(
            data=input_df, columns=["input"], os_family="Linux", include_paths=True
        )
        # for _, row in output_df[output_df['IoCType'] == 'url'].iterrows():
        #     print(row.Observable)
        self.assertGreater(output_df.shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv6"].shape[0], 2)
        self.assertEqual(output_df[output_df["IoCType"] == "url"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "linux_path"].shape[0], 8)
        self.assertEqual(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)

    def test_dataframe_ioc_types(self):

        input_df = pd.DataFrame.from_dict(
            data=TEST_CASES, orient="index", columns=["input"]
        )
        output_df = self.extractor.extract(
            data=input_df,
            columns=["input"],
            os_family="Windows",
            ioc_types=["ipv4", "url", "md5_hash"],
        )

        self.assertGreater(output_df.shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv6"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "url"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 0)

    def test_dataframe_new(self):

        input_df = pd.DataFrame.from_dict(
            data=TEST_CASES, orient="index", columns=["input"]
        )
        output_df = self.extractor.extract_df(
            data=input_df, columns=["input"], os_family="Windows", include_paths=True
        )

        self.assertGreater(output_df.shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv6"].shape[0], 2)
        self.assertEqual(output_df[output_df["IoCType"] == "url"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "windows_path"].shape[0], 6)
        self.assertEqual(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)

        input_df = pd.DataFrame.from_dict(
            data=TEST_CASES, orient="index", columns=["input"]
        )
        output_df = self.extractor.extract_df(
            data=input_df, columns=["input"], os_family="Linux", include_paths=True
        )
        # for _, row in output_df[output_df['IoCType'] == 'url'].iterrows():
        #     print(row.Observable)
        self.assertGreater(output_df.shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv6"].shape[0], 2)
        self.assertEqual(output_df[output_df["IoCType"] == "url"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "linux_path"].shape[0], 8)
        self.assertEqual(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 3)

    def test_dataframe_ioc_types_new(self):

        input_df = pd.DataFrame.from_dict(
            data=TEST_CASES, orient="index", columns=["input"]
        )
        output_df = self.extractor.extract_df(
            data=input_df,
            columns=["input"],
            os_family="Windows",
            ioc_types=["ipv4", "url", "md5_hash"],
        )

        self.assertGreater(output_df.shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv4"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "ipv6"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "url"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "windows_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "linux_path"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "md5_hash"].shape[0], 3)
        self.assertEqual(output_df[output_df["IoCType"] == "sha1_hash"].shape[0], 0)
        self.assertEqual(output_df[output_df["IoCType"] == "sha256_hash"].shape[0], 0)


if __name__ == "__main__":
    unittest.main()
