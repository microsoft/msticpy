# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""domain_utilstes extract test class."""

from ..msticpy.sectools import domain_utils 

def test_validate_domain():
    test_dom_val = domain_utils.DomainValidator()
    valid_tld = test_dom_val.validate_tld('www.goole.com')
    resolvable = test_dom_val.is_resolvable('www.google.com')
    blacklisted = test_dom_val.ssl_blacklisted('www.google.com')
    assert valid_tld == True
    assert resolvable == True
    assert blacklisted == False
