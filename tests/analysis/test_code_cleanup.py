# -------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See License.txt in the project root for
# license information.
# --------------------------------------------------------------------------
"""Test code cleanup module."""

import pytest
import pytest_check as check

from msticpy.analysis import code_cleanup

__author__ = "Ian Hellen"

# pylint: disable=redefined-outer-name

_OBFUS_CODE = """
If($XXXXXiOnTaBlE.XXXXrSIOn.MajOR -gE 3){$mYVAr=[rEF].XyyXyyXyy.
GetTYPE(\'System.SomeLIB\')."GEtFie`LD"(\'cachedTest\',\'N\'+\'onPublic,
Static\');If($mYVAr){$vAr2=$mYVAr.GetVaLUE($nUlL);$vAl=[XyyXyyXYy.GENerIC
.DIcTIOnarY[XyXyXy,SYsTEm.ObJEct]]::new();$Val.Add(\'TeS\'+\'t2\',0);$val
.Add(\'Test1\',0);$vAr2[\'XXXX_YYYYY_XXXXX\\Software\\Some\'+\'App\']=$Val}}
"""


@pytest.fixture(scope="module")
def ps_code():
    """Powershell code block."""
    return _OBFUS_CODE.replace("\n", "")


_EXPECTED_TEXT = r"""
if($xxxxxiontable.xxxxrsion.major -ge 3)
{
    $myvar=[ref].xyyxyyxyy.gettype('system.somelib')."getfield"('cachedtest','nonpublic,static')
    if($myvar)
    {
        $var2=$myvar.getvalue($null)
        $val=[xyyxyyxyy.generic.dictionary[xyxyxy,system.object]]::new()
        $val.add('test2',0)
        $val.add('test1',0)
        $var2['xxxx_yyyyy_xxxxx\software\someapp']=$val
    }
}
"""


def test_format_powershell(ps_code):
    """Test reformatting of powershell."""
    check.equal(code_cleanup.format_powershell(ps_code), _EXPECTED_TEXT.strip())
