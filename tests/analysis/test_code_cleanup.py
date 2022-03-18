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
If($PSVERSiOnTaBlE.PSVErSIOn.MajOR -gE 3){$mYVAr=[rEF].ASsEMbLY.
GetTYPE(\'System.SomeLIB\')."GEtFie`LD"(\'cachedTest\',\'N\'+\'onPublic,
Static\');If($mYVAr){$vAr2=$mYVAr.GetVaLUE($nUlL);$vAl=[CollEctIOns.GENerIC
.DIcTIOnarY[StrIng,SYsTEm.ObJEct]]::new();$Val.Add(\'TeS\'+\'t2\',0);$val
.Add(\'Test1\',0);$vAr2[\'HKEY_LOCAL_MACHINE\\Software\\Some\'+\'App\']=$Val}
ElSE{[SCRiptBlOCk]."GeTFIE`lD"(\'tes\',\'N\'+\'T3,Param\').SETValuE($NULL,
(New-OBjEcT sAMple.fIrST.iTEm))}$Ref=[ReF].AsseMBlY.GETTypE(\'System.Test
.Test2.\'+\'Utils\');$REf.GeTFIELd(\'testInitF\'+\'ailed\',\'NonReal,Dynamic\')
.SeTVaLUE($nulL,$tRUE);};
"""


@pytest.fixture(scope="module")
def ps_code():
    """Powershell code block."""
    return _OBFUS_CODE.replace("\n", "")


_EXPECTED_TEXT = r"""
if($psversiontable.psversion.major -ge 3)
{
    $myvar=[ref].assembly.gettype('system.somelib')."getfield"('cachedtest','nonpublic,static')
    if($myvar)
    {
        $var2=$myvar.getvalue($null)
        $val=[collections.generic.dictionary[string,system.object]]::new()
        $val.add('test2',0)
        $val.add('test1',0)
        $var2['hkey_local_machine\software\someapp']=$val
    }
    else
    {
        [scriptblock]."getfield"('tes','nt3,param').setvalue($null,(new-object sample.first.item))
    }
    $ref=[ref].assembly.gettype('system.test.test2.utils')
    $ref.getfield('testinitfailed','nonreal,dynamic').setvalue($null,$true)
}
"""


def test_format_powershell(ps_code):
    """Test reformatting of powershell."""
    check.equal(code_cleanup.format_powershell(ps_code), _EXPECTED_TEXT.strip())
