#!/bin/bash
# Test script for MSTICPY releases to PyPI Test
# Author: Ian Hellen

usage(){
    echo Usage:
    echo    $1 src_root_path package-version
}

if [ $# -ne 2 ]; then
    echo Usage:
    echo    $1 src_root_path package-version
    exit 0
fi

h_rule="----------------------------------------------------"
warn_l="!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"

env_name="mp_test_$2"
src_root=$( readlink -f $1 )
env_path="$src_root/$env_name"
mp_pkg_spec="msticpy==$2"

echo $h_rule
echo Started MSTICPy Test script
date
echo Using environment: $env_name
echo Source Root: $src_root
echo Environment scripts path: $env_path

if [ ! -d $src_root ]; then
    echo $src_root is not a valid directory
    usage $0
    exit 1
fi

# Check if MSTICPYCONFIG is set
if [ -z ${MSTICPYCONFIG+x} ]; then
    if [ -f $src_root/msticpyconfig.yaml ]; then
        export MSTICPYCONFIG=$src_root/msticpyconfig.yaml
    else
        echo The MSTICPYCONFIG variable must be defined and point
        echo to a value msticpyconfig.yaml with at least MaxMind auth key
        echo defined
        exit 1
    fi
fi

if [ -z ${MSTICPYCONFIG+x} ] || [ ! -f $MSTICPYCONFIG ]; then
    echo The MSTICPYCONFIG variable must be defined and point
    echo to a value msticpyconfig.yaml with at least MaxMind auth key
    echo defined
    exit 1
fi

echo Using MSTICPYCONFIG: $MSTICPYCONFIG
echo $h_rule

pushd $src_root > /dev/null

# clone repo
echo Cloning msticpy repo
if [ ! -f msticpy/setup.py ]; then
    git clone --no-single-branch --depth 50 https://github.com/Microsoft/msticpy msticpy
fi

pushd msticpy > /dev/null
git checkout --force master
git pull origin master
echo $h_rule

popd > /dev/null

# Python Venv

# Check on existence of venv
if [ ! -f $env_name/bin/activate ]; then
    echo Creating environment $env_name...
    python3 -m venv $env_name
fi

echo Activating environment $env_name...
source $env_name/bin/activate
python3 -m pip install wheel

echo
echo $h_rule
echo pip installing msticpy from PyPI...
python3 -m pip install --upgrade --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple $mp_pkg_spec

echo $h_rule
echo Installing nbconvert and jupyter extensions...
python3 -m pip install nbconvert
python3 -m pip install jupyter_contrib_nbextensions

nb_path=msticpy/docs/notebooks
pushd $nb_path > /dev/null

echo ""
echo $h_rule
echo "Running notebooks from $nb_path..."
nbconvert_opts="--ExecutePreprocessor.timeout=60 --ExecutePreprocessor.kernel_name=python3 --to notebook"

failures=0
success=0
for NB in Base64Unpack.ipynb EventTimeline.ipynb GeoIPLookups.ipynb NotebookWidgets.ipynb ProcessTree.ipynb
do
    echo $env_path/bin/jupyter nbconvert $nbconvert_opts --execute $NB
    pwd
    $env_path/bin/jupyter nbconvert $nbconvert_opts --execute $NB
    retVal=$?
    if [ $retVal -ne 0 ]; then
        echo Error encountered running notebook $NB
        failures=$( expr $failures + 1 )
    fi
    success=$( expr $success + 1 )
done

if [ $failures -ne 0 ] || [ $success -eq 0 ]; then
    echo $h_rule
    echo $warn_l
    echo $failures errors encountered running notebooks
    echo $success successful executions running notebooks
    echo $warn_l
    popd > /dev/null
    echo Completed with errors
    date
    exit 1
fi


echo $h_rule
echo $h_rule
echo All notebooks ran successfully.
echo $h_rule
echo $h_rule

echo ""
echo $h_rule
echo Cleaning up...
echo removing notebook output files
rm *.nbconvert.ipynb
popd > /dev/null


deactivate
echo ""
echo "Remove the $env_name environment? (y/n)"
read response
if [ $response == y ]; then
    rm -r $env_path
fi

echo $h_rule
echo Test completed successfully.
date
echo $h_rule

