@echo off
if "%1" equ "" goto usage
set h_rule=------------------------------------------------------------
echo %h_rule%
echo MSTICPY Package release test
echo %h_rule%

conda env list | findstr "%1"
if %ERRORLEVEL% neq 0 goto create_env
echo %1 is a current conda environment.
echo You should run this test in a clean environment.
echo Ctrl-C to abort or
pause

:create_env
echo %h_rule%
echo Creating environment %1...
call conda create --yes --name %1
echo Activating environment %1...
call conda activate %1
echo.
echo %h_rule%
echo Install Python
call conda install --yes python==3.7.6
call conda install --yes pip
echo.
echo %h_rule%
echo Installing msticpy...
pip install --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple msticpy

echo %h_rule%
echo Preparing to run notebooks. Crtl-C to abort.
echo.
echo Installing nbconvert and jupyter extensions...
call conda install --yes nbconvert
call conda install --yes jupyter_contrib_nbextensions
set nb_path=docs/notebooks
if "%2" neq "" set nb_path=%2
pushd %nb_path%
echo.
echo %h_rule%
echo Running notebooks from %nb_path%...
set nbconver_opts=--ExecutePreprocessor.timeout=60 --ExecutePreprocessor.kernel_name=python3 --to notebook
set NB=Base64Unpack.ipynb
jupyter nbconvert %nbconver_opts% --execute %NB%
if ERRORLEVEL 1 goto nb_error
set NB=EventTimeline.ipynb
jupyter nbconvert %nbconver_opts% --execute %NB%
if ERRORLEVEL 1 goto nb_error
set NB=FoliumMap.ipynb
jupyter nbconvert %nbconver_opts% --execute %NB%
if ERRORLEVEL 1 goto nb_error
set NB=GeoIPLookups.ipynb
jupyter nbconvert %nbconver_opts% --execute %NB%
if ERRORLEVEL 1 goto nb_error
set NB=NotebookWidgets.ipynb
jupyter nbconvert %nbconver_opts% --execute %NB%
if ERRORLEVEL 1 goto nb_error
set NB=ProcessTree.ipynb
jupyter nbconvert %nbconver_opts% --execute %NB%
if ERRORLEVEL 1 goto nb_error

echo %h_rule%
echo All notebooks ran successfully.


echo.
echo %h_rule%
echo Cleaning up...
echo removing notebook output files
del *.nbconvert.ipynb
call conda deactivate
echo.
echo About to remove the %1 environment. Ctrl-C to abort
pause
call conda env remove -n %1
popd
echo %h_rule%
echo Test completed.
echo %h_rule%

goto end

:nb_error
echo %h_rule%
echo Error encountered running notebook %NB%
echo Test Failed
echo %h_rule%
popd
goto end

:usage
echo Usage:
echo    %~n0 test-env-name [path-to-notebooks]
echo.

:end

