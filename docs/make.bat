REM @ECHO OFF

pushd %~dp0

REM Command file for Sphinx documentation

if "%SPHINXBUILD%" == "" (
	set SPHINXBUILD=sphinx-build
)
set SOURCEDIR=source
set BUILDDIR=build

if /I "%SPHINX_NOGEN%" NEQ "" goto no_gen_files
if "%1" == "" goto help
if /I "%1" NEQ "html" goto no_gen_files
if /I "%2" EQU "nogen" goto no_gen_files

REM Generate API source RST files
echo Regenerating API source files...
del /Q %SOURCEDIR%\api\*
set APIDOC_OPTS=--force --module-first --separate
sphinx-apidoc --o %SOURCEDIR%/api %APIDOC_OPTS% ../msticpy ../msticpy/sectools
del %SOURCEDIR%\api\modules.rst

REM generate query list
echo Generating query list documentation
python -m generate_query_docs doc --file source\data_acquisition\DataQueries.rst

:no_gen_files


%SPHINXBUILD% >NUL 2>NUL
if errorlevel 9009 (
	echo.
	echo.The 'sphinx-build' command was not found. Make sure you have Sphinx
	echo.installed, then set the SPHINXBUILD environment variable to point
	echo.to the full path of the 'sphinx-build' executable. Alternatively you
	echo.may add the Sphinx directory to PATH.
	echo.
	echo.If you don't have Sphinx installed, grab it from
	echo.http://sphinx-doc.org/
	exit /b 1
)

%SPHINXBUILD% -M %1 %SOURCEDIR% %BUILDDIR% %SPHINXOPTS%
goto end

:help
%SPHINXBUILD% -M help %SOURCEDIR% %BUILDDIR% %SPHINXOPTS%

:end
popd
