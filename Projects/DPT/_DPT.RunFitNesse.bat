@echo off
chcp 65001 > nul
setlocal

rem Change to the script's directory (Projects/DPT/)
pushd %~dp0

rem Navigate to the FitNesse directory relative to the project root
cd ..\..\..\SlimForDelphi\FitNesse

rem Execute FitNesse test
rem The command path "DptTestSuite.OpenUnitTests" is relative to FitNesseRoot.
java -jar fitnesse-standalone.jar -c "DptTestSuite.OpenUnitTests?test&format=text"

if %ERRORLEVEL% neq 0 (
    echo FitNesse test execution failed.
    popd
    exit /b %ERRORLEVEL%
)

popd
endlocal
