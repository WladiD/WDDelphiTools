@echo off
chcp 65001 > nul
setlocal

rem Change to the script's directory
pushd %~dp0

rem Navigate to the FitNesse directory relative to the project root
cd ..\..\..\SlimForDelphi\FitNesse

echo Running DptIdeExpert Tests (Target: Delphi 12 IDE on Port 9012)...
java -jar fitnesse-standalone.jar -c "WDProject.DPT.IdeExpert?suite&format=text"
set TEST_ERROR=%ERRORLEVEL%

if %TEST_ERROR% neq 0 (
    echo FitNesse test execution failed.
    popd
    exit /b %TEST_ERROR%
)

popd
endlocal
