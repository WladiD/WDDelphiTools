@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

set "DPT_EXE=..\DPT\DPT.exe"

if not exist "%DPT_EXE%" (
    echo ERROR: DPT.exe not found at "%DPT_EXE%". Please build DPT first.
    exit /b 1
)

set "PACKAGE_NAME=DptIdeExpert"

echo Unregistering package %PACKAGE_NAME%...
"%DPT_EXE%" RECENT RemovePackage "%PACKAGE_NAME%"

if %ERRORLEVEL% neq 0 (
    echo Unregistration failed or package was not registered.
    rem We don't exit with error here, as it might be fine if it wasn't registered.
)

echo Done.
popd
endlocal
