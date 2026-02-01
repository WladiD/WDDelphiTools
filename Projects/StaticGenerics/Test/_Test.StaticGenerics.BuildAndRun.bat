@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

..\..\DPT\DPT.exe RECENT BuildAndRun "Test.StaticGenerics.dproj" Win32 Debug --OnlyIfChanged
set BUILD_ERROR=%ERRORLEVEL%
if %BUILD_ERROR% neq 0 (
    echo Build or Run failed.
    popd
    exit /b %BUILD_ERROR%
)

popd
endlocal