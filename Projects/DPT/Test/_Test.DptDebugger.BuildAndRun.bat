@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

echo === Building and running Win32 tests ===
..\DPT.exe RECENT BuildAndRun "Test.DptDebugger.dproj" Win32 Debug
set BUILD_ERROR=%ERRORLEVEL%
if %BUILD_ERROR% neq 0 (
    echo Win32 Build or Run failed.
    popd
    exit /b %BUILD_ERROR%
)

echo.
echo === Building and running Win64 tests ===
..\DPT.exe RECENT BuildAndRun "Test.DptDebugger.dproj" Win64 Debug
set BUILD_ERROR=%ERRORLEVEL%
if %BUILD_ERROR% neq 0 (
    echo Win64 Build or Run failed.
    popd
    exit /b %BUILD_ERROR%
)

popd
endlocal