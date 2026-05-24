@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

echo === Building Win32 tests ===
..\DPT.exe RECENT Build "Test.DptDebugger.dproj" Win32 Debug
set BUILD_ERROR=%ERRORLEVEL%
if %BUILD_ERROR% neq 0 (
    echo Win32 Build failed.
    popd
    exit /b %BUILD_ERROR%
)

echo.
echo === Building Win64 tests ===
..\DPT.exe RECENT Build "Test.DptDebugger.dproj" Win64 Debug
set BUILD_ERROR=%ERRORLEVEL%
if %BUILD_ERROR% neq 0 (
    echo Win64 Build failed.
    popd
    exit /b %BUILD_ERROR%
)

popd
endlocal
