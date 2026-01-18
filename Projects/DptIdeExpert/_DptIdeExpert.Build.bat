@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

rem Call the base build script for DptIdeExpert.dproj
rem Building in Debug configuration for development
call ..\..\_BuildBase.bat "DptIdeExpert.dproj" Win32 Debug

if %ERRORLEVEL% neq 0 (
    echo Build failed!
    popd
    exit /b %ERRORLEVEL%
)

echo.
echo Build successful. 
echo BPL should be located in the default output directory (e.g., public documents or project dir).
echo You can register this package in the IDE via: Component -> Install Packages.
echo.

popd
endlocal
