@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

set "DELPHI_VERSION=RECENT"
if not "%~1"=="" set "DELPHI_VERSION=%~1"

rem Building in Debug configuration for development
..\DPT\DPT.exe "%DELPHI_VERSION%" Build "DptIdeExpert.dproj" Win32 Debug
set BUILD_ERROR=%ERRORLEVEL%
if %BUILD_ERROR% NEQ 0 (
    echo Build failed!
    popd
    exit /b %BUILD_ERROR%
)

echo.
echo Build successful. 
echo BPL should be located in the default output directory (e.g., public documents or project dir).

set "DPT_EXE=..\DPT\DPT.exe"
if not exist "%DPT_EXE%" goto :skip_check

"%DPT_EXE%" "%DELPHI_VERSION%" IsPackageRegistered DptIdeExpert > nul 2>&1
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo NOTE: The package has been created but not yet registered in the IDE.
    echo To register it, run: _DptIdeExpert.RegisterBpl.bat %DELPHI_VERSION%
    echo Or manually via: Component -^> Install Packages.
)

:skip_check
echo.

popd
endlocal
