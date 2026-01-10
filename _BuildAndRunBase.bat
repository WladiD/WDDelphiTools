@echo off
chcp 65001 > nul
setlocal

REM ==================================================================================
REM Base BuildAndRun script for Delphi Example projects.
REM Usage: call ..\_BuildAndRunBase.bat ExeName.exe BuildScript.bat [Args for Exe...]
REM Expects to be called from the project directory.
REM ==================================================================================

if "%~1"=="" (
    echo ERROR: Please specify the executable name as the first parameter.
    exit /b 1
)

if "%~2"=="" (
    echo ERROR: Please specify the build script name as the second parameter.
    exit /b 1
)

set "EXE_NAME=%~1"
set "BUILD_SCRIPT_NAME=%~2"
set "SCRIPT_DIR=%CD%\"
set "EXE_PATH=%SCRIPT_DIR%%EXE_NAME%"
set "BUILD_SCRIPT=%SCRIPT_DIR%%BUILD_SCRIPT_NAME%"

set "DO_BUILD=0"

if not exist "%EXE_PATH%" (
    echo %EXE_NAME% not found.
    set "DO_BUILD=1"
) else (
    rem Check if the file is older than 5 minutes
    powershell -NoProfile -Command "if ((Get-Item '%EXE_PATH%').LastWriteTime -lt (Get-Date).AddMinutes(-5)) { exit 1 } else { exit 0 }"
    if errorlevel 1 (
        echo %EXE_NAME% is older than 5 minutes.
        set "DO_BUILD=1"
    )
)

if "%DO_BUILD%"=="1" (
    echo Starting build process...
    if not exist "%BUILD_SCRIPT%" (
        echo ERROR: Build script "%BUILD_SCRIPT%" not found.
        exit /b 1
    )
    call "%BUILD_SCRIPT%"
    if errorlevel 1 (
        echo Build failed.
        exit /b %errorlevel%
    )
)

rem Shift arguments to pass remaining parameters to the exe
shift
shift

rem Execute the exe with remaining parameters
"%EXE_PATH%" %1 %2 %3 %4 %5 %6 %7 %8 %9
