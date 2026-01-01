@echo off
chcp 65001 > nul
setlocal

set "SCRIPT_DIR=%~dp0"
set "EXE_PATH=%SCRIPT_DIR%TmplCodeGen.exe"
set "BUILD_SCRIPT=%SCRIPT_DIR%Source\_TmplCodeGen.Build.bat"

set "DO_BUILD=0"

if not exist "%EXE_PATH%" (
    echo TmplCodeGen.exe not found.
    set "DO_BUILD=1"
) else (
    rem Check if the file is older than 5 minutes
    powershell -NoProfile -Command "if ((Get-Item '%EXE_PATH%').LastWriteTime -lt (Get-Date).AddMinutes(-5)) { exit 1 } else { exit 0 }"
    if errorlevel 1 (
        echo TmplCodeGen.exe is older than 5 minutes.
        set "DO_BUILD=1"
    )
)

if "%DO_BUILD%"=="1" (
    echo Starting build process...
    call "%BUILD_SCRIPT%"
    if errorlevel 1 (
        echo Build failed.
        exit /b %errorlevel%
    )
)

rem Execute the exe with all parameters
"%EXE_PATH%" %*
