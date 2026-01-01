@echo off
chcp 65001 > nul
REM ==================================================================================
REM Base build script for SlimForDelphi Example projects.
REM Usage: call ..\_BuildBase.bat ProjectFile.dproj
REM Expects to be called from the project directory.
REM ==================================================================================

if "%~1"=="" (
    echo ERROR: Please specify the project file (.dproj^) as the first parameter.
    exit /b 1
)

set "PROJECT_FILE=%~1"
set "BUILD_PLATFORM=Win32"
if not "%~2"=="" set "BUILD_PLATFORM=%~2"

REM Static path to Delphi 12!
set "RSVARS_PATH=C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"

for %%i in ("%RSVARS_PATH%\..\..") do set "PRODUCTVERSION=%%~nxi"

if not defined PRODUCTVERSION (
    echo ERROR: Could not determine PRODUCTVERSION from RSVARS_PATH.
    exit /b 1
)

echo Setting up Delphi environment...
call "%RSVARS_PATH%"

if %ERRORLEVEL% neq 0 (
    echo ERROR: Failed to set up Delphi environment.
    exit /b %ERRORLEVEL%
)

echo.
echo BDS environment variable is: "%BDS%"
echo PRODUCTVERSION is: "%PRODUCTVERSION%"
echo.

echo Building %PROJECT_FILE%...
msbuild "%PROJECT_FILE%" /t:Build /p:Configuration=Debug;Platform=%BUILD_PLATFORM%;PRODUCTVERSION=%PRODUCTVERSION%;DCC_Define=DEBUG

if %ERRORLEVEL% neq 0 (
    echo ERROR: Failed to build the project.
    exit /b %ERRORLEVEL%
)

echo Build successful.