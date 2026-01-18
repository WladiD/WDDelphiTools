@echo off
chcp 65001 > nul
REM ==================================================================================
REM Base build script for WDDelphiTools projects.
REM Usage: call ..\_BuildBase.bat ProjectFile.dproj [Platform] [Config] [DelphiVersion] [ExtraMSBuildParams]
REM
REM Parameters:
REM   1. ProjectFile (Required): Path to the .dproj file to build.
REM   2. Platform    (Optional): Target platform (e.g., Win32, Win64). Default: Win32.
REM   3. Config      (Optional): Build configuration (e.g., Debug, Release). Default: Debug.
REM   4. DelphiVer   (Optional): Delphi version alias (e.g., D12, D11, RECENT). Default: RECENT.
REM                              Used by DPT to locate the environment.
REM   5. ExtraParams (Optional): Additional arguments passed directly to MSBuild.
REM                              (e.g., "/t:Clean;Build" or "/p:DCC_Define=MYDEF")
REM ==================================================================================

if "%~1"=="" (
    echo ERROR: Please specify the project file ^(.dproj^) as the first parameter.
    exit /b 1
)

set "PROJECT_FILE=%~1"
set "BUILD_PLATFORM=Win32"
if not "%~2"=="" set "BUILD_PLATFORM=%~2"

set "BUILD_CONFIG=Debug"
if not "%~3"=="" set "BUILD_CONFIG=%~3"

set "DELPHI_VERSION=RECENT"
if not "%~4" == "" set "DELPHI_VERSION=%~4"
set "EXTRA_MSBUILD_PARAMS=%~5"

REM Default/Fallback path to Delphi 12
set "RSVARS_PATH=C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
set "DPT_EXE=%~dp0Projects\DPT\DPT.exe"

if not exist "%DPT_EXE%" goto :no_dpt

echo Detecting Delphi path via DPT %DELPHI_VERSION%...
for /f "usebackq delims=" %%I in (`"%DPT_EXE%" %DELPHI_VERSION% PrintPath BdsBinPath`) do (
    if exist "%%I\rsvars.bat" (
        set "RSVARS_PATH=%%I\rsvars.bat"
        echo Found: %%I
        goto :env_ready
    )
)

:no_dpt
echo WARNING: DPT not found or version %DELPHI_VERSION% not located.
echo Using fallback path: %RSVARS_PATH%

if not exist "%RSVARS_PATH%" (
    echo ERROR: Delphi environment not found.
    exit /b 1
)

:env_ready
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
echo BDS is: "%BDS%"
echo PRODUCTVERSION is: "%PRODUCTVERSION%"
echo.

echo Building %PROJECT_FILE%...
msbuild "%PROJECT_FILE%" /t:Build /p:Configuration=%BUILD_CONFIG%;Config=%BUILD_CONFIG%;Platform=%BUILD_PLATFORM%;PRODUCTVERSION=%PRODUCTVERSION% %EXTRA_MSBUILD_PARAMS%

if %ERRORLEVEL% neq 0 (
    echo ERROR: Failed to build the project.
    exit /b %ERRORLEVEL%
)

echo Build successful.
