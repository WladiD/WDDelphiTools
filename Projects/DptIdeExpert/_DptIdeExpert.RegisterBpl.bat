@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

if "%~1"=="" (
    echo ERROR: Delphi version argument required.
    echo Usage: %~nx0 ^<DelphiVersion^>
    echo Example: %~nx0 D12
    exit /b 1
)

set "DELPHI_VERSION=%~1"
set "DPT_EXE=..\DPT\DPT.exe"

if not exist "%DPT_EXE%" (
    echo ERROR: DPT.exe not found at "%DPT_EXE%". Please build DPT first.
    exit /b 1
)

echo Detecting BPL Output Path for %DELPHI_VERSION%...
for /f "usebackq delims=" %%I in (`"%DPT_EXE%" %DELPHI_VERSION% PrintPath BPLOutputPath-Win32`) do set "BPL_PATH=%%I"

if "%BPL_PATH%"=="" (
    echo ERROR: Could not determine BPL Output Path via DPT.
    exit /b 1
)

set "BPL_FILE=%BPL_PATH%\DptIdeExpert.bpl"

if not exist "%BPL_FILE%" (
    echo ERROR: DptIdeExpert.bpl not found at "%BPL_FILE%".
    echo Please build the package first using _DptIdeExpert.Build.bat.
    exit /b 1
)

echo Registering %BPL_FILE% in %DELPHI_VERSION%...
"%DPT_EXE%" %DELPHI_VERSION% RegisterPackage "%BPL_FILE%"

if %ERRORLEVEL% neq 0 (
    echo Registration failed.
    exit /b %ERRORLEVEL%
)

echo Registration successful.
endlocal