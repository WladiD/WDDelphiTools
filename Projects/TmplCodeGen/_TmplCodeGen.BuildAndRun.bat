@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"
set "DPT_EXE=%PROJ_DIR%..\DPT\DPT.exe"

if not exist "%DPT_EXE%" (
    echo ERROR: DPT.exe not found at "%DPT_EXE%".
    echo Please build DPT first.
    exit /b 1
)

"%DPT_EXE%" RECENT BuildAndRun "%PROJ_DIR%Source\TmplCodeGen.dproj" Win32 Debug --OnlyIfChanged -- %*
exit /b %ERRORLEVEL%
endlocal