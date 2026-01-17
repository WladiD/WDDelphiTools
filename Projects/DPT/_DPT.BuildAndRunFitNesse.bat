@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"
set "BUILD_CONFIG=FitNesse"
call "%PROJ_DIR%_DPT.BuildAndRun.bat"
endlocal
