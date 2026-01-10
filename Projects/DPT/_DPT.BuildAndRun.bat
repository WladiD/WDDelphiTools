@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"
call "%PROJ_DIR%..\..\_BuildAndRunBase.bat" "%PROJ_DIR%DPT.exe" "%PROJ_DIR%_DPT.Build.bat" %*
endlocal