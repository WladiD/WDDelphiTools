@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"
call "%PROJ_DIR%..\..\_BuildAndRunBase.bat" "%PROJ_DIR%DcuCompileTimes.exe" "%PROJ_DIR%_DcuCompileTimes.Build.bat" %*
endlocal