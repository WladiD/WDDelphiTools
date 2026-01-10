@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"
REM The Exe output path depends on the Delphi version and configuration, but _BuildBase defaults to Win32/Debug structure usually
REM if the dproj settings follow standard. Assuming Win32\Debug based on previous script.
call "%PROJ_DIR%..\..\..\_BuildAndRunBase.bat" "%PROJ_DIR%Win32\Debug\Test.TmplCodeGen.exe" "%PROJ_DIR%_Test.TmplCodeGen.Build.bat" %*
endlocal