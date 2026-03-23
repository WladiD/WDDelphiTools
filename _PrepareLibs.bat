@echo off
setlocal
chcp 65001 > nul

echo ======================================================================
echo Preparing Libraries for WDDelphiTools
echo ======================================================================
echo.

set "JCL_INC_DIR=%~dp0Lib\jcl\jcl\source\include"

if exist "%JCL_INC_DIR%\jcl.template.inc" (
    if not exist "%JCL_INC_DIR%\jcld37win32.inc" (
        echo [JCL] Generating missing include files for Delphi 13.1...
        copy /y "%JCL_INC_DIR%\jcl.template.inc" "%JCL_INC_DIR%\jcld37win32.inc" > nul
        copy /y "%JCL_INC_DIR%\jcl.template.inc" "%JCL_INC_DIR%\jcld37win64.inc" > nul
        echo [JCL] jcld37win32.inc and jcld37win64.inc created successfully.
    ) else (
        echo [JCL] Include files for Delphi 13.1 already exist.
    )
) else (
    echo [JCL] WARNING: JCL template not found. Did you initialize the submodules?
    echo Try running: git submodule update --init --recursive
)

echo.
echo ======================================================================
echo Done.
echo ======================================================================
endlocal
