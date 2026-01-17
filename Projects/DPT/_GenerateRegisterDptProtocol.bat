@echo off
setlocal EnableDelayedExpansion

set "TARGET_DIR=%~dp0"
if "%TARGET_DIR:~-1%"=="\" set "TARGET_DIR=%TARGET_DIR:~0,-1%"

set "EXE_PATH=%TARGET_DIR%\DPT.exe"
set "ESCAPED_PATH=!EXE_PATH:\=\\!"

set "OUTPUT_FILE=%~dp0RegisterDptProtocol.reg"

(
echo Windows Registry Editor Version 5.00
echo.
echo [HKEY_CLASSES_ROOT\dpt]
echo @="URL:DPT Protocol"
echo "URL Protocol"=""
echo.
echo [HKEY_CLASSES_ROOT\dpt\shell]
echo.
echo [HKEY_CLASSES_ROOT\dpt\shell\open]
echo.
echo [HKEY_CLASSES_ROOT\dpt\shell\open\command]
echo @="\"!ESCAPED_PATH!\" RECENT HandleProtocol \"%%1\""
) > "%OUTPUT_FILE%"

echo Generated %OUTPUT_FILE%:
echo ---------------------------------------------------
type "%OUTPUT_FILE%"
echo ---------------------------------------------------
echo.
echo You can now execute the .reg file to register the protocol.
pause

