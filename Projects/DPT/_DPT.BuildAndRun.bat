@echo off
chcp 65001 > nul
setlocal
set "PROJ_DIR=%~dp0"

REM ==================================================================================
REM DPT Self-Hosting Build and Run Script
REM Uses DPT's BuildAndRun action to build and run itself.
REM ==================================================================================

REM 1. Bootstrap Check: Ensure DPT.exe exists.
if not exist "%PROJ_DIR%DPT.exe" (
    echo ERROR: DPT.exe not found. Cannot bootstrap self-build.
    echo Please build manually using MSBuild or restore DPT.exe.
    exit /b 1
)

REM 2. Create Shadow Copy for the Builder (prevents file locking of DPT.exe)
copy /Y "%PROJ_DIR%DPT.exe" "%PROJ_DIR%_DPT_Runner.exe" > nul

REM 3. Run BuildAndRun using the shadow copy
REM    - Use RECENT Delphi version for the build process
REM    - Build DPT.dproj
REM    - Use Win32 Platform and Debug Config (defaults)
REM    - --OnlyIfChanged: Use smart build check
REM    - -- %*: Pass all script arguments to the built DPT.exe instance

"%PROJ_DIR%_DPT_Runner.exe" RECENT BuildAndRun "%PROJ_DIR%DPT.dproj" Win32 Debug --OnlyIfChanged -- %*
set EXIT_CODE=%ERRORLEVEL%

REM 4. Cleanup
del "%PROJ_DIR%_DPT_Runner.exe" > nul

exit /b %EXIT_CODE%
endlocal
