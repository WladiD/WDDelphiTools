@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

REM Create a shadow copy to avoid file locking during self-build (DPT.exe overwriting itself)
copy /Y "DPT.exe" "_DPT_Builder.exe" > nul

echo.
echo ------------------------------------------
echo  Building DPT using DPT (Self-Hosting)
echo ------------------------------------------
echo.

"_DPT_Builder.exe" RECENT Build "DPT.dproj" Win32 %BUILD_CONFIG%
set BUILD_ERROR=%ERRORLEVEL%

REM Clean up
del "_DPT_Builder.exe" > nul

if %BUILD_ERROR% neq 0 (
    echo Build failed.
    popd
    exit /b %BUILD_ERROR%
)

popd
endlocal