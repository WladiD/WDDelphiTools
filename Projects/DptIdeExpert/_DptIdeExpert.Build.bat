@echo off
chcp 65001 > nul
setlocal
pushd %~dp0

rem Call the base build script for DptIdeExpert.dproj
rem Building in Debug configuration for development
call ..\..\_BuildBase.bat "DptIdeExpert.dproj" Win32 Debug

if %ERRORLEVEL% neq 0 (
    echo Build failed!
    popd
    exit /b %ERRORLEVEL%
)

echo.
echo Build successful. 
echo BPL should be located in the default output directory (e.g., public documents or project dir).
echo.
echo NOTE: The package has been created but not yet registered in the IDE.
echo To register it, run: _DptIdeExpert.RegisterBpl.bat ^<DelphiVersion^> (e.g. D12)
echo Or manually via: Component -^> Install Packages.
echo.

popd
endlocal
