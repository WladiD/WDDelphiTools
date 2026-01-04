@echo off

REM Adjusted path to TmplCodeGen
set TOOL_DIR=..\..\..\Projects\TmplCodeGen

%TOOL_DIR%\_TmplCodeGen.BuildAndRun.bat Study.List || goto :error
%TOOL_DIR%\TmplCodeGen.exe Study.Dictionary || goto :error

%TOOL_DIR%\TmplCodeGen.exe include_partials Source\Study.Collections.Factory.pas || goto :error
%TOOL_DIR%\TmplCodeGen.exe include_partials Source\Study.Collections.Interfaces.pas || goto :error

REM Delete temp files
del Study.List.pas
del Study.Dictionary.pas
del Study.*.part.pas

echo Generation Complete.
goto :EOF

:error
echo Failed with error %errorlevel%.
:pause
