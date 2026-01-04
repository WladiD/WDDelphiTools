@echo off

REM Adjusted path to TmplCodeGen
set TOOL_DIR=..\..\..\Projects\TmplCodeGen

%TOOL_DIR%\TmplCodeGen.exe System.List || goto :error
%TOOL_DIR%\TmplCodeGen.exe System.Dictionary || goto :error

%TOOL_DIR%\TmplCodeGen.exe include_partials Source\System.Collections.Factory.pas || goto :error
%TOOL_DIR%\TmplCodeGen.exe include_partials Source\System.Collections.Interfaces.pas || goto :error

REM Delete temp files
del System.List.pas
del System.Dictionary.pas
del System.*.part.pas

echo Generation Complete.
goto :EOF

:error
echo Failed with error %errorlevel%.
:pause