@echo off

REM The first call goes to the batch, to ensure the exe exists
call ..\TmplCodeGen\_TmplCodeGen.BuildAndRun.bat System.Dictionary || goto :error
..\TmplCodeGen\TmplCodeGen.exe System.List || goto :error

..\TmplCodeGen\TmplCodeGen.exe include_partials Source\System.Collections.Factory.pas || goto :error
..\TmplCodeGen\TmplCodeGen.exe include_partials Source\System.Collections.Interfaces.pas || goto :error

REM Delete temp files
del System.List.pas
del System.Dictionary.pas
del System.*.part.pas

goto :EOF

:error
echo Failed with error %errorlevel%.
:pause