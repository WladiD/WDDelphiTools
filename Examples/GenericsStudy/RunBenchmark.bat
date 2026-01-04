@echo off
setlocal
pushd %~dp0

echo. 
echo ==============================================================================
echo  BENCHMARK: StaticGenerics vs Delphi vs Spring vs mORMot
echo ==============================================================================
echo. 

set SEARCH_PATH_SHARED=-I"..\Shared" -U"..\Shared"
set SEARCH_PATH_MORMOT=-I"..\..\..\Lib\mORMot\src" -U"..\..\..\Lib\mORMot\src"
set SEARCH_PATH_STATIC=-I"..\StaticGenerics\Source" -U"..\StaticGenerics\Source"

rem mORMot paths
set MORMOT_PATH=..\..\..\Lib\mORMot
set MORMOT_UNITS="%MORMOT_PATH%\src\core";"%MORMOT_PATH%\src\lib";"%MORMOT_PATH%\src\net";"%MORMOT_PATH%\src\orm";"%MORMOT_PATH%\src\db";"%MORMOT_PATH%\src\app"

rem Spring4D path
set SPRING_BASE=C:\WC\spring4d
set SEARCH_PATH_SPRING=-I"%SPRING_BASE%\Source" -U"%SPRING_BASE%\Source";"%SPRING_BASE%\Source\Base";"%SPRING_BASE%\Source\Base\Collections";"%SPRING_BASE%\Source\Core\Container";"%SPRING_BASE%\Source\Core\Services"
set DCC_CMD=dcc32 -B -Q -NS"System;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell"

echo [1/4] Compiling StaticGenerics...
cd StaticGenerics
rem Using MSBuild via _BuildBase.bat for StaticGenerics to leverage dproj configuration
call ..\..\..\_BuildBase.bat "Benchmark_StaticGenerics.dproj" > ..\build_static.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_static.log
    type ..\build_static.log
) else (
    rem MSBuild output is verbose, we can try to find lines count if reported by compiler hint
    findstr "lines," ..\build_static.log
)
cd ..

echo. 

echo [2/4] Compiling mORMotGenerics...
cd mORMotGenerics
%DCC_CMD% %SEARCH_PATH_SHARED% -U%MORMOT_UNITS% Benchmark_mORMotGenerics.dpr > ..\build_mormot.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_mormot.log
    type ..\build_mormot.log
) else (
    findstr "lines," ..\build_mormot.log
)
cd ..

echo. 

echo [3/4] Compiling DelphiGenerics...
cd DelphiGenerics
call ..\..\..\_BuildBase.bat "Benchmark_DelphiGenerics.dproj" > ..\build_delphi.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_delphi.log
    type ..\build_delphi.log
) else (
    findstr "lines," ..\build_delphi.log
)
cd ..

echo. 

echo [4/4] Compiling SpringGenerics...
cd SpringGenerics
%DCC_CMD% %SEARCH_PATH_SHARED% %SEARCH_PATH_SPRING% Benchmark_SpringGenerics.dpr > ..\build_spring.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_spring.log
    type ..\build_spring.log
) else (
    findstr "lines," ..\build_spring.log
)
cd ..

echo. 

echo Benchmark Complete.
popd