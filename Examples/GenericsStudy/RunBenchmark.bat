@echo off
setlocal
pushd %~dp0

if exist summary.log del summary.log

echo. 
echo ==============================================================================
echo  BENCHMARK: StaticGenerics vs Delphi vs Spring vs mORMot
echo ==============================================================================
echo. 

echo [1/4] Compiling StaticGenerics...
cd StaticGenerics
call ..\..\..\_BuildBase.bat "Benchmark_StaticGenerics.dproj" > ..\build_static.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_static.log
    type ..\build_static.log
) else (
    echo. >> ..\summary.log
    echo [StaticGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_static.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_static.log
)
cd ..

echo. 

echo [2/4] Compiling mORMotGenerics...
cd mORMotGenerics
call ..\..\..\_BuildBase.bat "Benchmark_mORMotGenerics.dproj" > ..\build_mormot.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_mormot.log
    type ..\build_mormot.log
) else (
    echo. >> ..\summary.log
    echo [mORMotGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_mormot.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_mormot.log
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
    echo. >> ..\summary.log
    echo [DelphiGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_delphi.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_delphi.log
    
)
cd ..

echo. 

echo [4/4] Compiling SpringGenerics...
cd SpringGenerics
call ..\..\..\_BuildBase.bat "Benchmark_SpringGenerics.dproj" > ..\build_spring.log
if %errorlevel% neq 0 (
    echo BUILD FAILED! See build_spring.log
    type ..\build_spring.log
) else (
    echo. >> ..\summary.log
    echo [SpringGenerics] >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_spring.log >> ..\summary.log
    findstr /C:"Embarcadero Delphi for" /C:"Copyright (c)" /C:"Zeilen," /C:"lines," ..\build_spring.log
)
cd ..

echo. 

echo Benchmark Complete.
popd