// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.DProjAnalyzer;

interface

uses

  Winapi.Windows,

  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  DPT.DProjAnalyzer;

type

  [TestFixture]
  TTestDProjAnalyzer = class
  private
    FTestFile: String;
    procedure CreateDProj(const Content: String);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;

    [Test]
    procedure GetConfigs;
    [Test]
    procedure GetDefaultConfig;
    [Test]
    procedure GetProjectSearchPath_Inheritance;
    [Test]
    procedure GetProjectSearchPath_Overwrite;
    [Test]
    procedure GetProjectFiles;
    [Test]
    procedure GetProjectOutputFile_Default;
    [Test]
    procedure GetProjectOutputFile_SimpleOutput;
    [Test]
    procedure GetProjectOutputFile_ConfigCondition;
    [Test]
    procedure GetProjectOutputFile_PlatformCondition;
    [Test]
    procedure GetProjectOutputFile_Variables;
    [Test]
    procedure GetProjectOutputFile_MultipleGroups;
    [Test]
    procedure GetProjectOutputFile_ComplexConditions;
    [Test]
    procedure GetProjectOutputFile_CustomVariableWithDefault;
    [Test]
    procedure GetProjectOutputFile_CustomVariableOverridden;
    [Test]
    procedure GetProjectOutputFile_ExeOutputElementCondition;
    [Test]
    procedure GetProjectOutputFile_EnvironmentVariableFallback;
  end;

implementation

{ TTestDProjAnalyzer }

procedure TTestDProjAnalyzer.Setup;
begin
  FTestFile := TPath.Combine(TPath.GetTempPath, 'TestProject.dproj');
end;

procedure TTestDProjAnalyzer.Teardown;
begin
  if FileExists(FTestFile) then
    TFile.Delete(FTestFile);
end;

procedure TTestDProjAnalyzer.CreateDProj(const Content: String);
begin
  TFile.WriteAllText(FTestFile, Content);
end;

procedure TTestDProjAnalyzer.GetConfigs;
var
  Analyzer: TDProjAnalyzer;
  Configs : TArray<String>;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <ItemGroup>
        <BuildConfiguration Include="Base">
          <Key>Base</Key>
        </BuildConfiguration>
        <BuildConfiguration Include="Release">
          <Key>Cfg_1</Key>
          <CfgParent>Base</CfgParent>
        </BuildConfiguration>
        <BuildConfiguration Include="Debug">
          <Key>Cfg_2</Key>
          <CfgParent>Base</CfgParent>
        </BuildConfiguration>
      </ItemGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Configs := Analyzer.GetConfigs;
    Assert.AreEqual(3, Length(Configs));
    Assert.AreEqual('Base', Configs[0]);
    Assert.AreEqual('Debug', Configs[1]);
    Assert.AreEqual('Release', Configs[2]);
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetDefaultConfig;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <Config Condition="'$(Config)'==''">Release</Config>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual('Release', Analyzer.GetDefaultConfig);
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectSearchPath_Inheritance;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_UnitSearchPath>BaseDir</DCC_UnitSearchPath>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Debug'">
        <DCC_UnitSearchPath>$(DCC_UnitSearchPath);DebugDir</DCC_UnitSearchPath>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual('BaseDir;DebugDir', Analyzer.GetProjectSearchPath('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectSearchPath_Overwrite;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_UnitSearchPath>BaseDir</DCC_UnitSearchPath>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Release'">
        <DCC_UnitSearchPath>ReleaseDir</DCC_UnitSearchPath>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual('ReleaseDir', Analyzer.GetProjectSearchPath('Release', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectFiles;
var
  Analyzer: TDProjAnalyzer;
  Files   : TArray<String>;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <ItemGroup>
        <DCCReference Include="Unit1.pas"/>
        <DCCReference Include="Sub\Unit2.pas"/>
      </ItemGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Files := Analyzer.GetProjectFiles;
    Assert.AreEqual(2, Length(Files));
    Assert.IsTrue(Files[0].EndsWith('Unit1.pas'));
    Assert.IsTrue(Files[1].EndsWith('Unit2.pas'));
    Assert.IsTrue(TPath.IsPathRooted(Files[0]));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_Default;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003"></Project>');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_SimpleOutput;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>.\Bin</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Bin\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_ConfigCondition;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup Condition="'$(Config)'=='Debug'">
        <DCC_ExeOutput>.\DebugBin</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Release'">
        <DCC_ExeOutput>.\ReleaseBin</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');

  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'DebugBin\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'ReleaseBin\TestProject.exe')), Analyzer.GetProjectOutputFile('Release', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_PlatformCondition;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup Condition="'$(Platform)'=='Win32'">
        <DCC_ExeOutput>.\Win32Bin</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Platform)'=='Win64'">
        <DCC_ExeOutput>.\Win64Bin</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');

  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Win32Bin\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Win64Bin\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win64'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_Variables;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>.\$(Platform)\$(Config)</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
  ''');

  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Win32\Debug\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_MultipleGroups;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>.\Base</DCC_ExeOutput>
      </PropertyGroup>
      <PropertyGroup Condition="'$(Config)'=='Debug'">
        <DCC_ExeOutput>.\Debug</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');

  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Debug\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Base\TestProject.exe')), Analyzer.GetProjectOutputFile('Release', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_ComplexConditions;
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
        <PropertyGroup>
            <Base>true</Base>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Config)'=='Debug' or '$(Cfg_1)'!=''">
            <Cfg_1>true</Cfg_1>
            <CfgParent>Base</CfgParent>
            <Base>true</Base>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Config)'=='Release' or '$(Cfg_2)'!=''">
            <Cfg_2>true</Cfg_2>
            <CfgParent>Base</CfgParent>
            <Base>true</Base>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Base)'!=''">
            <DCC_ExeOutput>.\BaseOutput</DCC_ExeOutput>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Cfg_1)'!=''">
            <DCC_ExeOutput>.\DebugOutput</DCC_ExeOutput>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Cfg_2)'!=''">
            <DCC_ExeOutput>.\ReleaseOutput</DCC_ExeOutput>
        </PropertyGroup>
    </Project>
    ''');

  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'DebugOutput\TestProject.exe')), Analyzer.GetProjectOutputFile('Debug', 'Win32'));
    Assert.AreEqual(ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'ReleaseOutput\TestProject.exe')), Analyzer.GetProjectOutputFile('Release', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_CustomVariableWithDefault;
var
  Analyzer: TDProjAnalyzer;
begin
  // Mirrors the TAIFUN_TEST_DIR pattern: a custom property declares its own
  // default via a Condition, and DCC_ExeOutput references the variable.
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <CUSTOM_DIR Condition="'$(CUSTOM_DIR)'==''">.\Default</CUSTOM_DIR>
        <DCC_ExeOutput>$(CUSTOM_DIR)</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(
      ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Default\TestProject.exe')),
      Analyzer.GetProjectOutputFile('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_CustomVariableOverridden;
var
  Analyzer: TDProjAnalyzer;
begin
  // When the custom property already has a value (set earlier in the file or
  // by an outer group), the conditional default must not overwrite it.
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <CUSTOM_DIR>.\Explicit</CUSTOM_DIR>
        <CUSTOM_DIR Condition="'$(CUSTOM_DIR)'==''">.\Default</CUSTOM_DIR>
        <DCC_ExeOutput>$(CUSTOM_DIR)</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(
      ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Explicit\TestProject.exe')),
      Analyzer.GetProjectOutputFile('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_ExeOutputElementCondition;
var
  Analyzer: TDProjAnalyzer;
begin
  // <DCC_ExeOutput Condition="..."> with element-level conditions:
  // only the matching one wins; the non-matching one must be ignored.
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput Condition="'$(OVERRIDE)'!=''">.\Override</DCC_ExeOutput>
        <DCC_ExeOutput Condition="'$(OVERRIDE)'==''">.\Fallback</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual(
      ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'Fallback\TestProject.exe')),
      Analyzer.GetProjectOutputFile('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.GetProjectOutputFile_EnvironmentVariableFallback;
const
  EnvVarName = 'DPT_TEST_OUTPUT_DIR_F1A2B3';
var
  Analyzer: TDProjAnalyzer;
begin
  // Variables not declared in the dproj must fall back to the process
  // environment so MSBuild-style external configuration keeps working.
  CreateDProj('''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
      <PropertyGroup>
        <DCC_ExeOutput>$(DPT_TEST_OUTPUT_DIR_F1A2B3)</DCC_ExeOutput>
      </PropertyGroup>
    </Project>
    ''');
  SetEnvironmentVariable(PChar(EnvVarName), PChar('.\FromEnv'));
  try
    Analyzer := TDProjAnalyzer.Create(FTestFile);
    try
      Assert.AreEqual(
        ExpandFileName(TPath.Combine(ExtractFilePath(FTestFile), 'FromEnv\TestProject.exe')),
        Analyzer.GetProjectOutputFile('Debug', 'Win32'));
    finally
      Analyzer.Free;
    end;
  finally
    SetEnvironmentVariable(PChar(EnvVarName), nil);
  end;
end;

end.
