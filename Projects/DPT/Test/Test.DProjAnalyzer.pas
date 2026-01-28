// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DProjAnalyzer;

interface

uses

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
    procedure TestGetConfigs;
    [Test]
    procedure TestGetDefaultConfig;
    [Test]
    procedure TestGetProjectSearchPath_Inheritance;
    [Test]
    procedure TestGetProjectSearchPath_Overwrite;
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

procedure TTestDProjAnalyzer.TestGetConfigs;
const
  CONTENT = 
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' +
    '  <ItemGroup>' +
    '    <BuildConfiguration Include="Base">' +
    '      <Key>Base</Key>' +
    '    </BuildConfiguration>' +
    '    <BuildConfiguration Include="Release">' +
    '      <Key>Cfg_1</Key>' +
    '      <CfgParent>Base</CfgParent>' +
    '    </BuildConfiguration>' +
    '    <BuildConfiguration Include="Debug">' +
    '      <Key>Cfg_2</Key>' +
    '      <CfgParent>Base</CfgParent>' +
    '    </BuildConfiguration>' +
    '  </ItemGroup>' +
    '</Project>';
var
  Analyzer: TDProjAnalyzer;
  Configs : TArray<String>;
begin
  CreateDProj(CONTENT);
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

procedure TTestDProjAnalyzer.TestGetDefaultConfig;
const
  CONTENT = 
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' +
    '  <PropertyGroup>' +
    '    <Config Condition="''$(Config)''==''''">Release</Config>' +
    '  </PropertyGroup>' +
    '</Project>';
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj(CONTENT);
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual('Release', Analyzer.GetDefaultConfig);
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.TestGetProjectSearchPath_Inheritance;
const
  CONTENT = 
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' +
    '  <PropertyGroup>' +
    '    <DCC_UnitSearchPath>BaseDir</DCC_UnitSearchPath>' +
    '  </PropertyGroup>' +
    '  <PropertyGroup Condition="''$(Config)''==''Debug''">' +
    '    <DCC_UnitSearchPath>$(DCC_UnitSearchPath);DebugDir</DCC_UnitSearchPath>' +
    '  </PropertyGroup>' +
    '</Project>';
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj(CONTENT);
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual('BaseDir;DebugDir', Analyzer.GetProjectSearchPath('Debug', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

procedure TTestDProjAnalyzer.TestGetProjectSearchPath_Overwrite;
const
  CONTENT = 
    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">' +
    '  <PropertyGroup>' +
    '    <DCC_UnitSearchPath>BaseDir</DCC_UnitSearchPath>' +
    '  </PropertyGroup>' +
    '  <PropertyGroup Condition="''$(Config)''==''Release''">' +
    '    <DCC_UnitSearchPath>ReleaseDir</DCC_UnitSearchPath>' +
    '  </PropertyGroup>' +
    '</Project>';
var
  Analyzer: TDProjAnalyzer;
begin
  CreateDProj(CONTENT);
  Analyzer := TDProjAnalyzer.Create(FTestFile);
  try
    Assert.AreEqual('ReleaseDir', Analyzer.GetProjectSearchPath('Release', 'Win32'));
  finally
    Analyzer.Free;
  end;
end;

end.
