// ======================================================================
// Copyright (c) 2025 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.Generator;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  TmplCodeGen.Common,
  TmplCodeGen.Generator,
  TmplCodeGen.Logger;

type

  TTestLogger = class(TInterfacedObject, ILogger)
  public
    LogMessages: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Log(const AMessage: String);
  end;

  [TestFixture]
  TTestTmplCodeGen = class
  private
    FLogger : ILogger;
    FTestDir: String;
    FPrefix : String;
    procedure CreateMockEnvironment;
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure TestProcessTemplate;
  end;

implementation

{ TTestLogger }

constructor TTestLogger.Create;
begin
  inherited Create;
  LogMessages := TStringList.Create;
end;

destructor TTestLogger.Destroy;
begin
  LogMessages.Free;
  inherited;
end;

procedure TTestLogger.Log(const AMessage: String);
begin
  LogMessages.Add(AMessage);
end;

{ TTestTmplCodeGen }

procedure TTestTmplCodeGen.Setup;
begin
  FLogger := TTestLogger.Create;
  FTestDir := TPath.Combine(TPath.GetTempPath, 'TmplCodeGenTest_' + TGuid.NewGuid.ToString);
  ForceDirectories(FTestDir);
  FPrefix := TPath.Combine(FTestDir, 'TestProj');
end;

procedure TTestTmplCodeGen.Teardown;
begin
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
  FLogger := nil;
end;

procedure TTestTmplCodeGen.CreateMockEnvironment;
begin
  // Create TEMPLATES directory
  var TemplatesPath := TPath.Combine(FTestDir, TemplatesDir);
  ForceDirectories(TemplatesPath);

  // Create Mock Conf Json
  var ConfJson := '{ "Template": "Main.pas.tmpl", "Data": "Hello World" }';
  TFile.WriteAllText(FPrefix + ConfJsonFileApndx, ConfJson);

  // Create Mock Template
  var TemplateContent := 'unit Test; interface // PostFixParamsDefine' + sLineBreak +
                         'procedure Run(); // PostFixParamsDefine' + sLineBreak +
                         'implementation end.';
  TFile.WriteAllText(TPath.Combine(TemplatesPath, 'Main.pas.tmpl'), TemplateContent);
end;

procedure TTestTmplCodeGen.TestProcessTemplate;
begin
  CreateMockEnvironment;

  // Change current directory to FTestDir because TTmplCodeGen uses relative path for TEMPLATES
  var OldDir := TDirectory.GetCurrentDirectory;
  TDirectory.SetCurrentDirectory(FTestDir);
  try
    var Generator := TTmplCodeGen.Create('TestProj', FLogger);
    try
      Generator.ProcessTemplate;
    finally
      Generator.Free;
    end;

    var OutputFile := 'TestProj' + OutputApndx;
    Assert.IsTrue(TFile.Exists(OutputFile), 'Output file should be created');

    var Content := TFile.ReadAllText(OutputFile);
    // PostFixParamsDefine should remove empty parentheses
    Assert.IsFalse(Content.Contains('procedure Run();'), 'PostFix should have removed parentheses');
    Assert.IsTrue(Content.Contains('procedure Run;'), 'PostFix should have corrected procedure declaration');
  finally
    TDirectory.SetCurrentDirectory(OldDir);
  end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestTmplCodeGen);

end.
