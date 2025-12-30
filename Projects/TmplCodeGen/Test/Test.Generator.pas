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
    FOldDir : String;
    FTestDir: String;
    FPrefix : String;
    procedure CreateCaseAMockEnvironment;
    procedure CreateMockEnvironment;
    procedure ExecuteTmplCodeGen(const APrefix: String);
  public
    [Setup]
    procedure Setup;
    [Teardown]
    procedure Teardown;
    [Test]
    procedure TestProcessTemplate;
    [Test]
    procedure TestProcessCaseA;
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
  
  FOldDir := TDirectory.GetCurrentDirectory;
  TDirectory.SetCurrentDirectory(FTestDir);
end;

procedure TTestTmplCodeGen.Teardown;
begin
  TDirectory.SetCurrentDirectory(FOldDir);
  
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

procedure TTestTmplCodeGen.ExecuteTmplCodeGen(const APrefix: String);
begin
  var Generator := TTmplCodeGen.Create(APrefix, FLogger);
  try
    Generator.ProcessTemplate;
  finally
    Generator.Free;
  end;
end;

procedure TTestTmplCodeGen.CreateCaseAMockEnvironment;
begin
  var TemplatesPath := TPath.Combine(FTestDir, TemplatesDir);
  ForceDirectories(TemplatesPath);

  // Base.Dictionary-conf.json
  TFile.WriteAllText(
    TPath.Combine(FTestDir, 'Base.Dictionary' + ConfJsonFileApndx), '''
    {
      "Template": "Base.Collections.Dictionary.TMPL.pas",
      "TCollectionsName": "CCollections",
      "types": [
        {
          "key_type": "Integer",
          "key_flat": "Integer",
          "value_type": "String",
          "value_flat": "String",
          "disable_key_interfaces": true,
          "disable_value_interfaces": true
        }
      ]
    }
    ''');

  // Base.List-conf.json
  TFile.WriteAllText(
    TPath.Combine(FTestDir, 'Base.List' + ConfJsonFileApndx),  '''
    {
      "Template": "Base.Collections.List.TMPL.pas",
      "TCollectionsName": "CCollections",
      "types": [
        {
          "type": "Integer",
          "type_flat": "Integer"
        }
      ]
    }
    ''');

  TFile.WriteAllText(
    TPath.Combine(TemplatesPath, 'Base.Collections.Dictionary.TMPL.pas'), '''
    unit Base.Dictionary;
    interface
    {$REGION 'DEFINE-PARTIAL / interface'}
    type
      MyDictionary = class end;
    {$ENDREGION 'DEFINE-PARTIAL / interface'}
    implementation
    end.
    ''');

  TFile.WriteAllText(
    TPath.Combine(TemplatesPath, 'Base.Collections.List.TMPL.pas'), '''
    unit Base.List;
    interface
    {$REGION 'DEFINE-PARTIAL / interface'}
    type
      MyList = class end;
    {$ENDREGION 'DEFINE-PARTIAL / interface'}
    implementation
    end.
    ''');
end;

procedure TTestTmplCodeGen.TestProcessTemplate;
begin
  CreateMockEnvironment;

  ExecuteTmplCodeGen('TestProj');
  var OutputFile := 'TestProj' + OutputApndx;
  Assert.IsTrue(TFile.Exists(OutputFile), 'Output file should be created');

  var Content := TFile.ReadAllText(OutputFile);
  // PostFixParamsDefine should remove empty parentheses
  Assert.IsFalse(Content.Contains('procedure Run();'), 'PostFix should have removed parentheses');
  Assert.IsTrue(Content.Contains('procedure Run;'), 'PostFix should have corrected procedure declaration');
end;

procedure TTestTmplCodeGen.TestProcessCaseA;
begin
  CreateCaseAMockEnvironment;

  // Process Base.Dictionary
  ExecuteTmplCodeGen('Base.Dictionary');
  Assert.IsTrue(TFile.Exists('Base.Dictionary.pas'), 'Base.Dictionary.pas should exist');
  Assert.IsTrue(TFile.Exists('Base.Dictionary-interface.part.pas'), 'Base.Dictionary-interface.part.pas should exist');

  // Process Base.List
  ExecuteTmplCodeGen('Base.List');
  Assert.IsTrue(TFile.Exists('Base.List.pas'), 'Base.List.pas should exist');
  Assert.IsTrue(TFile.Exists('Base.List-interface.part.pas'), 'Base.List-interface.part.pas should exist');
end;

initialization

TDUnitX.RegisterTestFixture(TTestTmplCodeGen);

end.
