// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
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

  Test.Base,
  Test.Logger,
  TmplCodeGen.Common,
  TmplCodeGen.Generator,
  TmplCodeGen.Logger;

type

  [TestFixture]
  TTestTmplCodeGen = class(TTestBase)
  private
    FTestTemplatesPath: String;
    function  CountOccurrences(const AText, ASubString: String): Integer;
    procedure CreateCaseAMockEnvironment;
    procedure ExecuteTmplCodeGen(const APrefix: String);
  public
    [Setup]
    procedure Setup; override;
    [Test]
    procedure TestProcessCaseA;
    [Test]
    procedure TestProcessPostFixParamsDefine;
  end;

implementation

{ TTestTmplCodeGen }

procedure TTestTmplCodeGen.Setup;
begin
  inherited;
  FTestTemplatesPath := TPath.Combine(FTestPath, TemplatesDir);
  ForceDirectories(FTestTemplatesPath);
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

function TTestTmplCodeGen.CountOccurrences(const AText, ASubString: String): Integer;
var
  PosIdx: Integer;
begin
  Result := 0;
  PosIdx := Pos(ASubString, AText);
  while PosIdx > 0 do
  begin
    Inc(Result);
    PosIdx := Pos(ASubString, AText, PosIdx + Length(ASubString));
  end;
end;

procedure TTestTmplCodeGen.CreateCaseAMockEnvironment;
begin
  // Base.Dictionary-conf.json
  TFile.WriteAllText(
    TPath.Combine(FTestPath, 'Base.Dictionary' + ConfJsonFileApndx), '''
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
    TPath.Combine(FTestPath, 'Base.List' + ConfJsonFileApndx),  '''
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
    TPath.Combine(FTestTemplatesPath, 'Base.Collections.Dictionary.TMPL.pas'), '''
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
    TPath.Combine(FTestTemplatesPath, 'Base.Collections.List.TMPL.pas'), '''
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

procedure TTestTmplCodeGen.TestProcessPostFixParamsDefine;
begin
  TFile.WriteAllText(TPath.Combine(FTestPath, 'TestProj' + ConfJsonFileApndx),
    '{ "Template": "Main.pas.tmpl", "Data": "Hello World" }');

  TFile.WriteAllText(TPath.Combine(FTestTemplatesPath, 'Main.pas.tmpl'), '''
    unit Test;
    interface

    procedure Run(); // PostFixParamsDefine
    procedure SecondProc(const AFirst: String;); // PostFixParamsDefine

    implementation

    procedure Run(); // PostFixParamsDefine
    begin
    end;

    procedure SecondProc(const AFirst: String;); // PostFixParamsDefine
    begin
    end;

    end.
    ''');

  ExecuteTmplCodeGen('TestProj');
  var OutputFile := 'TestProj' + OutputApndx;
  Assert.IsTrue(TFile.Exists(OutputFile), 'Output file should be created');

  var Content := TFile.ReadAllText(OutputFile);

  // PostFixParamsDefine should remove empty parentheses
  Assert.IsFalse(Content.Contains('procedure Run();'), 'PostFix should have removed parentheses');
  Assert.AreEqual(2, CountOccurrences(Content, 'procedure Run;'), 'Should occur twice (interface and implementation)');

  // PostFixParamsDefine should remove last ";" in the argument list
  Assert.IsFalse(Content.Contains('procedure SecondProc(const AFirst: String;);'), 'PostFix should have removed semicolon');
  Assert.AreEqual(2, CountOccurrences(Content, 'procedure SecondProc(const AFirst: String);'), 'Should occur twice (interface and implementation)');
end;

procedure TTestTmplCodeGen.TestProcessCaseA;
begin
  CreateCaseAMockEnvironment;

  ExecuteTmplCodeGen('Base.Dictionary');
  Assert.IsTrue(TFile.Exists('Base.Dictionary.pas'), 'Base.Dictionary.pas should exist');
  Assert.IsTrue(TFile.Exists('Base.Dictionary-interface.part.pas'), 'Base.Dictionary-interface.part.pas should exist');

  ExecuteTmplCodeGen('Base.List');
  Assert.IsTrue(TFile.Exists('Base.List.pas'), 'Base.List.pas should exist');
  Assert.IsTrue(TFile.Exists('Base.List-interface.part.pas'), 'Base.List-interface.part.pas should exist');
end;

initialization

TDUnitX.RegisterTestFixture(TTestTmplCodeGen);

end.
