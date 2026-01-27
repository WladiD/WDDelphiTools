// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Preprocessor;

interface

uses

  System.Classes,
  System.IOUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  TmplCodeGen.Logger,

  DPT.Preprocessor;

type

  [TestFixture]
  TTestDptPreprocessor = class
  private
    FTestDir: String;
    FInputFile: String;
    FConfFile: String;
    FTmplFile: String;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestSimpleConfig;
    [Test]
    procedure TestMultipleConfigs;
    [Test]
    procedure TestIncludePartials;
    [Test]
    procedure TestEmbeddedConfig;
  end;

implementation

procedure TTestDptPreprocessor.Setup;
begin
  FTestDir := TPath.Combine(TPath.GetTempPath, 'DptPreProcTest_' + TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTestDir);
  
  FInputFile := TPath.Combine(FTestDir, 'Input.dpr');
  FConfFile := TPath.Combine(FTestDir, 'TestConf-conf.json');
  FTmplFile := TPath.Combine(FTestDir, 'TestConf.TMPL.dproj');
end;

procedure TTestDptPreprocessor.TearDown;
begin
  if TDirectory.Exists(FTestDir) then
    TDirectory.Delete(FTestDir, True);
end;

procedure TTestDptPreprocessor.TestSimpleConfig;
var
  PreProcessor: TDptPreprocessor;
  ResultFile  : String;
begin
  // Setup files
  TFile.WriteAllText(FInputFile, 
    'program Input;' + sLineBreak +
    '// TmplCodeGen TestConf' + sLineBreak +
    'begin end.', TEncoding.UTF8);
    
  var EscapedTestDir: String := FTestDir.Replace('\', '\\', [rfReplaceAll]);
  TFile.WriteAllText(FConfFile, Format('{"Template": "TestConf.TMPL.dproj", "TemplatesPath": "%s"}', [EscapedTestDir]), TEncoding.UTF8);
  TFile.WriteAllText(FTmplFile, '<Project>Generated</Project>', TEncoding.UTF8);
  
  PreProcessor := TDptPreprocessor.Create(TSilentLogger.Create);
  try
    ResultFile := PreProcessor.Execute(FInputFile);
    
    Assert.AreEqual(TPath.Combine(FTestDir, 'TestConf.dproj'), ResultFile, 'Should return generated dproj');
    Assert.IsTrue(FileExists(ResultFile), 'Generated file should exist');
    Assert.AreEqual('<Project>Generated</Project>', TFile.ReadAllText(ResultFile), 'Content mismatch');
  finally
    PreProcessor.Free;
  end;
end;

procedure TTestDptPreprocessor.TestMultipleConfigs;
var
  Conf2File   : String;
  PreProcessor: TDptPreprocessor;
  ResultFile  : String;
  Tmpl2File   : String;
begin
  Conf2File := TPath.Combine(FTestDir, 'TestConf2-conf.json');
  Tmpl2File := TPath.Combine(FTestDir, 'TestConf2.TMPL.dproj');

  TFile.WriteAllText(FInputFile, 
    '// TmplCodeGen TestConf' + sLineBreak +
    '// TmplCodeGen TestConf2', TEncoding.UTF8);
    
  var EscapedTestDir: String := FTestDir.Replace('\', '\\', [rfReplaceAll]);
  TFile.WriteAllText(FConfFile, Format('{"Template": "TestConf.TMPL.dproj", "TemplatesPath": "%s"}', [EscapedTestDir]), TEncoding.UTF8);
  TFile.WriteAllText(FTmplFile, '<Project>1</Project>', TEncoding.UTF8);
  
  TFile.WriteAllText(Conf2File, Format('{"Template": "TestConf2.TMPL.dproj", "TemplatesPath": "%s"}', [EscapedTestDir]), TEncoding.UTF8);
  TFile.WriteAllText(Tmpl2File, '<Project>2</Project>', TEncoding.UTF8);

  PreProcessor := TDptPreprocessor.Create(TSilentLogger.Create);
  try
    ResultFile := PreProcessor.Execute(FInputFile);
    
    // Should return the last one
    Assert.AreEqual(TPath.Combine(FTestDir, 'TestConf2.dproj'), ResultFile);
    Assert.IsTrue(FileExists(TPath.Combine(FTestDir, 'TestConf.dproj')), 'First config should also be generated');
  finally
    PreProcessor.Free;
  end;
end;

procedure TTestDptPreprocessor.TestIncludePartials;
var
  PartialFile : String;
  PreProcessor: TDptPreprocessor;
  ResultFile  : String;
  TargetFile  : String;
begin
  TargetFile := TPath.Combine(FTestDir, 'Target.pas');
  PartialFile := TPath.Combine(FTestDir, 'MyPart.part.pas');
  
  TFile.WriteAllText(TargetFile, 
    'Before' + sLineBreak +
    '{$REGION ''INCLUDE-PARTIAL / ' + PartialFile + '''}' + sLineBreak +
    '{$ENDREGION ''INCLUDE-PARTIAL / ' + PartialFile + '''}' + sLineBreak +
    'After', TEncoding.UTF8);

  TFile.WriteAllText(PartialFile, 'Inserted Content', TEncoding.UTF8);

  TFile.WriteAllText(FInputFile, 
    Format('// TmplCodeGen include_partials %s', [TargetFile]), TEncoding.UTF8);

  PreProcessor := TDptPreprocessor.Create(TSilentLogger.Create);
  try
    ResultFile := PreProcessor.Execute(FInputFile);
    
    Assert.AreEqual(FInputFile, ResultFile, 'Should return input file as no dproj generated');
    
    var Content: String := TFile.ReadAllText(TargetFile);
    Assert.IsTrue(Content.Contains('Inserted Content'), 'Partial should be included');
  finally
    PreProcessor.Free;
  end;
end;

procedure TTestDptPreprocessor.TestEmbeddedConfig;
var
  PreProcessor: TDptPreprocessor;
  ResultFile  : String;
begin
  var EscapedTestDir: String := FTestDir.Replace('\', '\\', [rfReplaceAll]);
  var ConfContent: String := Format('{"Template": "TestConf.TMPL.dproj", "TemplatesPath": "%s"}', [EscapedTestDir]);

  TFile.WriteAllText(FInputFile,  
    Format('(* MyConfig-conf.json %s *)', [ConfContent]) + sLineBreak +
    '// TmplCodeGen MyConfig' + sLineBreak +
    'program Input;', TEncoding.UTF8);

  TFile.WriteAllText(FTmplFile, '<Project>Embedded</Project>', TEncoding.UTF8);

  PreProcessor := TDptPreprocessor.Create(TSilentLogger.Create);
  try
    ResultFile := PreProcessor.Execute(FInputFile);
    
    Assert.AreEqual(TPath.Combine(FTestDir, 'MyConfig.dproj'), ResultFile, 'Should return generated dproj from embedded config');
    Assert.IsTrue(FileExists(ResultFile), 'Generated file should exist');
    Assert.AreEqual('<Project>Embedded</Project>', TFile.ReadAllText(ResultFile), 'Content mismatch');
  finally
    PreProcessor.Free;
  end;
end;

end.
