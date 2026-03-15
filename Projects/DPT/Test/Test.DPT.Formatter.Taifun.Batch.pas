unit Test.DPT.Formatter.Taifun.Batch;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Math,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer,
  DPT.Formatter.DWS;

type
  [TestFixture]
  TTestTaifunFormatterBatch = class
  private
    FParser: TParseTreeParser;
    FWriter: TSyntaxTreeWriter;
    FFormatter: TDptDwsFormatter;
    FScriptPath: string;
    FInputDir: string;
    FOutputDir: string;
    FIdempotentDir: string;
    FGoodDir: string;
    procedure ClearDirectory(const ADir: string);
    procedure CompareFilesLineByLine(const AFile1, AFile2: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestIdempotence;
    [Test]
    procedure TestGoodFormat;
  end;

implementation

{ TTestTaifunFormatterBatch }

procedure TTestTaifunFormatterBatch.Setup;
var
  LTestDir: string;
begin
  FParser := TParseTreeParser.Create;
  FWriter := TSyntaxTreeWriter.Create;
  FFormatter := TDptDwsFormatter.Create;

  FScriptPath := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\Format\TaifunFormat.pas'));
  LTestDir := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\'));
  
  FInputDir := TPath.Combine(LTestDir, 'FormatTaifunData\A-Input');
  FOutputDir := TPath.Combine(LTestDir, 'FormatTaifunData\B-Output');
  FIdempotentDir := TPath.Combine(LTestDir, 'FormatTaifunData\C-Idempotent');
  FGoodDir := TPath.Combine(LTestDir, 'FormatTaifunData\Good');
end;

procedure TTestTaifunFormatterBatch.TearDown;
begin
  FFormatter.Free;
  FWriter.Free;
  FParser.Free;
end;

procedure TTestTaifunFormatterBatch.ClearDirectory(const ADir: string);
var
  LFile: string;
begin
  if TDirectory.Exists(ADir) then
  begin
    for LFile in TDirectory.GetFiles(ADir) do
      TFile.Delete(LFile);
  end
  else
    TDirectory.CreateDirectory(ADir);
end;

procedure TTestTaifunFormatterBatch.CompareFilesLineByLine(const AFile1, AFile2: string);
var
  Lines1, Lines2: TStringList;
  I: Integer;
begin
  Lines1 := TStringList.Create;
  Lines2 := TStringList.Create;
  try
    Lines1.LoadFromFile(AFile1, TEncoding.UTF8);
    Lines2.LoadFromFile(AFile2, TEncoding.UTF8);
    
    for I := 0 to Min(Lines1.Count, Lines2.Count) - 1 do
    begin
      if Lines1[I] <> Lines2[I] then
        Assert.Fail(Format('Datei %s unterscheidet sich in Zeile %d:'#13#10'B-Output  : %s'#13#10'C-Idempotent: %s', 
          [ExtractFileName(AFile1), I + 1, Lines1[I], Lines2[I]]));
    end;
    
    if Lines1.Count <> Lines2.Count then
      Assert.Fail(Format('Dateien haben unterschiedliche Zeilenanzahl: %s (%d vs %d)', 
        [ExtractFileName(AFile1), Lines1.Count, Lines2.Count]));
  finally
    Lines1.Free;
    Lines2.Free;
  end;
end;

procedure TTestTaifunFormatterBatch.TestIdempotence;
var
  LFiles: TArray<string>;
  LInputFile, LFileName, LOutputFile, LIdempotentFile: string;
  LSource, LFormatted1, LFormatted2: string;
  LUnit1, LUnit2: TCompilationUnitSyntax;
begin
  if not TDirectory.Exists(FInputDir) then
  begin
    Assert.IsTrue(True, 'Eingabeordner existiert nicht.');
    Exit;
  end;

  LFiles := TDirectory.GetFiles(FInputDir, '*.pas');
  if Length(LFiles) = 0 then
  begin
    Assert.IsTrue(True, 'Keine Dateien in A-Input gefunden.');
    Exit;
  end;
    
  ClearDirectory(FOutputDir);
  ClearDirectory(FIdempotentDir);

  FFormatter.LoadScript(FScriptPath);

  for LInputFile in LFiles do
  begin
    LFileName := ExtractFileName(LInputFile);
    LOutputFile := TPath.Combine(FOutputDir, LFileName);
    LIdempotentFile := TPath.Combine(FIdempotentDir, LFileName);

    LSource := TFile.ReadAllText(LInputFile, TEncoding.UTF8);

    // Erster Durchlauf: A -> B
    LUnit1 := FParser.Parse(LSource);
    try
      FFormatter.FormatUnit(LUnit1);
      LFormatted1 := FWriter.GenerateSource(LUnit1);
      TFile.WriteAllText(LOutputFile, LFormatted1, TEncoding.UTF8);
    finally
      LUnit1.Free;
    end;

    // Zweiter Durchlauf: B -> C
    LUnit2 := FParser.Parse(LFormatted1);
    try
      FFormatter.FormatUnit(LUnit2);
      LFormatted2 := FWriter.GenerateSource(LUnit2);
      TFile.WriteAllText(LIdempotentFile, LFormatted2, TEncoding.UTF8);
    finally
      LUnit2.Free;
    end;

    // Vergleich
    CompareFilesLineByLine(LOutputFile, LIdempotentFile);
  end;

  // Cleanup bei Erfolg
  ClearDirectory(FIdempotentDir);
end;

procedure TTestTaifunFormatterBatch.TestGoodFormat;
var
  LFiles: TArray<string>;
  LGoodFile, LFileName, LInputFile, LOutputFile: string;
  LSource, LFormatted: string;
  LUnit: TCompilationUnitSyntax;
begin
  if not TDirectory.Exists(FGoodDir) then
  begin
    Assert.IsTrue(True, 'Good-Ordner existiert nicht.');
    Exit;
  end;

  LFiles := TDirectory.GetFiles(FGoodDir, '*.pas');
  if Length(LFiles) = 0 then
  begin
    Assert.IsTrue(True, 'Keine Dateien im Good-Ordner gefunden.');
    Exit;
  end;

  FFormatter.LoadScript(FScriptPath);

  for LGoodFile in LFiles do
  begin
    LFileName := ExtractFileName(LGoodFile);
    LInputFile := TPath.Combine(FInputDir, LFileName);
    LOutputFile := TPath.Combine(FOutputDir, LFileName);

    if not TFile.Exists(LInputFile) then
      Assert.Fail('Gegenstück in A-Input fehlt für: ' + LFileName);

    LSource := TFile.ReadAllText(LInputFile, TEncoding.UTF8);

    LUnit := FParser.Parse(LSource);
    try
      FFormatter.FormatUnit(LUnit);
      LFormatted := FWriter.GenerateSource(LUnit);
      TFile.WriteAllText(LOutputFile, LFormatted, TEncoding.UTF8);
    finally
      LUnit.Free;
    end;

    // Vergleich der formatierten Ausgabe mit der Good-Datei
    CompareFilesLineByLine(LOutputFile, LGoodFile);
  end;
end;

end.
