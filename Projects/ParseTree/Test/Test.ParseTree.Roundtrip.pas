unit Test.ParseTree.Roundtrip;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.Threading,
  System.Types,
  DUnitX.TestFramework,
  ParseTree.Core,
  ParseTree.Tokens,
  ParseTree.Nodes,
  ParseTree.Parser,
  ParseTree.Writer;

type
  [TestFixture]
  TParseTreeRoundtripTest = class
  private
    procedure DoRoundtripTest(const AFilePath: string);
  public
    [Test]
    procedure TestAllDPTFilesParallel;
  end;

implementation

{ TParseTreeRoundtripTest }

procedure TParseTreeRoundtripTest.DoRoundtripTest(const AFilePath: string);
var
  LParser: TParseTreeParser;
  LOriContent: string;
  LTree: TCompilationUnitSyntax;
  LTreeWriter: TSyntaxTreeWriter;
  LNewContent: string;
  LBaseDir: string;
  LOutputFolder: string;
  LOutputFile: string;
  LOriLines: TArray<string>;
  LNewLines: TArray<string>;
  I: Integer;
  LMsg: string;
begin
  Assert.IsTrue(TFile.Exists(AFilePath), 'Source file does not exist: ' + AFilePath);
  
  LOriContent := TFile.ReadAllText(AFilePath, TEncoding.UTF8);
  LParser := TParseTreeParser.Create;
  try
    LTree := LParser.Parse(LOriContent);
    try
      LTreeWriter := TSyntaxTreeWriter.Create;
      try
        LNewContent := LTreeWriter.GenerateSource(LTree);
      finally
        LTreeWriter.Free;
      end;
    finally
      LTree.Free;
    end;
  finally
    LParser.Free;
  end;

  // Save to RoundTripTest output folder
  LBaseDir := ExtractFilePath(ParamStr(0));
  LOutputFolder := TPath.Combine(LBaseDir, 'RoundTripTest');
  if not TDirectory.Exists(LOutputFolder) then
    TDirectory.CreateDirectory(LOutputFolder);
    
  LOutputFile := TPath.Combine(LOutputFolder, ExtractFileName(AFilePath));
  TFile.WriteAllText(LOutputFile, LNewContent, TEncoding.UTF8);

  // Find and report first difference
  if LOriContent <> LNewContent then
  begin
    LOriLines := LOriContent.Split([#13#10]);
    LNewLines := LNewContent.Split([#13#10]);
    LMsg := 'Roundtrip failed for ' + ExtractFileName(AFilePath) + '. Output: ' + LOutputFile;
    for I := 0 to Length(LOriLines) - 1 do
    begin
      if (I >= Length(LNewLines)) then
      begin
        LMsg := LMsg + sLineBreak + Format('Line %d missing in roundtrip (original has %d lines, roundtrip has %d)',
          [I + 1, Length(LOriLines), Length(LNewLines)]);
        Break;
      end;
      if LOriLines[I] <> LNewLines[I] then
      begin
        LMsg := LMsg + sLineBreak + Format('First diff at line %d:', [I + 1])
          + sLineBreak + '  ORI: [' + LOriLines[I] + ']'
          + sLineBreak + '  RT:  [' + LNewLines[I] + ']';
        Break;
      end;
    end;
    if (Length(LNewLines) > Length(LOriLines)) then
      LMsg := LMsg + sLineBreak + Format('Roundtrip has %d extra lines', [Length(LNewLines) - Length(LOriLines)]);
    Assert.Fail(LMsg);
  end;
end;

procedure TParseTreeRoundtripTest.TestAllDPTFilesParallel;
var
  LProjectsDir: string;
  LSourceDir: string;
  LFiles: TStringDynArray;
  I: Integer;
begin
  LProjectsDir := TPath.GetFullPath(TPath.Combine(ExtractFilePath(ParamStr(0)), '..\..\..\..\'));
  LSourceDir := TPath.Combine(LProjectsDir, 'DPT\Source');
  
  if not TDirectory.Exists(LSourceDir) then
    Assert.Fail('Source directory does not exist: ' + LSourceDir);

  LFiles := TDirectory.GetFiles(LSourceDir, '*.pas', TSearchOption.soTopDirectoryOnly);
  
  for I := 0 to Length(LFiles) - 1 do
  begin
    DoRoundtripTest(LFiles[I]);
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TParseTreeRoundtripTest);

end.
