// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Lint.Setup.Task;

interface

uses

  System.Classes,
  System.SysUtils,
  System.IOUtils,

  DPT.Types;

type

  TDptLintSetupTask = class(TDptTaskBase)
  private
    FStyleFile: String;
    FSubAction: String;
    procedure Join;
    procedure Split;
  public
    procedure Execute; override;
    property SubAction: string read FSubAction write FSubAction;
    property StyleFile: string read FStyleFile write FStyleFile;
  end;

implementation

{ TDptLintSetupTask }

procedure TDptLintSetupTask.Execute;
begin
  if SameText(FSubAction, 'Split') then
    Split
  else if SameText(FSubAction, 'Join') then
    Join
  else
    raise Exception.Create('Unknown sub-action: ' + FSubAction + '. Use Split or Join.');
end;

procedure TDptLintSetupTask.Split;
var
  I           : Integer;
  LAnchorFound: Boolean;
  LBaseName   : String;
  LCol2Idx    : Integer;
  LCol3Idx    : Integer;
  LDescs      : TStringList;
  LLine       : String;
  LLines      : TArray<String>;
  LTemplate   : TStringList;
  LTests      : TStringList;
begin
  if not TFile.Exists(FStyleFile) then
    raise Exception.Create('Style file not found: ' + FStyleFile);

  LLines := TFile.ReadAllLines(FStyleFile, TEncoding.UTF8);
  if Length(LLines) = 0 then
    Exit;

  LCol2Idx := -1;
  LCol3Idx := -1;
  LAnchorFound := False;

  // 1. Find Anchor Line
  for I := 0 to High(LLines) do
  begin
    var LLineUpper := LLines[I].ToUpper;
    if LLineUpper.Contains('// START: STYLE-TEMPLATE') and
       LLineUpper.Contains('// START: AI-DESCRIPTIONS') and
       LLineUpper.Contains('// START: AI-GENERATED FITNESSE-TEST') then
    begin
      LCol2Idx := LLineUpper.IndexOf('// START: AI-DESCRIPTIONS');
      LCol3Idx := LLineUpper.IndexOf('// START: AI-GENERATED FITNESSE-TEST');
      LAnchorFound := True;
      Break;
    end;
  end;

  if not LAnchorFound then
    raise Exception.Create('Anchor line not found in style file. Cannot determine column positions.');

  LBaseName := TPath.Combine(ExtractFilePath(FStyleFile), TPath.GetFileNameWithoutExtension(FStyleFile));
  LTemplate := TStringList.Create;
  LDescs := TStringList.Create;
  LTests := TStringList.Create;
  try
    for I := 0 to High(LLines) do
    begin
      LLine := LLines[I];

      // Part 1: Template
      if LLine.Length >= LCol2Idx then
        LTemplate.Add(LLine.Substring(0, LCol2Idx))
      else
        LTemplate.Add(LLine);

      // Part 2: Descriptions
      if LLine.Length >= LCol2Idx then
      begin
        var LPart2: string;
        if LLine.Length >= LCol3Idx then
          LPart2 := LLine.Substring(LCol2Idx, LCol3Idx - LCol2Idx)
        else
          LPart2 := LLine.Substring(LCol2Idx);

        // We keep everything, including the "// " prefix for 1:1 roundtrip
        // but we want convenience. Let's store a special marker if it was "// "
        LDescs.Add(LPart2);
      end
      else
        LDescs.Add('');

      // Part 3: Tests
      if LLine.Length >= LCol3Idx then
      begin
        var LPart3 := LLine.Substring(LCol3Idx);
        LTests.Add(LPart3);
      end
      else
        LTests.Add('');
    end;

    LTemplate.SaveToFile(LBaseName + '.Template.pas', TEncoding.UTF8);
    LDescs.SaveToFile(LBaseName + '.Descriptions.txt', TEncoding.UTF8);
    LTests.SaveToFile(LBaseName + '.Tests.txt', TEncoding.UTF8);

    Writeln('Style file split into:');
    Writeln('  ' + ExtractFileName(LBaseName) + '.Template.pas');
    Writeln('  ' + ExtractFileName(LBaseName) + '.Descriptions.txt');
    Writeln('  ' + ExtractFileName(LBaseName) + '.Tests.txt');
  finally
    LTemplate.Free;
    LDescs.Free;
    LTests.Free;
  end;
end;

procedure TDptLintSetupTask.Join;
var
  I            : Integer;
  LBaseName    : String;
  LDescLine    : String;
  LDescs       : TStringList;
  LMax         : Integer;
  LOutput      : TStringList;
  LTemplate    : TStringList;
  LTemplateLine: String;
  LTestLine    : String;
  LTests       : TStringList;
begin
  LBaseName := TPath.Combine(ExtractFilePath(FStyleFile), TPath.GetFileNameWithoutExtension(FStyleFile));
  if not TFile.Exists(LBaseName + '.Template.pas') then
    raise Exception.Create('Template file missing: ' + LBaseName + '.Template.pas');
  if not TFile.Exists(LBaseName + '.Descriptions.txt') then
    raise Exception.Create('Descriptions file missing: ' + LBaseName + '.Descriptions.txt');
  if not TFile.Exists(LBaseName + '.Tests.txt') then
    raise Exception.Create('Tests file missing: ' + LBaseName + '.Tests.txt');

  LTemplate := TStringList.Create;
  LDescs := TStringList.Create;
  LTests := TStringList.Create;
  LOutput := TStringList.Create;
  try
    LTemplate.LoadFromFile(LBaseName + '.Template.pas', TEncoding.UTF8);
    LDescs.LoadFromFile(LBaseName + '.Descriptions.txt', TEncoding.UTF8);
    LTests.LoadFromFile(LBaseName + '.Tests.txt', TEncoding.UTF8);

    LMax := LTemplate.Count;
    if LDescs.Count > LMax then LMax := LDescs.Count;
    if LTests.Count > LMax then LMax := LTests.Count;

    for I := 0 to LMax - 1 do
    begin
      LTemplateLine := ''; if I < LTemplate.Count then LTemplateLine := LTemplate[I];
      LDescLine := ''; if I < LDescs.Count then LDescLine := LDescs[I];
      LTestLine := ''; if I < LTests.Count then LTestLine := LTests[I];

      // Since we kept the prefixes during split, join is now a simple concatenation
      // which guarantees 1:1 roundtrip.
      var NewLine := LTemplateLine + LDescLine + LTestLine;
      LOutput.Add(NewLine.TrimRight);
    end;

    LOutput.SaveToFile(FStyleFile, TEncoding.UTF8);
    Writeln('Successfully joined files into ' + FStyleFile);
  finally
    LTemplate.Free;
    LDescs.Free;
    LTests.Free;
    LOutput.Free;
  end;
end;

end.
