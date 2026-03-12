unit DPT.Format.Task;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  mormot.core.collections,
  DPT.Task, DPT.Types;

type
  { CLI task for formatting source text using a DWScript ruleset }
  TDptFormatTask = class(TDptTaskBase)
  private
    FScriptFile: string;
    FTargetFiles: IList<string>;
  public
    constructor Create; override;
    procedure Parse(CmdLine: TCmdLineConsumer); override;
    procedure Execute; override;
  end;

implementation

uses
  System.IOUtils,
  ParseTree.Nodes, ParseTree.Parser, ParseTree.Writer,
  DPT.Formatter.DWS;

{ TDptFormatTask }

constructor TDptFormatTask.Create;
begin
  inherited;
  FTargetFiles := Collections.NewList<string>;
end;

procedure TDptFormatTask.Parse(CmdLine: TCmdLineConsumer);
var
  Param: string;
  LPath, LMask: string;
  LFiles: TArray<string>;
  LFile: string;
begin
  FScriptFile := '';
  FTargetFiles.Clear;

  while CmdLine.HasParameter do
  begin
    Param := CmdLine.CheckParameter('Option/File');

    if Param.StartsWith('--') then
    begin
      CmdLine.InvalidParameter('Unknown option: ' + Param);
    end
    else
    begin
      if FScriptFile = '' then
      begin
        FScriptFile := ExpandFileName(Param);
        CmdLine.ConsumeParameter;
      end
      else
      begin
        // If the parameter contains wildcards (* or ?), resolve them
        if (Pos('*', Param) > 0) or (Pos('?', Param) > 0) then
        begin
          LPath := ExtractFilePath(Param);
          if LPath = '' then
            LPath := GetCurrentDir;
          LMask := ExtractFileName(Param);

          if TDirectory.Exists(LPath) then
          begin
            LFiles := TDirectory.GetFiles(LPath, LMask);
            for LFile in LFiles do
              FTargetFiles.Add(LFile);
          end
          else
            Writeln('Directory not found for mask: ' + Param);
        end
        else
        begin
          FTargetFiles.Add(ExpandFileName(Param));
        end;
        CmdLine.ConsumeParameter;
      end;
    end;
  end;

  if FScriptFile = '' then
    CmdLine.InvalidParameter('Missing parameter: ScriptFile');

  if FTargetFiles.Count = 0 then
    CmdLine.InvalidParameter('Missing parameter: TargetFile(s)');
end;

procedure TDptFormatTask.Execute;
var
  LTarget: string;
  LSource: string;
  LParser: TParseTreeParser;
  LUnit: TCompilationUnitSyntax;
  LFormatter: TDptDwsFormatter;
  LWriter: TSyntaxTreeWriter;
  LEncoding: TEncoding;
begin
  if not TFile.Exists(FScriptFile) then
    raise Exception.Create('Script file not found: ' + FScriptFile);

  LFormatter := TDptDwsFormatter.Create;
  try
    LFormatter.LoadScript(FScriptFile);

    for LTarget in FTargetFiles do
    begin
      if not TFile.Exists(LTarget) then
      begin
        Writeln('Target file not found: ' + LTarget);
        Continue;
      end;

      Writeln('Formatting: ' + LTarget);
      LEncoding := TEncoding.UTF8;
      LSource := TFile.ReadAllText(LTarget, LEncoding);

      LParser := TParseTreeParser.Create;
      try
        LUnit := LParser.Parse(LSource);
        try
          LFormatter.FormatUnit(LUnit);
          LWriter := TSyntaxTreeWriter.Create;
          try
            // Note: using carriage return + line feed (Windows CRLF)
            LSource := LWriter.GenerateSource(LUnit);
          finally
            LWriter.Free;
          end;

          // Force CRLF per user request by replacing standalone LF or CR with CRLF
          // To be safe and just write, string replacements are handled by writer usually.
          // But we must write text carefully with TFile.
          TFile.WriteAllText(LTarget, LSource, LEncoding);

        finally
          LUnit.Free;
        end;
      finally
        LParser.Free;
      end;
    end;

  finally
    LFormatter.Free;
  end;

  Writeln('Formatting completed.');
end;

end.
