// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Workflow.Session;

interface

uses

  Winapi.Windows,

  System.Classes,
  System.DateUtils,
  System.Generics.Collections,
  System.IOUtils,
  System.JSON,
  System.SysUtils,
  System.Variants,

  mormot.core.collections;

type

  TDptSessionFileEntry = record
    Path: string;
    Hash: string;
    LastLintTime: TDateTime;
    LintSuccess: Boolean;
  end;

  TDptSessionRunResult = record
    Target: string;
    ExitCode: Integer;
    RunTime: TDateTime;
  end;

  TDptSessionData = class
  public
    HostPID: DWORD;
    StartTime: TDateTime;
    Files: IList<TDptSessionFileEntry>;
    RunResults: IList<TDptSessionRunResult>;
    constructor Create;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
  end;

implementation

{ TDptSessionData }

constructor TDptSessionData.Create;
begin
  Files := Collections.NewList<TDptSessionFileEntry>;
  RunResults := Collections.NewList<TDptSessionRunResult>;
end;

procedure TDptSessionData.LoadFromFile(const AFileName: String);
var
  Content       : String;
  Entry         : TDptSessionFileEntry;
  I             : Integer;
  Item          : TJSONObject;
  JSONFiles     : TJSONArray;
  JSONObj       : TJSONObject;
  JSONRunResults: TJSONArray;
  RunRes        : TDptSessionRunResult;
begin
  Files.Clear;
  RunResults.Clear;
  if not TFile.Exists(AFileName) then Exit;
  
  Content := TFile.ReadAllText(AFileName);
  JSONObj := TJSONObject.ParseJSONValue(Content) as TJSONObject;
  if not Assigned(JSONObj) then Exit;
  
  try
    HostPID := JSONObj.GetValue<Cardinal>('HostPID', 0);
    StartTime := ISO8601ToDate(JSONObj.GetValue<string>('StartTime', ''));
    
    JSONFiles := JSONObj.GetValue('Files') as TJSONArray;
    if Assigned(JSONFiles) then
    begin
      for I := 0 to JSONFiles.Count - 1 do
      begin
        Item := JSONFiles.Items[I] as TJSONObject;
        Entry.Path := Item.GetValue<string>('Path', '');
        Entry.Hash := Item.GetValue<string>('Hash', '');
        Entry.LastLintTime := ISO8601ToDate(Item.GetValue<string>('LastLintTime', ''));
        Entry.LintSuccess := Item.GetValue<Boolean>('LintSuccess', False);
        Files.Add(Entry);
      end;
    end;

    JSONRunResults := JSONObj.GetValue('RunResults') as TJSONArray;
    if Assigned(JSONRunResults) then
    begin
      for I := 0 to JSONRunResults.Count - 1 do
      begin
        Item := JSONRunResults.Items[I] as TJSONObject;
        RunRes.Target := Item.GetValue<string>('Target', '');
        RunRes.ExitCode := Item.GetValue<Integer>('ExitCode', 0);
        RunRes.RunTime := ISO8601ToDate(Item.GetValue<string>('RunTime', ''));
        RunResults.Add(RunRes);
      end;
    end;
  finally
    JSONObj.Free;
  end;
end;

procedure TDptSessionData.SaveToFile(const AFileName: string);
var
  I             : Integer;
  Item          : TJSONObject;
  JSONFiles     : TJSONArray;
  JSONObj       : TJSONObject;
  JSONRunResults: TJSONArray;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('HostPID', TJSONNumber.Create(HostPID));
    JSONObj.AddPair('StartTime', DateToISO8601(StartTime));
    
    JSONFiles := TJSONArray.Create;
    for I := 0 to Files.Count - 1 do
    begin
      Item := TJSONObject.Create;
      Item.AddPair('Path', Files[I].Path);
      Item.AddPair('Hash', Files[I].Hash);
      Item.AddPair('LastLintTime', DateToISO8601(Files[I].LastLintTime));
      Item.AddPair('LintSuccess', TJSONBool.Create(Files[I].LintSuccess));
      JSONFiles.AddElement(Item);
    end;
    JSONObj.AddPair('Files', JSONFiles);

    JSONRunResults := TJSONArray.Create;
    for I := 0 to RunResults.Count - 1 do
    begin
      Item := TJSONObject.Create;
      Item.AddPair('Target', RunResults[I].Target);
      Item.AddPair('ExitCode', TJSONNumber.Create(RunResults[I].ExitCode));
      Item.AddPair('RunTime', DateToISO8601(RunResults[I].RunTime));
      JSONRunResults.AddElement(Item);
    end;
    JSONObj.AddPair('RunResults', JSONRunResults);
    
    TFile.WriteAllText(AFileName, JSONObj.ToJSON);
  finally
    JSONObj.Free;
  end;
end;

end.
