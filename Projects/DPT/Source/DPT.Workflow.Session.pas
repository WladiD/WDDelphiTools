// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Workflow.Session;

interface

uses
  Winapi.Windows,
  System.Variants,
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.DateUtils,
  System.Generics.Collections;

type
  TDptSessionFileEntry = record
    Path: string;
    Hash: string;
    LastLintTime: TDateTime;
    LintSuccess: Boolean;
  end;

  TDptSessionData = class
  public
    HostPID: DWORD;
    StartTime: TDateTime;
    Files: TList<TDptSessionFileEntry>;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFileName: string);
    procedure SaveToFile(const AFileName: string);
  end;

implementation

{ TDptSessionData }

constructor TDptSessionData.Create;
begin
  Files := TList<TDptSessionFileEntry>.Create;
end;

destructor TDptSessionData.Destroy;
begin
  Files.Free;
  inherited;
end;

procedure TDptSessionData.LoadFromFile(const AFileName: string);
var
  JSONObj: TJSONObject;
  JSONFiles: TJSONArray;
  I: Integer;
  Entry: TDptSessionFileEntry;
  Item: TJSONObject;
  Content: string;
begin
  Files.Clear;
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
  finally
    JSONObj.Free;
  end;
end;

procedure TDptSessionData.SaveToFile(const AFileName: string);
var
  JSONObj: TJSONObject;
  JSONFiles: TJSONArray;
  I: Integer;
  Item: TJSONObject;
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
    
    TFile.WriteAllText(AFileName, JSONObj.ToJSON);
  finally
    JSONObj.Free;
  end;
end;

end.
