// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Utils;

interface

procedure CheckAndExecutePreProcessor(var AProjectFile: String);

/// <summary>
///   Display name of the current Windows user (GetUserNameEx NameDisplay,
///   e.g. "Jane Doe"); falls back to the login name when Windows reports none,
///   for example on a machine without domain membership.
/// </summary>
function GetUserDisplayName: String;

implementation

uses

  Winapi.Windows,
  System.SysUtils,

  DPT.Preprocessor;

function GetUserNameExW(NameFormat: DWORD; lpNameBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;
  external 'secur32.dll';

function GetUserDisplayName: String;
const
  NameDisplay = 3;
var
  Buffer: array[0..1023] of Char;
  Len: DWORD;
begin
  Result := '';
  Len := Length(Buffer);
  if GetUserNameExW(NameDisplay, Buffer, Len) then
    SetString(Result, Buffer, Len);
  if Trim(Result) = '' then
    Result := GetEnvironmentVariable('USERNAME');
end;

procedure CheckAndExecutePreProcessor(var AProjectFile: String);
var
  PreProcessor: TDptPreprocessor;
begin
  if not SameText(ExtractFileExt(AProjectFile), '.dproj') then
  begin
    PreProcessor := TDptPreprocessor.Create;
    try
      AProjectFile := PreProcessor.Execute(AProjectFile);
    finally
      PreProcessor.Free;
    end;
  end;
end;

end.
