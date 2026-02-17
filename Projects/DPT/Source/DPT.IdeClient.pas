// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.IdeClient;

interface

uses

  System.SysUtils,
  System.Classes,

  IdTCPClient,

  Slim.List,

  DPT.Detection,
  DPT.Types;

type

  TDptIdeClient = class
  private
    function GetPortForVersion(AVersion: TDelphiVersion): Integer;
    function BuildSlimMessage(const AInstructions: String): String;
    function SendInstructions(APort: Integer; const AInstructions: String): Boolean;
  public
    function TryOpenUnit(ADelphiVersion: TDelphiVersion; const AUnitPath: String; ALine: Integer): Boolean;
    function TryOpenUnitOnPort(APort: Integer; const AUnitPath: String; ALine: Integer): Boolean;
  end;

implementation

{ TDptIdeClient }

function TDptIdeClient.GetPortForVersion(AVersion: TDelphiVersion): Integer;
begin
  // D12 (29) -> 9012.  (29 - 17 = 12?) No.
  // In Wizard: Round(CompilerVersion - 24).
  // D12 CompilerVersion 36.0 -> 12.
  // D11 CompilerVersion 35.0 -> 11.

  // DPT.Types defines TDelphiVersion enum and integer array (Mapping to BDS Version?).
  // D12 is 29 (BDS 23.0).
  // D11 is 28 (BDS 22.0).

  // Wait, my Wizard calculation was: 9000 + (CompilerVersion - 24).
  // D12 = 36.0 -> 12.
  // D11 = 35.0 -> 11.

  // I need to map TDelphiVersion to this suffix.
  case AVersion of
    dvD11: Result := 9011;
    dvD12: Result := 9012;
    // Older versions?
    dvD10_3: Result := 9000 + (33 - 24); // 9009?
    dvD10_1: Result := 9000 + (31 - 24); // 9007?
  else
    Result := 0;
  end;
end;

function TDptIdeClient.BuildSlimMessage(const AInstructions: String): String;
var
  LenStr: String;
begin
  LenStr := Format('%.6d', [Length(AInstructions)]);
  Result := LenStr + ':' + AInstructions;
end;

function TDptIdeClient.SendInstructions(APort: Integer; const AInstructions: String): Boolean;
var
  Client   : TIdTCPClient;
  Len      : Integer;
  LenHeader: String;
  Response : String;
begin
  Result := False;
  Client := TIdTCPClient.Create(nil);
  try
    Client.Host := '127.0.0.1';
    Client.Port := APort;
    Client.ConnectTimeout := 1000; // Increased timeout
    Client.ReadTimeout := 5000;
    Client.Connect;
    // Read version
    Response := Client.IOHandler.ReadLn;
    if Pos('Slim --', Response) <> 1 then
      Exit;

    // Send Instructions
    Client.IOHandler.Write(BuildSlimMessage(AInstructions));

    // Read Response Length
    LenHeader := Client.IOHandler.ReadString(6);
    Client.IOHandler.ReadChar; // Skip ':' separator

    Len := StrToIntDef(LenHeader, 0);
    if Len > 0 then
    begin
      Response := Client.IOHandler.ReadString(Len);
      if Pos('EXCEPTION', Response) = 0 then
        Result := True;
    end;

    Client.IOHandler.Write(BuildSlimMessage('bye'));
  finally
    Client.Free;
  end;
end;

function TDptIdeClient.TryOpenUnitOnPort(APort: Integer; const AUnitPath: String; ALine: Integer): Boolean;
var
  Instructions: String;
  MasterList  : TSlimList;
begin
  Result := False;
  if APort <= 0 then Exit;

  MasterList := SlimList([
    SlimList(['id1', 'make', 'inst_1', 'TDptIdeOpenUnitFixture']),
    SlimList(['id2', 'call', 'inst_1', 'setUnitPath', AUnitPath]),
    SlimList(['id3', 'call', 'inst_1', 'setLine', IntToStr(ALine)]),
    SlimList(['id4', 'call', 'inst_1', 'OpenUnit'])
  ]);
  
  try
    Instructions := SlimListSerialize(MasterList);
    Result := SendInstructions(APort, Instructions);
  finally
    MasterList.Free;
  end;
end;

function TDptIdeClient.TryOpenUnit(ADelphiVersion: TDelphiVersion; const AUnitPath: String; ALine: Integer): Boolean;
var
  Port: Integer;
begin
  Port := GetPortForVersion(ADelphiVersion);
  Result := TryOpenUnitOnPort(Port, AUnitPath, ALine);
end;

end.
