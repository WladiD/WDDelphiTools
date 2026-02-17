// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Types;

interface

uses

  System.SysUtils,
  System.Classes,

  JclIDEUtils,

  DPT.Logger;

type

  EInvalidParameter = class(Exception);

  TCmdLineConsumer = class
  private
    FCurrentMeaningParam: String;
    FCurrentParameter   : Integer;
  public
    constructor Create;
    function  CheckParameter(MeaningParam: String): String;
    procedure ConsumeParameter;
    function  HasParameter: Boolean;
    procedure InvalidParameter(ErrorMessage: String);
  end;

  TDelphiVersion = (dvUnknown, dvD2007, dvD10_1, dvD10_3, dvD11, dvD12);

function FindMostRecentDelphiVersion: TDelphiVersion;
function IsValidDelphiVersion(VersionString: String; out DelphiVersion: TDelphiVersion): Boolean;
function IsLatestVersionAlias(const AValue: String): Boolean;

const

  DptConfigFileName = 'DptConfig.ini';

  DelphiVersionStringArray: Array [TDelphiVersion] of String = (
    { dvUnknown } '',
    { dvD2007   } 'D2007',
    { dvD10_1   } 'D10.1',
    { dvD10_3   } 'D10.3',
    { dvD11     } 'D11',
    { dvD12     } 'D12');

  DelphiVersionIntegerArray: Array [TDelphiVersion] of Integer = (
    { dvUnknown } 0,
    { dvD2007   } 11,
    { dvD10_1   } 24,
    { dvD10_3   } 26,
    { dvD11     } 28,
    { dvD12     } 29);

implementation

{ TCmdLineConsumer }

constructor TCmdLineConsumer.Create;
begin
  FCurrentParameter := 1;
end;

function TCmdLineConsumer.HasParameter: Boolean;
begin
  Result := FCurrentParameter <= ParamCount;
end;

function TCmdLineConsumer.CheckParameter(MeaningParam: String): String;
begin
  FCurrentMeaningParam := MeaningParam;
  if not HasParameter then
    InvalidParameter('Parameter not available');
  Result := ParamStr(FCurrentParameter);
end;

procedure TCmdLineConsumer.ConsumeParameter;
begin
  Inc(FCurrentParameter);
end;

procedure TCmdLineConsumer.InvalidParameter(ErrorMessage: String);
begin
  if HasParameter then
    raise EInvalidParameter.CreateFmt('%s = "%s": %s', [FCurrentMeaningParam,
      ParamStr(FCurrentParameter), ErrorMessage])
  else
    raise EInvalidParameter.CreateFmt('%s: %s', [FCurrentMeaningParam, ErrorMessage]);
end;

function IsValidDelphiVersion(VersionString: String; out DelphiVersion: TDelphiVersion): Boolean;
begin
  Result := True;
  for var Loop: Integer := 1 to Integer(High(TDelphiVersion)) do
  begin
    DelphiVersion := TDelphiVersion(Loop);
    if VersionString = DelphiVersionStringArray[DelphiVersion] then
      Exit;
  end;
  DelphiVersion := dvUnknown;
  Result := False;
end;

function FindMostRecentDelphiVersion: TDelphiVersion;
var
  Installations: TJclBorRADToolInstallations;
begin
  Result := dvUnknown;
  Installations := TJclBorRADToolInstallations.Create;
  try
    for var Loop: Integer := Integer(High(TDelphiVersion)) downto 1 do
    begin
      if Installations.DelphiVersionInstalled[DelphiVersionIntegerArray[TDelphiVersion(Loop)]] then
      begin
        Result := TDelphiVersion(Loop);
        Break;
      end;
    end;
  finally
    Installations.Free;
  end;
end;

function IsLatestVersionAlias(const AValue: String): Boolean;
begin
  Result := SameText(AValue, 'LATEST') or SameText(AValue, 'RECENT');
end;

end.
