// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Types;

interface

uses

  System.SysUtils;

type

  EInvalidParameter = class(Exception);
  EDptRuntimeError = class(Exception);

  TDelphiVersion = (dvUnknown, dvD2007, dvD10_1, dvD10_3, dvD11, dvD12, dvD13);

  TAIMode = (amNone, amCursor, amGemini);

  TCmdLineConsumer = class
  private
    FCurrentMeaningParam: String;
    FCurrentParameter   : Integer;
    FParams             : TArray<String>;
  public
    constructor Create; overload;
    constructor Create(const AParams: TArray<String>); overload;
    function  CheckParameter(MeaningParam: String): String;
    procedure ConsumeParameter;
    function  HasParameter: Boolean;
    procedure InvalidParameter(ErrorMessage: String);
  end;

const

  DptConfigFileName = 'DptConfig.ini';

  DelphiVersionStringArray: Array [TDelphiVersion] of String = (
    { dvUnknown } '',
    { dvD2007   } 'D2007',
    { dvD10_1   } 'D10.1',
    { dvD10_3   } 'D10.3',
    { dvD11     } 'D11',
    { dvD12     } 'D12',
    { dvD13     } 'D13');

  DelphiVersionIntegerArray: Array [TDelphiVersion] of Integer = (
    { dvUnknown } 0,
    { dvD2007   } 11,
    { dvD10_1   } 24,
    { dvD10_3   } 26,
    { dvD11     } 28,
    { dvD12     } 29,
    { dvD13     } 37);

implementation

uses

  System.Classes,

  JclIDEUtils,

  DPT.Logger;

{ TCmdLineConsumer }

constructor TCmdLineConsumer.Create;
begin
  FCurrentParameter := 1;
end;

constructor TCmdLineConsumer.Create(const AParams: TArray<String>);
begin
  FParams := AParams;
  FCurrentParameter := 0; // 0-based for custom array
end;

function TCmdLineConsumer.HasParameter: Boolean;
begin
  if FParams <> nil then
    Result := FCurrentParameter < Length(FParams)
  else
    Result := FCurrentParameter <= ParamCount;
end;

function TCmdLineConsumer.CheckParameter(MeaningParam: String): String;
begin
  FCurrentMeaningParam := MeaningParam;
  if not HasParameter then
    InvalidParameter('Parameter not available');
  if FParams <> nil then
    Result := FParams[FCurrentParameter]
  else
    Result := ParamStr(FCurrentParameter);
end;

procedure TCmdLineConsumer.ConsumeParameter;
begin
  Inc(FCurrentParameter);
end;

procedure TCmdLineConsumer.InvalidParameter(ErrorMessage: String);
var
  LParam: String;
begin
  if HasParameter then
  begin
    if FParams <> nil then
      LParam := FParams[FCurrentParameter]
    else
      LParam := ParamStr(FCurrentParameter);
    raise EInvalidParameter.CreateFmt('%s = "%s": %s', [FCurrentMeaningParam,
      LParam, ErrorMessage])
  end
  else
    raise EInvalidParameter.CreateFmt('%s: %s', [FCurrentMeaningParam, ErrorMessage]);
end;

end.
