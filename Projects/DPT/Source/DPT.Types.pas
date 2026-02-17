// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Types;

interface

uses

  System.Classes,
  System.SysUtils,

  JclIDEUtils,

  DPT.Logger;

type

  EInvalidParameter = class(Exception);

  TDelphiVersion = (dvUnknown, dvD2007, dvD10_1, dvD10_3, dvD11, dvD12);

  TAIMode = (amNone, amCursor, amGemini);

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

end.
