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
  JclIDEUtils;

type

  EInvalidParameter = class(Exception);

  TCMDLineConsumer = class
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

  TDPTaskBase = class
  private
    FInstallation : TJclBorRADToolInstallation;
    FInstallations: TJclBorRADToolInstallations;
  protected
    function  Installation: TJclBorRADToolInstallation;
    procedure Output(const Text: String);
  public
    DelphiVersion: TDelphiVersion;
    IsX64: Boolean;
  public
    destructor Destroy; override;
    procedure Execute; virtual; abstract;
  end;

  TDPTaskClass = class of TDPTaskBase;

const

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

{ TCMDLineConsumer }

constructor TCMDLineConsumer.Create;
begin
  FCurrentParameter := 1;
end;

function TCMDLineConsumer.HasParameter: Boolean;
begin
  Result := FCurrentParameter <= ParamCount;
end;

function TCMDLineConsumer.CheckParameter(MeaningParam: String): String;
begin
  FCurrentMeaningParam := MeaningParam;
  if not HasParameter then
    InvalidParameter('Parameter not available');
  Result := ParamStr(FCurrentParameter);
end;

procedure TCMDLineConsumer.ConsumeParameter;
begin
  Inc(FCurrentParameter);
end;

procedure TCMDLineConsumer.InvalidParameter(ErrorMessage: String);
begin
  if HasParameter then
    raise EInvalidParameter.CreateFmt('%s = "%s": %s', [FCurrentMeaningParam,
      ParamStr(FCurrentParameter), ErrorMessage])
  else
    raise EInvalidParameter.CreateFmt('%s: %s', [FCurrentMeaningParam, ErrorMessage]);
end;

{ TDPTaskBase }

destructor TDPTaskBase.Destroy;
begin
  FInstallations.Free;
  inherited Destroy;
end;

function TDPTaskBase.Installation: TJclBorRADToolInstallation;
var
  DelphiVersionAsInt: Integer;
begin
  if Assigned(FInstallation) then
  begin
    Result := FInstallation;
    Exit;
  end;

  if not Assigned(FInstallations) then
    FInstallations := TJclBorRADToolInstallations.Create;

  DelphiVersionAsInt := DelphiVersionIntegerArray[DelphiVersion];

  if not FInstallations.DelphiVersionInstalled[DelphiVersionAsInt] then
    raise Exception.CreateFmt('Delphi Version %s (%d) not installed',
      [DelphiVersionStringArray[DelphiVersion], DelphiVersionAsInt]);

  FInstallation := FInstallations.DelphiInstallationFromVersion[DelphiVersionAsInt];
  FInstallation.OutputCallback := Output;
  Result := FInstallation;
end;

procedure TDPTaskBase.Output(const Text: String);
begin
  Writeln(Text);
end;

end.
