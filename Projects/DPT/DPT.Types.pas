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
    FCurrentParameter: Integer;
    FCurrentMeaningParam: string;

  public
    constructor Create;

    function HasParameter: Boolean;
    function CheckParameter(MeaningParam: string): string;
    procedure ConsumeParameter;
    procedure InvalidParameter(ErrorMessage: string);
  end;

  TDelphiVersion = (dvUnknown, dvD2007, dvD10_1, dvD10_3, dvD11, dvD12);

  TDPTaskBase = class
  private
    FInstallation: TJclBorRADToolInstallation;
    FInstallations: TJclBorRADToolInstallations;

  protected
    procedure Output(const Text: string);
    function Installation: TJclBorRADToolInstallation;

  public
    DelphiVersion: TDelphiVersion;
    IsX64: Boolean;

    destructor Destroy; override;
    procedure Execute; virtual; abstract;
  end;
  
  TDPTaskClass = class of TDPTaskBase;

const
  DelphiVersionStringArray: array [TDelphiVersion] of string = ('', 'D2007', 'D10.1', 'D10.3', 'D11', 'D12');
  DelphiVersionIntegerArray: array [TDelphiVersion] of Integer = (0, 11, 24, 26, 28, 29);

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

function TCMDLineConsumer.CheckParameter(MeaningParam: string): string;
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

procedure TCMDLineConsumer.InvalidParameter(ErrorMessage: string);
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

procedure TDPTaskBase.Output(const Text: string);
begin
  Writeln(Text);
end;

end.
