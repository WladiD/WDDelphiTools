unit DebugTarget.IfaceProbe;

// §6.36 fixture: an interface reference passed to a helper, declared in a
// SEPARATE unit on purpose. Pins the consumer-side live recovery
// (DPT.Debugger TryRecoverReferenceType / StrictObjectClassName):
// `evaluate AIface type=object` must walk the interface reference back to the
// implementing object's VMT and NAME the class (TDbgRecoverableImpl) instead
// of a bare "Object @ <addr>".
//
// Why a HELPER with the interface as a PARAMETER (not a plain local): a
// const-interface parameter is live for the whole helper body, so the
// breakpoint can't land before the value is assigned (a plain local's
// `Iface := TImpl.Create(...)` store is attributed to a fuzzy per-statement
// line, so a BP on the next line can resolve onto the Create call itself,
// before the store -- the value reads nil).
//
// Why a SEPARATE unit: the §6.37 getter-name resolution
// (TestPropertyGetterNameRecovered32) matches a property's 2-byte TargetId to
// a method record by file-offset PROXIMITY. Declaring this interface + impl
// INLINE in DebugTarget.dpr (next to TPropHost) tips CalcProp's getter window
// onto a colliding method record. Keeping it in its own compiland preserves
// that resolution. (The fixture tests that count modules / pin RVAs / pin
// segment VAs were refactored to derive from the .map so this extra unit
// doesn't break them.)

{$O-}            // keep params/locals addressable (spill home, no reg-only elision)
{$D+}
{$STACKFRAMES ON}

interface

procedure InterfaceLocalProbe;

implementation

uses
  System.SysUtils;

type
  IDbgRecoverable = interface
    function Marker: Integer;
  end;

  TDbgRecoverableImpl = class(TInterfacedObject, IDbgRecoverable)
  private
    FMarker: Integer;
  public
    constructor Create(AMarker: Integer);
    function Marker: Integer;
  end;

constructor TDbgRecoverableImpl.Create(AMarker: Integer);
begin
  inherited Create;
  FMarker := AMarker;
end;

function TDbgRecoverableImpl.Marker: Integer;
begin
  Result := FMarker;
end;

procedure ShowRecoverable(const AIface: IDbgRecoverable);
begin
  Writeln('IfaceLocal ', AIface.Marker); Flush(Output);   // iface-recovery bp here
end;

procedure InterfaceLocalProbe;
var
  Iface: IDbgRecoverable;
begin
  Iface := TDbgRecoverableImpl.Create(Integer($1FACE001));
  ShowRecoverable(Iface);
end;

end.
