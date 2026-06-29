unit DebugTarget.SharedHdr;

// §6.44 fixture: a nested SHARED-header record reached by the T<member> name
// convention. Declared in its OWN compiland ON PURPOSE: adding it inline to
// DebugTarget.dpr shifts CalcProp's §6.37 getter-name resolution window
// (file-offset PROXIMITY) onto a colliding method record and breaks
// TestPropertyGetterNameRecovered32 -- the exact reason DebugTarget.IfaceProbe
// is a separate unit too.
//
// THdr's name == 'T' + the member name "Hdr" (the §6.19/§6.41 T<X> family,
// applied to a NESTED record member). The §6.43 T<outerStem><member> form
// (TXHdrOwnerHdr) does NOT exist -- mirroring the live TUser.RecHeader, whose
// type is the SHARED record TRecHeader (not TUserRecHeader). TXHdrDecoy has
// the SAME 8-byte extent AND also declares "Version", so the §6.36
// size+next-field bridge is ambiguous (returns -1) and only the T<member>
// convention can pick THdr.

{$O-}            // keep params/locals addressable
{$D+}
{$STACKFRAMES ON}

interface

type
  THdr = record
    Version : Integer;
    Tag     : Integer;
  end;
  TXHdrDecoy = record         // same extent (8) + same leaf "Version"
    Version : Integer;
    Spare   : Integer;
  end;
  TXHdrOwner = record
    Hdr     : THdr;
    Payload : Integer;
  end;
  PXHdrOwner = ^TXHdrOwner;

procedure SharedHeaderRecordProbe;

implementation

uses
  System.SysUtils;

procedure SharedHeaderRecordProbe;
var
  HdrLoc : TXHdrOwner;
  HdrPtr : PXHdrOwner;
  Decoy  : TXHdrDecoy;
begin
  HdrLoc := Default(TXHdrOwner);
  HdrLoc.Hdr.Version := Integer($5A5A1234);
  HdrLoc.Hdr.Tag     := Integer($00C0FFEE);
  HdrLoc.Payload     := Integer($DEADBEEF);
  HdrPtr := @HdrLoc;
  Decoy  := Default(TXHdrDecoy);
  Decoy.Version := Integer($11112222);   // force TXHdrDecoy to be discovered
  // Reference HdrLoc, HdrPtr and Decoy on the BP line so the debug build
  // keeps them live (HdrPtr must hold @HdrLoc, not a stale register).
  Writeln('SharedHdr ', HdrPtr^.Hdr.Version, ' ', HdrLoc.Payload, ' ',
    Decoy.Version); Flush(Output); // shared-hdr bp here
end;

end.
