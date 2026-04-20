unit TaifunFormat.UsesSort;

interface

// Namespace group constants (ordering determines sort priority)
const
  GroupDelphiRTL  = 0;  // System.*, Winapi.*, Vcl.*, FMX.*, Data.*, Datasnap.*, Xml.*, Net.*, REST.*, Soap.*, Web.*
  GroupThirdParty = 1;
  GroupBase       = 2;
  GroupBaseUI     = 3;
  GroupBusiness   = 4;
  GroupBusinessUI = 5;
  GroupShared     = 6;  // Dms.Shared.*, Tos.Shared.*, Tpm.Shared.*
  GroupApp        = 7;  // Application namespaces: DMS, MKH, PB, PLZ, RKA, SOA, TDM, TED, TES, TFH, TFI, TFK, TFR, TFW, TIM, TLS, TMS, TOS, TPM, TRD

function GetNamespaceGroup(const AName: string): Integer;

implementation

function LowerStartsWith(const ALower, APrefix: string): Boolean;
begin
  Result := Copy(ALower, 1, Length(APrefix)) = APrefix;
end;

function GetNamespaceGroup(const AName: string): Integer;
var
  LLower: string;
begin
  LLower := LowerCase(AName);

  // Delphi RTL (all standard Embarcadero namespaces in one block)
  if LowerStartsWith(LLower, 'data.') or (LLower = 'data') or
     LowerStartsWith(LLower, 'datasnap.') or (LLower = 'datasnap') or
     LowerStartsWith(LLower, 'fmx.') or (LLower = 'fmx') or
     LowerStartsWith(LLower, 'net.') or (LLower = 'net') or
     LowerStartsWith(LLower, 'rest.') or (LLower = 'rest') or
     LowerStartsWith(LLower, 'soap.') or (LLower = 'soap') or
     LowerStartsWith(LLower, 'system.') or (LLower = 'system') or
     LowerStartsWith(LLower, 'vcl.') or (LLower = 'vcl') or
     LowerStartsWith(LLower, 'web.') or (LLower = 'web') or
     LowerStartsWith(LLower, 'winapi.') or (LLower = 'winapi') or
     LowerStartsWith(LLower, 'xml.') or (LLower = 'xml') then
    Exit(GroupDelphiRTL);

  // Base.UI.* must be checked BEFORE Base.*
  if LowerStartsWith(LLower, 'base.ui.') then
    Exit(GroupBaseUI);
  if LowerStartsWith(LLower, 'base.') or (LLower = 'base') then
    Exit(GroupBase);

  // Business.UI.* must be checked BEFORE Business.*
  if LowerStartsWith(LLower, 'business.ui.') then
    Exit(GroupBusinessUI);
  if LowerStartsWith(LLower, 'business.') or (LLower = 'business') then
    Exit(GroupBusiness);

  // Shared modules (checked before App so that Dms.Shared.* does not fall into GroupApp)
  if LowerStartsWith(LLower, 'dms.shared.') or
     LowerStartsWith(LLower, 'tos.shared.') or
     LowerStartsWith(LLower, 'tpm.shared.') then
    Exit(GroupShared);

  // Application-specific namespaces (sorted alphabetically within the block)
  if LowerStartsWith(LLower, 'dms.') or (LLower = 'dms') or
     LowerStartsWith(LLower, 'mkh.') or (LLower = 'mkh') or
     LowerStartsWith(LLower, 'pb.') or (LLower = 'pb') or
     LowerStartsWith(LLower, 'plz.') or (LLower = 'plz') or
     LowerStartsWith(LLower, 'rka.') or (LLower = 'rka') or
     LowerStartsWith(LLower, 'soa.') or (LLower = 'soa') or
     LowerStartsWith(LLower, 'tdm.') or (LLower = 'tdm') or
     LowerStartsWith(LLower, 'ted.') or (LLower = 'ted') or
     LowerStartsWith(LLower, 'tes.') or (LLower = 'tes') or
     LowerStartsWith(LLower, 'tfh.') or (LLower = 'tfh') or
     LowerStartsWith(LLower, 'tfi.') or (LLower = 'tfi') or
     LowerStartsWith(LLower, 'tfk.') or (LLower = 'tfk') or
     LowerStartsWith(LLower, 'tfr.') or (LLower = 'tfr') or
     LowerStartsWith(LLower, 'tfw.') or (LLower = 'tfw') or
     LowerStartsWith(LLower, 'tim.') or (LLower = 'tim') or
     LowerStartsWith(LLower, 'tls.') or (LLower = 'tls') or
     LowerStartsWith(LLower, 'tms.') or (LLower = 'tms') or
     LowerStartsWith(LLower, 'tos.') or (LLower = 'tos') or
     LowerStartsWith(LLower, 'tpm.') or (LLower = 'tpm') or
     LowerStartsWith(LLower, 'trd.') or (LLower = 'trd') then
    Exit(GroupApp);

  // Everything else is third-party
  Result := GroupThirdParty;
end;

end.
