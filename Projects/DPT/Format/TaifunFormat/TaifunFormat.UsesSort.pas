unit TaifunFormat.UsesSort;

interface

// Namespace group constants (ordering determines sort priority)
const
  GroupDelphiRTL  = 0;  // System.*, Winapi.*, Vcl.*, FMX.*, Data.*, Xml.*, Net.*, REST.*, Soap.*, Web.*
  GroupThirdParty = 1;
  GroupBase       = 2;
  GroupBaseUI     = 3;
  GroupBusiness   = 4;
  GroupBusinessUI = 5;
  GroupShared     = 6;  // Dms.Shared.*, Tos.Shared.*, Tpm.Shared.*
  GroupTfw        = 7;

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
  if LowerStartsWith(LLower, 'system.') or (LLower = 'system') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'winapi.') or (LLower = 'winapi') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'vcl.') or (LLower = 'vcl') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'fmx.') or (LLower = 'fmx') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'data.') or (LLower = 'data') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'xml.') or (LLower = 'xml') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'net.') or (LLower = 'net') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'rest.') or (LLower = 'rest') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'soap.') or (LLower = 'soap') then
    Exit(GroupDelphiRTL);
  if LowerStartsWith(LLower, 'web.') or (LLower = 'web') then
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

  // Shared modules
  if LowerStartsWith(LLower, 'dms.shared.') then
    Exit(GroupShared);
  if LowerStartsWith(LLower, 'tos.shared.') then
    Exit(GroupShared);
  if LowerStartsWith(LLower, 'tpm.shared.') then
    Exit(GroupShared);

  // Application-specific
  if LowerStartsWith(LLower, 'tfw.') or (LLower = 'tfw') then
    Exit(GroupTfw);

  // Everything else is third-party
  Result := GroupThirdParty;
end;

end.
