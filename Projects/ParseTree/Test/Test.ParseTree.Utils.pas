unit Test.ParseTree.Utils;

interface

uses
  System.SysUtils, System.IOUtils, System.SyncObjs;

/// <summary>Calculates a target output file path mimicking the source structure relative to ABaseDir.
/// Also ensures the target directory exists in a thread-safe manner using the provided ALock.</summary>
function GetTargetOutputFile(const ASourceFile, ABaseDir, ATargetRoot: string; const ALock: TCriticalSection): string;

implementation

function GetTargetOutputFile(const ASourceFile, ABaseDir, ATargetRoot: string; const ALock: TCriticalSection): string;
var
  LRelPath: string;
  LSanitizedPath: string;
begin
  if ASourceFile.StartsWith(ABaseDir, True) then
    LRelPath := ExtractRelativePath(ABaseDir, ASourceFile)
  else
  begin
    LSanitizedPath := StringReplace(TPath.GetFullPath(ASourceFile), ':', '', [rfReplaceAll]);
    while (Length(LSanitizedPath) > 0) and
          ((LSanitizedPath[1] = '\') or (LSanitizedPath[1] = '/')) do
      Delete(LSanitizedPath, 1, 1);
    LRelPath := LSanitizedPath;
  end;

  Result := TPath.Combine(ATargetRoot, LRelPath);

  // Ensure the target directory exists
  ALock.Enter;
  try
    if not TDirectory.Exists(TPath.GetDirectoryName(Result)) then
      TDirectory.CreateDirectory(TPath.GetDirectoryName(Result));
  finally
    ALock.Leave;
  end;
end;

end.
