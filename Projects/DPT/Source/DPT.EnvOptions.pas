// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.EnvOptions;

interface

uses

  JclIDEUtils,

  DPT.DProjAnalyzer;

type

  /// <summary>
  ///   The base values behind the Delphi build macros DPT expands itself.
  ///   Passed explicitly to <see cref="TEnvOptions.ExpandKnownMacros"/> so the
  ///   expansion stays a pure function, free of any installation dependency.
  /// </summary>
  TEnvMacros = record
    /// <summary>$(BDS) - the installation root, no trailing delimiter.</summary>
    Bds           : String;
    /// <summary>$(BDSUSERDIR) - the user's per-version Studio folder.</summary>
    BdsUserDir    : String;
    /// <summary>$(ProductVersion) - e.g. "23.0".</summary>
    ProductVersion: String;
    /// <summary>$(Platform) - 'Win32' / 'Win64'.</summary>
    Platform      : String;
    /// <summary>$(Config) - 'Release' / 'Debug' / ...</summary>
    Config        : String;
  end;

  /// <summary>
  ///   Reconstructs the unit search path the way the RAD Studio MSBuild
  ///   targets actually assemble it, so DPT's staleness check, search-path
  ///   printout and DCU auto-resolution all see exactly the directories the
  ///   compiler will.
  ///   <para>
  ///     The IDE-side library path is taken from the per-user
  ///     <c>EnvOptions.proj</c> - the file the Delphi targets import at build
  ///     time - instead of the JCL registry value, so it stays in sync with
  ///     what MSBuild really consumes. Every Delphi build macro that appears
  ///     in those paths (<c>$(BDS)</c>, <c>$(BDSLIB)</c>, <c>$(BDSUSERDIR)</c>,
  ///     <c>$(BDSCOMMONDIR)</c>, <c>$(Platform)</c>, <c>$(ProductVersion)</c>,
  ///     ...) is expanded.
  ///   </para>
  ///   <para>
  ///     All install-independent logic lives in the public <c>class</c>
  ///     functions below; the instance methods are thin wrappers that gather
  ///     the installation's values and delegate. This keeps the transformation
  ///     logic unit-testable without a Delphi installation on the machine.
  ///   </para>
  /// </summary>
  TEnvOptions = class
  private
    FInstallation  : TJclBorRADToolInstallation;
    FProductVersion: String;
    function GetBdsUserDir: String;
    function GetEnvOptionsFile: String;
    function ReadDelphiLibraryPath(const APlatform: String): String;
  public
    /// <param name="AInstallation">
    ///   The installation whose environment the paths are resolved against.
    ///   Not owned - the caller keeps ownership.
    /// </param>
    constructor Create(const AInstallation: TJclBorRADToolInstallation);

    // ---- pure helpers: no installation / no filesystem dependency ----

    /// <summary>
    ///   The product-version leaf of an installation root, e.g. "23.0" from
    ///   "...\Studio\23.0" (a trailing delimiter is tolerated).
    /// </summary>
    class function ProductVersionOf(const ARootDir: String): String;

    /// <summary>
    ///   The absolute EnvOptions.proj path MSBuild imports, for a given APPDATA
    ///   base and product version.
    /// </summary>
    class function EnvOptionsFileFor(const AAppData, AProductVersion: String): String;

    /// <summary>
    ///   Extracts the <c>&lt;DelphiLibraryPath&gt;</c> value for
    ///   <paramref name="APlatform"/> out of EnvOptions.proj content. Only
    ///   PropertyGroups carrying a <c>Condition</c> attribute are considered
    ///   (which also skips the leading empty <c>&lt;PropertyGroup/&gt;</c>),
    ///   and the platform is matched exactly so 'Win64' never picks up a
    ///   'Win64x' group. Returns '' when absent; the last matching group wins.
    /// </summary>
    class function ExtractLibraryPath(const AEnvOptionsXml, APlatform: String): String;

    /// <summary>
    ///   Derives <c>$(BDSUSERDIR)</c> from <c>$(BDSCOMMONDIR)</c> by re-rooting
    ///   the shared "...\Embarcadero\Studio\&lt;ver&gt;" tail (everything after
    ///   "\Documents\", matched case-insensitively) at
    ///   <paramref name="ADocumentsPath"/>. Uses
    ///   <paramref name="AFallbackSuffix"/> when the common dir carries no
    ///   "\Documents\" segment.
    /// </summary>
    class function BdsUserDirFrom(const ABdsCommonDir, ADocumentsPath,
      AFallbackSuffix: String): String;

    /// <summary>
    ///   Expands the macros DPT resolves itself (all except the ones left to
    ///   JCL's <c>SubstitutePath</c>). The full token including the closing
    ///   ')' is replaced, so <c>$(BDS)</c> never corrupts <c>$(BDSLIB)</c> and
    ///   the order is irrelevant. Case-insensitive; '' maps to ''.
    /// </summary>
    class function ExpandKnownMacros(const AValue: String; const AMacros: TEnvMacros): String;

    /// <summary>
    ///   Combines the project path and IDE library path in MSBuild order
    ///   (project first), splits on ';', drops empty entries, trims each and
    ///   strips a trailing path delimiter.
    /// </summary>
    class function SplitSearchPath(const AProjectPath, AIdePath: String): TArray<String>;

    // ---- installation-backed API ----

    /// <summary>
    ///   Expands every known Delphi build macro in <c>AValue</c> for the given
    ///   platform / configuration. Macros this class does not know about are
    ///   handed to JCL's <c>SubstitutePath</c>, which resolves
    ///   <c>$(BDSCOMMONDIR)</c>, <c>$(LANGDIR)</c> and the IDE's user-defined
    ///   environment variables.
    /// </summary>
    function ResolveMacros(const AValue, APlatform, AConfig: String): String;

    /// <summary>
    ///   The IDE library search path for the platform (<c>'Win32'</c> /
    ///   <c>'Win64'</c>), macro-resolved and sourced from
    ///   <c>EnvOptions.proj</c>. Falls back to the JCL registry value when the
    ///   file is missing or carries no entry for the platform.
    /// </summary>
    function LibrarySearchPath(const APlatform: String): String;

    /// <summary>
    ///   The full, ordered unit search path a build would use: the project's
    ///   own <c>DCC_UnitSearchPath</c> first, then the IDE library path -
    ///   exactly the order the RAD Studio targets concatenate them
    ///   (<c>_ObjectPath</c> in <c>CodeGear.Delphi.Targets</c>). All entries
    ///   are macro-resolved and trimmed; project entries stay relative to the
    ///   <c>.dproj</c> so the caller can resolve them against the project
    ///   directory itself.
    /// </summary>
    function EffectiveUnitSearchPath(AAnalyzer: TDProjAnalyzer;
      const AConfig, APlatform: String): TArray<String>;

    /// <summary>Product-version leaf of the installation root, e.g. "23.0".</summary>
    property ProductVersion: String read FProductVersion;

    /// <summary>Absolute path of the EnvOptions.proj that MSBuild imports.</summary>
    property EnvOptionsFile: String read GetEnvOptionsFile;
  end;

implementation

uses

  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions;

{ TEnvOptions }

constructor TEnvOptions.Create(const AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create;
  FInstallation := AInstallation;
  FProductVersion := ProductVersionOf(FInstallation.RootDir);
end;

class function TEnvOptions.ProductVersionOf(const ARootDir: String): String;
begin
  // The RAD Studio build system uses "23.0"-style versions; the installation
  // root always ends on exactly that folder (e.g. "...\Studio\23.0").
  Result := ExtractFileName(ExcludeTrailingPathDelimiter(ARootDir));
end;

class function TEnvOptions.EnvOptionsFileFor(const AAppData, AProductVersion: String): String;
begin
  // The Delphi targets import
  //   $(APPDATA)\Embarcadero\BDS\$(ProductVersion)\EnvOptions.proj
  Result := Format('%s\Embarcadero\BDS\%s\EnvOptions.proj',
    [ExcludeTrailingPathDelimiter(AAppData), AProductVersion]);
end;

class function TEnvOptions.ExtractLibraryPath(const AEnvOptionsXml, APlatform: String): String;
var
  Body : String;
  Cond : String;
  Group: TMatch;
  Val  : TMatch;
begin
  Result := '';

  // Only PropertyGroups that carry a Condition are relevant; the library path
  // always sits in a platform-conditioned group. Requiring the Condition
  // attribute also skips the leading empty <PropertyGroup/> cleanly.
  for Group in TRegEx.Matches(AEnvOptionsXml,
    '<PropertyGroup\s+Condition="([^"]*)">([\s\S]*?)</PropertyGroup>', [roIgnoreCase]) do
  begin
    Cond := Group.Groups[1].Value;
    Body := Group.Groups[2].Value;

    // Exact platform match ('Win64' must not also match 'Win64x').
    if not TRegEx.IsMatch(Cond,
      Format('''\$\(Platform\)''==''%s''', [APlatform]), [roIgnoreCase]) then
      Continue;

    Val := TRegEx.Match(Body,
      '<DelphiLibraryPath>([\s\S]*?)</DelphiLibraryPath>', [roIgnoreCase]);
    if Val.Success then
      Result := Val.Groups[1].Value; // last matching group wins
  end;
end;

class function TEnvOptions.BdsUserDirFrom(const ABdsCommonDir, ADocumentsPath,
  AFallbackSuffix: String): String;
const
  DocsSep = '\Documents\';
var
  P     : Integer;
  Suffix: String;
begin
  // $(BDSUSERDIR) mirrors $(BDSCOMMONDIR) but lives under the user's Documents
  // instead of the public Documents. Lift the shared
  // "...\Embarcadero\Studio\<ver>" tail off the common dir and re-root it at
  // the user's Documents folder. This stays correct across the
  // "RAD Studio" / "Studio" naming split and redirected Documents folders.
  P := Pos(UpperCase(DocsSep), UpperCase(ABdsCommonDir));
  if P > 0 then
    Suffix := Copy(ABdsCommonDir, P + Length(DocsSep), MaxInt)
  else
    Suffix := AFallbackSuffix;

  Result := ExcludeTrailingPathDelimiter(
    IncludeTrailingPathDelimiter(ADocumentsPath) + Suffix);
end;

class function TEnvOptions.ExpandKnownMacros(const AValue: String;
  const AMacros: TEnvMacros): String;
begin
  Result := AValue;
  if Result = '' then
    Exit;

  // The full token including the closing ')' is replaced, so "$(BDS)" never
  // corrupts "$(BDSLIB)" and the order among these is irrelevant.
  Result := StringReplace(Result, '$(BDSINCLUDE)', AMacros.Bds + '\include', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(BDSLIB)', AMacros.Bds + '\lib', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(BDSUSERDIR)', AMacros.BdsUserDir, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(ProductVersion)', AMacros.ProductVersion, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(Platform)', AMacros.Platform, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(Config)', AMacros.Config, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '$(BDS)', AMacros.Bds, [rfReplaceAll, rfIgnoreCase]);
end;

class function TEnvOptions.SplitSearchPath(const AProjectPath, AIdePath: String): TArray<String>;
var
  Combined: String;
  Entry   : String;
  Trimmed : String;
begin
  // MSBuild order: DCC_UnitSearchPath is searched before the IDE library path.
  if (AProjectPath <> '') and (AIdePath <> '') then
    Combined := AProjectPath + ';' + AIdePath
  else
    Combined := AProjectPath + AIdePath;

  Result := nil;
  for Entry in Combined.Split([';'], TStringSplitOptions.ExcludeEmpty) do
  begin
    Trimmed := Trim(Entry);
    if Trimmed <> '' then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := ExcludeTrailingPathDelimiter(Trimmed);
    end;
  end;
end;

function TEnvOptions.GetEnvOptionsFile: String;
begin
  Result := EnvOptionsFileFor(GetEnvironmentVariable('APPDATA'), FProductVersion);
end;

function TEnvOptions.GetBdsUserDir: String;
begin
  // JCL already resolves $(BDSCOMMONDIR) reliably; derive $(BDSUSERDIR) from it.
  Result := BdsUserDirFrom(
    FInstallation.SubstitutePath('$(BDSCOMMONDIR)'),
    TPath.GetDocumentsPath,
    'Embarcadero\Studio\' + FProductVersion);
end;

function TEnvOptions.ReadDelphiLibraryPath(const APlatform: String): String;
var
  FileName: String;
begin
  FileName := GetEnvOptionsFile;
  if not TFile.Exists(FileName) then
    Exit('');
  Result := ExtractLibraryPath(TFile.ReadAllText(FileName, TEncoding.UTF8), APlatform);
end;

function TEnvOptions.ResolveMacros(const AValue, APlatform, AConfig: String): String;
var
  Macros: TEnvMacros;
begin
  if AValue = '' then
    Exit('');

  Macros.Bds := ExcludeTrailingPathDelimiter(FInstallation.RootDir);
  Macros.BdsUserDir := GetBdsUserDir;
  Macros.ProductVersion := FProductVersion;
  Macros.Platform := APlatform;
  Macros.Config := AConfig;

  Result := ExpandKnownMacros(AValue, Macros);

  // Let JCL resolve the remainder ($(BDSCOMMONDIR), $(LANGDIR) and the IDE's
  // user-defined "Environment Variables") from the process environment and
  // registry - the same source the IDE writes EnvOptions.proj from.
  Result := FInstallation.SubstitutePath(Result);
end;

function TEnvOptions.LibrarySearchPath(const APlatform: String): String;
var
  Raw: String;
begin
  Raw := ReadDelphiLibraryPath(APlatform);
  if Raw <> '' then
    Result := ResolveMacros(Raw, APlatform, '')
  else if SameText(APlatform, 'Win64') then
    Result := FInstallation.LibrarySearchPath[bpWin64]
  else
    Result := FInstallation.LibrarySearchPath[bpWin32];
end;

function TEnvOptions.EffectiveUnitSearchPath(AAnalyzer: TDProjAnalyzer;
  const AConfig, APlatform: String): TArray<String>;
begin
  Result := SplitSearchPath(
    ResolveMacros(AAnalyzer.GetProjectSearchPath(AConfig, APlatform), APlatform, AConfig),
    LibrarySearchPath(APlatform));
end;

end.
