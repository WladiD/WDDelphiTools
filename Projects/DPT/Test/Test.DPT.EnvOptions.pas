// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.EnvOptions;

interface

uses

  System.SysUtils,

  DUnitX.TestFramework,

  DPT.EnvOptions;

type

  /// <summary>
  ///   Exercises the install-independent logic of <see cref="TEnvOptions"/>.
  ///   Every test targets a pure <c>class function</c> with hand-crafted
  ///   inputs, so the suite runs on any machine without a Delphi installation,
  ///   an EnvOptions.proj on disk or a specific user profile.
  /// </summary>
  [TestFixture]
  TTestEnvOptions = class
  private
    /// <summary>A representative EnvOptions.proj: a leading empty group, then
    ///  platform-conditioned groups (incl. the Win64/Win64x trap).</summary>
    function SampleEnvOptions: String;
    procedure AssertPaths(const AExpected: array of String; const AActual: TArray<String>);
    function Macros(const ABds, ABdsUserDir, AProductVersion, APlatform, AConfig: String): TEnvMacros;
  public
    // ---- ProductVersionOf ----
    [Test]
    procedure ProductVersionOf_NoTrailingDelimiter;
    [Test]
    procedure ProductVersionOf_TrailingDelimiter;

    // ---- EnvOptionsFileFor ----
    [Test]
    procedure EnvOptionsFileFor_BuildsImportPath;
    [Test]
    procedure EnvOptionsFileFor_ToleratesTrailingDelimiter;

    // ---- ExtractLibraryPath ----
    [Test]
    procedure ExtractLibraryPath_PicksWin32;
    [Test]
    procedure ExtractLibraryPath_PicksWin64;
    [Test]
    procedure ExtractLibraryPath_Win64DoesNotMatchWin64x;
    [Test]
    procedure ExtractLibraryPath_IgnoresEmptyAndConditionlessGroups;
    [Test]
    procedure ExtractLibraryPath_MissingPlatformIsEmpty;
    [Test]
    procedure ExtractLibraryPath_NoLibraryPathElementIsEmpty;
    [Test]
    procedure ExtractLibraryPath_LastMatchWins;

    // ---- ExpandKnownMacros ----
    [Test]
    procedure ExpandKnownMacros_BdsDoesNotCorruptBdsLib;
    [Test]
    procedure ExpandKnownMacros_ProductVersionAndPlatform;
    [Test]
    procedure ExpandKnownMacros_ConfigAndUserDir;
    [Test]
    procedure ExpandKnownMacros_CaseInsensitive;
    [Test]
    procedure ExpandKnownMacros_LeavesUnknownMacroForJcl;
    [Test]
    procedure ExpandKnownMacros_EmptyStaysEmpty;

    // ---- SplitSearchPath ----
    [Test]
    procedure SplitSearchPath_ProjectBeforeIde;
    [Test]
    procedure SplitSearchPath_TrimsAndStripsTrailingDelimiter;
    [Test]
    procedure SplitSearchPath_DropsEmptyEntries;
    [Test]
    procedure SplitSearchPath_IdeOnlyHasNoLeadingBlank;
    [Test]
    procedure SplitSearchPath_BothEmptyIsEmptyArray;

    // ---- BdsUserDirFrom ----
    [Test]
    procedure BdsUserDirFrom_ReRootsCommonDirAtUserDocuments;
    [Test]
    procedure BdsUserDirFrom_DocumentsMatchIsCaseInsensitive;
    [Test]
    procedure BdsUserDirFrom_FallbackWhenNoDocumentsSegment;
  end;

implementation

{ TTestEnvOptions }

function TTestEnvOptions.SampleEnvOptions: String;
begin
  // Mirrors the real layout: an empty <PropertyGroup/> up front, then one
  // group per platform. The Win64x group is present on purpose to prove that
  // a 'Win64' lookup does not spill into it. DelphiLibraryPath values are kept
  // raw (macros intact) - ExtractLibraryPath must not resolve them.
  Result := '''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
        <PropertyGroup/>
        <PropertyGroup Condition="'$(Platform)'=='Win64'">
            <DelphiLibraryPath>$(BDSLIB)\$(Platform)\release;WIN64_ONLY</DelphiLibraryPath>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Platform)'=='Win64x'">
            <DelphiLibraryPath>WIN64X_ONLY</DelphiLibraryPath>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Platform)'=='Win32'">
            <DelphiLibraryPath>$(BDSLIB)\$(Platform)\release;WIN32_ONLY</DelphiLibraryPath>
        </PropertyGroup>
    </Project>
    ''';
end;

function TTestEnvOptions.Macros(const ABds, ABdsUserDir, AProductVersion,
  APlatform, AConfig: String): TEnvMacros;
begin
  Result.Bds := ABds;
  Result.BdsUserDir := ABdsUserDir;
  Result.ProductVersion := AProductVersion;
  Result.Platform := APlatform;
  Result.Config := AConfig;
end;

procedure TTestEnvOptions.AssertPaths(const AExpected: array of String;
  const AActual: TArray<String>);
var
  I: Integer;
begin
  Assert.AreEqual(Length(AExpected), Length(AActual), 'entry count');
  for I := 0 to High(AExpected) do
    Assert.AreEqual(AExpected[I], AActual[I], 'entry ' + IntToStr(I));
end;

// ---- ProductVersionOf ----

procedure TTestEnvOptions.ProductVersionOf_NoTrailingDelimiter;
begin
  Assert.AreEqual('23.0',
    TEnvOptions.ProductVersionOf('C:\Program Files (x86)\Embarcadero\Studio\23.0'));
end;

procedure TTestEnvOptions.ProductVersionOf_TrailingDelimiter;
begin
  Assert.AreEqual('23.0',
    TEnvOptions.ProductVersionOf('C:\Program Files (x86)\Embarcadero\Studio\23.0\'));
end;

// ---- EnvOptionsFileFor ----

procedure TTestEnvOptions.EnvOptionsFileFor_BuildsImportPath;
begin
  Assert.AreEqual(
    'C:\Users\me\AppData\Roaming\Embarcadero\BDS\23.0\EnvOptions.proj',
    TEnvOptions.EnvOptionsFileFor('C:\Users\me\AppData\Roaming', '23.0'));
end;

procedure TTestEnvOptions.EnvOptionsFileFor_ToleratesTrailingDelimiter;
begin
  Assert.AreEqual(
    'C:\Users\me\AppData\Roaming\Embarcadero\BDS\23.0\EnvOptions.proj',
    TEnvOptions.EnvOptionsFileFor('C:\Users\me\AppData\Roaming\', '23.0'));
end;

// ---- ExtractLibraryPath ----

procedure TTestEnvOptions.ExtractLibraryPath_PicksWin32;
begin
  Assert.AreEqual('$(BDSLIB)\$(Platform)\release;WIN32_ONLY',
    TEnvOptions.ExtractLibraryPath(SampleEnvOptions, 'Win32'));
end;

procedure TTestEnvOptions.ExtractLibraryPath_PicksWin64;
begin
  Assert.AreEqual('$(BDSLIB)\$(Platform)\release;WIN64_ONLY',
    TEnvOptions.ExtractLibraryPath(SampleEnvOptions, 'Win64'));
end;

procedure TTestEnvOptions.ExtractLibraryPath_Win64DoesNotMatchWin64x;
begin
  // The exact-match guard: 'Win64' must not pick up the 'Win64x' group.
  Assert.AreEqual('$(BDSLIB)\$(Platform)\release;WIN64_ONLY',
    TEnvOptions.ExtractLibraryPath(SampleEnvOptions, 'Win64'));
  // And 'Win64x' resolves to its own group, not Win64's.
  Assert.AreEqual('WIN64X_ONLY',
    TEnvOptions.ExtractLibraryPath(SampleEnvOptions, 'Win64x'));
end;

procedure TTestEnvOptions.ExtractLibraryPath_IgnoresEmptyAndConditionlessGroups;
var
  Xml: String;
begin
  // A conditionless group carrying a DelphiLibraryPath must never be returned
  // for a platform lookup, and the empty <PropertyGroup/> must not derail the
  // scan.
  Xml := '''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
        <PropertyGroup/>
        <PropertyGroup>
            <DelphiLibraryPath>SHOULD_NOT_WIN</DelphiLibraryPath>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Platform)'=='Win32'">
            <DelphiLibraryPath>WIN32_ONLY</DelphiLibraryPath>
        </PropertyGroup>
    </Project>
    ''';
  Assert.AreEqual('WIN32_ONLY', TEnvOptions.ExtractLibraryPath(Xml, 'Win32'));
  Assert.AreEqual('', TEnvOptions.ExtractLibraryPath(Xml, 'Win64'));
end;

procedure TTestEnvOptions.ExtractLibraryPath_MissingPlatformIsEmpty;
begin
  Assert.AreEqual('', TEnvOptions.ExtractLibraryPath(SampleEnvOptions, 'OSX64'));
end;

procedure TTestEnvOptions.ExtractLibraryPath_NoLibraryPathElementIsEmpty;
var
  Xml: String;
begin
  Xml := '''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
        <PropertyGroup Condition="'$(Platform)'=='Win32'">
            <DelphiDCPOutput>$(BDSCOMMONDIR)\Dcp</DelphiDCPOutput>
        </PropertyGroup>
    </Project>
    ''';
  Assert.AreEqual('', TEnvOptions.ExtractLibraryPath(Xml, 'Win32'));
end;

procedure TTestEnvOptions.ExtractLibraryPath_LastMatchWins;
var
  Xml: String;
begin
  // Two groups match the platform (as MSBuild allows); the later one wins.
  Xml := '''
    <Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
        <PropertyGroup Condition="'$(Platform)'=='Win32'">
            <DelphiLibraryPath>FIRST</DelphiLibraryPath>
        </PropertyGroup>
        <PropertyGroup Condition="'$(Platform)'=='Win32'">
            <DelphiLibraryPath>SECOND</DelphiLibraryPath>
        </PropertyGroup>
    </Project>
    ''';
  Assert.AreEqual('SECOND', TEnvOptions.ExtractLibraryPath(Xml, 'Win32'));
end;

// ---- ExpandKnownMacros ----

procedure TTestEnvOptions.ExpandKnownMacros_BdsDoesNotCorruptBdsLib;
begin
  // "$(BDS)" as a full token must not match inside "$(BDSLIB)"/"$(BDSINCLUDE)".
  Assert.AreEqual(
    'C:\S\23.0\lib\Win32;C:\S\23.0\include;C:\S\23.0\Imports',
    TEnvOptions.ExpandKnownMacros(
      '$(BDSLIB)\Win32;$(BDSINCLUDE);$(BDS)\Imports',
      Macros('C:\S\23.0', '', '23.0', 'Win32', 'Release')));
end;

procedure TTestEnvOptions.ExpandKnownMacros_ProductVersionAndPlatform;
begin
  Assert.AreEqual(
    'tools\vcl\pas_23.0;DCU_23.0_Win64',
    TEnvOptions.ExpandKnownMacros(
      'tools\vcl\pas_$(ProductVersion);DCU_$(ProductVersion)_$(Platform)',
      Macros('C:\S\23.0', '', '23.0', 'Win64', 'Release')));
end;

procedure TTestEnvOptions.ExpandKnownMacros_ConfigAndUserDir;
begin
  Assert.AreEqual(
    'C:\Users\me\Documents\Embarcadero\Studio\23.0\Imports;out\Debug',
    TEnvOptions.ExpandKnownMacros(
      '$(BDSUSERDIR)\Imports;out\$(Config)',
      Macros('C:\S\23.0', 'C:\Users\me\Documents\Embarcadero\Studio\23.0', '23.0', 'Win32', 'Debug')));
end;

procedure TTestEnvOptions.ExpandKnownMacros_CaseInsensitive;
begin
  Assert.AreEqual('C:\S\23.0\Imports',
    TEnvOptions.ExpandKnownMacros('$(bds)\Imports',
      Macros('C:\S\23.0', '', '23.0', 'Win32', 'Release')));
end;

procedure TTestEnvOptions.ExpandKnownMacros_LeavesUnknownMacroForJcl;
begin
  // $(BDSCOMMONDIR) is resolved later by JCL's SubstitutePath, so this pure
  // step must leave it untouched.
  Assert.AreEqual('$(BDSCOMMONDIR)\Dcp',
    TEnvOptions.ExpandKnownMacros('$(BDSCOMMONDIR)\Dcp',
      Macros('C:\S\23.0', '', '23.0', 'Win32', 'Release')));
end;

procedure TTestEnvOptions.ExpandKnownMacros_EmptyStaysEmpty;
begin
  Assert.AreEqual('',
    TEnvOptions.ExpandKnownMacros('', Macros('C:\S\23.0', '', '23.0', 'Win32', 'Release')));
end;

// ---- SplitSearchPath ----

procedure TTestEnvOptions.SplitSearchPath_ProjectBeforeIde;
begin
  AssertPaths(['A', 'B', 'C', 'D'], TEnvOptions.SplitSearchPath('A;B', 'C;D'));
end;

procedure TTestEnvOptions.SplitSearchPath_TrimsAndStripsTrailingDelimiter;
begin
  // Whitespace trimmed; a trailing '\' (the real "...\slim\pas\" case) removed.
  AssertPaths(['A', 'slim\pas'], TEnvOptions.SplitSearchPath('  A  ; slim\pas\ ', ''));
end;

procedure TTestEnvOptions.SplitSearchPath_DropsEmptyEntries;
begin
  AssertPaths(['A', 'B'], TEnvOptions.SplitSearchPath('A;;B;', ''));
end;

procedure TTestEnvOptions.SplitSearchPath_IdeOnlyHasNoLeadingBlank;
begin
  // An empty project path must not inject a leading separator / blank entry.
  AssertPaths(['C', 'D'], TEnvOptions.SplitSearchPath('', 'C;D'));
end;

procedure TTestEnvOptions.SplitSearchPath_BothEmptyIsEmptyArray;
begin
  Assert.AreEqual(0, Length(TEnvOptions.SplitSearchPath('', '')));
end;

// ---- BdsUserDirFrom ----

procedure TTestEnvOptions.BdsUserDirFrom_ReRootsCommonDirAtUserDocuments;
begin
  Assert.AreEqual(
    'C:\Users\me\Documents\Embarcadero\Studio\23.0',
    TEnvOptions.BdsUserDirFrom(
      'C:\Users\Public\Documents\Embarcadero\Studio\23.0',
      'C:\Users\me\Documents',
      'Embarcadero\Studio\23.0'));
end;

procedure TTestEnvOptions.BdsUserDirFrom_DocumentsMatchIsCaseInsensitive;
begin
  Assert.AreEqual(
    'C:\Users\me\Documents\Embarcadero\Studio\23.0',
    TEnvOptions.BdsUserDirFrom(
      'C:\Users\Public\DOCUMENTS\Embarcadero\Studio\23.0',
      'C:\Users\me\Documents',
      'FALLBACK'));
end;

procedure TTestEnvOptions.BdsUserDirFrom_FallbackWhenNoDocumentsSegment;
begin
  Assert.AreEqual(
    'C:\Users\me\Documents\Embarcadero\Studio\23.0',
    TEnvOptions.BdsUserDirFrom(
      'D:\SomewhereElse\Studio\23.0',
      'C:\Users\me\Documents',
      'Embarcadero\Studio\23.0'));
end;

end.
