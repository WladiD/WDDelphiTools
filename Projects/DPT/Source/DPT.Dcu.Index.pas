// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Dcu.Index;

interface

uses

  System.Classes,
  System.SysUtils,

  mormot.core.collections,

  DPT.Dcu.Types;

const
  /// <summary>
  ///   JSON schema version emitted by the index builder. v2 added the
  ///   exported-symbol fields and the symbol-to-defining-unit map.
  ///   The reader still accepts v1 documents (the new fields just stay
  ///   empty in that case).
  /// </summary>
  DcuIndexSchemaVersion = 2;

type

  /// <summary>
  ///   One row per DCU in a built index. Carries every field the
  ///   <see cref="DPT.Dcu.Analyzer.TDcuAnalyzer"/> currently extracts so
  ///   queries can run without re-reading any DCU file.
  /// </summary>
  TDcuIndexEntry = record
    UnitName          : string;
    FilePath          : string;
    FileSize          : Int64;
    Magic             : string;
    Compiler          : TDcuKnownCompiler;
    Platform          : TDcuPlatform;
    PrimarySource     : string;
    Includes          : IList<string>;
    InterfaceUses     : IList<string>;
    ImplementationUses: IList<string>;
    TypeRefs          : IList<string>;
    MethodRefs        : IList<string>;
    ExportedTypes     : IList<string>;
    ExportedRoutines  : IList<string>;
  end;

  /// <summary>
  ///   The aggregated index: metadata plus the per-DCU rows and the
  ///   pre-computed reverse-import map. The reverse map is built once
  ///   at <c>Build</c> time so <c>Query --ImportedBy</c> answers in O(1).
  /// </summary>
  TDcuIndex = record
    Schema            : Integer;
    BuildTimestamp    : TDateTime;
    RootDirs          : IList<string>;
    Units             : IList<TDcuIndexEntry>;
    /// <summary>imported-unit-name -> [importing unit names]</summary>
    ReverseImportIndex: IKeyValue<string, IList<string>>;
    /// <summary>
    ///   exported-symbol-name (lower-case) -> [unit names declaring it].
    ///   Pre-computed at Build time so <c>DcuIndex Query --DefinedIn</c>
    ///   resolves in O(1). Multiple unit names per key are possible
    ///   when the same symbol exists across compiler/platform variants.
    /// </summary>
    SymbolToDefiningUnit: IKeyValue<string, IList<string>>;
  end;

  /// <summary>
  ///   Walks one or more directory trees, runs the DCU analyzer on every
  ///   matching file and aggregates the results into a TDcuIndex. Build
  ///   is the expensive step; downstream consumers operate on the cached
  ///   JSON via <see cref="TDcuIndexJson"/> and <see cref="TDcuIndexQuery"/>.
  /// </summary>
  /// <summary>
  ///   Per-file failure recorded during a Build. The builder collects
  ///   these instead of letting one bad DCU abort the whole index.
  /// </summary>
  TDcuIndexBuildFailure = record
    FilePath: string;
    Reason  : string;
    constructor Create(const AFilePath, AReason: string);
  end;

  TDcuIndexBuilder = class
  private
    class function CollectFiles(const ARootDirs: array of string;
      ARecursive: Boolean; const APattern: string): TArray<string>;
    class procedure BuildReverseIndex(var AIndex: TDcuIndex);
  public
    /// <summary>Builds an index from the given directories.</summary>
    /// <param name="ARecursive">Walk subdirectories.</param>
    /// <param name="AParallel">Analyze files concurrently.</param>
    /// <param name="APattern">File-mask filter (default <c>*.dcu</c>).</param>
    /// <param name="AFailures">Optional out-list collecting per-file
    ///   failures so a single bad DCU does not abort the whole build.</param>
    class function Build(const ARootDirs: array of string;
      ARecursive: Boolean = True; AParallel: Boolean = True;
      const APattern: string = '*.dcu';
      AFailures: IList<TDcuIndexBuildFailure> = nil): TDcuIndex;
  end;

  /// <summary>
  ///   Query layer over a loaded index. All lookups are case-insensitive
  ///   and return string lists (or full entries when richer detail is
  ///   needed).
  /// </summary>
  TDcuIndexQuery = class
  private
    FIndex: TDcuIndex;
  public
    constructor Create(const AIndex: TDcuIndex);
    /// <summary>Returns every entry whose unit name equals <c>AUnitName</c>.
    /// Multiple matches are possible when the same unit was indexed for
    /// several platforms.</summary>
    function FindByUnit(const AUnitName: string): IList<TDcuIndexEntry>;
    /// <summary>Returns the names of every unit that imports
    /// <c>AUnitName</c> (across both interface and implementation
    /// uses).</summary>
    function FindImportedBy(const AUnitName: string): IList<string>;
    /// <summary>Returns the union of interface and implementation uses
    /// of <c>AUnitName</c>.</summary>
    function FindImportsOf(const AUnitName: string): IList<string>;
    /// <summary>Returns the names of every unit that references
    /// <c>ASymbol</c> via its imported type or method cross-references.</summary>
    function FindReferences(const ASymbol: string): IList<string>;
    /// <summary>Returns every unit that **declares** <c>ASymbol</c> as
    /// one of its own exported types or routines. Inverse of
    /// <see cref="FindReferences"/>: that one finds consumers, this one
    /// finds the definition site.</summary>
    function FindDefinitionOf(const ASymbol: string): IList<string>;
    property Index: TDcuIndex read FIndex;
  end;

implementation

uses

  System.IOUtils,
  System.Threading,
  System.Types,

  DPT.Dcu.Analyzer;

function ListFromAnalyzerList(ASource: IList<TDcuUsesEntry>): IList<string>; overload;
var
  Entry: TDcuUsesEntry;
begin
  Result := Collections.NewList<string>;
  if ASource = nil then
    Exit;
  for Entry in ASource do
    Result.Add(Entry.UnitName);
end;

function ListFromAnalyzerList(ASource: IList<TDcuSourceRef>): IList<string>; overload;
var
  Ref: TDcuSourceRef;
begin
  Result := Collections.NewList<string>;
  if ASource = nil then
    Exit;
  for Ref in ASource do
    Result.Add(Ref.FileName);
end;

procedure SplitSymbols(ASource: IList<TDcuSymbolRef>;
  out AImpTypes, AImpMethods, AExpTypes, AExpRoutines: IList<string>);
var
  Sym: TDcuSymbolRef;
begin
  AImpTypes := Collections.NewList<string>;
  AImpMethods := Collections.NewList<string>;
  AExpTypes := Collections.NewList<string>;
  AExpRoutines := Collections.NewList<string>;
  if ASource = nil then
    Exit;
  for Sym in ASource do
    case Sym.Origin of
      dsoImported:
        case Sym.Kind of
          dskType  : AImpTypes.Add(Sym.Name);
          dskMethod: AImpMethods.Add(Sym.Name);
        end;
      dsoExported:
        case Sym.Kind of
          dskType  : AExpTypes.Add(Sym.Name);
          dskMethod: AExpRoutines.Add(Sym.Name);
        end;
    end;
end;

function AnalyzeOne(const AFilePath: string;
  out AErrorMsg: string): TDcuIndexEntry;
var
  R: TDcuAnalysisResult;
begin
  AErrorMsg := '';
  Result := Default(TDcuIndexEntry);
  Result.FilePath := AFilePath;
  try
    R := TDcuAnalyzer.Analyze(AFilePath);
  except
    on E: Exception do
    begin
      AErrorMsg := E.ClassName + ': ' + E.Message;
      Exit;
    end;
  end;
  Result.FileSize := R.FileSize;
  Result.Magic := R.Header.MagicHex;
  Result.Compiler := R.Header.DetectedCompiler;
  Result.Platform := R.Header.DetectedPlatform;
  Result.UnitName := R.Header.UnitName;
  Result.PrimarySource := R.Header.PrimarySource.FileName;
  Result.Includes := ListFromAnalyzerList(R.Header.IncludeSources);
  Result.InterfaceUses := ListFromAnalyzerList(R.InterfaceUses);
  Result.ImplementationUses := ListFromAnalyzerList(R.ImplementationUses);
  SplitSymbols(R.Symbols, Result.TypeRefs, Result.MethodRefs,
    Result.ExportedTypes, Result.ExportedRoutines);
end;

{ TDcuIndexBuildFailure }

constructor TDcuIndexBuildFailure.Create(const AFilePath, AReason: string);
begin
  FilePath := AFilePath;
  Reason := AReason;
end;

{ TDcuIndexBuilder }

class function TDcuIndexBuilder.CollectFiles(const ARootDirs: array of string;
  ARecursive: Boolean; const APattern: string): TArray<string>;
var
  Found    : TStringDynArray;
  RootDir  : string;
  SearchOpt: TSearchOption;
  Path     : string;
begin
  Result := nil;
  if ARecursive then
    SearchOpt := TSearchOption.soAllDirectories
  else
    SearchOpt := TSearchOption.soTopDirectoryOnly;
  for RootDir in ARootDirs do
  begin
    if not TDirectory.Exists(RootDir) then
      Continue;
    Found := TDirectory.GetFiles(RootDir, APattern, SearchOpt);
    for Path in Found do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Path;
    end;
  end;
end;

class procedure TDcuIndexBuilder.BuildReverseIndex(var AIndex: TDcuIndex);
var
  Bucket    : IList<string>;
  Entry     : TDcuIndexEntry;
  Imported  : string;
  LowerKey  : string;
  Symbol    : string;

  procedure RecordImport(const AImported, AImporter: string);
  begin
    LowerKey := LowerCase(AImported);
    if not AIndex.ReverseImportIndex.TryGetValue(LowerKey, Bucket) then
    begin
      Bucket := Collections.NewList<string>;
      AIndex.ReverseImportIndex.Add(LowerKey, Bucket);
    end;
    Bucket.Add(AImporter);
  end;

  procedure RecordDefinition(const ASymbol, ADefiningUnit: string);
  begin
    LowerKey := LowerCase(ASymbol);
    if not AIndex.SymbolToDefiningUnit.TryGetValue(LowerKey, Bucket) then
    begin
      Bucket := Collections.NewList<string>;
      AIndex.SymbolToDefiningUnit.Add(LowerKey, Bucket);
    end;
    Bucket.Add(ADefiningUnit);
  end;

begin
  AIndex.ReverseImportIndex := Collections.NewKeyValue<string, IList<string>>;
  AIndex.SymbolToDefiningUnit := Collections.NewKeyValue<string, IList<string>>;
  for Entry in AIndex.Units do
  begin
    for Imported in Entry.InterfaceUses do
      RecordImport(Imported, Entry.UnitName);
    for Imported in Entry.ImplementationUses do
      RecordImport(Imported, Entry.UnitName);
    for Symbol in Entry.ExportedTypes do
      RecordDefinition(Symbol, Entry.UnitName);
    for Symbol in Entry.ExportedRoutines do
      RecordDefinition(Symbol, Entry.UnitName);
  end;
end;

class function TDcuIndexBuilder.Build(const ARootDirs: array of string;
  ARecursive, AParallel: Boolean; const APattern: string;
  AFailures: IList<TDcuIndexBuildFailure>): TDcuIndex;
var
  Errors : TArray<string>;
  Files  : TArray<string>;
  I      : Integer;
  Results: TArray<TDcuIndexEntry>;
  RootDir: string;
begin
  Result := Default(TDcuIndex);
  Result.Schema := DcuIndexSchemaVersion;
  Result.BuildTimestamp := Now;
  Result.RootDirs := Collections.NewList<string>;
  Result.Units := Collections.NewPlainList<TDcuIndexEntry>;
  for RootDir in ARootDirs do
    Result.RootDirs.Add(ExcludeTrailingPathDelimiter(RootDir));

  Files := CollectFiles(ARootDirs, ARecursive, APattern);
  if Length(Files) = 0 then
  begin
    BuildReverseIndex(Result);
    Exit;
  end;

  SetLength(Results, Length(Files));
  SetLength(Errors, Length(Files));
  // Warm up the mORMot Collections type registration on the main thread
  // before fanning out: the first NewPlainList<TDcuUsesEntry> /
  // <TDcuSourceRef> / <TDcuSymbolRef> per type involves lazy RTTI
  // registration that is not safe to race.
  try
    Results[0] := AnalyzeOne(Files[0], Errors[0]);
  except
    on E: Exception do
      Errors[0] := E.ClassName + ': ' + E.Message;
  end;
  if AParallel and (Length(Files) > 1) then
    TParallel.For(0, High(Files),
      procedure(AIdx: Integer)
      begin
        try
          Results[AIdx] := AnalyzeOne(Files[AIdx], Errors[AIdx]);
        except
          on E: Exception do
            Errors[AIdx] := E.ClassName + ': ' + E.Message;
        end;
      end)
  else
    for I := 0 to High(Files) do
      try
        Results[I] := AnalyzeOne(Files[I], Errors[I]);
      except
        on E: Exception do
          Errors[I] := E.ClassName + ': ' + E.Message;
      end;

  for I := 0 to High(Results) do
    if (Errors[I] = '') and (Results[I].UnitName <> '') then
      Result.Units.Add(Results[I])
    else if (Errors[I] <> '') and (AFailures <> nil) then
      AFailures.Add(TDcuIndexBuildFailure.Create(Files[I], Errors[I]));

  BuildReverseIndex(Result);
end;

{ TDcuIndexQuery }

constructor TDcuIndexQuery.Create(const AIndex: TDcuIndex);
begin
  inherited Create;
  FIndex := AIndex;
end;

function TDcuIndexQuery.FindByUnit(const AUnitName: string): IList<TDcuIndexEntry>;
var
  Entry: TDcuIndexEntry;
begin
  Result := Collections.NewPlainList<TDcuIndexEntry>;
  if FIndex.Units = nil then
    Exit;
  for Entry in FIndex.Units do
    if SameText(Entry.UnitName, AUnitName) then
      Result.Add(Entry);
end;

function TDcuIndexQuery.FindImportedBy(const AUnitName: string): IList<string>;
var
  Bucket: IList<string>;
  Item  : string;
begin
  Result := Collections.NewList<string>;
  if FIndex.ReverseImportIndex = nil then
    Exit;
  if FIndex.ReverseImportIndex.TryGetValue(LowerCase(AUnitName), Bucket) then
    for Item in Bucket do
      Result.Add(Item);
end;

function TDcuIndexQuery.FindImportsOf(const AUnitName: string): IList<string>;
var
  Entry: TDcuIndexEntry;
  Item : string;
begin
  Result := Collections.NewList<string>;
  if FIndex.Units = nil then
    Exit;
  for Entry in FIndex.Units do
    if SameText(Entry.UnitName, AUnitName) then
    begin
      for Item in Entry.InterfaceUses do
        Result.Add(Item);
      for Item in Entry.ImplementationUses do
        Result.Add(Item);
    end;
end;

function TDcuIndexQuery.FindDefinitionOf(const ASymbol: string): IList<string>;
var
  Bucket: IList<string>;
  Item  : string;
begin
  Result := Collections.NewList<string>;
  if FIndex.SymbolToDefiningUnit = nil then
    Exit;
  if FIndex.SymbolToDefiningUnit.TryGetValue(LowerCase(ASymbol), Bucket) then
    for Item in Bucket do
      Result.Add(Item);
end;

function TDcuIndexQuery.FindReferences(const ASymbol: string): IList<string>;
var
  Entry      : TDcuIndexEntry;
  Hit        : Boolean;
  Ref        : string;
begin
  Result := Collections.NewList<string>;
  if FIndex.Units = nil then
    Exit;
  for Entry in FIndex.Units do
  begin
    Hit := False;
    for Ref in Entry.TypeRefs do
      if SameText(Ref, ASymbol) then
      begin
        Hit := True;
        Break;
      end;
    if not Hit then
      for Ref in Entry.MethodRefs do
        if SameText(Ref, ASymbol) then
        begin
          Hit := True;
          Break;
        end;
    if Hit then
      Result.Add(Entry.UnitName);
  end;
end;

end.
