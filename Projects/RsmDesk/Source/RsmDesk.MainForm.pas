// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit RsmDesk.MainForm;

// RsmDesk main window. Left: a TreeView listing every visualizable
// element the DPT.Rsm.* reader extracts from a processed .rsm file.
// Right: a detail viewer that renders the data of the selected node.
//
// The tree uses a VIRTUAL / lazy-loading pattern (FMX has no OnExpand
// event, so we subclass TTreeViewItem and override the virtual
// SetIsExpanded). Only the four category roots are created up front;
// each carries a placeholder child so its expander shows. The first
// time a node is expanded it materializes its children -- and large
// collections are split into nested range groups so every expand adds
// at most CHUNK children. Per-element detail text is built on demand
// when a node is selected, never pre-rendered for the whole file.
//
// The whole UI is built at runtime in BuildUI so the .fmx stays minimal
// (an empty form). The long-term build-out -- richer per-element
// viewers, jump-to-type navigation -- is the job of the
// "rsm-desk-expert" skill (see Projects/RsmDesk/RsmDesk.Coverage.md).

interface

uses
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.TreeView,
  FMX.Memo, FMX.ScrollBox,
  DPT.Rsm.Reader, DPT.Rsm.Model;

type
  /// Which reader collection a tree node draws from.
  /// Which data a tree node draws from. rcProcs/rcClasses/rcEnums index
  /// the matching reader collection directly. rcUnitUses indexes
  /// UnitUseSegments (the leaf level of the imports branch). rcImporters
  /// indexes FImporterGroups -- the synthetic "one entry per importing
  /// unit" grouping that drives the imports branch's top level.
  TRsmColl = (rcProcs, rcClasses, rcEnums, rcUnitUses, rcImporters);

  /// Role of a tree node. rnRoot/rnRange are lazy containers spanning
  /// the half-open index interval [Lo, Hi) of their collection;
  /// rnImporter is a lazy container for one importing unit (its segment
  /// block); rnElement is a leaf for the single element at index Lo.
  TRsmNodeKind = (rnRoot, rnRange, rnImporter, rnElement);

  /// A run of consecutive UnitUseSegments that share one $70 importer.
  /// Segments are contiguous per importer in the stream (each $70
  /// introduces a block), so an importer maps to one [Lo, Hi) range.
  TImporterGroup = record
    SrcIdx: Integer;   // index into Reader.SourceFiles, or -1
    Lo, Hi: Integer;   // [Lo, Hi) into Reader.UnitUseSegments
  end;

  TFormMain = class;

  /// A TreeViewItem that knows which slice of a reader collection it
  /// represents and materializes its children on first expand. The
  /// override of the virtual SetIsExpanded is the lazy-loading hook --
  /// FMX exposes no OnExpand event.
  TRsmTreeItem = class(TTreeViewItem)
  private
    FForm     : TFormMain;
    FColl     : TRsmColl;
    FNodeKind : TRsmNodeKind;
    FLo, FHi  : Integer;
    FPopulated: Boolean;
  protected
    procedure SetIsExpanded(const Value: Boolean); override;
  end;

  TFormMain = class(TForm)
  private
    FToolBar   : TToolBar;
    FOpenButton: TButton;
    FPathLabel : TLabel;
    FTree      : TTreeView;
    FSplitter  : TSplitter;
    FViewerHost: TLayout;
    FDetail    : TMemo;
    FReader    : TRsmReader;
    FOpenDialog: TOpenDialog;
    FImporterGroups: TArray<TImporterGroup>;
    procedure BuildUI;
    procedure DoOpenClick(Sender: TObject);
    procedure DoTreeChange(Sender: TObject);
    procedure LoadRsm(const AExePath: String);
    procedure BuildImporterGroups;
    procedure PopulateRoots;
    // Lazy materialization.
    function  CollCount(AColl: TRsmColl): Integer;
    function  NewNode(AOwner: TFmxObject; AColl: TRsmColl;
      AKind: TRsmNodeKind; ALo, AHi: Integer; const AText: String): TRsmTreeItem;
    procedure AddPlaceholder(AParent: TRsmTreeItem);
    procedure MaterializeRange(AColl: TRsmColl; ALo, AHi: Integer;
      AParent: TFmxObject);
    // Labels (cheap, built at materialization) and detail (built on select).
    function  RootLabel(AColl: TRsmColl): String;
    function  ImporterUnitName(AGroupIdx: Integer): String;
    function  ImporterLabel(AGroupIdx: Integer): String;
    function  ImporterDetail(AGroupIdx: Integer): String;
    function  ElementLabel(AColl: TRsmColl; AIndex: Integer): String;
    function  ElementDetail(AColl: TRsmColl; AIndex: Integer): String;
    // TypeIdx -> human-readable type name. Locals/globals are in the
    // 2-byte RSM registry id space (FindClassIdxByRsmTypeId); class/
    // record MEMBERS are in the file-offset space (FindStructByTypeIdx)
    // -- the two must NOT be mixed (confirmed via rsm-expert).
    function  PrimitiveName(AId: UInt16): String;
    function  ResolveStructType(ATypeIdx: UInt32): String;
    function  ResolveMemberType(const AMember: TRsmClassMember): String;
    // Self's type comes from the method's qualified name, NOT its
    // TypeIdx (which is a per-proc local-ref index, not a type id --
    // §6.27). Validated via FindClassByName so a non-method proc or a
    // generic edge case just yields '' (no annotation).
    function  ResolveSelfType(const AProcName: String): String;
  public
    procedure PopulateNode(ANode: TRsmTreeItem);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

const
  NL = sLineBreak;
  /// Maximum children created by a single expand. Larger collections
  /// nest range groups until every level fits inside this bound.
  CHUNK = 200;

function LocalKindStr(AKind: TRsmLocalKind): String;
begin
  case AKind of
    lkBpRel   : Result := 'bp-rel';
    lkRegister: Result := 'register';
  else
    Result := '?';
  end;
end;

/// One formatted locals row, kind-aware. Register params (Self, Sender,
/// ...) live in a CPU register, NOT at a BP offset -- show "reg#N", not
/// a misleading "BP+0". Compiler-generated unnamed slots arrive with
/// Name = '.' (the linker's placeholder for record-result / with /
/// temp slots, see DPT.Rsm.Format.md §4.4); relabel them as <temp> so
/// the row is self-explanatory instead of a bare dot.
function LocalLine(const L: TRsmLocal; const ATypeName: String): String;
var
  Nm, Loc: String;
begin
  if L.Kind = lkRegister then
    Loc := Format('reg#%d', [L.RegParamIdx])
  else
    Loc := Format('bp%+d', [L.BpOffset]);
  if L.Name = '.' then
    Nm := '<temp>'
  else
    Nm := L.Name;
  // A local/param TypeIdx is a per-proc local-ref index, NOT a global
  // type id (§6.27) -- it must NOT be resolved via the type registry
  // (that produced wrong names). So the trailing type column is filled
  // ONLY for Self (derived reliably from the proc name by the caller);
  // every other local shows just the raw id. Loc (reg#/bp) conveys kind.
  Result := Format('    %-26s %-10s TypeIdx=$%-5x %s',
    [Nm, Loc, L.TypeIdx, ATypeName]);
end;

/// The class/record part of a qualified method name: everything before
/// the last top-level '.' (dots inside generic '<...>' are ignored).
/// "TFormMain.Create" -> "TFormMain"; "Tfw.OpenFromUrl" -> "Tfw" (not a
/// class -> caller's FindClassByName rejects it); returns '' if none.
function ClassPartOfProcName(const AProcName: String): String;
var
  I, Depth, LastDot: Integer;
begin
  Depth := 0;
  LastDot := 0;
  for I := 1 to Length(AProcName) do
    case AProcName[I] of
      '<': Inc(Depth);
      '>': if Depth > 0 then Dec(Depth);
      '.': if Depth = 0 then LastDot := I;
    end;
  if LastDot > 1 then
    Result := Copy(AProcName, 1, LastDot - 1)
  else
    Result := '';
end;

function StructKindStr(AKind: TRsmStructKind): String;
begin
  case AKind of
    skClass : Result := 'class';
    skRecord: Result := 'record';
  else
    Result := '?';
  end;
end;

function UnitUseKindStr(AKind: TRsmUnitUseKind): String;
begin
  case AKind of
    uukType  : Result := 'type';
    uukSymbol: Result := 'symbol';
    uukFile  : Result := 'file';
  else
    Result := '?';
  end;
end;

{ TRsmTreeItem }

procedure TRsmTreeItem.SetIsExpanded(const Value: Boolean);
begin
  inherited;
  if Value and (FNodeKind in [rnRoot, rnRange, rnImporter]) and
     not FPopulated then
  begin
    // Set first: PopulateNode mutates children and must not re-enter.
    FPopulated := True;
    if FForm <> nil then
      FForm.PopulateNode(Self);
  end;
end;

{ TFormMain }

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FReader := TRsmReader.Create;
  BuildUI;
end;

destructor TFormMain.Destroy;
begin
  FReader.Free;
  inherited;
end;

procedure TFormMain.BuildUI;
begin
  FToolBar := TToolBar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.Align := TAlignLayout.Top;
  FToolBar.Height := 44;

  FOpenButton := TButton.Create(Self);
  FOpenButton.Parent := FToolBar;
  FOpenButton.Align := TAlignLayout.Left;
  FOpenButton.Width := 140;
  FOpenButton.Margins.Rect := RectF(6, 6, 6, 6);
  FOpenButton.Text := 'Open .exe / .rsm...';
  FOpenButton.OnClick := DoOpenClick;

  FPathLabel := TLabel.Create(Self);
  FPathLabel.Parent := FToolBar;
  FPathLabel.Align := TAlignLayout.Client;
  FPathLabel.Margins.Rect := RectF(8, 6, 8, 6);
  FPathLabel.Text := 'No file loaded.';

  FTree := TTreeView.Create(Self);
  FTree.Parent := Self;
  FTree.Align := TAlignLayout.Left;
  FTree.Width := 360;
  FTree.OnChange := DoTreeChange;

  FSplitter := TSplitter.Create(Self);
  FSplitter.Parent := Self;
  FSplitter.Align := TAlignLayout.Left;
  FSplitter.Width := 5;

  FViewerHost := TLayout.Create(Self);
  FViewerHost.Parent := Self;
  FViewerHost.Align := TAlignLayout.Client;

  FDetail := TMemo.Create(Self);
  FDetail.Parent := FViewerHost;
  FDetail.Align := TAlignLayout.Client;
  FDetail.ReadOnly := True;
  // Detail text is laid out in fixed-width columns (offsets, type ids,
  // names), so it needs a MONOSPACED font to line up -- a proportional
  // font makes the columns ragged. Drop Family/Size from StyledSettings
  // first, or the platform style's font wins over the one set here.
  FDetail.StyledSettings := FDetail.StyledSettings -
    [TStyledSetting.Family, TStyledSetting.Size];
  FDetail.Font.Family := 'Consolas';
  FDetail.Font.Size := 13;
  FDetail.Text :=
    'RsmDesk' + NL + NL +
    'Open a Delphi executable (its .rsm sidecar must sit next to it).' + NL +
    'The tree on the left will list every element the DPT.Rsm reader' + NL +
    'could extract; expand a branch to load its items on demand and' + NL +
    'select a node to inspect its data here.';

  FOpenDialog := TOpenDialog.Create(Self);
  FOpenDialog.Filter :=
    'Delphi executable (*.exe)|*.exe|RSM sidecar (*.rsm)|*.rsm|All files (*.*)|*.*';
end;

procedure TFormMain.DoOpenClick(Sender: TObject);
begin
  if FOpenDialog.Execute then
    LoadRsm(FOpenDialog.FileName);
end;

procedure TFormMain.DoTreeChange(Sender: TObject);
var
  Node: TRsmTreeItem;
begin
  if not (FTree.Selected is TRsmTreeItem) then Exit;
  Node := TRsmTreeItem(FTree.Selected);
  case Node.FNodeKind of
    rnElement:
      FDetail.Text := ElementDetail(Node.FColl, Node.FLo);
    rnImporter:
      FDetail.Text := ImporterDetail(Node.FLo);
    rnRoot:
      FDetail.Text := RootLabel(Node.FColl) + NL +
        'Expand to load items on demand.';
    rnRange:
      FDetail.Text := Format('Items %d .. %d (%d total in this group).',
        [Node.FLo, Node.FHi - 1, Node.FHi - Node.FLo]);
  end;
end;

procedure TFormMain.LoadRsm(const AExePath: String);
begin
  FPathLabel.Text := 'Loading ' + AExePath + ' ...';
  Application.ProcessMessages;
  try
    FReader.LoadFromFile(AExePath);
    BuildImporterGroups;
    PopulateRoots;
    FPathLabel.Text := AExePath;
    FDetail.Text :=
      'Loaded.' + NL +
      Format('Procedures: %d'  + NL, [FReader.Procs.Count]) +
      Format('Classes/Records: %d' + NL, [FReader.Classes.Count]) +
      Format('Enum types: %d' + NL, [FReader.EnumDefs.Count]) +
      Format('Imports: %d segments from %d importing unit(s)' + NL + NL,
        [FReader.UnitUseSegments.Count, Length(FImporterGroups)]) +
      'Expand a branch on the left to load its items.';
  except
    on E: Exception do
    begin
      FPathLabel.Text := 'Load failed.';
      FDetail.Text := Format('Failed to load %s'#13#10'%s: %s',
        [AExePath, E.ClassName, E.Message]);
    end;
  end;
end;

procedure TFormMain.BuildImporterGroups;
var
  I, J  : Integer;
  Cur   : Integer;
  Grp   : TImporterGroup;
begin
  // Segments are contiguous per importer ($70 introduces a block), so a
  // single forward pass that breaks on every SourceFileIdx change yields
  // one group per importing unit -- the top level of the imports branch.
  // Inline var keeps the mORMot IList type unnamed (no extra uses).
  FImporterGroups := nil;
  var Segs := FReader.UnitUseSegments;
  if Segs = nil then Exit;
  I := 0;
  while I < Segs.Count do
  begin
    Cur := Segs[I].SourceFileIdx;
    J := I + 1;
    while (J < Segs.Count) and (Segs[J].SourceFileIdx = Cur) do Inc(J);
    Grp.SrcIdx := Cur;
    Grp.Lo := I;
    Grp.Hi := J;
    FImporterGroups := FImporterGroups + [Grp];
    I := J;
  end;
end;

function TFormMain.CollCount(AColl: TRsmColl): Integer;
begin
  case AColl of
    rcProcs    : Result := FReader.Procs.Count;
    rcClasses  : Result := FReader.Classes.Count;
    rcEnums    : Result := FReader.EnumDefs.Count;
    rcUnitUses : Result := FReader.UnitUseSegments.Count;
    rcImporters: Result := Length(FImporterGroups);
  else
    Result := 0;
  end;
end;

function TFormMain.NewNode(AOwner: TFmxObject; AColl: TRsmColl;
  AKind: TRsmNodeKind; ALo, AHi: Integer; const AText: String): TRsmTreeItem;
begin
  Result := TRsmTreeItem.Create(AOwner);
  Result.FForm     := Self;
  Result.FColl     := AColl;
  Result.FNodeKind := AKind;
  Result.FLo       := ALo;
  Result.FHi       := AHi;
  Result.Text      := AText;
  Result.Parent    := AOwner;
end;

procedure TFormMain.AddPlaceholder(AParent: TRsmTreeItem);
var
  Dummy: TTreeViewItem;
begin
  // A single placeholder child so the expander arrow appears before the
  // real children exist. Removed on first expand by PopulateNode.
  Dummy := TTreeViewItem.Create(AParent);
  Dummy.Text := 'Loading...';
  Dummy.Parent := AParent;
end;

procedure TFormMain.PopulateRoots;

  procedure AddRoot(AColl: TRsmColl);
  var
    Root: TRsmTreeItem;
  begin
    Root := NewNode(FTree, AColl, rnRoot, 0, CollCount(AColl), RootLabel(AColl));
    if CollCount(AColl) > 0 then
      AddPlaceholder(Root);
  end;

begin
  FTree.BeginUpdate;
  try
    FTree.Clear;
    // Four visible roots. rcUnitUses is NOT a root -- the imports branch
    // is driven by rcImporters (grouped by importing unit); rcUnitUses
    // only supplies the segment leaves under each importer.
    AddRoot(rcProcs);
    AddRoot(rcClasses);
    AddRoot(rcEnums);
    AddRoot(rcImporters);
  finally
    FTree.EndUpdate;
  end;
end;

procedure TFormMain.PopulateNode(ANode: TRsmTreeItem);
var
  I: Integer;
begin
  FTree.BeginUpdate;
  try
    // Drop the placeholder (and any prior children, defensively).
    for I := ANode.Count - 1 downto 0 do
      ANode.Items[I].Free;
    if ANode.FNodeKind = rnImporter then
      // An importer node lists its block's segments (a contiguous range
      // of UnitUseSegments).
      MaterializeRange(rcUnitUses, FImporterGroups[ANode.FLo].Lo,
        FImporterGroups[ANode.FLo].Hi, ANode)
    else
      MaterializeRange(ANode.FColl, ANode.FLo, ANode.FHi, ANode);
  finally
    FTree.EndUpdate;
  end;
end;

procedure TFormMain.MaterializeRange(AColl: TRsmColl; ALo, AHi: Integer;
  AParent: TFmxObject);
var
  Span, GroupSize, I, GHi: Integer;
  Group: TRsmTreeItem;
begin
  Span := AHi - ALo;
  if Span <= 0 then Exit;

  if Span <= CHUNK then
  begin
    if AColl = rcImporters then
      // Importer level: each entry is itself an expandable container for
      // one importing unit's segment block, so give it a placeholder.
      for I := ALo to AHi - 1 do
        AddPlaceholder(NewNode(AParent, AColl, rnImporter, I, I + 1,
          ImporterLabel(I)))
    else
      // Leaf level: one element node per item.
      for I := ALo to AHi - 1 do
        NewNode(AParent, AColl, rnElement, I, I + 1, ElementLabel(AColl, I));
    Exit;
  end;

  // Too many to show at once: split into nested range groups so this
  // expand creates at most CHUNK children, each itself lazy.
  GroupSize := CHUNK;
  while (Span + GroupSize - 1) div GroupSize > CHUNK do
    GroupSize := GroupSize * CHUNK;

  I := ALo;
  while I < AHi do
  begin
    GHi := Min(I + GroupSize, AHi);
    Group := NewNode(AParent, AColl, rnRange, I, GHi,
      Format('[ %d .. %d ]  (%d)', [I, GHi - 1, GHi - I]));
    AddPlaceholder(Group);
    I := GHi;
  end;
end;

function TFormMain.RootLabel(AColl: TRsmColl): String;
begin
  case AColl of
    rcProcs    : Result := Format('Procedures (%d)', [CollCount(AColl)]);
    rcClasses  : Result := Format('Classes & Records (%d)', [CollCount(AColl)]);
    rcEnums    : Result := Format('Enum types (%d)', [CollCount(AColl)]);
    rcImporters: Result := Format('Imports by unit (%d)', [CollCount(AColl)]);
    rcUnitUses : Result := Format('Unit-use segments (%d)', [CollCount(AColl)]);
  else
    Result := '?';
  end;
end;

function TFormMain.ImporterUnitName(AGroupIdx: Integer): String;
var
  Idx: Integer;
begin
  Idx := FImporterGroups[AGroupIdx].SrcIdx;
  if (Idx >= 0) and (FReader.SourceFiles <> nil) and
     (Idx < FReader.SourceFiles.Count) then
    Result := FReader.SourceFiles[Idx].UnitName
  else
    Result := '(no $70 introducer)';
end;

function TFormMain.ImporterLabel(AGroupIdx: Integer): String;
var
  Grp: TImporterGroup;
begin
  Grp := FImporterGroups[AGroupIdx];
  Result := Format('%s  (%d)', [ImporterUnitName(AGroupIdx), Grp.Hi - Grp.Lo]);
end;

function TFormMain.ImporterDetail(AGroupIdx: Integer): String;
var
  Grp: TImporterGroup;
  S  : TRsmUnitUseSegment;
  J  : Integer;
begin
  Grp := FImporterGroups[AGroupIdx];
  Result := Format('Importing unit: %s' + NL, [ImporterUnitName(AGroupIdx)]);
  if (Grp.SrcIdx >= 0) and (FReader.SourceFiles <> nil) and
     (Grp.SrcIdx < FReader.SourceFiles.Count) then
    Result := Result + Format('Source file: %s' + NL,
      [FReader.SourceFiles[Grp.SrcIdx].SourceFile]);
  Result := Result + Format('Imports from %d unit(s):', [Grp.Hi - Grp.Lo]);
  for J := Grp.Lo to Grp.Hi - 1 do
  begin
    S := FReader.UnitUseSegments[J];
    Result := Result + NL + Format('    %-32s %d used', [S.UnitName, S.Refs.Count]);
  end;
end;

function TFormMain.PrimitiveName(AId: UInt16): String;
begin
  // Small, codebase-documented subset of the compiler built-in type ids
  // (DPT.Rsm.Model.pas member doc + DPT.Debugger.pas formatter table).
  // Conservative on purpose: an unknown id returns '' (no label) rather
  // than a wrong guess. Extend as ids are confirmed.
  case AId of
    $03FD: Result := 'Integer';
    $0415: Result := 'Word';
    $041D: Result := 'Double';
    $0401: Result := 'string';
    $0425: Result := 'Boolean';
  else
    Result := '';
  end;
end;

function TFormMain.ResolveStructType(ATypeIdx: UInt32): String;
var
  Idx: Integer;
begin
  // File-offset token space (class/record members). $0 is the
  // "no token assigned" sentinel, NOT a real id -- many structs carry
  // it, so FindStructByTypeIdx(0) would return whichever zero-TypeIdx
  // class comes first (e.g. TCacheDescriptor) and mislabel every
  // untyped member (procedure-pointer fields like TVariantManager's).
  // Treat it as unresolved.
  if ATypeIdx = 0 then Exit('');
  Idx := FReader.FindStructByTypeIdx(ATypeIdx);
  if Idx >= 0 then
    Result := Format('%s (%s)',
      [FReader.Classes[Idx].Name, StructKindStr(FReader.Classes[Idx].Kind)])
  else
    Result := '';
end;

function TFormMain.ResolveMemberType(const AMember: TRsmClassMember): String;
begin
  if AMember.PrimitiveTypeId <> 0 then
  begin
    Result := PrimitiveName(AMember.PrimitiveTypeId);
    if Result = '' then Result := Format('prim $%x', [AMember.PrimitiveTypeId]);
    Exit;
  end;
  if AMember.PointerTargetTypeIdx <> 0 then
  begin
    Result := ResolveStructType(AMember.PointerTargetTypeIdx);
    if Result <> '' then Result := '^' + Result;
    Exit;
  end;
  Result := ResolveStructType(AMember.TypeIdx);
end;

function TFormMain.ResolveSelfType(const AProcName: String): String;
var
  Cls: String;
  Idx: Integer;
begin
  Result := '';
  Cls := ClassPartOfProcName(AProcName);
  if Cls = '' then Exit;
  Idx := FReader.FindClassByName(Cls);
  if Idx >= 0 then
    Result := Format('%s (%s)',
      [FReader.Classes[Idx].Name, StructKindStr(FReader.Classes[Idx].Kind)]);
end;

function TFormMain.ElementLabel(AColl: TRsmColl; AIndex: Integer): String;
var
  C: TRsmClassInfo;
  E: TRsmEnumDef;
  S: TRsmUnitUseSegment;
begin
  case AColl of
    rcProcs:
      Result := FReader.Procs[AIndex].Name;
    rcClasses:
      begin
        C := FReader.Classes[AIndex];
        Result := Format('%s : %s', [C.Name, StructKindStr(C.Kind)]);
      end;
    rcEnums:
      begin
        E := FReader.EnumDefs[AIndex];
        Result := Format('%s (%s)', [E.TypeName, E.UnitName]);
      end;
    rcUnitUses:
      begin
        // Under an importer node this reads as "imports <N> symbols
        // from <declaring unit>".
        S := FReader.UnitUseSegments[AIndex];
        Result := Format('%s  (%d used)', [S.UnitName, S.Refs.Count]);
      end;
  else
    Result := '?';
  end;
end;

function TFormMain.ElementDetail(AColl: TRsmColl; AIndex: Integer): String;
var
  P : TRsmProc;
  C : TRsmClassInfo;
  M : TRsmClassMember;
  Pr: TRsmClassProperty;
  E : TRsmEnumDef;
  El: TRsmEnumElement;
  S : TRsmUnitUseSegment;
  R : TRsmUnitUseRef;
  J, PropCount, ElemCount: Integer;
  Importer: String;
  SelfType: String;
  DeclUnit: String;
begin
  case AColl of
    rcProcs:
      begin
        P := FReader.Procs[AIndex];
        // §4.18 proc -> declaring unit via the $70 source-file introducer
        // the scanner stamped onto SourceFileIdx. Empty for import thunks
        // (no Delphi declaring unit) -- label that honestly rather than blank.
        DeclUnit := FReader.DeclaringUnitOfProc(AIndex);
        if DeclUnit = '' then DeclUnit := '(none -- import thunk / no source)';
        Result := Format('Unit: %s' + NL +
                         'Name: %s' + NL +
                         'RVA (SegmentOffset): $%x' + NL +
                         'Size: %d bytes' + NL +
                         'Locals: %d',
                         [DeclUnit, P.Name, P.SegmentOffset, P.Size, P.Locals.Count]);
        SelfType := ResolveSelfType(P.Name);
        for J := 0 to P.Locals.Count - 1 do
          if P.Locals[J].Name = 'Self' then
            Result := Result + NL + LocalLine(P.Locals[J], SelfType)
          else
            Result := Result + NL + LocalLine(P.Locals[J], '');
      end;

    rcClasses:
      begin
        C := FReader.Classes[AIndex];
        if C.Properties <> nil then PropCount := C.Properties.Count
        else PropCount := 0;
        Result := Format('Name: %s' + NL +
                         'Kind: %s' + NL +
                         'TypeIdx: $%x' + NL +
                         'Parent: %s' + NL +
                         'ParentRawId: $%x' + NL +
                         'Members: %d   Properties: %d',
                         [C.Name, StructKindStr(C.Kind), C.TypeIdx,
                          C.ParentName, C.ParentRawId, C.Members.Count,
                          PropCount]);
        if C.Members.Count > 0 then
          Result := Result + NL + NL + 'Members:';
        for J := 0 to C.Members.Count - 1 do
        begin
          M := C.Members[J];
          Result := Result + NL +
            Format('    +%-5d %-24s TypeIdx=$%x  Prim=$%x  Size=%d  PtrTgt=$%x  %s',
              [M.Offset, M.Name, M.TypeIdx, M.PrimitiveTypeId, M.Size,
               M.PointerTargetTypeIdx, ResolveMemberType(M)]);
        end;
        if PropCount > 0 then
        begin
          Result := Result + NL + NL + 'Properties:';
          for J := 0 to C.Properties.Count - 1 do
          begin
            Pr := C.Properties[J];
            Result := Result + NL +
              Format('    %-24s -> %-20s  Prim=$%x  TargetId=$%x',
                [Pr.Name, Pr.UnderlyingField, Pr.PrimitiveTypeId, Pr.TargetId]);
          end;
        end;
      end;

    rcEnums:
      begin
        E := FReader.EnumDefs[AIndex];
        if E.Elements <> nil then ElemCount := E.Elements.Count
        else ElemCount := 0;
        Result := Format('TypeName: %s' + NL +
                         'UnitName: %s' + NL +
                         'Elements: %d',
                         [E.TypeName, E.UnitName, ElemCount]);
        if E.Elements <> nil then
          for J := 0 to E.Elements.Count - 1 do
          begin
            El := E.Elements[J];
            Result := Result + NL + Format('    ord %-6d %s',
              [El.Ordinal, El.Name]);
          end;
      end;

    rcUnitUses:
      begin
        S := FReader.UnitUseSegments[AIndex];
        if (S.SourceFileIdx >= 0) and (FReader.SourceFiles <> nil) and
           (S.SourceFileIdx < FReader.SourceFiles.Count) then
          Importer := FReader.SourceFiles[S.SourceFileIdx].UnitName
        else
          Importer := '(no $70 introducer)';
        Result := Format('Importing unit:  %s' + NL +
                         'Declaring unit:  %s' + NL +
                         'StartOffset: $%x' + NL +
                         'Used symbols: %d',
                         [Importer, S.UnitName, S.StartOffset, S.Refs.Count]);
        for J := 0 to S.Refs.Count - 1 do
        begin
          R := S.Refs[J];
          Result := Result + NL + Format('    %-7s %-28s Token=$%x',
            [UnitUseKindStr(R.Kind), R.Name, R.LinkToken]);
        end;
      end;
  else
    Result := '';
  end;
end;

end.
