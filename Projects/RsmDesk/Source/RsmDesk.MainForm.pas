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
  TRsmColl = (rcProcs, rcClasses, rcEnums, rcUnitUses);

  /// Role of a tree node. rnRoot/rnRange are lazy containers spanning
  /// the half-open index interval [Lo, Hi) of their collection;
  /// rnElement is a leaf for the single element at index Lo.
  TRsmNodeKind = (rnRoot, rnRange, rnElement);

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
    procedure BuildUI;
    procedure DoOpenClick(Sender: TObject);
    procedure DoTreeChange(Sender: TObject);
    procedure LoadRsm(const AExePath: String);
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
    function  ElementLabel(AColl: TRsmColl; AIndex: Integer): String;
    function  ElementDetail(AColl: TRsmColl; AIndex: Integer): String;
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
  if Value and (FNodeKind in [rnRoot, rnRange]) and not FPopulated then
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
    PopulateRoots;
    FPathLabel.Text := AExePath;
    FDetail.Text :=
      'Loaded.' + NL +
      Format('Procedures: %d'  + NL, [FReader.Procs.Count]) +
      Format('Classes/Records: %d' + NL, [FReader.Classes.Count]) +
      Format('Enum types: %d' + NL, [FReader.EnumDefs.Count]) +
      Format('Unit-use segments: %d' + NL + NL, [FReader.UnitUseSegments.Count]) +
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

function TFormMain.CollCount(AColl: TRsmColl): Integer;
begin
  case AColl of
    rcProcs   : Result := FReader.Procs.Count;
    rcClasses : Result := FReader.Classes.Count;
    rcEnums   : Result := FReader.EnumDefs.Count;
    rcUnitUses: Result := FReader.UnitUseSegments.Count;
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
var
  Coll: TRsmColl;
  Root: TRsmTreeItem;
begin
  FTree.BeginUpdate;
  try
    FTree.Clear;
    for Coll := Low(TRsmColl) to High(TRsmColl) do
    begin
      Root := NewNode(FTree, Coll, rnRoot, 0, CollCount(Coll), RootLabel(Coll));
      if CollCount(Coll) > 0 then
        AddPlaceholder(Root);
    end;
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
    rcProcs   : Result := Format('Procedures (%d)', [CollCount(AColl)]);
    rcClasses : Result := Format('Classes & Records (%d)', [CollCount(AColl)]);
    rcEnums   : Result := Format('Enum types (%d)', [CollCount(AColl)]);
    rcUnitUses: Result := Format('Unit-use segments (%d)', [CollCount(AColl)]);
  else
    Result := '?';
  end;
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
        S := FReader.UnitUseSegments[AIndex];
        Result := Format('%s (%d refs)', [S.UnitName, S.Refs.Count]);
      end;
  else
    Result := '?';
  end;
end;

function TFormMain.ElementDetail(AColl: TRsmColl; AIndex: Integer): String;
var
  P : TRsmProc;
  L : TRsmLocal;
  C : TRsmClassInfo;
  M : TRsmClassMember;
  Pr: TRsmClassProperty;
  E : TRsmEnumDef;
  El: TRsmEnumElement;
  S : TRsmUnitUseSegment;
  R : TRsmUnitUseRef;
  J, PropCount, ElemCount: Integer;
begin
  case AColl of
    rcProcs:
      begin
        P := FReader.Procs[AIndex];
        Result := Format('Name: %s' + NL +
                         'RVA (SegmentOffset): $%x' + NL +
                         'Size: %d bytes' + NL +
                         'Locals: %d',
                         [P.Name, P.SegmentOffset, P.Size, P.Locals.Count]);
        for J := 0 to P.Locals.Count - 1 do
        begin
          L := P.Locals[J];
          Result := Result + NL +
            Format('    %-24s BP%+d  TypeIdx=$%x  kind=%s  regIdx=%d',
              [L.Name, L.BpOffset, L.TypeIdx, LocalKindStr(L.Kind),
               L.RegParamIdx]);
        end;
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
            Format('    +%-5d %-24s TypeIdx=$%x  Prim=$%x  Size=%d  PtrTgt=$%x',
              [M.Offset, M.Name, M.TypeIdx, M.PrimitiveTypeId, M.Size,
               M.PointerTargetTypeIdx]);
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
        Result := Format('UnitName: %s' + NL +
                         'StartOffset: $%x' + NL +
                         'Refs: %d',
                         [S.UnitName, S.StartOffset, S.Refs.Count]);
        for J := 0 to S.Refs.Count - 1 do
        begin
          R := S.Refs[J];
          Result := Result + NL + Format('    %-7s %-28s RVA=$%x',
            [UnitUseKindStr(R.Kind), R.Name, R.Rva]);
        end;
      end;
  else
    Result := '';
  end;
end;

end.
