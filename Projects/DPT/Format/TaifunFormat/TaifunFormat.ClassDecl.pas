unit TaifunFormat.ClassDecl;

interface

uses DptFormatterAPI, TaifunFormat.Utils;

procedure FormatClassDeclaration(AClass: TClassDeclarationSyntax);

implementation

// Check whether a line (from trivia) has any non-whitespace content.
function LineHasContent(const ALine: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(ALine) do
    if (ALine[I] <> ' ') and (ALine[I] <> #9) and (ALine[I] <> #13) and (ALine[I] <> #10) then
    begin
      Result := True;
      Exit;
    end;
end;

// Adjust a token's leading trivia so it ends with the given indent.
// Rules:
//   1. First-line content (same line as previous token, before first newline)
//      is preserved in its original position — e.g. trailing comments.
//   2. Whitespace-only lines are collapsed (no blank lines between members).
//   3. Content-bearing lines (block comments, directives) are kept and appear
//      on their own lines, each prefixed with the given indent.
//   4. The final indent before the current token is exactly AIndent.
procedure SetLeadingIndent(AToken: TSyntaxToken; const AIndent: string);
var
  LTrivia: string;
  LIdx, LStart, P: Integer;
  LLine: string;
  LFirstLine: string;
  LFirstLineSeen: Boolean;
  LMiddlePart: string;
  LNewTrivia: string;
begin
  if not Assigned(AToken) then Exit;
  LTrivia := GetLeadingTrivia(AToken);

  LFirstLine := '';
  LFirstLineSeen := False;
  LMiddlePart := '';

  // Walk the trivia line by line.
  LIdx := 1;
  while LIdx <= Length(LTrivia) do
  begin
    P := LIdx;
    while (P <= Length(LTrivia)) and (LTrivia[P] <> #10) do Inc(P);

    if P <= Length(LTrivia) then
    begin
      // Line ends with \n (include it in LLine terminator but strip when processing)
      LLine := Copy(LTrivia, LIdx, P - LIdx);
      LStart := P + 1;
    end
    else
    begin
      // No trailing \n — everything remaining is the "final" line
      LLine := Copy(LTrivia, LIdx, Length(LTrivia) - LIdx + 1);
      LStart := Length(LTrivia) + 1;
    end;

    // Strip trailing \r
    while (Length(LLine) > 0) and (LLine[Length(LLine)] = #13) do
      Delete(LLine, Length(LLine), 1);

    // The "first line" is the content before the very first newline.
    // It is the trailing content of the previous token and must be kept.
    if not LFirstLineSeen then
    begin
      LFirstLine := LLine;
      LFirstLineSeen := True;

      // If the trivia has no newline at all, this is also the "final" (pre-indent) content.
      // In that case, we fall through without adding to middle lines and will just
      // append the content + newline + indent below.
      if LStart > Length(LTrivia) then
      begin
        // No newlines in trivia at all
        Break;
      end;
    end
    else if LineHasContent(LLine) then
      LMiddlePart := LMiddlePart + LLine + #13#10;
    // Whitespace-only middle lines are skipped (blank lines collapsed)

    LIdx := LStart;
  end;

  // Build the new trivia.
  LNewTrivia := '';
  // First-line content stays attached to previous token
  if LineHasContent(LFirstLine) then
    LNewTrivia := LFirstLine;
  // If there are middle lines OR the first line had content, ensure a newline
  if (LNewTrivia <> '') or (LMiddlePart <> '') or (LTrivia <> '') then
    LNewTrivia := LNewTrivia + #13#10;
  // Middle lines preserved with their original indentation (they may contain
  // directives or comments at different nesting levels).
  LNewTrivia := LNewTrivia + LMiddlePart + AIndent;

  ClearTrivia(AToken);
  AddLeadingTrivia(AToken, LNewTrivia);
end;

// Returns the group index (0-based) for member sorting:
//   0 = attribute (glued to next member, never sorts on its own)
//   1 = field
//   2 = all methods (class ctor/dtor, ctor/dtor, class proc/func, proc/func)
//   3 = property (incl. class property)
//   4 = other (unknown)
function GetMemberGroup(const AKind: string): Integer;
begin
  if AKind = 'attribute' then Exit(0);
  if AKind = 'field' then Exit(1);
  if (AKind = 'constructor') or (AKind = 'destructor') or
     (AKind = 'procedure') or (AKind = 'function') or
     (AKind = 'class constructor') or (AKind = 'class destructor') or
     (AKind = 'class procedure') or (AKind = 'class function') then Exit(2);
  if (AKind = 'property') or (AKind = 'class property') or (AKind = 'default') then Exit(3);
  Result := 4;
end;

// Returns the sort-order priority within group 2 (all methods):
//   0 = class constructor
//   1 = class destructor
//   2 = constructor
//   3 = destructor
//   4 = class procedure / class function (alphabetical)
//   5 = procedure / function (alphabetical)
function GetMethodSortPriority(const AKind: string): Integer;
begin
  if AKind = 'class constructor' then Exit(0);
  if AKind = 'class destructor' then Exit(1);
  if AKind = 'constructor' then Exit(2);
  if AKind = 'destructor' then Exit(3);
  if (AKind = 'class procedure') or (AKind = 'class function') then Exit(4);
  Result := 5;
end;

// Returns the sort-order priority within group 3 (properties):
//   0 = regular properties (not starting with On)
//   1 = event-handler properties (starting with On)
function GetPropertySortPriority(const AName: string): Integer;
begin
  if (Length(AName) >= 2) and (AName[1] = 'o') and (AName[2] = 'n') then
    Exit(1);
  Result := 0;
end;

// Returns the length of the member's keyword (for padding calculation).
function GetKeywordLen(const AKind: string): Integer;
begin
  if AKind = 'procedure' then Exit(9);
  if AKind = 'function' then Exit(8);
  if AKind = 'constructor' then Exit(11);
  if AKind = 'destructor' then Exit(10);
  if AKind = 'class constructor' then Exit(17);
  if AKind = 'class destructor' then Exit(16);
  if AKind = 'class procedure' then Exit(15);
  if AKind = 'class function' then Exit(14);
  if AKind = 'property' then Exit(8);
  if AKind = 'class property' then Exit(14);
  Result := 0;
end;

// Returns the alignment-group-id for a member (split from sort-group):
//   0 = field (no keyword alignment)
//   1 = class constructor/class destructor (aligned together)
//   2 = constructor/destructor (aligned together)
//   3 = procedure/function (and properties that align with them)
//   4 = class procedure/class function (and class properties that align with them)
//   5 = standalone property (section has no procedure/function)
//   6 = other
//   7 = standalone class property (section has no class procedure/function)
//
// Properties and class properties receive their group via
// GetEffectiveAlignmentGroup which knows whether the surrounding visibility
// section contains matching (class) procedures/functions.
function GetAlignmentGroup(const AKind: string): Integer;
begin
  if AKind = 'field' then Exit(0);
  if (AKind = 'class constructor') or (AKind = 'class destructor') then Exit(1);
  if (AKind = 'constructor') or (AKind = 'destructor') then Exit(2);
  if (AKind = 'procedure') or (AKind = 'function') then Exit(3);
  if (AKind = 'class procedure') or (AKind = 'class function') then Exit(4);
  if AKind = 'property' then Exit(5);
  if AKind = 'class property' then Exit(7);
  Result := 6;
end;

// Resolves the alignment group for a member considering section context.
// - 'property' joins the procedure/function alignment group (3) when the
//   section contains at least one procedure/function, otherwise stays
//   standalone (5) with 1-space padding.
// - 'class property' joins class procedure/function group (4) when the
//   section contains at least one of those, otherwise stays standalone (7).
function GetEffectiveAlignmentGroup(const AKind: string;
  AHasProcFunc, AHasClassProcFunc: Boolean): Integer;
begin
  if AKind = 'property' then
  begin
    if AHasProcFunc then Exit(3) else Exit(5);
  end;
  if AKind = 'class property' then
  begin
    if AHasClassProcFunc then Exit(4) else Exit(7);
  end;
  Result := GetAlignmentGroup(AKind);
end;

// Move trailing // comments from a member's next sibling's leading trivia
// to the current member's last token's trailing trivia.  This ensures that
// when members are sorted, the comment travels with the correct member.
procedure FixTrailingCommentsInSection(AClass: TClassDeclarationSyntax; ASectionIdx: Integer);
var
  LCount: Integer;
  LFirstTok: TSyntaxToken;
  LLastTok: TSyntaxToken;
  LTrivia: string;
  LComStart, LComEnd: Integer;
  LComment, LRest: string;
  LHasNL: Boolean;
  I, J: Integer;
begin
  LCount := GetClassMemberCount(AClass, ASectionIdx);
  if LCount < 2 then Exit;

  for I := 1 to LCount - 1 do
  begin
    LFirstTok := GetClassMemberFirstToken(AClass, ASectionIdx, I);
    LLastTok := GetClassMemberLastToken(AClass, ASectionIdx, I - 1);
    if not Assigned(LFirstTok) or not Assigned(LLastTok) then Continue;

    LTrivia := GetLeadingTrivia(LFirstTok);
    LComStart := Pos('//', LTrivia);
    if LComStart = 0 then Continue;

    LHasNL := False;
    for J := 1 to LComStart - 1 do
      if (LTrivia[J] = #13) or (LTrivia[J] = #10) then
      begin
        LHasNL := True;
        Break;
      end;
    if LHasNL then Continue;

    LComEnd := LComStart;
    while (LComEnd <= Length(LTrivia)) and (LTrivia[LComEnd] <> #13) and (LTrivia[LComEnd] <> #10) do
      Inc(LComEnd);

    LComment := Copy(LTrivia, 1, LComEnd - 1);
    LRest := Copy(LTrivia, LComEnd, Length(LTrivia) - LComEnd + 1);

    AddTrailingTrivia(LLastTok, LComment);
    ClearTrivia(LFirstTok);
    if LRest <> '' then
      AddLeadingTrivia(LFirstTok, LRest);
  end;

  // Also check the token after the last member (next visibility keyword or 'end')
  LLastTok := GetClassMemberLastToken(AClass, ASectionIdx, LCount - 1);
  if Assigned(LLastTok) then
  begin
    var LNextTok: TSyntaxToken := GetNextToken(LLastTok);
    if Assigned(LNextTok) then
    begin
      LTrivia := GetLeadingTrivia(LNextTok);
      LComStart := Pos('//', LTrivia);
      if LComStart > 0 then
      begin
        LHasNL := False;
        for J := 1 to LComStart - 1 do
          if (LTrivia[J] = #13) or (LTrivia[J] = #10) then
          begin
            LHasNL := True;
            Break;
          end;
        if not LHasNL then
        begin
          LComEnd := LComStart;
          while (LComEnd <= Length(LTrivia)) and (LTrivia[LComEnd] <> #13) and (LTrivia[LComEnd] <> #10) do
            Inc(LComEnd);
          LComment := Copy(LTrivia, 1, LComEnd - 1);
          LRest := Copy(LTrivia, LComEnd, Length(LTrivia) - LComEnd + 1);
          AddTrailingTrivia(LLastTok, LComment);
          ClearTrivia(LNextTok);
          if LRest <> '' then
            AddLeadingTrivia(LNextTok, LRest);
        end;
      end;
    end;
  end;
end;

procedure FormatClassDeclaration(AClass: TClassDeclarationSyntax);
var
  LSectionCount: Integer;
  LSectionIdx: Integer;
  LMemberCount: Integer;
  LI, LJ: Integer;
  LKinds: array of string;
  LNames: array of string;
  LSigs : array of string;
  LGroups: array of Integer;   // sort group (1..5)
  LAGroups: array of Integer;  // alignment group
  LAttachedTo: array of Integer; // -1, or index of member this attribute attaches to
  LIndices: array of Integer;
  LOrder: string;
  LToken: TSyntaxToken;
  LPadLen: Integer;
  LPadStr: string;
  LKind: string;
  LTempG, LTempIdx, LTempAG, LTempAttached: Integer;
  LTempName, LTempSig, LTempKind: string;
  LVisToken: TSyntaxToken;
begin
  if not Assigned(AClass) then Exit;

  LSectionCount := GetClassVisibilitySectionCount(AClass);
  if LSectionCount <= 0 then Exit;

  for LSectionIdx := 0 to LSectionCount - 1 do
  begin
    // Apply indentation to the visibility keyword.
    // Implicit first section (no keyword, e.g. `published` in form classes)
    // is still formatted — DFM streaming order is driven by the .dfm file,
    // not by field-declaration order, so sorting is safe.
    LVisToken := GetClassVisibilityKeyword(AClass, LSectionIdx);
    if Assigned(LVisToken) then
      SetLeadingIndent(LVisToken, '   ');

    // Move trailing // comments from next member's leading trivia to
    // previous member's last token, so they travel with the correct member.
    FixTrailingCommentsInSection(AClass, LSectionIdx);

    if not ClassSectionCanBeFormatted(AClass, LSectionIdx) then Continue;

    LMemberCount := GetClassMemberCount(AClass, LSectionIdx);
    if LMemberCount = 0 then Continue;

    // Collect member info
    LKinds.SetLength(LMemberCount);
    LNames.SetLength(LMemberCount);
    LSigs.SetLength(LMemberCount);
    LGroups.SetLength(LMemberCount);
    LAGroups.SetLength(LMemberCount);
    LAttachedTo.SetLength(LMemberCount);
    LIndices.SetLength(LMemberCount);

    var LHasOther: Boolean := False;
    var LHasProcFunc: Boolean := False;
    var LHasClassProcFunc: Boolean := False;
    for LI := 0 to LMemberCount - 1 do
    begin
      LKinds[LI] := GetClassMemberKind(AClass, LSectionIdx, LI);
      if LKinds[LI] = 'other' then LHasOther := True;
      if (LKinds[LI] = 'procedure') or (LKinds[LI] = 'function') then LHasProcFunc := True;
      if (LKinds[LI] = 'class procedure') or (LKinds[LI] = 'class function') then LHasClassProcFunc := True;
      LNames[LI] := LowerCase(GetClassMemberName(AClass, LSectionIdx, LI));
      LSigs[LI] := GetClassMemberSignature(AClass, LSectionIdx, LI);
      LGroups[LI] := GetMemberGroup(LKinds[LI]);
      LIndices[LI] := LI;
      LAttachedTo[LI] := -1;
    end;

    for LI := 0 to LMemberCount - 1 do
      LAGroups[LI] := GetEffectiveAlignmentGroup(LKinds[LI], LHasProcFunc, LHasClassProcFunc);

    // Safety: if section contains 'other' members (nested type/const inside
    // class, or special 'strict private type' visibility), preserve ALL
    // original member trivia.
    if LHasOther then Continue;

    // Resolve attachments:
    // - Attributes ([...]) attach FORWARD to the next real member
    // - 'default;' trailing modifier attaches BACKWARD to the previous property
    for LI := 0 to LMemberCount - 1 do
    begin
      if LKinds[LI] = 'attribute' then
      begin
        // Find the next non-attribute member
        for LJ := LI + 1 to LMemberCount - 1 do
          if (LKinds[LJ] <> 'attribute') and (LKinds[LJ] <> 'default') then
          begin
            LAttachedTo[LI] := LJ;
            LGroups[LI] := LGroups[LJ];
            LNames[LI] := LNames[LJ];
            LSigs[LI] := LSigs[LJ];
            Break;
          end;
      end
      else if LKinds[LI] = 'default' then
      begin
        // Find the previous property member
        for LJ := LI - 1 downto 0 do
          if LKinds[LJ] = 'property' then
          begin
            LAttachedTo[LI] := LJ;
            LGroups[LI] := LGroups[LJ];
            LNames[LI] := LNames[LJ];
            LSigs[LI] := LSigs[LJ];
            Break;
          end;
      end;
    end;

    // Insertion sort: primary = LGroups, secondary = method priority (for group 3)
    // tertiary = LNames, quaternary = LSigs.
    // Attributes inherit their target's keys AND are placed right BEFORE the target
    // (because original index ordering is stable within same keys).
    for LI := 1 to LMemberCount - 1 do
    begin
      LTempG := LGroups[LI];
      LTempIdx := LIndices[LI];
      LTempName := LNames[LI];
      LTempSig := LSigs[LI];
      LTempKind := LKinds[LI];
      LTempAG := LAGroups[LI];
      LTempAttached := LAttachedTo[LI];
      LJ := LI - 1;
      while LJ >= 0 do
      begin
        var LShouldMove: Boolean;
        LShouldMove := False;
        if LGroups[LJ] > LTempG then LShouldMove := True
        else if LGroups[LJ] = LTempG then
        begin
          // Within methods group: constructor first, destructor second
          if LTempG = 2 then
          begin
            var LPrioA: Integer := GetMethodSortPriority(LKinds[LJ]);
            var LPrioB: Integer := GetMethodSortPriority(LTempKind);
            // For attribute: inherit priority from its target's kind, not 'attribute'
            if LKinds[LJ] = 'attribute' then
              LPrioA := GetMethodSortPriority(LKinds[LAttachedTo[LJ]]);
            if LTempKind = 'attribute' then
              LPrioB := GetMethodSortPriority(LKinds[LTempAttached]);
            if LPrioA > LPrioB then LShouldMove := True
            else if LPrioA = LPrioB then
            begin
              if LNames[LJ] > LTempName then LShouldMove := True;
              // Overloads (same name): keep original relative order (stable sort)
            end;
          end
          else if LTempG = 3 then
          begin
            // Within property group: regular properties first, then On* events
            var LPropPrioA: Integer := GetPropertySortPriority(LNames[LJ]);
            var LPropPrioB: Integer := GetPropertySortPriority(LTempName);
            if LKinds[LJ] = 'attribute' then
              LPropPrioA := GetPropertySortPriority(LNames[LAttachedTo[LJ]]);
            if LTempKind = 'attribute' then
              LPropPrioB := GetPropertySortPriority(LNames[LTempAttached]);
            if LPropPrioA > LPropPrioB then LShouldMove := True
            else if LPropPrioA = LPropPrioB then
            begin
              if LNames[LJ] > LTempName then LShouldMove := True;
            end;
          end
          else
          begin
            if LNames[LJ] > LTempName then LShouldMove := True;
          end;
        end;
        if not LShouldMove then Break;

        LGroups[LJ + 1] := LGroups[LJ];
        LIndices[LJ + 1] := LIndices[LJ];
        LNames[LJ + 1] := LNames[LJ];
        LSigs[LJ + 1] := LSigs[LJ];
        LKinds[LJ + 1] := LKinds[LJ];
        LAGroups[LJ + 1] := LAGroups[LJ];
        LAttachedTo[LJ + 1] := LAttachedTo[LJ];
        Dec(LJ);
      end;
      LGroups[LJ + 1] := LTempG;
      LIndices[LJ + 1] := LTempIdx;
      LNames[LJ + 1] := LTempName;
      LSigs[LJ + 1] := LTempSig;
      LKinds[LJ + 1] := LTempKind;
      LAGroups[LJ + 1] := LTempAG;
      LAttachedTo[LJ + 1] := LTempAttached;
    end;

    // Build order string
    LOrder := '';
    for LI := 0 to LMemberCount - 1 do
    begin
      if LI > 0 then LOrder := LOrder + ',';
      LOrder := LOrder + IntToStr(LIndices[LI]);
    end;
    ReorderClassMembers(AClass, LSectionIdx, LOrder);

    // Now apply trivia to each (reordered) member.
    // Re-read kinds because reorder only moved nodes; LKinds/LAGroups still reflect sorted order.
    for LI := 0 to LMemberCount - 1 do
    begin
      LKind := GetClassMemberKind(AClass, LSectionIdx, LI);

      // 1. Indent: set leading trivia on first member token.
      // 'default;' trailing modifiers stay on the same line as the preceding property.
      LToken := GetClassMemberFirstToken(AClass, LSectionIdx, LI);
      if Assigned(LToken) then
      begin
        if LKind = 'default' then
        begin
          ClearTrivia(LToken);
          AddLeadingTrivia(LToken, ' ');
        end
        else
          SetLeadingIndent(LToken, '    ');
      end;

      // 2. Keyword alignment: compute padding and set trailing trivia on keyword token
      LToken := GetClassMemberKeywordToken(AClass, LSectionIdx, LI);
      if not Assigned(LToken) then Continue; // fields and attributes: no keyword alignment

      // Find longest keyword across all section members in the same alignment
      // group. Properties may join the proc/func group (see
      // GetEffectiveAlignmentGroup), so the run is not necessarily contiguous
      // — a full-section scan is required.
      var LMaxKwLen: Integer := 0;
      for LJ := 0 to LMemberCount - 1 do
        if LAGroups[LJ] = LAGroups[LI] then
        begin
          var LL: Integer := GetKeywordLen(LKinds[LJ]);
          if LL > LMaxKwLen then LMaxKwLen := LL;
        end;

      LPadLen := LMaxKwLen - GetKeywordLen(LKind) + 1;
      if LPadLen < 1 then LPadLen := 1;
      LPadStr := GetSep(' ', LPadLen);

      // Set padding as trailing trivia of the keyword token.
      // Default tokenizer typically stores the space as trailing of keyword
      // OR leading of the name — reset both so padding from keyword is authoritative.
      ClearTrailingTrivia(LToken);
      AddTrailingTrivia(LToken, LPadStr);

      // Clear name token's leading trivia so our padding controls the spacing
      var LNextTok: TSyntaxToken := GetNextToken(LToken);
      if Assigned(LNextTok) then
        ClearLeadingTrivia(LNextTok);
    end;
  end;
end;

end.
