unit Test.DPT.Formatter.Taifun.MethodImpl;

interface

uses
  System.Classes,
  System.StrUtils,
  System.SysUtils,

  DUnitX.TestFramework,

  Test.DPT.Formatter.Taifun.Base;

type

  [TestFixture]
  TTestTaifunFormatter_MethodImpl = class(TTestTaifunFormatterBase)
  public
    [Test]
    procedure TestFormatMethodImplementation;
    [Test]
    procedure TestFormatMethodImplementation_WithXmlDoc;
    [Test]
    procedure TestFormatMethodImplementation_NestedClass;
    [Test]
    procedure TestFormatMethodImplementation_NamespacedReturnType;
    [Test]
    procedure TestFormatMethodImplementation_ClassConstructor;
    [Test]
    procedure TestFormatMethodImplementation_GenericClass;
    [Test]
    procedure TestFormatMethodImplementation_NestedProcedure;
    [Test]
    procedure TestFormatMethodImplementation_NestedProcedureInClassMethod;
    [Test]
    procedure TestFormatMethodImplementation_XmlDocNewline;
    [Test]
    procedure TestFormatMethodImplementation_LocalVarNotBannered;
    [Test]
    procedure TestFormatMethodImplementation_LocalRecordWithoutTrailingSemicolon;
    [Test]
    procedure TestFormatMethodImplementation_ConditionalAsmElseBody;
    [Test]
    procedure TestFormatMethodImplementation_GenericMethodWithVarParamAndLocalVar;
  end;

implementation

{ TTestTaifunFormatter_MethodImpl }

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation;
var
  LResult : String;
  LResult2: String;
  LSource : String;
begin
  LSource := 'unit MyUnit; interface implementation procedure TMyClass.MyMethod; begin end; procedure TMyClass.MyMethod2; begin end; procedure TOtherClass.MyMethod; begin end; end.';

  LResult := FormatSource(LSource);

  // Check for class banner of TMyClass
  Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + '{ TMyClass' + StringOfChar(' ', 63) + ' }' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'procedure TMyClass.MyMethod;'), 'TMyClass banner missing');

  // Check for method 2 banner of TMyClass (no class banner this time)
  Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + 'procedure TMyClass.MyMethod2;'), 'Method 2 banner missing');

  // Check for class banner of TOtherClass
  Assert.IsTrue(LResult.Contains(#13#10#13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + '{ TOtherClass' + StringOfChar(' ', 60) + ' }' + #13#10 + '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 + 'procedure TOtherClass.MyMethod;'), 'TOtherClass banner missing');

  // Idempotence check
  LResult2 := FormatSource(LResult);
  Assert.AreEqual(LResult, LResult2, 'Formatting the methods should be idempotent');
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_WithXmlDoc;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    /// <summary>My summary</summary>
    /// <param name="A">Param A</param>
    /// <returns>Result</returns>
    procedure TMyClass.MyMethod;
    begin
    end;
    /// <summary>Second summary</summary>
    procedure TMyClass.MyMethod2;
    begin
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains('/// <summary>My summary</summary>'), 'XML-DOC summary should be preserved');
  Assert.IsTrue(LResult.Contains('/// <param name="A">Param A</param>'), 'XML-DOC param should be preserved');
  Assert.IsTrue(LResult.Contains('/// <returns>Result</returns>'), 'XML-DOC returns should be preserved');
  Assert.IsTrue(LResult.Contains('/// <summary>Second summary</summary>'), 'Second XML-DOC summary should be preserved');

  Assert.IsTrue(LResult.Contains(
    '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
    '{ TMyClass' + StringOfChar(' ', 63) + ' }' + #13#10 +
    '{ ' + StringOfChar('=', 71) + ' }' + #13#10 + #13#10 +
    '/// <summary>My summary'),
    'Class banner should be placed before the first XML-DOC comment'
  );

  Assert.IsTrue(LResult.Contains(
    '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 +
    '/// <summary>Second summary'),
    'Method banner should be placed before the second XML-DOC comment'
  );
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_NestedClass;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ======================================================================= }
    { CAppConnectionProvider                                                  }
    { ======================================================================= }

    class function CAppConnectionProvider.TCacheKey.Create(ASrc, ADst: TLicenseModuleKind; ASrcMdt: Word): TCacheKey;
    begin
      Result.SrcModule:=ASrc;
      Result.DstModule:=ADst;
      Result.SrcMdt:=ASrcMdt;
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(
    '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
    '{ CAppConnectionProvider.TCacheKey                                        }' + #13#10 +
    '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
    #13#10 +
    'class function CAppConnectionProvider.TCacheKey.Create'),
    'Class banner should contain the full nested class name and MUST NOT contain fragments of old class banners' + #13#10 + 'Actual result:' + #13#10 + LResult
  );
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_NamespacedReturnType;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    function CCopyWrongMdtBlobsInPrg.GetName: S_255;
    begin
      Result:='xxx';
    end;

    function CCopyWrongMdtBlobsInPrg.GetSortDate: Base.Types.DateTime.TDate;
    begin
      Result:=Default(TDate);
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(
    '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
    '{ CCopyWrongMdtBlobsInPrg                                                 }' + #13#10 +
    '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
    #13#10 +
    'function CCopyWrongMdtBlobsInPrg.GetName: S_255;'),
    'First method should have a full class banner. Actual:' + #13#10 + LResult
  );

  Assert.IsTrue(LResult.Contains(
    '{ ' + StringOfChar('-', 71) + ' }' + #13#10 +
    #13#10 +
    'function CCopyWrongMdtBlobsInPrg.GetSortDate: Base.Types.DateTime.TDate;'),
    'Second method should have a short method banner despite namespaced return type. Actual:' + #13#10 + LResult
  );
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_ClassConstructor;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    class constructor CBootstrapping.ClassCreate;
    begin
    end;

    class destructor CBootstrapping.ClassDestroy;
    var
      LogPath: String;
    begin
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(
    '{ ======================================================================= }' + #13#10 +
    '{ CBootstrapping                                                          }' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'class constructor CBootstrapping.ClassCreate;'),
    'Class constructor should be formatted properly. Actual:' + #13#10 + LResult
  );

  Assert.IsTrue(LResult.Contains(
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    #13#10 +
    'class destructor CBootstrapping.ClassDestroy;'),
    'Class destructor should be formatted properly. Actual:' + #13#10 + LResult
  );
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_GenericClass;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    constructor CConcurrentDictionary<TKey,TValue>.Create;
    begin
    end;

    destructor CConcurrentDictionary<TKey,TValue>.Destroy;
    begin
    end;

    function CConcurrentDictionary<TKey,TValue>.Contains(const AKey: TKey): Boolean;
    begin
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  Assert.IsTrue(LResult.Contains(
    '{ ======================================================================= }' + #13#10 +
    '{ CConcurrentDictionary<TKey,TValue>                                      }' + #13#10 +
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'constructor CConcurrentDictionary<TKey,TValue>.Create;'),
    'First generic class method should get the full banner. Actual:' + #13#10 + LResult
  );

  Assert.IsTrue(LResult.Contains(
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    #13#10 +
    'destructor CConcurrentDictionary<TKey,TValue>.Destroy;'),
    'Second generic class method should get short banner. Actual:' + #13#10 + LResult
  );

  Assert.IsTrue(LResult.Contains(
    '{ ----------------------------------------------------------------------- }' + #13#10 +
    #13#10 +
    'function CConcurrentDictionary<TKey,TValue>.Contains(const AKey: TKey): Boolean;'),
    'Third generic class method should get short banner, not a new class banner. Actual:' + #13#10 + LResult
  );
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_NestedProcedure;
var
  LResult: string;
  LResult2: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    function Outer: Integer;
      procedure Inner;
      begin
      end;
    begin
      Inner;
      Result:=0;
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  // Outer should have double banner (transition or first method)
  Assert.IsTrue(LResult.Contains(
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'function Outer: Integer;'),
    'Outer function should have a long separator. Actual:' + #13#10 + LResult
  );

  // Inner should have short banner with empty line BEFORE and AFTER
  Assert.IsTrue(LResult.Contains(
    #13#10#13#10 + '{ -------------------------- }' + #13#10 +
    #13#10 +
    '  procedure Inner;'),
    'Inner procedure should have a short separator with empty line before and after. Actual:' + #13#10 + LResult);

  Assert.IsTrue(LResult.Contains(
    '  end;' + #13#10 + #13#10 +
    '{ -------------------------- }' + #13#10 + #13#10 +
    'begin'),
    'Nested procedure should have a trailing short separator before the outer method block begins.');

  // Idempotence check
  LResult2 := FormatSource(LResult);
  Assert.AreEqual(LResult, LResult2, 'Formatting nested procedure should be idempotent');
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_NestedProcedureInClassMethod;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    class procedure CBootstrapping.Finit;
    var
      ClassName: String;
    procedure LogExecution(AProc: TProc);
    begin
      AProc;
    end;
    begin
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  // Finit should have normal class banner (assuming it's the first or just a method)
  Assert.IsTrue(LResult.Contains('{ CBootstrapping'), 'Finit should have a class banner. Actual result around method:'#13#10 + Copy(LResult, Pos('class procedure CBootstrapping.Finit', LResult) - 200, 400));
  Assert.IsTrue(LResult.Contains('{ ======================================================================= }' + #13#10 + #13#10 + 'class procedure CBootstrapping.Finit;'), 'Finit should have TWO empty lines before it. Actual result around method:'#13#10 + Copy(LResult, Pos('class procedure CBootstrapping.Finit', LResult) - 200, 400));

  // LogExecution should have SHORT banner, NOT a long transition banner
  Assert.IsFalse(LResult.Contains(
    '{ ======================================================================= }' + #13#10 +
    #13#10 +
    'procedure LogExecution(AProc: TProc);'),
    'Nested procedure should NOT have a long transition banner. Actual:' + #13#10 + LResult);

  Assert.IsTrue(LResult.Contains(
    #13#10#13#10 + '{ -------------------------- }' + #13#10 +
    #13#10 +
    'procedure LogExecution(AProc: TProc);'),
    'Nested procedure should have a short banner. Actual:' + #13#10 + LResult);
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_XmlDocNewline;
var
  LResult: string;
  LSource: string;
begin
  LSource := '''
    unit MyUnit;
    interface
    implementation
    { ----------------------------------------------------------------------- }

    /// <summary> If the fields of this table have GUI (visual) controls assinged to them, reads the data from those GUI controls
    /// </summary>
    procedure CBaseTable.GetData;
    begin
    end;
    end.
    ''';

  LResult := FormatSource(LSource);

  // The XML-DOC summary should end with a newline before the procedure keyword starts
  var LExpectedPart :=
    '/// </summary>' + #13#10 +
    'procedure CBaseTable.GetData;';

  Assert.IsTrue(LResult.Contains(LExpectedPart), 'There should be a newline between the XML-DOC summary and the procedure declaration. Actual result:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_LocalVarNotBannered;
var
  LResult: String;
  LSource: String;
begin
  // Reproducer: TArray<Word>=[] causes the lexer to emit >= (tkGreaterOrEquals).
  // ParseClassMember only decremented LNestLevel for tkGreaterThan, so the generic
  // closing > was missed, nesting stayed elevated, and the parser consumed past
  // the class end into the implementation section.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TMyClass = class' + #13#10 +
    '    function Search(var AKey: Integer; AIds: TArray<Word>=[]): Boolean;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'procedure TMyClass.DoWork;' + #13#10 +
    'var' + #13#10 +
    '  X: Integer;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Ensure no banner is inserted between procedure header and local var
  Assert.IsTrue(LResult.Contains('procedure TMyClass.DoWork;' + #13#10 + 'var'),
    'No banner should be inserted between method header and local var section. Actual:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_LocalRecordWithoutTrailingSemicolon;
var
  LResult: String;
  LSource: String;
begin
  // Reproducer: 'packed record' was not recognized as a record body because 'packed'
  // was consumed as the TypeTypeToken (identifier). The record fields then corrupted
  // all subsequent parsing. Also tests that 'strict private type' inside a class is
  // not split by a banner.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TData = packed record' + #13#10 +
    '    ID: Integer;' + #13#10 +
    '    Name: String;' + #13#10 +
    '  end;' + #13#10 +
    '  TMyClass = class' + #13#10 +
    '   strict private type' + #13#10 +
    '    TInner = record' + #13#10 +
    '      Value: Integer;' + #13#10 +
    '    end;' + #13#10 +
    '   strict private' + #13#10 +
    '    FValue: Integer;' + #13#10 +
    '   public' + #13#10 +
    '    procedure DoWork;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'procedure TMyClass.DoWork;' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // 'strict private type' must stay together — no banner before 'type'
  Assert.IsTrue(LResult.Contains('strict private type'),
    'strict private type must remain on one line. Actual:'#13#10 + LResult);
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_ConditionalAsmElseBody;
var
  LResult: String;
  LSource: String;
begin
  // Reproducer (Base.Utils.pas MemEqualZero): A method whose body is split by
  // {$IFDEF}/{$ELSE}/{$ENDIF} — one branch is `asm ... end;`, the other is
  // `var ... begin ... end;`. The parser used to close the method at the first
  // inner `end;`, treating the `{$ELSE}` branch as a new top-level declaration.
  // That then caused the formatter to inject a spurious class banner before the
  // following real method.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  TByteCast = array[0..7] of Byte;' + #13#10 +
    '  CUtils = class' + #13#10 +
    '    class function  MemEqualZero(const AData; ASize: Integer): Boolean; static;' + #13#10 +
    '    class function  MemNew(const [ref] ATarget: Pointer; ASize: Integer): Boolean; static;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'class function CUtils.MemEqualZero(const AData; ASize: Integer): Boolean;' + #13#10 +
    '{$IFDEF CPUX86}' + #13#10 +
    'asm' + #13#10 +
    '  CLD' + #13#10 +
    '  PUSH   EDI' + #13#10 +
    '  POP    EDI' + #13#10 +
    'end;' + #13#10 +
    '{$ELSE !CPUX86}' + #13#10 +
    'var' + #13#10 +
    '  Data: TByteCast absolute AData;' + #13#10 +
    '  Loop: Integer;' + #13#10 +
    'begin' + #13#10 +
    '  Result:=true;' + #13#10 +
    '  for Loop:=0 to ASize-1 do' + #13#10 +
    '    if Data[Loop]<>0 then' + #13#10 +
    '    begin' + #13#10 +
    '      Result:=false;' + #13#10 +
    '      Break;' + #13#10 +
    '    end;' + #13#10 +
    'end;' + #13#10 +
    '{$ENDIF !CPUX86}' + #13#10 +
    'class function CUtils.MemNew(const [ref] ATarget: Pointer; ASize: Integer): Boolean;' + #13#10 +
    'begin' + #13#10 +
    '  Result := True;' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // The class banner for CUtils must appear exactly once — before MemEqualZero,
  // not again in the middle after the {$ENDIF}.
  var LClassBanner := '{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
                      '{ CUtils' + StringOfChar(' ', 65) + ' }' + #13#10 +
                      '{ ' + StringOfChar('=', 71) + ' }';
  var LFirst := Pos(LClassBanner, LResult);
  Assert.IsTrue(LFirst > 0, 'CUtils class banner missing. Actual:'#13#10 + LResult);
  var LSecond := PosEx(LClassBanner, LResult, LFirst + 1);
  Assert.AreEqual(0, LSecond, 'CUtils class banner must appear exactly once — the conditional method body was incorrectly split. Actual:'#13#10 + LResult);

  // The {$ELSE} and {$ENDIF} directives must be preserved in the output.
  Assert.IsTrue(LResult.Contains('{$IFDEF CPUX86}'), '{$IFDEF CPUX86} directive must be preserved');
  Assert.IsTrue(LResult.Contains('{$ELSE !CPUX86}'), '{$ELSE !CPUX86} directive must be preserved');
  Assert.IsTrue(LResult.Contains('{$ENDIF !CPUX86}'), '{$ENDIF !CPUX86} directive must be preserved');
end;

procedure TTestTaifunFormatter_MethodImpl.TestFormatMethodImplementation_GenericMethodWithVarParamAndLocalVar;
var
  LResult: String;
  LSource: String;
begin
  // Reproducer (from Base.Utils.Arrays.pas): A generic constraint `<T:class>`
  // on a class member made ParseClassMember treat the constraint keyword `class`
  // as the start of a nested type body (because LPrevKind was tkColon from the
  // constraint separator). LDeclBlockNest got stuck > 0, swallowing every
  // following class member into one. The subsequent method implementations then
  // fell out of sync with the interface — a later method with a local `var`
  // section was parsed with its body closed at the signature semicolon, and the
  // formatter inserted a spurious banner between signature and var.
  LSource :=
    'unit MyUnit;' + #13#10 +
    'interface' + #13#10 +
    'type' + #13#10 +
    '  THelper = class helper for TArray' + #13#10 +
    '    class procedure FreeAndNil<T:class>(var AValues: TArray<T>); static;' + #13#10 +
    '    class procedure AddRange<T>(var AValues: TArray<T>; const AValuesToInsert: array of T); static;' + #13#10 +
    '  end;' + #13#10 +
    '  TOther = record' + #13#10 +
    '    Value: Integer;' + #13#10 +
    '  end;' + #13#10 +
    'implementation' + #13#10 +
    'class procedure THelper.FreeAndNil<T:class>(var AValues: TArray<T>);' + #13#10 +
    'begin' + #13#10 +
    'end;' + #13#10 +
    'class procedure THelper.AddRange<T>(var AValues: TArray<T>; const AValuesToInsert: array of T);' + #13#10 +
    'var' + #13#10 +
    '  Index: Integer;' + #13#10 +
    'begin' + #13#10 +
    '  Index:=Length(AValues);' + #13#10 +
    'end;' + #13#10 +
    'end.';

  LResult := FormatSource(LSource);

  // Without the fix, the `class` token inside `<T:class>` made ParseClassMember
  // leak `LDeclBlockNest` past the class boundary. The whole class body was
  // consumed as one giant member, TOther was swallowed into it, and the
  // implementation section ended up one type deep. The formatter therefore
  // never visited the implementation section, so its banner is missing.
  Assert.IsTrue(
    LResult.Contains('{ ' + StringOfChar('=', 71) + ' }' + #13#10 +
                     'implementation' + #13#10 +
                     '{ ' + StringOfChar('=', 71) + ' }'),
    'Implementation section banner missing — the class body parse leaked past the class end. Actual:'#13#10 + LResult);

  // With a correctly-parsed class, AddRange's local `var` section lives inside
  // the method and must NOT receive a unit-level method banner.
  Assert.IsFalse(
    LResult.Contains('AddRange<T>(var AValues: TArray<T>; const AValuesToInsert: array of T);' + #13#10 + #13#10 +
                     '{ ' + StringOfChar('-', 71) + ' }' + #13#10 + #13#10 + 'var'),
    'Method banner must not be inserted between AddRange signature and local var section. Actual:'#13#10 + LResult);

  Assert.IsTrue(
    LResult.Contains('AddRange<T>(var AValues: TArray<T>; const AValuesToInsert: array of T);' + #13#10 + 'var'),
    'Local var section must directly follow AddRange signature. Actual:'#13#10 + LResult);
end;

end.
