unit Test.DPT.LintFixtures;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  DUnitX.TestFramework,
  DPT.Lint.Fixtures,
  DPT.Lint.Context;

type
  [TestFixture]
  TTestDptLintFixtures = class
  public
    [Setup]
    procedure Setup;
    [Test]
    procedure DestructorsMustCallInherited_FailsOnNestedBlocks;
    [Test]
    procedure DestructorsMustCallInherited_SucceedsOnSimpleDestructor;
    [Test]
    procedure ValidateClassBanners_ReportsCorrectLine;
    [Test]
    procedure ValidateClassBanners_HandlesMisleadingSeparators;
    [Test]
    procedure ValidateClassBanners_DebugRegex;
    [Test]
    procedure ValidateClassBanners_SeparatorFollowedByMethod;
  end;

implementation

{ TTestDptLintFixtures }

procedure TTestDptLintFixtures.Setup;
begin
  TDptLintContext.Clear;
end;

procedure TTestDptLintFixtures.ValidateClassBanners_SeparatorFollowedByMethod;
var
  Fixture: TDptLintImplementationFixture;
  Context: TDptLintUnitContextFixture;
  Code: string;
begin
  Context := TDptLintUnitContextFixture.Create('Test.pas');
  try
    Context.DefineDoubleSeparatorLine('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ %-71s }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');

    Fixture := TDptLintImplementationFixture.Create;
    try
      Fixture.SetContext(Context);
      
      // Simulating Tfw.Ah.Dsp.Form.pas:
      // Line 340: Separator
      // Line 341: Directive
      // Line 342: Constructor (Should NOT be skipped!)
      Code := '''
        implementation
        { ======================================================================= }
        {$R *.dfm}
        { ======================================================================= }
        constructor TMyClass.Create;
        begin
        end;
        ''';
      
      Fixture.SetContent(Code);
      Fixture.SetLineOffset('338'); // implementation is at 339, separator at 340
      
      Fixture.ValidateClassBanners;
      
      // We expect a violation because the banner at line 340/342 is malformed (no class name).
      // Crucially, it must be reported at the constructor line.
      // Line counting (including lead empty line):
      // 1: (empty) -> 338
      // 2: implementation -> 339
      // 3: separator -> 340
      // 4: directive -> 341
      // 5: separator -> 342
      // 6: constructor -> 343
      Assert.AreEqual(1, TDptLintContext.Violations.Count, 'Should have detected the method after the separator');
      if TDptLintContext.Violations.Count > 0 then
      begin
        Assert.AreEqual(343, TDptLintContext.Violations[0].Line, 'Violation must be reported exactly at the method line');
      end;
    finally
      Fixture.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TTestDptLintFixtures.ValidateClassBanners_DebugRegex;
var
  Fixture: TDptLintImplementationFixture;
  Context: TDptLintUnitContextFixture;
  Code: string;
begin
  Context := TDptLintUnitContextFixture.Create('Test.pas');
  try
    Context.DefineDoubleSeparatorLine('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ %-71s }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');

    Fixture := TDptLintImplementationFixture.Create;
    try
      Fixture.SetContext(Context);
      
      // Use the EXACT line from the file (no leading spaces)
      Code := '''
        implementation
        { ======================================================================= }
        
        constructor TFormAhDsp.Create(AOwner: TComponent);
        begin
        end;
        ''';
      
      Fixture.SetContent(Code);
      Fixture.ValidateClassBanners;
      
      Assert.AreEqual(1, TDptLintContext.Violations.Count, 'Should have one violation for TFormAhDsp');
      if TDptLintContext.Violations.Count > 0 then
      begin
        // Line 4 (including leading empty line): constructor...
        Assert.AreEqual(4, TDptLintContext.Violations[0].Line, 'Should report at line 4');
      end;
    finally
      Fixture.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TTestDptLintFixtures.DestructorsMustCallInherited_FailsOnNestedBlocks;
var
  Fixture: TDptLintImplementationFixture;
  Code: string;
begin
  Fixture := TDptLintImplementationFixture.Create;
  try
    Code := '''
      destructor TMyClass.Destroy;
      begin
        if Assigned(FObj) then
        begin
          FObj.Free;
        end;
        inherited;
      end;
      ''';
    
    Fixture.SetContent(Code);
    Assert.IsTrue(Fixture.DestructorsMustCallInherited, 'Should find inherited even with nested blocks');
    Assert.AreEqual(0, TDptLintContext.Violations.Count, 'No violations expected');
  finally
    Fixture.Free;
  end;
end;

procedure TTestDptLintFixtures.DestructorsMustCallInherited_SucceedsOnSimpleDestructor;
var
  Fixture: TDptLintImplementationFixture;
  Code: string;
begin
  Fixture := TDptLintImplementationFixture.Create;
  try
    Code := '''
      destructor TMyClass.Destroy;
      begin
        inherited;
      end;
      ''';
    
    Fixture.SetContent(Code);
    Assert.IsTrue(Fixture.DestructorsMustCallInherited);
    Assert.AreEqual(0, TDptLintContext.Violations.Count);
  finally
    Fixture.Free;
  end;
end;

procedure TTestDptLintFixtures.ValidateClassBanners_ReportsCorrectLine;
var
  Fixture: TDptLintImplementationFixture;
  Context: TDptLintUnitContextFixture;
  Code: string;
begin
  Context := TDptLintUnitContextFixture.Create('Test.pas');
  try
    Context.DefineDoubleSeparatorLine('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ %-71s }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');

    Fixture := TDptLintImplementationFixture.Create;
    try
      Fixture.SetContext(Context);
      
      // Code with a class method that doesn't have its class banner above it.
      Code := '''
        implementation
        { ======================================================================= }
        { Malformed Banner - Missing Class Name }
        { ======================================================================= }

        procedure TMyClass.MyMethod;
        begin
        end;
        ''';
      
      Fixture.SetContent(Code);
      Fixture.SetLineOffset('100'); 
      
      Fixture.ValidateClassBanners;
      
      Assert.AreEqual(1, TDptLintContext.Violations.Count, 'Should have one violation');
      if TDptLintContext.Violations.Count > 0 then
      begin
        // 1: (empty)
        // 2: implementation
        // 3: { ===... }
        // 4: { Malformed Banner }
        // 5: { ===... }
        // 6: (blank)
        // 7: procedure TMyClass.MyMethod;
        // 100 + 6 = 106.
        Assert.AreEqual(106, TDptLintContext.Violations[0].Line, 'Violation should be reported at the method line');
      end;
    finally
      Fixture.Free;
    end;
  finally
    Context.Free;
  end;
end;

procedure TTestDptLintFixtures.ValidateClassBanners_HandlesMisleadingSeparators;
var
  Fixture: TDptLintImplementationFixture;
  Context: TDptLintUnitContextFixture;
  Code: string;
begin
  Context := TDptLintUnitContextFixture.Create('Test.pas');
  try
    Context.DefineDoubleSeparatorLine('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');
    Context.AddClassBannerLineFormat('{ %-71s }');
    Context.AddClassBannerLineFormat('{ ======================================================================= }');

    Fixture := TDptLintImplementationFixture.Create;
    try
      Fixture.SetContext(Context);
      
      // Code with a valid banner, followed by a method, then a MISLEADING separator line, then another method.
      Code := '''
        implementation
        { ======================================================================= }
        { TMyClass                                                                }
        { ======================================================================= }

        procedure TMyClass.FirstMethod;
        begin
        end;

        { ======================================================================= }
        { Missing name here! This SHOULD reset the current class banner.          }
        { ======================================================================= }

        procedure TMyClass.SecondMethod;
        begin
        end;
        ''';
      
      Fixture.SetContent(Code);
      Fixture.SetLineOffset('100'); 
      
      Fixture.ValidateClassBanners;
      
      // After fix, LCurrentClassBanner IS reset by the malformed banner block, 
      // so SecondMethod should report a violation.
      // BUT: The logic reports it at the FIRST method of the class (FirstMethod).
      Assert.AreEqual(1, TDptLintContext.Violations.Count, 'Should have one violation as the malformed banner reset the state');
      if TDptLintContext.Violations.Count > 0 then
      begin
        // Line 106 is FirstMethod
        Assert.AreEqual(106, TDptLintContext.Violations[0].Line, 'Violation should be reported at the FIRST method line of the class');
      end;
    finally
      Fixture.Free;
    end;
  finally
    Context.Free;
  end;
end;

end.
