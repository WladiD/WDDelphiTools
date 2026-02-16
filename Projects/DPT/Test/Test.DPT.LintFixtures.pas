unit Test.DPT.LintFixtures;

interface

uses
  System.Classes,
  System.SysUtils,
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
  end;

implementation

{ TTestDptLintFixtures }

procedure TTestDptLintFixtures.Setup;
begin
  TDptLintContext.Clear;
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

end.
