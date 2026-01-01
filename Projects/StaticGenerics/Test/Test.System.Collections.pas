unit Test.System.Collections;

interface

uses

  DUnitX.TestFramework,

  System.Collections.Factory,
  System.Collections.Interfaces;

type
  [TestFixture]
  TMyTestObject = class
  public
  end;

implementation

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);

end.
