// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit TmplCodeGen.Logger;

interface

type
  ILogger = interface
    ['{B8A1E7F0-1234-4567-89AB-CDEF01234567}']
    procedure Log(const AMessage: String);
  end;

  TConsoleLogger = class(TInterfacedObject, ILogger)
  public
    procedure Log(const AMessage: String);
  end;

implementation

{ TConsoleLogger }

procedure TConsoleLogger.Log(const AMessage: String);
begin
  Writeln(AMessage);
end;

end.
