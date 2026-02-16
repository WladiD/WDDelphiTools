// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

program DPT;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DPT.Application;

begin
  try
    TDptApplication.Run;
  except
    on E: Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      System.ExitCode := 1;
    end;
  end;
end.
