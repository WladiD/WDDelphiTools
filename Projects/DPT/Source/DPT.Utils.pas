// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Utils;

interface

uses
  System.SysUtils;

procedure CheckAndExecutePreProcessor(var AProjectFile: String);

implementation

uses
  DPT.Preprocessor;

procedure CheckAndExecutePreProcessor(var AProjectFile: String);
var
  PreProcessor: TDptPreprocessor;
begin
  if not SameText(ExtractFileExt(AProjectFile), '.dproj') then
  begin
    PreProcessor := TDptPreprocessor.Create;
    try
      AProjectFile := PreProcessor.Execute(AProjectFile);
    finally
      PreProcessor.Free;
    end;
  end;
end;

end.
