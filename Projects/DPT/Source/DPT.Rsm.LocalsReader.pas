// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit DPT.Rsm.LocalsReader;

// Backward-compatibility alias. The reader's public facade lives in
// DPT.Rsm.Reader as TRsmReader; this unit keeps TRsmLocalsReader as
// a type alias so existing callers (DPT.Debugger, the MCP server,
// tests built before the refactor) compile unchanged. New code
// should reference DPT.Rsm.Reader.TRsmReader directly.

interface

uses
  DPT.Rsm.Reader;

type
  TRsmLocalsReader = DPT.Rsm.Reader.TRsmReader;

implementation

end.
