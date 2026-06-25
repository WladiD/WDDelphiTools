unit DebugTarget.ComplexRec;

// Cross-unit complex-record fixture. Three records with 1 / 2 / 3 fields of
// mixed primitive types, an aggregate record (TComplexRec) that embeds all
// three plus its own trailing fields, and a typed pointer to the aggregate
// (PComplexRec). Declared in a SEPARATE unit so a LOCAL of TComplexRec --
// and a LOCAL PComplexRec pointing at it -- is a CROSS-UNIT type, exactly
// the §4.2/§4.15 unreliable-per-binary-alias regime that the §6.36
// RecordLocalNestedProbe already exercises for TXAdresse.
//
// Member names are deliberately unique across all records so the dotted-walk
// first-hop name fallback (FindRecordsByMemberName) and the §6.36 nested-record
// bridge (FindRecordBySizeAndMemberName) each resolve to a single record.
//
// Consumed by DebugTarget.dpr's ComplexRecLocalProbe and pinned by
// Test.DPT.MCP.Server.TestMcpEvaluateComplexRecordAndTypedPointer.

interface

type
  // One field.
  TCxRec1 = record
    C1Int: Integer;
  end;

  // Two fields, different types.
  TCxRec2 = record
    C2Int  : Integer;
    C2Word : Word;
  end;

  // Three fields, different types.
  TCxRec3 = record
    C3Int64 : Int64;
    C3Byte  : Byte;
    C3Bool  : Boolean;
  end;

  // Aggregate: the three records above plus its own trailing fields.
  TComplexRec = record
    CxR1   : TCxRec1;
    CxR2   : TCxRec2;
    CxR3   : TCxRec3;
    CxTag  : Integer;
    CxName : string[31];   // inline shortstring
  end;

  PComplexRec = ^TComplexRec;

implementation

end.
