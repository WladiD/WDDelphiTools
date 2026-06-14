unit DebugTarget.RecTypes;

// §6.36 fixture: record types declared in a SEPARATE unit, so a LOCAL of
// TXAdresse is a CROSS-UNIT record. Cross-unit types get the §4.2 / §4.15
// unreliable per-binary alias id, so neither the local's per-proc TypeIdx
// nor the nested Anschrift member's TypeIdx need resolve to a record in the
// registry -- reproducing Test.Lib's `Ad: TAdresse` dotted-walk failure
// (`Ad.Anschrift.Str`) in the controlled DebugTarget fixture WITHOUT any
// dependency on Test.Lib. Mirrors Base.Types.Business's TAdresse (a leading
// shortstring + a nested record of shortstrings). An inline (same-unit)
// version resolves fine (see the earlier TAdrLike probe), so the cross-unit
// boundary is the trigger.

interface

type
  TXAnschrift = record
    Str : string[60];
    Ort : string[50];
  end;

  TXAdresse = record
    Name      : string[40];
    Anschrift : TXAnschrift;
  end;

implementation

end.
