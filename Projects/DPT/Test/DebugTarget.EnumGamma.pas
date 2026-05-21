unit DebugTarget.EnumGamma;

// Third (last) sibling unit declaring the same enum type name. Its
// position at the END of DebugTarget.dpr's uses clause means an
// unqualified reference to "TStatus" in DebugTarget.dpr resolves
// to THIS unit's TStatus -- the "last wins" rule of Delphi's
// uses-order. Element prefix "sc" identifies its constants.

interface

type
  // Sparse declaration with WIDER gaps than EnumBeta to keep the
  // two sparse fixtures distinguishable in the byte stream when
  // reverse-engineering the $03 ENUM_DEF emission for non-
  // contiguous enums.
  TStatus = (scInit = 7, scWorking = 13, scComplete = 100);

implementation

end.
