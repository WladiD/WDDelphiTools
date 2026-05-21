unit DebugTarget.EnumGamma;

// Third (last) sibling unit declaring the same enum type name. Its
// position at the END of DebugTarget.dpr's uses clause means an
// unqualified reference to "TStatus" in DebugTarget.dpr resolves
// to THIS unit's TStatus -- the "last wins" rule of Delphi's
// uses-order. Element prefix "sc" identifies its constants.

interface

type
  TStatus = (scInit, scWorking, scComplete);

implementation

end.
