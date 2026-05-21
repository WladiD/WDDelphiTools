unit DebugTarget.EnumAlpha;

// First of three sibling units that declare an enum with the SAME
// type name (TStatus) but distinct element names. Each unit is
// pulled into DebugTarget.dpr via uses; consumers reference the
// type either fully qualified (DebugTarget.EnumAlpha.TStatus) or
// unqualified (TStatus) -- the unqualified form resolves to the
// LAST unit in the uses clause carrying TStatus, exercising the
// RSM reader's cross-unit name-collision handling.
//
// Element prefix "sa" identifies this unit's enum constants so a
// wrong-unit lookup surfaces with a recognisably wrong name.

interface

type
  TStatus = (saReady, saRunning, saDone);

implementation

end.
