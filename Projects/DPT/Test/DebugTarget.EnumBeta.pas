unit DebugTarget.EnumBeta;

// Second of three sibling units declaring the same enum type name.
// Element prefix "sb" identifies this unit's constants. See
// DebugTarget.EnumAlpha for the broader test setup.

interface

type
  // Sparse declaration: explicit ordinal values with gaps. Used to
  // exercise the RSM scanner's $03 ENUM_DEF handling for the
  // non-contiguous case.
  TStatus = (sbIdle = 1, sbActive = 5, sbStopped = 10);

implementation

end.
