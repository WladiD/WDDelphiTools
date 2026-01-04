unit DataUnit_4;

interface

type
  TRec_4_1 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_1): Boolean;
  end;

  TRec_4_2 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_2): Boolean;
  end;

  TRec_4_3 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_3): Boolean;
  end;

  TRec_4_4 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_4): Boolean;
  end;

  TRec_4_5 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_5): Boolean;
  end;

  TRec_4_6 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_6): Boolean;
  end;

  TRec_4_7 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_7): Boolean;
  end;

  TRec_4_8 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_8): Boolean;
  end;

  TRec_4_9 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_9): Boolean;
  end;

  TRec_4_10 = record
    Field1: Integer;
    Field2: string;
    Field3: TDateTime;
    Field4: Double;
    class operator Equal(const Left, Right: TRec_4_10): Boolean;
  end;

implementation

class operator TRec_4_1.Equal(const Left, Right: TRec_4_1): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_2.Equal(const Left, Right: TRec_4_2): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_3.Equal(const Left, Right: TRec_4_3): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_4.Equal(const Left, Right: TRec_4_4): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_5.Equal(const Left, Right: TRec_4_5): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_6.Equal(const Left, Right: TRec_4_6): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_7.Equal(const Left, Right: TRec_4_7): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_8.Equal(const Left, Right: TRec_4_8): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_9.Equal(const Left, Right: TRec_4_9): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;

class operator TRec_4_10.Equal(const Left, Right: TRec_4_10): Boolean;
begin
  Result := (Left.Field1 = Right.Field1) and
            (Left.Field2 = Right.Field2) and
            (Left.Field3 = Right.Field3) and
            (Left.Field4 = Right.Field4);
end;
end.
