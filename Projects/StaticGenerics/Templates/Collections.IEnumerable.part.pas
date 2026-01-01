{{={[ ]}=}}
  IEnumerable_{[type_flat]} = interface
    ['{[NewGuid]}']
    function GetEnumerator: IEnumerator_{[type_flat]};
    function ToArray(AOffset: Integer = 0; ACount: Integer = 0): TArray<{[type]}>;
  end;