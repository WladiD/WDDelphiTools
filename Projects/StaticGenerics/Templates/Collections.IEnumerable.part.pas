{{={[ ]}=}}
  IEnumerable_{[type_flat]} = interface
    ['{[NewGuid]}']
    function GetEnumerator: IEnumerator_{[type_flat]};
    function ToArray(AOffset: PtrInt = 0; ACount: PtrInt = 0): TArray<{[type]}>;
  end;