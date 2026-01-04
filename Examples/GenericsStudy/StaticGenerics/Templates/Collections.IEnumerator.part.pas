{{={[ ]}=}}
  IEnumerator_{[type_flat]} = interface
    ['{[NewGuid]}']
    function GetCurrent: {[type]};
    function MoveNext: Boolean;
    property Current: {[type]} read GetCurrent;
  end;