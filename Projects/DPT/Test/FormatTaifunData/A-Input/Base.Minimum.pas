unit Base.Minimum;

interface

uses

  System.Classes,

  Base.Progress.Interfaces,

  Base.UI.Progress.Form;

type

  TFormAbort = class(TFormProgress)
   public
    constructor Create(AOwner: TComponent; AAbort: PBoolean);
  end;

implementation

constructor TFormAbort.Create(AOwner: TComponent; AAbort: PBoolean);
begin
  inherited Create(AOwner,AAbort,fpsSingleProgress,20);
  BarVisible[1]:=false;
end;

end.
