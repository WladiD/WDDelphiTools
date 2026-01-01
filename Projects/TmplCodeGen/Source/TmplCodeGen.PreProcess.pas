unit TmplCodeGen.PreProcess;

interface

uses

  System.StrUtils,
  System.SysUtils,

  mormot.core.base,
  mormot.core.rtti,
  mormot.core.variants;

{ ----------------------------------------------------------------------- }

type

  TPreProcessConfigForCollectionsList = class
   strict private
    FConfJsonIn: TDocVariantData;
    FConfJsonOut: TDocVariantData;
    procedure EnableKnownFlagsForObjectTypes(ATypeObj: PDocVariantData);
    function  IsTypeKnownObjectType(const ATypeName: String): Boolean;
   public
    constructor Create(const AConfJsonIn: TDocVariantData);
    function Execute: Boolean;
    property ConfJsonIn: TDocVariantData read FConfJsonIn;
    property ConfJsonOut: TDocVariantData read FConfJsonOut;
  end;

{ ======================================================================= }
implementation
{ ======================================================================= }

{ TPreProcessConfigForCollectionsList }

constructor TPreProcessConfigForCollectionsList.Create(const AConfJsonIn: TDocVariantData);
begin
  FConfJsonIn:=AConfJsonIn;
end;

{ ----------------------------------------------------------------------- }

procedure TPreProcessConfigForCollectionsList.EnableKnownFlagsForObjectTypes(ATypeObj: PDocVariantData);
begin
  if not ATypeObj.Exists('IList')
    then ATypeObj.AddObject([],'IList');
  var ListFlagsObj: PDocVariantData:=_Safe(ATypeObj.Value['IList']);

  ListFlagsObj.AddOrUpdateValue('enable_all',true);
  ListFlagsObj.AddOrUpdateValue('enable_concat',true);
  ListFlagsObj.AddOrUpdateValue('enable_distinct',false);
  ListFlagsObj.AddOrUpdateValue('enable_remove_all',true);
  ListFlagsObj.AddOrUpdateValue('enable_where',true);
end;

{ ----------------------------------------------------------------------- }

/// <summary>Verarbeitet die Konfiguration und liefert True, wenn er irgendetwas daran verändert hat</summary>
function TPreProcessConfigForCollectionsList.Execute: Boolean;
begin
  Result:=
    FConfJsonIn.Exists('Template') and
    SameText(FConfJsonIn.S['Template'],'Collections.List.TMPL.pas') and
    FConfJsonIn.Exists('types');
  if not Result
    then Exit;

  var Target: TDocVariantData:=Default(TDocVariantData);
  Target.InitFrom(FConfJsonIn,true,true);

  for var LoopObj: PDocVariantData in _Safe(Target.Value['types']).Objects do
  begin
    if not LoopObj.Exists('type')
      then Continue;

    var TypeIsObjectExists: Boolean:=LoopObj.Exists('type_is_object');
    var TypeName: String:=LoopObj.S['type'];

    if not TypeIsObjectExists and IsTypeKnownObjectType(TypeName) then
    begin
      LoopObj.AddValue('type_is_object',true);
      TypeIsObjectExists:=true;
    end;

    if TypeIsObjectExists
      then EnableKnownFlagsForObjectTypes(LoopObj);
  end;

  Result:=not Target.Equals(FConfJsonIn);
  if Result
    then FConfJsonOut:=Target;
end;

{ ----------------------------------------------------------------------- }

function TPreProcessConfigForCollectionsList.IsTypeKnownObjectType(const ATypeName: String): Boolean;
begin
  Result:=
    StartsStr('C',ATypeName) or
    SameText(ATypeName,'TObject') or
    SameText(ATypeName,'TComponent');
end;

{ ======================================================================= }

end.
