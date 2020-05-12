unit DeepCloneComponentExample;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.TypInfo,
  System.StrUtils,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  WDDT.Vcl.RTTI,
  WDDT.Vcl.Controls;

type
  TDeepCloneComponentExampleForm = class(TForm)
    Panel1: TPanel;
    MainScrollBox: TScrollBox;
    ItemPanel: TPanel;
    CheckBox1: TCheckBox;
    MainLabel: TLabel;
    SubPanel: TPanel;
    Button1: TButton;
    ComboBox1: TComboBox;
    CloneButton: TButton;
    procedure CloneButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TDeepCloneComponentExampleForm.FormCreate(Sender: TObject);
begin
  SetGeneralMouseWheelHandler(Self);
end;

procedure TDeepCloneComponentExampleForm.CloneButtonClick(Sender: TObject);
var
  ClonedComponent: TComponent;
  ClonedItemPanel: TPanel absolute ClonedComponent;
  MainLabel: TLabel;
begin
  MainLabel := nil;

  // Here is the code of the main interest, where a clone of the ItemPanel with all sub controls is
  // created...
  ClonedComponent := DeepCloneComponent(ItemPanel, MainScrollBox,
    procedure(Target: TObject; PropInfo: PPropInfo; out Accept: Boolean)
    begin
      // The following properties on a TComboBox must be cloned at the end, so we skip them here...
      if (Target is TCustomCombo) and
        MatchText(string(PropInfo.Name), ['ItemHeight', 'ItemIndex', 'Items'])  then
        Accept := False;
    end,
    procedure(Source, Target: TComponent)
    var
      SourceCombo: TComboBox absolute Source;
      TargetCombo: TComboBox absolute Target;
    begin
      // ...and assign them in this AfterClone closure
      if Source is TComboBox then
      begin
        TargetCombo.Items := SourceCombo.Items;
        TargetCombo.ItemIndex := SourceCombo.ItemIndex;
      end
      // Save the cloned MainLabel to work with it after the whole clone is done
      else if SameText(Source.Name, 'MainLabel') then
        MainLabel := TLabel(Target);
    end);

  // Now we have our ClonedItemPanel...
  if ClonedComponent is TPanel then
  begin
    ClonedItemPanel.Top := CalcRequiredControlHeight(MainScrollBox) + 10;
    ClonedItemPanel.Visible := True;
    if Assigned(MainLabel) then
      MainLabel.Caption := Format('This is a cloned panel #%d', [MainScrollBox.ControlCount - 1]);
  end;
end;

end.
