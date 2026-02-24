// START: STYLE-TEMPLATE===========================================================================// START: AI-Descriptions===========================================================================
// ======================================================================                          // - Jede Unit muss mit diesem Banner beginnen
//                                                                                                 // - Jede Unit muss in UTF8 mit BOM und Windows-LineEndings (CRLF) encoded sein!
// TaifunUnitStyle - Kurzbeschreibung der Unit                                                     // - Hier steht der Name der Unit, der gleich dem Unit-Namen sein muss.
//                                                                                                 //   Es sollte eine Kurzbeschreibung existieren.
// Autor: Auflistung von Hauptverantwortlichen                                                     // - Es muss mindestens ein Hauptverantwortlicher vorhanden sein.
//                                                                                                 //   Mehrere Autoren werden mit "/" getrennt.
// ======================================================================                          // - Hier endet der Banner
                                                                                                   // - Obligatorische Leerzeile
{$I Tfw.Define.pas}                                                                                // - Es muss entweder eine "Base.Define.pas" oder "Tfw.Define.pas" included werden
                                                                                                   // - Obligatorische Leerzeile
unit TaifunUnitStyle;                                                                              //
                                                                                                   // - Obligatorische Leerzeile
{ ======================================================================= }                        // - Vor dem Interface-Keyword muss die doppelte Trennlinie stehen
interface                                                                                          //
{ ======================================================================= }                        // - Nach dem Interface-Keyword muss die doppelte Trennlinie stehen
                                                                                                   // - Obligatorische Leerzeile vor "uses"
uses                                                                                               // - "uses" muss alleine in einer Zeile stehen
                                                                                                   // - Obligatorische Leerzeile nach "uses"
  System.Classes,                                                                                  // - Je Zeile darf nur eine Unit stehen
  System.SysUtils,                                                                                 // - Alphabetische Auflistung von Delphi-Units in Gruppen:
  Vcl.Controls,                                                                                    //   - Delphi-Gruppe:      System.*, Winapi.*, VCL.*, Rest.*
  Vcl.Forms,                                                                                       //
  Vcl.Graphics,                                                                                    //
  Winapi.Messages,                                                                                 //
                                                                                                   //   - Nach jeder Gruppe kommt eine Leerzeile
  Spring.Collections,                                                                              //   - Third-Party:        *
  VirtualTrees,                                                                                    //
                                                                                                   //
  Base.Cmd,                                                                                        //   - Base-Gruppe:        Base.*
  Base.Types,                                                                                      //
  Base.Utils.Msg,                                                                                  //
                                                                                                   //
  Base.UI.Icons,                                                                                   //   - Base-UI-Gruppe:     Base.UI.*
                                                                                                   //
  Business.Cty.Typ,                                                                                //   - Business-Gruppe:    Business.*
                                                                                                   //
  Business.UI.Bank.Utils,                                                                          //   - Business-UI-Gruppe: Business.UI.*
                                                                                                   //
  Dms.Shared.App.Utils,                                                                            //   - Shared-Gruppe:      *.Shared.*
  Dms.Shared.Kw.Tree,                                                                              //
                                                                                                   //
  Tfw.Ad.Typ,                                                                                      //   - TFW-Gruppe:         Tfw.*
  Tfw.VBh.Typ;                                                                                     //
                                                                                                   // - Obligatorische Leerzeile nach der letzten Unit
{ ----------------------------------------------------------------------- }                        // - Obligatorische einfache Trennlinie, die den Uses-Bereich optisch trennt
                                                                                                   // - Obligatorische Leerzeile vor "type"
type                                                                                               // - "type" muss alleine stehen
                                                                                                   // - Obligatorische Leerzeile nach "type"
  TMyVariant1 = packed record                                                                      //
    SpecName : S_50;                                                                               //
    SpecValue: Integer;                                                                            //
  end;                                                                                             //
                                                                                                   //
  TMyVariant2 = packed record                                                                      //
    SpecName : S_50;                                                                               //
    SpecValue: S_255;                                                                              //
  end;                                                                                             //
                                                                                                   //
  TMyRecord = packed record                                                                        // - Namen von Records fangen immer mit "T" an
    GUID          : TGUID;                                                                         // - Doppelpunkte sind vertikal untereinander ausgerichtet
    Var1          : Integer;                                                                       //
    Var2          : TDate;                                                                         //
    Filler1       : Array[1..300] of Byte;                                                         //
    case Byte of                                                                                   //
      0: (Filler2 : Array[1..4096] of Byte);                                                       //
      1: (Variant1: TMyVariant1);                                                                  //
      2: (Variant2: TMyVariant2);                                                                  //
  end;                                                                                             //
                                                                                                   //
  CSomeClass = class                                                                               // - Klassennamen sollten mit "C" anfangen, können aber auch klassisch mit "T" anfangen
   strict private                                                                                  // - Die Keywords für die Sichtbarkeit sind mit einem zusätzlichen Leerzeichen eingerückt
    FText: String;                                                                                 // - Feldnamen fangen mit einem "F" an
    FValue: Word;                                                                                  // - Felder in einem zusammenhängenden Block sind alphabetisch sortiert
    FVarA: Integer;                                                                                // - Hier müssen die Doppelpunkte NICHT untereinander stehen
    function  GetVarA: Integer;                                                                    // - Die Namen von Funktionen und Prozeduren im gleichen zusammenhängenden Block
    procedure SetVarA(AValue: Integer);                                                            //   stehen untereinandner
   strict private class var                                                                        // - Die Keywords für die Sichtbarkeit leiten einen neuen Block ein
    FClassVar: String;                                                                             //
   strict protected                                                                                //
    procedure DoSomething;                                                                         // - Die Funktionen und Prozeduren in einem zusammenhängenden Block sind alphabetisch
    function  IsSomething: Boolean;                                                                //   nach Name sortiert
   public                                                                                          //
    constructor Create;                                                                            // - Konstruktoren und Destruktoren bilden eine eigene Gruppe (Ausrichtung gilt nur hier)
    destructor  Destroy; override;                                                                 // - Der Destruktor muss immer ein override haben (Gilt nicht für Klassendestruktoren)
    function  CalcValue: Word;                                                                     // - Die Namen von Prozeduren, Funktionen und Properties stehen im gleichen
    procedure CalcVar1;                                                                            //   zusammenhängenden Block untereinander
    property  Value: Word read FValue;                                                             // - Properties stehen am Ende eines Sichtbarkeits-Blocks und sind
    property  VarA: Integer read GetVarA write SetVarA;                                            //   alphabetisch nach Name sortiert
   published                                                                                       // - Im Published-Abschnitt können Variablen ohne "F"-Präfix stehen (z.B. für Komponenten)
    AnotherComponent: TComponent;                                                                  // - Diese müssen nicht zwingend mit "F" anfangen
    SomeComponent: TComponent;                                                                     //
   public                                                                                          //
    class procedure Init;                                                                          // - Klassen-Methoden/Konstruktoren stehen in einem eigenen Sichtbarkeitsblock
  end;                                                                                             //
                                                                                                   // - Obligatorische Leerzeile
{ ======================================================================= }                        // - Vor dem "implementation"-Keyword muss die doppelte Trennlinie stehen
implementation                                                                                     //
{ ======================================================================= }                        // - Nach dem "implementation"-Keyword muss die doppelte Trennlinie stehen
                                                                                                   // - Obligatorische Leerzeile vor "uses"
uses                                                                                               // - Es gelten die gleichen Regeln wie im Interface-Teil
                                                                                                   //
  System.Math,                                                                                     //
                                                                                                   //
  Base.Utils.Str;                                                                                  //
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
{ ======================================================================= }                        // - Vor jeder neuen Klassenimplementierunng steht ein Banner in diesem Format
{ CSomeClass                                                              }                        // - Der Name der Klasse muss mit dieser folgenden Klasse übereinstimmen
{ ======================================================================= }                        // - Banner wird abgeschlossen
                                                                                                   // - Obligatorische Leerzeile nach dem  Banner
constructor CSomeClass.Create;                                                                     // - Implementierung des Konstruktors steht im Implementierungsblock der Klasse als
begin                                                                                              //   Erstes, wenn vorhanden.
  FText:='Initial value';                                                                          //
end;                                                                                               //
                                                                                                   // - Nach jeder Methodenimplementation kommt eine Leerzeile...
{ ----------------------------------------------------------------------- }                        // - ...dann eine einfache Trennlinie...
                                                                                                   // - ..und eine Leerzeile
destructor CSomeClass.Destroy;                                                                     // - Wenn ein Destruktor vorhanden ist, folgt er gleich nach dem Konstruktor
begin                                                                                              //
  if IsSomething                                                                                   //
    then DoSomething;                                                                              //
  inherited;                                                                                       // - Der Destruktor muss irgendwo einen "inherited"-Aufruf beinhalten
end;                                                                                               //
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
function CSomeClass.CalcValue: Word;                                                               //
begin                                                                                              //
  Result:=FValue;                                                                                  // - Es werden keine Leerzeichen vor oder hinter dem Zuweisungsoperator eingefügt.
  Result:=Result*2+150-10;                                                                         // - Auch bei einfachen arithmetischen Operatoren (+-*/) werden keine Leerzeichen
end;                                                                                               //   davor oder danach eingefügt.
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
procedure CSomeClass.CalcVar1;                                                                     //
var                                                                                                // - Variablendeklarationsblock in einer Methode, Funktion oder Prozedur:
  Age         : Byte;                                                                              //   - Ist Alphabetisch nach Variablennamen sortiert
  Name        : String;                                                                            //   - Jede Zeile listet nur eine Variable
  PlaceOfBirth: String;                                                                            //   - Doppelpunkte stehen untereinander
  XyChromosome: Boolean;                                                                           //   - Nach dem Doppelpunkt kommt ein Leerzeichen gefolgt vom Variablentyp
begin                                                                                              //
                                                                                                   //
end;                                                                                               //
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
procedure CSomeClass.DoSomething;                                                                  //
begin                                                                                              //
                                                                                                   //
end;                                                                                               //
                                                                                                   //
                                                                                                   // - Mehrere Leerzeilen (auch mit \s+) werden zu einer Leerzeile kollabiert
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
function CSomeClass.GetVarA: Integer;                                                              //
begin                                                                                              //
  Result:=FVarA;                                                                                   //
end;                                                                                               //
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
class procedure CSomeClass.Init;                                                                   //
begin                                                                                              //
                                                                                                   //
end;                                                                                               //
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
function CSomeClass.IsSomething: Boolean;                                                          //
begin                                                                                              //
  Result:=true;                                                                                    //
end;                                                                                               //
                                                                                                   //
{ ----------------------------------------------------------------------- }                        //
                                                                                                   //
procedure CSomeClass.SetVarA(AValue: Integer);                                                     //
begin                                                                                              //
  FVarA:=AValue;                                                                                   //
end;                                                                                               //
                                                                                                   //
{ ======================================================================= }                        // - Vor dem Unit-"end." steht eine doppelte Trennlinie
                                                                                                   //
end.                                                                                               //
