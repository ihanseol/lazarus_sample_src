{***********************************************************}
{* "CA 7-segment display" unit for Display7seg application *}
{***********************************************************}

unit d7sega;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {**********}
  { Tfd7Sega }
  {**********}
  Tfd7Sega = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label10, Label11, Label18: TLabel;
    laF, laD, laB, laG, laE, laC, laA: TLabel;
    imDisplay, imRA, imRB, imRC, imRD, imRE, imRF, imRG: TImage;
    imSwitchF, imSwitchD, imSwitchB, imSwitchA: TImage;
    imSwitchG, imSwitchE, imSwitchC: TImage;
    shSwitchFa, shSwitchFb, shSwitchDa, shSwitchDb, shSwitchBa: TShape;
    shSwitchBb, shSwitchGa, shSwitchGb, shSwitchEa, shSwitchEb: TShape;
    shSwitchCa, shSwitchCb, shSwitchAa, shSwitchAb: TShape;
    shLedF, shLedD, shLedB, shLedG, shLedE, shLedC, shLedA: TShape;
    shDP, shSegD, shSegG, shSegA, shSegE, shSegC, shSegF, shSegB: TShape;
    Shape1, Shape100, Shape101, Shape102, Shape103, Shape104: TShape;
    Shape105, Shape106, Shape107, Shape108, Shape109, Shape110: TShape;
    Shape111, Shape112, Shape113, Shape114, Shape115, Shape116: TShape;
    Shape117, Shape118, Shape119, Shape120, Shape121, Shape122: TShape;
    Shape123, Shape124, Shape125, Shape126, Shape127, Shape128: TShape;
    Shape129, Shape130, Shape131, Shape132, Shape133, Shape134: TShape;
    Shape172, Shape173, Shape174, Shape175, Shape176, Shape25: TShape;
    Shape26, Shape29, Shape30, Shape32, Shape33, Shape36, Shape55: TShape;
    Shape56, Shape57, Shape58, Shape59, Shape60, Shape61, Shape62: TShape;
    Shape63, Shape78, Shape94, Shape95, Shape96, Shape97, Shape98, Shape99: TShape;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imSwitchAClick(Sender: TObject);
    procedure imSwitchBClick(Sender: TObject);
    procedure imSwitchCClick(Sender: TObject);
    procedure imSwitchDClick(Sender: TObject);
    procedure imSwitchEClick(Sender: TObject);
    procedure imSwitchFClick(Sender: TObject);
    procedure imSwitchGClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    aSwitches: TSwitches;
    aLeds: TLeds;
    dDisplay7S: TDisplay7S;
  end;

const
  // Switches description: Data = objects top positions: transparent image, long bar shape, short bar shape
  DSwitches: array[0..6] of TSwitchData = (
    (Name: 'SwitchA';
     Data: (64, 86, 66);
     Voltage: False),
    (Name: 'SwitchB';
     Data: (162, 184, 164);
     Voltage: False),
    (Name: 'SwitchC';
     Data: (258, 280, 260);
     Voltage: False),
    (Name: 'SwitchD';
     Data: (352,374, 354);
     Voltage: False),
    (Name: 'SwitchE';
     Data: (447, 469, 449);
     Voltage: False),
    (Name: 'SwitchF';
     Data: (543, 565, 545);
     Voltage: False),
    (Name: 'SwitchG';
     Data: (636, 658, 638);
     Voltage: False)
  );

var
  fd7Sega: Tfd7Sega;

implementation

{$R *.lfm}

{ Do CA 7-segment display simulation with 7 switches }

procedure DoSimulation(DSwitches: array of TSwitchData; var Switches: TSwitches; var Leds: TLeds; var Display7S: TDisplay7S; SX: Integer);

var
  I, IX: Integer;

begin
  IX := SX - 1;
  if Switches[IX].SwitchStatus = 'on' then begin
    // Switch is actually open; close it
    SwitchOnOff(DSwitches, Switches[IX], 'off');
    LedOnOff(Leds[IX], 'off');
  end
  else begin
    // Switch is actually closed; open it
    SwitchOnOff(DSwitches, Switches[IX], 'on');
    LedOnOff(Leds[IX], 'on');
  end;
  // Connect the display segments to the "switch voltage" (ground if switch is closed)
  for I := 1 to 7 do begin
    IX := I - 1;
    Display7S.LedVoltages[DisplaySegments[I]] := Switches[IX].SwitchVoltage;
  end;
  // Do 7-segment display simulation
  SegmentsOnOff(Display7S);
end;

{**********}
{ Tfd7Sega }
{**********}

{ Application start: Initializations }

procedure Tfd7Sega.FormCreate(Sender: TObject);

begin
  // Apply subscripts to switch labels
  laA.Caption := StringReplace(laA.Caption, '1', SUB_Digits[1], []); laB.Caption := StringReplace(laB.Caption, '2', SUB_Digits[2], []);
  laC.Caption := StringReplace(laC.Caption, '3', SUB_Digits[3], []); laD.Caption := StringReplace(laD.Caption, '4', SUB_Digits[4], []);
  laE.Caption := StringReplace(laE.Caption, '5', SUB_Digits[5], []); laF.Caption := StringReplace(laF.Caption, '6', SUB_Digits[6], []);
  laG.Caption := StringReplace(laG.Caption, '7', SUB_Digits[7], []);
  // Create array with "switches" as elements
  SetLength(aSwitches, 7);
  aSwitches[0].SwitchImg := imSwitchA; aSwitches[0].SwitchBLong := shSwitchAa; aSwitches[0].switchBShort := shSwitchAb;
  aSwitches[1].SwitchImg := imSwitchB; aSwitches[1].SwitchBLong := shSwitchBa; aSwitches[1].switchBShort := shSwitchBb;
  aSwitches[2].SwitchImg := imSwitchC; aSwitches[2].SwitchBLong := shSwitchCa; aSwitches[2].switchBShort := shSwitchCb;
  aSwitches[3].SwitchImg := imSwitchD; aSwitches[3].SwitchBLong := shSwitchDa; aSwitches[3].switchBShort := shSwitchDb;
  aSwitches[4].SwitchImg := imSwitchE; aSwitches[4].SwitchBLong := shSwitchEa; aSwitches[4].switchBShort := shSwitchEb;
  aSwitches[5].SwitchImg := imSwitchF; aSwitches[5].SwitchBLong := shSwitchFa; aSwitches[5].switchBShort := shSwitchFb;
  aSwitches[6].SwitchImg := imSwitchG; aSwitches[6].SwitchBLong := shSwitchGa; aSwitches[6].switchBShort := shSwitchGb;
  // Create array with "LEDs" as elements
  SetLength(aLeds, 7);
  aLeds[0].LedShape := shLedA; aLeds[1].LedShape := shLedB; aLeds[2].LedShape := shLedC; aLeds[3].LedShape := shLedD;
  aLeds[4].LedShape := shLedE; aLeds[5].LedShape := shLedF; aLeds[6].LedShape := shLedG;
  // Create array with "segments" as elements
  dDisplay7S.Segments['a'].SegmentShape := shSegA; dDisplay7S.Segments['b'].SegmentShape := shSegB;
  dDisplay7S.Segments['c'].SegmentShape := shSegC; dDisplay7S.Segments['d'].SegmentShape := shSegD;
  dDisplay7S.Segments['e'].SegmentShape := shSegE; dDisplay7S.Segments['f'].SegmentShape := shSegF;
  dDisplay7S.Segments['g'].SegmentShape := shSegG;
  // Display used is of type "common anode" (common pin connected to +Vs)
  dDisplay7S.DisplayType := 'ca'; dDisplay7S.CommonVoltage := True;
end;

{ Form getting the focus: Reset the simulation }

procedure Tfd7Sega.FormActivate(Sender: TObject);

begin
  // Set all switches to "off"; turn all Leds and segments off
  SwitchesAllOnOff(DSwitches, aSwitches, 'off');
  LedsAllOnOff(aLeds, 'off');
  SegmentsAllOnOff(dDisplay7S.Segments, 'off');
end;

{ Switch S1 - S7 pushed (user click on corresponding transparent image): Do the simulation (changing status of this switch) }

procedure Tfd7Sega.imSwitchAClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 1);
end;

procedure Tfd7Sega.imSwitchBClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 2);
end;

procedure Tfd7Sega.imSwitchCClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 3);
end;

procedure Tfd7Sega.imSwitchDClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 4);
end;

procedure Tfd7Sega.imSwitchEClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 5);
end;

procedure Tfd7Sega.imSwitchFClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 6);
end;

procedure Tfd7Sega.imSwitchGClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 7);
end;

{ Button "Close" pushed: Close the simualtion window }

procedure Tfd7Sega.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

