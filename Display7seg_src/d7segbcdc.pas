{*****************************************************}
{* "CC BCD decoder" unit for Display7seg application *}
{*****************************************************}

unit d7segbcdc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {*************}
  { TfD7SegBCDc }
  {*************}
  TfD7SegBCDc = class(TForm)
    StaticText1: TStaticText;
    Label1, Label10, Label11, Label12, Label13, Label18, Label9: TLabel;
    Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    laA, laB, laC, laD: TLabel;
    imDisplay, imIC4511, imRBCD4, imRBCD3, imRBCD2, imRBCD1: TImage;
    imRA, imRB, imRC, imRD, imRE, imRF, imRG: TImage;
    imSwitchA, imSwitchB, imSwitchC, imSwitchD: TImage;
    shSwitchAa, shSwitchAb, shSwitchBa, shSwitchBb: TShape;
    shSwitchCa, shSwitchCb, shSwitchDa, shSwitchDb: TShape;
    shLedA, shLedB, shLedC, shLedD: TShape;
    shDP, shSegA, shSegB, shSegC: TShape;
    shSegD, shSegE, shSegF, shSegG: TShape;
    Shape1, Shape101, Shape105, Shape107, Shape108, Shape109: TShape;
    Shape110, Shape112, Shape113, Shape115, Shape118, Shape121: TShape;
    Shape122, Shape123, Shape125, Shape132, Shape133, Shape134: TShape;
    Shape135, Shape172, Shape173, Shape174, Shape175, Shape177: TShape;
    Shape178, Shape179, Shape180, Shape181, Shape182, Shape183: TShape;
    Shape184, Shape185, Shape186, Shape187, Shape188, Shape189: TShape;
    Shape26, Shape29, Shape30, Shape32, Shape55, Shape58, Shape63: TShape;
    Shape64, Shape65, Shape66, Shape67, Shape68, Shape69, Shape70: TShape;
    Shape71, Shape72, Shape73, Shape74, Shape75, Shape78, Shape95: TShape;
    Shape96, Shape97, Shape98, Shape99: TShape;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imSwitchAClick(Sender: TObject);
    procedure imSwitchBClick(Sender: TObject);
    procedure imSwitchCClick(Sender: TObject);
    procedure imSwitchDClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    aSwitches: TSwitches;
    aLeds: TLeds;
    dDisplay7S: TDisplay7S;
    dIC4511: TIC4511;
  end;

const
  // Switches description: Data = objects top positions: transparent image, long bar shape, short bar shape
  DSwitches: array[0..3] of TSwitchData = (
    (Name: 'SwitchA';
     Data: (145, 167, 147);
     Voltage: True),
    (Name: 'SwitchB';
     Data: (241, 263, 243);
     Voltage: True),
    (Name: 'SwitchC';
     Data: (335, 357, 337);
     Voltage: True),
    (Name: 'SwitchD';
     Data: (430,452, 432);
     Voltage: True)
  );

var
  fD7SegBCDc: TfD7SegBCDc;

implementation

{$R *.lfm}

{ Do CC 7-segment display simulation with BCD (IC 4511) }

procedure DoSimulation(DSwitches: array of TSwitchData; var Switches: TSwitches; var Leds: TLeds;
  var IC4511: TIC4511; var Display7S: TDisplay7S; SX: Integer);

var
  I, IX: Integer;
  C: Char;

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
  // Connection of the BCD input pins to the "switch voltage" (+VS if switch is closed)
  for I := 1 to 4 do begin
    IX := I - 1;
    IC4511.Inputs[I] := Switches[IX].SwitchVoltage;
  end;
  // Do BCD (IC 4511) simulation
  SimulIC4511(IC4511);
  // Connection of BCD output pins to the 7-segment display pins a - g
  for C := 'a' to 'g' do
    Display7S.LedVoltages[C] := IC4511.Outputs[C];
  // Do 7-segment display simulation
  SegmentsOnOff(Display7S);
end;

{*************}
{ TfD7SegBCDc }
{*************}

{ Application start: Initializations }

procedure TfD7SegBCDc.FormCreate(Sender: TObject);

begin
  // Apply subscripts to switch labels
  laA.Caption := StringReplace(laA.Caption, '1', SUB_Digits[1], []); laB.Caption := StringReplace(laB.Caption, '2', SUB_Digits[2], []);
  laC.Caption := StringReplace(laC.Caption, '3', SUB_Digits[3], []); laD.Caption := StringReplace(laD.Caption, '4', SUB_Digits[4], []);
  // Create array with "switches" as elements
  SetLength(aSwitches, 4);
  aSwitches[0].SwitchImg := imSwitchA; aSwitches[0].SwitchBLong := shSwitchAa; aSwitches[0].switchBShort := shSwitchAb;
  aSwitches[1].SwitchImg := imSwitchB; aSwitches[1].SwitchBLong := shSwitchBa; aSwitches[1].switchBShort := shSwitchBb;
  aSwitches[2].SwitchImg := imSwitchC; aSwitches[2].SwitchBLong := shSwitchCa; aSwitches[2].switchBShort := shSwitchCb;
  aSwitches[3].SwitchImg := imSwitchD; aSwitches[3].SwitchBLong := shSwitchDa; aSwitches[3].switchBShort := shSwitchDb;
  // Create array with "LEDs" as elements
  SetLength(aLeds, 4);
  aLeds[0].LedShape := shLedA; aLeds[1].LedShape := shLedB; aLeds[2].LedShape := shLedC; aLeds[3].LedShape := shLedD;
  // Create array with "segments" as elements
  dDisplay7S.Segments['a'].SegmentShape := shSegA; dDisplay7S.Segments['b'].SegmentShape := shSegB;
  dDisplay7S.Segments['c'].SegmentShape := shSegC; dDisplay7S.Segments['d'].SegmentShape := shSegD;
  dDisplay7S.Segments['e'].SegmentShape := shSegE; dDisplay7S.Segments['f'].SegmentShape := shSegF;
  dDisplay7S.Segments['g'].SegmentShape := shSegG;
  // Display used is of type "common cathode" (common pin connected to ground)
  dDisplay7S.DisplayType := 'cc'; dDisplay7S.CommonVoltage := False;
end;

{ Form getting the focus: Reset the simulation }

procedure TfD7SegBCDc.FormActivate(Sender: TObject);

begin
  // Set all switches to "off"; turn all LEDs off
  SwitchesAllOnOff(DSwitches, aSwitches, 'off');
  LedsAllOnOff(aLeds, 'off');
  // Set LT, BL and LE pins as connected in this circuit
  dIC4511.LT := True; dIC4511.BL := True; dIC4511.LE := False;
  // Set display segments to "0"
  SegmentsZero(dDisplay7S.Segments);
end;

{ Switch S1 - S7 pushed (user click on corresponding transparent image): Do the simulation (changing status of this switch) }

procedure TfD7SegBCDc.imSwitchAClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dIC4511, dDisplay7S, 1);
end;

procedure TfD7SegBCDc.imSwitchBClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dIC4511, dDisplay7S, 2);
end;

procedure TfD7SegBCDc.imSwitchCClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dIC4511, dDisplay7S, 3);
end;

procedure TfD7SegBCDc.imSwitchDClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dIC4511, dDisplay7S, 4);
end;

{ Button "Close" pushed: Close the simualtion window }

procedure TfD7SegBCDc.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

