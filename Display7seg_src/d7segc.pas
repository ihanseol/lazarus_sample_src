{***********************************************************}
{* "CC 7-segment display" unit for Display7seg application *}
{***********************************************************}

unit d7segc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {**********}
  { TfD7Segc }
  {**********}
  TfD7Segc = class(TForm)
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label10, Label11, Label18: TLabel;
    laA, laB, laC, laD, laE, laF, laG: TLabel;
    imDisplay, imRA, imRB, imRC, imRD, imRE, imRF, imRG: TImage;
    imSwitchA, imSwitchB, imSwitchC, imSwitchD, imSwitchE, imSwitchF, imSwitchG: TImage;
    shSwitchAa, shSwitchAb, shSwitchBa, shSwitchBb, shSwitchCa, shSwitchCb: TShape;
    shSwitchDa, shSwitchDb, shSwitchEa, shSwitchEb, shSwitchFa, shSwitchFb, shSwitchGa, shSwitchGb: TShape;
    shLedA, shLedB, shLedC, shLedD, shLedE, shLedF, shLedG: TShape;
    shSegA, shSegB, shSegC, shSegD, shSegE, shSegF, shSegG, shDP: TShape;
    Shape1, Shape101, Shape102, Shape103, Shape104, Shape105, Shape106, Shape108: TShape;
    Shape110, Shape111, Shape113, Shape114, Shape115, Shape116, Shape117, Shape118: TShape;
    Shape119, Shape120, Shape121, Shape122, Shape123, Shape124, Shape125, Shape126: TShape;
    Shape127, Shape128, Shape129, Shape130, Shape131, Shape172, Shape173, Shape174: TShape;
    Shape175, Shape176, Shape25, Shape26, Shape29, Shape30, Shape32, Shape33: TShape;
    Shape36, Shape55, Shape56, Shape57, Shape58, Shape59, Shape60, Shape61: TShape;
    Shape62, Shape63, Shape78, Shape94, Shape95, Shape96: TShape;
    Shape97: TShape;
    Shape98: TShape;
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
     Voltage: True),
    (Name: 'SwitchB';
     Data: (162, 184, 164);
     Voltage: True),
    (Name: 'SwitchC';
     Data: (258, 280, 260);
     Voltage: True),
    (Name: 'SwitchD';
     Data: (352,374, 354);
     Voltage: True),
    (Name: 'SwitchE';
     Data: (447, 469, 449);
     Voltage: True),
    (Name: 'SwitchF';
     Data: (543, 565, 545);
     Voltage: True),
    (Name: 'SwitchG';
     Data: (636, 658, 638);
     Voltage: True)
  );

var
  fD7Segc: TfD7Segc;

implementation

{$R *.lfm}

{ Do CC 7-segment display simulation with 7 switches }

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
  // Connect the display segments to the "switch voltage" (+VS if switch is closed)
  for I := 1 to 7 do begin
    IX := I - 1;
    Display7S.LedVoltages[DisplaySegments[I]] := Switches[IX].SwitchVoltage;
  end;
  // Do 7-segment display simulation
  SegmentsOnOff(Display7S);
end;

{**********}
{ TfD7Segc }
{**********}

{ Application start: Initializations }

procedure TfD7Segc.FormCreate(Sender: TObject);

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
  // Display used is of type "common cathode" (common pin connected to ground)
  dDisplay7S.DisplayType := 'cc'; dDisplay7S.CommonVoltage := False;
end;

{ Form getting the focus: Reset the simulation }

procedure TfD7Segc.FormActivate(Sender: TObject);

begin
  // Set all switches to "off"; turn all Leds and segments off
  SwitchesAllOnOff(DSwitches, aSwitches, 'off');
  LedsAllOnOff(aLeds, 'off');
  SegmentsAllOnOff(dDisplay7S.Segments, 'off');
end;

{ Switch S1 - S7 pushed (user click on corresponding transparent image): Do the simulation (changing status of this switch) }

procedure TfD7Segc.imSwitchAClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 1);
end;

procedure TfD7Segc.imSwitchBClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 2);
end;

procedure TfD7Segc.imSwitchCClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 3);
end;

procedure TfD7Segc.imSwitchDClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 4);
end;

procedure TfD7Segc.imSwitchEClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 5);
end;

procedure TfD7Segc.imSwitchFClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 6);
end;

procedure TfD7Segc.imSwitchGClick(Sender: TObject);

begin
  DoSimulation(DSwitches, aSwitches, aLeds, dDisplay7S, 7);
end;

{ Button "Close" pushed: Close the simualtion window }

procedure TfD7Segc.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

