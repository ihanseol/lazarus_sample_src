{********************************************************}
{* "7-segment counter" unit for Display7seg application *}
{********************************************************}

unit d7segcount;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {****************}
  { TfD7SegCounter }
  {****************}
  TfD7SegCounter = class(TForm)
    StaticText1: TStaticText;
    Label1, Label10, Label11, Label13, Label14, Label15, Label16: TLabel;
    Label17, Label18, Label19, Label2, Label20, Label21, Label22: TLabel;
    Label3, Label4, Label5, Label6, Label7, Label8, Label9: TLabel;
    laS: TLabel;
    imDisplay, imIC7447, imIC7490: TImage;
    imR1, imSwitchS: TImage;
    imRA, imRB, imRC, imRD, imRE, imRF, imRG: TImage;
    shSwitchSa, shSwitchSb, shDP: TShape;
    shSegA, shSegB, shSegC, shSegD, shSegE, shSegF, shSegG: TShape;
    Shape1, Shape107, Shape108, Shape109, Shape132, Shape133, Shape134: TShape;
    Shape135, Shape136, Shape137, Shape138, Shape139, Shape140, Shape141: TShape;
    Shape142, Shape143, Shape144, Shape145, Shape172, Shape174, Shape175: TShape;
    Shape177, Shape178, Shape179, Shape180, Shape181, Shape182, Shape183: TShape;
    Shape184, Shape185, Shape186, Shape187, Shape188, Shape189, Shape29: TShape;
    Shape55, Shape58, Shape59, Shape63, Shape64, Shape65, Shape66, Shape67: TShape;
    Shape68, Shape69, Shape70, Shape71, Shape76, Shape77, Shape78, Shape95, Shape99: TShape;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imSwitchSMouseDown(Sender: TObject);
    procedure imSwitchSMouseUp(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    dSwitch: TSwitch;
    dDisplay7S: TDisplay7S;
    dIC7447: TIC7447;
    dIC7490: TIC7490;
  end;

const
  // Switch description: Data = object top positions: transparent image, long bar shape, short bar shape
  DSwitches: array[0..0] of TSwitchData = (
    (Name: 'SwitchS';
     Data: (72, 94, 74);
     Voltage: True)
  );

var
  fD7SegCounter: TfD7SegCounter;

implementation

{$R *.lfm}

{ Do CA 7-segment counter simulation (IC 7447 and IC 7490) }

procedure DoSimulation(var IC7490: TIC7490; var IC7447: TIC7447; var Display7S: TDisplay7S);

var
  I: Integer;
  C: Char;

begin
  // Do counter (IC 7490) simulation
  SimulIC7490(IC7490);
  // Connection of the counter output pins to the BCD input pins
  for I := 1 to 4 do
    IC7447.Inputs[I] := IC7490.Outputs[I];
  // Do BCD (IC 7447) simulation
  SimulIC7447(IC7447);
  // Connection of the BCD output pins to the 7-segment display pins a - g
  for C := 'a' to 'g' do
    Display7S.LedVoltages[C] := IC7447.Outputs[C];
  // Do 7-segment display simulation
  SegmentsOnOff(Display7S);
end;

{****************}
{ TfD7SegCounter }
{****************}

{ Application start: Initializations }

procedure TfD7SegCounter.FormCreate(Sender: TObject);

begin
  // Create switch record
  dSwitch.SwitchImg := imSwitchS; dSwitch.SwitchBLong := shSwitchSa; dSwitch.switchBShort := shSwitchSb;
  // Create array with "segments" as elements
  dDisplay7S.Segments['a'].SegmentShape := shSegA; dDisplay7S.Segments['b'].SegmentShape := shSegB;
  dDisplay7S.Segments['c'].SegmentShape := shSegC; dDisplay7S.Segments['d'].SegmentShape := shSegD;
  dDisplay7S.Segments['e'].SegmentShape := shSegE; dDisplay7S.Segments['f'].SegmentShape := shSegF;
  dDisplay7S.Segments['g'].SegmentShape := shSegG;
  // Display used is of type "common anode" (common pin connected to +Vs)
  dDisplay7S.DisplayType := 'ca'; dDisplay7S.CommonVoltage := True;
end;

{ Form getting the focus: Reset the simulation }

procedure TfD7SegCounter.FormActivate(Sender: TObject);

begin
  // Set switch to "off"
  SwitchOnOff(DSwitches, dSwitch, 'off');
  // Set counter to 0
  dIC7490.Count := 0;
  // Set display segments to "0"
  SegmentsZero(dDisplay7S.Segments);
end;

{ Pushbutton pushed (user mouse-down/mouse-up on corresponding transparent image):
  Do the simulation (incrementing the actual value within the counter IC) }

procedure TfD7SegCounter.imSwitchSMouseDown(Sender: TObject);

// Mouse-down: Pushbutton is pushed

begin
  SwitchOnOff(DSwitches, dSwitch, 'on');
  DoSimulation(dIC7490, dIC7447, dDisplay7S);                                  // do the simulation
end;

procedure TfD7SegCounter.imSwitchSMouseUp(Sender: TObject);

// Mouse-up: Pushbutton is released

begin
  SwitchOnOff(DSwitches, dSwitch, 'off');
end;

{ Button "Close" pushed: Close the simualtion window }

procedure TfD7SegCounter.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

