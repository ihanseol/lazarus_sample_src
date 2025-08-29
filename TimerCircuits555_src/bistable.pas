{**********************************************************}
{* Bistable circuit unit for TimerCircuits555 application *}
{**********************************************************}

unit bistable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {************}
  { TfBistable }
  {************}
  TfBistable = class(TForm)
    StaticText1, StaticText2: TStaticText;
    Label2, Label3, Label4, Label6, Label7, Label8, Label10: TLabel;
    laReset, laResetSw, laRs6, laTh4: TLabel;
    laR1, laR2, laLED1, laLED2: TLabel;
    edBistable, edLeds, edComponents: TMemo;
    shSwitch1a, shSwitch1b, shSwitch2a, shSwitch2b: TShape;
    shTh4a, shTh4b, shRs6a, shRs6b: TShape;
    shLed1, shLed1a, shLed1b, shLed2, shLed2a, shLed2b, shLed12c: TShape;
    shReset1, shReset2, shReset3, shReset4, shReset5, shResetR: TShape;
    Shape1, Shape2, Shape3, Shape4, Shape5, Shape7, Shape8, Shape9: TShape;
    Shape14, Shape15, Shape16, Shape17, Shape18, Shape19, Shape10, Shape20: TShape;
    Shape23, Shape24, Shape28, Shape30, Shape31, Shape32, Shape33: TShape;
    imSwitch1, imSwitch2: TImage;
    btClose: TButton;
    tiSwitches: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imSwitch1Click(Sender: TObject);
    procedure imSwitch2Click(Sender: TObject);
    procedure tiSwitchesTimer(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    sPushbutton: string;
    aTimerPins: TTimerPins;
  public
    sResetInput: string;
    bLed1Used, bLed2Used: Boolean;
  end;

var
  fBistable: TfBistable;

implementation

{$R *.lfm}

{************}
{ TfBistable }
{************}

{ Application start: Initialisation }

procedure TfBistable.FormCreate(Sender: TObject);

begin
  // Default circuit values
  sResetInput := 'reset';
  bLed1Used := False; bLed2Used := True;
  // Apply subscripts
  laR1.Caption := 'R' + SUB_1; laR2.Caption := 'R' + SUB_2;
  edComponents.Text := StringReplace(edComponents.Text, 'R1', 'R' + SUB_1, []);
  edComponents.Text := StringReplace(edComponents.Text, 'R2', 'R' + SUB_2, []);
end;

{ Window show-up: Display and initialize actual circuit and display actual circuit info }

procedure TfBistable.FormActivate(Sender: TObject);

const
  BistableTxt1a = 'When the SET switch is pushed, the active low Trigger input (pin 2) sets the 555 internal RS latch. The logic low at the latch output is inverted';
  BistableTxt1b = 'and the timer''s  output (pin 3) becomes  logic high. It will stay  high, when the  switch is released (the RS latch memorizes the logic high state).';
  BistableTxt24 = 'When the  RESET switch is pushed, the active low  Reset input (pin 4) resets the 555 internal RS latch.';
  BistableTxt26 = 'When the  RESET switch is pushed, the  active high  Threshold input (pin 6) resets the 555 internal RS latch.';
  BistableTxt34 = 'The logic high at the latch  output is inverted and the  timer''s output (pin 3) becomes logic  low. It will stay  low, when the switch is released (the RS latch memorizes the logic low state).';
  BistableTxt36 = 'The logic high at the  latch output is  inverted and the timer''s output (pin 3) becomes logic  low. It will stay low, when the switch is released (the RS latch memorizes the logic low state).';
  Led1Txt = 'LED1 goes on, if the timer output is logic low: LED1 = on indicates that the flip-flop is reset, LED1 = out that the flip-flop is set.';
  Led2Txt = 'LED2 goes on, if the timer output is logic high: LED2 = on indicates that the flip-flop is set, LED2 = out that the flip-flop is reset.';

var
  BistableTxt: string;

begin
  laTh4.Visible := False; laRs6.Visible := False;
  shTh4a.Visible := False; shTh4b.Visible := False;
  shRs6a.Visible := False; shRs6b.Visible := False;
  BistableTxt := BistableTxt1a + ' ' + BistableTxt1b + LineEnding;
  if sResetInput = 'reset' then begin
    // Circuit with reset being done via Reset pin
    BistableTxt +=  BistableTxt24 + ' ' + BistableTxt34;
    laRs6.Visible := True;
    shRs6a.Visible := True; shRs6b.Visible := True;
    laReset.Caption := '4'; laReset.Hint := 'Reset';
    shReset1.Height := 50;
    shReset2.Top := 344; shReset2.Height := 240;
    shReset4.Top := 579; shReset5.Top := 599;
    shResetR.Top := 269; laR1.Top := 296;
    imSwitch1.Top := 571; shSwitch1a.Top := 575; shSwitch1b.Top := 592;
    laResetSw.Top := 624;
  end
  else begin
    // Circuit with reset being done via Threshold pin
    BistableTxt +=  BistableTxt26 + ' ' + BistableTxt36;
    laTh4.Visible := True;
    shTh4a.Visible := True; shTh4b.Visible := True;
    laReset.Caption := '6'; laReset.Hint := 'Threshold';
    shReset1.Height := 100;
    shReset2.Top := 336; shReset2.Height := 244;
    shReset4.Top := 316; shReset5.Top := 336;
    shResetR.Top := 536; laR1.Top := 563;
    imSwitch1.Top := 308; shSwitch1a.Top := 312; shSwitch1b.Top := 329;
    laResetSw.Top := 361;
  end;
  // Actual circuit output (LEDs)
  edBistable.Clear; edBistable.Lines.AddText(BistableTxt);
  shLed1.Visible := False;  shLed2.Visible := False;
  laLed1.Visible := False;  laLed2.Visible := False;
  shLed1a.Visible := False; shLed1b.Visible := False;
  shLed2a.Visible := False; shLed2b.Visible := False;
  shLed12c.Visible := False;
  edLeds.Lines.Clear;
  if bLed1Used then begin
    // LED1 being actually used
    shLed1.Visible := True; laLed1.Visible := True;
    shLed1a.Visible := True; shLed1b.Visible := True;
    edLeds.Lines.AddText(Led1Txt);
    if bLed2Used then begin
      // LED2 also actually used
      shLed12c.Visible := True;
      edLeds.Lines.AddText(LineEnding);
    end;
  end;
  if bLed2Used then begin
    // LED2 being actually used
    shLed2.Visible := True; laLed2.Visible := True;
    shLed2a.Visible := True; shLed2b.Visible := True;
    edLeds.Lines.AddText(Led2Txt);
  end;
  if bLed1Used and bLed2used then begin
    // Usage of the 2 LEDs
    edLeds.Height:= 172; edComponents.Top := 382;
    edLeds.Text := StringReplace(edLeds.Text, 'LED1', 'LED' + SUB_1, [rfReplaceAll]);
    edLeds.Text := StringReplace(edLeds.Text, 'LED2', 'LED' + SUB_2, [rfReplaceAll]);
    laLED1.Caption := 'LED' + SUB_1; laLED2.Caption := 'LED' + SUB_2;
    laLED1.Left := 490; laLED2.Left := 490;
  end
  else begin
    // Usage of 1 single LED
    edLeds.Height:= 80; edComponents.Top := 290;
    edLeds.Text := StringReplace(edLeds.Text, 'LED1', 'LED', [rfReplaceAll]);
    edLeds.Text := StringReplace(edLeds.Text, 'LED2', 'LED', [rfReplaceAll]);
    edLeds.Text := 'The ' + edLeds.Text;
    laLED1.Caption := 'LED'; laLED2.Caption := 'LED';
    laLED1.Left := 500; laLED2.Left := 500;
  end;
  // Set initial voltage at the timer pins
  // Supposing +VS = 15 and using -1 for any value
  aTimerPins[1] := 0;  aTimerPins[2] := 15;
  aTimerPins[4] := 15; aTimerPins[5] := -1;
  aTimerPins[6] := 0;  aTimerPins[8] := 15;
  // Timer simulation for these voltages (with turning on/off of the LEDs)
  TimerAndLeds(aTimerPins, shLed1, shLed2);
end;

{ RESET button pushed: Timer simulation for corresponding pin voltages }

procedure TfBistable.imSwitch1Click(Sender: TObject);

begin
  // Move puhbutton down
  imSwitch1.Left := 85;
  shSwitch1a.Left := 104;
  shSwitch1b.Left := 89;
  // Change voltage at pin 0 or 4 (depending on actual circuit)
  if sResetInput = 'reset' then
    aTimerPins[4] := 0
  else
    aTimerPins[6] := 15;
  // Timer simulation for new voltages (with turning on/off of the LEDs)
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  // Start the timer (just to move the pushbutton up again)
  sPushbutton := 'Ta1';
  tiSwitches.Enabled := True;
end;

{ SET button pushed: Timer simulation for corresponding pin voltages }

procedure TfBistable.imSwitch2Click(Sender: TObject);

begin
  // Move puhbutton down
  imSwitch2.Left := 185;
  shSwitch2a.Left := 204;
  shSwitch2b.Left := 189;
  // Change voltage at pin 2
  aTimerPins[2] := 0;
  // Timer simulation for new voltages (with turning on/off of the LEDs)
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  // Start the timer (just to move the pushbutton up again)
  sPushbutton := 'Ta2';
  tiSwitches.Enabled := True;
end;

{ Switch timer routine (to move the pushbutton up after it has been pressed) }

procedure TfBistable.tiSwitchesTimer(Sender: TObject);

begin
  // Ta1 pushbutton
  if sPushbutton = 'Ta1' then begin
    imSwitch1.Left := 75;
    shSwitch1a.Left := 94;
    shSwitch1b.Left := 79;
    // Change voltage at pin 4 or 6
    if sResetInput = 'reset' then
      aTimerPins[4] := 15
    else
      aTimerPins[6] := 0;
  end
  // Ta2 pushbutton
  else begin
    imSwitch2.Left := 175;
    shSwitch2a.Left := 194;
    shSwitch2b.Left := 179;
    // Change voltage at pin 2
    aTimerPins[2] := 15;
  end;
  // Timer simulation for new voltages (with turning on/off of the LEDs)
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  // Stop the timer
  tiSwitches.Enabled := False;
end;

{ Button "Close" pushed: Close simulation window }

procedure TfBistable.btCloseClick(Sender: TObject);

begin
  tiSwitches.Enabled := False;
  Close;
end;

end.

