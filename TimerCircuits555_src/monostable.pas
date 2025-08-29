{************************************************************}
{* Monostable circuit unit for TimerCircuits555 application *}
{************************************************************}

unit monostable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {**************}
  { TfMonoStable }
  {**************}
  TfMonoStable = class(TForm)
    StaticText1, StaticText2: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    Label8, Label9, Label10, Label11, Label12, Label13, Label14, Label16: TLabel;
    edMonostable, edLeds, edComponents: TMemo;
    laC, laR, laLED1, laLED2, laTriggerSw, laResetSw, laResetRVal: TLabel;
    shLed1, shLed1a, shLed1b, shLed2, shLed2a, shLed2b, shLed12c: TShape;
    shSwitch1a, shSwitch1b, shSwitch2a, shSwitch2b: TShape;
    shResetR, shReset1, shReset2, shReset3, shReset4, shReset5, shReset6, shReset7: TShape;
    Shape1, Shape2, Shape3, Shape4, Shape5, Shape6, Shape7, Shape8, Shape9, Shape10: TShape;
    Shape11, Shape12, Shape13, Shape14, Shape15, Shape16, Shape17, Shape18, Shape19: TShape;
    Shape20, Shape21, Shape22, Shape23, Shape25, Shape26, Shape27, Shape30, Shape31: TShape;
    Shape32, Shape33, Shape40, Shape41, Shape42, Shape43, Shape44, Shape45, Shape46, Shape47: TShape;
    imSwitch1, imSwitch2: TImage;
    edC, edR, edT, edTime: TEdit;
    cobUC, cobUR, cobUT: TComboBox;
    btCalc, btClose: TButton;
    tiCapacitor, tiSwitches: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imSwitch1Click(Sender: TObject);
    procedure imSwitch2Click(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure tiCapacitorTimer(Sender: TObject);
    procedure tiSwitchesTimer(Sender: TObject);
    procedure edCChange(Sender: TObject);
    procedure edRChange(Sender: TObject);
    procedure edTChange(Sender: TObject);
    procedure cobURChange(Sender: TObject);
    procedure cobUTChange(Sender: TObject);
  private
    iT, iCobUR, iCobUT: Integer;
    bCalc, bCharge: Boolean;
    rStartTime, rTime: TDateTime;
    sPushbutton: string;
    aTimerPins: TTimerPins;
  public
    bLed1Used, bLed2Used, bResetUsed, bWarnings: Boolean;
    sCalc: string;
    pStdResistors: TPStdResistors;
  end;

var
  fMonoStable: TfMonoStable;

implementation

{$R *.lfm}

{**************}
{ TfMonoStable }
{**************}

{ Application start: Initialisation }

procedure TfMonoStable.FormCreate(Sender: TObject);

begin
  // Default circuit values
  bResetUsed := False; sCalc := 'time period';
  bLed1Used := False; bLed2Used := True;
  bWarnings := True;
end;

{ Window show-up: Display and initialize actual circuit and display actual circuit info }

procedure TfMonoStable.FormActivate(Sender: TObject);

const
  Led1Txt = 'LED1 goes on, if the timer output is logic low: LED1 = on indicates the stable (default) state, LED1 = out indicates the unstable state (triggered time period).';
  Led2Txt = 'LED2 goes on, if the timer output is logic high: LED2 = on indicates the unstable state (triggered time period), LED2 = out indicates the stable (default) state.';

begin
  bCalc := False; bCharge := False;
  if bResetUsed then begin
    // Circuit with RESET switch
    shReset1.Height := 50;
    shReset2.Visible := True; shReset3.Visible := True; shReset4.Visible := True;
    shReset5.Visible := True; shReset6.Visible := True; shReset7.Visible := True;
    shResetR.Visible := True; laResetRVal.Visible := True;
    imSwitch2.Visible := True; laResetSw.Visible := True;
    shSwitch2a.Visible := True; shSwitch2b.Visible := True;
  end
  else begin
    // Circuit without RESET switch
    shReset1.Height := 258;
    shReset2.Visible := False; shReset3.Visible := False; shReset4.Visible := False;
    shReset5.Visible := False; shReset6.Visible := False; shReset7.Visible := False;
    shResetR.Visible := False; laResetRVal.Visible := False;
    imSwitch2.Visible := False; laResetSw.Visible := False;
    shSwitch2a.Visible := False; shSwitch2b.Visible := False;
  end;
  // Actual circuit output (LEDs)
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
      // LED2 also being actually used
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
    edLeds.Height:= 208; edComponents.Top := 418;
    edLeds.Text := StringReplace(edLeds.Text, 'LED1', 'LED' + SUB_1, [rfReplaceAll]);
    edLeds.Text := StringReplace(edLeds.Text, 'LED2', 'LED' + SUB_2, [rfReplaceAll]);
    laLED1.Caption := 'LED' + SUB_1; laLED2.Caption := 'LED' + SUB_2;
    laLED1.Left := 530; laLED2.Left := 530;
  end
  else begin
    // Usage of 1 single LED
    edLeds.Height:= 117; edComponents.Top := 326;
    edLeds.Text := StringReplace(edLeds.Text, 'LED1', 'LED', [rfReplaceAll]);
    edLeds.Text := StringReplace(edLeds.Text, 'LED2', 'LED', [rfReplaceAll]);
    edLeds.Text := 'The ' + edLeds.Text;
    laLED1.Caption := 'LED'; laLED2.Caption := 'LED';
    laLED1.Left := 540; laLED2.Left := 540;
  end;
  // User input fields settings (depending on actual calculation)
  edC.Text := ''; edR.Text := ''; edT.Text := '';
  cobUC.ItemIndex := 0; cobUR.ItemIndex := 0; cobUT.ItemIndex := 1;
  iCobUR := 0; iCobUT := 1;
  if sCalc = 'resistance' then begin
    // Calculation of resistance
    edR.ReadOnly := True; edT.ReadOnly := False;
    edR.TabStop := False; edT.TabStop := True;
    edR.Color := clCream; edT.Color := clDefault;
  end
  else begin
    // Calculation of time period
    edR.ReadOnly := False; edT.ReadOnly := True;
    edR.TabStop := True; edT.TabStop := False;
    edR.Color := clDefault; edT.Color := clCream;
  end;
  // Set initial voltage at the timer pins
  // Supposing +VS = 15 and using -1 for any value
  aTimerPins[1] := 0;  aTimerPins[2] := 15;
  aTimerPins[4] := 15; aTimerPins[5] := -1;
  aTimerPins[6] := 0;  aTimerPins[8] := 15;
  // Timer simulation for these voltages (with turning on/off of the LEDs)
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  edC.SetFocus;
end;

{ Button "Calculation" pushed: Check user input and calculate the time period resp. the resistance }

procedure TfMonoStable.btCalcClick(Sender: TObject);

var
  C, R, R0, T: Real;
  Mess: string;

begin
  Mess := ''; edTime.Text := '';
  C := 0; R := 0; T := 0;
  // Read capacitance
  if edC.Text <> '' then
    C := StrToFloat(edC.Text);
  if C <= 0 then begin
    Mess := 'Capacitance C must be greater than 0';
    edC.SetFocus;
  end
  else begin
    case cobUC.ItemIndex of
      0: C *= 1E-6;
      1: C *= 1E-9;
    end;
  end;
  if Mess = '' then begin
    // If resistance is to be calculated, read time period
    if sCalc = 'resistance' then begin
      if edT.Text <> '' then
        T := StrToFloat(edT.Text);
      if T > 0 then begin
        // Valid time period (except if resultant resistance would be outside valid limits)
        case cobUT.ItemIndex of
          0: T *= 1E-3;
          2: T *= 60;
        end;
        // Check if resultant resistance is in valid limits
        // Adapt time period in order to get a standard resistor value
        R0 := T / (1.1 * C);
        if (R0 < 100) or (R0 > 10e+6) then begin
          // Resistance would be outside valid limits
          Mess := 'Resistance R should be between 100Ω and 10MΩ. Actually would be ';
          if R0 >= 1E+6 then
            Mess += RFormat(R0 * 1E-6, 3) + 'MΩ!'
          else if R0 >= 1E+3 then
            Mess += RFormat(R0 * 1E-3, 3) + 'kΩ!'
          else
            Mess += RFormat(R0, 3) + 'Ω!';
          edC.SetFocus;
        end
        else begin
          // Resistance will be in valid limits
          R := GetStandardResistor(pStdResistors, R0, C, T);
          if R <> R0 then begin
            // Adapt time period for standard resistor value
            if bWarnings then
              MessageDlg('Input adaption', 'Time period has been adapted for standard resistor value.', mtWarning, [mbOK], 0);
            T := 1.1 * R * C;
            case cobUT.ItemIndex of
              0: edT.Text := RFormat(T * 1E+3, 3);
              1: edT.Text := RFormat(T, 3);
              2: edT.Text := RFormat(T / 60, 3);
            end;
          end;
          if R >= 1E+6 then begin
            edR.Text := RFormat(R * 1E-6, 3);
            cobUR.ItemIndex := 1; iCobUR := 1;
          end
          else begin
            edR.Text := RFormat(R * 1E-3, 3);
            cobUR.ItemIndex := 0; iCobUR := 0;
          end;
        end;
      end
      else begin
        // Invalid time period
        Mess := 'Time period T must be greater than 0!';
        edT.SetFocus;
      end;
    end
    // If time period is to be calculated, read resistance
    else begin
      if edR.Text <> '' then
        R := StrToFloat(edR.Text);
      if R > 0 then begin
        // Valid resistance, but has to be within valid limits and be standard resistor value
        case cobUR.ItemIndex of
          0: R *= 1E+3;
          1: R *= 1E+6;
        end;
        // Check if resistance is within valid limits and is standard resistor value
        if (R < 100) or (R > 10e+6) then begin
          Mess := 'Resistance R should be between 100Ω and 10MΩ!';
          edR.SetFocus;
        end
        else if not IsStandardResistor(pStdResistors, R) then begin
          Mess := 'Resistance R should be a standard resistor!';
          edR.SetFocus;
        end
        else begin
          // Calculate time period
          T := 1.1 * R * C;
          if T >= 1 then begin
            edT.Text := RFormat(T, 3);
            cobUT.ItemIndex := 1; iCobUT := 1;
          end
          else begin
            edT.Text := RFormat(T * 1E+3, 3);
            cobUT.ItemIndex := 0; iCobUT := 0;
          end;
        end;
      end
      else begin
        // Invalid resistance
        Mess := 'Resistance R must be greater than 0!';
        edR.SetFocus;
      end;
    end;
  end;
  // User input is ok (calcuation has been done)
  if Mess = '' then begin
    iT := Round(1000 * T);                                                     // time period in ms
    bCalc := True;
  end
  // User input is invalid (calcuation has to be redone before the simulation becomes active)
  else begin
    MessageDlg('Input error', Mess, mtError, [mbOK], 0);
    bCalc := False;
  end;
end;

{ SET switch pushed }

procedure TfMonoStable.imSwitch1Click(Sender: TObject);

// In reality, the charge of the capacitor begins when this switch goes done
// (i.e. here) and not, as actually implemented (to avoid possible problems
// due to the two timers) when the switch is up again (switches timer routine)

begin
  if bCalc then begin
    // Move switch down
    imSwitch1.Left := 85;
    shSwitch1a.Left := 104;
    shSwitch1b.Left := 89;
    aTimerPins[2] := 0;
    TimerAndLeds(aTimerPins, shLed1, shLed2);
    // Start the switches timer: This will start the capacitor charge
    sPushbutton := 'Ta1';
    tiSwitches.Enabled := True;
  end
  else
    MessageDlg('Input error', 'Circuit calculation has not yet been done!', mtError, [mbOK], 0);
end;

{ RESET switch pushed }

procedure TfMonoStable.imSwitch2Click(Sender: TObject);

begin
  // Move switch down
  imSwitch2.Left := 483;
  shSwitch2a.Left := 502;
  shSwitch2b.Left := 487;
  aTimerPins[4] := 0;
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  // Disable the capacitor timer: This will end the capacitor charge (that becomes 0V if RESET is pressed)
  bCharge := False; tiCapacitor.Enabled := False;
  // Start the switches timer (to move up the switch)
  sPushbutton := 'Ta2';
  tiSwitches.Enabled := True;
end;

{ Switches timer }

procedure TfMonoStable.tiSwitchesTimer(Sender: TObject);

// For both switches: Move up of the switch
// For SET switch: Start capacitor timer (circuit unstable state); as said above, this
// is not really correct as the charge of the capacitor begins when the switch goes down

begin
  // Move switch up
  if sPushbutton = 'Ta1' then begin
    // SET switch
    imSwitch1.Left := 75;
    shSwitch1a.Left := 94;
    shSwitch1b.Left := 79;
    aTimerPins[2] := 15;
  end
  else begin
    // RESET switch
    imSwitch2.Left := 473;
    shSwitch2a.Left := 492;
    shSwitch2b.Left := 477;
    aTimerPins[4] := 15;
  end;
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  // Disable the timer
  tiSwitches.Enabled := False;
  if sPushbutton = 'Ta1' then begin
    // Start the capacitor timer = begin of the circuit's unstable state
    // Don't do this if the capacitor already has started charging!
    if not bCharge then begin
      bCharge := True;
      rStartTime := TimeStampToMSecs (DateTimeToTimeStamp (Now));              // actual time
      rTime := rStartTime + iT;                                                // time, when the capacitor charge is such that the circuit switches back to the stable state
      edTime.Text := '';
      if not tiCapacitor.Enabled then
        tiCapacitor.Enabled := True;
    end;
  end;
end;

{ Capacitor timer }

procedure TfMonoStable.tiCapacitorTimer(Sender: TObject);

// The implementation of the capacitor charge is not a "translation" of the electronics reality to program code
// Instead the routine considers the calculated time period and if this time is over, switches the circuit back to the stable state

var
  T, M, S, MS: Real;
  ST: string;

begin
  // Do only, if the capacitor is actually charging
  if bCharge then begin
    ST := '';
    T := TimeStampToMSecs(DateTimeToTimeStamp(Now)) - rStartTime;              // actual charging time
    S := Int(T / 1000); MS := T - S * 1000;
    M := Int(S / 60); S := S - M * 60;
    MS /= 1000;
    MS := Int(100 * MS);
    if M < 10 then
      ST := '0';
    ST += FloatToStr(M) + ':';
    if S < 10 then
      ST += '0';
    ST += FloatToStr(S) + ',' + FloatToStr(MS);
    if Length(ST) = 7 then
      ST += '0';
    edTime.Text := ST;
    // If the time period is over, switch the circuit back to the stable state
    if TimeStampToMSecs (DateTimeToTimeStamp (Now)) >= rTime then begin
      // Capacitor charge is such that pin 6 switches the 555 to logic low output (15V used for simplicity)
      aTimerPins[6] := 15;
      TimerAndLeds(aTimerPins, shLed1, shLed2);
      // With logic low output, the capacitor is discharged and pin 6 becomes 0V
      aTimerPins[6] := 0;
      TimerAndLeds(aTimerPins, shLed1, shLed2);
      bCharge := False;                                                        // capacitor charging phase is over
    end;
  end;
end;

{ Button "Close": Close the simulation wondow }

procedure TfMonoStable.btCloseClick(Sender: TObject);

begin
  tiSwitches.Enabled := False;
  tiCapacitor.Enabled := False;
  Close;
end;

{ Change of user input: Disable simulation until new calculation has been done }

procedure TfMonoStable.edCChange(Sender: TObject);

begin
  bCalc := False;
end;

procedure TfMonoStable.edRChange(Sender: TObject);

begin
  if sCalc = 'time period' then
    bCalc := False;
end;

procedure TfMonoStable.edTChange(Sender: TObject);

begin
  if sCalc = 'resistance' then
    bCalc := False;
end;

{ Disable user changes of calculated circuit values unit }

procedure TfMonoStable.cobURChange(Sender: TObject);

begin
  if sCalc = 'resistance' then
    cobUR.ItemIndex := iCobUR                                                  // set back to old unit
  else
    iCobUR := cobUR.ItemIndex;                                                 // save actual unit
end;

procedure TfMonoStable.cobUTChange(Sender: TObject);

begin
  if sCalc = 'time period' then
    cobUT.ItemIndex := iCobUT
  else
    iCobUT := cobUT.ItemIndex;
end;

end.

