{*********************************************************}
{* Astable circuit unit for TimerCircuits555 application *}
{*********************************************************}

unit astable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, common;

type
  {***********}
  { TfAstable }
  {***********}
  TfAstable = class(TForm)
    StaticText1, StaticText2: TStaticText;
    edAstable, edLeds, edComponents: TMemo;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9: TLabel;
    Label10, Label11, Label12, Label13, Label14, Label15, Label16: TLabel;
    Label17, Label18, Label19, Label20, Label21, Label22, Label23, Label24: TShape;
    laC, laR1, laR2, laLED1, laLED2, laRes1, laRes2, laUT: TLabel;
    edC, edR1, edR2, edT, edF, edTm, edTs: TEdit;
    edDutyCycle, edTimeMark, edTimeSpace: TEdit;
    cobUC, cobUR1, cobUR2, cobUTm, cobUTs: TComboBox;
    imDiode: TImage;
    shLed1, shLed1a, shLed1b, shLed2, shLed2a, shLed2b, shLed12c: TShape;
    shDiode1, shDiode2, shDiode3, shDiode4: TShape;
    Shape1, Shape4, Shape5, Shape6, Shape7, Shape9, Shape10, Shape11: TShape;
    Shape12, Shape13, Shape14, Shape15, Shape16, Shape17, Shape18: TShape;
    Shape20, Shape21, Shape22, Shape23, Shape30, Shape32, Shape33: TShape;
    Shape34, Shape40, Shape41, Shape42, Shape45, Shape46, Shape47: TShape;
    btCalc: TButton;
    btStart: TButton;
    btClose: TButton;
    tiCapacitor: TTimer;
    procedure edCChange(Sender: TObject);
    procedure edR1Change(Sender: TObject);
    procedure edR2Change(Sender: TObject);
    procedure edTmChange(Sender: TObject);
    procedure edTsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure cobUR1Change(Sender: TObject);
    procedure cobUR2Change(Sender: TObject);
    procedure cobUTmChange(Sender: TObject);
    procedure cobUTsChange(Sender: TObject);
    procedure tiCapacitorTimer(Sender: TObject);
  private
    iTm, iTs, iCobUR1, iCobUR2, iCobUTm, iCobUTs: Integer;
    sCapacitor: string;
    bCalc: Boolean;
    rChargeStartTime, rChargeTime, rDischargeStartTime, rDischargeTime: TDateTime;
    aTimerPins: TTimerPins;
  public
    bLed1Used, bLed2Used, bWarnings: Boolean;
    sCircuit, sCalc: string;
    pStdResistors: TPStdResistors;
  end;

var
  fAstable: TfAstable;

implementation

{$R *.lfm}

{***********}
{ TfAstable }
{***********}

{ Application start: Initialisation }

procedure TfAstable.FormCreate(Sender: TObject);

begin
  // Default circuit values
  sCalc := 'time period'; sCircuit := 'standard';
  bLed1Used := False; bLed2Used := True;
  bWarnings := True;
  // Apply subscripts
  laR1.Caption := 'R' + SUB_1; laR2.Caption := 'R' + SUB_2;
  laRes1.Caption := 'R' + SUB_1; laRes2.Caption := 'R' + SUB_2;
  laLED1.Caption := 'LED' + SUB_1; laLED2.Caption := 'LED' + SUB_2;
  edAstable.Text := StringReplace(edAstable.Text, 'R1', 'R' + SUB_1, [rfReplaceAll]);
  edAstable.Text := StringReplace(edAstable.Text, 'R2', 'R' + SUB_2, [rfReplaceAll]);
  edComponents.Text := StringReplace(edComponents.Text, 'R1', 'R' + SUB_1, []);
  edComponents.Text := StringReplace(edComponents.Text, 'R2', 'R' + SUB_2, []);
end;

{ Window show-up: Display and initialize actual circuit and display actual circuit info }

procedure TfAstable.FormActivate(Sender: TObject);

const
  Led1Txt = 'LED1 goes on, if the timer output is logic low: LED1 = on indicates the space time period, LED1 = out the mark time period.';
  Led2Txt = 'LED2 goes on, if the timer output is logic high: LED2 = on indicates the mark time period, LED2 = out the space time period.';

begin
  bCalc := False; sCapacitor := '';
  if sCircuit = 'standard' then begin
    // Standard circuit (no diode)
    imDiode.Visible := False;
    shDiode1.Visible := False; shDiode2.Visible := False;
    shDiode3.Left := 110; shDiode4.Left := 110;
    shDiode3.Width := 150; shDiode4.Width := 150;
  end
  else begin
    // Circuit with diode parallel to R2
    imDiode.Visible := True;
    shDiode1.Visible := True; shDiode2.Visible := True;
    shDiode3.Left := 34; shDiode4.Left := 34;
    shDiode3.Width := 226; shDiode4.Width := 226;
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
    edLeds.Height:= 102; edComponents.Top := 305;
    edLeds.Text := StringReplace(edLeds.Text, 'LED1', 'LED' + SUB_1, [rfReplaceAll]);
    edLeds.Text := StringReplace(edLeds.Text, 'LED2', 'LED' + SUB_2, [rfReplaceAll]);
    laLED1.Caption := 'LED' + SUB_1; laLED2.Caption := 'LED' + SUB_2;
  end
  else begin
    // Usage of 1 single LED
    edLeds.Height:= 48; edComponents.Top := 250;
    edLeds.Text := StringReplace(edLeds.Text, 'LED1', 'LED', [rfReplaceAll]);
    edLeds.Text := StringReplace(edLeds.Text, 'LED2', 'LED', [rfReplaceAll]);
    edLeds.Text := 'The ' + edLeds.Text;
    laLED1.Caption := 'LED'; laLED2.Caption := 'LED';
  end;
    // User input fields settings (depending on actual calculation)
  edC.Text := ''; edR1.Text := ''; edR2.Text := '';
  edTm.Text := ''; edTs.Text := ''; edT.Text := '';
  edT.Text := ''; edF.Text := ''; edDutyCycle.Text := '';
  cobUC.ItemIndex := 0; cobUR1.ItemIndex := 0; cobUR2.ItemIndex := 0; cobUTm.ItemIndex := 1; cobUTs.ItemIndex := 1;
  iCobUR1 := 0; iCobUR2 := 0; iCobUTm := 1; iCobUTs := 1;
  if sCalc = 'resistance' then begin
    // Calculation of resistances
    edR1.ReadOnly := True; edR2.ReadOnly := True; edTm.ReadOnly := False; edTs.ReadOnly := False;
    edR1.TabStop := False; edR2.TabStop := False; edTm.TabStop := True; edTs.TabStop := True;
    edR1.Color := clCream; edR2.Color := clCream; edTm.Color := clDefault; edTs.Color := clDefault;
  end
  else begin
    // Calculation of mark ans space time
    edR1.ReadOnly := False; edR2.ReadOnly := False; edTm.ReadOnly := True; edTs.ReadOnly := True;
    edR1.TabStop := True; edR2.TabStop := True; edTm.TabStop := False; edTs.TabStop := False;
    edR1.Color := clDefault; edR2.Color := clDefault; edTm.Color := clCream; edTs.Color := clCream;
  end;
  // Set initial voltage at the timer pins
  // Supposing +VS = 15 and using -1 for any value
  aTimerPins[1] := 0;  aTimerPins[2] := 0;
  aTimerPins[4] := 15; aTimerPins[5] := -1;
  aTimerPins[6] := 0;  aTimerPins[8] := 15;
  // Timer simulation for these voltages (with turning on/off of the LEDs)
  TimerAndLeds(aTimerPins, shLed1, shLed2);
  edC.SetFocus;
end;

{ Button "Calculation" pushed: Check user input and calculate the mark and space time resp. the resistances }

procedure TfAstable.btCalcClick(Sender: TObject);

var
  C, R1, R2, R10, R20, Tm, Ts, T, F, DutyCycle: Real;
  Mess: string;

begin
  Mess := ''; edTimeMark.Text := ''; edTimeSpace.Text := '';
  C := 0; R10 := 0; R20 := 0; Tm := 0; Ts := 0; T := 0; F := 0;
  // Read capacitance
  if edC.Text <> '' then
    C := StrToFloat(edC.Text);
  if C <= 0 then begin
    Mess := 'Capacitor C must be greater than 0';
    edC.SetFocus;
  end
  else begin
    case cobUC.ItemIndex of
      0: C *= 1E-6;
      1: C *= 1E-9;
    end;
  end;
  if Mess = '' then begin
    // If resistances are to be calculated, read mark and space time
    if sCalc = 'resistance' then begin
      if edTm.Text <> '' then
        Tm := StrToFloat(edTm.Text);
      if edTs.Text <> '' then
        Ts := StrToFloat(edTs.Text);
      if (Tm > 0) and (Ts > 0) then begin
        // In the case of a standard circuit (no diode), mark time can't be less than space time
        if (sCircuit = 'standard') and (Tm <= Ts) then begin
          Mess := 'These mark and space times are not possible with this circuit!';
          edTm.SetFocus;
        end
        else begin
          case cobUTm.ItemIndex of
            0: Tm *= 1E-3;
            2: Tm *= 60;
          end;
          case cobUTs.ItemIndex of
            0: Ts *= 1E-3;
            2: Ts *= 60;
          end;
          // Check if resultant resistances are in valid limits
          // Adapt time periods in order to get standard resistor values
          R20 := Ts / (0.7 * C);
          if sCircuit = 'standard' then
            R10 := (Tm - Ts) / (0.7 * C)
          else
            R10 := Tm / (0.7 * C);
          if (R10 < 100) or (R10 > 10e+6) then begin
            // Resistance R1 would be outside valid limits
            Mess := 'Resistance R1 should be between 100Ω and 10MΩ. Actually would be ';
            if R10 >= 1E+6 then
              Mess += RFormat(R10 * 1E-6, 3) + 'MΩ!'
            else if R10 >= 1E+3 then
              Mess += RFormat(R10 * 1E-3, 3) + 'kΩ!'
            else
              Mess += RFormat(R10, 3) + 'Ω!';
            edC.SetFocus;
          end
          else if (R20 < 100) or (R20 > 10e+6) then begin
            // Resistance R2 would be outside valid limits
            Mess := 'Resistance R2 should be between 100Ω and 10MΩ. Actually would be ';
            if R20 >= 1E+6 then
              Mess += RFormat(R20 * 1E-6, 3) + 'MΩ!'
            else if R20 >= 1E+3 then
              Mess += RFormat(R20 * 1E-3, 3) + 'kΩ!'
            else
              Mess += RFormat(R20, 3) + 'Ω!';
            edC.SetFocus;
          end
          else begin
            // Both resistance will be in valid limits
            R1 := GetStandardResistor(pStdResistors, R10, C, T);
            R2 := GetStandardResistor(pStdResistors, R20, C, T);
            if (R1 <> R10) or (R2 <> R20) then begin
              // Adapt time periods for standard resistor values
              if bWarnings then
                MessageDlg('Input adaption', 'Time periods have been adapted for standard resistor value.', mtWarning, [mbOK], 0);
              if sCircuit = 'standard' then
                Tm := 0.7 * (R1 + R2) * C
              else
                Tm := 0.7 * R1 * C;
              Ts := 0.7 * R2 * C;
              case cobUTm.ItemIndex of
                0: edTm.Text := RFormat(Tm * 1E+3, 3);
                1: edTm.Text := RFormat(Tm, 3);
                2: edTm.Text := RFormat(Tm / 60, 3);
              end;
              case cobUTs.ItemIndex of
                0: edTs.Text := RFormat(Ts * 1E+3, 3);
                1: edTs.Text := RFormat(Ts, 3);
                2: edTs.Text := RFormat(Ts / 60, 3);
              end;
            end;
            // Calculate frequency and duty cycle
            T := Tm + Ts;
            F := 1 / T;
            DutyCycle := Tm / T;
            // Fill in form fields
            if R1 >= 1E+6 then begin
              edR1.Text := RFormat(R1 * 1E-6, 3);
              cobUR1.ItemIndex := 1; iCobUR1 := 1;
            end
            else begin
              edR1.Text := RFormat(R1 * 1E-3, 3);
              cobUR1.ItemIndex := 0; iCobUR1 := 0;
            end;
            if R2 >= 1E+6 then begin
              edR2.Text := RFormat(R2 * 1E-6, 3);
              cobUR2.ItemIndex := 1; iCobUR2 := 1;
            end
            else begin
              edR2.Text := RFormat(R2 * 1E-3, 3);
              cobUR2.ItemIndex := 0; iCobUR2 := 0;
            end;
            if T >= 1 then begin
              edT.Text := RFormat(T, 3);
              laUT.Caption := 's';
            end
            else begin
              edT.Text := RFormat(T * 1E+3, 3);
              laUT.Caption := 'ms';
            end;
            if F > 10 then
              edF.Text := RFormat(F, 0)
            else
              edF.Text := RFormat(F, 3);
            edDutyCycle.Text := RFormat(100 * DutyCycle, 2);
          end;
        end;
      end
      else begin
        // Invalid mark or space time
        if Tm < 0 then begin
          Mess := 'Mark time Tm must be greater than 0!';
          edTm.SetFocus;
        end
        else begin
          Mess := 'Space time Ts must be greater than 0!';
          edTs.SetFocus;
        end;
      end;
    end
    // If mark and space time are to be calculated, read resistances
    else begin
      if edR1.Text <> '' then
        R1 := StrToFloat(edR1.Text);
      if edR2.Text <> '' then
        R2 := StrToFloat(edR2.Text);
      if (R1 > 0) and (R2 > 0) then begin
        // Valid resistances, but have to be within valid limits and be standard resistor values
        case cobUR1.ItemIndex of
          0: R1 *= 1E+3;
          1: R1 *= 1E+6;
        end;
        case cobUR2.ItemIndex of
          0: R2 *= 1E+3;
          1: R2 *= 1E+6;
        end;
        // Check if resistances are within valid limits and are standard resistor values
        if (R1 < 100) or (R1 > 10e+6) then begin
          Mess := 'Resistance R1 should be between 100Ω and 10MΩ!';
          edR1.SetFocus;
        end
        else if not IsStandardResistor(pStdResistors, R1) then begin
          Mess := 'Resistance R1 should be a standard resistor!';
          edR1.SetFocus;
        end
        else if (R2 < 100) or (R2 > 10e+6) then begin
          Mess := 'Resistance R2 should be between 100Ω and 10MΩ!';
          edR2.SetFocus;
        end
        else if not IsStandardResistor(pStdResistors, R2) then begin
          Mess := 'Resistance R2 should be a standard resistor!';
          edR2.SetFocus;
        end
        else begin
          // Calculate mark and space time
          if sCircuit = 'standard' then
            Tm := 0.7 * (R1 + R2) * C
          else
            Tm := 0.7 * R1 * C;
          Ts := 0.7 * R2 * C;
          // Calculate frequency and duty cycle
          T := Tm + Ts;
          F := 1 / T;
          DutyCycle := Tm / T;
          // Fill in form fields
          if Tm >= 1 then begin
            edTm.Text := RFormat(Tm, 3);
            cobUTm.ItemIndex := 1; iCobUTm := 1;
          end
          else begin
            edTm.Text := RFormat(Tm * 1E+3, 3);
            cobUTm.ItemIndex := 0; iCobUTm := 0;
          end;
          if Ts >= 1 then begin
            edTs.Text := RFormat(Ts, 3);
            cobUTs.ItemIndex := 1; iCobUTs := 1;
          end
          else begin
            edTs.Text := RFormat(Ts * 1E+3, 3);
            cobUTs.ItemIndex := 0; iCobUTs := 0;
          end;
          if T >= 1 then begin
            edT.Text := RFormat(T, 3);
            laUT.Caption := 's';
          end
          else begin
            edT.Text := RFormat(T * 1E+3, 3);
            laUT.Caption := 'ms';
          end;
          if F > 10 then
            edF.Text := RFormat(F, 0)
          else
            edF.Text := RFormat(F, 3);
          edDutyCycle.Text := RFormat(100 * DutyCycle, 2);
        end;
      end
      else begin
        // Invalid resistance
        if R1 < 0 then begin
          Mess := 'Resistance R1 must be greater than 0!';
          edR1.SetFocus;
        end
        else begin
          Mess := 'Resistance R2 must be greater than 0!';
          edR2.SetFocus;
        end;
      end;
    end;
  end;
  // User input is ok (calcuation has been done)
  if Mess = '' then begin
    iTm := Round(1000 * Tm); iTs := Round(1000 * Ts);                          // mark and space time in ms
    bCalc := True;
  end
  // User input is invalid (calcuation has to be redone before the simulation becomes active)
  else begin
    MessageDlg('Input error', Mess, mtError, [mbOK], 0);
    bCalc := False;
  end;
end;

{ Button "Start/Stop" pushed: Start resp. stop the astable circuit simulation }

procedure TfAstable.btStartClick(Sender: TObject);

begin
  edTimeMark.Text := ''; edTimeSpace.Text := '';
  // Button "Start" pushed: Start the simulation
  if btStart.Caption = 'Start' then begin
    // Do only, if calculation has been done for actual circuit values
    if bCalc then begin
      // Simulation startup values
      sCapacitor := 'charge';                                                  // capacitor is charging
      rChargeStartTime := TimeStampToMSecs (DateTimeToTimeStamp (Now));        // time when capacitor started charging
      rChargeTime := rChargeStartTime + iTm;                                   // time when capacitor charge will be such, that the circuit state changes
      // Next button push will be to stop the simulation
      btStart.Caption := 'Stop';
      // Start the capacitor timer
      tiCapacitor.Enabled := True;
    end
    else
      MessageDlg('Input error', 'Circuit calculation has not yet been done!', mtError, [mbOK], 0);
  end
  // Button "Stop" pushed: Stop the simulation
  else begin
    // Stop the timer
    tiCapacitor.Enabled := False;
    // Capacitor is no more charging (is as if current is dropped)
    sCapacitor := '';
    // Next button push will be to start the simulation
    btStart.Caption := 'Start';
  end;
end;

{ Capacitor timer }

procedure TfAstable.tiCapacitorTimer(Sender: TObject);

// The implementation of the capacitor charge is not a "translation" of the electronics reality to program code
// Instead the routine considers the calculated time periods and if the mark resp. space time is over, switches the circuit's state

var
  T, M, S, MS: Real;
  ST: string;

begin
  // Capacitor is actually charging
  if sCapacitor = 'charge' then begin
    ST := '';
    T := TimeStampToMSecs(DateTimeToTimeStamp(Now)) - rChargeStartTime;        // actual charging time
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
    edTimeMark.Text := ST;
    // If the mark time is over, switch the circuit's state
    if TimeStampToMSecs (DateTimeToTimeStamp (Now)) >= rChargeTime then begin
      // Capacitor charge is such that pin 6 switches the 555 to logic low output (15V used for simplicity)
      aTimerPins[6] := 15; aTimerPins[2] := 15;
      TimerAndLeds(aTimerPins, shLed1, shLed2);
      // With logic low at pin 7, the capacitor will now start discharging
      rDischargeStartTime := TimeStampToMSecs (DateTimeToTimeStamp (Now));
      rDischargeTime := rDischargeStartTime + iTs;
      sCapacitor := 'discharge';
    end;
  end
  // Capacitor is actually discharging
  else if sCapacitor = 'discharge' then begin
    ST := '';
    T := TimeStampToMSecs(DateTimeToTimeStamp(Now)) - rDischargeStartTime;
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
    edTimeSpace.Text := ST;
    // If the space time is over, switch the circuit's state
    if TimeStampToMSecs (DateTimeToTimeStamp (Now)) >= rDisChargeTime then begin
      // Capacitor charge is such that pin 2 switches the 555 to logic high output (0V used for simplicity)
      aTimerPins[2] := 0; aTimerPins[6] := 0;
      TimerAndLeds(aTimerPins, shLed1, shLed2);
      // With logic high at pin 7, the capacitor will now start charging again
      rChargeStartTime := TimeStampToMSecs (DateTimeToTimeStamp (Now));
      rChargeTime := rChargeStartTime + iTm;
      sCapacitor := 'charge';
    end;
  end
end;

{ Button "Close" pushed: Close the simulation window }

procedure TfAstable.btCloseClick(Sender: TObject);

begin
  tiCapacitor.Enabled := False; sCapacitor := '';
  Close;
end;

{ Change of user input: Disable simulation until new calculation has been done }

procedure TfAstable.edCChange(Sender: TObject);

begin
  bCalc := False;
end;

procedure TfAstable.edR1Change(Sender: TObject);

begin
  if sCalc = 'time period' then
    bCalc := False;
end;

procedure TfAstable.edR2Change(Sender: TObject);

begin
  if sCalc = 'time period' then
    bCalc := False;
end;

procedure TfAstable.edTmChange(Sender: TObject);

begin
  if sCalc = 'resistance' then
    bCalc := False;
end;

procedure TfAstable.edTsChange(Sender: TObject);

begin
  if sCalc = 'resistance' then
    bCalc := False;
end;

procedure TfAstable.cobUR1Change(Sender: TObject);

begin
  if sCalc = 'resistance' then
    cobUR1.ItemIndex := iCobUR1
  else
    iCobUR1 := cobUR1.ItemIndex;
end;

{ Disable user changes of calculated circuit values unit }

procedure TfAstable.cobUR2Change(Sender: TObject);

begin
  if sCalc = 'resistance' then
    cobUR2.ItemIndex := iCobUR2                                                // set back to old unit
  else
    iCobUR2 := cobUR2.ItemIndex;                                               // save actual unit
end;

procedure TfAstable.cobUTmChange(Sender: TObject);

begin
  if sCalc = 'time period' then
    cobUTm.ItemIndex := iCobUTm
  else
    iCobUTm := cobUTm.ItemIndex;
end;

procedure TfAstable.cobUTsChange(Sender: TObject);

begin
  if sCalc = 'time period' then
    cobUTs.ItemIndex := iCobUTs
  else
    iCobUTs := cobUTs.ItemIndex;
end;

end.

