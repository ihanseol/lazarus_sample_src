{***********************************************************}
{* Astable multivibrator unit for OpAmplifiers application *}
{***********************************************************}

unit opamp6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type
  {**********}
  { TfOpAmp6 }
  {**********}
  TfOpAmp6 = class(TForm)
    mMenu: TMainMenu;
    mWindow, mWindowClose: TMenuItem;
    Shape1: TShape;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9: TLabel;
    Label10, Label11, Label12, Label13, Label14, Label15: TLabel;
    Label16, Label17, Label18, Label19, Label20: TLabel;
    Shape2, Shape5, Shape6, Shape7: TShape;
    imAmp, imLeds: TImage;
    shLEDGreen, shLEDRed: TShape;
    edVOut: TEdit;
    edR1, edR2, edRf, edC: TEdit;
    edVA, edVB, edVS: TEdit;
    edT0, edT, edF: TEdit;
    btStart: TButton;
    tiOpAmp: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure mWindowCloseClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tiOpAmpTimer(Sender: TObject);
  private
    rVSatPlus, rVSatMinus, rR1, rR2, rRf, rC, rV1, rV2, rVA, rVB, rT: Real;
  public
    rVCC: Real;
  end;

var
  fOpAmp6: TfOpAmp6;

implementation

{$R *.lfm}

{**********}
{ TfOpAmp6 }
{**********}

{ Form show-up: Initialisation }

procedure TfOpAmp6.FormActivate(Sender: TObject);

begin
  rVSatPlus := rVCC - 1; rVSatMinus := -rVCC + 1;
  edVOut.Text := '';
  edR1.Enabled := True; edR2.Enabled := True; edRf.Enabled := True; edC.Enabled := True;
  edR1.Text := ''; edR2.Text := ''; edRf.Text := ''; edC.Text := '';
  edVA.Text := ''; edVB.Text := ''; edVS.Text := '';
  edT0.Text := ''; edT.Text := ''; edF.Text := '';
  shLEDGreen.Brush.Style := bsDiagCross; shLEDRed.Brush.Style := bsDiagCross;
  btStart.Caption := 'Start';
end;

{ Menu item "Window > Close": Close astable multivibrator circuit window }

procedure TfOpAmp6.mWindowCloseClick(Sender: TObject);

begin
  Close;
end;

{ Button "Start/Stop": Start/stop astable multivibrator simulation }

procedure TfOpAmp6.btStartClick(Sender: TObject);

var
  T0, VS: Real;
  Mess: string;

begin
  // Button "Start": Get user parameters and start simulation (do calculations)
  if btStart.Caption = 'Start' then begin
    Mess := '';
    if (edR1.Text = '') or (edR2.Text = '') or (edRf.Text = '') or (edC.Text = '') then begin
      if edR1.Text = '' then begin
        Mess := 'Resistance R1 not specified!';
        edR1.SetFocus;
      end
      else if edR2.Text = '' then begin
        Mess := 'Resistance R2 not specified!';
        edR2.SetFocus;
      end
      else if edRf.Text = '' then begin
        Mess := 'Resistance Rf not specified!';
        edRf.SetFocus;
      end
      else begin
        Mess := 'Capacitance C not specified!';
        edC.SetFocus;
      end;
    end
    else begin
      rR1 := StrToFloat(edR1.Text); rR2 := StrToFloat(edR2.Text);
      rRf := StrToFloat(edRf.Text); rC := StrToFloat(edC.Text);
      if (rR1 <= 0) or (rR2 <= 0) or (rRf <= 0) then begin
        Mess := 'Resistances must be greater than 0!';
        if rR1 <= 0 then
          edR1.SetFocus
        else if rR2 <= 0 then
          edR2.SetFocus
        else
          edRf.SetFocus;
      end
      else if rC <= 0 then begin
        Mess := 'Capacitance C must be greater than 0!';
        edC.SetFocus
      end;
    end;
    if Mess = '' then begin
      // Set initial values and start simulation (by enabling the timer)
      edR1.Enabled := False; edR2.Enabled := False; edRf.Enabled := False; edC.Enabled := False;
      // Oscillator start values (for t=0: supposing op-amp iput difference is positive)
      rR1 *= 1000; rR2 *= 1000; rRf *= 1000; rC *= 1E-6;
      // Calculate circuit values
      rT := 0; rVB := rVSatPlus * rR2 / (rR1 + rR2); VS := rVB; rVA := -VS;
      T0 := rRf * rC * Ln(1 + (2 * rR2 / rR1));
      rV1 := -rVSatPlus * ((rR2 / (rR1 + rR2)) + 1); rV2 := rVSatPlus;
      // Fill circuit values into form fields
      edVA.Text := FloatToStrF(rVA, ffFixed, 0, 2); edVB.Text := FloatToStrF(rVB, ffFixed, 0, 2); edVOut.Text := FloatToStrF(rVSatPlus, ffFixed, 0, 2);
      edVS.Text := '±' + FloatToStrF(VS, ffFixed, 0, 2);
      edT0.Text := FloatToStrF(T0, ffFixed, 0, 2);
      edT.Text := FloatToStrF(2 * T0, ffFixed, 0, 2);
      edF.Text := FloatToStrF(1 / (2 * T0), ffFixed, 0, 2);
      // Set initial brush style (marking on/off state) of LED shapes
      shLedGreen.Brush.Style := bsSolid; shLedRed.Brush.Style := bsDiagCross;
      // Change button caption
      btStart.Caption := 'Stop';
      // Enable the timer (simulation code in timer routine...)
      tiOpAmp.Enabled := True;
    end
    else
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end
  // Button "Stop": Stop simulation (by disabling the timer)
  else begin
    tiOpAmp.Enabled := False;
    edR1.Enabled := True; edR2.Enabled := True; edRf.Enabled := True; edC.Enabled := True;
    btStart.Caption := 'Start';
  end;
end;

{ Timer routine: Main code for op-amp astable multivibrator simulation }

procedure TfOpAmp6.tiOpAmpTimer(Sender: TObject);

begin
  rT += 0.05;                                                                  // simulation update every 5/100 sec
  // Positive VB voltage
  if rVB > 0 then begin
    rVA := rV1 * Exp(-rT / (rRf * rC)) + rV2;                                  // capacitor charging formula (positive charge)
    edVA.Text := FloatToStrF(rVA, ffFixed, 0, 2);
    // Switch voltage reached
    if rVA > rVB then begin
      rVA := rVB;
      edVOut.Text := FloatToStrF(rVSatMinus, ffFixed, 0, 2);                   // Vout saturates to Vsat-
      rVB := rVSatMinus * rR2 / (rR1 + rR2);                                   // new (negative) VB value
      edVB.Text := FloatToStrF(rVB, ffFixed, 0, 2);
      shLedRed.Brush.Style := bsSolid;                                         // red LED goes on
      shLedGreen.Brush.Style := bsDiagCross;                                   // green LED goes out
      rT := 0;
    end;
  end
  // Negative VB voltage
  else begin
    rVA := -rV1 * Exp(-rT / (rRf * rC)) - rV2;                                 // capacitor charging formula (negative charge)
    edVA.Text := FloatToStrF(rVA, ffFixed, 0, 2);
    // Switch voltage reached
    if rVA < rVB then begin
      rVA := rVB;
      edVOut.Text := FloatToStrF(rVSatPlus, ffFixed, 0, 2);                    // Vout saturates to Vsat+
      rVB := rVSatPlus * rR2 / (rR1 + rR2);                                    // new (positive) VB value
      edVB.Text := FloatToStrF(rVB, ffFixed, 0, 2);
      shLedGreen.Brush.Style := bsSolid;                                       // green LED goes on
      shLedRed.Brush.Style := bsDiagCross;                                     // red LED goes off
      rT := 0;
    end;
  end;
end;

end.

