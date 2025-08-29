{**************************************************************}
{* Non-inverting amplifiers unit for OpAmplifiers application *}
{**************************************************************}

unit opamp3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, ComCtrls;

type
  {**********}
  { TfOpAmp3 }
  {**********}
  TfOpAmp3 = class(TForm)
    mMenu: TMainMenu;
    mWindow, mWindowClose: TMenuItem;
    mOptions, mOptionsTrackBar: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    Shape1: TShape;
    imAmp: TImage;
    Label10, Label11, Label12, Label13: TLabel;
    edVIn, edVOut: TEdit;
    edR1, edR2: TEdit;
    edVA, edI1, edI2: TEdit;
    btStart: TButton;
    tbVIn: TTrackBar;
    procedure FormActivate(Sender: TObject);
    procedure mWindowCloseClick(Sender: TObject);
    procedure mOptionsTrackBarClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tbVInChange(Sender: TObject);
  private
    iOldPos: Integer;
  public
    rVCC: Real;
  end;

var
  fOpAmp3: TfOpAmp3;

implementation

{$R *.lfm}

{ Calculate op-amp not-inverting amplifier output voltage (and other circuit values) }

procedure CalculateVOut(VCC: Real; FieldVIn, FieldR1, FieldR2: TEdit; var FieldVA, FieldI1, FieldI2, FieldVOut: TEdit; out Mess: string);

// The procedure reads all user parameters from the form fields and fills all calculated values into the form fields

var
  VSatPlus, VSatMinus, R1, R2, VIn, VA, J1, J2, VOut: Real;

begin
  Mess := '';
  // Get user parameters
  if (FieldR1.Text = '') or (FieldR2.Text = '') then begin
    if FieldR1.Text = '' then begin
      Mess := 'Resistance R1 not specified!';
      FieldR1.SetFocus;
    end
    else begin
      Mess := 'Resistance R2 not specified!';
      FieldR2.SetFocus;
    end;
  end
  else begin
    R1 := StrToFloat(FieldR1.Text); R2 := StrToFloat(FieldR2.Text);
    if (R1 <= 0) or (R2 <= 0) then begin
      if R1 <= 0 then begin
        Mess := 'Resistance R1 must be greater than 0!';
        FieldR1.SetFocus;
      end
      else begin
        Mess := 'Resistance R2 must be greater than 0!';
        FieldR2.SetFocus;
      end;
    end
    else begin
      if FieldVIn.Text = '' then begin
        Mess := 'No input voltage specified!';
        FieldVIn.SetFocus;
      end
      else begin
        VIn := StrToFloat(FieldVIn.Text);
        if (VIn < -VCC) or (VIn > VCC) then begin
          Mess := 'Input voltage should be between ' + FloatToStrF(-VCC, ffFixed, 0, 0) + 'V and ' + FloatToStrF(VCC, ffFixed, 0, 0) + 'V!';
          FieldVIn.SetFocus;
        end
        else begin
          // Non-inverting amplifier calculations
          VSatPlus := VCC - 1; VSatMinus := -VCC + 1;
          R1 *= 1000; R2 *= 1000;
          VA := VIn;
          J1 := VA / R1; J2 := J1;
          VOut := (1 + (R2 / R1)) * VIn;
          if Abs(VOut) > VSatPlus then begin
            // Amplification can't exceed VSat
            if VOut > 0 then
              VOut := VSatPlus
            else
              VOut := VSatMinus;
            MessageDlg('Saturated output voltage', 'Vout is saturated. No linear amplification for actual parameters!', mtWarning, [mbOK], 0);
          end;
          FieldVA.Text := FloatToStrF(VA, ffFixed, 0, 2);
          FieldI1.Text := FloatToStrF(1000 * J1, ffFixed, 0, 2);
          FieldI2.Text := FloatToStrF(1000 * J2, ffFixed, 0, 2);
          FieldVOut.Text := FloatToStrF(VOut, ffFixed, 0, 2);
        end;
      end;
    end;
  end;
end;

{**********}
{ TfOpAmp3 }
{**********}

{ Form show-up: Initialisation }

procedure TfOpAmp3.FormActivate(Sender: TObject);

begin
  edVIn.Text := '0,00'; edVOut.Text := '';
  edR1.Enabled := True; edR2.Enabled := True;
  edR1.Text := ''; edR2.Text := '';
  edVA.Text := ''; edI1.Text := ''; edI2.Text := '';
  tbVIn.Position := 0; iOldPos := 0;
  if btStart.Caption <> 'Calculate' then
    btStart.Caption := 'Start';
end;

{ Menu item "Window > Close": Close non-inverting amplifier window }

procedure TfOpAmp3.mWindowCloseClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Use trackbar": Toggle between trackbar and manual input }

procedure TfOpAmp3.mOptionsTrackBarClick(Sender: TObject);

begin
  edVIn.Text := '0,00'; edVOut.Text := ''; tbVIn.Position := 0;
  edVa.Text := ''; edI1.Text := ''; edI2.Text := '';
  edR1.Enabled := True; edR2.Enabled := True;
  if mOptionsTrackBar.Checked then begin
    mOptionsTrackBar.Checked := False;
    tbVIn.Enabled := False;
    edVIn.ReadOnly := False; edVIn.TabStop := True;
    edVIn.SetFocus;
    btStart.Caption := 'Calculate';
  end
  else begin
    mOptionsTrackBar.Checked := True;
    tbVIn.Enabled := True;
    edVIn.ReadOnly := True; edVIn.TabStop := False;
    btStart.Caption := 'Start';
    btStart.SetFocus;
  end;
end;

{ Button "Start/Stop/Calculate": Start/stop non-inverting amplifier simulation resp. do calculation for manually entered voltage }

procedure TfOpAmp3.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Button "Start" resp. "Calculate": Do calculations
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Calculate') then begin
    CalculateVOut(rVCC, edVIn, edR1, edR2, edVA, edI1, edI2, edVOut, Mess);
    if Mess = '' then begin
      if btStart.Caption = 'Start' then begin
        edR1.Enabled := False; edR2.Enabled := False;
        btStart.Caption := 'Stop';
      end;
    end
    else
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end
  // Button "Stop": Stop simulation
  else begin
    edR1.Enabled := True; edR2.Enabled := True;
    btStart.Caption := 'Start';
  end;
end;

{ Trackbar position change: Get Vin and update calculations for this value }

procedure TfOpAmp3.tbVInChange(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn.Text := FloatToStrF(tbVIn.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, edVIn, edR1, edR2, edVA, edI1, edI2, edVOut, Mess);
      iOldPos := tbVIn.Position;
    end
    else
      // Trackbar only active if simulation is started
      tbVIn.Position := iOldPos;
  end;
end;

end.

