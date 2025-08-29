{**********************************************************}
{* Inverting amplifiers unit for OpAmplifiers application *}
{**********************************************************}

unit opamp2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus;

type

  {**********}
  { TfOpAmp2 }
  {**********}
  TfOpAmp2 = class(TForm)
    mMenu: TMainMenu;
    mWindow, mWindowClose: TMenuItem;
    mOptions: TMenuItem;
    mOptionsTrackBar: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    Label10, Label11, Label12, Label13: TLabel;
    Shape1: TShape;
    imAmp: TImage;
    edVIn, edVOut: TEdit;
    edR1, edRf: TEdit;
    edVA, edI1, edIf: TEdit;
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
  fOpAmp2: TfOpAmp2;

implementation

{$R *.lfm}

{ Calculate op-amp inverting amplifier output voltage (and other circuit values) }

procedure CalculateVOut(VCC: Real; FieldVIn, FieldR1, FieldRf: TEdit; var FieldVA, FieldI1, FieldIf, FieldVOut: TEdit; out Mess: string);

// The procedure reads all user parameters from the form fields and fills all calculated values into the form fields

var
  VSatPlus, VSatMinus, R1, Rf, VIn, VA, J1, Jf, VOut: Real;

begin
  Mess := '';
  // Get user parameters
  if (FieldR1.Text = '') or (FieldRf.Text = '') then begin
    if FieldR1.Text = '' then begin
      Mess := 'Resistance R1 not specified!';
      FieldR1.SetFocus;
    end
    else begin
      Mess := 'Resistance Rf not specified!';
      FieldRf.SetFocus;
    end;
  end
  else begin
    R1 := StrToFloat(FieldR1.Text); Rf := StrToFloat(FieldRf.Text);
    if (R1 <= 0) or (Rf <= 0) then begin
      if R1 <= 0 then begin
        Mess := 'Resistance R1 must be greater than 0!';
        FieldR1.SetFocus;
      end
      else begin
        Mess := 'Resistance Rf must be greater than 0!';
        FieldRf.SetFocus;
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
          // Inverting amplifier calculations
          VSatPlus := VCC - 1; VSatMinus := -VCC + 1;
          R1 *= 1000; Rf *= 1000;                                              // transform to Ω
          VA := 0;
          J1 := VIn / R1; Jf := J1;
          VOut := - (Rf / R1) * VIn;
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
          FieldIf.Text := FloatToStrF(1000 * Jf, ffFixed, 0, 2);
          FieldVOut.Text := FloatToStrF(VOut, ffFixed, 0, 2);
        end;
      end;
    end;
  end;
end;

{**********}
{ TfOpAmp2 }
{**********}

{ Form show-up: Initialisation }

procedure TfOpAmp2.FormActivate(Sender: TObject);

begin
  edVIn.Text := '0,00'; edVOut.Text := '';
  edR1.Enabled := True; edRf.Enabled := True;
  edR1.Text := ''; edRf.Text := '';
  edVA.Text := ''; edI1.Text := ''; edIf.Text := '';
  tbVIn.Position := 0; iOldPos := 0;
  if btStart.Caption <> 'Calculate' then
    btStart.Caption := 'Start';
end;

{ Menu item "Window > Close": Close inverting amplifier window }

procedure TfOpAmp2.mWindowCloseClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Use trackbar": Toggle between trackbar and manual input }

procedure TfOpAmp2.mOptionsTrackBarClick(Sender: TObject);

begin
  edVIn.Text := '0,00'; edVOut.Text := ''; tbVIn.Position := 0;
  edVa.Text := ''; edI1.Text := ''; edIf.Text := '';
  edR1.Enabled := True; edRf.Enabled := True;
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

{ Button "Start/Stop/Calculate": Start/stop inverting amplifier simulation resp. do calculation for manually entered voltage }

procedure TfOpAmp2.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Button "Start" resp. "Calculate": Do calculations
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Calculate') then begin
    CalculateVOut(rVCC, edVIn, edR1, edRf, edVA, edI1, edIf, edVOut, Mess);
    if Mess = '' then begin
      if btStart.Caption = 'Start' then begin
        edR1.Enabled := False; edRf.Enabled := False;
        btStart.Caption := 'Stop';
      end;
    end
    else
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end
  // Button "Stop": Stop simulation
  else begin
    edR1.Enabled := True; edRf.Enabled := True;
    btStart.Caption := 'Start';
  end;
end;

{ Trackbar position change: Get Vin and update calculations for this value }

procedure TfOpAmp2.tbVInChange(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      edVIn.Text := FloatToStrF(tbVIn.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, edVIn, edR1, edRf, edVA, edI1, edIf, edVOut, Mess);
      iOldPos := tbVIn.Position;
    end
    else
      // Trackbar active only, if simulation has been started
      tbVIn.Position := iOldPos;
  end;
end;

end.

