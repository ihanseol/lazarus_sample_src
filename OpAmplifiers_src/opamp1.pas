{*************************************************}
{* Comparators unit for OpAmplifiers application *}
{*************************************************}

unit opamp1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus;

type
  {**********}
  { TfOpAmp1 }
  {**********}
  TfOpAmp1 = class(TForm)
    mMenu: TMainMenu;
    mWindow, mWindowClose: TMenuItem;
    mOptions, mOptionsTrackBar: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3: TLabel;
    Shape1: TShape;
    imAmp: TImage;
    edVIn, edVOut: TEdit;
    edV: TEdit;
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
    iAol: Integer;
    rVCC, rVMin: Real;
  end;

var
  fOpAmp1: TfOpAmp1;

implementation

{$R *.lfm}

{ Calculate op-amp comparator output voltage (and other circuit values) }

procedure CalculateVOut(VCC, VMin: Real; Aol: Integer; FieldVIn, FieldV:TEdit; var FieldVOut: TEdit; out Mess: string);

// The procedure reads all user parameters from the form fields and fills all calculated values into the form fields

var
  VSatPlus, VSatMinus, V, VIn, VOut: Real;

begin
  Mess := '';
  // Get user parameters
  if FieldV.Text = '' then begin
    Mess := 'No compare voltage specified!';
    FieldV.SetFocus;
  end
  else begin
    V := StrToFloat(FieldV.Text);
    if (V < -VCC) or (V > VCC) then begin
      Mess := 'Compare voltage should be between ' + FloatToStrF(-VCC, ffFixed, 0, 0) + ' and ' + FloatToStrF(VCC, ffFixed, 0, 0) + '!';
      FieldV.SetFocus;
    end
    else begin
      if FieldVIn.Text = '' then begin
        Mess := 'No input voltage specified!';
        FieldVIn.SetFocus;
      end
      else begin
        VIn := StrToFloat(FieldVIn.Text);
        if (VIn < 0) or (VIn > VCC) then begin
          Mess := 'Input voltage should be between 0V and ' + FloatToStrF(VCC, ffFixed, 0, 0) + 'V!';
          FieldVIn.SetFocus;
        end
        else begin
          if VIn = V then
            VOut := 0
          else begin
            if Abs(VIn - V) < VMin then begin
              // Voltage to small for saturated output
              MessageDlg('Non-saturated output voltage', 'Voltage difference to small to saturate Vout!', mtWarning, [mbOK], 0);
              VOut := Aol * (VIn - V);
            end
            else begin
              // Normal case: VSat+ or VSat- output voltage
              VSatPlus := VCC - 1; VSatMinus := -VCC + 1;
              if VIn < V then
                VOut := VSatMinus
              else if VIn > V then
                VOut := VSatPlus
            end;
          end;
          FieldVOut.Text := FloatToStrF(VOut, ffFixed, 0, 2);
        end;
      end;
    end;
  end;
end;

{**********}
{ TfOpAmp1 }
{**********}

{ Form show-up: Initialisation }

procedure TfOpAmp1.FormActivate(Sender: TObject);

begin
  edVIn.Text := '0,00'; edVOut.Text := '';
  edV.Enabled := True; edV.Text := '';
  tbVIn.Position := 0; iOldPos := 0;
  if btStart.Caption <> 'Calculate' then
    btStart.Caption := 'Start';
end;

{ Menu item "Window > Close": Close comparators window }

procedure TfOpAmp1.mWindowCloseClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Use trackbar": Toggle between trackbar and manual input }

procedure TfOpAmp1.mOptionsTrackBarClick(Sender: TObject);

begin
  edVIn.Text := '0,00'; edVOut.Text := ''; tbVIn.Position := 0;
  edV.Enabled := True;
  if mOptionsTrackBar.Checked then begin
    mOptionsTrackBar.Checked := False;
    tbVIn.Enabled := False;                                                    // trackbar disabled for manual input
    edVIn.ReadOnly := False; edVIn.TabStop := True;
    edVIn.SetFocus;
    btStart.Caption := 'Calculate';
  end
  else begin
    mOptionsTrackBar.Checked := True;
    tbVIn.Enabled := True;
    edVIn.ReadOnly := True; edVIn.TabStop := False;                            // manual entry disabled for trackbar input
    btStart.Caption := 'Start';
    btStart.SetFocus;
  end;
end;

{ Button "Start/Stop/Calculate": Start/stop comparator simulation resp. do calculation for manually entered voltage }

procedure TfOpAmp1.btStartClick(Sender: TObject);

var
  Mess: string;

begin
  // Button "Start" resp. "Calculate": Do calculations
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Calculate') then begin
    CalculateVOut(rVCC, rVMin, iAol, edVIn, edV, edVOut, Mess);
    if Mess = '' then begin
      // Proceed if user parameters are ok
      if btStart.Caption = 'Start' then begin
        // Button "Start" only
        edV.Enabled := False;                                                  // disable compare voltage entry until "Stop" pushed
        btStart.Caption := 'Stop';
      end;
    end
    else
      // Invalid user values
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
  end
  // Button "Stop": Stop the simulation
  else begin
    edV.Enabled := True;                                                       // re-enable compare voltage entry
    btStart.Caption := 'Start';
  end;
end;

{ Trackbar position change: Get Vin and update calculations for this value }

procedure TfOpAmp1.tbVInChange(Sender: TObject);

var
  Mess: string;

begin
  if btStart.Caption <> 'Calcuate' then begin
    if btStart.Caption = 'Stop' then begin
      // Trackbar reaction only if simulation has been started
      edVIn.Text := FloatToStrF(tbVIn.Position / 20, ffFixed, 0, 2);
      CalculateVOut(rVCC, rVMin, iAol, edVIn, edV, edVOut, Mess);
      iOldPos := tbVIn.Position;
    end
    else
      // Inactive trackbar if simulation not yet started
      tbVIn.Position := iOldPos;
  end;
end;

end.

