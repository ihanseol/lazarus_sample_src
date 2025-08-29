{*********************************}
{* Main unit for HSPressure unit *}
{*********************************}

unit hsp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls;

type
  {**************}
  { TfHSPressure }
  {**************}
  TfHSPressure = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsG, mOptionsG1, mOptionsG2, mOptionsG3: TMenuItem;
    mOptionsPressure, mOptionsPressure1, mOptionsPressure2, mOptionsPressure3: TMenuItem;
    mOptionsDecimals, mOptionsDecimals1, mOptionsDecimals2, mOptionsDecimals3: TMenuItem;
    mOptionsWater1, mOptionsTrackbar: TMenuItem;
    mHelp, mHelpAbout, Separator1: TMenuItem;
    StaticText1: TStaticText;
    imContainer, imManometer: TImage;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    laUDensity, laDepth, laUDepth, laUPressure: TLabel;
    cobLiquids: TComboBox;
    edDensity, edDepth, edPressure: TEdit;
    shDown, shUp, Shape1, Shape2: TShape;
    btCalc: TButton;
    tbManometer: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsG1Click(Sender: TObject);
    procedure mOptionsG2Click(Sender: TObject);
    procedure mOptionsG3Click(Sender: TObject);
    procedure mOptionsPressure1Click(Sender: TObject);
    procedure mOptionsPressure2Click(Sender: TObject);
    procedure mOptionsPressure3Click(Sender: TObject);
    procedure mOptionsDecimals1Click(Sender: TObject);
    procedure mOptionsDecimals2Click(Sender: TObject);
    procedure mOptionsDecimals3Click(Sender: TObject);
    procedure mOptionsWater1Click(Sender: TObject);
    procedure mOptionsTrackbarClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure cobLiquidsChange(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure tbManometerChange(Sender: TObject);
  private
    iDecimals: Integer;
    rG, rDensity, rDepth, rPressure, rDensityH2O: Real;
    sUPressure: string;
  end;

const
  SUP_3 = #$C2#$B3;

var
  fHSPressure: TfHSPressure;

implementation

{$R *.lfm}

{ Nth power of a real number }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Format of a real number }

function RFormat(R: Real; F: Integer): string;

var
  R0: Real;
  SR: string;

begin
  SR := '';
  if R = 0 then
    SR := '0'
  else begin
    if F >= 0 then begin
      R0 := Round(R * Power(10, F)) / Power(10, F);
      if Abs(R0) < Power(10, -F) then
        SR := FloatToStrF(R, ffExponent, F, 0)
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Pressure conversion }

function PConvert(OldPressure: Real; OldUnit, NewUnit: string): Real;

var
  NewPressure: Real;

begin
  NewPressure := OldPressure;
  if NewUnit <> OldUnit then begin
    if OldUnit = 'hP' then begin
      if NewUnit = 'atm' then
        NewPressure /= 1.013
      else
        NewPressure *= 760 / 1.013;
    end
    else if OldUnit = 'atm' then begin
      if NewUnit = 'hP' then
        NewPressure *= 1.013
      else
        NewPressure *= 760;
    end
    else begin
      if NewUnit = 'hP' then
        NewPressure *= 1.013 / 760
      else
        NewPressure /= 760;
    end;
  end;
  Result := NewPressure;
end;

{ Read simulation values from form }

procedure ReadValues(out Density, Depth: Real; out Mess: string);

begin
  Mess := '';
  if fHSPressure.edDensity.Text = '' then begin
    Mess := 'Density value is missing!';
    fHSPressure.edDensity.SetFocus;
  end
  else
    Density := StrToFloat(fHSPressure.edDensity.Text);
  if fHSPressure.edDepth.Text = '' then begin
    if Mess = '' then begin
      Mess := 'Depth valus is missing!';
      fHSPressure.edDepth.SetFocus;
    end;
  end
  else begin
    Depth := StrToFloat(fHSPressure.edDepth.Text);
    fHSPressure.edDepth.Text := FloatToStrF(Depth, ffFixed, 0, 2);
  end;
  if Mess = '' then begin
    if Density <= 0 then begin
      Mess := 'Density cannot be negative or zero!';
      fHSPressure.edDensity.SetFocus;
    end
    else if Depth < 0 then begin
      Mess := 'Depth cannot be negative!';
      fHSPressure.edDepth.SetFocus;
    end
    else if Depth > 5 then begin
      Mess := 'Depth cannot exceed 5 cm!';
      fHSPressure.edDepth.SetFocus;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Data error', Mess, mtError, [mbOK], 0);
end;

{ Hydrostatic pressure calculation }

function HydrostaticPressure(Density, Depth, G: Real; F: Integer): Real;

var
  Pressure, DPressure: Real;

begin
  Pressure := G * (1E+3 * Density) * (1E-2 * Depth) * 1E-2; DPressure := Pressure;
  // Transform pressure value to actually used unit
  if fHSPressure.mOptionsPressure2.Checked then
    DPressure := PConvert(DPressure, 'hP', 'atm')
  else if fHSPressure.mOptionsPressure3.Checked then
    DPressure := PConvert(DPressure, 'hP', 'mm Hg');
  // Display pressure value
  fHSPressure.edPressure.Text := RFormat(DPressure, F);
  Result := Pressure;
end;

{ Reset the simulation }

procedure SimulReset;

begin
  // Move manometer out of the container
  fHSPressure.imManometer.Top := 72;
  // Reset trackbar at zero
  fHSPressure.tbManometer.Position := 0;
  fHSPressure.edDepth.Text := FloatToStrF(0, ffFixed, 0, 2);
  // Clear pressure edit field
  fHSPressure.edPressure.Text := '';
  // Hide the liquid shapes in the manometer tube
  fHSPressure.shDown.Visible := False;
  fHSPressure.shUp.Visible := False;
end;

{ Move manometer and adapt liquid variation in the tube }

procedure MoveManometer(Depth, Pressure: Real);

var
  H: Integer;

begin
  if Depth = 0 then begin
    // Move manometer out of the container
    SimulReset;
  end
  else begin
    // Move manometer into the container
    fHSPressure.imManometer.Top := Round(98 + 40 * Depth);
    // Show liquid variation in the tube
    fHSPressure.shDown.Visible := False;
    fHSPressure.shUp.Visible := False;
    H := Round(Pressure * (63 / 10));
    // Pressure can be measured (liquid in tube doesn't fall outside the graph)
    if Pressure <= 13 then begin
      fHSPressure.shDown.Visible := True;
      fHSPressure.shUp.Visible := True;
      // Liquid variation in the left arm of the U-tube
      fHSPressure.shDown.Height := H;
      fHSPressure.shDown.Top := fHSPressure.imManometer.Top + 142;
      // Liquid variation in the right arm of the U-tube
      fHSPressure.shUp.Height := H;
      fHSPressure.shUp.Top := fHSPressure.imManometer.Top + 138 - H;
    end
    // Pressure cannot be measured (liquid in tube falls outside the graph)
    else
      MessageDlg('Manometer error', 'Manometer cannot measure this pressure!', mtWarning, [mbOK], 0);
  end;
end;

{**************}
{ TfHSPressure }
{**************}

{ Application start: Initialization }

procedure TfHSPressure.FormCreate(Sender: TObject);

begin
  edDepth.Text := FloatToStrF(0.00, ffFixed, 0, 2);
  laUDensity.Caption := StringReplace(laUDensity.Caption, '3', SUP_3, []);
  rG := 9.81; sUPressure := 'hP'; iDecimals := 3; rDensityH2O := 1;
end;

{ Menu item "File > Exit": Exit application }

procedure TfHSPressure.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Value of g > ...": Select value of acceleration due to gravity }

procedure TfHSPressure.mOptionsG1Click(Sender: TObject);

begin
  mOptionsG1.Checked := True; mOptionsG2.Checked := False; mOptionsG3.Checked := False;
  rG := 9.8;
end;

procedure TfHSPressure.mOptionsG2Click(Sender: TObject);

begin
  mOptionsG2.Checked := True; mOptionsG1.Checked := False; mOptionsG3.Checked := False;
  rG := 9.81;
end;

procedure TfHSPressure.mOptionsG3Click(Sender: TObject);

begin
  mOptionsG3.Checked := True; mOptionsG2.Checked := False; mOptionsG1.Checked := False;
  rG := 10;
end;

{ Menu items "Options > Unit of pressures > ...": Select unit of pressures }

procedure TfHSPressure.mOptionsPressure1Click(Sender: TObject);

begin
  mOptionsPressure1.Checked := True; mOptionsPressure2.Checked := False; mOptionsPressure3.Checked := False;
  sUPressure := 'hP';
  if edPressure.Text <> '' then
    edPressure.Text := RFormat(PConvert(StrToFloat(edPressure.Text), laUPressure.Caption, sUPressure), iDecimals);
  laUPressure.Caption := sUPressure;
end;

procedure TfHSPressure.mOptionsPressure2Click(Sender: TObject);

begin
  mOptionsPressure2.Checked := True; mOptionsPressure1.Checked := False; mOptionsPressure3.Checked := False;
  sUPressure := 'atm';
  if edPressure.Text <> '' then
    edPressure.Text := RFormat(PConvert(StrToFloat(edPressure.Text), laUPressure.Caption, sUPressure),iDecimals);
  laUPressure.Caption := sUPressure;
end;

procedure TfHSPressure.mOptionsPressure3Click(Sender: TObject);

begin
  mOptionsPressure3.Checked := True; mOptionsPressure2.Checked := False; mOptionsPressure1.Checked := False;
  sUPressure := 'mm Hg';
  if edPressure.Text <> '' then
    edPressure.Text := RFormat(PConvert(StrToFloat(edPressure.Text), laUPressure.Caption, sUPressure),iDecimals);
  laUPressure.Caption := sUPressure;
end;

{ Menu items "Options > Decimal digits > ...": Select number of decimal digits of pressures }

procedure TfHSPressure.mOptionsDecimals1Click(Sender: TObject);

var
  Mess: string;

begin
  mOptionsDecimals1.Checked := True; mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := False;
  iDecimals := 1;
  if (edDensity.Text <> '') and (edDepth.Text <> '') and (edPressure.Text <> '') then begin
    ReadValues(rDensity, rDepth, Mess);
    if Mess = '' then
      rPressure := HydrostaticPressure(rDensity, rDepth, rG, iDecimals);
  end;
end;

procedure TfHSPressure.mOptionsDecimals2Click(Sender: TObject);

var
  Mess: string;

begin
  mOptionsDecimals2.Checked := True; mOptionsDecimals1.Checked := False; mOptionsDecimals3.Checked := False;
  iDecimals := 2;
  if (edDensity.Text <> '') and (edDepth.Text <> '') and (edPressure.Text <> '') then begin
    ReadValues(rDensity, rDepth, Mess);
    if Mess = '' then
      rPressure := HydrostaticPressure(rDensity, rDepth, rG, iDecimals);
  end;
end;

procedure TfHSPressure.mOptionsDecimals3Click(Sender: TObject);

var
  Mess: string;

begin
  mOptionsDecimals3.Checked := True; mOptionsDecimals2.Checked := False; mOptionsDecimals1.Checked := False;
  iDecimals := 3;
  if (edDensity.Text <> '') and (edDepth.Text <> '') and (edPressure.Text <> '') then begin
    ReadValues(rDensity, rDepth, Mess);
    if Mess = '' then
      rPressure := HydrostaticPressure(rDensity, rDepth, rG, iDecimals);
  end;
end;

{ Menu item "Options > Water density = 1": Toggle water density = 1 resp. 0.998 }

procedure TfHSPressure.mOptionsWater1Click(Sender: TObject);

var
  Mess: string;

begin
  if mOptionsWater1.Checked then begin
    mOptionsWater1.Checked := False;
    rDensityH2O := 0.998;
  end
  else begin
    mOptionsWater1.Checked := True;
    rDensityH2O := 1;
  end;
  edDensity.Text := FloatToStr(rDensityH2O);
  if (edDensity.Text <> '') and (edDepth.Text <> '') and (edPressure.Text <> '') then begin
    ReadValues(rDensity, rDepth, Mess);
    if Mess = '' then
      rPressure := HydrostaticPressure(rDensity, rDepth, rG, iDecimals);
  end;
end;

{ Menu item "Options > Use trackbar": Toggle usage of trackbar resp. manual depth input }

procedure TfHSPressure.mOptionsTrackbarClick(Sender: TObject);

begin
  if mOptionsTrackbar.Checked then begin
    mOptionsTrackbar.Checked := False;
    laDepth.Visible := False; tbManometer.Visible := False;
    edDepth.ReadOnly := False; edDepth.TabStop := True; edDepth.Color := clDefault;
    btCalc.Visible := True;
  end
  else begin
    mOptionsTrackbar.Checked := True;
    laDepth.Visible := True; tbManometer.Visible := True;
    edDepth.ReadOnly := True; edDepth.TabStop := False; edDepth.Color := clCream;
    btCalc.Visible := False;
    SimulReset;
  end;
end;

{ Menu item "Help > About": Display application help }

procedure TfHSPressure.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics simulation:' + LineEnding;
  S += 'Hydrostatic pressure in liquids.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2023.';
  MessageDlg('About "HSPressure"', S, mtInformation, [mbOK], 0);
end;

{ Usage of different liquid: Adapt density value }

procedure TfHSPressure.cobLiquidsChange(Sender: TObject);

var
  Density: Real;

begin
  edDensity.ReadOnly := True; edDensity.TabStop := False; edDensity.Color := clCream;
  case cobLiquids.ItemIndex of
    0: begin
      edDensity.ReadOnly := False; edDensity.TabStop := True; edDensity.Color := clDefault;
    end;
    1: Density := rDensityH2O;
    2: Density := 1.026;
    3: Density := 0.789;
    4: Density := 0.879;
    5: Density := 1.489;
    6: Density := 0.6550;
    7: Density := 13.546;
  end;
  if cobLiquids.ItemIndex > 0 then
    edDensity.Text := FloatToStr(Density);
  SimulReset;
end;

{ Button "Calculation" pushed: Do simulation for actual data values }

procedure TfHSPressure.btCalcClick(Sender: TObject);

var
  Mess: string;

begin
  ReadValues(rDensity, rDepth, Mess);
  if Mess = '' then begin
    rPressure := HydrostaticPressure(rDensity, rDepth, rG, iDecimals);
    MoveManometer(rDepth, rPressure);
  end;
end;

{ Depth trackbar position changed: Do simulation for actual data values }

procedure TfHSPressure.tbManometerChange(Sender: TObject);

var
  Mess: string;

begin
  edDepth.Text := FloatToStrF(tbManometer.Position / 100, ffFixed, 0, 2);
  ReadValues(rDensity, rDepth, Mess);
  if Mess = '' then begin
    rPressure := HydrostaticPressure(rDensity, rDepth, rG, iDecimals);
    MoveManometer(rDepth, rPressure);
  end;
end;

end.

