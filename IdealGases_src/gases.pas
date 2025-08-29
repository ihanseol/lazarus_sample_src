{******************************************}
{*  Main unit for IdealGases application  *}
{******************************************}

unit gases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus, PopupNotifier, help;

type
  { TfGases }
  TfGases = class(TForm)
    mFile: TMenuItem;
    mFileReset: TMenuItem;
    mFileExit: TMenuItem;
    mMain: TMainMenu;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    imVOverflow: TImage;
    popAbout: TPopupNotifier;
    shP0: TShape;
    shP: TShape;
    shT0: TShape;
    shT: TShape;
    edVolume: TEdit;
    edPressure: TEdit;
    edPressureATM: TEdit;
    edTemperature: TEdit;
    edTemperatureC: TEdit;
    edGas: TEdit;
    edGas1: TEdit;
    edGas2: TEdit;
    edPressure1: TEdit;
    edPressure2: TEdit;
    rbBoyle: TRadioButton;
    rbAmontons: TRadioButton;
    rbCharles: TRadioButton;
    rbAvogadro: TRadioButton;
    rbIdeal: TRadioButton;
    memoLaw: TMemo;
    laVolume: TLabel;
    laPressure: TLabel;
    laTemperature: TLabel;
    laGas1: TLabel;
    laGas2: TLabel;
    tbVolume: TTrackBar;
    tbTemperature: TTrackBar;
    tbPressure: TTrackBar;
    tbGas1: TTrackBar;
    tbGas2: TTrackBar;
    memoDescription: TMemo;
    cbResult: TComboBox;
    btCalc: TButton;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18, Label19, Label20: TLabel;
    Label21, Label22, Label23, Label24, Label25: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure mFileResetClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbBoyleChange(Sender: TObject);
    procedure rbAmontonsChange(Sender: TObject);
    procedure rbCharlesChange(Sender: TObject);
    procedure rbAvogadroChange(Sender: TObject);
    procedure rbIdealChange(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure tbPressureChange(Sender: TObject);
    procedure tbTemperatureChange(Sender: TObject);
    procedure tbGas1Change(Sender: TObject);
    procedure tbGas2Change(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure cbResultChange(Sender: TObject);
  private
    iMolecules1, iMolecules2: Integer;
    rV, rP, rT, rN1, rN2, rN: Real;
    aMolecules1, aMolecules2: array of TShape;
  end;

const
  R = 8.314;
  Properties: array[0..3] of string = ('Volume', 'Pressure', 'Temperature', 'Gas quantity');

var
  fGases: TfGases;

function GasVolume(P, T, N: Real): Real;
function GasPressure(V, T, N: Real): Real;
function GasTemperature(V, P, N: Real): Real;
function GasQuantity(V, P, T: Real): Real;
procedure AppReset(var V, P, T, N, N1, N2: Real);
procedure DisplayGasValues(V, P, T, N, N1, N2: Real);
procedure MoleculesDraw(V, N1, N2: Real; var M1, M2: Integer);
procedure TemperatureDraw(T: Real);
procedure PressureDraw(P: Real);
procedure TrackbarsEnable(VEnable, PEnable, TEnable, G1Enable, G2Enable: Boolean);
procedure ResultsEnable(VEnable, PEnable, TEnable, G1Enable, G2Enable: Boolean);
procedure SetVolume(V: Real);
procedure SetPressure(P: Real);
procedure SetTemperature(T: Real; TMin, TMax: Integer);
procedure SetQuantity(N: Real; Gas: Integer);
procedure GasQuantities(NOld, N: Real; var N1, N2: Real);

implementation

{$R *.lfm}

{ Function: Calculate gas volume }

function GasVolume(P, T, N: Real): Real;

begin
  GasVolume := N * R * T / P;
end;

{ Function: Calculate gas pressure }

function GasPressure(V, T, N: Real): Real;

begin
  GasPressure := N * R * T / V;
end;

{ Function: Calculate gas temperature }

function GasTemperature(V, P, N: Real): Real;

begin
  GasTemperature := (V * P) / (N * R);
end;

{ Function: Calculate gas quantity (number of moles) }

function GasQuantity(V, P, T: Real): Real;

begin
  GasQuantity := (V * P) / (T * R);
end;

{ Set gas properties and trackbars to default values }

procedure AppReset(var V, P, T, N, N1, N2: Real);

begin
  V := 22.4; P := 101.325; T := 273.15;
  N := 1; N1 := 0.9; N2 := 0.1;
  DisplayGasValues(V, P, T, N, N1, N2);
  fGases.tbVolume.Position := 224;
  fGases.tbPressure.Position := 100;
  fGases.tbTemperature.Position := 0;
  fGases.tbGas1.Position := 90;
  fGases.tbGas2.Position := 10;
end;

{ Display gas properties }

procedure DisplayGasValues(V, P, T, N, N1, N2: Real);

var
  PATM, P1, P2, TC: Real;

begin
  V := Round(10 * V) / 10;
  P := Round(1000 * P) / 1000; PATM := Round(100 * (P / 101.325)) / 100;
  T := Round(1000 * T) / 1000; TC := Round(100 * (T - 273.15)) / 100;
  N := Round(100 * N) / 100; N1 := Round(100 * N1) / 100; N2 := Round(100 * N2) / 100;
  P1 := Round(1000 * ((N1 / N) * P)) / 1000; P2 := P - P1;
  fGases.edPressure.Text := FloatToStr(P);
  fGases.edPressureATM.Text := FloatToStr(PATM);
  fGases.edPressure1.Text := FloatToStr(P1);
  fGases.edPressure2.Text := FloatToStr(P2);
  fGases.edTemperature.Text := FloatToStr(T);
  fGases.edTemperatureC.Text := FloatToStr(TC);
  fGases.edVolume.Text := FloatToStr(V);
  fGases.edGas.Text := FloatToStr(N);
  fGases.edGas1.Text := FloatToStr(N1);
  fGases.edGas2.Text := FloatToStr(N2);
end;

{ Draw gas volume/quantity (as a certain number of "molecules") }

procedure MoleculesDraw(V, N1, N2: Real; var M1, M2: Integer);

// Molecules are drawn as colored TShape objects which are dynamically created during runtime. If the actual number of molecules is greater than
// the number of shapes existing, new TShape objects are created. If there are more TShape objects than actual molecules, those not needed are
// destroyed. The TShape objects are placed in 2 dynamic arrays which allows to easily handle them. Display within the previewed volume area on
// the form is done by setting the Left and Top properties of the TShape objects to random values.

const
  RMolecules1 = 20; RMolecules2 = 30;                                          // molecule size
  ColMolecules1 = clBlue; ColMolecules2 = clLime;                              // molecule color
  VSurface = 630 * 315;                                                        // maximum volume surface on the form (width=630, height=315)
  VLeft = 20; VTop = 20;                                                       // left and top margins

var
  IN1, IN2, I: Integer;
  VWidth, VHeight: Integer;
  Vol: Real;

begin
  Vol := VSurface * (V / 100);                                                 // actual volume on the form
  VHeight := Round(Sqrt(Vol / 2)); VWidth := 2 * VHeight;                      // actual volume's width and height
  // Actual volume fits (in previewed place) on the form and number of molecules = 1 .. 1000
  if (VWidth >= 30) and (VHeight >= 30) and (VWidth <= 630) and (VHeight <= 315) and
     (Round(100 * N1) + Round(100 * N2) > 0) and (Round(100 * N1) + Round(100 * N2) <= 1000) then begin
    fGases.imVOverflow.Visible := False;
    IN1 := Round(100 * N1); IN2 := Round(100 * N2);                            // actual number of molecules
    // Remove molecules not needed (by destroying the corresponding TShape objects)
    if IN1 - 1 < M1 then begin
      for I := IN1 to M1 do
        fGases.aMolecules1[I].Destroy;
    end;
    if IN2 - 1 < M2 then begin
      for I := IN2 to M2 do
        fGases.aMolecules2[I].Destroy;
    end;
    // Set array length (number of elements) to actual value
    SetLength(fGases.aMolecules1, IN1); SetLength(fGases.aMolecules2, IN2);
    // Create new molecules for Gas 1 (if needed)
    for I := 0 to IN1 - 1 do begin
      if I > M1 then begin
        fGases.aMolecules1[I] := TShape.Create(fGases.aMolecules1[I]);
        fGases.aMolecules1[I].Parent := fGases;
        fGases.aMolecules1[I].Shape := stEllipse;
        fGases.aMolecules1[I].Width := RMolecules1;
        fGases.aMolecules1[I].Height := RMolecules1;
        fGases.aMolecules1[I].Brush.Color := ColMolecules1;
      end;
      // Place molecule at random position in previewed form area
      fGases.aMolecules1[I].Left := Random(VWidth - RMolecules1) + VLeft;
      fGases.aMolecules1[I].Top := Random(VHeight - RMolecules1) + VTop;
      fGases.aMolecules1[I].Visible := True;
    end;
    // Create new molecules for Gas 2 (if needed)
    for I := 0 to IN2 - 1 do begin
      if I > M2 then begin
        fGases.aMolecules2[I] := TShape.Create(fGases.aMolecules2[I]);
        fGases.aMolecules2[I].Parent := fGases;
        fGases.aMolecules2[I].Shape := stEllipse;
        fGases.aMolecules2[I].Width := RMolecules2;
        fGases.aMolecules2[I].Height := RMolecules2;
        fGases.aMolecules2[I].Brush.Color := ColMolecules2;
      end;
      // Place molecule at random position in previewed form area
      fGases.aMolecules2[I].Left := Random(VWidth - RMolecules2) + VLeft;
      fGases.aMolecules2[I].Top := Random(VHeight - RMolecules2) + VTop;
      fGases.aMolecules2[I].Visible := True;
    end;
    M1 := IN1 - 1; M2 := IN2 - 1;                                              // keep track of number of TShape objects created
  end
  // Actual volume does not fit (in previewed place) on the form
  else begin
    // Remove molecules (by making them invisible)
    for I := 0 to M1 do
      fGases.aMolecules1[I].Visible := False;
    for I := 0 to M2 do
      fGases.aMolecules2[I].Visible := False;
    // Display the "volume overflow" picture
    fGases.imVOverflow.Visible := True;
  end;
end;

{ Draw thermometer }

procedure TemperatureDraw(T: Real);

begin
  T -= 273.15;                                                                 // convert to °C
  // Temperature within thermometer limits (-250 to 1000)
  if Round(250 * ((T + 250) / 1250)) <= 250 then begin
    fGases.shT.Height := Round(250 * ((T + 250) / 1250));
    fGases.shT.Top := Round(20 + 250 * (1 - (T + 250) / 1250));
    fGases.shT.Brush.Color := clSilver;
  end
  // // Temperature outside thermometer limits
  else begin
    fGases.shT.Height := 250;
    fGases.shT.Top := 20;
    fGases.shT.Brush.Color := clRed;
  end;
end;

{ Draw barometer }

procedure PressureDraw(P: Real);

begin
  P /= 101.325;                                                                // convert to atm
  // Pressure within barometer limits (0 to 5)
  if Round(250 * (P / 5)) <= 250 then begin
    fGases.shP.Brush.Color := clSilver;
    fGases.shP.Height := Round(250 * (P / 5));
    fGases.shP.Top := Round(20 + 250 * (1 - P / 5));
  end
    // Pressure outside barometer limits
  else begin
    fGases.shP.Brush.Color := clRed;
    fGases.shP.Height := 250;
    fGases.shP.Top := 20;
  end;
end;

{ Enable/disable trackbars }

procedure TrackbarsEnable(VEnable, PEnable, TEnable, G1Enable, G2Enable: Boolean);

begin
  fGases.tbVolume.Enabled := VEnable;
  fGases.tbPressure.Enabled := PEnable;
  fGases.tbTemperature.Enabled := TEnable;
  fGases.tbGas1.Enabled := G1Enable;
  fGases.tbGas2.Enabled := G2Enable;
end;

{ Add/don't add gas property to value to be calculated combobox }

procedure ResultsEnable(VEnable, PEnable, TEnable, G1Enable, G2Enable: Boolean);

begin
  fGases.cbResult.Clear;
  if VEnable then
    fGases.cbResult.Items.AddText(Properties[0]);
  if PEnable then
    fGases.cbResult.Items.AddText(Properties[1]);
  if TEnable then
    fGases.cbResult.Items.AddText(Properties[2]);
  if G1Enable or G2Enable then
    fGases.cbResult.Items.AddText(Properties[3]);
end;

{ Set position on volume trackbar }

procedure SetVolume(V: Real);

begin
  // Actual volume outside trackbar limits
  if (Round(10 * V) - fGases.tbVolume.Min < -1) or (Round(10 * V) - fGases.tbVolume.Max > 1) then begin
    fGases.laVolume.Font.Color := clRed;
    if Round(10 * V) - fGases.tbVolume.Min < -1 then
      fGases.tbVolume.Position := fGases.tbVolume.Min
    else
      fGases.tbVolume.Position := fGases.tbVolume.Max;
  end
  // Actual pressure within trackbar limits
  else begin
    fGases.laVolume.Font.Color := clDefault;
    fGases.tbVolume.Position := Round(10 * V);
  end;
end;

{ Set position on pressure trackbar }

procedure SetPressure(P: Real);

begin
  P /= 101.325;                                                                // transform to atm
  // Actual pressure outside trackbar limits
  if (Round(100 * P) - fGases.tbPressure.Min < -1) or (Round(100 * P) - fGases.tbPressure.Max > 1) then begin
    fGases.laPressure.Font.Color := clRed;
    if Round(100 * P) - fGases.tbPressure.Min < -1 then
      fGases.tbPressure.Position := fGases.tbPressure.Min
    else
      fGases.tbPressure.Position := fGases.tbPressure.Max;
  end
  // Actual pressure within trackbar limits
  else begin
    fGases.laPressure.Font.Color := clDefault;
    fGases.tbPressure.Position := Round(100 * P);
  end;
end;

{ Set position on temperature trackbar }

procedure SetTemperature(T: Real; TMin, TMax: Integer);

begin
  T -= 273.15;                                                                 // transform to °C
  // Change trackbar limits
  fGases.tbTemperature.Min := TMin; fGases.tbTemperature.Max := TMax;
  // Actual temperature outside trackbar limits
  if (Round(T) - fGases.tbTemperature.Min < -1) or (Round(T) - fGases.tbTemperature.Max > 1) then begin
    if Round(T) - fGases.tbTemperature.Max > 1 then
      fGases.tbTemperature.Position := fGases.tbTemperature.Max
    else
      fGases.tbTemperature.Position := fGases.tbTemperature.Min;
    fGases.laTemperature.Font.Color := clRed;
  end
  // Actual temperature within trackbar limits
  else begin
    fGases.tbTemperature.Position := Round(T);
    fGases.laTemperature.Font.Color := clDefault;
  end;
end;

{ Set position on gas quantity trackbar }

procedure SetQuantity(N: Real; Gas: Integer);

var
  tbGas: TTrackbar;
  laGas: TLabel;

begin
  // Choose Gas 1 or gas 2 trackbar
  if Gas = 1 then begin
    laGas := fGases.laGas1;
    tbGas := fGases.tbGas1;
  end
  else begin
    laGas := fGases.laGas2;
    tbGas := fGases.tbGas2;
  end;
    // Actual gas quantity outside trackbar limits
  if (Round(100 * N) - tbGas.Min < -1) or (Round(100 * N) - tbGas.Max > 1) then begin
    laGas.Font.Color := clRed;
    if Round(100 * N) - tbGas.Min < -1 then
      tbGas.Position := tbGas.Min
    else
      tbGas.Position := tbGas.Max;
  end
    // Actual temperature inside trackbar limits
  else begin
    laGas.Font.Color := clDefault;
    tbGas.Position := Round(100 * N);
  end;
end;

{ Determine quantity for each of the 2 gases (from total quantity) }

procedure GasQuantities(NOld, N: Real; var N1, N2: Real);

var
  NDiff: Real;

begin
  // As far as possible, half of the total gas quantity is added to / subtracted from each of the 2 individual
  // gas quantities. If this results in a value outside the corresponding trackbar limits, the individual gas
  // quantities are adapted in order to fit into the desired interval.
  NDiff := N - NOld;
  N1 += NDiff / 2; N2 := N - N1;                                               // try same quantity changes for both gases
  // Total gas quantity greater than before
  if NDiff > 0 then begin
    if not (N - 3.57 > 0.01) then begin                                        // adapt values only if total gas quantity within limits
      if N2 - 1.57 > 0.01 then begin                                           // second gas quantity outside limits (greater than 1.57)
        N2 := 1.57; N1 := N - N2;
      end
      else if N1 - 2 > 0.01 then begin                                         // first gas quantity outside limits (greater than 2)
        N1 := 2; N2 := N - N1;
      end;
    end;
  end
  // Total gas quantity less than before
  else begin
    if N2 < 0 then begin                                                       // second gas quantity outside limits (less than 0)
      N2 := 0; N1 := N;
    end
    else if N1 - 0.45 < -0.01 then begin                                       // first gas quantity outside limits (less than 0.45)
      N1 := 0.45; N2 := N - N1;
      if N2 < 0 then begin
        N2 := 0; N1 := N;
      end;
    end
  end;
end;

{***********}
{* TfGases *}
{***********}

{ Application start: Initialisation = Set all default values }

procedure TfGases.FormCreate(Sender: TObject);

begin
  iMolecules1 := -1; iMolecules2 := -1;                                        // no molecules (shapes) yet created
  AppReset(rV, rP, rT, rN, rN1, rN2);
  rbIdeal.Checked := True;
  MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
  TemperatureDraw(rT); PressureDraw(rP);
  ResultsEnable(True, True, True, True, True);
  TrackbarsEnable(False, False, False, False, False);
  btCalc.Visible := True; btCalc.Enabled := False;
  Randomize;
end;

{ Menu item "File > Reset": Reset application to default values }

procedure TfGases.mFileResetClick(Sender: TObject);

begin
  AppReset(rV, rP, rT, rN, rN1, rN2);
  MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
  TemperatureDraw(rT); PressureDraw(rP);
end;

{ Menu item "File > Exit": Exit application }

procedure TfGases.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Program help": Display program help }

procedure TfGases.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > Program about": Display program info }

procedure TfGases.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if popAbout.Visible then
    popAbout.Hide
  else begin
    S := 'Chemistry: Determination of gas properties based on the Ideal Gas Law.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, March, 2018';
    popAbout.Text := S;
    popAbout.Show;
  end;
end;

{ Select Boyle’s law (Volume and pressure) }

procedure TfGases.rbBoyleChange(Sender: TObject);

var
  S: string;

begin
  S := 'Boyle’s law: The volume of a given amount of gas held at constant temperature is inversely ';
  S += 'proportional to the pressure under which it is measured.';
  memoLaw.Clear; memoLaw.Lines.AddText(S);
  S := 'Use "Volume" track bar to change gas volume and calculate corresponding gas pressure.' + Chr(13);
  S += 'Use "Pressure" track bar to change gas pressure and calculate corresponding gas volume.';
  memoDescription.Clear; memoDescription.Lines.AddText(S);
  TrackbarsEnable(True, True, False, False, False);                            // disable trackbars, except volume and pressure
  ResultsEnable(True, True, False, False, False);
  btCalc.Visible := False;
end;

{ Select Amontons’s law (Pressure and temperature) }

procedure TfGases.rbAmontonsChange(Sender: TObject);

var
  T: Real;
  S: string;

begin
  S := 'Amontons’s law (Gay-Lussac’s law): The pressure of a given amount of gas is directly proportional to its temperature ';
  S += 'on the kelvin scale when the volume is held constant.';
  memoLaw.Clear; memoLaw.Lines.AddText(S);
  S := 'Use "Pressure" track bar to change gas pressure and calculate corresponding gas temperature.' + Chr(13);
  S += 'Use "Temperature" track bar to change gas temperature and calculate corresponding gas pressure.';
  memoDescription.Clear;
  memoDescription.Lines.AddText(S);
  if rbAmontons.Checked then begin
    // Change limits of temperature trackbar (Min-197, max=338)
    T := rT;
    SetTemperature(rT, -197, 338);
    rT := T;
  end;
  TrackbarsEnable(False, True, True, False, False);                            // disable trackbars, except pressure and temperature
  ResultsEnable(False, True, True, False, False);
  btCalc.Visible := False;
end;

{ Select Charles’s law (Volume and temperature) }

procedure TfGases.rbCharlesChange(Sender: TObject);

var
  T: Real;
  S: string;

begin
  S := 'Charles’s law: The volume of a given amount of gas is directly proportional to its temperature ';
  S += 'on the kelvin scale when the pressure is held constant.';
  memoLaw.Clear; memoLaw.Lines.AddText(S);
  S := 'Use "Volume" track bar to change gas volume and calculate corresponding gas temperature.' + Chr(13);
  S += 'Use "Temperature" track bar to change gas temperature and calculate corresponding gas volume.';
  memoDescription.Clear; memoDescription.Lines.AddText(S);
  if rbCharles.Checked then begin
    // Change limits of temperature trackbar (Min-150, max=702)
    T := rT;
    SetTemperature(rT, -150, 702);
    rT := T;
  end;
  TrackbarsEnable(True, False, True, False, False);                            // disable trackbars, except volume and temperature
  ResultsEnable(True, False, True, False, False);
  btCalc.Visible := False;
end;

{ Select Avogadro’s law (Volume and quantity) }

procedure TfGases.rbAvogadroChange(Sender: TObject);

var
  S: string;

begin
  S := 'Avogadro’s law: For a confined gas, the volume and number of moles are directly proportional ';
  S += 'if the pressure and temperature both remain constant.';
  memoLaw.Clear; memoLaw.Lines.AddText(S);
  S := 'Use "Volume" track bar to change gas volume and calculate corresponding gas quantity (number of moles).' + Chr(13);
  S += 'Use "Gas quantity" track bars to change gas 1 / gas 2 quantity (number of moles) and calculate corresponding gas volume.';
  memoDescription.Clear; memoDescription.Lines.AddText(S);
  TrackbarsEnable(True, False, False, True, True);                             // disable trackbars, except volume and both gases' quantity
  ResultsEnable(True, False, False, True, True);
  btCalc.Visible := False;
end;

{ Select Ideal Gases law (with 3 gas properties known, determine the 4th one) }

procedure TfGases.rbIdealChange(Sender: TObject);

var
  T: Real;
  S: string;

begin
  S := 'Ideal gas law: A relation between the pressure, volume, temperature, and number of moles of an ideal gas, ';
  S += 'obtained by combining Boyle’s, Amontons’s, Charles’s and Avogadro’s laws.';
  memoLaw.Clear; memoLaw.Lines.AddText(S);
  S := 'In the "Value to be calculated" combobox, choose the gas property you want to determine. Use the track bars ';
  S += 'to assign values to the other gas properties. When done, push the "Calculate" button to do the calculation.';
  memoDescription.Clear; memoDescription.Lines.AddText(S);
  if rbIdeal.Checked then begin
    // Change limits of temperature trackbar (Min-150, max=702)
    T := rT;
    SetTemperature(rT, -197, 702);
    rT := T;
  end;
  TrackbarsEnable(False, False, False, False, False);                          // disable all trackbars until the property to be calculated has been selected
  cbResult.Enabled := True; ResultsEnable(True, True, True, True, True);
  btCalc.Visible := True; btCalc.Enabled := False;
end;

{ Gas volume changed (by changing actual position on trackbar) }

procedure TfGases.tbVolumeChange(Sender: TObject);

var
  NOld, G: Real;

begin
  rV := tbVolume.Position / 10;                                                // read gas volume from trackbar
  if (Round(10 * rV) - tbVolume.Min > -1) and (Round(10 * rV) - tbVolume.Max < 1) then
    laVolume.Font.Color := clDefault;                                          // reset "overflow condition" (could be so from previous actions)
  // Boyle's law selected: calculate new pressure value
  if rbBoyle.Checked then begin
    rP := GasPressure(rV, rT, rN);
    // Change position on pressure trackbar
    G := rP;
    SetPressure(rP);
    rP := G;
    cbResult.Text := 'Pressure'; cbResult.Enabled := False;
    PressureDraw(rP);
  end
  // Charles's law selected: calculate new temperature value
  else if rbCharles.Checked then begin
    rT := GasTemperature(rV, rP, rN);
    // Change position on temperature trackbar
    G := rT;
    SetTemperature(rT, -150, 702);
    rT := G;
    cbResult.Text := 'Temperature'; cbResult.Enabled := False;
    TemperatureDraw(rT);
  end
  // Avogadro's law selected: determine new quantity values
  else if rbAvogadro.Checked then begin
    NOld := rN;
    rN := GasQuantity(rV, rP, rT);
    GasQuantities(NOld, rN, rN1, rN2);                                         // determine quantity for the 2 gases
    // Change position on gas 1 quantity trackbar
    G := rN1;
    SetQuantity(rN1, 1);
    rN1 := G;
    // Change position on gas 2 quantity trackbar
    G := rN2;
    SetQuantity(rN2, 2);
    rN2 := G;
    cbResult.Text := 'Gas quantity'; cbResult.Enabled := False;
  end;
  DisplayGasValues(rV, rP, rT, rN, rN1, rN2);
  MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
end;

{ Gas pressure changed (by changing actual position on trackbar) }

procedure TfGases.tbPressureChange(Sender: TObject);

var
  G: Real;

begin
  rP := tbPressure.Position / 100;                                             // read gas pressure from trackbar
  if (Round(100 * rP) - tbPressure.Min > -1) and (Round(100 * rP) - tbPressure.Max < 1) then
    laPressure.Font.Color := clDefault;                                        // reset "overflow condition" (could be so from previous actions)
  rP *= 101.325;                                                               // transform to kPa
  // Boyle's law selected: calculate new volume value
  if rbBoyle.Checked then begin
    rV := GasVolume(rP, rT, rN);
    // Change position on volume trackbar
    G := rV;
    SetVolume(rV);
    rV := G;
    cbResult.Text := 'Volume'; cbResult.Enabled := False;
    MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
  end
  // Amontons's law selected: calculate new temperature value
  else if rbAmontons.Checked then begin
    rT := GasTemperature(rV, rP, rN);
    // Change position on temperature trackbar
    G := rT;
    SetTemperature(rT, -197, 338);
    rT := G;
    cbResult.Text := 'Temperature'; cbResult.Enabled := False;
    TemperatureDraw(rT);
  end;
  DisplayGasValues(rV, rP, rT, rN, rN1, rN2);
  PressureDraw(rP);
end;

{ Gas temperature changed (by changing actual position on trackbar) }

procedure TfGases.tbTemperatureChange(Sender: TObject);

var
  G: Real;

begin
  rT := tbTemperature.Position;                                                // read gas temperature from trackbar
  if (Round(rT) - tbTemperature.Min > -1) and (Round(rT) - tbTemperature.Max < 1) then
    laTemperature.Font.Color := clDefault;                                     // reset "overflow condition" (could be so from previous actions)
  rT += 273.15;                                                                // transform to K
  // Charles's law selected: calculate new volume value
  if rbCharles.Checked then begin
    rV := GasVolume(rP, rT, rN);
    // Change position on volume trackbar
    G := rV;
    SetVolume(rV);
    rV := G;
    cbResult.Text := 'Volume'; cbResult.Enabled := False;
    MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
  end
  // Amontons's law selected: calculate new pressure value
  else if rbAmontons.Checked then begin
    rP := GasPressure(rV, rT, rN);
    // Change position on pressure trackbar
    G := rP;
    SetPressure(rP);
    rP := G;
    cbResult.Text := 'Pressure'; cbResult.Enabled := False;
    PressureDraw(rP);
  end;
  DisplayGasValues(rV, rP, rT, rN, rN1, rN2);
  TemperatureDraw(rT);
end;

{ Gas 1 quantity changed (by changing actual position on trackbar) }

procedure TfGases.tbGas1Change(Sender: TObject);

var
  G: Real;

begin
  rN1 := tbGas1.Position / 100;                                                // read gas 1 quantity from trackbar
  rN := rN1 + rN2;                                                             // calculate total gas quantity
  if (Round(100 * rN1) - tbGas1.Min > -1) and (Round(100 * rN1) - tbGas1.Max < 1) then
    laGas1.Font.Color := clDefault;                                            // reset "overflow condition" (could be so from previous actions)
  // Avogadro's law selected: calculate new volume value
  if rbAvogadro.Checked then begin
    rV := GasVolume(rP, rT, rN);
    // Change position on volume trackbar
    G := rV;
    SetVolume(rV);
    rV := G;
    cbResult.Text := 'Volume'; cbResult.Enabled := False;
  end;
  DisplayGasValues(rV, rP, rT, rN, rN1, rN2);
  MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
end;

{ Gas 2 quantity changed (by changing actual position on trackbar) }

procedure TfGases.tbGas2Change(Sender: TObject);

var
  G: Real;

begin
  rN2 := tbGas2.Position / 100;                                                // read gas 2 quantity from trackbar
  rN := rN1 + rN2;                                                             // calculate total gas quantity
  if (Round(100 * rN2) - tbGas2.Min > -1) and (Round(100 * rN2) - tbGas2.Max < 1) then
    laGas2.Font.Color := clDefault;                                            // reset "overflow condition" (could be so from previous actions)
  // Avogadro's law selected: calculate new volume value
  if rbAvogadro.Checked then begin
    rV := GasVolume(rP, rT, rN);
    // Change position on volume trackbar
    G := rV;
    SetVolume(rV);
    rV := G;
    cbResult.Text := 'Volume'; cbResult.Enabled := False;
  end;
  DisplayGasValues(rV, rP, rT, rN, rN1, rN2);
  MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
end;

{ Ideal gas law: Select gas property to be calculated }

procedure TfGases.cbResultChange(Sender: TObject);

var
  IX: Integer;

begin
  // Applicable only for "Ideal gas law" selection
  if rbIdeal.Checked then begin
    IX := cbResult.ItemIndex;
    TrackbarsEnable(True, True, True, True, True);                             // enable all trackbars, then disable the one corresponding to the property to be calculated
    ResultsEnable(True, True, True, True, True);
    // Disable trackbar for the property to be calculated
    case IX of
      0: begin
           tbVolume.Enabled := False;
         end;
      1: begin
           tbPressure.Enabled := False;
         end;
      2: begin
           tbTemperature.Enabled := False;
         end;
      3: begin
           tbGas1.Enabled := False; tbGas2.Enabled := False;
         end;
    end;
    cbResult.Text := Properties[IX];                                           // gas property to be calculated
    btCalc.Enabled := True;
  end;
end;

{ Ideal gas law: Calculate selected gas property (when "Calculate" button is pressed) }

procedure TfGases.btCalcClick(Sender: TObject);

var
  IX: Integer;
  NOld, G: Real;

begin
  IX := cbResult.ItemIndex;                                                    // read property to be calculated from combobox
  // Calculate selected gas property
  case IX of
    // Calculate gas volume
    0: begin
         rV := GasVolume(rP, rT, rN);
         // Change position on volume trackbar
         G := rV;
         SetVolume(rV);
         rV := G;
         MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
       end;
    // Calculate gas pressure
    1: begin
         rP := GasPressure(rV, rT, rN);
         // Change position on pressure trackbar
         G := rP;
         SetPressure(rP);
         rP := G;
         PressureDraw(rP);
       end;
    // Calculate gas temperature
    2: begin
         rT := GasTemperature(rV, rP, rN);
         // Change position on temperature trackbar
         G := rT;
         SetTemperature(rT, -197, 702);
         rT := G;
         TemperatureDraw(rT);
       end;
    // Determine gas 1 and gas 2 quantity
    3: begin
         NOld := rN;
         rN := GasQuantity(rV, rP, rT);
         GasQuantities(NOld, rN, rN1, rN2);                                         // determine quantity for the 2 gases
         // Change position on gas 1 quantity trackbar
         G := rN1;
         SetQuantity(rN1, 1);
         rN1 := G;
         // Change position on gas 2 quantity trackbar
         G := rN2;
         SetQuantity(rN2, 2);
         rN2 := G;
         MoleculesDraw(rV, rN1, rN2, iMolecules1, iMolecules2);
       end;
  end;
  DisplayGasValues(rV, rP, rT, rN, rN1, rN2);
end;

end.

