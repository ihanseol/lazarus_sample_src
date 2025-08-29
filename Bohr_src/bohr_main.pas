{**************************************}
{* Main unit for the Bohr application *}
{**************************************}

unit bohr_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Grids, Menus, bohr_help;

type
  {********}
  { TfBohr }
  {********}
  TfBohr = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsSeries, mOptionsColoration, mOptionsWidth, mOptionsSpectrum0, mOptionsEnergyJ: TMenuItem;
    mOptionsSeries1, mOptionsSeries2, mOptionsSeries3, mOptionsSeries4: TMenuItem;
    mOptionsSeries5, mOptionsSeriesAll3, mOptionsSeriesAll5: TMenuItem;
    mOptionsColorationColor, mOptionsColorationColor2, mOptionsColorationNone, MenuItem3, mOptionsColorationSeries: TMenuItem;
    mOptionsWidth1, mOptionsWidth2, mOptionsWidth3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    shOrbit1, shOrbit2, shOrbit3, shOrbit4, shOrbit5: TShape;
    shSpectrum, shAxis: TShape;
    Shape3, Shape7, Shape8, Shape9, Shape10: TShape;
    Shape11, Shape13, Shape14, Shape15, Shape16, Shape17: TShape;
    stTick1, stTick2, stTick3, stTick4, stTick5: TStaticText;
    laSeries, laSeries2, Label1, Label2: TLabel;
    cobSeries: TComboBox;
    sgLevelValues: TStringGrid;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsSeries1Click(Sender: TObject);
    procedure mOptionsSeries2Click(Sender: TObject);
    procedure mOptionsSeries3Click(Sender: TObject);
    procedure mOptionsSeries4Click(Sender: TObject);
    procedure mOptionsSeries5Click(Sender: TObject);
    procedure mOptionsSeriesAll3Click(Sender: TObject);
    procedure mOptionsSeriesAll5Click(Sender: TObject);
    procedure mOptionsColorationColorClick(Sender: TObject);
    procedure mOptionsColorationColor2Click(Sender: TObject);
    procedure mOptionsColorationNoneClick(Sender: TObject);
    procedure mOptionsColorationSeriesClick(Sender: TObject);
    procedure mOptionsWidth1Click(Sender: TObject);
    procedure mOptionsWidth2Click(Sender: TObject);
    procedure mOptionsWidth3Click(Sender: TObject);
    procedure mOptionsSpectrum0Click(Sender: TObject);
    procedure mOptionsEnergyJClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure cobSeriesChange(Sender: TObject);
  private
    iNF1, iNF2, iNFD, iWidth: Integer;
    sColoration: string;
    shOrbits: array[1..5] of TShape;
    shWaveLengths: array[1..35] of TShape;
    stTicks: array[0..4] of TStaticText;
  end;

const
  iR = 1.097E+7;                                                               // Rydberg constant
  rE0 = -13.6;                                                                 // hydrogen electron ground-state energy
  rEVtoJ = 1.6E-19;                                                            // eV to J conversion factor
  clIndigo = $82004B; clOrange = $00A5FF;
  clDarkRed = $00008B; clFireBrick = $2222B2; clIndianRed = $5C5CCD;
  clViolet = $EE82EE; clPlum = $DDA0DD; clThistle = $D8BFD8; clLavender = $FAE6E6;

var
  fBohr: TfBohr;

implementation

{$R *.lfm}

{ Fill electron energy values into table }

procedure FillEnergy(UEnergy: string; E0, EVtoJ: Real);

var
  N: Integer;
  E1, E: Real;
  ES: string;

begin
  E1 := E0;
  fBohr.sgLevelValues.Cells[2, 0] := 'Energy [' + UEnergy + ']';
  if UEnergy = 'J' then
    E1 *= EVtoJ;                                                               // energy values in joule
  for N := 1 to 10 do begin
    // 10 energy levels (ni = 1..10)
    E := E1 / Sqr(N);
    if UEnergy = 'eV' then begin
      ES := '  ' + FloatToStrF(E, ffFixed, 0, 3);                              // fixed format for eV values
      if N = 1 then
        Delete(ES, 1, 1);                                                      // delete one space for proper alignment
    end
    else begin
      ES := ' ' + FloatToStrF(E, ffExponent, 3, 2);                            // scientific notation for J values
    end;
    fBohr.sgLevelValues.Cells[2, N] := ES;
  end;
end;

{ Fill "series selection" combobox }

procedure FillSeries(N: Integer);

var
  I: Integer;

begin
  fBohr.cobSeries.Clear;
  for I := 1 to N do
    fBohr.cobSeries.Items.AddText('nf=' + IntToStr(I));                        // values from 1..3 resp. from 1..5
  fBohr.cobSeries.ItemIndex := 1;                                              // initial value: nf=2 (Balmer series)
end;

{ Get display color (table value) for given wavelength emission }

function GetDisplayColor(L: Real; Coloration: string): string;

var
  Colour: string;

begin
  Colour := '';
  // Human visible colors
  if (L > 380) and (L <= 425) then
    Colour := 'violet'
  else if (L > 425) and (L <= 445) then
    Colour := 'indigo'
  else if (L > 445) and (L <= 520) then
    Colour := 'blue'
  else if (L > 520) and (L <= 565) then
    Colour := 'green'
  else if (L > 565) and (L <= 590) then
    Colour := 'yellow'
  else if (L > 590) and (L <= 625) then
    Colour := 'orange'
  else if (L > 625) and (L <= 740) then
    Colour := 'red'
  // Infra-red
  else if L >= 780 then begin
    Colour := 'IR';
    if Coloration = 'color2' then begin
      // Extended band coloration
      if L <= 1400 then
        Colour := 'IR-A'
      else if L <= 3000 then
        Colour := 'IR-B'
      else
        Colour := 'IR-C';
    end;
  end
  // Ultra-violet
  else begin
    Colour := 'UV';
    if Coloration = 'color2' then begin
      // Extended band coloration
      if L <= 121 then
        Colour := 'E-UV'
      else if L <= 200 then
        Colour := 'F-UV'
      else if L <= 300 then
        Colour := 'M-UV'
      else
        Colour := 'N-UV';
    end;
  end;
  Result := Colour;
end;

{ Get color (spectrum bands) for given wavelength emission }

function GetColor(L: Real; Coloration: string): TColor;

var
  Colour: TColor;

begin
  // Human visible colors
  Colour := clBlack;
  if (L > 380) and (L <= 425) then
    Colour := clFuchsia
  else if (L > 425) and (L <= 445) then
    Colour := clIndigo
  else if (L > 445) and (L <= 520) then
    Colour := clBlue
  else if (L > 520) and (L <= 565) then
    Colour := clGreen
  else if (L > 565) and (L <= 590) then
    Colour := clYellow
  else if (L > 590) and (L <= 625) then
    Colour := clOrange
  else if (L > 625) and (L <= 740) then
    Colour := clRed
  // Infra-red
  else if L >= 780 then begin
    Colour := clDarkRed;
    if Coloration = 'color2' then begin
      // Extended band coloration
      if L <= 1400 then
        Colour := clIndianRed
      else if L <= 3000 then
        Colour := clFireBrick
      else
        Colour := clDarkRed;
    end;
  end
  // Ultra-violet
  else begin
    Colour := clPlum;
    if Coloration = 'color2' then begin
      // Extended band coloration
      if L <= 121 then
        Colour := clLavender
      else if L <= 200 then
        Colour := clThistle
      else if L <= 300 then
        Colour := clPlum
      else
        Colour := clViolet;
    end;
  end;
  Result := Colour;
end;

{********}
{ TfBohr }
{********}

{ Application start: Initialisatiuon }

procedure TfBohr.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create array with orbit shapes and spectrum axis static texts
  shOrbits[1] := shOrbit1; shOrbits[2] := shOrbit2; shOrbits[3] := shOrbit3;
  shOrbits[4] := shOrbit4; shOrbits[5] := shOrbit5;
  stTicks[0] := stTick1; stTicks[1] := stTick2; stTicks[2] := stTick3;
  stTicks[3] := stTick4; stTicks[4] := stTick5;
  // Create array with shape objects to be used as spectrum wave bands
  for I := 1 to 35 do begin
    shWavelengths[I] := TShape.Create(shWavelengths[I]);
    shWavelengths[I].Parent := Self;
    shWavelengths[I].Shape := stRectangle;
    shWavelengths[I].Height := shSpectrum.Height;
    shWavelengths[I].Top := shSpectrum.Top;
    shWavelengths[I].Visible := False;
  end;
  // Default values: nf = 2 (Balmer series); simple colors coloration
  iNF1 := 2; iNF2 := 2; sColoration := 'color'; iWidth := 2;
end;

{ Menu item "File > Exit": Exit application }

procedure TfBohr.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Emission series > ...": Select emission series (nf) }

procedure TfBohr.mOptionsSeries1Click(Sender: TObject);

begin
  mOptionsSeries1.Checked := True;     mOptionsSeries2.Checked := False; mOptionsSeries3.Checked := False;
  mOptionsSeries4.Checked := False;    mOptionsSeries5.Checked := False;
  mOptionsSeriesAll3.Checked := False; mOptionsSeriesAll5.Checked := False;
  iNF1 := 1; iNF2 := 1;
  if mOptionsColorationSeries.Enabled and mOptionsColorationSeries.Checked then begin
    // Disable access to "Emission series" coloration option (for one single series calculation)
    mOptionsColorationSeries.Click;
    mOptionsColorationSeries.Enabled := False;
  end;
  cobSeries.Visible := False;
end;

procedure TfBohr.mOptionsSeries2Click(Sender: TObject);

begin
  mOptionsSeries1.Checked := False;    mOptionsSeries2.Checked := True; mOptionsSeries3.Checked := False;
  mOptionsSeries4.Checked := False;    mOptionsSeries5.Checked := False;
  mOptionsSeriesAll3.Checked := False; mOptionsSeriesAll5.Checked := False;
  iNF1 := 2; iNF2 := 2;
  if mOptionsColorationSeries.Enabled and mOptionsColorationSeries.Checked then begin
    mOptionsColorationSeries.Click;
    mOptionsColorationSeries.Enabled := False;
  end;
  cobSeries.Visible := False;
end;

procedure TfBohr.mOptionsSeries3Click(Sender: TObject);

begin
  mOptionsSeries1.Checked := False;    mOptionsSeries2.Checked := False; mOptionsSeries3.Checked := True;
  mOptionsSeries4.Checked := False;    mOptionsSeries5.Checked := False;
  mOptionsSeriesAll3.Checked := False; mOptionsSeriesAll5.Checked := False;
  iNF1 := 3; iNF2 := 3;
  if mOptionsColorationSeries.Enabled and mOptionsColorationSeries.Checked then begin
    mOptionsColorationSeries.Click;
    mOptionsColorationSeries.Enabled := False;
  end;
  cobSeries.Visible := False;
end;

procedure TfBohr.mOptionsSeries4Click(Sender: TObject);

begin
  mOptionsSeries1.Checked := False;    mOptionsSeries2.Checked := False; mOptionsSeries3.Checked := False;
  mOptionsSeries4.Checked := True;     mOptionsSeries5.Checked := False;
  mOptionsSeriesAll3.Checked := False; mOptionsSeriesAll5.Checked := False;
  iNF1 := 4; iNF2 := 4;
  if mOptionsColorationSeries.Enabled and mOptionsColorationSeries.Checked then begin
    mOptionsColorationSeries.Click;
    mOptionsColorationSeries.Enabled := False;
  end;
  cobSeries.Visible := False;
end;

procedure TfBohr.mOptionsSeries5Click(Sender: TObject);

begin
  mOptionsSeries1.Checked := False;    mOptionsSeries2.Checked := False; mOptionsSeries3.Checked := False;
  mOptionsSeries4.Checked := False;    mOptionsSeries5.Checked := True;
  mOptionsSeriesAll3.Checked := False; mOptionsSeriesAll5.Checked := False;
  iNF1 := 5; iNF2 := 5;
  if mOptionsColorationSeries.Enabled and mOptionsColorationSeries.Checked then begin
    mOptionsColorationSeries.Click;
    mOptionsColorationSeries.Enabled := False;
  end;
  cobSeries.Visible := False;
end;

procedure TfBohr.mOptionsSeriesAll3Click(Sender: TObject);

// Lyman, Balmer and Paschen series

begin
  mOptionsSeries1.Checked := False;   mOptionsSeries2.Checked := False; mOptionsSeries3.Checked := False;
  mOptionsSeries4.Checked := False;   mOptionsSeries5.Checked := False;
  mOptionsSeriesAll3.Checked := True; mOptionsSeriesAll5.Checked := False;
  iNF1 := 1; iNF2 := 3;
  // Enable access to "Emission series" coloration option (for multiple series calculation)
  // Show combobox, where user can select the nf orbit, for which to display the wavelength values in the table
  mOptionsColorationSeries.Enabled := True;
  FillSeries(iNF2);
  cobSeries.Visible := True;
end;

procedure TfBohr.mOptionsSeriesAll5Click(Sender: TObject);

// All series for nf = 1..5

begin
  mOptionsSeries1.Checked := False;    mOptionsSeries2.Checked := False; mOptionsSeries3.Checked := False;
  mOptionsSeries4.Checked := False;    mOptionsSeries5.Checked := False;
  mOptionsSeriesAll3.Checked := False; mOptionsSeriesAll5.Checked := True;
  iNF1 := 1; iNF2 := 5;
  mOptionsColorationSeries.Enabled := True;
  FillSeries(iNF2);
  cobSeries.Visible := True;
end;

{ Menu items "Options > Wave coloration > ...": Select how to colorate the spectrum bands (and what to display as color in the table) }

procedure TfBohr.mOptionsColorationColorClick(Sender: TObject);

begin
  mOptionsColorationColor.Checked := True;  mOptionsColorationColor2.Checked := False;
  mOptionsColorationNone.Checked  := False; sColoration := 'color';
end;

procedure TfBohr.mOptionsColorationColor2Click(Sender: TObject);

begin
  mOptionsColorationColor.Checked := False; mOptionsColorationColor2.Checked := True;
  mOptionsColorationNone.Checked  := False; sColoration := 'color2';
end;

procedure TfBohr.mOptionsColorationNoneClick(Sender: TObject);

begin
  mOptionsColorationColor.Checked := False; mOptionsColorationColor2.Checked := False;
  mOptionsColorationNone.Checked  := True;  sColoration := 'none';
end;

procedure TfBohr.mOptionsColorationSeriesClick(Sender: TObject);

// This selection is special, because not part of general selection (independent toggle option)

begin
  if mOptionsColorationSeries.Checked then begin
    mOptionsColorationSeries.Checked := False;
    mOptionsColorationNone.Enabled := True;
  end
  else begin
    mOptionsColorationSeries.Checked := True;
    // "None" coloration does not make sense in "Emission series" coloration context
    if mOptionsColorationNone.Checked then
      mOptionsColorationColor.Click;
    mOptionsColorationNone.Enabled := False;
  end;
end;

{ Menu items "Options > Wave band width > ...": Select width (tickness) of the spectrum bands }

procedure TfBohr.mOptionsWidth1Click(Sender: TObject);

begin
  mOptionsWidth1.Checked := True;  mOptionsWidth2.Checked := False;
  mOptionsWidth3.Checked := False; iWidth := 1;
end;

procedure TfBohr.mOptionsWidth2Click(Sender: TObject);

begin
  mOptionsWidth1.Checked := False; mOptionsWidth2.Checked := True;
  mOptionsWidth3.Checked := False; iWidth := 2;
end;

procedure TfBohr.mOptionsWidth3Click(Sender: TObject);

begin
  mOptionsWidth1.Checked := False; mOptionsWidth2.Checked := False;
  mOptionsWidth3.Checked := True;  iWidth := 3;
end;

{ Menu item "Options > Spectrum origin at 0": Toogle origin of spectrum axis (minimum wavelength displayed or 0) }

procedure TfBohr.mOptionsSpectrum0Click(Sender: TObject);

begin
  if mOptionsSpectrum0.Checked then
    mOptionsSpectrum0.Checked := False
  else
    mOptionsSpectrum0.Checked := True;
end;

{ Menu item "Options > Energy values in joule": Toogle unit of energy values in table (eV or J) }

procedure TfBohr.mOptionsEnergyJClick(Sender: TObject);

begin
  if mOptionsEnergyJ.Checked then begin
    mOptionsEnergyJ.Checked := False;
    FillEnergy('eV', rE0, rEVtoJ);
  end
  else begin
    mOptionsEnergyJ.Checked := True;
    FillEnergy('J', rE0, rEVtoJ);
  end;
end;

{ Menu item "Help > Help": Display application help text) }

procedure TfBohr.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about) }

procedure TfBohr.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry: Bohr’s Theory of the Hydrogen Atom.' + LineEnding;
  S += 'Version 1.0, © allu, January 2022.';
  MessageDlg('About "Bohr"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculation" pushed: Do calculation, fill table, draw spectrum }

procedure TfBohr.btCalcClick(Sender: TObject);

const
  WCounts: array[1..5] of Integer = (
    // Number of waves sum (nf1, nf1 + nf2, etc), used to find back start of a series in an all waves array
    9, 17, 24, 30, 35
  );
  Colors: array[1..5] of TColor = (
    // "Emission series" coloration colors for nf = 1..5
    clFuchsia, clRed, clOrange, clLime, clAqua
  );

var
  NF, NI, LS, LSMax, N, I, J: Integer;
  Colour: TColor;
  LMin, LMax: Real;
  S: string;
  L: array[1..35] of Real;

begin
  // Clear the form
  for I := 1 to 5 do
    shOrbits[I].Pen.Color := clBlack;
  for I := 1 to 35 do
    shWaveLengths[I].Visible := False;
  for I := 0 to 4 do
    stTicks[I].Visible := False;
  for I := 1 to 10 do begin
    fBohr.sgLevelValues.Cells[3, I] := '';
    fBohr.sgLevelValues.Cells[4, I] := '';
  end;
  // Series for which wavelengths are displayed in table. Coloration of the electron orbits
  if iNF1 = iNF2 then begin
    // Single series calculation
    iNFD := iNF1;
    shOrbits[iNFD].Pen.Color := clBlue;                                        // highlighting of the nf orbit for which the calculation is done
  end
  else begin
    // Multiple series calculation
    iNFD := StrToInt(RightStr(cobSeries.Text, 1));                             // nf for table values is selected in combobox by user
    if mOptionsColorationSeries.Checked then begin
      // "Emission series" coloration
      for I := INF1 to INF2 do
        shOrbits[I].Pen.Color := Colors[I];                                    // color electron orbits the same way as the spectrum bands
    end;
  end;
  // Display series for which wavelength and color table values are actually displayed
  laSeries.Caption := 'nf = ' + IntToStr(iNFD);
  case iNFD of
    1: laSeries.Caption := laSeries.Caption + ' (Lyman)';
    2: laSeries.Caption := laSeries.Caption + ' (Balmer)';
    3: laSeries.Caption := laSeries.Caption + ' (Paschen)';
  end;
  laSeries.Visible := True; laSeries2.Visible := True;
  // Calculate wavelengths and fill in table values
  // Do this for each of the series for which the calculation has to be done
  N := 0; LMax := 0; LMin := MaxInt;
  for NF := iNF1 to iNF2 do begin
    // Do calculation for ni values from nf+1 to 10
    for NI := NF + 1 to 10 do begin
      // Calculate emission wave length
      Inc(N);
      L[N] := 1E+9 * (1 / (iR * (1 / Sqr(NF) - 1 / Sqr(NI))));                 // store wavelengths into sequential array
      if L[N] < LMin then
        LMin := L[N];                                                          // save minimum wave length (used for spectrum x-axis display)
      if L[N] > LMax then
        LMax := L[N];                                                          // save maximum wave length (used for spectrum x-values calculation)
      // For actual nf, fill in the table values
      if NF = iNFD then begin
        S := '     ' + FloatToStrF(L[N], ffFixed, 0, 0);
        if L[N] < 100 then
          S := '  ' + S
        else if L[N] < 1000 then
          S := ' ' + S;
        sgLevelValues.Cells[3, NI] := S;
        sgLevelValues.Cells[4, NI] := GetDisplayColor(L[N], sColoration);
      end;
    end;
  end;
  // Calculate and display wavelength values at the predefined ticks
  LSMax := shAxis.Width;
  for I := 0 to 4 do begin
    if fBohr.mOptionsSpectrum0.Checked then begin
      // Value for x-axis started at 0
      stTicks[I].Caption := FloatToStrF(I * LMax / 4, ffFixed, 0, 0);
    end
    else begin
      // Value for x-axis started with minimum wavelength
      stTicks[I].Caption := FloatToStrF(LMin + I * (LMax - LMin) / 4, ffFixed, 0, 0);
    end;
    stTicks[I].Visible := True;
  end;
  // Draw the spectrun bands (for all wavelengths calculated)
  Colour := clWhite;
  for I := 1 to N do begin
    // Calculate position of the band on the spectrum (depends on axis-start)
    if fBohr.mOptionsSpectrum0.Checked then
      LS := Round((L[I] / LMax) * LSMax) + shAxis.Left
    else
      LS := Round(((L[I] - LMin) / (LMax - LMin)) * LSMax) + shAxis.Left;
    // Display the spectrum band for actual wavelength
    shWavelengths[I].Left := LS;                                               // calculated position on the spectrum
    shWavelengths[I].Width := iWidth;                                          // band thickness (as selected by the user)
    if mOptionsColorationSeries.Checked then begin
      // Wavelength coloration = "Emission series": Use different colors, depending on series (nf)
      for J := 5 downto 1 do begin
        if I <= WCounts[J] then                                                // WCounts[J] contains number of bands sums (= start of series)
          Colour := Colors[J];
      end;
    end
    else begin
      // Wavelength coloration = "Wave color" or "Wave color (extended)" : Use different colors, depending on color wave (visible, IR, UV)
      if not mOptionsColorationNone.Checked then
        Colour := GetColor(L[I], sColoration);
    end;
    shWavelengths[I].Brush.Color := Colour;
    shWavelengths[I].Pen.Color := Colour;
    shWavelengths[I].Visible := True;
  end;
end;

{ Combobox changed by user: Redo calculations with new value for table wavelengths and colors values }

procedure TfBohr.cobSeriesChange(Sender: TObject);

begin
  btCalc.Click;
end;

end.

