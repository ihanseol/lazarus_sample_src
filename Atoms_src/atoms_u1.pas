{***********************************}
{* Main unit for Atoms application *}
{***********************************}

unit atoms_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, Grids, atoms_u2;

const
  NMaxElements = 118;
  NMaxShells = 7;
  NMaxSubshells = 19;

type
  TElement = record
    Symbol, Name: string;
    AtomicNumber, AtomicMassNumber: Integer;
    ElementGroup: string;
    Stable: Boolean;
  end;
  TElements = array[1 .. NMaxElements] of TElement;
  TOrbital = record
    Symbol: Char;
    MaxElectrons: Integer;
  end;
  TOrbitals = array[0 .. 3] of TOrbital;
  TShellCounts = array[1 .. NMaxShells] of Integer;
  {*********}
  { TfAtoms }
  {*********}
  TfAtoms = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsSort: TMenuItem;
    mSettingsSort1, mSettingsSort2, mSettingsSort3: TMenuItem;
    mHelp, mHelpChemistry, mHelpProgram, mHelpAbout: TMenuItem;
    imGraph: TImage;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10, Label11: TLabel;
    cobElements: TComboBox;
    edAtomicNumber: TEdit;
    edMassNumber: TEdit;
    edIsotopeMass: TEdit;
    edIonCharge: TEdit;
    edElectrons: TEdit;
    edProtons: TEdit;
    edNeutrons: TEdit;
    btDraw: TButton;
    btClear: TButton;
    sgShells: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsSort1Click(Sender: TObject);
    procedure mSettingsSort2Click(Sender: TObject);
    procedure mSettingsSort3Click(Sender: TObject);
    procedure mHelpChemistryClick(Sender: TObject);
    procedure mHelpProgramClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    iWidth, iHeight: Integer;
    clElectron, clProton, clNeutron: TColor;
    aElements: TElements;
    Bitmap : TBitmap;
  end;

const
  Subshells: array[0..NMaxSubshells - 1] of string = (
    '1s', '2s', '2p', '3s', '3p', '4s', '3d', '4p', '5s', '4d', '5p', '6s', '4f', '5d', '6p', '7s', '5f', '6d', '7p'
  );
  Orbitals: TOrbitals = (
    (Symbol: 's'; MaxElectrons: 2),
    (Symbol: 'p'; MaxElectrons: 6),
    (Symbol: 'd'; MaxElectrons: 10),
    (Symbol: 'f'; MaxElectrons: 14)
  );
  ElectronColor = $9AFA00;
  ProtonColor = $B469FF;
  NeutronColor = $E16941;

var
  fAtoms: TfAtoms;

implementation

{$R *.lfm}

{ Read elements data from file }

procedure ReadElements(out Elements: TElements);

var
  I: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'elements.txt'); Reset(InFile);
  I := 0;
  while not EoF(InFile) do begin
    readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin                   // ligne starting with '#' is comment
      Inc(I);
      with Elements[I] do begin
        Symbol := Trim(Copy(Line, 5, 2));                                      // columns 5-6    element symbol
        Name := Trim(Copy(Line, 8, 13));                                       // columns 8-20   element name
        AtomicNumber := StrToInt(Copy(Line, 1, 3));                            // columns 1-3    atomic number (Z)
        AtomicMassNumber := StrToInt(Copy(Line, 26, 3));                       // columns 26-28  atomic mass number (A)
        ElementGroup := Copy(Line, 22, 2);                                     // columns 22-23  element group code
        if Copy(Line, 25, 1) = '*' then                                        // column  25     '*' indicating an instable element
          Stable := False
        else
          Stable := True;
      end;
    end;
  end;
  Close(InFile);
end;

{ Fill elements into combobox }

procedure FillElementList(var Elements: TElements);

var
  I: Integer;
  S: string;

begin
  fAtoms.cobElements.Clear;
  for I := 1 to NMaxElements do begin
    S := IntToStr(Elements[I].AtomicNumber);
    if Length(S) = 1 then
      S := '  ' + S
    else if Length(S) = 2 then
      S := ' ' + S;
    S += '  ' + Elements[I].Symbol;
    if Length(Elements[I].Symbol) = 1 then
      S += ' ';
    S += '  ' + Elements[I].Name;
    fAtoms.cobElements.Items.AddText(S);
  end;
end;

{ Sort combobox elements }

procedure SortElementList(SortOption: Integer; var Elements: TElements);

var
  I, J: Integer;
  Swap: Boolean;
  Element: TElement;

begin
  for I := 1 to NMaxElements - 1 do begin
    for J := I + 1 to NMaxElements do begin
      Swap := False;
      case SortOption of
        1: if Elements[I].AtomicNumber > Elements[J].AtomicNumber then         // sort by atomic number
             Swap := True;
        2: if Elements[I].Symbol > Elements[J].Symbol then                     // sort by element symbol
             Swap := True;
        3: if Elements[I].Name > Elements[J].Name then                         // sort by element name
             Swap := True;
      end;
      if Swap then begin
        Element := Elements[I]; Elements[I] := Elements[J]; Elements[J] := Element;
      end;
    end;
  end;
  FillElementList(Elements)                                                    // re-fill the combobox
end;

{ Clear the drawing surface by displaying a white rectangle }

procedure GraphClear(W, H: Integer);

begin
  fAtoms.imGraph.Picture.Bitmap.Canvas.Pen.Color := clWhite;
  fAtoms.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fAtoms.imGraph.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw circles to represent the shells }

procedure DrawShells(W, H, NShells: Integer);

var
  CenterX, CenterY, Radius, I: Integer;

begin
  CenterX := W div 2; CenterY := H div 2;
  for I := NShells - 1 downto 0 do begin
    fAtoms.imGraph.Picture.Bitmap.Canvas.Brush.Color := clWhite;
    fAtoms.imGraph.Picture.Bitmap.Canvas.Pen.Color := clBlack;
    Radius := (H div 4) + I * (H div 4) div (NMaxShells - 1) - 20;
    fAtoms.imGraph.Picture.Bitmap.Canvas.EllipseC(CenterX, CenterY, Radius, Radius);
  end;
end;

{ Display the color legend }

procedure ColorLegend(W, H, E, P, N: TColor);

begin
  fAtoms.imGraph.Picture.Bitmap.Canvas.Font.Size := 10;
  fAtoms.imGraph.Picture.Bitmap.Canvas.Font.Color := P;
  fAtoms.imGraph.Picture.Bitmap.Canvas.TextOut(W - 70, H - 60, 'Protons');
  fAtoms.imGraph.Picture.Bitmap.Canvas.Font.Color := N;
  fAtoms.imGraph.Picture.Bitmap.Canvas.TextOut(W - 70, H - 45, 'Neutrons');
  fAtoms.imGraph.Picture.Bitmap.Canvas.Font.Color := E;
  fAtoms.imGraph.Picture.Bitmap.Canvas.TextOut(W - 70, H - 30, 'Electrons');
end;

{ Draw the nucleus }

procedure DrawNucleus(W, H, Protons, Neutrons: Integer; ProtonsColor, NeutronsColor: TColor);

var
  CenterX, CenterY, X, Y, NLength, NHeight, I: Integer;
  Colour: TColor;

begin
  CenterX := W div 2; CenterY := H div 2;
  NLength := Round(Int(Sqrt(Protons + Neutrons))); NHeight := NLength;
  if Protons + Neutrons > NLength * NHeight then begin
    if (Protons + Neutrons) - (NLength * NHeight) > NLength then
      NLength += 1;
    NHeight += 1;
  end;
  Y := CenterY - (10 * NHeight) div 2 + 5;
  X := CenterX - (10 * NLength) div 2;
  for I := 1 to Protons + Neutrons do begin
    if I <= Protons then                                                       // set color for protons
      Colour := ProtonsColor
    else                                                                       // set color for neutrons
      Colour := NeutronsColor;
    fAtoms.imGraph.Picture.Bitmap.Canvas.Brush.Color := Colour;
    fAtoms.imGraph.Picture.Bitmap.Canvas.Pen.Color := Colour;
    fAtoms.imGraph.Picture.Bitmap.Canvas.EllipseC(X, Y, 5, 5);
    X += 10;
    if X >= CenterX + (10 * NLength) div 2 then begin
      X := CenterX - (10 * NLength) div 2;
      Y += 10;
    end;
  end;
end;

{ Draw the electrons (onto the shell circles) }

procedure DrawElectrons(W, H, Electrons: Integer; var ShellCounts: TShellCounts; ElectronsColor: TColor);

var
  CenterX, CenterY, Radius, X, Y, SX, Count, I, J, K: Integer;
  Alpha, SAlpha: Real;

begin
  CenterX := W div 2; CenterY := H div 2;
  SX := 0; Count := 0; J := 0;
  for I := 1 to Electrons do begin
    if I > Count then begin
      Inc(SX);
      Count += ShellCounts[SX];
      Radius := (H div 4) + (SX - 1) * (H div 4) div (NMaxShells - 1) - 20;
      J := 0;
    end;
    Inc(J);
    // 1 to 4 electrons
    if J < 5 then begin
      case J of
        1: begin X := Radius; Y := 0; end;
        2: begin X := -Radius; Y := 0; end;
        3: begin X := 0; Y := -Radius; end;
        4: begin X := 0; Y := Radius; end;
      end;
    end
    // More then 4 electrons
    else begin
      if J = 5 then begin
        // Calculate angle in order to evenly distribute the electrons on the shell circles
        if ShellCounts[SX] > 28 then
          SAlpha := (Pi / 2) / 8
        else if ShellCounts[SX] > 24 then
          SAlpha := (Pi / 2) / 7
        else if ShellCounts[SX] > 20 then
          SAlpha := (Pi / 2) / 6
        else if ShellCounts[SX] > 16 then
          SAlpha := (Pi / 2) / 5
        else if ShellCounts[SX] > 12 then
          SAlpha := (Pi / 2) / 4
        else if ShellCounts[SX] > 8 then
          SAlpha := (Pi / 2) / 3
        else
          SAlpha := (Pi / 2) / 2;
        Alpha := SAlpha; K := 0;
      end;
      Inc(K);
      // Display the electron in 1 of the 4 circle quadrants
      case K of
        1: begin X := Round(Radius * Cos(Alpha)); Y := -Round(Radius * Sin(Alpha)); end;
        2: begin X := -Round(Radius * Cos(Alpha)); Y := Round(Radius * Sin(Alpha)); end;
        3: begin X := Round(Radius * Cos(Alpha)); Y := Round(Radius * Sin(Alpha)); end;
        4: begin X := -Round(Radius * Cos(Alpha)); Y := -Round(Radius * Sin(Alpha)); end;
      end;
      // Increment the angle for display of next 4 electrons
      if K = 4 then begin
        Alpha += SAlpha; K := 0;
      end;
    end;
    X += CenterX; Y += CenterY;
    fAtoms.imGraph.Picture.Bitmap.Canvas.Brush.Color := ElectronsColor;
    fAtoms.imGraph.Picture.Bitmap.Canvas.Pen.Color := ElectronsColor;
    fAtoms.imGraph.Picture.Bitmap.Canvas.EllipseC(X, Y, 5, 5);
  end;
end;

{ Check if element belongs to the periodic table main group }

function MainGroup(Group: string): Boolean;

var
  MG: Boolean;

begin
  MG := True;
  if (Group = 'TM') or (Group = 'LA') or (Group = 'AC') then
    MG := False;
  MainGroup := MG;
end;

{ Determine the subshell configuration for a given (uncharged or charged atom) }

procedure SubshellConfiguration(Element: TElement; Charge: Integer; out Configuration: string; out SubshellCounts: array of Integer);

var
  Z, E, SsCount, SsMax, SX, SSX, I, J: Integer;
  Subshell: string;

begin
  for I := 0 to NMaxSubshells - 1 do
    SubshellCounts[I] := 0;
  Z := Element.AtomicNumber; E := Z;
  if Charge < 0 then                                                           // for cations, just add the suppl. electrons as usual
    E += Abs(Charge);
  Configuration := '';
  for I := 0 to 18 do
    SubshellCounts[I] := 0;
  SX := 0; SsCount := 0;
  // Count electrons in the different subshells (in order to add them to the subshells as previwed by the Aufbau principle)
  // Ignore anions for the moment (do as if there wasn't a charge )
  for I := 1 to E do begin                                                     // E = Z for atoms/anios, Z + ... for cations
    // Start a new subshell
    if SsCount = 0 then begin
      SubShell := Subshells[SX];
      // Determine maximum number of electons
      for J :=  0 to 3 do begin
        if RightStr(SubShell, 1)[1] = Orbitals[J].Symbol then
          SsMax := Orbitals[J].MaxElectrons;
      end;
    end;
    // Count electrons for this subshell
    Inc(SsCount);
    // Subshell has reached maximum of electrons
    if SsCount = SsMax then begin
      // Add the electrons counted to this subshell
      SubShellCounts[SX] := SsCount;
      // Point to next subshell
      Inc(SX);
      // Reset electron counter for the new subshell
      SsCount := 0;
    end
    // Add remaining electrons to the actual (last) subshell for actual element
    else if I = E then
      SubShellCounts[SX] := SsCount;
  end;
  // Correct the pointer to last subshell used for actual element
  if SsCount = 0 then
    Dec(SX);
  // Now, remove electrons for anions
  if Charge > 0 then begin
    // Special case for transition metals and inner transition metals
    if not MainGroup(Element.ElementGroup) then begin
      SSX := 0;
      if (Subshells[SX] = '3d') or (Subshells[SX] = '4d') or (Subshells[SX] = '4f') or (Subshells[SX] = '5f') or (Subshells[SX] = '5d') or (Subshells[SX] = '6d') then begin
        // Find s subshell where electrons have to be removed (before removing them from d or f subshells)
        if (Subshells[SX] = '3d') or (Subshells[SX] = '4d') or (Subshells[SX] = '4f') or (Subshells[SX] = '5f') then
          SSX := 1
        else
          SSX := 2;
        // Remove 1 or 2 electrons from s subshell
        if SSX <> 0 then begin
          if SubshellCounts[SX - SSX] >= Charge then begin
            SubshellCounts[SX - SSX] -= Charge;
            Charge := 0;
          end
          else begin
            Charge -= SubshellCounts[SX - SSX];
            SubshellCounts[SX - SSX] := 0;
          end;
        end;
      end;
    end;
  end;
  while Charge > 0 do begin
    // Remove electrons from subshells in inverse order than they have been added by the Aufbau principle
      if SubshellCounts[SX] >= Charge then begin
        SubshellCounts[SX] -= Charge;
        Charge := 0;
      end
      else begin
        Charge -= SubshellCounts[SX];
        SubshellCounts[SX] := 0;
        Dec(SX);
      end;
  end;
  // Create a string represenation of the actual element's electrons configuration
  for I := 0 to 18 do begin
    if SubshellCounts[I] <> 0 then begin
      Configuration += SubShells[I] + IntToStr(SubshellCounts[I]) + ' ';
    end;
  end;
end;

{ Count the electrons for each shell }

procedure ShellConfiguration(var SubshellCounts: array of Integer; out ShellCount: Integer; out ShellCounts: TShellCounts);

var
  I, SX: Integer;

begin
  ShellCount := 0;
  for I := 1 to NMaxShells do
    ShellCounts[I] := 0;
  for I := 0 to NMaxSubshells - 1 do begin
    SX := StrToInt(LeftStr(Subshells[I], 1));
    ShellCounts[SX] += SubshellCounts[I];
  end;
  for I := 1 to NMaxShells do begin
    if Shellcounts[I] <> 0 then
      Inc(ShellCount);
  end;
end;

{*********}
{ TfAtoms }
{*********}

{ Application start: Initialisation }

procedure TfAtoms.FormCreate(Sender: TObject);

begin
  // Drawing surface
  iWidth := imGraph.Width; iHeight := imGraph.Height;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iWidth;
  Bitmap.Height := iHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imGraph.Picture.Graphic := Bitmap;
  // Clear the drawing surface
  GraphClear(iWidth, iHeight);
  // Draw all 7 shells
  DrawShells(iWidth, iHeight, NMaxShells);
  // Read element data and fill the combobox
  ReadElements(aElements);
  FillElementList(aElements);
  clElectron := ElectronColor;
  clProton := ProtonColor;
  clNeutron := NeutronColor;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfAtoms.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Elements sort > ...": Sort combobox elements }

procedure TfAtoms.mSettingsSort1Click(Sender: TObject);

begin
  mSettingsSort1.Checked := True;
  mSettingsSort2.Checked := False;
  mSettingsSort3.Checked := False;
  SortElementList(1, aElements);
end;

procedure TfAtoms.mSettingsSort2Click(Sender: TObject);

begin
  mSettingsSort1.Checked := False;
  mSettingsSort2.Checked := True;
  mSettingsSort3.Checked := False;
  SortElementList(2, aElements);
end;

procedure TfAtoms.mSettingsSort3Click(Sender: TObject);

begin
  mSettingsSort1.Checked := False;
  mSettingsSort2.Checked := False;
  mSettingsSort3.Checked := True;
  SortElementList(3, aElements);
end;

{ Menu item "Help > Chemistry help": Display chemistry help text }

procedure TfAtoms.mHelpChemistryClick(Sender: TObject);

begin
  fHelp.memoHelp.Clear;
  fHelp.memoHelp.Lines.LoadFromFile('chemistry.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display program help text }

procedure TfAtoms.mHelpProgramClick(Sender: TObject);

begin
  fHelp.memoHelp.Clear;
  fHelp.memoHelp.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program about info }

procedure TfAtoms.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry: Graphical reresentation of atomic structure.' + LineEnding;
  S += 'Version 1.0, © allu, Nov. 2018 - Jan. 2019.';
  MessageDlg('About "Atoms"', S, mtInformation, [mbOK], 0);
end;

{ Button "Clear": Clear input fields }

procedure TfAtoms.btClearClick(Sender: TObject);

begin
  edIsotopeMass.Text := '';
  edIonCharge.Text := '';
  edAtomicNumber.Text := '';
  edMassNumber.Text := '';
  edElectrons.Text := ''; edElectrons.Font.Style := [];
  edProtons.Text := '';
  edNeutrons.Text := ''; edProtons.Font.Style := [];
end;

{ Button "Draw": Determine and draw the atomic structure for given (uncharged or charged) isotope }

procedure TfAtoms.btDrawClick(Sender: TObject);

const
  // UTF-8 codes for number superscripts
  SUP_Digits: array[0..9] of string = (
    #$E2#$81#$B0, #$C2#$B9, #$C2#$B2, #$C2#$B3, #$E2#$81#$B4, #$E2#$81#$B5, #$E2#$81#$B6, #$E2#$81#$B7, #$E2#$81#$B8, #$E2#$81#$B9
  );

var
  Z, A, Electrons, Protons, Neutrons, Charge, C1, C2, I, J, IX: Integer;
  ShellCount: Integer;
  Configuration, S, SS: string;
  Element: TElement;
  ShellCounts: TShellCounts;
  SubshellCounts: array[0 .. NMaxSubshells - 1] of Integer;

begin
  if cobElements.ItemIndex <> - 1 then begin
    S := cobElements.Items[cobElements.ItemIndex];
    if mSettingsSort1.Checked then
      Z := StrToInt(Trim(LeftStr(S, 3)));                                      // get atomic number from combobox
    for I := 0 to NMaxElements do
      if Z = aElements[I].AtomicNumber then
        IX := I;
    Element := aElements[IX];                                                  // get data for this element (as TElement record)
    A := Element.AtomicMassNumber;                                             // atomic mass number
    edAtomicNumber.Text := IntToStr(Z);
    edMassNumber.Text := IntToStr(A);
    // Determine number of protons, neutrons and electrons
    Protons := Z;
    Electrons := Protons;
    if edIonCharge.Text = '' then begin
      // Uncharged atoms
      Charge := 0;
      edElectrons.Font.Style := [];
    end
    else begin
      // Charged atoms (ions)
      Charge := StrToInt(edIonCharge.Text);
      Electrons += -Charge;                                                    // number of electrons <> Z
      edElectrons.Font.Style := [fsBold];
    end;
    if edIsotopeMass.Text = '' then begin
      // Periodic table isotope
      edNeutrons.Font.Style := [];
      Neutrons := A - Protons;
    end
    else begin
      // Non-periodic table isotope
      Neutrons := StrToInt(edIsotopeMass.Text) - Protons;                      // number of neutrons <> A - Z
      edNeutrons.Font.Style := [fsBold];
    end;
    edElectrons.Text := IntToStr(Electrons);
    edProtons.Text := IntToStr(Protons);
    edNeutrons.Text := IntToStr(Neutrons);
    // Determine subshell configuration
    SubshellConfiguration(Element, Charge, Configuration, SubshellCounts);
    // Determine shell configuration
    ShellConfiguration(SubshellCounts, ShellCount, ShellCounts);
    // Display shell and subshell configuration in stringgrid
    for I := 1 to NMaxShells do begin
      if ShellCounts[I] = 0 then begin
        sgShells.Cells[1, I] := '';
        sgShells.Cells[2, I] := '';
      end
      else begin
        S := '    ' + IntToStr(ShellCounts[I]);
        if Length(S) = 5 then
          S := ' ' + S;
        sgShells.Cells[1, I] := S;
        S := '';
        for J := 0 to NMaxSubshells - 1 do begin
          if (SubshellCounts[J] <> 0) and (StrToInt(LeftStr(Subshells[J], 1)) = I) then begin
            SS := SubShells[J];
            C1 := SubshellCounts[J] div 10; C2 := SubshellCounts[J] mod 10;
            if C1 <> 0 then
              SS += SUP_Digits[C1];
            SS += SUP_Digits[C2];
            S += SS + ' ';
          end;
        end;
        sgShells.Cells[2, I] := S;
      end;
    end;
    // Create the atomic structure graph
    GraphClear(iWidth, iHeight);
    DrawShells(iWidth, iHeight, ShellCount);
    ColorLegend(iWidth, iHeight, clElectron, clProton, clNeutron);
    DrawNucleus(iWidth, iHeight, Protons, Neutrons, clProton, clNeutron);
    DrawElectrons(iWidth, iHeight, Electrons, ShellCounts, clElectron);
  end;
end;

end.

