{**************************************}
{* Main unit of Solutions application *}
{**************************************}

unit solutions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, help;

type
  TElement = record
    AtomicNumber: Integer;
    Name, Symbol: string;
    AtomicMass: Real;
  end;
  TElements = array of TElement;
  TSolvent = record
    Name, Formula, Formula2: string;
    Density: Real;
    List: Integer;
  end;
  TSolvents = array of TSolvent;
  TElementCount = record
    Element: string;
    NElement: Integer;
  end;
  TMoleculeAtoms = record
    NAtoms: Integer;
    Atoms: array of TElementCount;
  end;
  {*************}
  { TfSolutions }
  {*************}
  TfSolutions = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mOptions, mOptionsSolvents, mOptionsSolventsShort, mOptionsSolventsLong, mOptionsSolventsFormulas: TMenuItem;
    mOptionsAtoms, mOptionsAtoms0, mOptionsAtoms2, mOptionsAtoms3, mOptionsAtoms4, mOptionsAtoms5: TMenuItem;
    mOptionsDensity, mOptionsDensity2, mOptionsDensity3, mOptionsDensity4, mOptionsDensityH2O: TMenuItem;
    mOptionsConc, mOptionsConc2, mOptionsConc3, mOptionsConcMVmg: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout, MenuItem1, Separator1, Separator2: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label10, Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18, Label20: TLabel;
    Label21, Label23, Label24, Label25, Label27, Label28, Label29, Label33, Label30, Label31, Label35: TLabel;
    Label36, Label37, Label38, Label22, Label2, Label3, Label4, Label7, Label8, Label9: TLabel;
    laSolventUMass, laSoluteUMass, laUSoluteQty, laUQtySolute, laUQtySolvent: TLabel;
    laSolventName2, laSoluteQty, laSolventQty, laSolventName1: TLabel;
    edConcentration, edMassVolPercent, edQtySolute, edMolPercent, edQtySolvent, edSolventMass: TEdit;
    edSoluteMolWeight, edMassPercent, edSoluteDensity, edSoluteMass, edSolventMolWeight, edSolventName: TEdit;
    edVolPercent, edMolFractSolute, edSoluteQty, edMolality, edNormality, edMolarity, edSolventVolume: TEdit;
    edSolventDensity, edSoluteFormula: TEdit;
    cobSoluteKind, cobSolventName, cobSoluteType, cobUConcentration, cobUSoluteQty: TComboBox;
    btConcentration: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsSolventsShortClick(Sender: TObject);
    procedure mOptionsSolventsLongClick(Sender: TObject);
    procedure mOptionsSolventsFormulasClick(Sender: TObject);



    procedure btConcentrationClick(Sender: TObject);
    procedure cobSoluteKindChange(Sender: TObject);
    procedure cobSolventNameChange(Sender: TObject);
    procedure edSoluteFormulaEditingDone(Sender: TObject);
    procedure mOptionsAtoms0Click(Sender: TObject);
    procedure mOptionsAtoms2Click(Sender: TObject);
    procedure mOptionsAtoms3Click(Sender: TObject);
    procedure mOptionsAtoms4Click(Sender: TObject);
    procedure mOptionsAtoms5Click(Sender: TObject);
    procedure mOptionsDensity2Click(Sender: TObject);
    procedure mOptionsDensity3Click(Sender: TObject);
    procedure mOptionsDensity4Click(Sender: TObject);
    procedure mOptionsDensityH2OClick(Sender: TObject);
    procedure mOptionsConc2Click(Sender: TObject);
    procedure mOptionsConc3Click(Sender: TObject);
    procedure mOptionsConcMVmgClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
  private
    iAtomicMassDigits, iDensityList, iDensityDigits, iConcDigits, iSolute: Integer;
    rDensityWater, rSoluteDensity, rSoluteQty, rSolventDensity, rSolventVol, rSolventMass, rMolecularMass: Real;
    rSolventMolecularMass, rConcentration, rNormality, rMolarity, rMolality, rSoluteMass, rSoluteMol: Real;
    rSolventMol, rSoluteMolFraction, rSolventMolFraction, rMolPercent, rMassPercent, rVolPercent, rMassVolPercent: Real;
    sFormula, sMVConc: string;
    bDensityFormulas: Boolean;
    aElements: TElements;
    aSolvents: TSolvents;
  end;

var
  fSolutions: TfSolutions;

implementation

{$R *.lfm}

{ Apply chemical formula subscripts }

function ChemicalFormula(Formula: string): string;

var
  I: Integer;
  ChFormula: string;

begin
  ChFormula := Formula;
  for I := 0 to 9 do
    ChFormula := StringReplace(ChFormula, IntToStr(I), SUB_Digits[I], [rfReplaceAll]);
  Result := ChFormula;
end;

{ Remove chemical formula subscripts }

function TextFormula(Formula: string): string;

var
  I: Integer;
  TxFormula: string;

begin
  TxFormula := Formula;
  for I := 0 to 9 do
    TxFormula := StringReplace(TxFormula, SUB_Digits[I], IntToStr(I), [rfReplaceAll]);
  Result := TxFormula;
end;

{ Read elements (atomic masses) from text file }

procedure ReadElements(out Elements: TElements);

var
  N: Integer;
  Line, S: string;
  InFile: Text;

begin
  SetLength(Elements, 0);
  Assign(InFile, 'elements.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Elements, N);
      Elements[N - 1].AtomicNumber := StrToInt(Copy(Line, 1, 3));
      Elements[N - 1].Name := Trim(Copy(Line, 8, 15));
      Elements[N - 1].Symbol := Trim(Copy(Line, 5, 2));
      S := StringReplace(RightStr(Line, 9), '.', '', []);
      Elements[N - 1].AtomicMass := StrToInt(S) / 100000;
    end;
  end;
  Close(InFile);
end;

{ Read solvents from text file }

procedure ReadSolvents(out Solvents: TSolvents);

var
  N: Integer;
  Line, S: string;
  InFile: Text;

begin
  SetLength(Solvents, 0);
  Assign(InFile, 'solvents.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Solvents, N);
      Solvents[N - 1].Name := Trim(Copy(Line, 1, 25));
      Solvents[N - 1].Formula := Trim(Copy(Line, 26, 15));
      Solvents[N - 1].Formula2 := ChemicalFormula(Solvents[N - 1].Formula);
      S := StringReplace(Copy(Line, 41, 6), '.', '', []);
      Solvents[N - 1].Density := StrToInt(S) / 10000;
      Solvents[N - 1].List := StrToInt(Copy(Line, 51, 1));
    end;
  end;
  Close(InFile);
end;

{ Calculation of the nth power of real number }

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

{ Real number format (with removal of unsignificant decimal digits) }

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
        SR := FloatToStrF(R, ffExponent, F, 0)                                 // scientific notation if number to small to be correctly displayed
      else
        SR := FloatToStr(R0);                                                  // formatted floating-point display
    end;
  end;
  Result := SR;
end;

{ Get solvent name from formula given }

function GetSolventName(var Solvents: TSolvents; Solvent: string): string;

var
  I: Integer;
  Name: string;

begin
  Name := '';
  for I := 0 to Length(Solvents) - 1 do begin
    if Solvent = Solvents[I].Formula2 then
      Name := Solvents[I].Name;
  end;
  Result := Name;
end;

{ Get solvent formula from name given }

function GetSolventFormula(var Solvents: TSolvents; Solvent: string): string;

var
  I: Integer;
  Formula: string;

begin
  Formula := '';
  for I := 0 to Length(Solvents) - 1 do begin
    if Solvent = Solvents[I].Name then
      Formula := Solvents[I].Formula2;
  end;
  Result := Formula;
end;

{ Get density of given solvent }

function GetSolventDensity(var Solvents: TSolvents; Solvent: string): Real;

var
  I: Integer;
  Density: Real;

begin
  Density := 0;
  if ((Solvent = 'water') or (Solvent = ChemicalFormula('H2O'))) and fSolutions.mOptionsDensityH2O.Checked then
    Density := 1
  else begin
    for I := 0 to Length(Solvents) - 1 do begin
      if (Solvent = Solvents[I].Name) or (Solvent = Solvents[I].Formula2) then
        Density := Solvents[I].Density;
    end;
  end;
  Result := Density;
end;

{ Fill solvents combobox (short or long list) }

procedure FillSolvents(var Solvents: TSolvents; DensityWater: Real; List, DensityDigits: Integer; UseFormula: Boolean);

var
  N, NX, I: Integer;
  SName: string;

begin
  if UseFormula then begin
    fSolutions.laSolventName1.Caption := 'Formula';
    fSolutions.laSolventName2.Caption := 'Name';
  end
  else begin
    fSolutions.laSolventName1.Caption := 'Name';
    fSolutions.laSolventName2.Caption := 'Formula';
  end;
  fSolutions.cobSolventName.Clear;
  fSolutions.cobSolventName.Items.AddText('--custom--');                       // add "custom" list entry, allowing user to specify a density
  N := 0;
  for I := 0 to Length(Solvents) - 1 do begin
    if Solvents[I].List <= List then begin
      Inc(N); SName := Solvents[I].Name;
      if RightStr(SName, 1) = '*' then
        SName := LeftStr(SName, Length(SName) - 1);
      if Solvents[I].Name = 'water' then
        NX := N;
      if UseFormula then begin
        fSolutions.cobSolventName.Items.AddText(Solvents[I].Formula2);
        fSolutions.edSolventName.Text := SName;
      end
      else begin
        fSolutions.cobSolventName.Items.AddText(SName);
        fSolutions.edSolventName.Text := Solvents[I].Formula2;
      end;
    end;
  end;
  fSolutions.cobSolventName.ItemIndex := NX;                                   // default selection = "water"
  fSolutions.edSolventDensity.ReadOnly := True; fSolutions.edSolventDensity.TabStop := False;
  fSolutions.edSolventDensity.Color := clCream;
  fSolutions.edSolventDensity.Text := RFormat(DensityWater, DensityDigits);
end;

{ Fill quantity units combobox depending on how the quantity is expressed }

procedure FillQtyUnits(MolKind: string);

const
  MolMasses: array[0..1] of string = (
    'mmol', 'mol'
  );
  Masses: array[0..2] of string = (
    'mg', 'g', 'kg'
  );
  Volumes: array[0..3] of string = (
    'ml', 'cl', 'dl', 'L'
  );

var
  I: Integer;

begin
  fSolutions.cobUSoluteQty.Clear;
  if MolKind = 'molarity' then begin
    fSolutions.cobUSoluteQty.Visible := False;                                 // hide the combobox
    fSolutions.laUSoluteQty.Visible := True;                                   // display the "M" label
  end
  else begin
    fSolutions.cobUSoluteQty.Visible := True;                                  // show the combobox
    fSolutions.laUSoluteQty.Visible := False;                                  // hide the label
    if MolKind = 'molqty' then begin
      for I := 0 to 1 do
        fSolutions.cobUSoluteQty.Items.AddText(MolMasses[I]);
      fSolutions.cobUSoluteQty.ItemIndex := 1;
    end
    else if MolKind = 'mass' then begin
      for I := 0 to 2 do
        fSolutions.cobUSoluteQty.Items.AddText(Masses[I]);
      fSolutions.cobUSoluteQty.ItemIndex := 1;
    end
    else if MolKind = 'volume' then begin
      for I := 0 to 3 do
        fSolutions.cobUSoluteQty.Items.AddText(Volumes[I]);
      fSolutions.cobUSoluteQty.ItemIndex := 0;
    end;
  end;
end;

{ Get molecule's atoms and their occurences within the molecule }

procedure GetAtoms(SMolecule: string; out Molecule: TMoleculeAtoms);

var
  NAtms, NElemnt, Bracket1Elemnt1, Bracket2Elemnt1, BracketElemnt1, BracketElemnt2, NBracketElemnts, I, J: Integer;
  Elemnt, Mess: string;
  Temp: TMoleculeAtoms;

begin
  SetLength(Temp.Atoms, 0);
  NAtms := 0; Mess := '';
  I := 0;
  // Parse the string character by character
  while (I < Length(SMolecule)) and (Mess = '') do begin
    Inc(I);
    // This starts an atoms group
    if SMolecule[I] = '(' then
      Bracket1Elemnt1 := NAtms + 1                                             // remember this group's starting atom index
    // This starts another atoms group
    else if SMolecule[I] = '[' then
      Bracket2Elemnt1 := NAtms + 1                                             // remember this group's starting atom index
    // This ends the one or the other atoms groups
    else if (SMolecule[I] = ')') or (SMolecule[I] = ']') then begin
      // Get first atom's index (depending on which group it actually is)
      if SMolecule[I] = ')' then
        BracketElemnt1 := Bracket1Elemnt1
      else
        BracketElemnt1 := Bracket2Elemnt1;
      // The group's ending atom index
      BracketElemnt2 := NAtms;
      // Brackets are supposed to be followed by 1 or 2 numbers (of occurence)
      if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
        Inc(I);
        NBracketElemnts := StrToInt(SMolecule[I]);
        if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
          Inc(I);
          NBracketElemnts := 10 * NBracketElemnts + StrToInt(SMolecule[I]);
        end;
      end;
      // Multiply occurence of all atoms of the group by the number succeeding the bracket
      for J := BracketElemnt1 to BracketElemnt2 do
        Temp.Atoms[J].NElement *= NBracketElemnts;
    end
    // An uppercase letter indicates a new atom
    else begin
      if SMolecule[I] in ['A'..'Z'] then begin
        Elemnt := SMolecule[I]; NElemnt := 1; Inc(NAtms);
        // The atom may be just an uppercase letter or an uppercase plus a lowercase letter
        if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['a'..'z']) then begin
          Inc(I);
          Elemnt += SMolecule[I];
        end;
        // The atom letter(s) may be followed by 1 or 2 numbers (of occurence)
        if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
          Inc(I);
          NElemnt := StrToInt(SMolecule[I]);
          if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
            Inc(I);
            NElemnt := 10 * NElemnt + StrToInt(SMolecule[I]);
          end;
        end;
        // Fill in the molecule's atom and its number of occurences
        Temp.NAtoms := NAtms; SetLength(Temp.Atoms, NAtms + 1);
        Temp.Atoms[NAtms].Element := Elemnt;
        Temp.Atoms[NAtms].NElement := NElemnt;
      end
      // Something wrong with chemical formula (inexisting element symbol)
      else begin
        Mess := 'Invalid molecule formula!';
        MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
        Molecule.NAtoms := 0; SetLength(Molecule.Atoms, 0);
      end;
    end;
  end;
  // The temporary array so far filled-in generally contains several entries for a given element; create
  // the procedure output array by eliminating these doubles (and summing up all occurences for this element)
  // This is done in 2 steps: First add occurences and set element name in redundant array element to '', then
  // create final array by keeping only those array elements where the name is different from ''
  if Mess = '' then begin
    for I := 2 to Temp.NAtoms do begin
      for J := 1 to I - 1 do begin
        if Temp.Atoms[I].Element = Temp.Atoms[J].Element then begin
          Temp.Atoms[I].NElement += Temp.Atoms[J].NElement;
          Temp.Atoms[J].Element := '';
        end;
      end;
    end;
    NAtms := 0; SetLength(Molecule.Atoms, 0);
    for I := 1 to Temp.NAtoms do begin
      if Temp.Atoms[I].Element <> '' then begin
        Inc(NAtms);
        Molecule.NAtoms := NAtms; SetLength(Molecule.Atoms, NAtms + 1);
        Molecule.Atoms[NAtms] := Temp.Atoms[I];
      end;
    end;
  end;
end;

{ Molecular weight calculation }

function GetMolecularMass(var Molecule: TMoleculeAtoms; var Elements: TElements; Digits: Integer): Real;

var
  NAtom, I, J: Integer;
  Atom, Mess: string;
  AtomMass, MolMass: Real;

// Molecule is an array of records, containing atom symbols and their occurences within the molecule
// Elements is an array of records, containing the elements data read from the elements.txt file

begin
  MolMass := 0; Mess := '';
  I := 1;
  // Do this until all atoms processed or if an error (inexisting element) occurs
  while (I <= Molecule.NAtoms) and (Mess = '') do begin
    Atom := Molecule.Atoms[I].Element;                                         // atom symbol
    NAtom := Molecule.Atoms[I].NElement;                                       // occurences of this atom
    AtomMass := -1; J := 0;
    // Find atom in elements array
    while (J < Length(Elements)) and (AtomMass = -1) do begin
      if Atom = Elements[J].Symbol then
        AtomMass := Round(Power(10, Digits) * Elements[J].AtomicMass) / Power(10, Digits);
      Inc(J);
    end;
    // If atom is an existing element, add atom weight to total molecular weight
    if AtomMass <> -1 then begin
      AtomMass *= NAtom;
      MolMass += AtomMass;
    end
    // If atom is an unexisting element, display error message, set molecular weight to 0 and leave the loop
    else begin
      Mess := 'Molecule''s chemical formula contains invalid atom ' + Atom + '!';
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
      MolMass := 0;
    end;
    Inc(I);
  end;
  Result := MolMass;
end;

{ Calculate acid or mineral base normality for given molarity }

function GetNormality(Molecule, Formula: string; Molarity: Real): Real;

var
  Normality: Real;
  N: Integer;

begin
  Normality := -1; N := 0;
  if Molecule = 'organic acid' then begin
    // Organic acids must end in COOH or (COOH)n (also recognized: formula starting with HOOC)
    if RightStr(Formula, 4) = 'COOH' then
      N := 1
    else if (Copy(Formula, Length(Formula) - 6, 6) = '(COOH)') and (RightStr(Formula, 1)[1] in ['1'..'9']) then
      N := StrToInt(RightStr(Formula, 1));
    if LeftStr(Formula, 4) = 'HOOC' then
      N += 1;
  end
  else if Molecule = 'mineral acid' then begin
    // Mineral acids must start with H or Hn
    if LeftStr(Formula, 1) = 'H' then begin
      if Copy(Formula, 2, 1)[1] in ['1'..'9'] then
        N := StrToInt(Copy(Formula, 2, 1))
      else
        N := 1;
    end;
  end
  else if Molecule = 'mineral base' then begin
    // Mineral bases must end with OH or (OH)n
    if (RightStr(Formula, 2) = 'OH') and (RightStr(Formula, 4) <> 'COOH') then
      N := 1
    else if (Copy(Formula, Length(Formula) - 4, 4) = '(OH)') and (Copy(Formula, Length(Formula) - 6, 6) <> '(COOH)') and (RightStr(Formula, 1)[1] in ['1'..'9']) then
      N := StrToInt(RightStr(Formula, 1));
  end;
  if N <> 0 then
    Normality := Molarity * N;                                                 // normality = molarity * number of H+/OH-
  Result := Normality;
end;

{*************}
{ TfSolutions }
{*************}

{ Application start: Initialisations }

procedure TfSolutions.FormCreate(Sender: TObject);

begin
  fSolutions.mOptionsDensityH2O.Caption := StringReplace(fSolutions.mOptionsDensityH2O.Caption, '2', SUB_Digits[2], []);
  // Read elements and solvents from text files
  ReadElements(aElements);
  ReadSolvents(aSolvents);
  // Start-up default values
  iDensityList := 1; bDensityFormulas := False;
  iAtomicMassDigits := 2; iDensityDigits := 2; iConcDigits := 3;
  sMVConc := 'g/dl'; rDensityWater := 1;
  mOptionsSolventsShort.Click;                                                 // fill solvents combobox (short list)
end;

{ Menu item "File > Exit": Exit application }

procedure TfSolutions.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Solvents list > Short list": Display short solvents list in solvent selection combobox }

procedure TfSolutions.mOptionsSolventsShortClick(Sender: TObject);

begin
  mOptionsSolventsShort.Checked := True; mOptionsSolventsLong.Checked := False;
  iDensityList := 1;
  FillSolvents(aSolvents, rDensityWater, iDensityList, iDensityDigits, bDensityFormulas);
end;

{ Menu item "Options > Solvents list > Long list": Display long solvents list in solvent selection combobox }

procedure TfSolutions.mOptionsSolventsLongClick(Sender: TObject);

begin
  mOptionsSolventsShort.Checked := False; mOptionsSolventsLong.Checked := True;
  iDensityList := 2;
  FillSolvents(aSolvents, rDensityWater, iDensityList, iDensityDigits, bDensityFormulas);
end;

{ Menu item "Options > Solvents list > Use formulas": Toggle display of solvent names or formulas }

procedure TfSolutions.mOptionsSolventsFormulasClick(Sender: TObject);

begin
  if mOptionsSolventsFormulas.Checked then
    mOptionsSolventsFormulas.Checked := False
  else
    mOptionsSolventsFormulas.Checked := True;
  bDensityFormulas := mOptionsSolventsFormulas.Checked;
  FillSolvents(aSolvents, rDensityWater, iDensityList, iDensityDigits, bDensityFormulas);
end;

{ Menu items "Options > Atomic mass values > ...": Choose how many decimal digits of atomic masse values should be considered for calculations }

procedure TfSolutions.mOptionsAtoms0Click(Sender: TObject);

begin
  mOptionsAtoms0.Checked := True;  mOptionsAtoms2.Checked := False;
  mOptionsAtoms3.Checked := False; mOptionsAtoms4.Checked := False; mOptionsAtoms5.Checked := False;
  iAtomicMassDigits := 0;
end;

procedure TfSolutions.mOptionsAtoms2Click(Sender: TObject);

begin
  mOptionsAtoms0.Checked := False; mOptionsAtoms2.Checked := True;
  mOptionsAtoms3.Checked := False; mOptionsAtoms4.Checked := False; mOptionsAtoms5.Checked := False;
  iAtomicMassDigits := 2;
end;

procedure TfSolutions.mOptionsAtoms3Click(Sender: TObject);

begin
  mOptionsAtoms0.Checked := False; mOptionsAtoms2.Checked := False;
  mOptionsAtoms3.Checked := True;  mOptionsAtoms4.Checked := False; mOptionsAtoms5.Checked := False;
  iAtomicMassDigits := 3;
end;

procedure TfSolutions.mOptionsAtoms4Click(Sender: TObject);

begin
  mOptionsAtoms0.Checked := False; mOptionsAtoms2.Checked := False;
  mOptionsAtoms3.Checked := False; mOptionsAtoms4.Checked := True;  mOptionsAtoms5.Checked := False;
  iAtomicMassDigits := 4;
end;

procedure TfSolutions.mOptionsAtoms5Click(Sender: TObject);

begin
  mOptionsAtoms0.Checked := False; mOptionsAtoms2.Checked := False;
  mOptionsAtoms3.Checked := False; mOptionsAtoms4.Checked := False; mOptionsAtoms5.Checked := True;
  iAtomicMassDigits := 5;
end;

{ Menu items "Options > Density values > ...": Choose how many decimal digits of solvent density values should be considered for calculations }

procedure TfSolutions.mOptionsDensity2Click(Sender: TObject);

begin
  mOptionsDensity2.Checked := True; mOptionsDensity3.Checked := False; mOptionsDensity4.Checked := False;
  iDensityDigits := 2;
  edSolventDensity.Text := RFormat(GetSolventDensity(aSolvents, cobSolventName.Text), iDensityDigits);
end;

procedure TfSolutions.mOptionsDensity3Click(Sender: TObject);

begin
  mOptionsDensity2.Checked := False; mOptionsDensity3.Checked := True; mOptionsDensity4.Checked := False;
  iDensityDigits := 3;
  edSolventDensity.Text := RFormat(GetSolventDensity(aSolvents, cobSolventName.Text), iDensityDigits);
end;

procedure TfSolutions.mOptionsDensity4Click(Sender: TObject);

begin
  mOptionsDensity2.Checked := False; mOptionsDensity3.Checked := False; mOptionsDensity4.Checked := True;
  iDensityDigits := 4;
  edSolventDensity.Text := RFormat(GetSolventDensity(aSolvents, cobSolventName.Text), iDensityDigits);
end;

{ Menu items "Options > Density values > H2O density = 1g/ml": Toggle precision of H2O density value }

procedure TfSolutions.mOptionsDensityH2OClick(Sender: TObject);

begin
  if mOptionsDensityH2O.Checked then
    mOptionsDensityH2O.Checked := False
  else
    mOptionsDensityH2O.Checked := True;
  rDensityWater := GetSolventDensity(aSolvents, 'water');
  if cobSolventName.Text = 'water' then
    edSolventDensity.Text := RFormat(rDensityWater, iDensityDigits);
end;

{ Menu items "Options > Calculation values > ...": Choose how many decimal digits of calculated values should be displayed }

procedure TfSolutions.mOptionsConc2Click(Sender: TObject);

begin
  mOptionsConc2.Checked := True; mOptionsConc3.Checked := False;
  iConcDigits := 2;
end;

procedure TfSolutions.mOptionsConc3Click(Sender: TObject);

begin
  mOptionsConc2.Checked := False; mOptionsConc3.Checked := True;
  iConcDigits := 3;
end;

{ Menu items "Options > Calculation values > m/v% in mg/100ml": Toggle mass-volume percentage unit in g/dl or mg/dl }

procedure TfSolutions.mOptionsConcMVmgClick(Sender: TObject);

begin
  if mOptionsConcMVmg.Checked then begin
    mOptionsConcMVmg.Checked := False;
    sMVConc := 'g/dl';
  end
  else begin
    mOptionsConcMVmg.Checked := True;
    sMVConc := 'mg/dl';
  end;
end;

{ Menu item "Help > Help": Display application help }

procedure TfSolutions.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfSolutions.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry: Solution concentrations.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January 2021 - May 2022.';
  MessageDlg('About "SimpleSolutions"', S, mtInformation, [mbOK], 0);
end;

{ Molecule formula input done: Apply subscripts for propper chemical formula display }

procedure TfSolutions.edSoluteFormulaEditingDone(Sender: TObject);

begin
  sFormula := edSoluteFormula.Text;
  edSoluteFormula.Text := ChemicalFormula(edSoluteFormula.Text);
end;

{ Solute quantity kind changes (due to user selection): Adapt form components }

procedure TfSolutions.cobSoluteKindChange(Sender: TObject);

begin
  if cobSoluteKind.ItemIndex = 0 then begin
    laSoluteQty.Caption := 'Molarity';
    FillQtyUnits('molarity');
    edVolPercent.Enabled := False;
  end
  else if cobSoluteKind.ItemIndex = 1 then begin
    laSoluteQty.Caption := 'Mol quantity';
    FillQtyUnits('molqty');
    edVolPercent.Enabled := False;
  end
  else if cobSoluteKind.ItemIndex = 2 then begin
    laSoluteQty.Caption := 'Mass (weight)';
    FillQtyUnits('mass');
    edVolPercent.Enabled := False;
  end
  else begin
    laSoluteQty.Caption := 'Volume';
    FillQtyUnits('volume');
    edVolPercent.Enabled := True;
  end;
end;

{ Solvent name changes (due to solvent selection by user): Adapt form components }

procedure TfSolutions.cobSolventNameChange(Sender: TObject);

begin
  if cobSolventName.ItemIndex = 0 then begin
    // Custom solvent: Give user possibility to enter a density value
    edSolventDensity.ReadOnly := False; edSolventDensity.TabStop := True;
    edSolventDensity.Color := clDefault;
    edSolventName.Text := '';
  end
  else begin
    // Solvent from the list: Fill in (read-only) density value
    edSolventDensity.ReadOnly := True; edSolventDensity.TabStop := False;
    edSolventDensity.Color := clCream;
    if mOptionsSolventsFormulas.Checked then
      edSolventName.Text := GetSolventName(aSolvents, cobSolventName.Text)
    else
      edSolventName.Text := GetSolventFormula(aSolvents, cobSolventName.Text);
    edSolventDensity.Text := RFormat(GetSolventDensity(aSolvents, cobSolventName.Text), iDensityDigits);
  end;
end;

{ Button "Compute" pushed: Get user data and do the calculation }

procedure TfSolutions.btConcentrationClick(Sender: TObject);

var
  I: Integer;
  Qty, Concentration: Real;
  SoluteFormula, SolventFormula: string;
  Ch: Char;
  OK: Boolean;
  Molecule: TMoleculeAtoms;

begin
  SoluteFormula := ''; OK := True; iSolute := -1;
  // Check if user has entered a solute
  if edSoluteFormula.Text <> '' then begin
    SoluteFormula := TextFormula(edSoluteFormula.Text);
  end
  else begin
    MessageDlg('Data error', 'You must enter a solute!', mtError, [mbOK], 0);
    edSoluteFormula.SetFocus;
    OK := False;
  end;
  if OK then begin
    if cobSoluteKind.ItemIndex = 3 then begin
      // If the solute is given as a volume, check if user has entered a (valid) solute density
      if edSoluteDensity.Text = '' then
        rSoluteDensity := 0
      else
        rSoluteDensity := StrToFloat(edSoluteDensity.Text);
      if rSoluteDensity <= 0 then begin
        MessageDlg('Data error', 'Missing or invalid solute density!', mtError, [mbOK], 0);
        edSoluteDensity.SetFocus;
        OK := False;
      end;
    end;
  end;
  if OK then begin
    // Check if user has entered a (valid) solute quantity
    if edSoluteQty.Text = '' then
      rSoluteQty := 0
    else
      rSoluteQty := StrToFloat(edSoluteQty.Text);
    if rSoluteQty <= 0 then begin
      MessageDlg('Data error', 'Missing or invalid solute quantity!', mtError, [mbOK], 0);
      edSoluteQty.SetFocus;
      OK := False;
    end;
  end;
  if OK then begin
    // Check if user has entered a (valid) solvent density
    if edSolventDensity.Text = '' then
      rSolventDensity := 0
    else
      rSolventDensity := StrToFloat(edSolventDensity.Text);
    if rSolventDensity <= 0 then begin
      MessageDlg('Data error', 'Missing or invalid solvent density!', mtError, [mbOK], 0);
      edSolventDensity.SetFocus;
      OK := False;
    end;
  end;
  if OK then begin
    // Check if user has entered a (valid) solvent volume
    if edSolventVolume.Text = '' then
      rSolventVol := 0
    else
      rSolventVol := StrToFloat(edSolventVolume.Text);
    if rSolventVol <= 0 then begin
      MessageDlg('Data error', 'Missing or invalid solvent volume!', mtError, [mbOK], 0);
      edSolventVolume.SetFocus;
      OK := False;
    end
  end;
  if OK then begin
    // Calculate solvent molecular mass and weight mass
    if mOptionsSolventsFormulas.Checked then
      SolventFormula := TextFormula(cobSolventName.Text)
    else
      SolventFormula := TextFormula(edSolventName.Text);
    GetAtoms(SolventFormula, Molecule);
    rSolventMolecularMass := GetMolecularMass(Molecule, aElements, iAtomicMassDigits);
    rSolventMass := Round(1000 * Power(10, iDensityDigits) * rSolventDensity * rSolventVol) / Power(10, iDensityDigits);
    edSolventMolWeight.Text := RFormat(rSolventMolecularMass, iConcDigits);
    // Adapt solute quantity to unit actually used
    if cobSoluteKind.Text = 'volume' then begin
      case cobUSoluteQty.ItemIndex of
        0: rSoluteQty *= 1E-3;
        1: rSoluteQty *= 1E-2;
        2: rSoluteQty *= 1E-1;
      end;;
    end
    else if cobSoluteKind.Text <> 'molarity' then begin
      case cobUSoluteQty.ItemIndex of
        0: rSoluteQty *= 1E-3;
        2: rSoluteQty *= 1E+3;
      end;
    end;
    // Check solute molecule for invalid characters
    I := 1;
    while (I <= Length(SoluteFormula)) and OK do begin
      Ch := SoluteFormula[I];
      if not (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '(', ')', '[', ']']) then
        OK := False;
      Inc(I);
    end;
    // If no invalid characters found, calculate and display solute molecular weight
    if OK then begin
      GetAtoms(SoluteFormula, Molecule);
      if Molecule.NAtoms > 0 then begin
        rMolecularMass := GetMolecularMass(Molecule, aElements, iAtomicMassDigits);
        edSoluteMolWeight.Text := RFormat(rMolecularMass, iConcDigits);
        // Calculate solute molarity (depending on what solute quantity actually stands for)
        if cobSoluteKind.Text = 'molarity' then begin
          rMolarity := rSoluteQty;
          rMolality := rMolarity * rSolventVol / (rSolventMass / 1000);
        end
        else begin
          if cobSoluteKind.Text = 'mol quantity' then begin
            rMolarity := rSoluteQty / rSolventVol;
            rMolality := rSoluteQty / (rSolventMass / 1000);
          end
          else begin
            if cobSoluteKind.Text = 'mass (weight)' then begin
              rMolarity := (rSoluteQty / rMolecularMass) / rSolventVol;
              rMolality := (rSoluteQty / rMolecularMass) / (rSolventMass / 1000);
            end
            else if cobSoluteKind.Text = 'volume' then begin
              Qty := 1000 * rSoluteQty * rSoluteDensity;
              rMolarity := (Qty / rMolecularMass) / rSolventVol;
              rMolality := (Qty / rMolecularMass) / (rSolventMass / 1000);
            end;
          end;
        end;
        // Calculate and display quantity, concentration and percentage values
        rSoluteMol := rMolarity * rSolventVol;
        rSolventMol := rSolventMass / rSolventMolecularMass;
        rConcentration := rMolarity * rMolecularMass; Concentration := rConcentration;
        rSoluteMass := rConcentration * rSolventVol;
        if rSolventMass < 1 then begin
          edSolventMass.Text := RFormat(rSolventMass * 1000, iConcDigits);
          laSolventUMass.Caption := 'mg';
        end
        else if rSolventMass > 1000 then begin
          edSolventMass.Text := RFormat(rSolventMass / 1000, iConcDigits);
          laSolventUMass.Caption := 'kg';
        end
        else begin
          edSolventMass.Text := RFormat(rSolventMass, iConcDigits);
          laSolventUMass.Caption := 'g';
        end;
        if rSoluteMass < 1 then begin
          edSoluteMass.Text := RFormat(rSoluteMass * 1000, iConcDigits);
          laSoluteUMass.Caption := 'mg';
        end
        else if rSoluteMass > 1000 then begin
          edSoluteMass.Text := RFormat(rSoluteMass / 1000, iConcDigits);
          laSoluteUMass.Caption := 'kg';
        end
        else begin
          edSoluteMass.Text := RFormat(rSoluteMass, iConcDigits);
          laSoluteUMass.Caption := 'g';
        end;
        if rSoluteMol < 0.1 then begin
          edQtySolute.Text := RFormat(rSoluteMol * 1000, iConcDigits);
          laUQtySolute.Caption := 'mmol';
        end
        else begin
          edQtySolute.Text := RFormat(rSoluteMol, iConcDigits);
          laUQtySolute.Caption := 'mol';
        end;
        if rSolventMol < 0.1 then begin
          edQtySolvent.Text := RFormat(rSolventMol * 1000, iConcDigits);
          laUQtySolvent.Caption := 'mmol';
        end
        else begin
          edQtySolvent.Text := RFormat(rSolventMol, iConcDigits);
          laUQtySolvent.Caption := 'mol';
        end;
        edMolarity.Text := RFormat(rMolarity, iConcDigits);
        edMolality.Text := RFormat(rMolality, iConcDigits);
        case cobUConcentration.ItemIndex of
          1: Concentration *= 1E+3;
        end;
        edConcentration.Text := RFormat(Concentration, iConcDigits);
        rSoluteMolFraction := rSoluteMol / (rSoluteMol + rSolventMol);
        rSolventMolFraction := rSolventMol / (rSoluteMol + rSolventMol);
        edMolFractSolute.Text := RFormat(rSoluteMolFraction, iConcDigits);
        rMolPercent := 100 * rSoluteMolFraction;
        rMassPercent := 100 * ((rConcentration * rSolventVol) / (rConcentration * rSolventVol + rSolventMass));
        if cobSoluteKind.ItemIndex = 3 then
          rVolPercent := 100 * (rSoluteQty / (rSoluteQty + rSolventVol));
        if sMVConc = 'mg/dl' then
          rMassVolPercent := 1000 * rConcentration / 10
        else
          rMassVolPercent := rConcentration / 10;
        edMolPercent.Text := RFormat(rMolPercent, iConcDigits);
        edMassPercent.Text := RFormat(rMassPercent, iConcDigits);
        edVolPercent.Text := '';
        if cobSoluteKind.ItemIndex = 3 then
          edVolPercent.Text := RFormat(rVolPercent, iConcDigits);
        edMassVolPercent.Text := RFormat(rMassVolPercent, iConcDigits);
      end;
      edNormality.Text := ''; edNormality.Enabled := False;
      if cobSoluteType.ItemIndex <> 0 then begin
        // For acids and mineral bases, calculate normality
        rNormality := GetNormality(cobSoluteType.Items[cobSoluteType.ItemIndex], SoluteFormula, rMolarity);
        if rNormality = -1 then begin
          MessageDlg('Data error', 'Invalid ' + cobSoluteType.Items[cobSoluteType.ItemIndex] + '!', mtWarning, [mbOK], 0);
        end
        else begin
          edNormality.Enabled := True;
          edNormality.Text := RFormat(rNormality, iConcDigits);
        end;
      end;
    end
    else
      MessageDlg('Molecule error', 'Invalid character: ' + Ch, mtError, [mbOK], 0);
  end;
end;

end.

