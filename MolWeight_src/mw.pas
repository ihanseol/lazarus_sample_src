{***************************************}
{* Main unit for MolWeight application *}
{***************************************}

unit mw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, comp, help;

type
  TMolWeight = record
    Name, Symbol, Symbol2: string;
    MolWeight: Real;
  end;
  TMolWeights = array of TMolWeight;
  TElement = record
    Element: string;
    NElement: Integer;
  end;
  TMolecule = record
    NAtoms: Integer;
    Atoms: array of TElement;
  end;
  TBase = record
    Base: Char;
    NBase: Integer;
  end;
  TNucleotide = record
    NBases: Integer;
    Bases: array of TBase;
  end;
  TAminoAcid = record
    AminoAcid: string;
    NAminoAcid: Integer;
  end;
  TProtein = record
    NAminoAcids: Integer;
    AminoAcids: array of TAminoAcid;
  end;
  { ----------- }
  { TfMolWeight }
  { ----------- }
  TfMolWeight = class(TForm)
    mMenu: TMainMenu;
    mFile: TMenuItem;
    mFileExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsBaseUC, mOptionsUracil, mOptionsAminoAcidLC, mOptionsUncertain: TMenuItem;
    mExtras: TMenuItem;
    mExtrasComposition: TMenuItem;
    mHelp: TMenuItem;
    mHelpNucleotides, mHelpHelp, mHelpAbout: TMenuItem;
    Label1: TLabel;
    rbFormula, rbNucleotide, rbProtein: TRadioButton;
    cobNucleotide, cobProtein: TComboBox;
    imOpenNucleotide, imOpenProtein: TImage;
    edFormula: TEdit;
    edNucleotide, edProtein: TMemo;
    Label2: TLabel;
    edMolWeight: TEdit;
    btCalc: TButton;
    btClear: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsBaseUCClick(Sender: TObject);
    procedure mOptionsUracilClick(Sender: TObject);
    procedure mOptionsAminoAcidLCClick(Sender: TObject);
    procedure mOptionsUncertainClick(Sender: TObject);
    procedure mExtrasCompositionClick(Sender: TObject);
    procedure mHelpNucleotidesClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure imOpenNucleotideClick(Sender: TObject);
    procedure imOpenProteinClick(Sender: TObject);
    procedure edFormulaChange(Sender: TObject);
    procedure edNucleotideChange(Sender: TObject);
    procedure edProteinChange(Sender: TObject);
  private
    rMolWeight: Real;
    sMolecule: AnsiString;
    bCalcDone: Boolean;
    MWElements, MWBasesDNA, MWBasesRNA, MWAminoAcids: TMolWeights;
    Molecule: TMolecule;
    Nucleotide: TNucleotide;
    Protein: TProtein;
  end;

var
  fMolWeight: TfMolWeight;

implementation

{$R *.lfm}

{ Read elements, bases and amino acids data from text files }

procedure ReadMolWeights(out Elements, BasesDNA, BasesRNA, AminoAcids: TMolWeights);

const
  FileNames: array[1..4] of string = (
    'elements.txt', 'dna.txt', 'rna.txt', 'aa.txt'
  );

var
  N, I, C: Integer;
  MW: Real;
  S: string;
  MolWeightRec: TMolWeight;
  MolWeightFile: Text;

begin
  SetLength(Elements, 0); SetLength(BasesDNA, 0); SetLength(BasesRNA, 0); SetLength(AminoAcids, 0);
  // 4 files: elements, DNA bases, RNA bases, amino acids
  for I := 1 to 4 do begin
    N := 0;
    Assign(MolWeightFile, FileNames[I]); Reset(MolWeightFile);
    while not EoF(MolWeightFile) do begin
      Readln(MolWeightFile, S);
      if S <> '' then begin
        with MolWeightRec do begin
          Name := Trim(LeftStr(S, 15));                                        // chars  1 - 15: name
          Symbol := Trim(Copy(S, 17, 2));                                      // chars 17 - 18: element symbol or base/amino acid code
          Symbol2 := Trim(Copy(S, 20, 3));                                     // chars 20 - 22: amino acid 3-letters code
          Val(Copy(S, 24, 8), MW, C);                                          // chars 24 - 31: molecular weight
          if C = 0 then
            MolWeight := MW                                                    // mol. weight value
          else
            MolWeight := 0;                                                    // '--------' used with uncertain bases/amino acids
        end;
        Inc(N);
        // Fill values read into array, depending on actual molecule-type processed
        case I of
          1: begin
               SetLength(Elements, Length(Elements) + 1);
               Elements[N - 1] := MolWeightRec;
             end;
          2: begin
               SetLength(BasesDNA, Length(BasesDNA) + 1);
               BasesDNA[N - 1] := MolWeightRec;
             end;
          3: begin
               SetLength(BasesRNA, Length(BasesRNA) + 1);
               BasesRNA[N - 1] := MolWeightRec;
             end;
          4: begin
              SetLength(AminoAcids, Length(AminoAcids) + 1);
               AminoAcids[N - 1] := MolWeightRec;
             end;
        end;
      end;
    end;
    Close(MolWeightFile);
  end;
end;

{ Get molecule's atoms and their occurences within the molecule }

procedure GetAtoms(SMolecule: string; out Molecule: TMolecule);

var
  NAtms, NElemnt, Bracket1Elemnt1, Bracket2Elemnt1, BracketElemnt1, BracketElemnt2, NBracketElemnts, I, J: Integer;
  Elemnt: string; Temp: TMolecule; Mess: string;

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
  // The temporary array so far filled in generally contains several entries for a given element; create
  // the procedure output array by eliminating these doubles (and summing up all occurences for given element )
  // This is done in 2 steps: first add occurences and set element name in redundant array element to '', then
  // create definitif array by keeping only those array elements where the name is different from ''
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

{ Standard molecule molecular weight calculation }

function MolecularWeight(var Molecule: TMolecule; var Elements: TMolWeights): Real;

var
  NAtom, I, J: Integer;
  Atom, Mess: string;
  AtomWeight, MolWeight: Real;

// Molecule is an array of records, containing atom symbols and their occurences within the molecule
// Elements is an array of records, containing the elements data read from the elements.txt file

begin
  MolWeight := 0; Mess := '';
  I := 1;
  // Do this until all atoms processed or if an error (inexisting element) occured
  while (I <= Molecule.NAtoms) and (Mess = '') do begin
    Atom := Molecule.Atoms[I].Element;                                         // atom symbol
    NAtom := Molecule.Atoms[I].NElement;                                       // occurences of this atom
    AtomWeight := -1; J := 0;
    // Find atom in elements array
    while (J < Length(Elements)) and (AtomWeight = -1) do begin
      if Atom = Elements[J].Symbol then
        AtomWeight := Elements[J].MolWeight;
      Inc(J);
    end;
    // If atom is an existing element, add atom weight to total molecular weight
    if AtomWeight <> -1 then begin
      AtomWeight *= NAtom;
      MolWeight += AtomWeight;
    end
    // If atom is an unexisting element, display error message, set molecular weight to 0 and leave the loop
    else begin
      Mess := 'Molecule''s chemical formula contains invalid atom ' + Atom + '!';
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
      MolWeight := 0;
    end;
    Inc(I);
  end;
  MolecularWeight := MolWeight;
end;

{ Compute nucleotide's reverse complement }

function ReverseComplement(NuclType: string; SNucleotide: AnsiString): AnsiString;

const
  Base: array[1..16] of Char = (
    'a', 't', 'u', 'c', 'g', 'm', 'r', 'w', 's', 'y', 'k', 'v', 'h', 'd', 'b', 'n'
  );
  BaseComp: array[1..16] of Char = (
    't', 'a', 'a', 'g', 'c', 'k', 'y', 'w', 's', 'r', 'm', 'b', 'd', 'h', 'v', 'n'
  );

var
  I, J: Integer;
  RevComp: AnsiString;

begin
  RevComp := '';
  for I := 1 to Length(SNucleotide) do begin
    for J := 1 to 16 do begin
      if SNucleotide[I] = Base[J] then
        RevComp += BaseComp[J];
    end;
  end;
  // Make correction for RNA molecule, by changing 't' to standard 'u'
  if NuclType = 'RNA' then
    RevComp := StringReplace(RevComp, 't', 'u', [rfReplaceAll]);
  ReverseComplement := RevComp;
end;

{ Get nucleotide's bases and their occurences within the molecule }

procedure GetBases(NuclType: string; var SMolecule: AnsiString; out Nucleotide: TNucleotide);

var
  NBases, I, J: Integer;
  Base: Char;
  New: Boolean;

// Note that the molecule AnsiString may be modified by this procedure!

begin
  if fMolWeight.mOptionsBaseUC.Checked then
    SMolecule := LowerCase(SMolecule);                                         // transform all base codes to lowercase
  if fMolWeight.mOptionsUracil.Checked and (RightStr(NuclType, 3) = 'RNA') then
    SMolecule := StringReplace(SMolecule, 't', 'u', [rfReplaceAll]);           // RNA: replace 't' by 'u'
  if LeftStr(NuclType, 2) = 'ds' then
    SMolecule += ReverseComplement(RightStr(NuclType, 3), SMolecule);          // compute reverse complement and add it to the nucleotide AnsiString
  NBases := 0; SetLength(Nucleotide.Bases, 0);
  // Do this for each base of the nucleotide
  for I := 1 to Length(SMolecule) do begin
    Base := SMolecule[I];
    New := True;
    // Check if this base code has already been processed
    for J := 1 to NBases do begin
      if Base = Nucleotide.Bases[J].Base then begin
        // If this base code already is part of the array, increment its number of occurences
        Nucleotide.Bases[J].NBase += 1;
        New := False;
      end;
    end;
    // New (not yet part of the array) base code: create new array entry (with number of occurences = 1)
    if New then begin
      Inc(NBases);
      Nucleotide.NBases := NBases; SetLength(Nucleotide.Bases, NBases + 1);
      Nucleotide.Bases[NBases].Base := Base;
      Nucleotide.Bases[NBases].NBase := 1;
    end;
  end;
end;

{ Oligonucleotide molecular weight calculation }

function MolecularWeightNucleotide(NuclType: string; Nucleotide: TNucleotide; Bases: TMolWeights): Real;

var
  NBases, Uncertain, I, J: Integer;
  Base, Mess: string;
  BaseWeight, MolWeight: Real;

// Nucleotide is an array of records, containing base codes and their occurences within the molecule
// Bases is an array of records, containing the bases data read from the dna.txt/rna.txt file

// Oligonucleotide molecular weight calculation considerations: cf. oligo.txt help text

begin
  MolWeight := 0; Uncertain := 0; Mess := '';
  I := 1;
  // Do this until all bases processed or if error (bad base code) occured
  while (I <= Nucleotide.NBases) and (Mess = '') do begin
    Base := Nucleotide.Bases[I].Base;
    NBases := Nucleotide.Bases[I].NBase;
    BaseWeight := -1; J := 0;
    // Find the base code in the bases array
    while (J < Length(Bases)) and (BaseWeight = -1) do begin
      if Base = Bases[J].Symbol then begin
        // Get the base mol. weight (for uncertain bases, only if options set allow to use them)
        if (Bases[J].MolWeight <> 0) or ((Bases[J].MolWeight = 0) and fMolWeight.mOptionsUncertain.Checked) then begin
          BaseWeight := Bases[J].MolWeight;
        end;
      end;
      Inc(J);
    end;
    // Base is a standard or uncertain base code
    if BaseWeight <> -1 then begin
      if fMolWeight.mOptionsUncertain.Checked and (BaseWeight = 0) then
        // (Allowed) uncertain base code: ignore them for mol. weight calculation (but count their number in nucleotide)
        Uncertain += NBases
      else begin
        // Standard base code: add base weight to total mol. weight
        BaseWeight *= NBases;
        MolWeight += BaseWeight;
      end;
    end
    // Inexisting base code: display error message, set mol. weight to 0 and leave loop
    else begin
      Mess := 'Nucleotide contains invalid base ' + Base + '!';
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
      MolWeight := 0;
    end;
    Inc(I);
  end;
  // Display warning if nucleotide contains (allowed) uncertain bases
  if (Mess = '') and (Uncertain > 0) then
    MessageDlg('Incomplete data', 'Calculation doesn''t include molecular weight of ' + IntToStr(Uncertain) + ' uncertain base(s)!', mtWarning, [mbOK], 0);
  // Make DNA/RNA molecular weight adjustments (cf. oligo.txt help text)
  if MolWeight <> 0 then begin
    if RightStr(NuclType, 3) = 'DNA' then begin
      // DNA mol. weight
      MolWeight -= 61.96;
      if LeftStr(NuclType, 2) = 'ds' then
        MolWeight -= 61.96;
    end
    else begin
      // RNA mol. weight
      MolWeight += 159;
      if LeftStr(NuclType, 2) = 'ds' then
        MolWeight += 159;
    end;
  end;
  MolecularWeightNucleotide := MolWeight;
end;

{ Get protein's amino acids and their occurences within the molecule }

procedure GetAminoAcids(Code: string; var SMolecule: AnsiString; out Protein: TProtein);

var
  NAmAc, I, J: Integer;
  AmAc, S: string;
  New: Boolean;

// Note that the molecule AnsiString may be modified by this procedure!

begin
  if Code = '1-letter code' then begin
    // 1-letter amino acid codes
    if fMolWeight.mOptionsAminoAcidLC.Checked then
      SMolecule := UpperCase(SMolecule);                                       // transform all amino acid codes to uppercase
  end
  else begin
    // 3-letters amino acid codes: allow any upper/lowercase mixture (?), therefor
    // transform codes to first letter uppercase, others lowercase (as are in aa.txt)
    SMolecule := LowerCase(SMolecule);
    I := 1;
    while I <= Length(SMolecule) - 2 do begin
      S := UpperCase(SMolecule[I]);
      SMolecule[I] := S[1];
      I += 3;
    end;
  end;
  NAmAc := 0; SetLength(Protein.AminoAcids, 0);
  I := 1;
  // Do this for all amino acids of the protein
  while I <= Length(SMolecule) do begin
    // Get the amino acid code (depending on letter-coding used)
    if Code = '3-letters code' then begin
      AmAc := Copy(SMolecule, I, 3); I += 3;
    end
    else begin
      AmAC := SMolecule[I]; I += 1;
    end;
    New := True;
    // Check if this amino acid code has already been processed
    for J := 1 to NAmAc do begin
      if AmAc = Protein.AminoAcids[J].AminoAcid then begin
        // If this amino acid code already is part of the array, increment its number of occurences
        Protein.AminoAcids[J].NAminoAcid += 1;
        New := False;
      end;
    end;
    // New (not yet part of the array) amino acid code: create new array entry (with number of occurences = 1)
    if New then begin
      Inc(NAmAc);
      Protein.NAminoAcids := NAmAc; SetLength(Protein.AminoAcids, NAmAc + 1);
      Protein.AminoAcids[NAmAc].AminoAcid := AmAc;
      Protein.AminoAcids[NAmAc].NAminoAcid := 1;
    end;
  end;
end;

{ Protein molecular weight calculation }

function MolecularWeightProtein(Code: string; Protein: TProtein; AminoAcids: TMolWeights): Real;

var
  NAmAc, Uncertain, I, J: Integer;
  AmAc, Mess: string;
  AmAcWeight, MolWeight: Real;

// Protein is an array of records, containing amino acids codes and their occurences within the molecule
// AminoAcids is an array of records, containing the amino acids data read from the aa.txt file

// Protein molecular weight calculation considerations: just sum up the mol. weight of the constituting amino acids

begin
  MolWeight := 0; Uncertain := 0; Mess := '';
  I := 1;
  // Proceed until all amino acids processed or error (bad amino acid code) occurs
  while (I <= Protein.NAminoAcids) and (Mess = '') do begin
    AmAc := Protein.AminoAcids[I].AminoAcid;
    NAmAc := Protein.AminoAcids[I].NAminoAcid;
    AmAcWeight := -1; J := 0;
    // Find the amino acid code in the amino acids array
    while (J < Length(AminoAcids)) and (AmAcWeight = -1) do begin
      // // Get the amino acid molecular weight
      if Code = '1-letter code' then begin
        if AmAc = AminoAcids[J].Symbol then begin
          // Get the amino acid mol. weight (for uncertain amino acids, only if options set allow to use them)
          if (AminoAcids[J].MolWeight <> 0) or ((AminoAcids[J].MolWeight = 0) and fMolWeight.mOptionsUncertain.Checked) then
            AmAcWeight := AminoAcids[J].MolWeight;
        end;
      end
      else begin
        if AmAc = AminoAcids[J].Symbol2 then begin
          // Get the amino acid mol. weight (for uncertain amino acids, only if options set allow to use them)
          if (AminoAcids[J].MolWeight <> 0) or ((AminoAcids[J].MolWeight = 0) and fMolWeight.mOptionsUncertain.Checked) then
            AmAcWeight := AminoAcids[J].MolWeight;
        end;
      end;
      Inc(J);
    end;
    if AmAcWeight <> -1 then begin
      if fMolWeight.mOptionsUncertain.Checked and (AmAcWeight = 0) then
        // (Allowed) uncertain amino code: ignore them for mol. weight calculation (but count their number in protein)
        Uncertain += NAmAc
      else begin
        // Existing amino acid code: add amino acid's weight to total mol. weight
        AmAcWeight *= NAmAc;
        MolWeight += AmAcWeight;
      end;
    end
    // Inexisting amino acid code: display error message, set mol. weight to 0 and leave loop
    else begin
      Mess := 'Protein contains invalid amino acid ' + AmAc + '!';
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0);
      MolWeight := 0;
    end;
    Inc(I);
  end;
  // Display warning if nucleotide contains (allowed) uncertain bases
  if (Mess = '') and (Uncertain > 0) then
    MessageDlg('Incomplete data', 'Calculation doesn''t include molecular weight of ' + IntToStr(Uncertain) + ' uncertain amino acid(s)!', mtWarning, [mbOK], 0);
  MolecularWeightProtein := MolWeight;
end;

{***************}
{* TfMolWeight *}
{***************}

{ Application start: Initialisation }

procedure TfMolWeight.FormCreate(Sender: TObject);

begin
  // Set length of all arrays to 0
  SetLength(Molecule.Atoms, 0); SetLength(Nucleotide.Bases, 0); SetLength(Protein.AminoAcids, 0);
  SetLength(MWElements, 0); SetLength(MWBasesDNA, 0); SetLength(MWBasesRNA, 0); SetLength(MWAminoAcids, 0);
  // Read molecular weight files (into arrays)
  ReadMolWeights(MWElements, MWBasesDNA, MWBasesRNA, MWAminoAcids);
  // The "Extras" menu item has only to work, if mol. weight calculation has been done before
  bCalcDone := False;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfMolWeight.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > ...": Check/uncheck the different option items }

procedure TfMolWeight.mOptionsBaseUCClick(Sender: TObject);

begin
  if mOptionsBaseUC.Checked then
    mOptionsBaseUC.Checked := False
  else
    mOptionsBaseUC.Checked := True;                                            // uppercase base codes allowed (default = yes)
end;

procedure TfMolWeight.mOptionsUracilClick(Sender: TObject);

begin
  if mOptionsUracil.Checked then
    mOptionsUracil.Checked := False
  else
    mOptionsUracil.Checked := True;                                            // usage of "t" instaed of "u" in RNA allowed (default = no)
end;

procedure TfMolWeight.mOptionsAminoAcidLCClick(Sender: TObject);

begin
  if mOptionsAminoAcidLC.Checked then
    mOptionsAminoAcidLC.Checked := False
  else
    mOptionsAminoAcidLC.Checked := True;                                       // lowercase (1-letter) amino acid codes allowed (default = no)
end;

procedure TfMolWeight.mOptionsUncertainClick(Sender: TObject);

begin
  if mOptionsUncertain.Checked then
    mOptionsUncertain.Checked := False
  else
    mOptionsUncertain.Checked := True;                                         // uncertain IUP/IUPAC codes allowed (default = no)
end;

{ Menu item "Extras > Chemical composition": Display molecule's chemical composition }

procedure TfMolWeight.mExtrasCompositionClick(Sender: TObject);

var
  Total, Uncertain, Count, Count2, I, J : Integer;
  Atom, Base, AminoAcid, SNumber: string;

begin
  // Do only, if molecular weight has been calculated before (array values have been assigned)
  if bCalcDone then begin
    // Initialize counters and grid
    Total := 0;                                                                // total number of atoms, bases resp. amino acids
    Uncertain := 0;                                                            // total number of uncertain bases resp. amino acids
    Count := 0;                                                                // grid rows used by atoms, resp. standard bases or amino acids
    Count2 := 0;                                                               // grid rows used by uncertain bases resp. amino acids
    fComposition.grComposition.Clear;                                          // clear the grid
    fComposition.grComposition.RowCount := 24;                                 // maximum number of grid rows (allows molecule with all std. amino acids)
    fComposition.grComposition.Height := 628;                                  // grid height, corresponding to 24 rows (title row + 23 data rows)
    // Grid title values
    fComposition.grComposition.Cells[1, 0] := 'Count';
    fComposition.grComposition.Cells[2, 0] := 'Percentage';
    // Atom composition of standard molecule
    if rbFormula.Checked then begin
      fComposition.stTitle.Caption := 'Chemical composition of ' + sMolecule;
      // Do this for each atom symbol of the actual molecule
      for I := 1 to Molecule.NAtoms do begin
        // Look up atom's name and its occurence within the molecule (in array read from file)
        for J := 0 to Length(MWElements) - 1 do begin
          if Molecule.Atoms[I].Element = MWElements[J].Symbol then
            Atom := MWElements[J].Name + ' (' + MWElements[J].Symbol + ')';
        end;
        // Fill in grid values
        fComposition.grComposition.Cells[0, I] := Atom;
        fComposition.grComposition.Cells[1, I] := IntToStr(Molecule.Atoms[I].NElement);
        Total += Molecule.Atoms[I].NElement;
      end;
      // Set the number of grid rows used (values will be needed below)
      Count := Molecule.NAtoms; Count2 := 0;
    end
    // Base composition of oligonucleotide
    else if rbNucleotide.Checked then begin
      fComposition.stTitle.Caption := 'Base composition of ' + RightStr(cobNucleotide.Text, 3) + ' sequence';
      // Do this for each base code of the actual nucleotide
      for I := 1 to Nucleotide.NBases do begin
        // Molecule is DNA
        if RightStr(cobNucleotide.Text, 3) = 'DNA' then begin
          // Look up base name and its occurence within the nucleotide (in array read from file)
          for J := 0 to Length(MWBasesDNA) - 1 do begin
            if LowerCase(Nucleotide.Bases[I].Base) = MWBasesDNA[J].Symbol then begin
              // Uncertain DNA base codes (all counted under "Uncertain")
              if MWBasesDNA[J].MolWeight = 0 then begin
                Base := 'Uncertain';
                Uncertain += Nucleotide.Bases[I].NBase;
                Count2 := 1;
              end
              // Standard DNA base codes
              else begin
                Base := MWBasesDNA[J].Name + ' (' + MWBasesDNA[J].Symbol + ')';
                Inc(Count);
              end;
            end;
          end;
        end
        // Molecule is RNA
        else begin
          // Look up base name and its occurence within the nucleotide (in array read from file)
          for J := 0 to Length(MWBasesRNA) - 1 do begin
            if LowerCase(Nucleotide.Bases[I].Base) = MWBasesRNA[J].Symbol then begin
              // Uncertain RNA base codes (all counted under "Uncertain")
              if MWBasesDNA[J].MolWeight = 0 then begin
                Base := 'Uncertain';
                Uncertain += Nucleotide.Bases[I].NBase;
                Count2 := 1;
              end
              // Standard RNA base codes
              else begin
                Base := MWBasesRNA[J].Name + ' (' + MWBasesRNA[J].Symbol + ')';
                Inc(Count);
              end;
            end;
          end;
        end;
        // Fill in grid values (standard base codes only)
        if Base <> 'Uncertain' then begin
          fComposition.grComposition.Cells[0, Count] := Base;
          fComposition.grComposition.Cells[1, Count] := IntToStr(Nucleotide.Bases[I].NBase);
          Total += Nucleotide.Bases[I].NBase;
        end;
      end;
      // At end of list, fill in uncertain bases (if any)
      if Count2 <> 0 then begin
        fComposition.grComposition.Cells[0, Count + 1] := 'Uncertain';
        fComposition.grComposition.Cells[1, Count + 1] := IntToStr(Uncertain);
        Total += Uncertain;
      end;
    end
    // Amino acid composition of protein
    else begin
      fComposition.stTitle.Caption := 'Amino acid composition of protein sequence';
      // Do this for all amino acid codes of the actual protein
      for I := 1 to Protein.NAminoAcids do begin
        // Look up amino name and its occurence within the protein (in array read from file)
        for J := 0 to Length(MWAminoAcids) - 1 do begin
          // Look up depends on usage of 1- or 3-letter-code
          if ((cobProtein.Text = '1-letter code') and (Protein.AminoAcids[I].AminoAcid = MWAminoAcids[J].Symbol)) or
              ((cobProtein.Text = '3-letters code') and (Protein.AminoAcids[I].AminoAcid = MWAminoAcids[J].Symbol2)) then begin
            // Uncertain amino acids (count all under "Uncertain")
            if MWAminoAcids[J].MolWeight = 0 then begin
              AminoAcid := 'Uncertain';
              Uncertain += Protein.AminoAcids[I].NAminoAcid;
              Count2 := 1;
            end
            // Standard amino acid codes
            else begin
              AminoAcid := MWAminoAcids[J].Name + ' (' + Protein.AminoAcids[I].AminoAcid + ')';
              Inc(Count);
            end;
          end;
        end;
        // Fill in grid values (standard amino acid codes only)
        if AminoAcid <> 'Uncertain' then begin
          fComposition.grComposition.Cells[0, Count] := AminoAcid;
          SNumber := IntToStr(Protein.AminoAcids[I].NAminoAcid);
          if Protein.AminoAcids[I].NAminoAcid < 10 then
            SNumber := ' ' + SNumber;
          fComposition.grComposition.Cells[1, Count] := SNumber;
          Total += Protein.AminoAcids[I].NAminoAcid;
        end;
      end;
      // At end of list, fill in uncertain amino acids (if any)
      if Count2 <> 0 then begin
        fComposition.grComposition.Cells[0, Count + 1] := 'Uncertain';
        fComposition.grComposition.Cells[1, Count + 1] := IntToStr(Uncertain);
        Total += Uncertain;
      end;
    end;
    // Compute and fill in atoms resp. standard bases or amino acids percentages
    for I := 1 to Count do begin
      SNumber := FloatToStrF(100 * StrToInt(fComposition.grComposition.Cells[1, I]) / Total, ffFixed, 0, 2) + ' %';
      if 100 * StrToInt(fComposition.grComposition.Cells[1, I]) / Total < 10 then  // grid value alignment
        SNumber := ' ' + SNumber;
      fComposition.grComposition.Cells[2, I] := SNumber;
    end;
    // Compute and fill in uncertain bases resp. amino acids percentage (if any)
    if Count2 <> 0 then begin
      SNumber := FloatToStrF(100 * Uncertain / Total, ffFixed, 0, 2) + ' %';
      if 100 * Uncertain / Total < 10 then                                         // grid value alignment
        SNumber := ' ' + SNumber;
      fComposition.grComposition.Cells[2, Count + 1] := SNumber;
    end;
    // Fill in total number of atoms, bases resp. amino acids
    fComposition.grComposition.Cells[0, Count + Count2 + 2] := 'Total';
    fComposition.grComposition.Cells[1, Count + Count2 + 2] := IntToStr(Total);
    // Determine number of grid rows and corresponding grid height
    fComposition.grComposition.RowCount := Count + Count2 + 3;
    fComposition.grComposition.Height := 628 - (24 - (Count + Count2 + 2)) * 25;
    fComposition.Show;
  end;
end;

{ Menu item "Help > Oligonucleotides": Display DNA/RNA molecular weight calculation info }

procedure TfMolWeight.mHelpNucleotidesClick(Sender: TObject);
begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.memoHelp.Clear;
    fHelp.memoHelp.Lines.LoadFromFile('oligo.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > Program Help": Display program help text }

procedure TfMolWeight.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.memoHelp.Clear;
    fHelp.memoHelp.Lines.LoadFromFile('help.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > Program About": Display program about info }

procedure TfMolWeight.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Calculation of molecular weight and chemical composition ';
  S += 'of standard molecules, DNA/RNA sequences and proteins.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June, 2018.';
  MessageDlg('About "MolWeight"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculate": Calculate the molecular weight }

procedure TfMolWeight.btCalcClick(Sender: TObject);

var
  First, I: Integer;
  MoleculeType, AmAcCode, Mess: string;
  Ch: Char;
  InvalidChar: Boolean;

begin
  edMolWeight.Text := ''; Mess := '';
  // Check if the user entered a molecule
  if rbFormula.Checked and (edFormula.Text = '') then
    Mess := 'Please, enter a molecule'
  else if rbNucleotide.Checked and (edNucleotide.Text = '') then
    Mess := 'Please, enter a nucleotide or select a DNA/RNA file'
  else if rbProtein.Checked and (edProtein.Text = '') then
    Mess := 'Please, enter a protein or select a protein file';
  // Compute mol. weight, depending on what kind of molecule has been entered
  if Mess = '' then begin
    // Compute mol. weight of standard molecule (chemical formula)
    if rbFormula.Checked then begin
      sMolecule := edFormula.Text;
      sMolecule := StringReplace(sMolecule, '-', '', [rfReplaceAll]);          // allow formulas such CH3-CH3
      I := 1; InvalidChar := False;
      // Check molecule for invalid characters
      while (I <= Length(sMolecule)) and not InvalidChar do begin
        Ch := sMolecule[I];
        if not (Ch in ['a'..'z', 'A'..'Z', '0'..'9', '(', ')', '[', ']']) then
          InvalidChar := True;
        Inc(I);
      end;
      // If no invalid characters found, calculate and display molecular weight
      if not InvalidChar then begin
        GetAtoms(sMolecule, Molecule);                                         // get molecule's atoms and their occurences
        if Molecule.NAtoms > 0 then begin
          rMolWeight := MolecularWeight(Molecule, MWElements);                 // compute standard molecule's molecular weight
          if rMolWeight <> 0 then begin
            edMolWeight.Text := FloatToStrF(rMolWeight, ffNumber, 0, 3);
            bCalcDone := True;                                                 // setting this True, enables chem. comp. calculation
          end;
        end;
      end
      // Molecule entered contains invalid characters
      else
        Mess := 'Invalid character "' + Ch + '" at position ' + IntToStr(I - 1);
    end
    // Compute mol. weight of oligonucleotide (IUP/IUPAC codes)
    else if rbNucleotide.Checked then begin
      MoleculeType := cobNucleotide.Text;                                      // ssDNA, ssRNA, dsDNA or dsRNA
      sMolecule := '';
      // Add a CR/LF to the Memo; this is necessary because otherwise you
      // get anerror if user does not terminate entry with ENTER key!
      edNucleotide.Lines.Append(Chr(13));
      // Check if nucleotide entered is Fasta format or raw data
      if LeftStr(edNucleotide.Text, 1) = '>' then
        First := 1                                                             // Fasta: molecule data starting in 2nd line
      else
        First := 0;
      // Assign Memo lines to a AnsiString variable (AnsiString = inlimited in length)
      for I := First to edNucleotide.Lines.Count do begin
        if edNucleotide.Lines.Strings[I] <> '' then
          sMolecule += edNucleotide.Lines.Strings[I];
      end;
      SMolecule := StringReplace(SMolecule, ' ', '', [rfReplaceAll]);          // eliminate blanks (as used to separate codes in many files)
      I := 1; InvalidChar := False;
      // Check nucleotide for invalid characters
      while (I <= Length(sMolecule)) and not InvalidChar do begin
        Ch := sMolecule[I];
        if not (Ch in ['a'..'z', 'A'..'Z']) then                               // bad base codes not eliminated (just as inexisting element symbols)
          InvalidChar := True;
        Inc(I);
      end;
      // If no invalid character found, compute and display mol. weight
      if not InvalidChar then begin
        GetBases(MoleculeType, sMolecule, Nucleotide);                         // get nucleotide's bases and their occurences
        if RightStr(MoleculeType, 3) = 'DNA' then
          // Molecular weight for DNA
          rMolWeight := MolecularWeightNucleotide(MoleculeType, Nucleotide, MWBasesDNA)  // DNA molecular weight calculation
        else
          // Molecular weight for RNA
          rMolWeight := MolecularWeightNucleotide(MoleculeType, Nucleotide, MWBasesRNA); // RNA molecular weight calculation
        if rMolWeight <> 0 then begin
          edMolWeight.Text := FloatToStrF(rMolWeight, ffNumber, 0, 3);
          bCalcDone := True;
        end;
      end
      // Nucleotide entered contains invalid character
      else
        Mess := 'Invalid character "' + Ch + '" at position ' + IntToStr(I - 1);
    end
    // Compute mol. weight of protein (IUP/IUPAC codes)
    else begin
      AmAcCode := cobProtein.Text;
      // Add a CR/LF to the Memo; this is necessary because otherwise you
      // get anerror if user does not terminate entry with ENTER key!
      sMolecule := ''; edProtein.Lines.Append(Chr(13));
      // Check if protein entered is Fasta format or raw data
      if LeftStr(edProtein.Text, 1) = '>' then
        First := 1
      else
        First := 0;
      // Assign Memo lines to a AnsiString variable
      for I := First to edProtein.Lines.Count do
        if edProtein.Lines.Strings[I] <> '' then
          sMolecule += edProtein.Lines.Strings[I];
      SMolecule := StringReplace(SMolecule, ' ', '', [rfReplaceAll]);          // eliminate blanks (as used to separate codes in many files)
      I := 1; InvalidChar := False;
      // Check protein for invalid characters
      while (I <= Length(sMolecule)) and not InvalidChar do begin
        Ch := sMolecule[I];
        if not (Ch in ['a'..'z', 'A'..'Z']) then                               // bad amino acid codes not eliminated
          InvalidChar := True;
        Inc(I);
      end;
      // If no invalid character found, compute and display mol. weight
      if not InvalidChar then begin
        if (cobProtein.Text = '3-letters code') and (Length(sMolecule) mod 3 <> 0) then  // 3-letters code: be sure amino acid number is multiple of 3
          Mess := 'Invalid 3-letter code format'
        else begin
          GetAminoAcids(AmAcCode, sMolecule, Protein);                         // get protein's amino acids and their occurences
          rMolWeight := MolecularWeightProtein(AmAcCode, Protein, MWAminoAcids);  // protein mol. weight calculation
          if rMolWeight <> 0 then begin
            edMolWeight.Text := FloatToStrF(rMolWeight, ffNumber, 0, 3);
            bCalcDone := True;
          end;
        end;
      end
      // Protein entered contains invalid characetr
      else
        Mess := 'Invalid character "' + Ch + '" at position ' + IntToStr(I - 1);
    end;
  end;
  // If the molecule entered wasn't correct, display error message
  if Mess <> '' then begin
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
    edMolWeight.Text := '';
  end;
end;

{ Button "Clear": Clear all molecule input fields }

procedure TfMolWeight.btClearClick(Sender: TObject);

begin
  edFormula.Text := '';
  edNucleotide.Clear;
  edProtein.Clear;
  edMolWeight.Text := '';
end;

{ "Open file" icons: Read nucleotide resp. protein from file }

procedure TfMolWeight.imOpenNucleotideClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpen.InitialDir := GetCurrentDir;
  if dlgOpen.Execute then begin
    FileName := dlgOpen.Filename;
    edNucleotide.Lines.LoadFromFile(FileName);
  end;
end;

procedure TfMolWeight.imOpenProteinClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpen.InitialDir := GetCurrentDir;
  if dlgOpen.Execute then begin
    FileName := dlgOpen.Filename;
    edProtein.Lines.LoadFromFile(FileName);
  end;
end;

{ Auto-check molecule-type radio button, when user enters data in corresponding input field }
{ With new data entered, set bCalcDone to False = disable chemical composition calculation }

procedure TfMolWeight.edFormulaChange(Sender: TObject);

begin
  if edFormula.Text <> '' then
    rbFormula.Checked := True;
  bCalcDone := False;
end;

procedure TfMolWeight.edNucleotideChange(Sender: TObject);

begin
  if edNucleotide.Text <> '' then
    rbNucleotide.Checked := True;
  bCalcDone := False;
end;

procedure TfMolWeight.edProteinChange(Sender: TObject);

begin
  if edProtein.Text <> '' then
    rbProtein.Checked := True;
  bCalcDone := False;
end;

end.

