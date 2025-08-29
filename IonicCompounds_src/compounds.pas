{**********************************************}
{* Main unit for "IonicCompounds" application *}
{**********************************************}

unit compounds;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, LCLIntf, Grids;

type
  TAnion = record
    AMolecule: Char;
    AFormula, AName: string;
    ACharges: array of Integer;
  end;
  TAnions = array of TAnion;
  TCation = record
    CMolecule: Char;
    CFormula, CName: string;
    CCharge, CHydrogens: Integer;
  end;
  TCations = array of TCation;
  {*************}
  { TfCompounds }
  {*************}
  TfCompounds = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileName, mFileFormula, mFileExit: TMenuItem;
    mOptions, mOptionsQuestions: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, laName, laFormula: TLabel;
    edName, edUName, edFormula, edUFormula, edEval: TEdit;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileNameClick(Sender: TObject);
    procedure mFileFormulaClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iTest, iAnion, iCation: Integer;
    iQuestions, iQuestions0, iQuestion, iCorrect: Integer;
    sFormula, sName, sUFormula, sUName: string;
    aAnions: TAnions;
    aCations: TCations;
    aDone: array of record
      AnionDone, CationDone: Integer;
    end;
  end;

var
  fCompounds: TfCompounds;

implementation

{$R *.lfm}

{ Greatest Common Divisor }

function GCD (N1, N2: Integer): Integer;

var
  N0, NR : INTEGER;

begin
  N0 := 1;
  repeat
    NR := N1 mod N2;
    if NR = 0 then
      N0 := N2
    else begin
      N1 := N2;
      N2 := NR;
    end;
  until NR = 0;
  GCD := N0;
end;

{ Least Common Multiple }

function LCM (N1, N2: Integer): Integer;

var
  G : Integer;

begin
  G := GCD(N1, N2);
  LCM := (N1 * N2) div G;
end;

{ Transform all numbers to corr. subscripts }

function ApplySubScripts(S0: string): string;

const
  Subscripts: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84,
    #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  I: Integer;
  S: string;

begin
  S := S0;
  for I := 0 to 9 do
    S := StringReplace(S, IntToStr(I), Subscripts[I], [rfReplaceAll]);
  Result := S;
end;

{ Formatted grid output (right-alignment) }

function GFormat(N: Integer; S0: string): string;

var
  I, L: Integer;
  S: string;

begin
  S := IntToStr(N) + S0; L:= Length(S);
  for I := L to 4 do
    S := ' ' + S;
  Result := S;
end;

{ Read anions file }

procedure ReadAnions(out Anions: TAnions);

var
  N, C: Integer;
  Line, Charges: string;
  Mol: Char;
  InFile: Text;

begin
  Assign(InFile, 'anions.txt'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Trim(Line);
    if Line <> '' then begin
      if LeftStr(Line, 1) = '*' then begin
        // Lines beginning with an asterisk indicate if following items are atoms or ions
        Mol := Copy(Line, 3, 1)[1];
      end
      else begin
        // Normal lines: Anion data
        Inc(N); SetLength(Anions, N);
        with Anions[N - 1] do begin
          AMolecule := Mol;
          AFormula := Trim(Copy(Line, 1, 10));
          AName := Trim(Copy(Line, 11, 15));
          // Transition metals can mostly form ions with
          // different charges; ex: (Fe)2+ and (Fe)3+
          // The most common charges of this anion are
          // stored into the record's "Charges" array
          Charges := Trim(Copy(Line, 26, 10));
          C := 0;
          repeat
            Inc(C); SetLength(ACharges, C);
            ACharges[C - 1] := StrToInt(LeftStr(Charges, 2));
            Delete(Charges, 1, 2);
          until Charges = '';
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Read cations file }

procedure ReadCations(out Cations: TCations);

var
  N: Integer;
  Line: string;
  Mol: Char;
  InFile: Text;

begin
  Assign(InFile, 'cations.txt'); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Trim(Line);
    if Line <> '' then begin
      if LeftStr(Line, 1) = '*' then begin
        // Lines beginning with an asterisk indicate if following items are atoms or ions
        Mol := Copy(Line, 3, 1)[1];
      end
      else begin
        // Normal lines: Cation data
        Inc(N); SetLength(Cations, N);
        with Cations[N - 1] do begin
          CMolecule := Mol;
          CFormula := Trim(Copy(Line, 1, 10));
          CName := Trim(Copy(Line, 11, 15));
          CCharge := StrToInt(Copy(Line, 26, 2));
          // For carbonates, sulfates, sulfites, and phosphates:
          // Number of hydrogen ions that may not dissolve in
          // an aquous solution of the corresponding acid, thus
          // possibility to form (HCO3)-, (HSO4)-, (HSO3)-,
          // (HPO4)2- and (H2PO4)- ions
          if Length(Line) > 28 then
            CHydrogens := StrToInt(Copy(Line, 31, 1))
          else
            CHydrogens := 0;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Determine the formula of an ionic compound by combining a given anion and a given cation }

function CompoundFormula(Anion: string; AnionCharge: Integer; Cation: string; CationCharge, Hydrogens: Integer): string;

var
  A, C: Integer;
  Formula: string;

begin
  Formula := '';
  if Hydrogens = 1 then begin
    // This actually is for (HCO3)-, (HSO4)-, (HSO3)-, and (HPO4)2-
    Cation := 'H' + Cation;
    CationCharge += 1;
  end
  else if Hydrogens = 2 then begin
    // This actually is for (H2PO4)-
    Cation := 'H2' + Cation;
    CationCharge += 2;
  end;
  if Abs(AnionCharge) = Abs(CationCharge) then begin
    // If the charges are the same, formula = anion name + cation name
    // Ex: Na+ and Cl- -> NaCl; Ca2+ and (SO4)2- -> CaSO4
    Formula := Anion + Cation;
  end
  else  begin
    // If the charges are not the same, we must calculate the number
    // of anions (m) and cations (n) in the ionic compound
    A := LCM(Abs(AnionCharge), Abs(CationCharge)) div Abs(AnionCharge);
    C := LCM(Abs(AnionCharge), Abs(CationCharge)) div Abs(CationCharge);
    // Anion part: (anion-name)m,
    // but use parentheses only for polyatomic anions (Hg2, NH4)
    // and do not display m if equals 1
    if (A > 1) and (Length(Anion) > 2) then
      Formula := '(';
    Formula += Anion;
    if (A > 1) and (Length(Anion) > 2) then
      Formula += ')';
    if A > 1 then
      Formula += IntToStr(A);
    // Cation part: (cation-name)n,
    // but use parentheses only for polyatomic anions (so, not for N, O, Cl, Br...)
    // and do not display n if equals 1
    if (C > 1) and ((Length(Cation) > 2) or ((Length(Cation) = 2) and not (Cation[2] in ['a'..'z']))) then
      Formula += '(';
    Formula += Cation;
    if (C > 1) and ((Length(Cation) > 2) or ((Length(Cation) = 2) and not (Cation[2] in ['a'..'z']))) then
      Formula += ')';
    if C > 1 then
      Formula += IntToStr(C);
  end;
  // Some corrections that have to be done...
  Formula := StringReplace(Formula, 'HOH', 'H2O', []);
  Formula := StringReplace(Formula, 'H2HPO3', 'H3PO3', []);
  Formula := StringReplace(Formula, 'H3N', 'NH3', []);
  Result := Formula;
end;

{ Determine the name of an ionic compound with given formula }

function CompoundName(Anion: TAnion; Cation: TCation; AnionCharge, Hydrogens: Integer): string;

const
  Roman: array[1..6] of string = (
    'I', 'II', 'III', 'IV', 'V', 'VI'
  );

var
  P: Integer;
  Name: string;

begin
  if Anion.AFormula = 'H' then begin
    // Ionic compounds with anion is H+
    // These are actually acids, and except for H2O, NH3, H2S, HCN, and H2O2
    // the name of the acid is the one expected by the program
    if Cation.CMolecule = 'A' then begin
      if (Cation.CFormula = 'O') then
        Name := 'water'
      else if (Cation.CFormula = 'N') then
        Name := 'hydrogen nitride'
      else if (Cation.CFormula = 'S') then
        Name := 'hydrogen sulfide'
      else begin
        // Acid names like hydrochloric acid, bydroiodic acid...
        Name := 'hydro';
        P := Pos('_', Cation.CName);
        Name += LeftStr(Cation.CName, P - 1) + 'ic';
        Name += ' acid';
      end;
    end
    else begin
      if (Cation.CFormula = 'OH') then
        Name := 'water'
      else if (Cation.CFormula = 'CN') then
        Name := 'hydrogen cyanide'
      else if (Cation.CFormula = 'O2') then
        Name := 'hydrogen peroxide'
      else begin
        Name += Cation.CName;
        // Cations in -ate give acids in -ic (ex: NO3- -> HNO3 = nitric acid),
        // cations in -ite give acuds in -ous (ex: NO2- -> HNO2 = nitrous acid)
        // However: sulfate -> sulfuric (and not sulfic),
        //          sulfite -> sulfurous (and not sulfous),
        //          phosphate -> phosphoric (and not phosphic),
        Name := StringReplace(Name, 'sulfate', 'sulfuric', []);
        Name := StringReplace(Name, 'sulfite', 'sulfurous', []);
        Name := StringReplace(Name, 'phosphate', 'phosphoric', []);
        Name := StringReplace(Name, 'ate', 'ic', []);
        Name := StringReplace(Name, 'ite', 'ous', []);
        Name += ' acid';
      end;
    end;
  end
  else begin
    // Ionic compounds with anion is not H+
    // The anion part of the name expected by the program is
    // the anion name followed by the charge as Roman number
    // between parentheses
    // Ex: Fe(II) and Fe(III), Cr(II), Cr(III) and Cr(VI)
    // The number of charges should be omitted if
    // the anion always has the same charge
    // Ex: Na, K, Ca, Ag, Zn;  Ag(I) and Zn(II) are accepted
    Name := Anion.AName;
    if Length(Anion.ACharges) > 1 then
      Name += '(' + Roman[AnionCharge] + ')'
    else if Anion.AFormula = 'Hg' then                                         // special case for (Hg)2+ anion ->  mercury(II)
      Name += '(II)';
    Name += ' ';
    // The cation part of the name depends on the cation being
    // mono- or polyatomic
    if Cation.CMolecule = 'A' then begin
      // Mono-atomic cations: names are ending in "ide"
      // EX: chor-ide, iod-ide
      P := Pos('_', Cation.CName);
      Name += LeftStr(Cation.CName, P - 1) + 'ide';
    end
    else begin
      // Poly-atomic cations: just use the cation name
      // Ex: nitrate, nitrite
      // For (CO3)2-, (SO4)2-, (SO3)2-, and (PO4)3-
      // the possibility of (HCO3)-, (HSO4)-, (HSO3)-,
      // (HPO4)2-, and (H2PO4)- ions must be condidered
      // The program awaits the names as "(di)hydrogen" + cation-name
      // Examples:
      // HCO3  -> hydrogen carbonate (bicarboante also accepted),
      // HPO4  -> hydrogen phospate (monohydrogen phosphate also accepted),
      // H2PO4 -> dihydrogen phosphate
      if Hydrogens > 0 then begin
        if Cation.CHydrogens = 1 then
          Name += 'hydrogen '
        else begin
          if Hydrogens = 1 then
            Name += 'hydrogen '
          else
            Name += 'dihydrogen ';
        end;
      end;
      Name += Cation.CName
    end;
  end;
  Result := Name;
end;

{ Get alternate name for ionic component }

function AlternateName(Compound0: string): string;

const
  Compounds: array[0..14, 0..1] of string = (
    // Most common name for NH3
    ('hydrogen nitride', 'ammonia'),
    // Name of the corresponding acid
    ('hydrogen sulfide', 'hydrosulfuric acid'),
    ('hydrogen cyanide', 'hydrocyanic acid'),
    ('hydrogen peroxide', 'perhydroxic acid'),
    // HCO3, HSo4, HSo3: usage of "bi-" instead of "hydrogen"
    ('hydrogen carbonate', 'bicarbonate'),
    ('hydrogen sulfate', 'bisulfate'),
    ('hydrogen sulfite', 'bisulfite'),
    // HPO4: as H2PO4 also exists, "monohydrogen" may be used
    ('hydrogen phosphate', 'monohydrogen phosphate'),
    // P2O7 and H4P2O7 are commonly named using "pyro" instead of "di"
    ('diphosphate', 'pyrophosphate'),
    ('diphosphoric acid', 'pyrophosphoric acid'),
    // (HPO3)2- ion is commonly named "phosphite",
    // H3PO3 is commonly named phosphorous acid
    ('phosphonate', 'phosphite'),
    ('phosphonic acid', 'phosphorous acid'),
    // For transition metal ions with always the same charge, allow
    // to specify the charge as with others where this is mandatory
    ('zinc', 'zinc(II)'),
    ('silver', 'silver(I)'),
    ('cadmium', 'cadmium(II)')
  );

var
  I: Integer;
  Compound: string;

begin
  Compound := Compound0;
  for I := 0 to 14 do
    Compound := StringReplace(Compound, Compounds[I, 0], Compounds[I, 1], []);
  Result := Compound;
end;

{*************}
{ TfCompounds }
{*************}

{ Application start: Initialization }

procedure TfCompounds.FormCreate(Sender: TObject);

// Some controls are moved to their final position within the application window, here

begin
  laName.Top := laFormula.Top; edName.Top := edFormula.Top;
  edUFormula.Top := edUName.Top;
  ReadAnions(aAnions); ReadCations(aCations);                                  // Read anions and cations files
  iQuestions0 := 20;
  Randomize;
  mFileName.Click;
end;

{ Menu item "File > Find compound name": Prepare for "Find compound name" exercise }

procedure TfCompounds.mFileNameClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 1;
  laFormula.Visible := True; edFormula.Visible := True; edUName.Visible := True;
  laName.Visible := False; edName.Visible := False; edUFormula.Visible := False;
  edFormula.Text := ''; edUName.Text := ''; edEval.Text := '';
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  btQuestion.Enabled := True; btQuestion.Caption := 'Start';
end;

{ Menu item "File > Find compound formula": Prepare for "Find compound formula" exercise }

procedure TfCompounds.mFileFormulaClick(Sender: TObject);

var
  I: Integer;

begin
  iTest := 2;
  laFormula.Visible := False; edFormula.Visible := False; edUName.Visible := False;
  laName.Visible := True; edName.Visible := True; edUFormula.Visible := True;
  edName.Text := ''; edUFormula.Text := ''; edEval.Text := '';
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  btQuestion.Enabled := True; btQuestion.Caption := 'Start';
end;

{ Menu item "File > Exit": Exit application }

procedure TfCompounds.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User input of the number of exercise questions }

procedure TfCompounds.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Ionic compounds', 'Number of exercise questions', IntToStr(iQuestions0));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 10 then
      iQuestions0 := 10;                                                       // arbitrarly fixed minimum
  end;
end;

{ Menu item "Help > Help": Open default text editor and load the file "readme.txt" }

procedure TfCompounds.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('readme.txt');
end;

{ Menu item "Help > About": Display application about }

procedure TfCompounds.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mineral chemistry:' + LineEnding;
  S += 'Nomenclature of ionic compounds exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January-February 2025.';
  MessageDlg('About "IonicCompounds"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer" pushed: Generate exercise question resp. check user's answer }

procedure TfCompounds.btQuestionClick(Sender: TObject);

var
  AnionCharge, CationCharge, Hydrogens, I, P: Integer;
  Anion, Cation, S: string;
  OK: Boolean;

begin
  // Button "Start/Question" pushed: Generate exercise question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Question') then begin
    // Button "Start": Some initializations before starting with the exercise
    if btQuestion.Caption = 'Start' then begin
      SetLength(aDone, 0);                                                     // "clear" the anion/cation done array
      iQuestions := iQuestions0;                                               // make "number of questions" entered by user become active
      iQuestion := 0; iCorrect := 0;                                           // reset questions and correct answers counters
    end;
    Inc(iQuestion);
    repeat
      // Get a random anion and a random cation among those anion-cation couples not yet done
      OK := True;
      iAnion  := Random(Length(aAnions));
      iCation := Random(Length(aCations));
      for I := 0 to Length(aDone) - 1 do begin
        if (iAnion = aDone[I].AnionDone) and (iCation = aDone[I].CationDone) then
          OK := False;
      end;
    until OK;
    // Add anion-cation couple to the anion/cation done array
    SetLength(aDone, Length(aDone) + 1);
    aDone[Length(aDone) - 1].AnionDone  := iAnion;
    aDone[Length(aDone) - 1].CationDone := iCation;
    // Anion/cation and their charges
    Anion  := aAnions[iAnion].AFormula;
    Cation := aCations[iCation].CFormula;
    AnionCharge  := aAnions[iAnion].ACharges[Random(Length(aAnions[iAnion].ACharges))];
    CationCharge := aCations[ication].CCharge;
    // Number of hydrogens (for CO3, SO4, etc)
    if (aCations[ication].CHydrogens = 0) or (Anion = 'H') then
      Hydrogens := 0
    else
      Hydrogens := Random(aCations[iCation].CHydrogens) + 1;
    // Compute the ionic component's formula
    sFormula := CompoundFormula(Anion, AnionCharge, Cation, CationCharge, Hydrogens);
    // Compute the ionic component's name
    sName := CompoundName(aAnions[iAnion], aCations[iCation], AnionCharge, Hydrogens);
    // Display the question item (formula or name)
    if iTest = 1 then begin
      // "Find name" exercise: Display the formula
      edFormula.Text := ApplySubscripts(sFormula);
      edUName.Text := '';
      edUName.SetFocus
    end
    else begin
      // "Find formula" exercise: Display the name
      edName.Text := sName;
      if Random(2) = 0 then begin
        // Display common name or alternate name (if this one is given and is not
        // 'zinc(II)', 'silver(I)', or 'cadmium(II)'
        S := AlternateName(sName);
        P := Pos('(', S);
        if P = 0 then
          edName.Text := S;
      end;
      edUFormula.Text := '';
      edUFormula.SetFocus;
    end;
    edEval.Text := '';
    btQuestion.Caption := 'Answer';                                            // next button push is for checking user answer
  end
  // Button "Answer" pushed: Check user's answer
  else begin
    OK := False;
    if iTest = 1 then begin
      // "Find name" exercise: Check if name entered by user equals the common or the alternate name of the compound
      sUName := edUName.Text;
      if (sUName = sName) or (sUName = AlternateName(sName)) then
        OK := True;
    end
    else begin
      // "Find formula" exercise: Check if name entered by user equals the formula of the compound
      sUFormula := edUFormula.Text;
      edUFormula.Text := ApplySubscripts(sUFormula);
      if sUFormula = sFormula then
        OK := True;
    end;
    // Question evaluation
    if OK then begin
      // Correct answer
      edEval.Text := 'This is correct.';
      Inc(iCorrect);
    end
    else begin
      // False answer
      edEval.Text := 'False! Correct is: ';
      if iTest = 1 then
        edEval.Text := edEval.Text + sName
      else
        edEval.Text := edEval.Text + ApplySubscripts(sFormula);
    end;
    // Fill in the evaluation grid
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * iCorrect / iQuestion), '%');
    if iQuestion < iQuestions then begin
      // If there are questions left, continue
      btQuestion.Caption := 'Question';                                        // next button push is for generating a new question
      btQuestion.SetFocus;
    end
    else begin
      // If all questions are done, terminate the exercise
      MessageDlg('Ionic compounds', 'Exercise terminated...', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // disable the button until user chooses to do another exercise
    end;
  end;
end;

end.

