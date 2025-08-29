{**************************************}
{* Main unit for AcidBase application *}
{**************************************}

unit acidbase_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8, acidbase_list, acidbase_help;

type
  TAcidBase = record
    ABName, ABFormula: string;
    ABGroups, ABGroupsUsed: Byte;
  end;
  TAcidsBases = array of TAcidBase;
  TIons = record
    Ion1Molecule, Ion2Molecule: string;
    Ion1Charge, Ion2Charge: Integer;
    Ion1Number, Ion2Number: Integer;
  end;
  TAcidBaseDone = record
    AcidDone, BaseDone: Integer;
  end;
  TAcidsBasesDone = array of TAcidBaseDone;
  TEditFields2x2 = array[0..2, 0..2] of TEdit;
  TStringArray2x2 = array[0..2, 0..2] of string;
  {************}
  { TfAcidBase }
  {************}
  TfAcidBase = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsSaltAlways: TMenuItem;
    mSettingsMolName, mSettingsMolNameQ, mSettingsMolNameA: TMenuItem;
    mHelp, mHelpList, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13: TLabel;
    laQuestion, laReaction, laEquation: TLabel;
    edAcidName, edAcidFormula, edAcidIons: TEdit;
    edBaseName, edBaseFormula, edBaseIons: TEdit;
    edSaltName, edSaltFormula, edSaltIons: TEdit;
    edEquation: TEdit;
    Memo1: TMemo;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btShow: TButton;
    btSubscipt: TButton;
    btSuperscript: TButton;
    btArrow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsSaltAlwaysClick(Sender: TObject);
    procedure mSettingsMolNameQClick(Sender: TObject);
    procedure mSettingsMolNameAClick(Sender: TObject);
    procedure mHelpListClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btSubsciptClick(Sender: TObject);
    procedure btSuperscriptClick(Sender: TObject);
    procedure btArrowClick(Sender: TObject);
    procedure edAcidFormulaEnter(Sender: TObject);
    procedure edAcidIonsEnter(Sender: TObject);
    procedure edBaseFormulaEnter(Sender: TObject);
    procedure edBaseIonsEnter(Sender: TObject);
    procedure edSaltFormulaEnter(Sender: TObject);
    procedure edSaltIonsEnter(Sender: TObject);
    procedure edEquationEnter(Sender: TObject);
  private
    iQuestionsTemp, iQuestions, iQuestion, iCorrect, iAcid, iBase: Integer;
    sEquation: string;
    bTablesFilled: Boolean;
    aAcids, aBases: TAcidsBases;
    rdAcidIons, rdBaseIons, rdSaltIons: TIons;
    edCurrentField: TEdit;
    aAcidsBasesDone: TAcidsBasesDone;
    aAnswerItems: TStringArray2x2;
    edQuestionItems: TEditFields2x2;
  end;

const
  MaxCoeff = 6;                                                                // used for equation balancing (works with all examples included)
  SUP_PLUS = #$E2#$81#$BA;
  SUP_MINUS = #$E2#$81#$BB;
  SUB_Digits: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84, #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );
  SUP_Digits: array[0..9] of string = (
     #$E2#$81#$B0, #$C2#$B9, #$C2#$B2, #$C2#$B3, #$E2#$81#$B4, #$E2#$81#$B5, #$E2#$81#$B6, #$E2#$81#$B7, #$E2#$81#$B8, #$E2#$81#$B9
  );
  CoeffCodes: array[0..MaxCoeff] of Char = (
    '?', '$', '%', '&', '&', '#', '*'
  );

var
  fAcidBase: TfAcidBase;

implementation

{$R *.lfm}

{ Format numbers for grid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN += S;                                                                   // this is for the '%' sign
  Result := SN;
end;

{ Check if a character is numeric }

function IsNumeric(C: Char): Boolean;

var ItIs: Boolean;

begin
  ItIs := False;
  if C in ['0'..'9'] then
    ItIs := True;
  Result := ItIs;
end;

{ Check if an acid is monoatomic }

function IsMonoAtomic(Formula: string): Boolean;

var
  ItIs: Boolean;

begin
  ItIs := False;
  Delete(Formula, 1, 1);                                                       // delete hydrogen symbol
  if LeftStr(Formula, 1)[1] in ['1' .. '9'] then
    Delete(Formula, 1, 1);                                                     // delete number of hydrogens (if any)
  if Length(Formula) = 1 then
    // Ex: HF
    ItIs := True
  else if (Length(Formula) = 2) and (Formula[2] in ['1'..'9', 'a'..'z']) then
    // Ex: HCl, or HN3, but not HCN
    ItIs := True
  else if (Length(Formula) = 3) and (Formula[2] in ['a'..'z']) and (Formula[3] in ['1'..'9']) then
    // No example actually included
    ItIs := True;
  Result := ItIs;
end;

{ Apply subscript to formula (or just convert number to subscript) }

function FormulaSubscripts(Formula: string): string;

var
  I: Integer;

begin
  for I := 2 to 9 do
    Formula := StringReplace(Formula, IntToStr(I), SUB_Digits[I], [rfReplaceAll]);
  Result := Formula;
end;

{ Apply superscript to formula (or just convert number or +/- sign to superscript) }

function FormulaSuperscripts(Formula: string): string;

var
  I: Integer;

begin
  for I := 2 to 9 do
    Formula := StringReplace(Formula, IntToStr(I), SUP_Digits[I], [rfReplaceAll]);
  Formula := StringReplace(Formula, '+', SUP_Plus, []);
  Formula := StringReplace(Formula, '-', SUP_Minus, []);
  Result := Formula;
end;

{ Apply subscripts to (molecular) equation }

function EquationSubscripts(Equation: string): string;

var
  P, P1, P2: Integer;
  SubEquation: string;

begin
  Equation := StringReplace(Equation, ' ', '', [rfReplaceAll]);                // remove all spaces
  SubEquation := '';
  // Cut equation text in pieces, each piece being molecule number plus the molecule itself
  // Take molecule number as is; apply subscripts to molecule formula
  repeat
    if IsNumeric(UTF8Copy(Equation, 1, 1)[1]) then begin
      // "Piece" starts with a molecule number: just keep it
      SubEquation += UTF8Copy(Equation, 1, 1);
      UTF8Delete(Equation, 1, 1)
    end;
    // Cut the equation by using '+' and '→' as "piece" delimiters
    P1 := UTF8Pos('+', Equation); P2 := UTF8Pos('→', Equation); P := 0;
    if (P1 > 0) and ((P2 = 0) or (P1 < P2)) then
      // There is a '+' and it is located before the eventual '→'
      P := P1
    else if (P2 > 0) and (P2 < P1) then
      // There is a '→' (and as the '+' before have already been treated, is located before the next '+')
      P := P2;
    if P > 0 then begin
      // "Piece" separator found
      SubEquation += FormulaSubscripts(UTF8Copy(Equation, 1, P - 1)) + ' ' + UTF8Copy(Equation, P, 1) + ' ';
      UTF8Delete(Equation, 1, P);
    end
    else begin
      // Last "piece" of the equation
      SubEquation += FormulaSubscripts(Equation);
      Equation := '';
    end;
  until Equation = '';
  Result := SubEquation;
end;

{ Read acids and bases from text files and fill TAcidsBases arrays }

procedure ReadAcidsBases(out Acids, Bases: TAcidsBases);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Acids, 0); SetLength(Bases, 0);
  // Read acids
  Assign(InFile, 'acids.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Acids, N);
      Acids[N - 1].ABName := Trim(Copy(Line, 1, 30));
      Acids[N - 1].ABFormula := Trim(Copy(Line, 31, 10));
      Acids[N - 1].ABGroups := StrToInt(Copy(Line, 41, 1));
      Acids[N - 1].ABGroupsUsed := StrToInt(Copy(Line, 42, 1));
    end;
  end;
  Close(InFile);
  // Read bases
  Assign(InFile, 'bases.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Bases, N);
      Bases[N - 1].ABName := Trim(Copy(Line, 1, 30));
      Bases[N - 1].ABFormula := Trim(Copy(Line, 31, 10));
      Bases[N - 1].ABGroups := StrToInt(Copy(Line, 41, 1));
      Bases[N - 1].ABGroupsUsed := StrToInt(Copy(Line, 42, 1));
    end;
  end;
  Close(InFile);
end;

{ Clear form edit fields }

procedure ResetForm(var EditFields: TEditFields2x2);

var
  I, J: Integer;

begin
  fAcidBase.laQuestion.Caption := 'Reaction:';
  for I := 0 to 2 do begin
    for J := 0 to 2 do begin
      EditFields[I, J].Text := '';
      EditFields[I, J].Color := clDefault;
      EditFields[I, J].ReadOnly := False;
      EditFields[I, J].TabStop := True;
      EditFields[I, J].Color := clDefault;
    end;
  end;
  fAcidBase.edEquation.Text := ''; fAcidBase.edEquation.Color := clDefault;
end;

{ Determine ion formula (with sub-/superscripts) }

function IonFormula(Ions: TIons; N: Integer): string;

// The data to create the formula is passed as a TIons record,
// N indicates if the formula has to be for the first or second ion
// (this is no proper coding...)

var
  Mol, SCharge: string;
  Charge: Integer;

begin
  // Use first or second ion data, depending on N
  if N = 1 then begin
    Mol := Ions.Ion1Molecule; Charge := Ions.Ion1Charge;
  end
  else begin
    Mol := Ions.Ion2Molecule; Charge := Ions.Ion2Charge;
  end;
  // Molecule (with subscripts)
  Mol := FormulaSubscripts(Mol);
  // Ion charge (as superscript)
  SCharge := '';
  if Abs(Charge) > 1 then
    // Has to be '+'/'-' instead of '1+'/'1-'
    SCharge += IntToStr(Abs(Charge));
  if Charge > 0 then
    SCharge += '+'
  else
    SCharge += '-';
  SCharge := FormulaSuperscripts(SCharge);
  Result := Mol + SCharge;
end;

{ Determine salt molecular formula (with subscripts) }

function SaltFormula(SaltIons: TIons): string;

// The data to create the formula is passed as a TIons record,
// first ion being the anion, second one being the cation

var
  Mol1, Mol2: string;

begin
  // Anion
  Mol1 := SaltIons.Ion1Molecule;
  if SaltIons.Ion1Number > 1 then begin
    // Number of anions only if > 1
    if (Length(Mol1) > 2) or ((Length(Mol1) = 2) and not (Mol1[2] in ['a'..'z'])) then
      // Anion is an atomic group: use round brackets
      Mol1 := '(' + Mol1 + ')' + IntToStr(SaltIons.Ion1Number)
    else
      // Anion is monoatomic (no brackets)
      Mol1 += IntToStr(SaltIons.Ion1Number);
  end;
  // Cation
  Mol2 := SaltIons.Ion2Molecule;
  if SaltIons.Ion2Number > 1 then begin
    // Number of cations only if > 1
    if (Length(Mol2) > 2) or ((Length(Mol2) = 2) and not (Mol2[2] in ['a'..'z'])) then
      // Cation is an atomic group: use round brackets
      Mol2 := '(' + Mol2 + ')' + IntToStr(SaltIons.Ion2Number)
    else
      // Cation is monoatomic (no brackets)
      Mol2 += IntToStr(SaltIons.Ion2Number)
  end;
  // Formula = anion + cation
  Result := Mol1 + Mol2;
end;

{ Determine name of a salt }

function SaltName(Acid, Base: TAcidBase; AcidIonStep: Integer): string;

// The data to create the name is passed as two TAcidsBases records (with info
// about the acid and base, the salts results from) and the actual ionization
// step (telling if and how many hydrogens are still part of the salt)

// The salt name globaly is the base name (withou "hydroxide") plus a modification
// of the acid name (without "acid" and without "hydro"), the modification to be
// done, depending on the fact, if the acid is monoatomic or not
// Have a look at a chemistry book for details

const
  // hydrogen, dihydrogen... based salt nomenclature
  NHydrogens: array[1..4] of string =
    ( '', 'di', 'tri', 'tetra');

var
  H: Integer;
  SAcid, SBase: string;

begin
  // Salt name part, taken from the acid name
  SAcid := LowerCase(Acid.ABName);
  SAcid := StringReplace(SAcid, ' acid', '', []);
  SAcid := StringReplace(SAcid, 'hydro', '', []);
  if IsMonoatomic(Acid.ABFormula) then begin
    // Monoatomic acids
    SAcid := StringReplace(SAcid, 'ic', 'ide', []);
  end
  else begin
    // Polyatomic acids
    if RightStr(SAcid, 2) = 'ic' then begin
      // Acids in -ic become salts in -ate
      if RightStr(SAcid, 8) = 'sulfuric' then
        SAcid := StringReplace(SAcid, 'sulfuric', 'sulfate', [])
      else if RightStr(SAcid, 10) = 'phosphoric' then
        SAcid := StringReplace(SAcid, 'phosphoric', 'phosphate', [])
      else
        SAcid := StringReplace(SAcid, 'ic', 'ate', []);
    end
    else if RightStr(SAcid, 3) = 'ous' then begin
      // Acids in -ous become salts in -ite
      if RightStr(SAcid, 9) = 'sulfurous' then
        SAcid := StringReplace(SAcid, 'sulfurous', 'sulfite', [])
      else if RightStr(SAcid, 11) = 'phosphorous' then
        SAcid := StringReplace(SAcid, 'phosphorous', 'phosphite', [])
      else
        SAcid := StringReplace(SAcid, 'ous', 'ite', []);
    end
  end;
  // Salt name part, taken from the base name
  if Base.ABName = 'Ammonia' then
    SBase := 'Ammonium'
  else
    SBase := StringReplace(Base.ABName, ' hydroxide', '', []);
  // Considering eventually not ionized hydrogens
  H := Acid.ABGroups - AcidIonStep;
  if H > 0 then
    SAcid := NHydrogens[H] + 'hydrogen ' + SAcid;
  // Salt name = "base name part" + "acid name part"
  Result := SBase + ' ' + SAcid;
end;

{ Perform acid-base reaction for given acid and base and given acid ionization step }

procedure AcidBaseReaction(Acid, Base: TAcidBase; IonisationStep: Integer; out AcidIons, BaseIons, SaltIons: TIons);

// Procedure outputs are TIons records for acid, base and salt (first ion = anion, second ion = cation)

var
  HLeft, I, J: Integer;

begin
  // Acid ionization
  AcidIons.Ion1Molecule := 'H'; AcidIons.Ion1Charge := 1;
  if IsNumeric(Copy(Acid.ABFormula, 2, 1)[1]) then begin
    // Polyproteic acid: Ions produced depend on ionization step
    AcidIons.Ion1Number := IonisationStep;
    HLeft := StrToInt(Copy(Acid.ABFormula, 2, 1)) - AcidIons.Ion1Number;
    AcidIons.Ion2Molecule := Acid.ABFormula;
    Delete(AcidIons.Ion2Molecule, 1, 2);
    if HLeft > 0 then begin
      // Not all hydrogen ionized: one or more 'H' to add to ion formula
      if HLeft = 1 then
        AcidIons.Ion2Molecule := 'H' + AcidIons.Ion2Molecule
      else
        AcidIons.Ion2Molecule := 'H' + IntToStr(HLeft) + AcidIons.Ion2Molecule;
    end;
  end
  else begin
    // Monoproteic acid
    AcidIons.Ion2Molecule := RightStr(Acid.ABFormula, Length(Acid.ABFormula) - 1);
    AcidIons.Ion1Number := 1;
  end;
  AcidIons.Ion2Charge := -AcidIons.Ion1Number;
  AcidIons.Ion2Number := 1;
  // Base ionization
  if Base.ABFormula = 'NH3' then begin
    // Considering NH3 + H20 -> NH4OH before doing acid-base reaction
    Base.ABFormula := 'NH4OH';                                                 // locally changed only...
    Base.ABGroups := 1; Base.ABGroupsUsed := 1;
  end;
  BaseIons.Ion2Molecule := 'OH'; BaseIons.Ion2Charge := -1;
  if Base.ABGroups = 1 then begin
    // One single -OH group
    BaseIons.Ion2Number := 1;
    BaseIons.Ion1Molecule := LeftStr(Base.ABFormula, Length(Base.ABFormula) - 2);
    BaseIons.Ion1Charge := 1;
  end
  else begin
    // Multiple -OH groups
    BaseIons.Ion2Number := StrToInt(RightStr(Base.ABFormula, 1));
    BaseIons.Ion1Molecule := LeftStr(Base.ABFormula, Length(Base.ABFormula) - 5);
    BaseIons.Ion1Charge := BaseIons.Ion2Number;
  end;
  BaseIons.Ion1Number := 1;
  SaltIons.Ion1Molecule := BaseIons.Ion1Molecule; SaltIons.Ion2Molecule := AcidIons.Ion2Molecule;
  SaltIons.Ion1Charge   := BaseIons.Ion1Charge;   SaltIons.Ion2Charge   := AcidIons.Ion2Charge;
  // Determination of the number of salt ions (first = anion, second = cation)
  for I := MaxCoeff downto 1 do begin
    for J := MaxCoeff downto 1 do begin
      if SaltIons.Ion1Charge * I + SaltIons.Ion2Charge * J = 0 then begin
        SaltIons.Ion1Number := I;
        SaltIons.Ion2Number := J;
      end;
    end;
  end;
end;

{ Determine acid-base reaction molecular equation (without subscripts) }

function AcidBaseEquation(Acid, Base, Salt: string; AcidIons, BaseIons, SaltIons: TIons): string;

// The data to create the equation is passed as three TAcidsBases records (with info about acid, base and salt)

var
  I1, I2, I3, I4, C1, C2, C3, C4: Integer;
  Equ: string;

begin
  // Determination of the reactants and products coefficients (equation balancing)
  // This simple way to balance an acid-base reaction equation considers atomic groups, rather
  // than single atoms and thus can't be used for other kinds of chemical reactions
  C1 := 0; C2 := 0; C3 := 0; C4 := 0;
  for I1 := MaxCoeff downto 1 do begin
    for I2 := MaxCoeff downto 1 do begin
      for I3 := MaxCoeff downto 1 do begin
        for I4 := MaxCoeff downto 1 do begin
          if (I1 * AcidIons.Ion1Number + I2 * BaseIons.Ion2Number = 2 * I4) and
             (I1 * AcidIons.Ion2Number = I3 * SaltIons.Ion2Number) and
             (I2 * BaseIons.Ion1Number = I3 * SaltIons.Ion1Number) and
             (I2 * BaseIons.Ion2Number = I4 * 1) then begin
            C1 := I1; C2 := I2; C3 := I3; C4 := I4;
          end;
        end;
      end;
    end;
  end;
  // Putting coefficients and molecules together
  Equ := IntToStr(C1) + Acid + ' + ' + IntToStr(C2);
  if Base = 'NH3' then
    // NH3 reacts with water before reacting with the acid
    Equ += 'NH4OH'
  else
    Equ += Base;
  Equ +=  ' → ' + IntToStr(C3) + Salt + ' + ' + IntToStr(C4) + 'H2O';
  Equ := StringReplace(Equ, '1', '', [rfReplaceAll]);                          // coefficients of 1 have not to be written
  Result := Equ;
end;

{************}
{ TfAcidBase }
{************}

{ Application start: Initialisation }

procedure TfAcidBase.FormCreate(Sender: TObject);

begin
  edQuestionItems[0, 0] := edAcidName; edQuestionItems[0, 1] := edAcidFormula; edQuestionItems[0, 2] := edAcidIons;
  edQuestionItems[1, 0] := edBaseName; edQuestionItems[1, 1] := edBaseFormula; edQuestionItems[1, 2] := edBaseIons;
  edQuestionItems[2, 0] := edSaltName; edQuestionItems[2, 1] := edSaltFormula; edQuestionItems[2, 2] := edSaltIons;
  ReadAcidsBases(aAcids, aBases);
  iQuestionsTemp := 20;
  bTablesFilled := False;                                                      // variable used to avoid filling the table on fList more than once...
  Randomize;
  mTestNew.Click;
end;

{ Menu item "Test > New": Start a new test }

procedure TfAcidBase.mTestNewClick(Sender: TObject);

var
  I: Integer;

begin
  // Make chosen number of questions active
  iQuestions := iQuestionsTemp;
  // Reset evaluation counters
  iQuestion := 0; iCorrect := 0;
  // The number of acid-base pairs being asked = the actually chosen number of test questions
  SetLength(aAcidsBasesDone, iQuestions);
  // Set all acid-base pairs as "not yet done"
  for I := 0 to Length(aAcidsBasesDone) - 1 do begin
    aAcidsBasesDone[I].AcidDone := -1; aAcidsBasesDone[I].BaseDone := -1;
  end;
  // Clear form edit fields
  ResetForm(edQuestionItems);
  // Reset evaluation grid
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  // "Block" editing buttons by setting actual edit field to "nothing"
  edCurrentField := nil;
  // Set button properties
  btQuestion.Caption := 'Question'; btQuestion.Enabled := True; btShow.Enabled := False;
end;

{ Menu item "Test > Exit": Exit application }

procedure TfAcidBase.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Number of questions...": User entry of number of test questions }

procedure TfAcidBase.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Acid-base reactions', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
  if iQuestionsTemp < 10 then
    iQuestionsTemp := 10;                                                      // arbitrarily fixed minimum
end;

{ Menu item "Settings > Ask always for salt": Toggle include/don't include questions, asking for acid and base }

procedure TfAcidBase.mSettingsSaltAlwaysClick(Sender: TObject);

begin
  if mSettingsSaltAlways.Checked then
    mSettingsSaltAlways.Checked := False
  else
    mSettingsSaltAlways.Checked := True;
end;

{ Menu item "Settings > Molecule names > Ask in questions": Toggle include/don't include questions where name is given instead of formula }

procedure TfAcidBase.mSettingsMolNameQClick(Sender: TObject);

begin
  if mSettingsMolNameQ.Checked then
    mSettingsMolNameQ.Checked := False
  else
    mSettingsMolNameQ.Checked := True;
end;

{ Menu item "Settings > Molecule names > Ask in answers": Toggle user must/must not enter the molecule names }

procedure TfAcidBase.mSettingsMolNameAClick(Sender: TObject);

begin
  if mSettingsMolNameA.Checked then
    mSettingsMolNameA.Checked := False
  else
    mSettingsMolNameA.Checked := True;
end;

{ Menu item "Help > Acids/bases list": Display list of acids/bases used in the exercises }

procedure TfAcidBase.mHelpListClick(Sender: TObject);

var
  I: Integer;

begin
  if not bTablesFilled then begin
    // If first time execution, fill the acids and bases string-grids on fList form
    for I := 0 to Length(aAcids) - 1 do begin
      fList.sgAcids.Cells[0, I + 1] := aAcids[I].ABName;
      fList.sgAcids.Cells[1, I + 1] := FormulaSubscripts(aAcids[I].ABFormula);
    end;
    for I := 0 to Length(aBases) - 1 do begin
      fList.sgBases.Cells[0, I + 1] := aBases[I].ABName;
      fList.sgBases.Cells[1, I + 1] := FormulaSubscripts(aBases[I].ABFormula);
    end;
    bTablesFilled := True;
  end;
  // Show or hide the Acids/bases list window
  if fList.Visible then
    fList.Close
  else
    fList.Show;
end;

{ Menu item "Help > Application help": Display application help }

procedure TfAcidBase.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfAcidBase.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry trainer: Mineral acid-base reaction exercises.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, May-August 2020.';
  MessageDlg('About "AcidBase"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new question (acid-base reaction) resp. check user answer}

procedure TfAcidBase.btQuestionClick(Sender: TObject);

var
  QType, QType2, AcidIonisationStep, I, J: Integer;
  Salt, Equ, Answer: string;
  OK, Correct: Boolean;
  AnswerItems: TStringArray2x2;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    ResetForm(edQuestionItems);
    Inc(iQuestion);
    laQuestion.Caption := 'Reaction ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ':';
    // Get random acid and base until found a pair that has not yet been used together
    repeat
      OK := True;
      iAcid := Random(Length(aAcids)); iBase := Random(Length(aBases));
      if iQuestion > 1 then begin
        for I := 1 to iQuestion - 1 do begin
          if (iAcid = aAcidsBasesDone[I - 1].AcidDone)  and (iBase = aAcidsBasesDone[I - 1].BaseDone) then
            OK := False;
        end;
      end;
    until OK;
    // Mark this acid-base pair as "done"
    aAcidsBasesDone[iQuestion - 1].AcidDone := iAcid; aAcidsBasesDone[iQuestion - 1].BaseDone := iBase;
    // Write acid/base name and formula into array (will be used for answer check)
    aAnswerItems[0, 0] := aAcids[iAcid].ABName; aAnswerItems[0, 1] := FormulaSubscripts(aAcids[iAcid].ABFormula);
    aAnswerItems[1, 0] := aBases[iBase].ABName; aAnswerItems[1, 1] := FormulaSubscripts(aBases[iBase].ABFormula);
    // Random ionization step (for polyproteic acids)
    laReaction.Visible := False;
    AcidIonisationStep := aAcids[iAcid].ABGroups;
    if aAcids[iAcid].ABGroups > 1 then begin
      if Random(2) = 0 then
        AcidIonisationStep := aAcids[iAcid].ABGroups - Random(aAcids[iAcid].ABGroupsUsed);
      laReaction.Visible := True;
      laReaction.Caption := 'Acid ionisation step = ' + IntToStr(AcidIonisationStep) + '. ';
    end;
    // Perform the acid-base reaction
    AcidBaseReaction(aAcids[iAcid], aBases[iBase], AcidIonisationStep, rdAcidIons, rdBaseIons, rdSaltIons);
    // Get salt formula (from ions record)
    Salt := SaltFormula(rdSaltIons);
    // Fill in rest of array (salt name and formula, all ions)
    aAnswerItems[2, 1] := FormulaSubscripts(Salt);
    aAnswerItems[2, 0] := SaltName(aAcids[iAcid], aBases[iBase], AcidIonisationStep);
    aAnswerItems[0, 2] := IonFormula(rdAcidIons, 1) + ', ' + IonFormula(rdAcidIons, 2);
    aAnswerItems[1, 2] := IonFormula(rdBaseIons, 1) + ', ' + IonFormula(rdBaseIons, 2);
    aAnswerItems[2, 2] := IonFormula(rdSaltIons, 1) + ', ' + IonFormula(rdSaltIons, 2);
    // Get reaction equation
    sEquation := AcidBaseEquation(aAcids[iAcid].ABFormula, aBases[iBase].ABFormula, Salt, rdAcidIons, rdBaseIons, rdSaltIons);
    // Randomly (based on selected possibilities) show one or the other data (and have user to determine the one not shown)
    if mSettingsSaltAlways.Checked or (aBases[iBase].ABFormula = 'NH3') then
      QType := 1                                                               // QType: show acid and base resp. show salt
    else
      QType := Random(3);
    if not mSettingsMolNameQ.Checked then
      QType2 := 1                                                              // QType2: show formula resp. show name
    else
      QType2 := Random(3);
    if QType > 0 then begin
      // Question type: Acid and base are given (salt to be determined by user)
      if QType2 > 0 then begin
        // Acid and base are given by formula
        edAcidFormula.ReadOnly := True; edAcidFormula.TabStop := False; edAcidFormula.Color := clCream;
        edAcidFormula.Text := FormulaSubscripts(aAcids[iAcid].ABFormula);
        edBaseFormula.ReadOnly := True; edBaseFormula.TabStop := False; edBaseFormula.Color := clCream;
        edBaseFormula.Text := FormulaSubscripts(aBases[iBase].ABFormula);
      end
      else begin
        // Acid and base are given by name
        edAcidName.ReadOnly := True; edAcidName.TabStop := False; edAcidName.Color := clCream;
        edBaseName.ReadOnly := True; edBaseName.TabStop := False; edBaseName.Color := clCream;
        edAcidName.Text := aAcids[iAcid].ABName; edBaseName.Text := aBases[iBase].ABName;
      end;
      if not mSettingsMolNameA.Checked then begin
        // If molecule names are not included in the test, display acid and base name
        edAcidName.ReadOnly := True; edAcidName.TabStop := False; edAcidName.Color := clCream;
        edBaseName.ReadOnly := True; edBaseName.TabStop := False; edBaseName.Color := clCream;
        edSaltName.ReadOnly := True; edSaltName.TabStop := False; edSaltName.Color := clCream;
        edAcidName.Text := aAcids[iAcid].ABName; edBaseName.Text := aBases[iBase].ABName;
      end;
    end
    else begin
      // Question type: Salt is given (acid and base to be determined by user)
      if QType2 > 0 then begin
        // Salt is given by formula
        edSaltFormula.ReadOnly := True; edSaltFormula.TabStop := False; edSaltFormula.Color := clCream;
        edSaltFormula.Text := FormulaSubscripts(aAnswerItems[2, 1]);
        if not mSettingsMolNameA.Checked then begin
          // If molecule names are not included in the test, display salt name
          edSaltName.ReadOnly := True; edSaltName.TabStop := False; edSaltName.Color := clCream;
          edSaltName.Text := aAnswerItems[2, 0];
        end;
      end
      else begin
        // Salt is given by name
        edSaltName.ReadOnly := True; edSaltName.TabStop := False; edSaltName.Color := clCream;
        edSaltName.Text := aAnswerItems[2, 0];
      end;
      if not mSettingsMolNameA.Checked then begin
        // If molecule names are not included in the test, set fields for acid and base names read-only
        edAcidName.ReadOnly := True; edAcidName.TabStop := False; edAcidName.Color := clCream;
        edBaseName.ReadOnly := True; edBaseName.TabStop := False; edBaseName.Color := clCream;
      end;
    end;
    // Focus the first edit field that has to be filled in (i.e. the first not read-only one)
    for I := 2 downto 0 do begin
      for J := 2 downto 0 do begin
        if not edQuestionItems[I, J].ReadOnly then
          edQuestionItems[I, J].SetFocus;
      end;
    end;
    // Next button push will be for user answer entry
    btQuestion.Caption := 'Answer'; btShow.Enabled := False;
  end
  // Button "Answer": Check user answer
  else begin
    if not mSettingsMolNameA.Checked then begin
      // If molecule names are not included in the test, display all names now
      edAcidName.Text := aAnswerItems[0, 0];
      edBaseName.Text := aAnswerItems[1, 0];
      edSaltName.Text := aAnswerItems[2, 0];
    end;
    Correct := True;
    // Check all items entered by the user; if one isn't correct, the global answer is false
    for I := 0 to 2 do begin
      for J := 0 to 2 do begin
        if not edQuestionItems[I, J].ReadOnly then begin
          // Do checking for user entered items only
          AnswerItems[I, J] := edQuestionItems[I, J].Text;
          Answer := aAnswerItems[I, J];
          if J = 2 then begin
            // Eliminate spaces around the comma, that separates the ions (for user conveniance)
            AnswerItems[I, J] := StringReplace(AnswerItems[I, J], ' ', '', [rfReplaceAll]);
            Answer := StringReplace(Answer, ' ', '', [rfReplaceAll]);
          end;
          if AnswerItems[I, J] <> Answer then begin
            // Wrong answer: Global answer is false; color the fields with wrong answers
            Correct := False;
            edQuestionItems[I, J].Color := clRed;
          end;
        end;
      end;
    end;
    // Check the reaction equation (comparing strings without subscripts)
    Equ := edEquation.Text;
    for I := 0 to 9 do
      // Replace subscripts in user equation by "ordinary numbers"
      Equ := StringReplace(Equ, SUB_Digits[I], IntToStr(I), [rfReplaceAll]);
    // Remove all spaces form user equation
    Equ := StringReplace(Equ, ' ', '', [rfReplaceAll]);
    // Remove all spaces from calculated equation (that actually has no subscripts)
    Answer := StringReplace(sEquation, ' ', '', [rfReplaceAll]);
    if Equ <> Answer then begin
      // Wrong answer: Global answer is false;; color the equation field
      Correct := False;
      edEquation.Color := clRed;
    end;
    // Update evaluation counters and fill-in evaluation grid
    if Correct then
      Inc(iCorrect)
    else
      btShow.Enabled := True;                                                  // Give user possibility to view correct answer
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * iCorrect / iQuestion), '%');
    btQuestion.Caption := 'Question';
    // All questions done: End of test message
    if iQuestion = iQuestions then begin
      MessageDlg('Acid-base reactions', 'All questions done. End of test.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // user has to select "New" in the "Test" menu to continue
    end;
  end;
end;

{ Button "Show": Show all answer items }

procedure TfAcidBase.btShowClick(Sender: TObject);

var
  I, J: Integer;

begin
  // Show acid, base, salt answers
  for I := 0 to 2 do begin
    for J := 0 to 2 do begin
      if edQuestionitems[I, J].Color = clRed then begin
        // Do only for false answers (with colored edit field)
        edQuestionitems[I, J].Color := clYellow;
        edQuestionitems[I, J].Text := aAnswerItems[I, J];
      end;
    end;
  end;
  // Show reaction equation
  if edEquation.Color = clRed then begin
    edEquation.Color := clYellow;
    edEquation.Text := EquationSubscripts(sEquation);                          // equation has to be tranformed first (applying subscripts)
  end;
end;

{ Button "Subscript": Transform last character in actual edit field to subscript }

procedure TfAcidBase.btSubsciptClick(Sender: TObject);

var
  Chr, Subscript: string;

begin
  if (edCurrentField <> nil) and (edCurrentField.Text <> '') then begin
    Chr := RightStr(edCurrentField.Text, 1);
    if not IsNumeric(Chr[1]) then
      Chr := '';
    if Chr <> '' then begin
      // Transform numeric characters only
      Subscript := FormulaSubscripts(Chr);
      edCurrentField.Text := StringReplace(edCurrentField.Text, Chr, Subscript, []);
      edCurrentField.SetFocus;
    end;
  end;
end;

{ Button "Superscript": Transform last character in actual edit field to superscript }

procedure TfAcidBase.btSuperscriptClick(Sender: TObject);

var
  Chr, Superscript: string;

begin
  if (edCurrentField <> nil) and (edCurrentField.Text <> '') then begin
    Chr := RightStr(edCurrentField.Text, 1);
    if (Chr <> '+') and (Chr <> '-') then begin
      if not IsNumeric(Chr[1]) then
        Chr := '';
    end;
    if Chr <> '' then begin
      // Transform numeric characters and '+'/'-' signs only
      Superscript := FormulaSuperscripts(Chr);
      edCurrentField.Text := StringReplace(edCurrentField.Text, Chr, Superscript, []);
      edCurrentField.SetFocus;
    end;
  end;
end;

{ "Arrow" button: Insert arrow into actual edit field (in fact: into equation field) }

procedure TfAcidBase.btArrowClick(Sender: TObject);

begin
  if (edCurrentField <> nil) and (edCurrentField = edEquation) and (edCurrentField.Text <> '') then begin
    edEquation.Text := edEquation.Text + ' → ';
    edEquation.SetFocus;
  end;
end;

{ Mouse entry into a given edit fields: Define this edit field as the actually edited one (-> "insert" buttons action) }

procedure TfAcidBase.edAcidFormulaEnter(Sender: TObject);

begin
  edCurrentField := edAcidFormula;
end;

procedure TfAcidBase.edAcidIonsEnter(Sender: TObject);

begin
  edCurrentField := edAcidIons;
end;

procedure TfAcidBase.edBaseFormulaEnter(Sender: TObject);

begin
  edCurrentField := edBaseFormula;
end;

procedure TfAcidBase.edBaseIonsEnter(Sender: TObject);

begin
  edCurrentField := edBaseIons;
end;

procedure TfAcidBase.edSaltFormulaEnter(Sender: TObject);

begin
  edCurrentField := edSaltFormula;
end;

procedure TfAcidBase.edSaltIonsEnter(Sender: TObject);

begin
  edCurrentField := edSaltIons;
end;

procedure TfAcidBase.edEquationEnter(Sender: TObject);

begin
  edCurrentField := edEquation;
end;

end.

