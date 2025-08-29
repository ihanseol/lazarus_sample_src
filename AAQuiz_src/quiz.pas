{************************************}
{* Main unit for AAQuiz application *}
{************************************}

unit quiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

type
  TArray = array[0..3] of string;
  TStructure = record
    Aliphatic, Aromatic, Cyclic, SulfurContaining, Acidic, Basic, Neutral: Boolean;
  end;
  TPolarity = record
    Polar, Nonpolar: Boolean;
  end;
  TCharge = record
    Positive, Negative, Uncharged: Boolean;
  end;
  TPolarity2 = record
    Positive, Negative, PolarUncharged, Nonpolar: Boolean;
  end;
  THydropathy = record
    Hydrophobic, Hydrophilic, Neutral: Boolean;
  end;
  TRequirement = record
    Essential, SemiEssential, NonEssential: Boolean;
  end;
  TAminoAcid = record
    AAName, AACode3, AAFormula1, AAFormula2: string;
    AAMolWeight: Real;
    AASize: string;
    AAStructure: TStructure;
    AACharge: TCharge;
    AAPolarity: TPolarity;
    AAPolarity2: TPolarity2;
    AAHydropathy: THydropathy;
    AARequirement: TRequirement;
  end;
  TAminoAcids = array['A'..'Z'] of TAminoAcid;
  {**********}
  { TfAAQuiz }
  {**********}
  TfAAQuiz = class(TForm)
    mMenu: TMainMenu;
    mQuiz: TMenuItem;
    mQuiz10, mQuiz9, mQuiz8, mQuiz7, mQuiz6, mQuiz5: TMenuItem;
    mQuiz4, mQuiz3, mQuiz2, mQuiz1, mQuizExit: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laNQuestion, laQuestion, lacode1, lacode3: TLabel;
    edCode1, edCode3, edFormula, edEval: TEdit;
    rbSelect1, rbSelect2, rbSelect3, rbSelect4: TRadioButton;
    cbSelect1, cbSelect2, cbSelect3, cbSelect4: TCheckBox;
    cobAminoAcids: TComboBox;
    btAction: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuiz1Click(Sender: TObject);
    procedure mQuiz2Click(Sender: TObject);
    procedure mQuiz3Click(Sender: TObject);
    procedure mQuiz4Click(Sender: TObject);
    procedure mQuiz6Click(Sender: TObject);
    procedure mQuiz7Click(Sender: TObject);
    procedure mQuiz8Click(Sender: TObject);
    procedure mQuiz9Click(Sender: TObject);
    procedure mQuiz10Click(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
  private
    iQuiz, iQuestion, iCorrect: Integer;
    sTitle: string;
    aAnswers: array[0..3] of string;
    aDone: array['A'..'Z'] of Boolean;
    aAminoAcids: TAminoAcids;
    rbSelects: array[0..3] of TRadioButton;
    cbSelects: array[0..3] of TCheckbox;
  end;

var
  fAAQuiz: TfAAQuiz;

implementation

{$R *.lfm}

{ Read amino acids data from text file into TAminoAcids array }

procedure ReadAminoAcids(out AminoAcids: TAminoAcids);

var
  Line: string;
  Aa: Char;
  InFile: Text;

begin
  for Aa := 'A' to 'Z' do
    AminoAcids[Aa].AAName := 'undef';                                          // some letters don't code for any amino acid
  Assign(InFile, 'aa.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      // 1-letter code is index of array
      Aa := LeftStr(Line, 1)[1];
      // 3-letter code, name, formulas, size and molecular weight
      AminoAcids[Aa].AACode3 := Copy(Line, 3, 3);
      AminoAcids[Aa].AAName := Trim(Copy(Line, 7, 13));
      AminoAcids[Aa].AAFormula1 := Trim(Copy(Line, 21, 10));
      AminoAcids[Aa].AAFormula2 := Trim(Copy(Line, 32, 32));
      AminoAcids[Aa].AAMolWeight := StrToFloat(Trim(Copy(Line, 65, 5))) / 100;
      AminoAcids[Aa].AASize := Trim(Copy(Line, 71, 10));
      // Side-chain structure
      AminoAcids[Aa].AAStructure.Aliphatic := False; AminoAcids[Aa].AAStructure.Aromatic := False;
      AminoAcids[Aa].AAStructure.Cyclic := False;    AminoAcids[Aa].AAStructure.SulfurContaining := False;
      AminoAcids[Aa].AAStructure.Acidic := False;    AminoAcids[Aa].AAStructure.Basic := False;
      AminoAcids[Aa].AAStructure.Neutral := False;
      case Copy(Line, 82, 1)[1] of
        'L': AminoAcids[Aa].AAStructure.Aliphatic := True;
        'R': AminoAcids[Aa].AAStructure.Aromatic := True;
        'C': AminoAcids[Aa].AAStructure.Cyclic := True;
        'S': AminoAcids[Aa].AAStructure.SulfurContaining := True;
        'A': AminoAcids[Aa].AAStructure.Acidic := True;
        'B': AminoAcids[Aa].AAStructure.Basic := True;
        'N': AminoAcids[Aa].AAStructure.Neutral := True;
      end;
      // Polarity
      AminoAcids[Aa].AAPolarity.Polar := False; AminoAcids[Aa].AAPolarity.NonPolar := False;
      case Copy(Line, 84, 1)[1] of
        'P': AminoAcids[Aa].AAPolarity.Polar := True;
        'N': AminoAcids[Aa].AAPolarity.NonPolar := True;
      end;
      // Charge
      AminoAcids[Aa].AACharge.Positive := False; AminoAcids[Aa].AACharge.Negative := False;
      AminoAcids[Aa].AACharge.Uncharged := False;
      case Copy(Line, 86, 1)[1] of
        '+': AminoAcids[Aa].AACharge.Positive := True;
        '-': AminoAcids[Aa].AACharge.Negative := True;
        else AminoAcids[Aa].AACharge.Uncharged := True;
      end;
      // Charge/Polarity
      AminoAcids[Aa].AAPolarity2.Positive := False;       AminoAcids[Aa].AAPolarity2.Negative := False;
      AminoAcids[Aa].AAPolarity2.PolarUncharged := False; AminoAcids[Aa].AAPolarity2.Nonpolar := False;
      case Copy(Line, 88, 1)[1] of
        '+': AminoAcids[Aa].AAPolarity2.Positive := True;
        '-': AminoAcids[Aa].AAPolarity2.Negative := True;
        'U': AminoAcids[Aa].AAPolarity2.PolarUncharged := True;
        'N': AminoAcids[Aa].AAPolarity2.Nonpolar := True;
      end;
      // Hydropathy
      AminoAcids[Aa].AAHydropathy.Hydrophobic := False; AminoAcids[Aa].AAHydropathy.Hydrophilic := False;
      AminoAcids[Aa].AAHydropathy.Neutral := False;
      case Copy(Line, 90, 1)[1] of
        'O': AminoAcids[Aa].AAHydropathy.Hydrophobic := True;
        'I': AminoAcids[Aa].AAHydropathy.Hydrophilic := True;
        'N': AminoAcids[Aa].AAHydropathy.Neutral := True;
      end;
      // Nutritive requirement
      AminoAcids[Aa].AARequirement.Essential := False; AminoAcids[Aa].AARequirement.SemiEssential := False;
      AminoAcids[Aa].AARequirement.NonEssential := False;
      case Copy(Line, 92, 1)[1] of
        'E': AminoAcids[Aa].AARequirement.Essential := True;
        'S': AminoAcids[Aa].AARequirement.SemiEssential := True;
        'N': AminoAcids[Aa].AARequirement.NonEssential := True;
      end;
    end;
  end;
  Close(InFile);
end;

{ Get amino acid side-chain }

function GetSideChain(AAStructure: TStructure; Structure0: string): string;

// If no structure is specified as second function argument, the side chain of
// is taken from the TStructure argument; otherwise a random structure different
// from the second argument is returned

const
  Sidechains: array[0..6] of string = (
    'aliphatic', 'aromatic', 'cyclic', 'sulfuric', 'acidic', 'basic', 'neutral'
  );

var
  Structure: string;

begin
  if Structure0 = '' then begin
    // Get side-chain from TStructure argument
    if AAStructure.Aliphatic then
      Structure := Sidechains[0]
    else if AAStructure.Aromatic then
      Structure := Sidechains[1]
    else if AAStructure.Cyclic then
      Structure := Sidechains[02]
    else if AAStructure.SulfurContaining then
      Structure := Sidechains[3]
    else if AAStructure.Acidic then
      Structure := Sidechains[4]
    else if AAStructure.Basic then
      Structure := Sidechains[5]
    else
      Structure := Sidechains[6];
  end
  else begin
    // Get random side-chain
    repeat
      Structure := Sidechains[Random(7)];
    until Structure <> Structure0;
  end;
  Result := Structure;
end;

{ Get polarity of actual amino acid }

function GetPolarity(AminoAcid: TAminoAcid): string;

var
  Polarity: string;

begin
  if AminoAcid.AAPolarity.Polar then
    Polarity := 'polar'
  else
    Polarity := 'nonpolar';
  Result := Polarity;
end;

{ Get charge of actual amino acid }

function GetCharge(AminoAcid: TAminoAcid): string;

var
  Charge: string;

begin
  if AminoAcid.AACharge.Positive then
    Charge := 'positive'
  else if AminoAcid.AACharge.Negative then
    Charge := 'negative'
  else
    Charge := 'uncharged';
  Result := Charge;
end;

{ Get polarity/charge of actual amino acid }

function GetPolarityCharge(AminoAcid: TAminoAcid): string;

var
  ChargePolarity: string;

begin
  if AminoAcid.AAPolarity2.Positive then
    ChargePolarity := 'positive'
  else if AminoAcid.AAPolarity2.Negative then
    ChargePolarity := 'negative'
  else if AminoAcid.AAPolarity2.PolarUncharged then
    ChargePolarity := 'uncharged polar'
  else
    ChargePolarity := 'nonpolar';
  Result := ChargePolarity;
end;

{ Get hydropathy of actual amino acid }

function GetHydropathy(AminoAcid: TAminoAcid): string;

var
  Hydropathy: string;

begin
  if AminoAcid.AAHydropathy.Hydrophobic then
    Hydropathy := 'hydrophobic'
  else if AminoAcid.AAHydropathy.Hydrophilic then
    Hydropathy := 'hydrophilic'
  else
    Hydropathy := 'neutral';
  Result := Hydropathy;
end;

{ Get nutritive requirement of actual amino acid }

function GetRequirement(AminoAcid: TAminoAcid): string;

var
  Requirement: string;

begin
  if AminoAcid.AARequirement.Essential then
    Requirement := 'essential'
  else if AminoAcid.AARequirement.NonEssential then
    Requirement := 'non-essential'
  else
    Requirement := 'semi-essential';
  Result := Requirement;
end;

{ Get random polarity }

function GetRandomPolarity: string;

const
  Polarities: array[0..1] of string = (
    'polar', 'unpolar'
  );

begin
  Result := Polarities[Random(2)];
end;

{ Get random charge }

function GetRandomCharge: string;

const
  Charges: array[0..2] of string = (
    'positive', 'negative', 'uncharged'
  );

begin
  Result := Charges[Random(3)];
end;

{ Get random polarity/charge }

function GetRandomPolarityCharge: string;

const
  PolarityCharges: array[0..3] of string = (
    'positive', 'negative', 'uncharged polar', 'nonpolar'
  );

begin
  Result := PolarityCharges[Random(4)];
end;

{ Get random hydropathy }

function GetRandomHydropathy: string;

const
  Hydropathies: array[0..2] of string = (
    'hydrophobic', 'hydrophilic', 'neutral'
  );

begin
  Result := Hydropathies[Random(2)];
end;

{ Get user answer from radio button selected }

function GetAnswer(var Selects: array of TRadiobutton): string;

var
  I: Integer;
  Answer: string;

begin
  for I := 0 to 3 do begin
    if Selects[I].Visible then begin
      if Selects[I].Checked then
        Answer := Selects[I].Caption;
    end;
  end;
  Result := Answer;
end;

{ Get user answers from check boxes selected }

function GetAnswers(var Selects: array of TCheckbox): TArray;

var
  I: Integer;
  Answers: TArray;

begin
  for I := 0 to 3 do begin
    if Selects[I].Checked then
      Answers[I] := 'true'
    else
      Answers[I] := 'false';
  end;
  Result := Answers;
end;

{**********}
{ TfAAQuiz }
{**********}

{ Application start: Initialization }

procedure TfAAQuiz.FormCreate(Sender: TObject);

begin
  // Re-arrange components on the form
  rbSelect1.Top := laCode1.Top; rbSelect2.Top := laCode1.Top; rbSelect3.Top := laCode1.Top; rbSelect4.Top := laCode1.Top;
  cbSelect1.Top := laCode1.Top; cbSelect2.Top := laCode1.Top; cbSelect3.Top := laCode1.Top; cbSelect4.Top := laCode1.Top;
  rbSelects[0] := rbSelect1; rbSelects[1] := rbSelect2; rbSelects[2] := rbSelect3; rbSelects[3] := rbSelect4;
  cbSelects[0] := cbSelect1; cbSelects[1] := cbSelect2; cbSelects[2] := cbSelect3; cbSelects[3] := cbSelect4;
  // Initialize
  sTitle := 'Amino acids quiz';
  ReadAminoAcids(aAminoAcids);                                                 // read amino acids data from text file
  Randomize;
  // Prepare for a type 1 quiz
  mQuiz1.Click;
end;

{ Menu item "Quiz > Amino acid codes (I)": Prepare for a type 1 quiz }

procedure TfAAQuiz.mQuiz1Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Amino acid codes (I).';
  laQuestion.Caption := 'Find amino acid codes.';
  laCode1.Visible := True; laCode3.Visible := True; edCode1.Visible := True; edCode3.Visible := True;
  cobAminoAcids.Visible := False; edFormula.Visible := False;
  rbSelect1.Visible := False; rbSelect2.Visible := False; rbSelect3.Visible := False; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  edCode1.Text := ''; edCode1.Text := ''; edEval.Text := '';
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  iQuiz := 1;
end;

{ Menu item "Quiz > Amino acid codes (II)": Prepare for a type 2 quiz }

procedure TfAAQuiz.mQuiz2Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Amino acid codes (II).';
  laQuestion.Caption := 'Find amino acid name by code.';
  laCode1.Visible := False; laCode3.Visible := False;
  edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := True; edFormula.Visible := False;
  rbSelect1.Visible := False; rbSelect2.Visible := False; rbSelect3.Visible := False; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  edEval.Text := '';
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  iQuiz := 2;
end;

{ Menu item "Quiz > Formulas (I)": Prepare for a type 3 quiz }

procedure TfAAQuiz.mQuiz3Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Formulas (I).';
  laQuestion.Caption := 'Find amino acid name by formula.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := True; edFormula.Visible := False;
  rbSelect1.Visible := False; rbSelect2.Visible := False; rbSelect3.Visible := False; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  edEval.Text := '';
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  iQuiz := 3;
end;

{ Menu item "Quiz > Formulas (II)": Prepare for a type 4 quiz }

procedure TfAAQuiz.mQuiz4Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Formulas (II).';
  laQuestion.Caption := 'Find amino acid formula.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := False; edFormula.Visible := True;
  edFormula.Text := ''; edEval.Text := '';
  rbSelect1.Visible := False; rbSelect2.Visible := False; rbSelect3.Visible := False; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  iQuiz := 4;
end;

{ Menu item "Quiz > Size and molecular weight": Prepare for a type 6 quiz }

procedure TfAAQuiz.mQuiz6Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Size and weight.';
  laQuestion.Caption := 'Find amino acid size or molecular weight.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := False; edFormula.Visible := False;
  rbSelect1.Visible := True; rbSelect2.Visible := True; rbSelect3.Visible := True; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False;
  rbSelect1.Caption := ''; rbSelect2.Caption := ''; rbSelect3.Caption := ''; rbSelect4.Visible := False;
  rbSelect2.Left := rbSelect1.Left + 130; rbSelect3.Left := rbSelect1.Left + 263;   // re-arrange radio buttons position
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  edEval.Text := '';
  iQuiz := 6;
end;

{ Menu item "Quiz > Side-chain structure": Prepare for a type 7 quiz }

procedure TfAAQuiz.mQuiz7Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Side-chain structure.';
  laQuestion.Caption := 'Find structure of amino acid''s side-chain.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := False; edFormula.Visible := False;
  rbSelect1.Visible := True; rbSelect2.Visible := True; rbSelect3.Visible := True; rbSelect4.Visible := True;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False; rbSelect4.Checked := False;
  rbSelect1.Caption := ''; rbSelect2.Caption := ''; rbSelect3.Caption := ''; rbSelect4.Caption := '';
  rbSelect2.Left := rbSelect1.Left + 130; rbSelect3.Left := rbSelect1.Left + 263; rbSelect4.Left := rbSelect1.Left + 382;
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  edEval.Text := '';
  iQuiz := 7;
end;

{ Menu item "Quiz > Properties (I)": Prepare for a type 8 quiz }

procedure TfAAQuiz.mQuiz8Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Properties (I).';
  laQuestion.Caption := 'Find amino acid''s properties.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := False; edFormula.Visible := False;
  rbSelect1.Visible := True; rbSelect2.Visible := True; rbSelect3.Visible := True; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False;
  rbSelect1.Caption := ''; rbSelect2.Caption := ''; rbSelect3.Caption := '';
  rbSelect2.Left := rbSelect1.Left + 130; rbSelect3.Left := rbSelect1.Left + 263;
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  edEval.Text := '';
  iQuiz := 8;
end;

{ Menu item "Quiz > Properties (II)": Prepare for a type 9 quiz }

procedure TfAAQuiz.mQuiz9Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Properties (II).';
  laQuestion.Caption := 'Find amino acids with given property.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := False; edFormula.Visible := False;
  rbSelect1.Visible := False; rbSelect2.Visible := False; rbSelect3.Visible := False; rbSelect4.Visible := False;
  cbSelect1.Visible := True; cbSelect2.Visible := True; cbSelect3.Visible := True; cbSelect4.Visible := True;
  cbSelect1.Checked := False; cbSelect2.Checked := False; cbSelect3.Checked := False; cbSelect4.Checked := False;
  cbSelect1.Caption := ''; cbSelect2.Caption := ''; cbSelect3.Caption := ''; cbSelect4.Caption := '';
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  edEval.Text := '';
  iQuiz := 9;
end;

{ Menu item "Quiz > Nutritive requirement": Prepare for a type 10 quiz }

procedure TfAAQuiz.mQuiz10Click(Sender: TObject);

begin
  stTitle.Caption := sTitle + ': Nutritive requirement.';
  laQuestion.Caption := 'Find amino acid''s nutritive requirement for humans.';
  laCode1.Visible := False; laCode3.Visible := False; edCode1.Visible := False; edCode3.Visible := False;
  cobAminoAcids.Visible := False; edFormula.Visible := False;
  rbSelect1.Visible := True; rbSelect2.Visible := True; rbSelect3.Visible := True; rbSelect4.Visible := False;
  cbSelect1.Visible := False; cbSelect2.Visible := False; cbSelect3.Visible := False; cbSelect4.Visible := False;
  rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False;
  rbSelect1.Caption := ''; rbSelect2.Caption := ''; rbSelect3.Caption := '';
  rbSelect2.Left := rbSelect1.Left + 155; rbSelect3.Left := rbSelect1.Left + 315;
  laNQuestion.Caption := 'Question'; btAction.Caption := 'Start'; btAction.Enabled := True;
  edEval.Text := '';
  iQuiz := 10;
end;

{ Menu item "Quiz > Exit": Exit the application }

procedure TfAAQuiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfAAQuiz.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Biochemistry: Amino acids quiz.' + LineEnding;
  S += 'Quiz concerning amino acid codes, formulas, molecular weight and size, ';
  S += 'side-chain structure, properties and human nutritive requirement.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2024.';
  MessageDlg('About "AAQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer" pushed: Generate quiz question resp. check user answer }

procedure TfAAQuiz.btActionClick(Sender: TObject);

var
  N, R, P0, P1, P2, P3, I, P: Integer;
  Formula, Sidechain0, Sidechain1, Sidechain2, Sidechain3, S1, S2: string;
  Aa, Aa1, Aa2: Char;
  Correct: Boolean;
  UAnswers: TArray;

begin
  if (btAction.Caption = 'Start') or (btAction.Caption = 'Question') then begin
    // Button "Start/Question" pushed: Generate quiz question }
    if btAction.Caption = 'Start' then begin
      // Button "Start/Question" pushed: Quiz initialization
      for Aa := 'A' to 'Z' do
        aDone[aA] := False;
      for I := 0 to 3 do
        aAnswers[I] := '';
      iQuestion := 0; iCorrect := 0;
    end;
    Inc(iQuestion);
    laNQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/20.';
    edEval.Text := '';
    // Get random amino acid (among those not yet used in this quiz)
    repeat
      N := Random(26);
      Aa := Chr(N + Ord('A'));
    until (not aDone[Aa]) and (aAminoAcids[Aa].AAName <> 'undef');
    aDone[Aa] := True;                                                         // mark this amino acid as used
    if iQuiz = 1 then begin
      // Type 1 quiz: Amino acid codes (I)
      laQuestion.Caption := 'What are the codes of amino acid ' + UpperCase(aAminoAcids[Aa].AAName) + '?';
      edCode1.Text := ''; edCode3.Text := ''; edCode1.SetFocus;
      aAnswers[0] := Aa; aAnswers[1] := aAminoAcids[Aa].AACode3;               // user has to enter 2 answers in this quiz
      edCode1.SetFocus;
    end
    else if iQuiz = 2 then begin
      // Type 2 quiz: Amino acid codes (II)
      laQuestion.Caption := 'Which amino acid is coded by the letter ' + Aa + '?';
      aAnswers[0] := aAminoAcids[Aa].AAName;
      cobAminoAcids.ItemIndex := 0;
    end
    else if iQuiz = 3 then begin
      // Type 3 quiz: Formulas (I)
      if Random(2) = 0 then
        Formula := aAminoAcids[Aa].AAFormula1
      else begin
        Formula := aAminoAcids[Aa].AAFormula2;
        // Do not use formulas including "Ph" coding for phenol
        P := Pos('Ph', Formula);
        if P > 0 then
          Formula := aAminoAcids[Aa].AAFormula1;
      end;
      laQuestion.Caption := 'Which amino acid has the formula ' + Formula + '?';
      aAnswers[0] := aAminoAcids[Aa].AAName;
      cobAminoAcids.ItemIndex := 0;
    end
    else if iQuiz = 4 then begin
      // Type 4 quiz: Formulas (II)
      laQuestion.Caption := 'What is the formula of amino acid ' + UpperCase(aAminoAcids[Aa].AAName) + '?';
      edFormula.Text := ''; edFormula.SetFocus;
      aAnswers[0] := aAminoAcids[Aa].AAFormula1;
      edFormula.SetFocus;
    end
    else if iQuiz = 6 then begin
      // Type 6 quiz: Size and molecular weight
      if Random(2) = 0 then begin
        // Molecule size question
        laQuestion.Caption := 'What is the size of ' + UpperCase(aAminoAcids[Aa].AAName) + '?';
        rbSelect1.Caption := '(very) small'; rbSelect2.Caption := 'medium'; rbSelect3.Caption := '(very) large';
        rbSelect2.Left := rbSelect1.Left + 144;
        aAnswers[0] := aAminoAcids[Aa].AASize;
      end
      else begin
        // Molecule weight question
        laQuestion.Caption := 'What is the molecular weight of ' + UpperCase(aAminoAcids[Aa].AAName) + '?';
        P0 := Random(3);                                                       // radio button with correct answer
        rbSelects[P0].Caption := FloatToStr(aAminoAcids[Aa].AAMolWeight);
        // Two further random amino acids (for the two remaining molecular weight proposals)
        repeat
          N := Random(26); Aa1 := Chr(N + Ord('A'));
          N := Random(26); Aa2 := Chr(N + Ord('A'));
        until (aAminoAcids[Aa1].AAName <> 'undef') and (aAminoAcids[Aa2].AAName <> 'undef') and (Aa1 <> Aa) and (Aa2 <> Aa) and (Aa1 <> Aa2);
        // Radio button positions of these two molecular weight values
        repeat
          P1 := Random(3); P2 := Random(3);
        until (P1 <> P0) and (P2 <> P0) and (P1 <> P2);
        // These radio buttons' captions
        rbSelects[P1].Caption := FloatToStr(aAminoAcids[Aa1].AAMolWeight);
        rbSelects[P2].Caption := FloatToStr(aAminoAcids[Aa2].AAMolWeight);
        rbSelect2.Left := rbSelect1.Left + 130;                                // re-arrange radio button on the form
        aAnswers[0] := FloatToStr(aAminoAcids[Aa].AAMolWeight);
      end;
      rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False;
    end
    else if iQuiz = 7 then begin
      // Type 7 quiz: Side-chain structure
      laQuestion.Caption := 'Which side-chain structure class ' + UpperCase(aAminoAcids[Aa].AAName) + ' belongs to?';
      SideChain0 := GetSideChain(aAminoAcids[Aa].AAStructure, '');
      aAnswers[0] := SideChain0;                                               // this is the correct answer
      // Three random side-chain structures for the remaining 3 radio buttons
      repeat
        SideChain1 := GetSideChain(aAminoAcids[Aa].AAStructure, SideChain0);
        SideChain2 := GetSideChain(aAminoAcids[Aa].AAStructure, SideChain0);
        SideChain3 := GetSideChain(aAminoAcids[Aa].AAStructure, SideChain0);
      until (SideChain1 <> SideChain2) and (SideChain1 <> SideChain3) and (SideChain2 <> SideChain3);
      // Random positionning (radio button) for the 4 answer proposals
      P0 := Random(3);                                                         // correct answer radio button position
      rbSelects[P0].Caption := SideChain0;
      repeat
        P1 := Random(4); P2 := Random(4); P3 := Random(4);                     // false answers radio buttons position
      until (P1 <> P0) and (P2 <> P0) and (P3 <> P0) and (P1 <> P2) and (P1 <> P3) and (P2 <> P3);
      // Radio buttons captions
      rbSelects[P1].Caption := Sidechain1; rbSelects[P2].Caption := Sidechain2; rbSelects[P3].Caption := Sidechain3;
      rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False; rbSelect4.Checked := False;
    end
    else if iQuiz = 8 then begin
      // Type 8 quiz: Properties (I)
      if Random(2) = 0 then begin
        // Polarits/charge question
        rbSelect4.Visible := True; rbSelect4.Checked := False; rbSelect4.Left := rbSelect1.Left + 453;
        laQuestion.Caption := 'What is the polarity/charge of ' + UpperCase(aAminoAcids[Aa].AAName) + '?';
        rbSelects[0].Caption := 'nonpolar'; rbSelects[1].Caption := 'positive';
        rbSelects[2].Caption := 'negative'; rbSelects[3].Caption := 'uncharged polar';
        aAnswers[0] := GetPolarityCharge(aAminoAcids[Aa]);
      end
      else begin
        // Hydropathy question
        rbSelect4.Visible := False; rbSelect2.Left := rbSelect1.Left + 160; rbSelect3.Left := rbSelect1.Left + 315;
        laQuestion.Caption := 'What is the hydropathy of ' + UpperCase(aAminoAcids[Aa].AAName) + '?';
        rbSelects[0].Caption := 'hydrophobic'; rbSelects[1].Caption := 'hydrophylic';
        rbSelects[2].Caption := 'neutral';
        aAnswers[0] := GetHydropathy(aAminoAcids[Aa]);
      end;
      rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False;
    end
    else if iQuiz = 9 then begin
      // Type 9 quiz: Properties (II)
      R := Random(4);
      case R of
        0: begin S2 := GetRandomPolarity; laQuestion.Caption := 'Polarity: Which of these amino acids are ' + S2 + '?'; end;
        1: begin S2 := GetRandomCharge; laQuestion.Caption := 'Charge: Which of these amino acids are ' + S2 + '?'; end;
        2: begin S2 := GetRandomPolarityCharge; laQuestion.Caption := 'Polarity/Charge: Which of these amino acids are ' + S2 + '?'; end;
        3: begin S2 := GetRandomHydropathy; laQuestion.Caption := 'Hydropathy: Which of these amino acids are ' + S2 + '?'; end;
      end;
      // Reset amino acids list every 5 questions (5 questions x 4 answer proposals = 20 amino acids done)
      if iQuiz mod 4 = 1 then begin
        for Aa := 'A' to 'Z' do
          aDone[aA] := False;
      end;
      // Get 4 random amino acids (thus: number of correct answers = 0 to 4)
      for I := 0 to 3 do begin
        repeat
          N := Random(26);
          Aa := Chr(N + Ord('A'));
        until (not aDone[Aa]) and (aAminoAcids[Aa].AAName <> 'undef');
        aDone[Aa] := True;
        cbSelects[I].Caption := aAminoAcids[Aa].AAName;
        // Get property value for actual random amino acid
        case R of
          0: S1 := GetPolarity(aAminoAcids[Aa]);
          1: S1 := GetCharge(aAminoAcids[Aa]);
          2: S1 := GetPolarityCharge(aAminoAcids[Aa]);
          3: S1 := GetHydropathy(aAminoAcids[Aa]);
        end;
        // If the amino acid's property value equals the one in the question test, set answer to "true", otherwise to "false"
        // The function GetAnswers does similarly by setting an answer to "true" if the user has selected the corr. check box
        if S1 = S2 then
          aAnswers[I] := 'true'
        else
          aAnswers[I] := 'false';
      end;
      cbSelect1.Checked := False; cbSelect2.Checked := False; cbSelect3.Checked := False; cbSelect4.Checked := False;
    end
    else if iQuiz = 10 then begin
      // Type 10 quiz: Nutritive requirement
      laQuestion.Caption := 'What is the nutritive requirement of ' + UpperCase(aAminoAcids[Aa].AAName) + ' for humans?';
      rbSelects[0].Caption := 'non-essential'; rbSelects[1].Caption := 'semi-essential';
      rbSelects[2].Caption := 'essential';
      rbSelect1.Checked := False; rbSelect2.Checked := False; rbSelect3.Checked := False;
      aAnswers[0] := GetRequirement(aAminoAcids[Aa]);
    end;
    btAction.Caption := 'Answer';                                              // next button push will be to check user answer
  end
  else begin
    // Button "Answer" pushed: Check user answer
    case iQuiz of
      // Get user answer(s) from form component(s), the component(s) depending on the quiz type
      1:           begin UAnswers[0] := edCode1.Text; UAnswers[1] := edCode3.Text; end;  // 2 answers (edit fields)
      2, 3:        UAnswers[0] := cobAminoAcids.Text;                          // 1 answer (combo box)
      4:           UAnswers[0] := edFormula.Text;                              // 1 answer (edit field)
      6, 7, 8, 10: UAnswers[0] := GetAnswer(rbSelects);                        // 1 answer (radio button checked)
                9: UAnswers := GetAnswers(cbSelects);                          // 4 answers (the 4 check boxes, selected or not)
    end;
    Correct := True;
    if iQuiz = 9 then begin
      // Special case: type 9 quiz - 4 answers to check
      for I := 0 to 3 do begin
         if UAnswers[I] <> aAnswers[I] then
           Correct := False;
      end;
    end
    else begin
      // Normal case: other quiz types - 1 or 2 answers to check
      for I := 0 to 1 do begin
        if (I = 0) or ((I = 1) and (aAnswers[1] <> '')) then begin
          S1 := aAnswers[I]; S2 := UAnswers[I];
          if iQuiz = 6 then begin
            // This is necessary because small/large and very small/large are grouped under the same radio button
            S1 := StringReplace(S1, 'very ', '', []);
            S2 := StringReplace(S2, '(very) ', '', []);
          end;
          // If for one of the users answers, it differs from the calculated answer, then the answer to the quiz question is false
          if S1 <> S2 then
            Correct := False;
        end;
      end;
    end;
    if Correct then begin
      // Correct user answer
      edEval.Text := 'This answer is correct!';
      Inc(iCorrect);                                                           // counter of correct answers
    end
    else begin
      // Wrong user answer
      edEval.Text := 'This answer is false!';
      if iQuiz = 9 then begin
        // Type 9 quiz: Display the number of the correct answer(s) check box(es)
        edEval.Text := edEval.Text + ' Correct selection(s): ';
        for I := 0 to 3 do begin
          if aAnswers[I] = 'true' then
            edEval.Text := edEval.Text + IntToStr(I + 1) + ', ';
        end;
        if RightStr(edEval.Text, 2) = ', ' then
          edEval.Text := LeftStr(edEval.Text, Length(edEval.Text) - 2)         // remove comma at end of line
        else
          edEval.Text := edEval.Text + 'none';                                 // no comma at end of line, means that there aren't any correct answers
      end
      else begin
        // Other quiz types: Display the correct answer(s)
        edEval.Text := edEval.Text + ' Correct is: ' + aAnswers[0];
        if aAnswers[1] <> '' then
          edEval.Text := edEval.Text + ' ; ' + aAnswers[1];
      end;
    end;
    btAction.Caption := 'Question';                                            // next button push will be to generate a new question
    if iQuestion = 20 then begin
      // All 20 questions done: Display number of correct answers and success percentage and terminate the quiz (disable the button)
      S1 := 'All 20 questions of this quiz are done. You answered ' + IntToStr(iCorrect);
      S1 += ' questions correctly. That is a score of ' + IntToStr(Round(100 * iCorrect/20)) + '%.';
      MessageDlg('End of quiz', S1, mtInformation, [mbOK], 0);
      btAction.Enabled := False;
    end;
  end;
end;

end.

