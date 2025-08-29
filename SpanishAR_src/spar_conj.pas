{************************************************************}
{* Display conjugation table unit for SpanishAR application *}
{************************************************************}

unit spar_conj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles, LazUTF8, conjar;

type
  TEditInd  = array[1..5, 1..6] of TEdit;
  TEditSubj = array[1..2, 1..6] of TEdit;
  TEditImp  = array[1..6] of TEdit;
  {********}
  { TfConj }
  {********}
  TfConj = class(TForm)
    stTitle: TStaticText;
    Label2, Label5, Label6, Label3: TLabel;
    laInfinitve, laGerund, laPastParticiple, laImperative: TLabel;
    laIndPres, laIndImpf, laIndPret, laIndFut, laIndCond: TLabel;
    laSubjPres, laSubjImpf, laImpPres: TLabel;
    edInfinitive, edGerund, edParticiple: TEdit;
    edIndPres11, edIndPres12, edIndPres13, edIndPres21, edIndPres22, edIndPres23: TEdit;
    edIndImpf11, edIndImpf12, edIndImpf13, edIndImpf21, edIndImpf22, edIndImpf23: TEdit;
    edIndPret11, edIndPret12, edIndPret13, edIndPret21, edIndPret22, edIndPret23: TEdit;
    edIndFut11, edIndFut12, edIndFut13, edIndFut21, edIndFut22, edIndFut23: TEdit;
    edIndCond11, edIndCond12, edIndCond13, edIndCond21, edIndCond22, edIndCond23: TEdit;
    edSubjPres11, edSubjPres12, edSubjPres13, edSubjPres21, edSubjPres22, edSubjPres23: TEdit;
    edSubjImpf11, edSubjImpf12, edSubjImpf13, edSubjImpf21, edSubjImpf22, edSubjImpf23: TEdit;
    edImperative11, edImperative12, edImperative13: TEdit;
    edImperative21, edImperative22, edImperative23: TEdit;
    btAffirmative, btNegative: TButton;
    btMasculine, btFeminine: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btAffirmativeClick(Sender: TObject);
    procedure btFeminineClick(Sender: TObject);
    procedure btMasculineClick(Sender: TObject);
    procedure btNegativeClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
    sForm, sGender: string;
    bImpPron, bSepSuffix, bSepRoot, bColorIrreg, bColorSame: Boolean;
    Verb, VerbType: TVerb;
    VerbFile: TINIFile;
  end;

var
  EditInd: TEditInd;
  EditSubj: TEditSubj;
  EditImp: TEditImp;
  fConj: TfConj;

implementation

{$R *.lfm}

{ Adapt separators between verb parts for display as selected on the main form }

procedure AdaptSeparators(var VConj: string; SepSuffix, SepStem: Boolean);

// This sub also removes the irregularity tags

begin
  // Not existing conjugated verb form
  if VConj = 'n/a' then
    VConj := '-----'
  // Normal case (conjugated verb form exists)
  else begin
    // The conjugated verb as returned by the conjugation routines contains 1 (normal case)
    // or 2 (imperfect, future...) separators. Depending on the separators display options,
    // these are removed or shown
    if SepSuffix and SepStem then
      VConj := StringReplace(VConj, '--', '-', [])
    else if SepSuffix then begin
      VConj := StringReplace(VConj, '--', '~~', []);
      VConj := StringReplace(VConj, '-', '', []);
      VConj := StringReplace(VConj, '~~', '-', []);
    end
    else
      VConj := StringReplace(VConj, '-', '', [rfReplaceAll]);
  end;
  // Remove irregularity tags
  VConj := StringReplace(VConj, '$', '', []);
  VConj := StringReplace(VConj, '#', '', []);
  VConj := StringReplace(VConj, '*', '', []);
  VConj := StringReplace(VConj, '+', '', []);
end;

{ Color display of irregularities }

function FontColor(Verb: TVerb; VerbConj: string; ColorIrreg, ColorSame: Boolean): TColor;

const
  clDarkOrange = $007FFF;
  clDeepPink   = $9314FF;

var
  P: Integer;
  Colour: TColor;

// Color used depends on "irregularity tag" within the conjugated verb form string

begin
  Colour := clDefault;
  if VerbConj <> 'n/a' then begin
    if ColorIrreg then begin
      if UpperCase(Verb.Conjugation) = 'S' then begin
        // Semi-irregular verbs
        P := UTF8Pos('+', VerbConj);
        if P > 0 then
          Colour := clBlue;
      end
      else if Verb.Conjugation = 'I' then begin
        // Irregular verbs
        P := UTF8Pos('$', VerbConj);
        if P > 0 then
          Colour := clRed
        else begin
          P := UTF8Pos('#', VerbConj);
          if P > 0 then
            Colour := clDeepPink
          else begin
            P := UTF8Pos('*', VerbConj);
            if P > 0 then
              Colour := clDarkOrange
            else begin
              P := UTF8Pos('+', VerbConj);
              if P > 0 then
                Colour := clBlue;
            end;
          end;
        end;
      end;
    end;
  end;
  // Use always the same color (if this option has been selected)
  if (Colour <> clDefault) and ColorSame then
    Colour := clRed;
  Result := Colour;
end;

{ Conjugation of given verb using selectd form and gender and fill in of the conjugation display form fields }

procedure Conjugate(Verb, VerbType: TVerb; Form, Gender: string;
  ImpPron, SepSuffix, SepRoot, ColorIrreg, ColorSame: Boolean; var VerbFile: TINIFile);

var
  M, T, N, P, PE: Integer;
  VerbName, VerbConjugated, Mood, Tense, Number, Person, VerbConj, S: string;

begin
  VerbName := Verb.VerbName;
  // Explicit title of what exactly represents the conjugation shown
  S := 'Conjugation of the verb "' + VerbName + '"';
  if Form <> 'affirmative' then begin
    S += ' (' + Form + ' form';
    if Gender = 'masculine' then
      S += ')';
  end;
  if Gender = 'feminine' then begin
    if Form = 'affirmative' then
      S += ' (feminine)'
    else
      S += '; feminine)';
  end;
  fConj.stTitle.Caption := S + '.';
  // Determine the conjugated verb form for all moods, tenses, numbers and persons
  // (with the form and gender as actually selected). Adapt separators and color
  // for display as selected and write the conjugated verb form into the edit fields
  for M := 1 to NMoods do begin
    Mood := Moods[M];
    for T := 1 to NTenses[M] do begin
      Tense := Tenses[M, T];
      // "Infinitive" and "Participle"
      if (Mood = 'infinitive') or (Mood = 'participle') then begin
        VerbConj := Conjugation(Verb, VerbType, Mood, Tense, Form, '-', '-', Gender, ImpPron, VerbFile);
        VerbConjugated := VerbConj;
        AdaptSeparators(VerbConjugated, SepSuffix, SepRoot);
        if Mood = 'infinitive' then begin
          fConj.edInfinitive.Text := VerbConjugated; fConj.edInfinitive.Font.Color := FontColor(Verb, VerbConj, ColorIrreg, ColorSame);
        end
        else begin
          if Tense = 'gerund' then begin
            fConj.edGerund.Text := VerbConjugated; fConj.edGerund.Font.Color := FontColor(Verb, VerbConj, ColorIrreg, ColorSame);
          end
          else begin
            fConj.edParticiple.Text := VerbConjugated; fConj.edParticiple.Font.Color := FontColor(Verb, VerbConj, ColorIrreg, ColorSame);
          end;
        end;
      end
      // Imperative
      else if Mood = 'imperative' then begin
        for N := 1 to NNumbers do begin
          Number := Numbers[N];
          for P := 1 to NPersons do begin
            Person := Persons[P];
            VerbConj := Conjugation(Verb, VerbType, Mood, Tense, Form, Number, Person, Gender, ImpPron, VerbFile);
            VerbConjugated := VerbConj;
            AdaptSeparators(VerbConjugated, SepSuffix, SepRoot);
            PE := (N - 1) * 3 + P;
            if PE <> 1 then begin
              // 1st pers sg has no imperative
              EditImp[PE].Text := VerbConjugated; EditImp[PE].Font.Color := FontColor(Verb, VerbConj, ColorIrreg, ColorSame);
            end;
          end;
        end;
      end
      // Indicative and subjunctive
      else begin
        for N := 1 to NNumbers do begin
          Number := Numbers[N];
          for P := 1 to NPersons do begin
            Person := Persons[P];
            VerbConj := Conjugation(Verb, VerbType, Mood, Tense, Form, Number, Person, Gender, ImpPron, VerbFile);
            VerbConjugated := VerbConj;
            AdaptSeparators(VerbConjugated, SepSuffix, SepRoot);
            PE := (N - 1) * 3 + P;
            if Mood = 'indicative' then begin
              EditInd[T, PE].Text := VerbConjugated; EditInd[T, PE].Font.Color := FontColor(Verb, VerbConj, ColorIrreg, ColorSame);
            end
            else begin
              EditSubj[T, PE].Text := VerbConjugated; EditSubj[T, PE].Font.Color := FontColor(Verb, VerbConj, ColorIrreg, ColorSame);
            end;
          end;
        end;
      end;
    end;
  end;
end;

{**********}
{* TfConj *}
{**********}

{ Application start: Create array with indicative, subjunctive and imperative edit fields }

procedure TfConj.FormCreate(Sender: TObject);

begin
  EditInd[1, 1]  := edIndPres11;  EditInd[1, 2]  := edIndPres12;  EditInd[1, 3]  := edIndPres13;
  EditInd[1, 4]  := edIndPres21;  EditInd[1, 5]  := edIndPres22;  EditInd[1, 6]  := edIndPres23;
  EditInd[2, 1]  := edIndImpf11;  EditInd[2, 2]  := edIndImpf12;  EditInd[2, 3]  := edIndImpf13;
  EditInd[2, 4]  := edIndImpf21;  EditInd[2, 5]  := edIndImpf22;  EditInd[2, 6]  := edIndImpf23;
  EditInd[3, 1]  := edIndPret11;  EditInd[3, 2]  := edIndPret12;  EditInd[3, 3]  := edIndPret13;
  EditInd[3, 4]  := edIndPret21;  EditInd[3, 5]  := edIndPret22;  EditInd[3, 6]  := edIndPret23;
  EditInd[4, 1]  := edIndFut11;   EditInd[4, 2]  := edIndFut12;   EditInd[4, 3]  := edIndFut13;
  EditInd[4, 4]  := edIndFut21;   EditInd[4, 5]  := edIndFut22;   EditInd[4, 6]  := edIndFut23;
  EditInd[5, 1]  := edIndCond11;  EditInd[5, 2]  := edIndCond12;  EditInd[5, 3]  := edIndCond13;
  EditInd[5, 4]  := edIndCond21;  EditInd[5, 5]  := edIndCond22;  EditInd[5, 6]  := edIndCond23;
  EditSubj[1, 1] := edSubjPres11; EditSubj[1, 2] := edSubjPres12; EditSubj[1, 3] := edSubjPres13;
  EditSubj[1, 4] := edSubjPres21; EditSubj[1, 5] := edSubjPres22; EditSubj[1, 6] := edSubjPres23;
  EditSubj[2, 1] := edSubjImpf11; EditSubj[2, 2] := edSubjImpf12; EditSubj[2, 3] := edSubjImpf13;
  EditSubj[2, 4] := edSubjImpf21; EditSubj[2, 5] := edSubjImpf22; EditSubj[2, 6] := edSubjImpf23;
  EditImp[1] := edImperative11;  EditImp[2] := edImperative12; EditImp[3] := edImperative13;
  EditImp[4] := edImperative21;  EditImp[5] := edImperative22; EditImp[6] := edImperative23;
end;

{ Form activation: Do conjugation of verb selected by user }

procedure TfConj.FormActivate(Sender: TObject);

begin
  Conjugate(Verb, VerbType, sForm, sGender, bImpPron, bSepSuffix, bSepRoot, bColorIrreg, bColorSame, VerbFile);
end;

{ Button "Affirmative": Conjugate the verb with affirmative form }

procedure TfConj.btAffirmativeClick(Sender: TObject);

begin
  btAffirmative.Font.Style := [fsBold]; btNegative.Font.Style := []; sForm := 'affirmative';
  Conjugate(Verb, VerbType, sForm, sGender, bImpPron, bSepSuffix, bSepRoot, bColorIrreg, bColorSame, VerbFile);
end;

{ Button "Negative": Conjugate the verb with negative form }

procedure TfConj.btNegativeClick(Sender: TObject);

begin
  btAffirmative.Font.Style := []; btNegative.Font.Style := [fsBold]; sForm := 'negative';
  Conjugate(Verb, VerbType, sForm, sGender, bImpPron, bSepSuffix, bSepRoot, bColorIrreg, bColorSame, VerbFile);
end;

{ Button "Masculine": Conjugate the verb with masculine gender }

procedure TfConj.btMasculineClick(Sender: TObject);

begin
  btMasculine.Font.Style := [fsBold]; btFeminine.Font.Style := []; sGender := 'masculine';
  Conjugate(Verb, VerbType, sForm, sGender, bImpPron, bSepSuffix, bSepRoot, bColorIrreg, bColorSame, VerbFile);
end;

{ Button "Feminine": Conjugate the verb with feminine gender }

procedure TfConj.btFeminineClick(Sender: TObject);

begin
  btMasculine.Font.Style := []; btFeminine.Font.Style := [fsBold]; sGender := 'feminine';
  Conjugate(Verb, VerbType, sForm, sGender, bImpPron, bSepSuffix, bSepRoot, bColorIrreg, bColorSame, VerbFile);
end;

{ Button "Close": Close the conjugation table window }

procedure TfConj.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

