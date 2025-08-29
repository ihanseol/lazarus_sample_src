{****************************************}
{* Main unit for the Latin1 application *}
{****************************************}

unit latin1_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, latin1_nouns, latin1_adjs, latin1_help, latin_declensions;

type
  TNouns = array of record
    Nominative, Genitive: string;
    Genus: Char;
    FrenchNoun: string;
    FrenchGenus: Char;
    Person: Boolean;
    NAdjectives: Byte;
    Adjectives: array[1..12] of Byte;
  end;
  TAdjs = array of record
    Masculine, Feminine, Neuter, FrenchMasculine, FrenchFeminine: string;
    AdjPos: Char;
  end;
  { TfLatin1 }
  TfLatin1 = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mDeclension, mDeclensionN1, mDeclensionN2, mDeclensionN3, mDeclensionN4: TMenuItem;
    mDeclensionN5, MenuItem7, mDeclensionA1, mDeclensionA2: TMenuItem;
    mDeclensionN1Rosa, mDeclensionN2Dominus, mDeclensionN2Ager, mDeclensionN2Puer, mDeclensionN2Templum: TMenuItem;
    mDeclensionN3Civis, mDeclensionN3Mare, mDeclensionN3Consul, mDeclensionN3Corpus, mDeclensionN3Pater: TMenuItem;
    mDeclensionN3Urbs, mDeclensionN3Animal, mDeclensionN3Febris, mDeclensionN3Vis: TMenuItem;
    mDeclensionN4Manus, mDeclensionN4Cornu, mDeclensionN4Domus, mDeclensionN5Res: TMenuItem;
    mDeclensionA1Bonus, mDeclensionA1Miser,  mDeclensionA1Pulcher: TMenuItem;
    mDeclensionA2Acer, mDeclensionA2Fortis, mDeclensionA2Prudens, mDeclensionA2Audax, mDeclensionA2Vetus: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsNominative, mOptionsPerson, mOptionsFrench: TMenuItem;
    mHelp, mHelpGrammar, mHelpProgram, mHelpAbout: TMenuItem;
    rbNouns, rbAdjs, rbAll: TRadioButton;
    cbNouns1, cbNouns2, cbNouns3, cbNouns4, cbNouns5: TCheckBox;
    cbAdjs1, cbAdjs2: TCheckBox;
    laQuestion: TLabel;
    edQuestion, edAnswer, edResult: TEdit;
    Label1, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    edRQuestion: TEdit;
    edRCorrect: TEdit;
    edRFalse: TEdit;
    edRSuccess: TEdit;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mDeclensionA1BonusClick(Sender: TObject);
    procedure mDeclensionA1MiserClick(Sender: TObject);
    procedure mDeclensionA1PulcherClick(Sender: TObject);
    procedure mDeclensionA2AcerClick(Sender: TObject);
    procedure mDeclensionA2AudaxClick(Sender: TObject);
    procedure mDeclensionA2FortisClick(Sender: TObject);
    procedure mDeclensionA2PrudensClick(Sender: TObject);
    procedure mDeclensionA2VetusClick(Sender: TObject);
    procedure mDeclensionN1RosaClick(Sender: TObject);
    procedure mDeclensionN2AgerClick(Sender: TObject);
    procedure mDeclensionN2DominusClick(Sender: TObject);
    procedure mDeclensionN2PuerClick(Sender: TObject);
    procedure mDeclensionN2TemplumClick(Sender: TObject);
    procedure mDeclensionN3AnimalClick(Sender: TObject);
    procedure mDeclensionN3CivisClick(Sender: TObject);
    procedure mDeclensionN3ConsulClick(Sender: TObject);
    procedure mDeclensionN3CorpusClick(Sender: TObject);
    procedure mDeclensionN3FebrisClick(Sender: TObject);
    procedure mDeclensionN3MareClick(Sender: TObject);
    procedure mDeclensionN3PaterClick(Sender: TObject);
    procedure mDeclensionN3UrbsClick(Sender: TObject);
    procedure mDeclensionN3VisClick(Sender: TObject);
    procedure mDeclensionN4CornuClick(Sender: TObject);
    procedure mDeclensionN4DomusClick(Sender: TObject);
    procedure mDeclensionN4ManusClick(Sender: TObject);
    procedure mDeclensionN5ResClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsNominativeClick(Sender: TObject);
    procedure mOptionsPersonClick(Sender: TObject);
    procedure mOptionsFrenchClick(Sender: TObject);
    procedure mHelpGrammarClick(Sender: TObject);
    procedure mHelpProgramClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure rbAdjsChange(Sender: TObject);
    procedure rbAllChange(Sender: TObject);
    procedure rbNounsChange(Sender: TObject);
  private
    iQuestions0, iQuestions, iQuestion, iCorrect, iFalse: Integer;
    sTest, sNewTest, sAnswer: string;
    bPerson: Boolean;
    aAllNouns, aNouns: TNouns;
    aAllAdjs, aAdjs: TAdjs;
  end;

var
  fLatin1: TfLatin1;

implementation

{$R *.lfm}

{ Read Latin nouns from text file }

procedure ReadNouns(var Nouns: TNouns);

var
  N, P: Integer;
  Line, LineLatin, LineFrench: string;
  Pers: Char;
  InFile: Text;

{ Line format: char 1 = P or -, indicating if noun is or not a person; chars 3-4: not used; chars 5-endofline: }
{ nominative "comma" genitive "comma" genus ("m" or "f") "space colon space" French name "comma" French genus }

begin
  N := 0;
  Assign(InFile, 'latinsubst.txt');
  Reset(InFile);
  while not EoF(Infile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Pers := LeftStr(Line, 1)[1];
      Line := RightStr(Line, Length(Line) - 4);
      Inc(N); SetLength(Nouns, N);
      P := Pos(' : ', Line);
      LineLatin := LeftStr(Line, P - 1);
      LineFrench := RightStr(Line, Length(Line) - (P + 2));
      // Fill data read into TNouns variable
      with Nouns[N - 1] do begin
        P := Pos(', ', LineLatin);
        Nominative := LeftStr(LineLatin, P - 1);
        Genitive := Copy(LineLatin, P + 2, Length(LineLatin) - (P + 2) - 2);
        Genus := RightStr(LineLatin, 1)[1];
        FrenchNoun := LeftStr(LineFrench, Length(LineFrench) - 3);
        FrenchGenus := RightStr(LineFrench, 1)[1];
        if UpperCase(Pers) = 'P' then
          Person := True
        else
          Person := False;
      end;
    end;
  end;
  Close(Infile);
end;

{ Read Latin adjectives from text file }

procedure ReadAdjs(var Adjs: TAdjs);

{ Line format: chars 1 - 3: adjective count, will be used as index for nouns-adjectves array, to avoid surprises, must be identical to }
{ number of adjectives read; chars 5-endofline: }
{ masculine "comma" feminine "comma" neuter "space colon space" French masculine "comma" French feminine "comma" French adjective position ("+" or "-")}

var
  N, IX, P: Integer;
  Line, LineLatin, LineFrench: string;
  InFile: Text;
  Err: Boolean;

begin
  N := 0;
  Assign(InFile, 'latinadj.txt');
  Reset(InFile);
  Err := False;
  while not EoF(Infile) and not Err do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      IX := StrToInt(LeftStr(Line, 3));
      Line := RightStr(Line, Length(Line) - 4);
      Inc(N);
      if N <> IX then begin
        MessageDlg('Erreur programme', 'Impossible de correctement lire le fichier latinadj.txt!', mtError, [mbOK], 0);
        Err := True;
        Application.Terminate;
      end
      else begin
        SetLength(Adjs, N);
        P := Pos(' : ', Line);
        LineLatin := LeftStr(Line, P - 1);
        LineFrench := RightStr(Line, Length(Line) - (P + 2));
        // Fill data read into TAdjs variable
        with Adjs[N - 1] do begin
          P := Pos(', ', LineLatin);
          Masculine := LeftStr(LineLatin, P - 1);
          LineLatin := Copy(LineLatin, P + 2, Length(LineLatin) - (P + 1));
          P := Pos(', ', LineLatin);
          Feminine := LeftStr(LineLatin, P - 1);
          Neuter := RightStr(LineLatin, Length(LineLatin) - (P + 1));
          P := Pos(', ', LineFrench);
          FrenchMasculine := LeftStr(LineFrench, P - 1);
          LineFrench := Copy(LineFrench, P + 2, Length(LineFrench) - (P + 1));
          P := Pos(', ', LineFrench);
          FrenchFeminine := LeftStr(LineFrench, P - 1);
          AdjPos := RightStr(LineFrench, 1)[1];
        end;
      end;
    end;
  end;
  Close(Infile);
end;

{ Read Latin noun-adjectives from text file }

procedure ReadNounAdjs(var Nouns: TNouns);

{ The file contains, for each noun, a list of the adjectives that may be used with it when generating the exrcises; line format: }
{ noun (same and same order as in latinsubst.txt file) "space colon space", chars 15-endofline: adjective indexes (3 digits, followed }
{ by 2 spaces), indexes corresponding to those used as counters in the latinsubst.txt file }

var
  N, P: Integer;
  Line, Noun: string;
  Err: Boolean;
  InFile: Text;

begin
  N := 0;
  Assign(InFile, 'latinsubstadj.txt');
  Reset(InFile);
  Err := False;
  while not EoF(Infile) and not Err do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      P := Pos(' : ', Line);
      Noun := LeftStr(Line, P - 1);
      if Nouns[N].Nominative <> Noun then begin
        MessageDlg('Erreur programme', 'Impossible de correctement lire le fichier latinsubstadj.txt!', mtError, [mbOK], 0);
        Err := True;
        Application.Terminate;
      end
      else begin
        Line := RightStr(Line, Length(Line) - 14);
        P := 1;
        Nouns[N].NAdjectives := 0;
        // Continue reading adjective indexes until end of line is reached; fill them into the TNouns variable (together with their count for actual noun)
        while P < Length(Line) do begin
          if Nouns[N].NAdjectives < 12 then begin
            Inc(Nouns[N].NAdjectives);
            Nouns[N].Adjectives[Nouns[N].NAdjectives] := StrToInt(Copy(Line, P, 3)) - 1;
          end;
          P += 5;
        end;
        Inc(N);
      end;
    end;
  end;
  Close(Infile);
end;

{ Clear form fields and set exercise result counters to zero }

procedure ResetCounts(var Q, C, F: Integer);

begin

  Q := 0; C := 0; F := 0;
  flatin1.laQuestion.Caption := 'Question'; fLatin1.edQuestion.Clear; fLatin1.edAnswer.Clear; fLatin1.edResult.Clear;
  fLatin1.edRQuestion.Clear; fLatin1.edRCorrect.Clear; fLatin1.edRFalse.Clear;
  fLatin1.edRSuccess.Clear; fLatin1.edRSuccess.Color := clDefault;
end;

{ Preparations for a new nouns (or nouns with adjective) exercise }

procedure NewNounExercise(AllNouns: TNouns; var Nouns: TNouns; var Q, C, F: Integer);

{ From the TNouns variable, containing all nouns, the procedure creates a new one, containing only thosse nouns belonging to the declensions selected }

var
  I, J: Integer;
  Gen1, Gen2, Nom: string;
  OK: Boolean;

begin
  J := 0;
  for I := 0 to Length(AllNouns) - 1 do begin
    Gen1 := RightStr(AllNouns[I].Genitive, 1); Gen2 := RightStr(AllNouns[I].Genitive, 2); Nom := RightStr(AllNouns[I].Nominative, 2);
    OK := False;
    // Declension determined by suffix of genetive
    if fLatin1.cbNouns1.Checked and (Gen2 = 'ae') then
      OK := True
    else if fLatin1.cbNouns2.Checked and (Gen1 = 'i') and (Nom <> 'es') then
      OK := True
    else if fLatin1.cbNouns3.Checked and ((AllNouns[I].Nominative = 'vis') or (Gen2 = 'is')) then
      OK := True
    else if fLatin1.cbNouns4.Checked and (Gen2 = 'us') then
      OK := True
    else if fLatin1.cbNouns5.Checked and (Gen2 = 'ei') and (Nom = 'es') then
      OK := True;
    if OK then begin
      Inc(J); SetLength(Nouns, J);
      Nouns[J - 1] := AllNouns[I];
    end;
  end;
  ResetCounts(Q, C, F);
  fLatin1.btQuestion.Caption := 'Question';
  fLatin1.btQuestion.Enabled := True;
end;

{ Preparations for a new adjectives-only exercise }

procedure NewAdjExercise(AllAdjs: TAdjs; var Adjs: TAdjs; var Q, C, F: Integer);

{ From the TAdjs variable, containing all adjectives, the procedure creates a new one, containing only thosse adjectives belonging to the classes selected }

var
  I, J: Integer;
  Feminine, Neuter: string;
  OK: Boolean;

begin
  J := 0;
  for I := 0 to Length(AllAdjs) - 1 do begin
    Feminine := RightStr(AllAdjs[I].Feminine, 1); Neuter := RightStr(AllAdjs[I].Neuter, 2);
    OK := False;
    // Class is determined by considering that feminine in -a and neuter in -um = 1st class adjective
    if fLatin1.cbAdjs1.Checked and (Feminine = 'a')  and (Neuter = 'um') then
      OK := True
    else if fLatin1.cbAdjs2.Checked and not ((Feminine = 'a')  and (Neuter = 'um')) then
      OK := True;
    if OK then begin
      Inc(J); SetLength(Adjs, J);
      Adjs[J - 1] := AllAdjs[I];
    end;
  end;
  ResetCounts(Q, C, F);
  fLatin1.btQuestion.Caption := 'Question';
  fLatin1.btQuestion.Enabled := True;
end;

{ Display complete declension (all cases) for model noun (display done using a TStringGrid object) }

procedure DisplayDeclensionNouns(Nominative, Genitive, Genus: string);

var
  I, J: Integer;
  Number, Casus, S: string;

begin
  if (Nominative = 'rosa') then
    S := '1ère'
  else if (Nominative = 'dominus') or (Nominative = 'ager') or (Nominative = 'puer') or (Nominative = 'templum') then
    S := '2e'
  else if (Nominative = 'manus') or (Nominative = 'cornu') or (Nominative = 'domus') then
    S := '4e'
  else if Nominative = 'res' then
    S := '5e'
  else
    S := '3e';
  S += ' déclinaison: ' + Nominative + ', ' + Genitive;
  fNouns.stTitle.Caption := S;
  for I := 1 to 2 do begin
    Number := AllNumbers[I];
    for J := 1 to 6 do begin
      Casus := AllCasus[J];
      fNouns.sgDecelension.Cells[I, J] := DeclensionNouns(Nominative, Genitive, Genus, Casus, Number);
    end;
  end;
  fNouns.ShowModal;
end;

{ Display complete declension (all cases and all 3 genus) for modal adjectivee (display done using a TStringGrid object) }

procedure DisplayDeclensionAdjs(Masculine, Feminine, Neuter: string);

var
  I, J, K: Integer;
  Number, Casus, Genus, S: string;

begin
  if (RightStr(Feminine, 1) = 'a') and (RightStr(Neuter, 2) = 'um') then
    S := '1ère'
  else
    S := '2e';
  S += ' classe: ' + Masculine + ', ';
  if Feminine <> '3id' then
    S += Feminine + ', ';
  S += Neuter;
  fAdjs.stTitle.Caption := S;
  for I := 1 to 3 do begin
    Genus := AllGenus[I];
    for J := 1 to 2 do begin
      Number := AllNumbers[J];
      for K := 1 to 6 do begin
        Casus := AllCasus[K];
        fAdjs.sgDecelension.Cells[I + 1, (J - 1) * 6 + K] := DeclensionAdjs(Masculine, Feminine, Neuter, Genus, Casus, Number, True);
        if ((Genus = 'm') or (Genus = 'f')) and (Number = 'sg') and (Casus = 'abl') and (RightStr(fAdjs.sgDecelension.Cells[I + 1, (J - 1) * 6 + K], 3) = 'nte') then begin
          S := LeftStr(fAdjs.sgDecelension.Cells[I + 1, (J - 1) * 6 + K], Length(fAdjs.sgDecelension.Cells[I + 1, (J - 1) * 6 + K]) - 1) + 'i';
          S := fAdjs.sgDecelension.Cells[I + 1, (J - 1) * 6 + K] + '/' + S;
          fAdjs.sgDecelension.Cells[I + 1, (J - 1) * 6 + K] := S;
        end;
      end;
    end;
  end;
  fAdjs.ShowModal;
end;

{**********}
{ TfLatin1 }
{**********}

{ Application start: Initialisation }

procedure TfLatin1.FormCreate(Sender: TObject);

begin
  SetLength(aAllNouns, 0); SetLength(aAllAdjs, 0);
  SetLength(aAllNouns, 0); SetLength(aAllAdjs, 0);
  SetLength(aNouns, 0); SetLength(aAdjs, 0);
  ReadNouns(aAllNouns);                                                                  // Read Latin nouns
  ReadAdjs(aAllAdjs);                                                                    // Read Latin adjectives
  ReadNounAdjs(aAllNouns);                                                               // Read noun-adjectives data
  sNewTest := 'nouns'; sTest := 'nouns';
  bPerson := True;
  iQuestions0 := 20;
  NewNounExercise(aAllNouns, aNouns, iQuestion, iCorrect, iFalse);                       // Prepare for new nouns-ony exercise
  Randomize;
end;

{ Menu item "Exercice > Nouveau" : Prepare for new exercise }

procedure TfLatin1.mExerciseNewClick(Sender: TObject);

begin
  iQuestions := iQuestions0;
  if (sNewTest = 'nouns') or (sNewTest = 'all') then begin
    // Create list with nouns to be used with actual exercise
    if not (cbNouns1.Checked or cbNouns2.Checked or cbNouns3.Checked or cbNouns4.Checked or cbNouns5.Checked) then begin
      MessageDlg('Sélection invalides', 'Il faut sélectionner au moins une déclinaison de substantifs!', mtError, [mbOK], 0);
      btQuestion.Enabled := False;
    end
    else begin
      SetLength(aNouns, 0);
      NewNounExercise(aAllNouns, aNouns, iQuestion, iCorrect, iFalse); sTest := sNewTest;
    end;
  end
  else begin
    // Create list with adjectives to be used with actual exercise
    if not (cbAdjs1.Checked or cbAdjs2.Checked) then begin
      MessageDlg('Sélection invalides', 'Il faut sélectionner au moins une classe d''adjectifs!', mtError, [mbOK], 0);
      btQuestion.Enabled := False;
    end
    else begin
      SetLength(aAdjs, 0);
      NewAdjExercise(aAllAdjs, aAdjs, iQuestion, iCorrect, iFalse); sTest := sNewTest;
    end;
  end;
end;

{ Menu item "Exercice > Quitter": Exit the application }

procedure TfLatin1.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Déclinaisons...": Model declensions display }

procedure TfLatin1.mDeclensionN1RosaClick(Sender: TObject);

begin
  DisplayDeclensionNouns('rosa', 'rosae', 'f');
end;

procedure TfLatin1.mDeclensionN2DominusClick(Sender: TObject);

begin
  DisplayDeclensionNouns('dominus', 'domini', 'm');
end;

procedure TfLatin1.mDeclensionN2AgerClick(Sender: TObject);

begin
  DisplayDeclensionNouns('ager', 'agri', 'm');
end;

procedure TfLatin1.mDeclensionN2PuerClick(Sender: TObject);

begin
  DisplayDeclensionNouns('puer', 'pueri', 'm');
end;

procedure TfLatin1.mDeclensionN2TemplumClick(Sender: TObject);

begin
  DisplayDeclensionNouns('templum', 'templi', 'n');
end;

procedure TfLatin1.mDeclensionN3CivisClick(Sender: TObject);

begin
  DisplayDeclensionNouns('civis', 'civis', 'm');
end;

procedure TfLatin1.mDeclensionN3MareClick(Sender: TObject);

begin
  DisplayDeclensionNouns('mare', 'maris', 'n');
end;

procedure TfLatin1.mDeclensionN3ConsulClick(Sender: TObject);

begin
  DisplayDeclensionNouns('consul', 'consulis', 'm');
end;

procedure TfLatin1.mDeclensionN3CorpusClick(Sender: TObject);

begin
  DisplayDeclensionNouns('corpus', 'corporis', 'n');
end;

procedure TfLatin1.mDeclensionN3PaterClick(Sender: TObject);

begin
  DisplayDeclensionNouns('pater', 'patris', 'm');
end;

procedure TfLatin1.mDeclensionN3UrbsClick(Sender: TObject);

begin
  DisplayDeclensionNouns('urbs', 'urbis', 'f');
end;

procedure TfLatin1.mDeclensionN3AnimalClick(Sender: TObject);

begin
  DisplayDeclensionNouns('animal', 'animalis', 'n');
end;

procedure TfLatin1.mDeclensionN3FebrisClick(Sender: TObject);

begin
  DisplayDeclensionNouns('febris', 'febris', 'f');
end;

procedure TfLatin1.mDeclensionN3VisClick(Sender: TObject);

begin
  DisplayDeclensionNouns('vis', '---', 'f');
end;

procedure TfLatin1.mDeclensionN4ManusClick(Sender: TObject);

begin
  DisplayDeclensionNouns('manus', 'manus', 'f');
end;

procedure TfLatin1.mDeclensionN4CornuClick(Sender: TObject);

begin
  DisplayDeclensionNouns('cornu', 'cornus', 'n');
end;

procedure TfLatin1.mDeclensionN4DomusClick(Sender: TObject);

begin
  DisplayDeclensionNouns('domus', 'domus', 'f');
end;

procedure TfLatin1.mDeclensionN5ResClick(Sender: TObject);

begin
  DisplayDeclensionNouns('res', 'rei', 'f');
end;

procedure TfLatin1.mDeclensionA1BonusClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('bonus', 'bona', 'bonum');
end;

procedure TfLatin1.mDeclensionA1MiserClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('miser', 'misera', 'miserum');
end;

procedure TfLatin1.mDeclensionA1PulcherClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('pulcher', 'pulchra', 'pulchrum');
end;

procedure TfLatin1.mDeclensionA2AcerClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('acer', 'acris', 'acre');
end;

procedure TfLatin1.mDeclensionA2FortisClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('fortis', 'fortis', 'forte');
end;

procedure TfLatin1.mDeclensionA2PrudensClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('prudens', '3id', 'prudentis');
end;

procedure TfLatin1.mDeclensionA2AudaxClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('audax', '3id', 'audacis');
end;

procedure TfLatin1.mDeclensionA2VetusClick(Sender: TObject);

begin
  DisplayDeclensionAdjs('vetus', '3id', 'veteris');
end;

{ Menu item "Options > Nombre de questions ...": Choose number of exercise questions}

procedure TfLatin1.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Déclinaisons latines', 'Nombre de questions', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 10 then
      iQuestions0 := 10;
  end;
end;

{ Menu item "Options > Adjectifs se rapportant à une personne": Choose if adjectives (in adjective-only exercises) qualify a person or an object }

procedure TfLatin1.mOptionsPersonClick(Sender: TObject);

begin
  if mOptionsPerson.Checked then
    mOptionsPerson.Checked := False
  else
    mOptionsPerson.Checked := True;
  bPerson := mOptionsPerson.Checked;
end;

{ Menu item "Options > Exclure le nominatif singulier (masculin): Choose to exclude or not nominative singular (masculine) questions }

procedure TfLatin1.mOptionsNominativeClick(Sender: TObject);

begin
  if mOptionsNominative.Checked then
    mOptionsNominative.Checked := False
  else
    mOptionsNominative.Checked := True;
end;

{ Menu item "Options > Questions (Noms/adjectifs) en français": Choose if questions are displayed in Latin or in French }

procedure TfLatin1.mOptionsFrenchClick(Sender: TObject);

begin
  if mOptionsFrench.Checked then
    mOptionsFrench.Checked := False
  else
    mOptionsFrench.Checked := True;
end;

{ Menu item "Aide > Aide grammaire": Display Latin grammar help }

procedure TfLatin1.mHelpGrammarClick(Sender: TObject);

begin
  fHelp.memoHelp.Lines.LoadFromFile('help1.txt');                                        // load help text from file help1.txt
  fHelp.Show;                                                                            // display help (as a form)
end;

{ Menu item "Aide > Aide programme": Display Latin1 program help }

procedure TfLatin1.mHelpProgramClick(Sender: TObject);

begin
  begin
    fHelp.memoHelp.Lines.LoadFromFile('help2.txt');                                        // load help text from file help2.txt
    fHelp.Show;                                                                            // display help (as a form)
  end;
end;

{ Menu item "Aide > Info programme": Display Latin1 program about }

procedure TfLatin1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Génération d''exercices de déclinaison des' + Chr(13) + 'substantifs et adjectifs latins.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, 2018-2024.';
  MessageDlg('Grammaire latine', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Réponse": Generate new question resp. check user answer }

procedure TfLatin1.btQuestionClick(Sender: TObject);

const
  AllCasusFrench: array[1..6] of string = (
    'nominatif', 'vocatif', 'accusatif', 'génitif', 'datif', 'ablatif'
  );
  AllNumbersFrench: array[1..2] of string = (
    'singulier', 'pluriel'
  );
  AllGenusFrench: array[1..3] of string = (
    'masculin', 'féminin', 'neutre'
  );

var
  Index, Index2, R: Integer;
  Casus, Number, Genus, GenusFr, CasusFr, NumberFr, Noun, Adj: string;

begin
  // Generate new exercise question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions);
    // Nouns-only exercise
    if sTest = 'nouns' then begin
      repeat
        R := Random(6) + 1; CasusFr := AllCasusFrench[R]; Casus := AllCasus[R];
        R := Random(2) + 1; NumberFr := AllNumbersFrench[R]; Number := AllNumbers[R];
      until not mOptionsNominative.Checked or (Casus <> 'nom') or (Number <> 'sg');
      repeat
        Index := Random(Length(aNouns));
        with aNouns[Index] do
          sAnswer := DeclensionNouns(Nominative, Genitive, Genus, Casus, Number);
      until sAnswer <> '---';
      if mOptionsFrench.Checked then
        edQuestion.Text := aNouns[Index].FrenchNoun + ', ' + CasusFr + ' ' + NumberFr
      else
        edQuestion.Text := aNouns[Index].Nominative + ', ' + CasusFr + ' ' + NumberFr;
    end
    // Adjectives-only exercise
    else if sTest = 'adjs' then begin
      repeat
        R := Random(6) + 1; CasusFr := AllCasusFrench[R]; Casus := AllCasus[R];
        R := Random(2) + 1; NumberFr := AllNumbersFrench[R]; Number := AllNumbers[R];
        R := Random(3) + 1; GenusFr := AllGenusFrench[R]; Genus := AllGenus[R];
      until not mOptionsNominative.Checked or (Casus <> 'nom') or (Number <> 'sg') or (Genus <> 'm');
      Index := Random(Length(aAdjs));
      with aAdjs[Index] do
        sAnswer := DeclensionAdjs(Masculine, Feminine, Neuter, Genus, Casus, Number, bPerson);
      if mOptionsFrench.Checked then
        edQuestion.Text := aAdjs[Index].FrenchMasculine + ', ' + CasusFr + ' ' + NumberFr + ' ' + GenusFr
      else
        edQuestion.Text := aAdjs[Index].Masculine + ', ' + CasusFr + ' ' + NumberFr + ' ' + GenusFr;
    end
    // Nouns with adjective exercise
    else begin
      // Random noun (from those selected under point 1)
      repeat
        R := Random(6) + 1; CasusFr := AllCasusFrench[R]; Casus := AllCasus[R];
        R := Random(2) + 1; NumberFr := AllNumbersFrench[R]; Number := AllNumbers[R];
      until not mOptionsNominative.Checked or (Casus <> 'nom') or (Number <> 'sg');
      repeat
        Index := Random(Length(aNouns));
        with aNouns[Index] do
          sAnswer := DeclensionNouns(Nominative, Genitive, Genus, Casus, Number);        // declension of the noun
      until sAnswer <> '---';
      sAnswer += ' ';
      if mOptionsFrench.Checked then                                                     // Display French noun if this is selected
        Noun := aNouns[Index].FrenchNoun
      else
        Noun := aNouns[Index].Nominative;
      // The genus for the adjective = the genus of the noun and the adjective is (randomly) chosen from those "allowed" for this specific noun
      Genus := aNouns[Index].Genus;
      R := Random(aNouns[Index].NAdjectives) + 1;
      Index2 := aNouns[Index].Adjectives[R];
      with aAllAdjs[Index2] do
        sAnswer += DeclensionAdjs(Masculine, Feminine, Neuter, Genus, Casus, Number, aNouns[Index].Person); // declension of the adjective
      // If the question is to be displayed in French, determine the following:
      //   - adjective preceeding or succeeding the noun
      //   - masculine or feminine form of adjective (depending on noun's genus)
      //   - if adjective preceeds noun, transform masculines like "beau" to "bel" if followed by vowels and 'h'
      //   - do not display article 'un'/'une' if adjective starts with article 'le'/'la'
      if mOptionsFrench.Checked then begin
        // French adjective preceeding the noun
        if aAllAdjs[Index2].AdjPos = '-' then begin
          if aNouns[Index].FrenchGenus = 'm' then begin
            Adj := aAllAdjs[Index2].FrenchMasculine;
            if (RightStr(Adj, 3) = 'eau') and ((LeftStr(Noun, 1)[1] in ['i', 'u', 'e', 'o', 'a', 'h']) or (LeftStr(Noun, 2) = 'é')) then
              Adj := LeftStr(Adj, Length(Adj) - 3) + 'el';
            edQuestion.Text := Adj + ' ' + Noun + ', ' + CasusFr + ' ' + NumberFr
          end
          else
            edQuestion.Text := aAllAdjs[Index2].FrenchFeminine + ' ' + Noun + ', ' + CasusFr + ' ' + NumberFr
        end
        // French adjective succeeding the noun
        else begin
          if aNouns[Index].FrenchGenus = 'm' then
            edQuestion.Text := Noun + ' ' + aAllAdjs[Index2].FrenchMasculine + ', ' + CasusFr + ' ' + NumberFr
          else
            edQuestion.Text := Noun + ' ' + aAllAdjs[Index2].FrenchFeminine + ', ' + CasusFr + ' ' + NumberFr;
        end;
      end
      // Question is displayed in Latin (noun preceeding adjective?)
      else
        edQuestion.Text := Noun + ' ' + aAllAdjs[Index2].Masculine + ', ' + CasusFr + ' ' + NumberFr;
      // If question is displayed in French, add undetermined article
      if mOptionsFrench.Checked then begin
        if aNouns[Index].FrenchGenus = 'm' then begin
          if LeftStr(aAllAdjs[Index2].FrenchMasculine, 3) <> 'le ' then
            edQuestion.Text := 'un ' + edQuestion.Text;
        end
        else begin
          if LeftStr(aAllAdjs[Index2].FrenchFeminine, 3) <> 'la ' then
            edQuestion.Text := 'une ' + edQuestion.Text;
        end;
      end;
    end;
    edAnswer.Text := ''; edResult.Text := '';
    // Next push on button will be for checking the user answer
    btQuestion.Caption := 'Réponse';
    edAnswer.SetFocus;
  end
  // Check user answer to actual exercise question
  else begin
    edRQuestion.Text := IntToStr(iQuestion);
    // Correct answer
    if edAnswer.Text = sAnswer then begin
      edResult.Text := 'Bonne réponse';
      edResult.Font.Color := clLime;
      Inc(iCorrect);
    end
    // False answer
    else begin
      edResult.Text := 'Faux! Correct = ' + sAnswer;
      edResult.Font.Color := clREd;
      Inc(iFalse);
    end;
    edRCorrect.Text := IntToStr(iCorrect); edRFalse.Text := IntToStr(iFalse);
    edRSuccess.Text := IntToStr(Round(100 * iCorrect / iQuestion)) + '%';
    btQuestion.Caption := 'Question';
    btQuestion.SetFocus;
    // If all questions (as selected) have been done, display message and block button, until "Exercice > Nouveau" is executed
    if iQuestion = iQuestions then begin
      MessageDlg('Fin de l''exercice', 'Pour faire un autre, choisissez "Nouveau" dans le menu "Exercice", s''il-vous-plaît!', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Changement of radiobutton (= exercise type) selection }

procedure TfLatin1.rbNounsChange(Sender: TObject);

begin
  if rbNouns.Checked then
    sNewTest := 'nouns'
end;

procedure TfLatin1.rbAdjsChange(Sender: TObject);

begin
  if rbAdjs.Checked then
    sNewTest := 'adjs';
end;

procedure TfLatin1.rbAllChange(Sender: TObject);

begin
  if rbAll.Checked then
    sNewTest := 'all';
end;

end.

