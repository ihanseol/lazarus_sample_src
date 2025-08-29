{************************************}
{* Main unit for GRgods application *}
{************************************}

unit grgods_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, grgods_help;

type
  TGod = record
    NameGreek, NameRoman: string;
    Sex, Olympian: Char;
    Realm, Symbollist: string;
    Realms, Symbols: array of string;
  end;
  TGods = array of TGod;
  {**********}
  { TfGRgods }
  {**********}
  TfGRgods = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizSex, mQuizCounterpart, mQuizOlymp: TMenuItem;
    mQuizRealms1, mQuizRealms2, mQuizSymbols1, mQuizSymbols2, mQuizExit: TMenuItem;
    mOptions, mOptionsNames, mOptionsNamesGreek, mOptionsNamesRoman: TMenuItem;
    mOptionsInput, mOptionsInputList, mOptionsInputManual, mOptionsMainGods: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, laTitle, laQuestion: TLabel;
    edName: TEdit;
    cobNames: TComboBox;
    rbAnswer1, rbAnswer2: TRadioButton;
    cbAnswer1, cbAnswer2, cbAnswer3, cbAnswer4, cbAnswer5,  cbAnswer6: TCheckBox;
    btAction: TButton;
    btCancel: TButton;
    edEval: TEdit;
    sgEval: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure mQuizSexClick(Sender: TObject);
    procedure mQuizCounterpartClick(Sender: TObject);
    procedure mQuizOlympClick(Sender: TObject);
    procedure mQuizRealms1Click(Sender: TObject);
    procedure mQuizRealms2Click(Sender: TObject);
    procedure mQuizSymbols1Click(Sender: TObject);
    procedure mQuizSymbols2Click(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsNamesGreekClick(Sender: TObject);
    procedure mOptionsNamesRomanClick(Sender: TObject);
    procedure mOptionsInputListClick(Sender: TObject);
    procedure mOptionsInputManualClick(Sender: TObject);
    procedure mOptionsMainGodsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    iQuiz, iList, iGods, iGod, iQuestions, iQuestion, iCorrect: Integer;
    sNames, sInput, sAnswer: string;
    aGods: TGods;
    aDone: array of Boolean;
    cbAnswers: array[0..5] of TCheckBox;
  end;

var
  fGRgods: TfGRgods;

implementation

{$R *.lfm}

{ Format number for right-alignment in the string grid }

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
    SN += ' '
  else
    SN += S;
  Result := SN;
end;

{ Read gods data from text file }

procedure ReadGods(out Gods: TGods);

var
  N, NR, NS, P: Integer;
  Line, SRealm, SSymbol, S: string;
  InFile: Text;

begin
  SetLength(Gods, 0); N := 0;
  Assign(InFile, 'gods.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Gods, N);
      with Gods[N - 1] do begin
        NameRoman := Trim(LeftStr(Line, 15));                                  // cols  1 - 15: Roman name of the god
        NameGreek := Trim(Copy(Line, 16, 15));                                 // cols 16 - 30: Greek name of the god
        Sex := Copy(Line, 31, 1)[1];                                           // col 31: sex of the god ('m' or 'f')
        Olympian := Copy(Line, 93, 1)[1];                                      // col 93: Olympian god or not ('O' or '-')
        Realm := Trim(Copy(Line, 33, 60));                                     // cols 33 - 92: realms list
        // Create array with the different realms
        S := Realm; NR := 0;
        S := StringReplace(S, ' and ', ', ', []);                              // this "and" has to be treated as a realm separator
        while S <> '' do begin
          P := Pos(', ', S);                                                   // all realms are now separated by commas
          if P > 0 then begin
            SRealm := Copy(S, 1, P - 1);
            Delete(S, 1, Length(SRealm) + 2);
          end
          else begin
            SRealm := S;
            S := '';
          end;
          // Add "god(dess) of" to realms, for which nothing is specified
          P := Pos('god', SRealm);
          if P = 0 then begin
            P := Pos('king', SRealm);
            if P = 0 then begin
              P := Pos('ruler', SRealm);
              if P = 0 then begin
                P := Pos('queen', SRealm);
                if P = 0 then
                  P := Pos('messenger', SRealm);
              end;
            end;
          end;
          if P = 0 then begin
            if Sex = 'm' then
              SRealm := 'god of ' + SRealm
            else
              SRealm := 'goddess of ' + SRealm;
          end;
          // Add the realm to the realms array for this god
          Inc(NR); SetLength(Realms, NR);
          Realms[NR - 1] := SRealm;
        end;
        Symbollist := ''; SetLength(Symbols, 0);
        // Create array with the different symbols
        if Length(Line) > 95 then begin
          Symbollist := Trim(Copy(Line, 95, Length(Line)));                    // cols 95-: symbols list
          S := Symbollist; NS := 0;
          while S <> '' do begin
            P := Pos(', ', S);                                                 // in the file, the symbols are separated by commas
            if P > 0 then begin
              SSymbol := Copy(S, 1, P - 1);
              Delete(S, 1, Length(SSymbol) + 2);
            end
            else begin
              SSymbol := S;
              S := '';
            end;
            // Add the symbol to the symbols array for this god
            Inc(NS); SetLength(Symbols, NS);
            Symbols[NS - 1] := SSymbol;
          end;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Fill the gods combobox (with the names corr. to actual selections) }

procedure FillNames(var Gods: TGods; Names: string; Count: Integer);

var
  I, J: Integer;
  Temp: string;

begin
  fGRgods.cobNames.Clear;
  for I := 0 to Count - 1 do begin                                             // "Count" = 14 or full list
    if Names = 'greek' then
      fGRgods.cobNames.Items.AddText(Gods[I].NameGreek)                        // use Greek names
    else
      fGRgods.cobNames.Items.AddText(Gods[I].NameRoman);                       // use Roman names
  end;
  // Sort the names in the combobox
  for I := 0 to Count - 2 do begin
    for J := I + 1 to Count - 1 do begin
      if fGRgods.cobNames.Items[J] < fGRgods.cobNames.Items[I] then begin
        Temp := fGRgods.cobNames.Items[I]; fGRgods.cobNames.Items[I] := fGRgods.cobNames.Items[J]; fGRgods.cobNames.Items[J] := Temp;
      end;
    end;

  end;
  fGRgods.cobNames.ItemIndex := 0;
end;

{ Get god name from the Gods array (depending on actual names selection) }

function GetGodName(var Gods: TGods; IX: Integer; Names: string; GetCounterPart: Boolean): string;

var
  God, CounterPart: string;

begin
  if Names = 'greek' then begin
    God := Gods[IX].NameGreek;
    CounterPart := Gods[IX].NameRoman;
  end
  else begin
    God := Gods[IX].NameRoman;
    CounterPart := Gods[IX].NameGreek;
  end;
  if GetCounterPart then begin
    // If this Boolean is set true, the function returns the Roman name if "Greek" is actually selected and vice-versa
    God := CounterPart;
  end;
  Result := God;
end;

{ Generate random realms and set the realms checkboxes labels }

function CreateRealmAnswers(var Gods: TGods; NGods, IX: Integer): string;

var
  G, N, I, J: Integer;
  Answers, S: string;
  OK: Boolean;
  Realms: array[0..5] of string;

begin
  for I := 0 to 5 do
    fGRgods.cbAnswers[I].Checked := False;
  Answers := '000000';                                                         // the answer string is a sequence of 1s or 0s
  // Start by choosing 4 realms of other gods
  for I := 0 to 4 do begin
    repeat
      OK := True;
      // Random god
      repeat
        G := Random(NGods);
      until G <> IX;
      // Random realm of this god
      Realms[I] := Gods[G].Realms[Random(Length(Gods[G].Realms))];
      // Some realms would result in a sensless answer selection list: exclude them now
      if (Gods[IX].NameGreek = 'Zeus') and (Realms[I] = 'queen of the gods') then
        OK := False
      else if (Gods[IX].NameGreek = 'Hera') and (Realms[I] = 'king of the gods') then
        OK := False
      else if (Gods[IX].NameGreek = 'Venus') and (Realms[I] = 'god of love') then
        OK := False
      else if (Gods[IX].NameGreek = 'Eros') and (Realms[I] = 'goddess of love') then
        OK := False
      else begin
        // Be sure that realms are unique
        for J := 0 to I - 1 do begin
          if Realms[I] = Realms[J] then
            OK := False;
        end;
      end;
    until OK;
  end;
  // Set the combobox labels
  for I := 0 to 4 do
    fGRgods.cbAnswers[I].Caption := ' ' + Realms[I];
  // Now, randomly exchange some bad answers by correct ones
  J := Random(4);
  if J = 3 then
    N := Length(Gods[IX].Realms)                                               // use all realms of actual god
  else
    N := J;                                                                    // use 0, 1 or 2 realms of actual god
  if N > Length(Gods[IX].Realms) then                                          // for some gods there is only 1 realm
    N := Length(Gods[IX].Realms);
  // Randomly place the correct answers
  for I := 1 to N do begin
    repeat
      J := Random(5) + 1;
    until Answers[J] = '0';
    fGRgods.cbAnswers[J - 1].Caption := ' ' + Gods[IX].Realms[I - 1];
    Answers[J] := '1';                                                         // this is a correct answer (user has to select corr. checkbox)
  end;
  // Depending on the sex of the actual god, adapt the checkbox labels
  for I := 0 to 4 do begin
    S := fGRgods.cbAnswers[I].Caption;
    if Gods[IX].Sex = 'm' then begin
      S := StringReplace(S, 'goddess', 'god', []);
      S := StringReplace(S, 'queen', 'king', []);
    end
    else begin
      S := StringReplace(S, 'goddess', '*', []);
      S := StringReplace(S, 'gods', '#', []);
      S := StringReplace(S, 'god', 'goddess', []);
      S := StringReplace(S, '*', 'goddess', []);
      S := StringReplace(S, '#', 'gods', []);
      S := StringReplace(S, 'king', 'queen', []);
    end;
    fGRgods.cbAnswers[I].Caption := S;
  end;
  if N = 0 then
    Answers[6] := '1';                                                         // if no correct answer, user has to select the "--none--" checkbox
  Result := Answers;
end;

{ Generate random symbols and set the symbols checkboxes labels }

function CreateSymbolAnswers(var Gods: TGods; NGods, IX: Integer): string;

var
  G, N, I, J: Integer;
  Answers: string;
  OK: Boolean;
  Symbols: array[0..5] of string;

begin
  for I := 0 to 5 do
    fGRgods.cbAnswers[I].Checked := False;
  Answers := '000000';                                                         // the answer string is a sequence of 1s and 0s
  // Start by choosing 4 symbols of other gods
  for I := 0 to 4 do begin
    repeat
      OK := True;
      // Random god
      repeat
        G := Random(NGods);
      until G <> IX;
      // Random symbol of this god
      Symbols[I] := Gods[G].Symbols[Random(Length(Gods[G].Symbols))];
      // Some symbols would result in a sensless answer selection list: exclude them now
      if (Gods[IX].NameGreek = 'Apollo') and (Symbols[I] = 'bow and arrow') then
        OK := False
      else if (Gods[IX].NameGreek = 'Artemis') and (Symbols[I] = 'bow and arrows') then
        OK := False
      else if (Gods[IX].NameGreek = 'Hades') and (Symbols[I] = 'helmet') then
        OK := False
      else if (Gods[IX].NameGreek = 'Hermes') and (Symbols[I] = 'invisibility helmet') then
        OK := False
      else if (Gods[IX].NameGreek = 'Hestia') and (Symbols[I] = 'fire') then
        OK := False
      else if (Gods[IX].NameGreek = 'Hephaestus') and (Symbols[I] = 'fire') then
        OK := False
      else begin
        // Be sure symbols are unique
        for J := 0 to I - 1 do begin
          if Symbols[I] = Symbols[J] then
            OK := False;
        end;
      end;
    until OK;
  end;
  // Set the combobox labels
  for I := 0 to 4 do
    fGRgods.cbAnswers[I].Caption := ' ' + Symbols[I];
  // Now, randomly exchange some bad answers by correct ones
  J := Random(4);
  if J = 3 then
    N := Length(Gods[IX].Symbols)                                              // use all symbols of actual god
  else
    N := J;                                                                    // use 0, 1 or 2 symbols of actual god
  // Randomly place the correct answers
  for I := 1 to N do begin
    repeat
      J := Random(5) + 1;
    until Answers[J] = '0';
    fGRgods.cbAnswers[J - 1].Caption := ' ' + Gods[IX].Symbols[I - 1];
    Answers[J] := '1';                                                         // this is a correct answer (user has to select corr. checkbox)
  end;
  if N = 0 then
    Answers[6] := '1';                                                         // if no correct answer, user has to select the "--none--" checkbox
  Result := Answers;
end;

{ Get user answer (checkbox selections) }

function ReadAnnswerCheckboxes: string;

var
  I: Integer;
  Answers: string;

begin
  Answers := '';
  for I := 0 to 5 do begin
    if fGRgods.cbAnswers[I].Checked then
      Answers += '1'                                                           // checkbox checked = "1"
    else
      Answers += '0';                                                          // checkbox unchecked = "0"
    Result := Answers;
  end;
end;

{**********}
{ TfGRgods }
{**********}

{ Application start: Initialisations }

procedure TfGRgods.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create array with checkboxes
  cbAnswers[0] := cbAnswer1; cbAnswers[1] := cbAnswer2; cbAnswers[2] := cbAnswer3;
  cbAnswers[3] := cbAnswer4; cbAnswers[4] := cbAnswer5; cbAnswers[5] := cbAnswer6;
  // Re-arrange form controls
  edName.Top := rbAnswer1.Top;
  cobNames.Left := 10; cobNames.Top := rbAnswer1.Top;
  for I := 0 to 2 do begin
    cbAnswers[I].Top := rbAnswer1.Top;
    cbAnswers[I + 3].Top := rbAnswer1.Top + 40;
  end;
  // Read the gods data
  ReadGods(aGods);
  // Start random number generator
  Randomize;
  // Start quiz of type 1 with default settings
  iQuiz := 1; sNames := 'greek'; sInput := 'list'; iList := 14;
  mQuizSex.Click;
end;

{ Menu item "Quiz > Gods and goddesses": Start a type 1 quiz }

procedure TfGRgods.mQuizSexClick(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Gods and goddesses.';
  laQuestion.Caption := 'Is this a god or a goddess?';
  rbAnswer1.Visible := True; rbAnswer2.Visible := True;
  rbAnswer1.Caption := ' God'; rbAnswer2.Caption := ' Goddess';
  rbAnswer1.Checked := False; rbAnswer2.Checked := False;
  edName.Visible := False; cobNames.Visible := False;
  for I := 0 to 5 do
    cbAnswers[I].Visible := False;
  iQuiz := 1;
  btAction.Enabled := True;
end;

{ Menu item "Quiz > Greek and Roman gods": Start a type 2 quiz }

procedure TfGRgods.mQuizCounterpartClick(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Greek and Roman gods.';
  if sNames = 'greek' then
    laQuestion.Caption := 'What is the Roman name of this god?'
  else
    laQuestion.Caption := 'What is the Greek name of this god?';
  if sNames = 'greek' then
    FillNames(aGods, 'roman', iList)
  else
    FillNames(aGods, 'greek', iList);
  rbAnswer1.Visible := False; rbAnswer2.Visible := False;
  if mOptionsInputList.Checked then begin
    edName.Visible := False; cobNames.Visible := True;
  end
  else begin
    edName.Visible := True; edName.Text := ''; cobNames.Visible := False;
  end;
  for I := 0 to 5 do
    cbAnswers[I].Visible := False;
  iQuiz := 2;
  btAction.Caption := 'Start'; btAction.Enabled := True; btCancel.Enabled := False;
end;

{ Menu item "Quiz > Olympian gods": Start a type 3 quiz }

procedure TfGRgods.mQuizOlympClick(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Olympian gods.';
  laQuestion.Caption := 'Is this an Olympian god?';
  rbAnswer1.Visible := True; rbAnswer2.Visible := True;
  rbAnswer1.Caption := ' Olympian'; rbAnswer2.Caption := ' Not Olympian';
  rbAnswer1.Checked := False; rbAnswer2.Checked := False;
  edName.Visible := False; cobNames.Visible := False;
  for I := 0 to 5 do
    cbAnswers[I].Visible := False;
  iQuiz := 3;
  btAction.Caption := 'Start'; btAction.Enabled := True; btCancel.Enabled := False;
end;

{ Menu item "Quiz > Gods and their realms (I)": Start a type 4 quiz }

procedure TfGRgods.mQuizRealms1Click(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Gods and their realms (I).';
  laQuestion.Caption := 'Who is the god of these realms?';
  FillNames(aGods, sNames, iList);
  rbAnswer1.Visible := False; rbAnswer2.Visible := False;
  if mOptionsInputList.Checked then begin
    edName.Visible := False; cobNames.Visible := True;
  end
  else begin
    edName.Visible := True; edName.Text := ''; cobNames.Visible := False;
  end;
  for I := 0 to 5 do
    cbAnswers[I].Visible := False;
  iQuiz := 4;
  btAction.Caption := 'Start'; btAction.Enabled := True; btCancel.Enabled := False;
end;

{ Menu item "Quiz > Gods and their realms (II)": Start a type 5 quiz }

procedure TfGRgods.mQuizRealms2Click(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Gods and their realms (II).';
  laQuestion.Caption := 'Which of these realms are associated with this god?';
  rbAnswer1.Visible := False; rbAnswer2.Visible := False;
  edName.Visible := False; cobNames.Visible := False;
  for I := 0 to 5 do begin
    cbAnswers[I].Visible := True;
    if I < 5 then
      cbAnswers[I].Caption := ' Realm ' + IntToStr(I + 1);
  end;
  iQuiz := 5;
  btAction.Caption := 'Start'; btAction.Enabled := True; btCancel.Enabled := False;
end;

{ Menu item "Quiz > Gods and their symbols (I)": Start a type 6 quiz }

procedure TfGRgods.mQuizSymbols1Click(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Gods and their symbols (I).';
  laQuestion.Caption := 'Who is the god associated with these symbols?';
  FillNames(aGods, sNames, iList);
  rbAnswer1.Visible := False; rbAnswer2.Visible := False;
  if mOptionsInputList.Checked then begin
    edName.Visible := False; cobNames.Visible := True;
  end
  else begin
    edName.Visible := True; edName.Text := ''; cobNames.Visible := False;
  end;
  for I := 0 to 5 do
    cbAnswers[I].Visible := False;
  iQuiz := 6;
  btAction.Caption := 'Start'; btAction.Enabled := True; btCancel.Enabled := False;
end;

{ Menu item "Quiz > Gods and their symbols (II)": Start a type 7 quiz }

procedure TfGRgods.mQuizSymbols2Click(Sender: TObject);

var
  I: Integer;

begin
  laTitle.Caption := 'Gods and their symbols (II).';
  laQuestion.Caption := 'Which of these symbols are associated with this god?';
  rbAnswer1.Visible := False; rbAnswer2.Visible := False;
  edName.Visible := False; cobNames.Visible := False;
  for I := 0 to 5 do begin
    cbAnswers[I].Visible := True;
    if I < 5 then
      cbAnswers[I].Caption := ' Symbol ' + IntToStr(I + 1);
  end;
  iQuiz := 7;
  btAction.Caption := 'Start'; btAction.Enabled := True; btCancel.Enabled := False;
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfGRgods.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > God names > ...": Select Greek or Roman god names }

procedure TfGRgods.mOptionsNamesGreekClick(Sender: TObject);

begin
  mOptionsNamesGreek.Checked := True; mOptionsNamesRoman.Checked := False;
  sNames := 'greek';
end;

procedure TfGRgods.mOptionsNamesRomanClick(Sender: TObject);

begin
  mOptionsNamesGreek.Checked := False; mOptionsNamesRoman.Checked := True;
  sNames := 'roman';
end;

{ Menu items "Options > Name input > ...": Select name selection list or manually name input }

procedure TfGRgods.mOptionsInputListClick(Sender: TObject);

begin
  mOptionsInputList.Checked := True; mOptionsInputManual.Checked := False;
  sInput := 'list';
end;

procedure TfGRgods.mOptionsInputManualClick(Sender: TObject);

begin
  mOptionsInputList.Checked := False; mOptionsInputManual.Checked := True;
  sInput := 'manual';
end;

{ Menu item "Options > Main gods only": Toggle between main gods and full god list }

procedure TfGRgods.mOptionsMainGodsClick(Sender: TObject);

begin
  if mOptionsMainGods.Checked then begin
    mOptionsMainGods.Checked := False;
    iList := Length(aGods);
  end
  else begin
    mOptionsMainGods.Checked := True;
    iList := 14;
  end;
end;

{ Menu item "Help > Help": Display application help }

procedure TfGRgods.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfGRgods.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Greek and Roman gods quiz.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Oktober 2022.';
  MessageDlg('About "GRgods"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next/Answer" pushed: Generate new question resp. check user answer }

procedure TfGRgods.btActionClick(Sender: TObject);

var
  I: Integer;
  UAnswer, S: string;

begin
  // Button "Start/Next" pushed: Generate new question
  if (btAction.Caption = 'Start') or (btAction.Caption = 'Next') then begin
    // Button "Start" pushed: Some initialisations to do
    if btAction.Caption = 'Start' then begin
      mOptions.Enabled := False;                                               // disable the "Options" menu (until quiz over or canceled)
      // Determine number of gods (14 main gods or full list) to be used for this quiz
      if iQuiz = 3 then
        iGods := Length(aGods)                                                 // always use full list with "Olympian gods" quiz
      else if (iQuiz = 6) or (iQuiz = 7) then
        iGods := 14                                                            // always main gods only for both symbol quizes
      else
        iGods := iList;                                                        // normal case: number of gods depends on corr. option selection
      // Set all gods as not yet been done
      SetLength(aDone, iGods);
      for I := 0 to iGods - 1 do
        aDone[I] := False;
      // Clear the form and reset variables
      for I := 0 to 3 do
        sgEval.Cells[1, I] := '';
      iQuestions := 10; iQuestion := 0; iCorrect := 0;
      btCancel.Enabled := True;
    end;
    // Common part for "Start" and "Next" button
    Inc(iQuestion);
    repeat
      iGod := Random(iGods);                                                   // random god (among those not yet done)
    until aDone[iGod] = False;
    aDone[iGod] := True;                                                       // set this god as done
    // Create question text and determine answer (depending on quiz type)
    case iQuiz of
      1: begin
        laQuestion.Caption := 'Is ' + GetGodName(aGods, iGod, sNames, False) + ' a god or a goddess?';
        sAnswer := aGods[iGod].Sex;
      end;
      2: begin
        if sNames = 'greek' then
          laQuestion.Caption := 'What is the Roman name of '
        else
          laQuestion.Caption := 'What is the Greek name of ';
        laQuestion.Caption := laQuestion.Caption + GetGodName(aGods, iGod, sNames, False) + '?';
        sAnswer := GetGodName(aGods, iGod, sNames, True);
      end;
      3: begin
        S := 'Is ' + GetGodName(aGods, iGod, sNames, False) + ' an Olympian god?';
        if aGods[iGod].Sex = 'f' then
          S := StringReplace(S, 'god', 'goddess', []);
        laQuestion.Caption := S;
        sAnswer := aGods[iGod].Olympian;
      end;
      4: begin
        S := 'Who is the ' + aGods[iGod].Realm + '?';
        if aGods[iGod].NameGreek <> 'Hera' then
          S := StringReplace(S, 'goddess', 'god', []);
        laQuestion.Caption := S;
        sAnswer := GetGodName(aGods, iGod, sNames, False);
      end;
      5: begin
        laQuestion.Caption := 'Which of these realms are associated with ' + GetGodName(aGods, iGod, sNames, False) + '?';
        sAnswer := CreateRealmAnswers(aGods, iGods, iGod);                     // random realms; answer string made of 1s and 0s
      end;
      6: begin
        laQuestion.Caption := 'Who is the god associated with: ' + aGods[iGod].Symbollist + '?';
        sAnswer := GetGodName(aGods, iGod, sNames, False);
      end;
      7: begin
        laQuestion.Caption := 'Which of these symbols are associated with ' + GetGodName(aGods, iGod, sNames, False) + '?';
        sAnswer := CreateSymbolAnswers(aGods, iGods, iGod);                    // random symbols; answer string made of 1s and 0s
      end;
    end;
    edEval.Text := ''; edEval.Color := clForm;
    btAction.Caption := 'Answer';
  end
  // Button "Answer": Check user answer
  else begin
    case iQuiz of
      // Quiz type 1: User answer = radiobuttons
      1: begin
        if rbAnswer1.Checked then
          UAnswer := 'm'
        else if rbAnswer2.Checked then
          UAnswer := 'f';
      end;
      // Quiz types 2, 4, 6: User answer = edit field or combobox selection
      2, 4, 6: begin
        if mOptionsInputManual.Checked then
          UAnswer := edName.Text
        else
          UAnswer := cobNames.Text;
      end;
      // Quiz type 3: User answer = radiobuttons
      3: begin
        if rbAnswer1.Checked then
          UAnswer := 'O'
        else if rbAnswer2.Checked then
          UAnswer := '-';
      end;
      // Quiz type 5, 7: User answer = checkboxes selection
      5, 7: begin
        UAnswer := ReadAnnswerCheckboxes;
      end;
    end;
    // Evaluation
    if UAnswer = sAnswer then begin
      // Correct answer
      edEval.Text := 'This is correct!';
      edEval.Color := clLime;
      Inc(iCorrect);
    end
    else begin
      // False answer
      edEval.Text := 'This is false!';
      edEval.Color := clRed;
    end;
    // Update string grid
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * iCorrect / iQuestion), '%');
    // Check if there are still questions left; if not, terminate the quiz
    if iQuestion < iQuestions then begin
      btAction.Caption := 'Next';
    end
    else begin
      MessageDlg('Gods quiz', 'This quiz is over. You may use the "Quiz" menu items to try another one.', mtInformation, [mbOK], 0);
      btAction.Caption := 'Start';
      mOptions.Enabled := True;                                                // re-enable the "Options" menu
      btAction.Enabled := False;                                               // disable buttons until a new quiz is started from the "Quiz" menu
      btCancel.Enabled := False;
    end;
  end;
end;

{ Button "Cancel" pushed: User termination of the quiz }

procedure TfGRgods.btCancelClick(Sender: TObject);

begin
  MessageDlg('Gods quiz', 'The quiz has been terminated by the user. You may use the "Quiz" menu items to try another one.', mtInformation, [mbOK], 0);
  btAction.Caption := 'Start';
  mOptions.Enabled := True;                                                    // re-enable the "Options" menu
  btAction.Enabled := False;                                                   // disable buttons until a new quiz is started from the "Quiz" menu
  btCancel.Enabled := False;
end;

end.

