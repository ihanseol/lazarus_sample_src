//
// Arithmetic3: "Die Kinder zählen die Autos" (main unit for GUI application)
//

unit arithmetics3_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ExtCtrls, PopupNotifier, arithmetics3_common, arithmetics3_userdata, arithmetics3_help;

type
  { TFormArithmetic3 }
  TFormArithmetic3 = class(TForm)
    QMemo: TMemo;
    QSave: TSaveDialog;
    Title: TLabel;
    MainMenu: TMainMenu;
    MenuArithmetic3: TMenuItem;
    MenuItemArithmetic3Display: TMenuItem;
    MenuItemArithmetic3Test1: TMenuItem;
    MenuItemArithmetic3Test2: TMenuItem;
    MenuItemArithmetic3Exit: TMenuItem;
    MenuSettings: TMenuItem;
    MenuSettingsTQ: TMenuItem;
    MenuSettingsTQChange: TMenuItem;
    MenuSettingsTypes: TMenuItem;
    MenuSettingsType1: TMenuItem;
    MenuSettingsType2: TMenuItem;
    MenuSettingsType4: TMenuItem;
    MenuSettingsType3: TMenuItem;
    MenuSettingsType5: TMenuItem;
    MenuExtras: TMenuItem;
    MenuExtrasFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuHelpHelp: TMenuItem;
    MenuHelpInfo: TMenuItem;
    QuestionN: TEdit;
    QuestionTxt: TMemo;
    Label1: TLabel;
    AnswersCorrect: TEdit;
    Label2: TLabel;
    AnswersFalse: TEdit;
    Label3: TLabel;
    AnswersPercent: TEdit;
    Label4: TLabel;
    Answer: TEdit;
    AnswerEval: TEdit;
    Button0: TButton;
    PAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MenuItemArithmetic3DisplayClick(Sender: TObject);
    procedure MenuItemArithmetic3Test1Click(Sender: TObject);
    procedure MenuItemArithmetic3Test2Click(Sender: TObject);
    procedure MenuItemArithmetic3ExitClick(Sender: TObject);
    procedure MenuSettingsTQChangeClick(Sender: TObject);
    procedure MenuSettingsType1Click(Sender: TObject);
    procedure MenuSettingsType2Click(Sender: TObject);
    procedure MenuSettingsType4Click(Sender: TObject);
    procedure MenuSettingsType3Click(Sender: TObject);
    procedure MenuSettingsType5Click(Sender: TObject);
    procedure MenuExtrasFileClick(Sender: TObject);
    procedure MenuHelpHelpClick(Sender: TObject);
    procedure MenuHelpInfoClick(Sender: TObject);
    procedure Button0Click(Sender: TObject);
  end;

const
  Title0 = 'Arithmetik Aufgaben: Die Kinder zählen die Autos';
  QTypeLimits: array[1..6] of Integer = (0, 21, 40, 69, 84, 99);               // indexes for the different question main types

var
  FormArithmetic3: TFormArithmetic3;
  QTypesUsed: array[1..NQuestionTypes] of Boolean;
  Test2Questions, NQuestions, NQuestion, NQuestionUsed: Integer;
  Result, CountCorrect, CountFalse: Integer;
  Task: string;

procedure ApplicationReset(Task: string);
procedure QTypesUsedUpdate(QType: Integer; Used: Boolean);

implementation

{$R *.lfm}

{ Application reset (clear form) }

procedure ApplicationReset(Task: string);

var
  I: Integer;
  Title, Counts: string;
  Txt: array[1..8] of string;

begin
  Title := Title0;
  if task = 'display' then
    Counts := ''
  else begin
    Title += ' - Test ' + Copy(Task, Length(Task), 1);
    Counts := '0';
  end;
  FormArithmetic3.Title.Caption := Title;
  FormArithmetic3.QuestionN.Text := '';
  FormArithmetic3.AnswersCorrect.Text := Counts;
  FormArithmetic3.AnswersFalse.Text := Counts;
  FormArithmetic3.AnswersPercent.Color := cl3DLight;
  if Counts = '0' then
    Counts += '%';
  FormArithmetic3.AnswersPercent.Text := Counts;
  FormArithmetic3.Answer.Text := '';
  FormArithmetic3.AnswerEval.Text := '';
  FormArithmetic3.Button0.Caption := 'Frage 1';
  //FormArithmetic3.Button0.SetFocus;
  for I := 1 to 8 do
    Txt[I] := '';
  FormArithmetic3.QuestionTxt.Lines.AddStrings(Txt, True);
  NQuestion := 0; NQuestionUsed := 0;
  CountCorrect := 0; CountFalse := 0;
end;

{ Mark actual taype questions to be used for the test }

procedure QTypesUsedUpdate(QType: Integer; Used: Boolean);

var
  I: Integer;

begin
  for I := QTypeLimits[QType] + 1 to QTypeLimits[QType + 1] do
    QTypesUsed[I] := Used;
end;

{ ================ }
{ TFormArithmetic3 }
{ ================ }

{ Application start: Initialisation}

procedure TFormArithmetic3.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  Randomize;
  Test2Questions := 25;
  NQuestions := 25;
  Task := 'test2';
  PAbout.Text := 'Arithmetik Aufgaben für Grundschüler' + Chr(13) + Chr(13) + 'Version 1.0, © allu, 2017';
  for I := 1 to NQuestionTypes do
    QTypesUsed[I] := True;                                                     // all questions used at startup
  //NQuestion := 0; NQuestionUsed := 0;
  //CountCorrect := 0; CountFalse := 0;
  ApplicationReset(Task);
end;

{ Form activation: Read number of questions from user data input form }

procedure TFormArithmetic3.FormActivate(Sender: TObject);

// When the user has entered the number of questions and presses the OK button on the user data input form,
// focus comes back to the main form. This procedure being then executed, the value entered by the user is
// read from the data input form and stored into the main form.

begin
  Test2Questions := StrToInt(FormUserData.T2Questions.Text);
  MenuSettingsTQ.Caption := 'Test 2 Aufgaben = ' + IntToStr(Test2Questions);
  if MenuSettingsType2.Checked then
    NQuestions := Test2Questions;
end;

{ Menu item "Aufgaben > Aufgabenbeispiele anzeigen": Set task to display example questions (including results) }

procedure TFormArithmetic3.MenuItemArithmetic3DisplayClick(Sender: TObject);

begin
  Task := 'display';
  NQuestions := NQuestionTypes;
  Answer.TabStop := False;
  Answer.ReadOnly := True;
  ApplicationReset(Task);
end;

{ Menu item "Aufgaben > Test 1 (alle Aufgabenbeispiele)": Set task to do test with all questions (of selected types) }

procedure TFormArithmetic3.MenuItemArithmetic3Test1Click(Sender: TObject);

begin
  Task := 'test1';
  NQuestions := NQuestionTypes;
  Answer.TabStop := True;
  Answer.ReadOnly := False;
  ApplicationReset(Task);
end;

{ Menu item "Aufgaben > Test 2 (zufällige Aufgaben Auswahl)": Set task to do test with random questions (of selected types) }

procedure TFormArithmetic3.MenuItemArithmetic3Test2Click(Sender: TObject);

begin
  Task := 'test2';
  NQuestions := Test2Questions;
  Answer.TabStop := True;
  Answer.ReadOnly := False;
  ApplicationReset(Task);
end;

{ Menu item "Aufgaben > Programm verlassen": Exit application }

procedure TFormArithmetic3.MenuItemArithmetic3ExitClick(Sender: TObject);

begin
  Close();
end;

{ Menu item "Einstellungen > Test 2 Aufgaben = N > Ändern": change number of questions for test 2 }

procedure TFormArithmetic3.MenuSettingsTQChangeClick(Sender: TObject);

// The application displays the user data input form, where the user can enter a new number of questions value. This value
// will be read into the main form when this one gets the focus again (after the OK button on the data entry form was pressed).

begin
  FormUserData.Show;
  FormUserData.T2Questions.Text := IntToStr(Test2Questions);
end;

{ Menu item "Einstellungen > Aufgaben Auswahl > Addition/Subtraktion": Select/unselect type 1 questions }

procedure TFormArithmetic3.MenuSettingsType1Click(Sender: TObject);

begin
  if MenuSettingsType1.Checked then
    MenuSettingsType1.Checked := False
  else
    MenuSettingsType1.Checked := True;
  QTypesUsedUpdate(1, MenuSettingsType1.Checked);                              // mark/unmark the questions of type 1
end;

{ Menu item "Einstellungen > Aufgaben Auswahl > Brüche (Typ 1)": Select/unselect type 2 questions }

procedure TFormArithmetic3.MenuSettingsType2Click(Sender: TObject);

begin
  if MenuSettingsType2.Checked then
    MenuSettingsType2.Checked := False
  else
    MenuSettingsType2.Checked := True;
  QTypesUsedUpdate(2, MenuSettingsType2.Checked);                              // mark/unmark the questions of type 2
end;

{ Menu item "Einstellungen > Aufgaben Auswahl > Vielfache (Typ 1)": Select/unselect type 3 questions }

procedure TFormArithmetic3.MenuSettingsType3Click(Sender: TObject);

begin
  if MenuSettingsType3.Checked then
    MenuSettingsType3.Checked := False
  else
    MenuSettingsType3.Checked := True;
  QTypesUsedUpdate(3, MenuSettingsType3.Checked);                              // mark/unmark the questions of type 3
end;

{ Menu item "Einstellungen > Aufgaben Auswahl > Brüche (Typ 2)": Select/unselect type 4 questions }

procedure TFormArithmetic3.MenuSettingsType4Click(Sender: TObject);

begin
  if MenuSettingsType4.Checked then
    MenuSettingsType4.Checked := False
  else
    MenuSettingsType4.Checked := True;
  QTypesUsedUpdate(4, MenuSettingsType4.Checked);                              // mark/unmark the questions of type 4
end;

{ Menu item "Einstellungen > Aufgaben Auswahl > Vielfache (Typ 2)": Select/unselect type 5 questions }

procedure TFormArithmetic3.MenuSettingsType5Click(Sender: TObject);

begin
  if MenuSettingsType5.Checked then
    MenuSettingsType5.Checked := False
  else
    MenuSettingsType5.Checked := True;
  QTypesUsedUpdate(5, MenuSettingsType5.Checked);                              // mark/unmark the questions of type 5
end;

{ Menu item "Extras > Beispielaufgaben Textdatei erstellen": Create text file with all question examples (including result) }

procedure TFormArithmetic3.MenuExtrasFileClick(Sender: TObject);

var
  QT, Q, Result, I: Integer;
  QText, QN, S: string;

begin
  QMemo.Clear; Q := 0; QText := ''; Result := -1;
  for QT := 1 to NQuestionTypes do begin
    if QTypesUsed[QT] = True then begin
      Inc(Q);
      QN := 'Frage ' + IntToStr(Q) + ':'; S := '';
      QuestionText(QT, QText, Result);
      QText += '  *' + IntToStr(Result) + '*' + Chr(13);
      QMemo.Append(QN);
      for I := 1 to Length(QN) do
        S += '-';
      QMemo.Append(S);
      // Format question text (132 chars per line)
      if Length(QText) <= 132 then
        QMemo.Append(QText)
      else begin
        I := 132;
        while QText[I] <> ' ' do
          Dec(I);
        S := Copy(QText, 1, I);
        QMemo.Append(S);
        S := Copy(QText, I + 1, Length(QText) - I);
        QMemo.Append(S);
      end;
    end;
  end;
  S := GetUserDir + 'Documents';
  if not DirectoryExists(S) then
    S := '';
  QSave.InitialDir := S;
  if QSave.Execute then
    QMemo.Lines.SaveToFile(QSave.Filename);
end;

{ Menu item "Hilfe > Hilfe": Display application help contents }

procedure TFormArithmetic3.MenuHelpHelpClick(Sender: TObject);

begin
  if not FormHelp.Visible then                                                 // show Help form
    FormHelp.Show;
end;

{ Menu item "Hilfe > Info": Display application information }

procedure TFormArithmetic3.MenuHelpInfoClick(Sender: TObject);

begin
  if PAbout.Visible then
    PAbout.Hide
  else
    PAbout.Show;
end;

{ Button "Frage" / "Antwort" clicked: Execute selected task / Check user answer }

procedure TFormArithmetic3.Button0Click(Sender: TObject);

var
  QType, UserResult, I: Integer;
  QText, S: string;
  Percent: Real;
  Txt: array[1..8] of string;

begin
  if Button0.Caption = 'Fertig' then begin
    ApplicationReset(Task);
    Button0.SetFocus;
  end
  else if Copy(Button0.Caption, 1, 5) = 'Frage' then begin
    // Display / generate question
    for I := 1 to 8 do
      Txt[I] := '';
    repeat
      Inc(NQuestion);
      if NQuestionUsed < NQuestions then begin
        if Task = 'test2' then
          QType := Random(NQuestionTypes) + 1
        else
          QType := NQuestion;
        if QTypesUsed[QType] = True then begin
          Inc(NQuestionUsed);
          QuestionText(QType, QText, Result);
          S := 'Frage ' + IntToStr(NQuestionUsed);
          QuestionN.Caption := S;
          Txt[1] := QText;
          if Task = 'display' then begin
            // Option Display: Also display result
            Txt[3] := 'Resultat = ' + IntToStr(Result);
            if (NQuestionUsed < NQuestions) and (NQuestion < NQuestionTypes) then
              Button0.Caption := 'Frage ' + IntToStr(NQuestionUsed + 1)
            else
              Button0.Caption := 'Fertig';
          end
          else begin
            // Option Test: Wait for answer
            Answer.Text := '';
            AnswerEval.Text := '';
            Button0.Caption := 'Antwort';
            Answer.SetFocus;
          end;
          QuestionTxt.Lines.AddStrings(Txt, True);
        end;
      end
    until (QTypesUsed[QType] = True) or (NQuestionUsed >= NQuestions) or (NQuestion >= NQuestionTypes);
  end
  else begin
    // Check answer (user result)
    if Answer.Text = '' then                                                        // avoid error message if no user entry
      UserResult := -1
    else
      UserResult := StrToInt(Answer.Text);
    if UserResult = Result then begin
      // Correct answer
      AnswerEval.Font.Color := clLime;
      AnswerEval.Text := 'Richtig!';
      Inc(CountCorrect);
      AnswersCorrect.Text := IntToStr(CountCorrect);
    end
    else begin
      // False answer
      AnswerEval.Font.Color := clRed;
      S := 'Falsch! Richtige Antwort = ' + IntToStr(Result);
      AnswerEval.Text := S;
      Inc(CountFalse);
      AnswersFalse.Text := IntToStr(CountFalse);
    end;
    Percent := 100 * (Round(100 * (CountCorrect / NQuestionUsed)) / 100);     // 2 decimal digits
    if Percent < 50 then
      AnswersPercent.Color := clRed
    else if Percent < 60 then
      AnswersPercent.Color := clYellow
    else
      AnswersPercent.Color := clLime;
    S := FloatToStr(Percent) + '%';
    AnswersPercent.Text := S;
    if (NQuestionUsed < NQuestions) and (NQuestion < NQuestionTypes) then
      Button0.Caption := 'Frage ' + IntToStr(NQuestionUsed + 1)
    else
      Button0.Caption := 'Fertig';
    Button0.SetFocus;
  end;
end;

end.

