{*************************************}
{* Main unit for Pulleys application *}
{*************************************}

unit pulley;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  pulley1, pulley2, pulley3, pulley4, pulley5, pulley6, pulley7, pulley8, pulley9, pulley10, help;

type
  {***********}
  { TfPulleys }
  {***********}
  TfPulleys = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsPulleys, mOptionsG: TMenuItem;
    mOptionsG1, mOptionsG2, mOptionsG3: TMenuItem;
    mOptionsPulleys1, mOptionsPulleys2a, mOptionsPulleys2c, mOptionsPulleys2d, mOptionsPulleys3, mOptionsPulleys4: TMenuItem;
    mOptionsPulleys5, mOptionsPulleys6a, mOptionsPulleys6b, mOptionsPulleys6c, mOptionsPulleys7: TMenuItem;
    mOptionsPulleys8a, mOptionsPulleys8b, mOptionsPulleys9, mOptionsPulleys10: TMenuItem;
    MenuItem1, mOptionsPulleysAll, mOptionsPulleysNone: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    edQuestion, edPulleys: TEdit;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    btAbort: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsPulleys1Click(Sender: TObject);
    procedure mOptionsPulleys2aClick(Sender: TObject);
    procedure mOptionsPulleys2cClick(Sender: TObject);
    procedure mOptionsPulleys2dClick(Sender: TObject);
    procedure mOptionsPulleys3Click(Sender: TObject);
    procedure mOptionsPulleys4Click(Sender: TObject);
    procedure mOptionsPulleys5Click(Sender: TObject);
    procedure mOptionsPulleys6aClick(Sender: TObject);
    procedure mOptionsPulleys6bClick(Sender: TObject);
    procedure mOptionsPulleys6cClick(Sender: TObject);
    procedure mOptionsPulleys7Click(Sender: TObject);
    procedure mOptionsPulleys8aClick(Sender: TObject);
    procedure mOptionsPulleys8bClick(Sender: TObject);
    procedure mOptionsPulleys9Click(Sender: TObject);
    procedure mOptionsPulleys10Click(Sender: TObject);
    procedure mOptionsPulleysAllClick(Sender: TObject);
    procedure mOptionsPulleysNoneClick(Sender: TObject);
    procedure mOptionsG1Click(Sender: TObject);
    procedure mOptionsG2Click(Sender: TObject);
    procedure mOptionsG3Click(Sender: TObject);
    procedure HelpHelpClick(Sender: TObject);
    procedure HelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAbortClick(Sender: TObject);
  private
    iTest, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    rG: Real;
    aSelectedExercises: array[1..15] of Boolean;
  end;

var
  fPulleys: TfPulleys;

implementation

{$R *.lfm}

{***********}
{ TfPulleys }
{***********}

{ Application start: Initialisation }

procedure TfPulleys.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  Randomize;
  iQuestionsTemp := 10; iQuestions := 10;
  for I := 1 to 15 do
    aSelectedExercises[I] := True;
  aSelectedExercises[3] := False; aSelectedExercises[4] := False; aSelectedExercises[10] := False;
  rG := 9.8;
end;

{ Menu item "Test > New": Prepare for a new test }

procedure TfPulleys.mTestNewClick(Sender: TObject);

var
  N, I: Integer;

begin
  // Check if user selected at least one kind of pulley system
  N := 0;
  for I := 1 to 15 do begin
    if aSelectedExercises[I] then
      Inc(N);
  end;
  // Proceed, if ok
  if N > 0 then begin
    iQuestions := iQuestionsTemp; iQuestion := 0; iCorrect := 0;
    edQuestions.Text := IntToStr(iQuestions);
    edPulleys.Text := IntToStr(N) + '/15';
    edQuestion.Text := ''; edCorrect.Text := ''; edFalse.Text := '';
    edSuccess.Text := ''; edSuccess.Color := clDefault;
    mOptions.Enabled := False;
    btQuestion.Enabled := True;                                                // Enable button to start the test
    btAbort.Enabled := False;
  end
  else
    MessageDlg('Selection error', 'At least one pulley system has to be selected!', mtError, [mbOK], 0);
end;

{ Menu item "Test > Exit": Exit application }

procedure TfPulleys.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions ...": User choice of number of test questions }

procedure TfPulleys.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Pulley exercises', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
end;

{ Menu items "Options > Pulley system > ...": Select or unselect pulley system }

procedure TfPulleys.mOptionsPulleys1Click(Sender: TObject);

begin
  if mOptionsPulleys1.Checked then
    mOptionsPulleys1.Checked := False
  else
    mOptionsPulleys1.Checked := True;
  aSelectedExercises[1] := mOptionsPulleys1.Checked;
end;

procedure TfPulleys.mOptionsPulleys2aClick(Sender: TObject);

begin
  if mOptionsPulleys2a.Checked then
    mOptionsPulleys2a.Checked := False
  else
    mOptionsPulleys2a.Checked := True;
  aSelectedExercises[2] := mOptionsPulleys2a.Checked;
end;

procedure TfPulleys.mOptionsPulleys2cClick(Sender: TObject);

begin
  if mOptionsPulleys2c.Checked then
    mOptionsPulleys2c.Checked := False
  else
    mOptionsPulleys2c.Checked := True;
  aSelectedExercises[3] := mOptionsPulleys2c.Checked;
end;

procedure TfPulleys.mOptionsPulleys2dClick(Sender: TObject);

begin
  if mOptionsPulleys2d.Checked then
    mOptionsPulleys2d.Checked := False
  else
    mOptionsPulleys2d.Checked := True;
  aSelectedExercises[4] := mOptionsPulleys2d.Checked;
end;

procedure TfPulleys.mOptionsPulleys3Click(Sender: TObject);

begin
  if mOptionsPulleys3.Checked then
    mOptionsPulleys3.Checked := False
  else
    mOptionsPulleys3.Checked := True;
  aSelectedExercises[5] := mOptionsPulleys3.Checked;
end;

procedure TfPulleys.mOptionsPulleys4Click(Sender: TObject);

begin
  if mOptionsPulleys4.Checked then
    mOptionsPulleys4.Checked := False
  else
    mOptionsPulleys4.Checked := True;
  aSelectedExercises[7] := mOptionsPulleys4.Checked;
end;

procedure TfPulleys.mOptionsPulleys5Click(Sender: TObject);

begin
  if mOptionsPulleys5.Checked then
    mOptionsPulleys5.Checked := False
  else
    mOptionsPulleys5.Checked := True;
  aSelectedExercises[6] := mOptionsPulleys5.Checked;
end;

procedure TfPulleys.mOptionsPulleys6aClick(Sender: TObject);

begin
  if mOptionsPulleys6a.Checked then
    mOptionsPulleys6a.Checked := False
  else
    mOptionsPulleys6a.Checked := True;
  aSelectedExercises[8] := mOptionsPulleys6a.Checked;
end;

procedure TfPulleys.mOptionsPulleys6bClick(Sender: TObject);

begin
  if mOptionsPulleys6b.Checked then
    mOptionsPulleys6b.Checked := False
  else
    mOptionsPulleys6b.Checked := True;
  aSelectedExercises[9] := mOptionsPulleys6b.Checked;
end;

procedure TfPulleys.mOptionsPulleys6cClick(Sender: TObject);

begin
  if mOptionsPulleys6c.Checked then
    mOptionsPulleys6c.Checked := False
  else
    mOptionsPulleys6c.Checked := True;
  aSelectedExercises[10] := mOptionsPulleys6c.Checked;
end;

procedure TfPulleys.mOptionsPulleys7Click(Sender: TObject);

begin
  if mOptionsPulleys7.Checked then
    mOptionsPulleys7.Checked := False
  else
    mOptionsPulleys7.Checked := True;
  aSelectedExercises[11] := mOptionsPulleys7.Checked;
end;

procedure TfPulleys.mOptionsPulleys8aClick(Sender: TObject);

begin
  if mOptionsPulleys8a.Checked then
    mOptionsPulleys8a.Checked := False
  else
    mOptionsPulleys8a.Checked := True;
  aSelectedExercises[12] := mOptionsPulleys8a.Checked;
end;

procedure TfPulleys.mOptionsPulleys8bClick(Sender: TObject);

begin
  if mOptionsPulleys8b.Checked then
    mOptionsPulleys8b.Checked := False
  else
    mOptionsPulleys8b.Checked := True;
  aSelectedExercises[13] := mOptionsPulleys8b.Checked;
end;

procedure TfPulleys.mOptionsPulleys9Click(Sender: TObject);

begin
  if mOptionsPulleys9.Checked then
    mOptionsPulleys9.Checked := False
  else
    mOptionsPulleys9.Checked := True;
  aSelectedExercises[14] := mOptionsPulleys9.Checked;
end;

procedure TfPulleys.mOptionsPulleys10Click(Sender: TObject);

begin
  if mOptionsPulleys10.Checked then
    mOptionsPulleys10.Checked := False
  else
    mOptionsPulleys10.Checked := True;
  aSelectedExercises[15] := mOptionsPulleys10.Checked;
end;

{ Menu item "Options > Pulley system > Select all pulley systems": Select all available pulley systems at once }

procedure TfPulleys.mOptionsPulleysAllClick(Sender: TObject);

var
  I: Integer;

begin
  mOptionsPulleys1.Checked  := True; mOptionsPulleys2a.Checked := True; //mOptionsPulleys2c.Checked := True;
  mOptionsPulleys2d.Checked := True; mOptionsPulleys3.Checked  := True; mOptionsPulleys4.Checked  := True;
  mOptionsPulleys5.Checked  := True; mOptionsPulleys6a.Checked := True; mOptionsPulleys6b.Checked := True;
  mOptionsPulleys6c.Checked := True; mOptionsPulleys7.Checked  := True; mOptionsPulleys8a.Checked := True;
  mOptionsPulleys8b.Checked := True; mOptionsPulleys9.Checked  := True; mOptionsPulleys10.Checked := True;
  for I := 1 to 15 do
    aSelectedExercises[I] := True;
  aSelectedExercises[4] := False;                                              // not implemented...
end;

{ Menu item "Options > Pulley system > Unselect all pulley systems": Unselect all available pulley systems at once }

procedure TfPulleys.mOptionsPulleysNoneClick(Sender: TObject);

var
  I: Integer;

begin
  mOptionsPulleys1.Checked  := False; mOptionsPulleys2a.Checked := False; //mOptionsPulleys2c.Checked := False;
  mOptionsPulleys2d.Checked := False; mOptionsPulleys3.Checked  := False; mOptionsPulleys4.Checked  := False;
  mOptionsPulleys5.Checked  := False; mOptionsPulleys6a.Checked := False; mOptionsPulleys6b.Checked := False;
  mOptionsPulleys6c.Checked := False; mOptionsPulleys7.Checked  := False; mOptionsPulleys8a.Checked := False;
  mOptionsPulleys8b.Checked := False; mOptionsPulleys9.Checked  := False; mOptionsPulleys10.Checked := False;
  for I := 1 to 15 do
    aSelectedExercises[I] := False;
end;

{ Menu items "Options > Acceleraton due to gravity > ...": Choose one of 3 possible values for g }

procedure TfPulleys.mOptionsG1Click(Sender: TObject);

begin
  mOptionsG1.Checked := True;  mOptionsG2.Checked := False;  mOptionsG3.Checked := False;
  rG := 9.8;
end;

procedure TfPulleys.mOptionsG2Click(Sender: TObject);

begin
  mOptionsG1.Checked := False;  mOptionsG2.Checked := True;  mOptionsG3.Checked := False;
  rG := 9.81;
end;

procedure TfPulleys.mOptionsG3Click(Sender: TObject);

begin
  mOptionsG1.Checked := False;  mOptionsG2.Checked := False;  mOptionsG3.Checked := True;
  rG := 10;
end;

{ Menu item "Help > Help": Dusplay application help text }

procedure TfPulleys.HelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Dusplay application about }

procedure TfPulleys.HelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics (dynamics) trainer: Pulleys.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March-April 2020.';
  MessageDlg('About "Pulleys"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next": Choose a random kind of pulley system and open corresponding exercise window; update evaluation counters when exercise is done }

procedure TfPulleys.btQuestionClick(Sender: TObject);

var
  Success: Integer;
  Correct: Boolean;

begin
  Inc(iQuestion);
  repeat
    iTest := Random(15) + 1;
  until aSelectedExercises[iTest];                                             // random, but has to be one of those, that user has selected
  // Set variables on exercise form; open the window; get exercise evaluation (correct, false) when exercise windows is closed
  case iTest of
         1: begin
              fPulleys1.iQuestion := iQuestion; fPulleys1.iQuestions := iQuestions; fPulleys1.rG := rG;
              fPulleys1.ShowModal;
              Correct := fPulleys1.bCorrect;
            end;
      2..4: begin
              fPulleys2.iQuestion := iQuestion; fPulleys2.iQuestions := iQuestions; fPulleys2.iCase := iTest - 2; fPulleys2.rG := rG;
              fPulleys2.ShowModal;
              Correct := fPulleys2.bCorrect;
            end;
         5: begin
              fPulleys3.iQuestion := iQuestion; fPulleys3.iQuestions := iQuestions; fPulleys3.rG := rG;
              fPulleys3.ShowModal;
              Correct := fPulleys3.bCorrect;
            end;
         6: begin
              fPulleys5.iQuestion := iQuestion; fPulleys5.iQuestions := iQuestions; fPulleys5.rG := rG;
              fPulleys5.ShowModal;
              Correct := fPulleys5.bCorrect;
            end;
         7: begin
              fPulleys4.iQuestion := iQuestion; fPulleys4.iQuestions := iQuestions; fPulleys4.rG := rG;
              fPulleys4.ShowModal;
              Correct := fPulleys4.bCorrect;
            end;
     8..10: begin
              fPulleys6.iQuestion := iQuestion; fPulleys6.iQuestions := iQuestions; fPulleys6.iCase := iTest - 8; fPulleys6.rG := rG;
              fPulleys6.ShowModal;
              Correct := fPulleys6.bCorrect;
            end;
        11: begin
              fPulleys7.iQuestion := iQuestion; fPulleys7.iQuestions := iQuestions; fPulleys7.rG := rG;
              fPulleys7.ShowModal;
              Correct := fPulleys7.bCorrect;
            end;
    12..13: begin
              fPulleys8.iQuestion := iQuestion; fPulleys8.iQuestions := iQuestions; fPulleys8.iCase := iTest - 12; fPulleys8.rG := rG;
              fPulleys8.ShowModal;
              Correct := fPulleys8.bCorrect;
            end;
        14: begin
              fPulleys9.iQuestion := iQuestion; fPulleys9.iQuestions := iQuestions; fPulleys9.rG := rG;
              fPulleys9.ShowModal;
              Correct := fPulleys9.bCorrect;
            end;
        15: begin
              fPulleys10.iQuestion := iQuestion; fPulleys10.iQuestions := iQuestions; fPulleys10.rG := rG;
              fPulleys10.ShowModal;
              Correct := fPulleys10.bCorrect;
            end;
  end;
  // Fill in evaluation counters, depending on "return value" from exercise form
  edQuestion.Text := IntToStr(iQuestion);
  if Correct then
    Inc(iCorrect);
  edCorrect.Text := IntToStr(iCorrect);
  edFalse.Text := IntToStr(iQuestion - iCorrect);
  Success := Round(100 * (iCorrect / iQuestion));
  edSuccess.Text:= IntToStr(Success) + '%';
  // Different colors, depending on success percentage
  if Success >= 60 then
    edSuccess.Color := clLime
  else if Success >= 50 then
    edSuccess.Color := clYellow
  else
    edSuccess.Color := clRed;
  // All questions done: End of test
  if iQuestion = iQuestions then begin
    btQuestion.Caption := 'Start'; btQuestion.Enabled := False; btAbort.Enabled := False;
    mOptions.Enabled := True;                                                  // re-enable "Options" menu!
    MessageDlg('Pulley exercises', 'All questions have been done. End of test.', mtInformation, [mbOK], 0);
  end
  // Still questions left: Continue...
  else begin
    btQuestion.Caption := 'Next'; btAbort.Enabled := True;
  end;
end;

{ Button "Abort": Terminate current test }

procedure TfPulleys.btAbortClick(Sender: TObject);

begin
  if btQuestion.Caption <> 'Start' then
    MessageDlg('Pulley exercises', 'Test aborted by user...', mtInformation, [mbOK], 0);
  mOptions.Enabled := True;                                                    // allows user to set options, before starting a new test
  btQuestion.Enabled := False; btQuestion.Caption := 'Start'; btAbort.Enabled := False;
end;

end.

