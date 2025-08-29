{***************************************}
{* Main unit for Converter application *}
{***************************************}

unit convert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, PopupNotifier;

type
  { TfConverter }
  TfConverter = class(TForm)
    mMenu: TMainMenu;
    mTest, mTestNew, mTestExit: TMenuItem;
    mSettings, mSettingsExercises: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Memo1: TMemo;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    cobMeasure: TComboBox;
    laExercise: TLabel;
    edValue1: TEdit;
    edUnit1: TEdit;
    edValue2: TEdit;
    edUnit2: TEdit;
    edEval: TEdit;
    edQuestions: TEdit;
    edCorrect: TEdit;
    edFalse: TEdit;
    edSuccess: TEdit;
    btQuestion: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSettingsExercisesClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure cobMeasureChange(Sender: TObject);
  private
    iSQuestions, iSMeasure, iQuestions, iMeasure, iQuestion, iCorrect, iFalse: Integer;
    rValue2: Double;
  end;

var
  fConverter: TfConverter;

implementation

{ Prepare for a new test: Clear all }

procedure NewTest(SQuestions, SMeasure: Integer; out Questions, Measure, Question, QCorrect, QFalse: Integer);

begin
  Questions := SQuestions; Measure := SMeasure;                                          // selections made, now become active
  fConverter.laExercise.Caption := 'Aufgabe';
  fConverter.edValue1.Text := ''; fConverter.edValue2.Text := '';
  fConverter.edUnit1.Text := '';  fConverter.edUnit2.Text := '';
  fConverter.edEval.Text := ''; fConverter.edEval.Color := clCream;
  fConverter.edCorrect.Text := ''; fConverter.edFalse.Text := '';
  fConverter.edSuccess.Text := ''; fConverter.edSuccess.Color := clCream;
  fConverter.btQuestion.Caption := 'Frage'; fConverter.btQuestion.Enabled := True;
  Question := 0; QCorrect := 0; QFalse := 0;
end;

{$R *.lfm}

{***************}
{* TfConverter *}
{***************}

{ Application start: Initialisation }

procedure TfConverter.FormCreate(Sender: TObject);

begin
  iSQuestions := 10; iSMeasure := 0;
  NewTest(iSQuestions, iSMeasure, iQuestions, iMeasure, iQuestion, iCorrect, iFalse);    // prepare for new test
  Randomize;
end;

{ Menu item "Test > Neuer Test": Prepare for a new test }

procedure TfConverter.mTestNewClick(Sender: TObject);

begin
  NewTest(iSQuestions, iSMeasure, iQuestions, iMeasure, iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > Verlassen": Exit application }

procedure TfConverter.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Anzahl der Aufgaben": Choose number of exercises }

procedure TfConverter.mSettingsExercisesClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Rechentrainer: Maßeinheiten', 'Anzahl der Aufgaben', IntToStr(iQuestions));
  if S <> '' then
    iSQuestions := StrToInt(S);
end;

{ Menu item "Hilfe > Über": Display program about }

procedure TfConverter.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'Rechentrainer für Primärschüler mit Aufgaben zum Thema "Maßeinheiten Umrechnung": ';
    S += 'Längen, Flächen, Volumen, Inhalte, Gewichte.' + Chr(13) + Chr(13);
    S += '© allu, September, 2018.';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Frage/Anwort" pressed: Generate new exercise, resp. check pupil's answer }

procedure TfConverter.btQuestionClick(Sender: TObject);

const
  // Superscripts for square and cube units
  SUP_2 = #$C2#$B2;
  SUP_3 = #$C2#$B3;
  // Number of units for the different exercises
  NMeasures: array[0..5] of Integer = (
    5, 6, 4, 6, 7, 6
  );
  // Measurement units for the different exercises
  Measures: array[0..5, 0..6] of string = (
    ('m', 'mm', 'cm', 'dm', 'km', '', ''),
    ('m2', 'mm2', 'cm2', 'dm2', 'km2', 'ha', ''),
    ('m3', 'mm3', 'cm3', 'dm3', '', '', ''),
    ('l', 'ml', 'cl', 'dl', 'hl', 'Fuder', ''),
    ('l', 'dl', 'cl', 'ml', 'mm3', 'cm3', 'dm3'),
    ('kg', 'g', 'mg', 't', 'Ztr', 'DZtr', '')
  );
  // Measurement unit conversions: for the different exercises,
  // conversion of 1st unit mentionned to each of the others
  Conversions: array[0..5, 0..6] of Double = (
    (1, 1000, 100, 10, 0.001, 0, 0),
    (1, 1E+6, 1E+4, 100, 1E-6, 1E-4, 0),
    (1, 1E+9, 1E+6, 1000, 0, 0, 0),
    (1, 1000, 100, 10, 0.01, 0.001, 0),
    (1, 10, 100, 1000, 1E+6, 1000, 1),
    (1, 1000, 1E+6, 0.001, 0.02, 0.01, 0)
  );

var
  UX1, UX2, V, Success: Integer;
  Conv1, Conv2, Value1, UValue2, Round1, Round2: Double;
  Unit1, Unit2: string;
  OK: Boolean;

begin
  // Button "Frage": Generate a new exercise
  // ---------------------------------------
  if btQuestion.Caption = 'Frage' then begin
    Inc(iQuestion);
    laExercise.Caption := 'Aufgabe ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    // Get 2 distinct random measurement units, with arbitrar condition
    // that the one is less than 1e+6 times bigger/smaller than the other
    repeat
      OK := True;
      UX1 := Random(NMeasures[iMeasure]); UX2 := Random(NMeasures[iMeasure]);
      if UX1 = UX2 then
        OK := False
      else begin
        Conv1 := Conversions[iMeasure, UX1]; Conv2 := Conversions[iMeasure, UX2];
        if (Conv2 < 1E-6 * Conv1) or (Conv2 > 1E+6 * Conv1) then
          OK := False
        else begin
          Unit1 := Measures[iMeasure, UX1]; Unit2 := Measures[iMeasure, UX2];
        end;
      end;
    until OK;
    // Get random value, but arbitrarily choose what digits may appear after the decimal point
    V := Random(6);
    case V of
      0: Value1 := (Random(9) + 1) / 1000;
      1: Value1 := Random(10) / 100 + (Random(2) + 1) * 5 / 1000;
      2: Value1 := Random(10) / 10 + (Random(4) + 1) * 2.5 / 100;
      3: Value1 := Random(10) + 1 + Random(10) / 10 + (Random(4) + 1) * 2.5 / 100;
      4: Value1 := Random(90) + 10 + Random(10) / 10 + (Random(4) + 1) * 2.5 / 100;
      5: Value1 := Random(900) + 100 + Random(10) / 10 + (Random(2) + 1) * 5 / 100;
    end;
    // Determine at which decimal position the value must be rounded
    if V = 5 then
      Round1 := 100
    else
      Round1 := 1000;
    // Calculate the 2nd (= the converted) value
    rValue2 := Value1 * (Conv2 / Conv1);
    // Determine at which decimal position the converted value must be rounded
    Round2 := Round1 / (Conv2 / Conv1);
    // If one value is more than 1e+6 times bigger/smaller than the other,
    // arbitrarily use bigger values (to avoid "to small" or "to big" numbers )
    if rValue2 < 1E-6 then begin
      Value1 *= 1000; RValue2 *= 1000;
      Round1 := Round1 / 1000; Round2 := Round2 / 1000;                                  // adapt the position where the values must be rounded
    end
    else if rValue2 > 1E+6 then begin
      Value1 /= 1000; rValue2 /= 1000;
      Round1 := Round1 * 1000; Round2 := Round2 * 1000;                                  // adapt the position where the values must be rounded
    end;
    // Round the values; this is important to avoid erroneous digits after the
    // decimal point (and also to get a proper display)
    Value1  := Round(Round1 * Value1) / Round1;
    rValue2 := Round(Round2 * rValue2) / Round2;
    // Display the exercise question
    edValue1.Text := FloatToStr(Value1);
    edValue2.Text := '';
    edEval.Text := ''; edEval.Color := clCream;
    Unit1 := StringReplace(Unit1, '2', SUP_2, []); Unit2 := StringReplace(Unit2, '2', SUP_2, []);
    Unit1 := StringReplace(Unit1, '3', SUP_3, []); Unit2 := StringReplace(Unit2, '3', SUP_3, []);
    edUnit1.Text := Unit1; edUnit2.Text := Unit2;
    edValue2.SetFocus;
    // Next button push will be to check pupil's answer
    btQuestion.Caption := 'Antwort';
  end
  // Button "Anwort": Check pupil's answer
  // -------------------------------------
  else begin
    edQuestions.Text := IntToStr(iQuestion);
    // Get pupil's answer from form
    if edValue2.Text = '' then
      UValue2 := 0
    else
      UValue2 := StrToFloat(edValue2.Text);
    // Pupil's answer is correct
    if UValue2 = rValue2 then begin
      Inc(iCorrect);
      edEval.Text := 'Richtig!';
      edEval.Color := clCream;
    end
    // Pupil's answer is false
    else begin
      Inc(iFalse);
      edEval.Text := 'Falsch! Richtig = ' + FloatToStr(rValue2);
      edEval.Color := clRed;
    end;
    // Update evaluation data
    edCorrect.Text := IntToStr(iCorrect); edFalse.Text := IntToStr(iFalse);
    Success := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Success) + '%';
    if Success >= 60 then
      edSuccess.Color := clLime
    else if Success >= 50 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clRed;
    // Next button push is to generate a new question
    btQuestion.Caption := 'Frage';
    // All questions done: Display message (and disable button, until "Neuer Test" has been clicked )
    if iQuestion = iQuestions then begin
      MessageDlg('Ende des Tests', 'Um einen anderen Test zu starten, bitte, "Neuer Test" im Menü "Test" verwenden.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Selection of exercise type (actual measures) }

procedure TfConverter.cobMeasureChange(Sender: TObject);

begin
  iSMeasure := cobMeasure.ItemIndex;
end;

end.

