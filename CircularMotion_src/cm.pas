{********************************************}
{* Main unit for CircularMotion application *}
{********************************************}

unit cm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls;

type
  {******}
  { TfCM }
  {******}
  TfCM = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsQuestions, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    Label7, Label8, Label9, Label10, Label11, Label12: TLabel;
    Label13, Label14, Label15, Label16, Label17, Label18: TLabel;
    Label19, Label20, Label21, Label22, Label24, Label26, Label27: TLabel;
    edRadius, edCircumference: TEdit;
    edVelocityA, edVelocityT, edPeriod, edFrequency: TEdit;
    edMass, edAcceleration, edForceC, edEnergy, edEval: TEdit;
    btExercise: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btExerciseClick(Sender: TObject);
  private
    iQuestions, iQuestions0, iQuestion, iCorrect, iCircle, iRotation: Integer;
    rR, rCirc, rO, rV, rT, rF, rM, rFC, rA, rE: Real;
  end;

var
  fCM: TfCM;

implementation

{$R *.lfm}

{ Integer power of a real number }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Format real number (F decimal digits) }

function RFormat(R: Real; F: Integer): string;

// The function removes unsignificant decimal zeroes

 var
   R0: Real;
   SR: string;

begin
  SR := '';
  if R = 0 then
    SR := '0'
  else begin
    if F >= 0 then begin
      R0 := Round(R * Power(10, F)) / Power(10, F);
      if Abs(R0) < Power(10, -F) then
        SR := FloatToStrF(R, ffExponent, F, 0)                                 // for F decimal digits, result would be 0...
      else
        SR := FloatToStr(R0);
    end;
  end;
  Result := SR;
end;

{ Check user answer for given calculation value }

procedure CheckAnswer(Value, UValue: Real; EdField: TEdit; var OK: Boolean);

begin
  if (RFormat(Value, 2) <> RFormat(UValue, 2)) then begin
    EdField.Text := RFormat(Value, 2);
    EdField.Color := clRed;
    OK := False;
  end;
end;

{******}
{ TfCM }
{******}

{ Application start: Initialization }

procedure TfCM.FormCreate(Sender: TObject);

begin
  iQuestions0 := 10;
  Randomize;
  mExerciseNew.Click;
end;

{ Menu item "Exercise > New": Prepare for new exercise }

procedure TfCM.mExerciseNewClick(Sender: TObject);

// "New" is mandatory for options to become active

begin
  iQuestions := iQuestions0;
  iQuestion := 0; iCorrect := 0;
  edRadius.Text := ''; edCircumference.Text := ''; edMass.Text := '';
  edVelocityA.Text := ''; edVelocityT.Text := ''; edPeriod.Text := ''; edFrequency.Text := '';
  edAcceleration.Text := ''; edForceC.Text := ''; edEnergy.Text := '';
  edRadius.Color := clDefault; edCircumference.Color := clDefault; edMass.Color := clDefault;
  edVelocityA.Color := clDefault; edVelocityT.Color := clDefault; edPeriod.Color := clDefault; edFrequency.Color := clDefault;
  edAcceleration.Color := clDefault; edForceC.Color := clDefault; edEnergy.Color := clDefault;
  edEval.Text := '';
  btExercise.Caption := 'Start'; btExercise.Enabled := True;
end;

{ Menu item "Exercise > Exit": Exit application }

procedure TfCM.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Number of questions": User input of number of exercise questions }

procedure TfCM.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Circular motion', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 5 then
      iQuestions0 := 5;                                                        // arbitrarly fixed minimum = 5
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfCM.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics:' + LineEnding;
  S += 'Circular motion exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September-October 2023.';
  MessageDlg('About "CircularMotion"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Question/Answer": Generate question resp. check user answer }

procedure TfCM.btExerciseClick(Sender: TObject);

var
  R, Circ, O, V, T, F, FC, A, E: Real;
  S: string;
  OK: Boolean;

begin
  // Button "Start/Question": Generate question
  if (btExercise.Caption = 'Start') or (btExercise.Caption = 'Question') then begin
    Inc(iQuestion);
    rR := Random(21) + 5; rCirc := 2 * Pi * rR;
    // Given value = radius (66,66%) or circumference
    iCircle := Random(3);
    if iCircle = 0 then begin
      rCirc := Round(rCirc);
      rR := rCirc / (2 * Pi);
    end;
    rT := Random(16) + 5; rF := 1 / rT;
    rO := 2 * Pi / rT; rV := rO * rR;
    // Given value = period, frquency, angular velocity or speed
    iRotation := Random(4);
    if iRotation = 1 then begin
      rF := Int(100 * rF) / 100;
      rT := 1 / rF;
      rO := 2 * Pi / rT; rV := rO * rR;
    end
    else if iRotation = 2 then begin
      rO := Int(100 * rO) / 100;
      rT := 2 * Pi / rO; rF := 1 / rT;
      rV := rO * rR;
    end
    else if iRotation = 3 then begin
      rV := Round(rV); rO := rV / rR;
      rT := 2 * Pi / rO; rF := 1 / rT;
    end;
    // Mass is always given
    rM := Random(25) + 1;
    // These values have always to be calculated
    rA := Sqr(rO) * rR;
    rFC := rM * Sqr(rV) / rR;
    rE := 0.5 * rM * Sqr(rV);
    // Adapt edit fields depending on given value is radius or circumference
    if iCircle > 0 then begin
      edRadius.ReadOnly := True; edRadius.TabStop := False; edRadius.Color := clCream;
      edCircumference.ReadOnly := False; edCircumference.TabStop := True; edCircumference.Color := clDefault;
      edRadius.Text := FloatToStr(rR); edCircumference.Text := '';
      edCircumference.SetFocus;
    end
    else begin
      edRadius.ReadOnly := False; edRadius.TabStop := True; edRadius.Color := clDefault;
      edCircumference.ReadOnly := True; edCircumference.TabStop := False; edCircumference.Color := clCream;
      edRadius.Text := ''; edCircumference.Text := FloatToStr(rCirc);
      edRadius.SetFocus;
    end;
    // Adapt edit fields depending on given value is period, frequency, angular velocity or speed
    edVelocityA.ReadOnly := False; edVelocityT.ReadOnly := False; edPeriod.ReadOnly := False; edFrequency.ReadOnly := False;
    edVelocityA.TabStop := True; edVelocityT.TabStop := True; edPeriod.TabStop := True; edFrequency.TabStop := True;
    edVelocityA.Color := clDefault; edVelocityT.Color := clDefault; edPeriod.Color := clDefault; edFrequency.Color := clDefault;
    edVelocityA.Text := ''; edVelocityT.Text := ''; edPeriod.Text := ''; edFrequency.Text := '';
    if iRotation = 0 then begin
      edPeriod.ReadOnly := True; edPeriod.TabStop := False; edPeriod.Color := clCream;
      edPeriod.Text := FloatToStr(rT);
    end
    else if iRotation = 1 then begin
      edFrequency.ReadOnly := True; edFrequency.TabStop := False; edFrequency.Color := clCream;
      edFrequency.Text := FloatToStr(rF);
    end
    else if iRotation = 2 then begin
      edVelocityA.ReadOnly := True; edVelocityA.TabStop := False; edVelocityA.Color := clCream;
      edVelocityA.Text := FloatToStr(rO);
    end
    else begin
      edVelocityT.ReadOnly := True; edVelocityT.TabStop := False; edVelocityT.Color := clCream;
      edVelocityT.Text := FloatToStr(rV);
    end;
    // Given mass and empty calculation fields
    edMass.Text := FloatToStr(rM); edMass.Color := clCream;
    edAcceleration.Text := ''; edForceC.Text := ''; edEnergy.Text := '';
    edAcceleration.Color := clDefault; edForceC.Color := clDefault; edEnergy.Color := clDefault;
    // Clear evaluation field
    edEval.Text := '';
    // Next button push is user answer
    btExercise.Caption := 'Answer';
  end
  // Button "Answer": Check user answer
  else begin
    // Set value of empty field to 0 (to avoid runtime error)
    R := 0; Circ := 0;
    O := 0; V := 0; T := 0; F := 0;
    FC := 0; A := 0; E := 0;
    if edRadius.Text <> '' then
      R := StrToFloat(edRadius.Text);
    if edCircumference.Text <> '' then
      Circ := StrToFloat(edCircumference.Text);
    if edPeriod.Text <> '' then
      T := StrToFloat(edPeriod.Text);
    if edFrequency.Text <> '' then
      F := StrToFloat(edFrequency.Text);
    if edVelocityA.Text <> '' then
      O := StrToFloat(edVelocityA.Text);
    if edVelocityT.Text <> '' then
      V := StrToFloat(edVelocityT.Text);
    if edForceC.Text <> '' then
      FC := StrToFloat(edForceC.Text);
    if edAcceleration.Text <> '' then
      A := StrToFloat(edAcceleration.Text);
    if edEnergy.Text <> '' then
      E := StrToFloat(edEnergy.Text);
    // Check user answers value by value (input field by input field)
    OK := True;
    CheckAnswer(rR, R, edRadius, OK);
    CheckAnswer(rCirc, Circ, edCircumference, OK);
    CheckAnswer(rT, T, edPeriod, OK);
    CheckAnswer(rF, F, edFrequency, OK);
    CheckAnswer(rO, O, edVelocityA, OK);
    CheckAnswer(rV, V, edVelocityT, OK);
    CheckAnswer(rFC, FC, edForceC, OK);
    CheckAnswer(rA, A, edAcceleration, OK);
    CheckAnswer(rE, E, edEnergy, OK);
    // Fill in evaluation field
    edEval.Text := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ': ';
    if OK then begin
      // All answers are correct
      edEval.Text := edEval.Text + 'All answers are correct.';
      Inc(iCorrect);
    end
    else begin
      // One or more answers are false
      edEval.Text := edEval.Text + 'One or more of your answers are false!';
    end;
    // If there are questions remaining, continue the exercise
    if iQuestion < iQuestions then
      btExercise.Caption := 'Question'
    // If all questions have been done, terminate the exercise
    else begin
      S := 'Exercise done. ';
      S += 'From ' + IntToStr(iQuestions) + ' questions, you answered ' + IntToStr(iCorrect) + ' correctly. ';
      S += 'Your score for this exercise: ' + RFormat(100 * iCorrect / iQuestions, 2) + '%.';
      MessageDlg('Circular Motion', S, mtInformation, [mbOK], 0);
      btExercise.Enabled := False;                                             // Disable the button (user has to choose "New")
    end;
  end;
end;

end.

