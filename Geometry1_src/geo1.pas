{*******************************************}
{* Main unit for the Geometry1 application *}
{*******************************************}

unit geo1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  StdCtrls, ExtCtrls, PopupNotifier, help;

type
  { TfGeo1 }
  TfGeo1 = class(TForm)
    mGeo1: TMainMenu;
    mTest: TMenuItem;
    mTestNew: TMenuItem;
    mTestExit: TMenuItem;
    mSettings: TMenuItem;
    mSetExercises: TMenuItem;
    mSetExercArea: TMenuItem;
    mSetExercCirc: TMenuItem;
    mSetLevel: TMenuItem;
    mSetLevel1: TMenuItem;
    mSetLevel2: TMenuItem;
    mSetLevel3: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    StaticText2: TStaticText;
    stSurface: TStaticText;
    imSurface: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    laV1: TLabel; edV1: TEdit;
    laV2: TLabel; edV2: TEdit;
    laV3: TLabel; edV3: TEdit;
    laV4: TLabel; edV4: TEdit;
    edQuestion: TEdit;
    edCorrect: TEdit;
    edFalse: TEdit;
    edSuccess: TEdit;
    laExercise: TLabel; edAnswer: TEdit;
    edEvaluation: TEdit;
    StaticText1: TStaticText;
    btQuestion: TButton;
    btAnswer: TButton;
    Popup: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mTestNewClick(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mSetExercAreaClick(Sender: TObject);
    procedure mSetExercCircClick(Sender: TObject);
    procedure mSetLevel1Click(Sender: TObject);
    procedure mSetLevel2Click(Sender: TObject);
    procedure mSetLevel3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
  private
    iSurfaces, iSurface, iQuestion, iCorrect, iFalse: Integer;
    sExercise: string;
    rAnswer, rUAnswer: Real;
    bPythagoras, bNoAnswer: Boolean;
  end;

const
  Pi = 3.14159265;
  Surfaces: array[1..10] of string = (
    'Quadrat', 'Rechteck', 'Dreieck', 'Parallelogramm', 'Trapez', 'Keis',
    'Gleichseitiges Dreieck', 'Gleichschenkliges Dreieck', 'Gleichschenkliges Trapez', 'Kreissegment'
  );

var
  fGeo1: TfGeo1;

implementation

{$R *.lfm}

{ New test: Reset form fields and variables }

procedure NewTest(var Q, C, F: Integer);

begin
  fGeo1.stSurface.Caption := '';
  fGeo1.imSurface.Picture.Clear;
  fGeo1.laV1.Visible := False; fGeo1.edV1.Visible := False;
  fGeo1.laV2.Visible := False; fGeo1.edV2.Visible := False;
  fGeo1.laV3.Visible := False; fGeo1.edV3.Visible := False;
  fGeo1.laV4.Visible := False; fGeo1.edV4.Visible := False;
  fGeo1.edQuestion.Text := ''; Q := 0;
  fGeo1.edCorrect.Text := '';  C := 0;
  fGeo1.edFalse.Text := '';    F := 0;
  fGeo1.edSuccess.Text := '';
  fGeo1.edSuccess.Color := clForm;
  fGeo1.edAnswer.Text := '';
  fGeo1.edEvaluation.Text := '';
  fGeo1.bNoAnswer := False;
  fGeo1.btAnswer.Enabled := False;
end;

{ Colored display of success (percentage of correct answers) }

procedure DisplaySuccess(Q, C: Integer);

var
  P: Real;

begin
  P := 100 * (C / Q);
  P := Round(100 * P) / 100;                                                   // rounded to 2 fractional digits
  fGeo1.edSuccess.Text := FloatToStr(P) + '%';
  if P < 50 then
    fGeo1.edSuccess.Color := clRed
  else if P < 60 then
    fGeo1.edSuccess.Color := clYellow
  else
    fGeo1.edSuccess.Color := clLime;
end;

{ Questions for surface = square }

procedure SquareQuestion(Pyth: Boolean; var A: Real);

var
  QType: Integer;
  D: Real;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  if QType = 1 then begin
    A := Random(10) + 1;                                                       // a is given
    fGeo1.laV1.Caption := 'a';
    fGeo1.edV1.Text := FloatToStr(A);
  end
  else begin
    D := Random(8) + 3;                                                        // a must be calculated from d
    fGeo1.laV1.Caption := 'd';
    fGeo1.edV1.Text := FloatToStr(D);
    A := D / Sqrt(2);
  end;
end;

{ Questions for surface = rectangle }

procedure RectangleQuestion(Pyth: Boolean; var A, B: Real);

var
  QType: Integer;
  D: Real;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(3) + 1;
  if QType = 1 then begin
    repeat
      A := Random(10) + 1;                                                     // a and b are given
      B := Random(10) + 1;
    until A > B;
    fGeo1.laV1.Caption := 'a';
    fGeo1.edV1.Text := FloatToStr(A);
    fGeo1.laV2.Caption := 'b';
    fGeo1.edV2.Text := FloatToStr(B);
  end
  else begin
    repeat
      D := Random(6) + 5;
      if QType = 2 then begin
        A := Random(Round(D) - 1) + 1;                                         // a is given
        B := Sqrt(Sqr(D) - Sqr(A));                                            // b must be calculated from d
      end
      else begin
        B := Random(Round(D) - 1) + 1;                                         // b is given
        A := Sqrt(Sqr(D) - Sqr(B));                                            // a must be calculated from d
      end;
    until A > B;
    if QType = 2 then begin
      fGeo1.laV1.Caption := 'a';
      fGeo1.edV1.Text := FloatToStr(A);
    end
    else begin
      fGeo1.laV1.Caption := 'b';
      fGeo1.edV1.Text := FloatToStr(B);
    end;
    fGeo1.laV2.Caption := 'd';
    fGeo1.edV2.Text := FloatToStr(D);
  end;
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
end;

{ Questions for surface = triangle }

procedure TriangleQuestion(Ex: string; var A, B, C, H: Real);

begin
  if Ex = 'circ' then begin
    repeat
      A := Random(10) + 1;                                                     // circumference: a, b and c are given
      B := Random(10) + 1;
      C := Random(10) + 1;
    until (C > A) and (C > B) and (A <> B);
    H := 0;
    fGeo1.laV1.Caption := 'a';
    fGeo1.edV1.Text := FloatToStr(A);
    fGeo1.laV2.Caption := 'b';
    fGeo1.edV2.Text := FloatToStr(B);
    fGeo1.laV3.Caption := 'c';
    fGeo1.edV3.Text := FloatToStr(C);
  end
  else begin
    repeat
      C := Random(10) + 1;                                                     // area: c and h are given
      H := Random(10) + 1;
    until H < C;
    A := 0; B := 0;
    fGeo1.laV1.Caption := 'c';
    fGeo1.edV1.Text := FloatToStr(C);
    fGeo1.laV2.Caption := 'h';
    fGeo1.edV2.Text := FloatToStr(H);
  end;
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
  if Ex = 'circ' then begin
    fGeo1.laV3.Visible := True; fGeo1.edV3.Visible := True;
  end;
end;

{ Questions for surface = parallelogram }

procedure ParallelogramQuestion(Ex: String; var A, B, H: Real);

begin
  if Ex = 'circ' then begin
    repeat
      A := Random(10) + 1;                                                     // circumference: a and b are given
      B := Random(10) + 1;
    until B < A;
    H := 0;
    fGeo1.laV2.Caption := 'b';
    fGeo1.edV2.Text := FloatToStr(B);
  end
  else begin
    repeat
      A := Random(10) + 1;                                                     // area: a and h are given
      H := Random(10) + 1;
    until H < A;
    B := 0;
    fGeo1.laV2.Caption := 'h';
    fGeo1.edV2.Text := FloatToStr(H);
  end;
  fGeo1.laV1.Caption := 'a';
  fGeo1.edV1.Text := FloatToStr(A);
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
end;

{ Questions for surface = trapezoid }

procedure TrapezoidQuestion(Ex: string; var A, B, C, D, H: Real);

begin
  if Ex = 'circ' then begin
    repeat
      A := Random(10) + 1;                                                     // circumference: a, b, c and d are given
      B := Random(10) + 1;
      C := Random(10) + 1;
      D := Random(10) + 1;
    until (A > B) and (A > C) and (A > D) and (B <> D);;
    H := 0;
    fGeo1.laV2.Caption := 'b';
    fGeo1.edV2.Text := FloatToStr(B);
    fGeo1.laV3.Caption := 'c';
    fGeo1.edV3.Text := FloatToStr(C);
    fGeo1.laV4.Caption := 'd';
    fGeo1.edV4.Text := FloatToStr(D);
  end
  else begin
    repeat
      A := Random(10) + 1;                                                     // area: a, c and h are given
      C := Random(10) + 1;
      H := Random(10) + 1;
    until (A > C) and (A > H);
    B := 0; D := 0;
    fGeo1.laV2.Caption := 'c';
    fGeo1.edV2.Text := FloatToStr(C);
    fGeo1.laV3.Caption := 'h';
    fGeo1.edV3.Text := FloatToStr(H);
  end;
  fGeo1.laV1.Caption := 'a';
  fGeo1.edV1.Text := FloatToStr(A);
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
  fGeo1.laV3.Visible := True; fGeo1.edV3.Visible := True;
  if Ex = 'circ' then begin
    fGeo1.laV4.Visible := True; fGeo1.edV4.Visible := True;
  end;
end;

{ Questions for surface = circle }

procedure CircleQuestion(var R: Real);

var
  QType, D: Real;

begin
  QType := Random(2) + 1;
  if QType = 1 then begin
    R := Random(10) + 1;                                                       // r is given
    fGeo1.laV1.Caption := 'r';
    fGeo1.edV1.Text := FloatToStr(R);
  end
  else begin
    D := Random(10) + 1;                                                       // d is given
    R := D / 2;
    fGeo1.laV1.Caption := 'd';
    fGeo1.edV1.Text := FloatToStr(D);
  end;
end;

{ Questions for surface = equilateral triangle }

procedure Triangle2Question(Pyth: Boolean; var A, H: Real);

var
  QType: Integer;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  A := Random(10) + 1;                                                         // a is always given
  H := Sqrt(3) * A / 2;                                                        // h is given or must be calculated from a
  H := Round(1000 * H) / 1000;
  fGeo1.laV1.Caption := 'a';
  fGeo1.edV1.Text := FloatToStr(A);
  if QType = 1 then begin
    fGeo1.laV2.Caption := 'h';
    fGeo1.edV2.Text := FloatToStr(H);
    fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
  end;
end;

{ Questions for surface = isosceles triangle }

procedure Triangle3Question(Ex: string; Pyth: Boolean; var A, S, H: Real);

var
  QType: Integer;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  if ((QType = 1) and (Ex = 'circ')) or ((QType = 2) and (Ex = 'area')) then begin
    repeat
      A := Random(10) + 1;                                                     // a and s are given
      S := Random(10) + 1;
    until (A < S);
    H := Sqrt(Sqr(S) - Sqr(A / 2));                                            // for area: h is given or must be calculated
    fGeo1.laV2.Caption := 's';
    fGeo1.edV2.Text := FloatToStr(S);
  end
  else begin
    repeat
      A := Random(10) + 1;                                                     // a and h are given
      H := Random(10) + 1;
    until (A < H);
    S := Sqrt(Sqr(H) + Sqr(A / 2));                                            // for circumference: s is given or must be calculated
    fGeo1.laV2.Caption := 'h';
    fGeo1.edV2.Text := FloatToStr(H);
  end;
  fGeo1.laV1.Caption := 'a';
  fGeo1.edV1.Text := FloatToStr(A);
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
end;

{ Questions for surface = isosceles trapezoid }

procedure Trapezoid2Question(Ex: string; Pyth: Boolean; var A, B, C, H: Real);

var
  QType: Integer;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  if ((QType = 1) and (Ex = 'circ')) or ((QType = 2) and (Ex = 'area')) then begin
    repeat
      A := Random(10) + 1;                                                     // a, b and c are given
      C := Random(10) + 1;
      B := Random(10) + 1;
    until (A > C) and (A > B) and (B > (A - C) / 2);
    H := Sqrt(Sqr(B) - Sqr((A - C) / 2));                                      // for area: h is given or must be calculated
    fGeo1.laV2.Caption := 'b';
    fGeo1.edV2.Text := FloatToStr(B);
    fGeo1.laV3.Caption := 'c';
    fGeo1.edV3.Text := FloatToStr(C);
  end
  else begin
    repeat
      A := Random(10) + 1;                                                     // a, c and h are given
      C := Random(10) + 1;
      H := Random(10) + 1;
    until (A > C) and (A > H);
    B := Sqrt(Sqr(H) + Sqr((A - C) / 2));                                      // for circumference: b is given or must be calculated
    fGeo1.laV2.Caption := 'c';
    fGeo1.edV2.Text := FloatToStr(C);
    fGeo1.laV3.Caption := 'h';
    fGeo1.edV3.Text := FloatToStr(H);
  end;
  fGeo1.laV1.Caption := 'a';
  fGeo1.edV1.Text := FloatToStr(A);
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
  fGeo1.laV3.Visible := True; fGeo1.edV3.Visible := True;
end;

{ Questions for surface = sector }

procedure SectorQuestion(var R, Alpha, B: Real);

var
  QType: Integer;

begin
  QType := Random(3) + 1;
  if QType = 1 then begin
    R := Random(10) + 1;                                                       // r and α are given
    Alpha := Random(81) + 5;
    B := R * Pi * Alpha / 180;
    fGeo1.laV1.Caption := 'r';
    fGeo1.edV1.Text := FloatToStr(R);
    fGeo1.laV2.Caption := 'α';
    fGeo1.edV2.Text := FloatToStr(Alpha);
  end
  else if QType = 2 then begin
    repeat
      R := Random(10) + 1;                                                     // r and b are given
      B := Random(5) + 1;
      Alpha := (180 * B) / (R * Pi);
    until (Alpha >= 5) and (Alpha <= 85);
    fGeo1.laV1.Caption := 'r';
    fGeo1.edV1.Text := FloatToStr(R);
    fGeo1.laV2.Caption := 'b';
    fGeo1.edV2.Text := FloatToStr(B);
  end
  else begin
    B := Random(5) + 1;                                                        // b and α are given
    Alpha := Random(81) + 5;
    R := (180 * B) / (Pi * Alpha);
    fGeo1.laV1.Caption := 'b';
    fGeo1.edV1.Text := FloatToStr(B);
    fGeo1.laV2.Caption := 'α';
    fGeo1.edV2.Text := FloatToStr(Alpha);
  end;
  fGeo1.laV2.Visible := True; fGeo1.edV2.Visible := True;
end;

{ --------------------------------------- }
{ Functions to calculate the surface area }
{ --------------------------------------- }

function SquareArea(A: Real): Real;

begin
  SquareArea := Sqr(A);
end;

function RectangleArea(A, B: Real): Real;

begin
  RectangleArea := A * B;
end;

function TriangleArea(C, H: Real): Real;

begin
  TriangleArea := (C * H) / 2;
end;

function ParallelogramArea(G, H: Real): Real;

begin
  ParallelogramArea := G * H;
end;

function TrapezoidArea(A, C, H: Real): Real;

begin
  TrapezoidArea := ((A + C) * H) / 2;
end;

function CircleArea(R: Real): Real;

begin
  CircleArea := Pi * Sqr(R);
end;

function SectorArea(R, Alpha: Real): Real;

begin
  SectorArea := Pi * Sqr(R) * (Alpha / 360);
end;

{ ------------------------------------------------ }
{ Functions to calculate the surface circumference }
{ ------------------------------------------------ }

function SquareCircumference(A: Real): Real;

begin
  SquareCircumference := 4 * A;
end;

function RectangleCircumference(A, B: Real): Real;

begin
  RectangleCircumference := 2 * (A + B);
end;

function TriangleCircumference(A, B, C: Real): Real;

begin
  TriangleCircumference := A + B + C;
end;

function ParallelogramCircumference(A, B: Real): Real;

begin
  ParallelogramCircumference := 2 * (A + B);
end;

function TrapezoidCircumference(A, B, C, D: Real): Real;

begin
  TrapezoidCircumference := A + B + C + D;
end;

function CircleCircumference(R: Real): Real;

begin
  CircleCircumference := 2 * Pi * R;
end;

function SectorCircumference(R, B: Real): Real;

begin
  SectorCircumference := 2 * R + B;
end;

{**********}
{* TfGeo1 *}
{**********}

{ Application start: Set default values and reset variables and form controls }

procedure TfGeo1.FormCreate(Sender: TObject);

begin
  sExercise := 'area'; iSurfaces := 10; bPythagoras := False;
  NewTest(iQuestion, iCorrect, iFalse);
  Randomize;
end;

{ Menu item "Test > Neu": Start new test }

procedure TfGeo1.mTestNewClick(Sender: TObject);

begin
  NewTest(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > Verlassen": Exit the application }

procedure TfGeo1.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Berechnung > Flächeninhalt": Exercises = area calculations }

procedure TfGeo1.mSetExercAreaClick(Sender: TObject);

begin
  if not mSetExercArea.Checked then begin
    mSetExercArea.Checked := True;
    mSetExercCirc.Checked := False;
    sExercise := 'area';
    laExercise.Caption := 'Flächeninhalt';
  end;
end;

{ Menu item "Einstellungen > Berechnung > Flächenumfang": Exercises = circumference calculations }

procedure TfGeo1.mSetExercCircClick(Sender: TObject);

begin
  if not mSetExercCirc.Checked then begin
    mSetExercCirc.Checked := True;
    mSetExercArea.Checked := False;
    sExercise := 'circ';
    laExercise.Caption := 'Flächenumfang';
  end;
end;

{ Menu item "Einstellungen > Aufgaben > Grundflächen": Exercises = 6 basic surfaces }

procedure TfGeo1.mSetLevel1Click(Sender: TObject);

begin
  if not mSetLevel1.Checked then begin
    mSetLevel1.Checked := True;
    mSetLevel2.Checked := False;
    mSetLevel3.Checked := False;
    iSurfaces := 6;
    bPythagoras := False;
  end;
end;

{ Menu item "Einstellungen > Aufgaben > Alle Flächen": Exercises = all 10 surfaces }

procedure TfGeo1.mSetLevel2Click(Sender: TObject);

begin
  if not mSetLevel2.Checked then begin
    mSetLevel2.Checked := True;
    mSetLevel1.Checked := False;
    mSetLevel3.Checked := False;
    iSurfaces := 10;
    bPythagoras := False;
  end;
end;

{ Menu item "Einstellungen > Aufgaben > Pythagoras": Exercises = all surfaces with Pythagoras required questions enabled }

procedure TfGeo1.mSetLevel3Click(Sender: TObject);

begin
  if not mSetLevel3.Checked then begin
    mSetLevel3.Checked := True;
    mSetLevel1.Checked := False;
    mSetLevel2.Checked := False;
    iSurfaces := 10;
    bPythagoras := True;
  end;
end;

{ Menu item "Hilfe > Hilfe": Display Help text }

procedure TfGeo1.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;                                                                // dislay help as a second form
end;

{ Menu item "Hilfe > Über": Display About text }

procedure TfGeo1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if Popup.Visible then
      Popup.Visible := False
  else begin
    S := 'Erstellen von Mathematik-Aufgaben zur Berechnung von Inhalt und Umfang ';
    S += 'von geometrischen Flächen.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, Februar, 2018.';
    Popup.Title := 'Geometrie';
    Popup.Text := S;
    Popup.Visible := True;                                                     // display about as popup notifier
  end;
end;

{ Button "Frage": Generate a new question }

procedure TfGeo1.btQuestionClick(Sender: TObject);

var
  A, B, C, D, S, H, R, Alpha: Real;
  Filename: string;

begin
  A := 0; B := 0; C := 0; D := 0; S := 0; H := 0; R := 0; Alpha := 0;
  edAnswer.Text := ''; edEvaluation.Text := '';
  // First check is there was an unanswered question before. If so, increment number of false answers
  if bNoAnswer then begin
    Inc(iFalse);
    edFalse.Text := IntToStr(iFalse);
    DisplaySuccess(iQuestion, iCorrect);
  end;
  // New question generation
  Inc(iQuestion); edQuestion.Text := IntToStr(iQuestion);
  iSurface := Random(iSurfaces) + 1;                                           // a random surface from the 6 resp. 10 available
  stSurface.Caption := Surfaces[iSurface];
  Filename := 'surfaces/s' + IntToStr(iSurface) + '.jpg';
  DoDirSeparators (Filename);                                                  // directory separators for current platform
  imSurface.Picture.LoadFromFile(Filename);                                    // display the pic of the surface chosen
  laV1.Visible := True; edV1.Visible := True;                                  // first variable always visible; others will be set visible if effectively used
  laV2.Visible := False; edV2.Visible := False;
  laV3.Visible := False; edV3.Visible := False;
  laV4.Visible := False; edV4.Visible := False;
  // Generate question and do calculation depending on surface chosen and exercise type selected
  case iSurface of
     1: begin
          SquareQuestion(bPythagoras, A);
          if sExercise = 'area' then
            rAnswer := SquareArea(A)
          else
            rAnswer := SquareCircumference(A);
        end;
     2: begin
          RectangleQuestion(bPythagoras, A, B);
          if sExercise = 'area' then
            rAnswer := RectangleArea(A, B)
          else
            rAnswer := RectangleCircumference(A, B);
        end;
     3: begin
          TriangleQuestion(sExercise, A, B, C, H);
          if sExercise = 'area' then
            rAnswer := TriangleArea(C, H)
          else
            rAnswer := TriangleCircumference(A, B, C);
        end;
     4: begin
          ParallelogramQuestion(sExercise, A, B, H);
          if sExercise = 'area' then
            rAnswer := ParallelogramArea(A, H)
          else
            rAnswer := ParallelogramCircumference(A, B);
        end;
     5: begin
          TrapezoidQuestion(sExercise, A, B, C, D, H);
          if sExercise = 'area' then
            rAnswer := TrapezoidArea(A, C, H)
          else
            rAnswer := TrapezoidCircumference(A, B, C, D);
        end;
     6: begin
          CircleQuestion(R);
          if sExercise = 'area' then
            rAnswer := CircleArea(R)
          else
            rAnswer := CircleCircumference(R);
        end;
     7: begin
          Triangle2Question(bPythagoras, A, H);
          if sExercise = 'area' then
            rAnswer := TriangleArea(A, H)
          else
            rAnswer := TriangleCircumference(A, A, A);
        end;
     8: begin
          Triangle3Question(sExercise, bPythagoras, A, S, H);
          if sExercise = 'area' then
            rAnswer := TriangleArea(A, H)
          else
            rAnswer := TriangleCircumference(A, S, S);
        end;
     9: begin
          Trapezoid2Question(sExercise, bPythagoras, A, B, C, H);
          if sExercise = 'area' then
            rAnswer := TrapezoidArea(A, C, H)
          else
            rAnswer := TrapezoidCircumference(A, B, C, B);
        end;
    10: begin
          SectorQuestion(R, Alpha, B);
          if sExercise = 'area' then
            rAnswer := SectorArea(R, Alpha)
          else
            rAnswer := SectorCircumference(R, B);
        end;
  end;
  rAnswer := Round(1000 * rAnswer) / 1000;                                     // result rounded to 3 fracional digits
  // Some variables to be set for program continuation
  btAnswer.Enabled := True;                                                    // enable Answer button
  bNoAnswer := True;                                                           // actual question has not yet been answered
  edAnswer.SetFocus;                                                           // focus the Answer field
end;

{ Button "Antwort": Check user's answer }

procedure TfGeo1.btAnswerClick(Sender: TObject);

begin
  if edAnswer.Text = '' then
    rUAnswer := 0
  else
    rUAnswer := StrToFloat(edAnswer.Text);
  // Correct answer
  if Abs(rAnswer - rUAnswer) < 0.001 then begin                                // answer = correct if equality up to 3 first fractional digits
    Inc(iCorrect);
    edCorrect.Text := IntToStr(iCorrect);
    edEvaluation.Text := 'Diese Antwort ist richtig!';
    edEvaluation.Font.Color := clLime;
  end
  // False answer
  else begin
    Inc(iFalse);
    edFalse.Text := IntToStr(iFalse);
    edEvaluation.Text := 'Falsch! Richtige Anwort ist ' + FloatToStr(rAnswer);
    edEvaluation.Font.Color := clRed;
  end;
  // Display success percentage
  DisplaySuccess(iQuestion, iCorrect);
  // Some variables to be set for program continuation
  btAnswer.Enabled := False;                                                   // disable Answer button
  bNoAnswer := False;                                                          // actual question has been answered
  btQuestion.SetFocus;                                                         // focus the Question button
end;

end.

