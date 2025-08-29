{***************************************}
{* Main unit for Geometry2 application *}
{***************************************}

unit geo2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, PopupNotifier, help;

type
  { TfGeo2 }
  TfGeo2 = class(TForm)
    edV5: TEdit;
    edV6: TEdit;
    laV5: TLabel;
    laV6: TLabel;
    mSetLevel3: TMenuItem;
    mGeo2: TMainMenu;
    mTest: TMenuItem;
    mTestNew: TMenuItem;
    mTestExit: TMenuItem;
    mSettings: TMenuItem;
    mSetExercises: TMenuItem;
    mSetExercVolume: TMenuItem;
    mSetExercArea: TMenuItem;
    mSetLevel: TMenuItem;
    mSetLevel1: TMenuItem;
    mSetLevel2: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    StaticText2: TStaticText;
    stSolid: TStaticText;
    imSolid: TImage;
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
    procedure mSetExercVolumeClick(Sender: TObject);
    procedure mSetExercAreaClick(Sender: TObject);
    procedure mSetLevel1Click(Sender: TObject);
    procedure mSetLevel2Click(Sender: TObject);
    procedure mSetLevel3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btAnswerClick(Sender: TObject);
  private
    iSolids, iSolid, iQuestion, iCorrect, iFalse: Integer;
    sExercise: string;
    rAnswer, rUAnswer: Real;
    bPythagoras, bNoAnswer: Boolean;
  end;

const
  Pi = 3.14159265;
  Solids: array[1..9] of string = (
    'Würfel', 'Quader', 'Zylinder', 'Kegel', 'Kugel', 'Pyramide mit quadratischer Grundfläche',
    'Pyramide mit rechteckiger Grundfläche', 'Pyramidenstumpf', 'Kegelstumpf'
  );

var
  fGeo2: TfGeo2;

implementation

{$R *.lfm}

{ Function: cube }

function Cube(X: Real): Real;

begin
  Cube := X * X * X;
end;

{ New test: Reset form fields and variables }

procedure NewTest(var Q, C, F: Integer);

begin
  fGeo2.stSolid.Caption := '';
  fGeo2.imSolid.Picture.Clear;
  fGeo2.laV1.Visible := False; fGeo2.edV1.Visible := False;
  fGeo2.laV2.Visible := False; fGeo2.edV2.Visible := False;
  fGeo2.laV3.Visible := False; fGeo2.edV3.Visible := False;
  fGeo2.laV4.Visible := False; fGeo2.edV4.Visible := False;
  fGeo2.laV5.Visible := False; fGeo2.edV5.Visible := False;
  fGeo2.laV6.Visible := False; fGeo2.edV6.Visible := False;
  fGeo2.edQuestion.Text := ''; Q := 0;
  fGeo2.edCorrect.Text := '';  C := 0;
  fGeo2.edFalse.Text := '';    F := 0;
  fGeo2.edSuccess.Text := '';
  fGeo2.edSuccess.Color := clForm;
  fGeo2.edAnswer.Text := '';
  fGeo2.edEvaluation.Text := '';
  fGeo2.bNoAnswer := False;
  fGeo2.btAnswer.Enabled := False;
end;

{ Colored display of success (percentage of correct answers) }

procedure DisplaySuccess(Q, C: Integer);

var
  P: Real;

begin
  P := 100 * (C / Q);
  P := Round(100 * P) / 100;                                                   // rounded to 2 fractional digits
  fGeo2.edSuccess.Text := FloatToStr(P) + '%';
  if P < 50 then
    fGeo2.edSuccess.Color := clRed
  else if P < 60 then
    fGeo2.edSuccess.Color := clYellow
  else
    fGeo2.edSuccess.Color := clLime;
end;

{ Questions for solid = cube }

procedure CubeQuestion(Pyth: Boolean; out A: Real);

var
  QType: Integer;
  D: Real;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  if QType = 1 then begin
    // a is given
    A := Random(10) + 1;
    fGeo2.laV1.Caption := 'a';
    fGeo2.edV1.Text := FloatToStr(A);
  end
  else begin
    // a must be calculated from d
    D := Random(8) + 5;
    fGeo2.laV1.Caption := 'd';
    fGeo2.edV1.Text := FloatToStr(D);
    A := D / Sqrt(3);
  end;
end;

{ Questions for solid = cuboid }

procedure CuboidQuestion(Pyth: Boolean; out A, B, C: Real);

var
  QType: Integer;
  D: Real;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(4) + 1;
  if QType = 1 then begin
    // a and b are given
    repeat
      A := Random(10) + 1;
      B := Random(10) + 1;
      C := Random(10) + 1;
    until (A <= C) and (A < B) and (C < B);
    fGeo2.laV1.Caption := 'a';
    fGeo2.edV1.Text := FloatToStr(A);
    fGeo2.laV2.Caption := 'b';
    fGeo2.edV2.Text := FloatToStr(B);
    fGeo2.laV3.Caption := 'c';
    fGeo2.edV3.Text := FloatToStr(C);
  end
  else begin
    // a, b or c must be calculated from d
    repeat
      A := -1; B := -1; C := -1; D := Random(8) + 5;
      if QType = 2 then begin
        // b must be calculated from d
        A := Random(10) + 1;
        C := Random(10) + 1;
        if Sqr(D) > Sqr(A) + Sqr(C) then
          B := Sqrt(Sqr(D) - Sqr(A) - Sqr(C));
      end
      else if QType = 3 then begin
        // a must be calculated from d
        B := Random(10) + 1;
        C := Random(10) + 1;
        if Sqr(D) > Sqr(B) + Sqr(C) then
          A := Sqrt(Sqr(D) - Sqr(B) - Sqr(C));
      end
      else begin
        // c must be calculated from d
        A := Random(10) + 1;
        B := Random(10) + 1;
        if Sqr(D) > Sqr(A) + Sqr(B) then
          C := Sqrt(Sqr(D) - Sqr(A) - Sqr(B));
      end
    until (A <> -1) and (B <> -1) and (C <> -1) and (A <= C) and (A < B) and (C < B);
    if QType = 2 then begin
      fGeo2.laV1.Caption := 'a';
      fGeo2.edV1.Text := FloatToStr(A);
      fGeo2.laV2.Caption := 'c';
      fGeo2.edV2.Text := FloatToStr(C);
    end
    else if QType = 3 then begin
      fGeo2.laV1.Caption := 'b';
      fGeo2.edV1.Text := FloatToStr(B);
      fGeo2.laV2.Caption := 'c';
      fGeo2.edV2.Text := FloatToStr(C);
    end
    else begin
      fGeo2.laV1.Caption := 'a';
      fGeo2.edV1.Text := FloatToStr(A);
      fGeo2.laV2.Caption := 'b';
      fGeo2.edV2.Text := FloatToStr(B);
    end;
    fGeo2.laV3.Caption := 'd';
    fGeo2.edV3.Text := FloatToStr(D);
  end;
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
  fGeo2.laV3.Visible := True; fGeo2.edV3.Visible := True;
end;

{ Questions for solid = cylinder }

procedure CylinderQuestion(out R, H: Real);

begin
  repeat
    R := Random(10) + 1;
    H := Random(10) + 1;
  until H > 2 * R;
  fGeo2.laV1.Caption := 'r';
  fGeo2.edV1.Text := FloatToStr(R);
  fGeo2.laV2.Caption := 'h';
  fGeo2.edV2.Text := FloatToStr(H);
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
end;

{ Questions for solid = cone }

procedure ConeQuestion(Ex: string; Pyth: Boolean; out R, H, S: Real);

var
  QType: Integer;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  // Volume exercises
  if Ex = 'volume' then begin
    repeat
      H := -1;
      if QType = 1 then begin
        // r and h are given
        R := Random(10) + 1;
        H := Random(10) + 1;
        S := Sqrt(Sqr(H) + Sqr(R));
      end
      else begin
        // r given; h must be calculated from s
        R := Random(10) + 1;
        S := Random(8) + 5;
        if Sqr(S) > Sqr(R) then
          H := Sqrt(Sqr(S) - Sqr(R));
      end;
    until (H <> -1) and (R < H);
  end
  // Surface area exercises
  else begin
    repeat
      H := -1;
      if QType = 1 then begin
        // r and s are given
        R := Random(10) + 1;
        S := Random(8) + 5;
        if Sqr(S) > Sqr(R) then
          H := Sqrt(Sqr(S) - Sqr(R));
      end
      else begin
        // r given; s must be calculated from h
        R := Random(10) + 1;
        H := Random(8) + 3;
        S := Sqrt(Sqr(H) + Sqr(R));
      end;
    until (H <> -1) and (R < H);
  end;
  fGeo2.laV1.Caption := 'r';
  fGeo2.edV1.Text := FloatToStr(R);
  if ((Ex = 'volume') and (QType = 1)) or ((Ex = 'area') and (QType = 2)) then begin
    fGeo2.laV2.Caption := 'h';
    fGeo2.edV2.Text := FloatToStr(H);
  end
  else begin
    fGeo2.laV2.Caption := 's';
    fGeo2.edV2.Text := FloatToStr(S);
  end;
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
end;

{ Questions for solid = sphere }

procedure SphereQuestion(out R: Real);

var
  QType: Integer;
  D: Real;

begin
  QType := Random(2) + 1;
  if QType = 1 then begin
    // r is given
    R := Random(10) + 1;
    fGeo2.laV1.Caption := 'r';
    fGeo2.edV1.Text := FloatToStr(R);
  end
  else begin
    // d = 2r is given
    D := Random(10) + 1;
    R := D / 2;
    fGeo2.laV1.Caption := 'd';
    fGeo2.edV1.Text := FloatToStr(D);
  end;
end;

{ Questions for solid = pyramid 1 }

procedure Pyramid1Question(Ex: string; Pyth: Boolean; out A, H, HS: Real);

var
  QType: Integer;
  S, Temp1, Temp2: Real;

begin
  if not Pyth then begin
    if Ex = 'volume' then
      QType := 1
    else
      QType := 2;
  end
  else
    QType := Random(3) + 1;
  // a is always given
  repeat
    H := -1; HS := -1;
    A := Random(10) + 1;
    Temp1 := Random(10) + 1;
    Temp2 := Random(8) + 5;
    if QType = 1 then begin
      // h is given
      H := Temp1;
      HS := Sqrt(Sqr(H) + Sqr(A / 2));
    end
    else if QType = 2 then begin
      // hs is given
      HS := Temp2;
      if Sqr(HS) > Sqr(A / 2) then
        H := Sqrt(Sqr(HS) - Sqr(A / 2));
    end
    else begin
      // h/hs must be calculated from s
      S := Temp1;
      if Sqr(S) > Sqr(A / 2) then begin
        HS := Sqrt(Sqr(S) - Sqr(A / 2));
        if Sqr(HS) > Sqr(A / 2) then
          H := Sqrt(Sqr(HS) - Sqr(A / 2));
      end;
    end;
  until (H <> -1) and (HS <> -1) and (A < H);
  fGeo2.laV1.Caption := 'a';
  fGeo2.edV1.Text := FloatToStr(A);
  if QType = 1 then begin
    fGeo2.laV2.Caption := 'h';
    fGeo2.edV2.Text := FloatToStr(H);
  end
  else if QType = 2 then begin
    fGeo2.laV2.Caption := 'hs';
    fGeo2.edV2.Text := FloatToStr(HS);
  end
  else begin
    fGeo2.laV2.Caption := 's';
    fGeo2.edV2.Text := FloatToStr(S);
  end;
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
end;

{ Questions for solid = pyramid 2 }

procedure Pyramid2Question(Ex: string; Pyth: Boolean; out A, B, H, HA, HB: Real);

var
  QType: Integer;
  Temp1, Temp2: Real;

begin
  if not Pyth then begin
    if Ex = 'volume' then
      QType := 1
    else
      QType := 2;
  end
  else
    QType := Random(4) + 1;
  // a and b are always given
  repeat
    H := -1; HA := -1; HB := -1;
    A := Random(10) + 1;
    B := Random(10) + 1;
    Temp1 := Random(10) + 1;
    Temp2 := Random(8) + 5;
    if QType = 1 then begin
      // h is given
      H  := Temp1;
      HA := Sqrt(Sqr(H) + Sqr(B / 2));
      HB := Sqrt(Sqr(H) + Sqr(A / 2));
    end
    else if QType = 2 then begin
      // ha and hb are given
      H := Temp1;
      HA := Round(10000 * Sqrt(Sqr(H) + Sqr(B / 2))) / 10000;                  // ha rounded to 4 decimals
      HB := Round(10000 * Sqrt(Sqr(H) + Sqr(A / 2))) / 10000;                  // hb rounded to 4 decimals
      if (Sqr(HA) > Sqr(B / 2)) and (Sqr(HB) > Sqr(A / 2)) then
        H  := Sqrt(Sqr(HA) - Sqr(B / 2));                                      // recalculation of h with rounded ha value
    end
    else if QType = 3 then begin
      // h/hb must be calculated from ha
      HA := Temp2;
      if Sqr(HA) > Sqr(B / 2) then
        H := Sqrt(Sqr(HA) - Sqr(B / 2));
      HB := Sqrt(Sqr(H) + Sqr(A / 2));
    end
    else begin
      // h/ha must be calculated from hb
      HB := Temp2;
      if Sqr(HB) > Sqr(A / 2) then
        H := Sqrt(Sqr(HB) - Sqr(A / 2));
      HA := Sqrt(Sqr(H) + Sqr(B / 2));
    end;
  until (H <> -1) and (HA <> -1) and (HB <> -1) and (A <> B);
  fGeo2.laV1.Caption := 'a';
  fGeo2.edV1.Text := FloatToStr(A);
  fGeo2.laV2.Caption := 'b';
  fGeo2.edV2.Text := FloatToStr(B);
  if QType = 1 then begin
    fGeo2.laV3.Caption := 'h';
    fGeo2.edV3.Text := FloatToStr(H);
  end
  else if QType = 2 then begin
    fGeo2.laV3.Caption := 'ha';
    fGeo2.edV3.Text := FloatToStr(HA);
    fGeo2.laV4.Caption := 'hb';
    fGeo2.edV4.Text := FloatToStr(HB);
    fGeo2.laV4.Visible := True; fGeo2.edV4.Visible := True;
  end
  else if QType = 3 then begin
    fGeo2.laV3.Caption := 'ha';
    fGeo2.edV3.Text := FloatToStr(HA);
  end
  else begin
    fGeo2.laV3.Caption := 'hb';
    fGeo2.edV3.Text := FloatToStr(HB);
  end;
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
  fGeo2.laV3.Visible := True; fGeo2.edV3.Visible := True;
end;

{ Questions for solid = pyramid 3 }

procedure Pyramid3Question(Ex: string; Pyth: Boolean; out A1, B1, A2, B2, H, HA, HB: Real);

var
  QType: Integer;
  Temp1, Temp2: Real;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(4) + 1;
  // a1, a2, b1 and b2 are always given
  repeat
    H := -1; HA := -1; HB := -1;
    repeat
      A1 := Random(10) + 1;
      B1 := Random(10) + 1;
      A2 := Random(10) + 1;
      B2 := Random(10) + 1;
    until (A2 > A1) and (B2 > B1);
    Temp1 := Random(10) + 1;
    Temp2 := Random(8) + 5;
    // 'Volume' exercise
    if Ex = 'volume' then begin
      if (QType = 1) or (QType = 4) then begin
        // h is also given
        H  := Temp1;
        HA := Sqrt(Sqr(H) + Sqr(B2 - B1) / 4);
        HB := Sqrt(Sqr(H) + Sqr(A2 - A1) / 4);
      end
      else if QType = 2 then begin
        // h must be calculated from ha
        HA := Temp2;
        if Sqr(HA) > Sqr(B2 - B1) / 4 then begin
          H  := Sqrt(Sqr(HA) - Sqr(B2 - B1) / 4);
          HB := Sqrt(Sqr(H) + Sqr(A2 - A1) / 4);
        end;
      end
      else begin
        // h must be calculated from hb
        HB := Temp2;
        if Sqr(HB) > Sqr(A2 - A1) / 4 then begin
          H  := Sqrt(Sqr(HB) - Sqr(A2 - A1) / 4);
          HA := Sqrt(Sqr(H) + Sqr(B2 - B1) / 4);
        end;
      end;
    end
    // 'Area' exercise
    else begin
      if QType = 1 then begin
        // ha and hb are also given
        H := Temp1;
        HA := Round(10000 * Sqrt(Sqr(H) + Sqr(B2 - B1) / 4)) / 10000;          // ha rounded to 4 decimals
        HB := Round(10000 * Sqrt(Sqr(H) + Sqr(A2 - A1) / 4)) / 10000;          // hb rounded to 4 decimals
        if (Sqr(HA) > Sqr(B2 - B1) / 4) and (Sqr(HB) > Sqr(A2 - A1) / 4) then
          H := Sqrt(Sqr(HA) - Sqr(B2 - B1) / 4);                               // recalculation of h with rounded ha value
      end
      else if QType = 2 then begin
        // ha and hb must be calculated from h
        H  := Temp1;
        HA := Sqrt(Sqr(H) + Sqr(B2 - B1) / 4);
        HB := Sqrt(Sqr(H) + Sqr(A2 - A1) / 4);
      end
      else if QType = 3 then begin
        // ha must be calculated from hb
        HB := Temp2;
        if Sqr(HB) > Sqr(A2 - A1) / 4 then begin
          H  := Sqrt(Sqr(HB) - Sqr(A2 - A1) / 4);
          HA := Sqrt(Sqr(H) + Sqr(B2 - B1) / 4);
        end;
      end
      else begin
        // hb must be calculated from ha
        HA := Temp2;
        if Sqr(HA) > Sqr(B2 - B1) / 4 then begin
          H  := Sqrt(Sqr(HA) - Sqr(B2 - B1) / 4);
          HB := Sqrt(Sqr(H) + Sqr(A2 - A1) / 4);
        end;
      end
    end;
  until (H <> -1) and (HA <> -1) and (HB <> -1);
  fGeo2.laV1.Caption := 'a1';
  fGeo2.edV1.Text := FloatToStr(A1);
  fGeo2.laV2.Caption := 'b1';
  fGeo2.edV2.Text := FloatToStr(B1);
  fGeo2.laV3.Caption := 'a2';
  fGeo2.edV3.Text := FloatToStr(A2);
  fGeo2.laV4.Caption := 'b2';
  fGeo2.edV4.Text := FloatToStr(B2);
  if Ex = 'volume' then begin
    if (QType = 1) or (QType = 4) then begin
      fGeo2.laV5.Caption := 'h';
      fGeo2.edV5.Text := FloatToStr(H);
    end
    else if QType = 2 then begin
      fGeo2.laV5.Caption := 'ha';
      fGeo2.edV5.Text := FloatToStr(HA);
    end
    else begin
      fGeo2.laV5.Caption := 'hb';
      fGeo2.edV5.Text := FloatToStr(HB);
    end
  end
  else begin
    if QType = 1 then begin
      fGeo2.laV5.Caption := 'ha';
      fGeo2.edV5.Text := FloatToStr(HA);
      fGeo2.laV6.Caption := 'hb';
      fGeo2.edV6.Text := FloatToStr(HB);
      fGeo2.laV6.Visible := True; fGeo2.edV6.Visible := True;
    end
    else if QType = 2 then begin
      fGeo2.laV5.Caption := 'h';
      fGeo2.edV5.Text := FloatToStr(H);
    end
    else if QType = 3 then begin
      fGeo2.laV5.Caption := 'hb';
      fGeo2.edV5.Text := FloatToStr(HB);
    end
    else begin
      fGeo2.laV5.Caption := 'ha';
      fGeo2.edV5.Text := FloatToStr(HA);
    end
  end;
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
  fGeo2.laV3.Visible := True; fGeo2.edV3.Visible := True;
  fGeo2.laV4.Visible := True; fGeo2.edV4.Visible := True;
  fGeo2.laV5.Visible := True; fGeo2.edV5.Visible := True;
end;

{ Questions for solid = cone 2 }

procedure Cone2Question(Ex: string; Pyth: Boolean; out R1, R2, H, M: Real);

var
  QType: Integer;

begin
  if not Pyth then
    QType := 1
  else
    QType := Random(2) + 1;
  // R1 and R2 are always given
  repeat
    R1 := Random(10) + 1;
    R2 := Random(10) + 1;
    H := Random(10) + 1;
    M := Random(8) + 5;
  until (R1 > R2) and (H > R1) and (M > H);
  // Volume exercises
  if Ex = 'volume' then begin
    if QType = 1 then begin
      // h is also given
      M := Sqrt(Sqr(H) + Sqr(R1 - R2));
    end
    else begin
      // h must be calculated from m
      H := Sqrt(Sqr(M) - Sqr(R1 - R2));
    end;
  end
  // Surface area exercises
  else begin
    if QType = 1 then begin
      // m is also given
      H := Sqrt(Sqr(M) - Sqr(R1 - R2));
    end
    else begin
      // m must be calculated from h
      M := Sqrt(Sqr(H) + Sqr(R1 - R2));
    end;
  end;
  fGeo2.laV1.Caption := 'R';
  fGeo2.edV1.Text := FloatToStr(R1);
  fGeo2.laV2.Caption := 'r';
  fGeo2.edV2.Text := FloatToStr(R2);
  if ((Ex = 'volume') and (QType = 1)) or ((Ex = 'area') and (QType = 2)) then begin
    fGeo2.laV3.Caption := 'h';
    fGeo2.edV3.Text := FloatToStr(H);
  end
  else begin
    fGeo2.laV3.Caption := 'm';
    fGeo2.edV3.Text := FloatToStr(M);
  end;
  fGeo2.laV2.Visible := True; fGeo2.edV2.Visible := True;
  fGeo2.laV3.Visible := True; fGeo2.edV3.Visible := True;
end;

{ --------------------------------------- }
{ Functions to calculate the solid volume }
{ --------------------------------------- }

function CubeVolume(A: Real): Real;

begin
  CubeVolume := Cube(A);
end;

function CuboidVolume(A, B, C: Real): Real;

begin
  CuboidVolume := A * B * C;
end;

function CylinderVolume(R, H: Real): Real;

begin
  CylinderVolume := Sqr(R) * Pi * H;
end;

function ConeVolume(R, H: Real): Real;

begin
  ConeVolume := (1 / 3) * Sqr(R) * Pi * H;
end;

function SphereVolume(R: Real): Real;

begin
  SphereVolume := (4 / 3) * Cube(R) * Pi;
end;

function Pyramid1Volume(A, H: Real): Real;

begin
  Pyramid1Volume := (1 / 3) * Sqr(A) * H;
end;

function Pyramid2Volume(A, B, H: Real): Real;

begin
  Pyramid2Volume := (1 / 3) * A * B * H;
end;

function Pyramid3Volume(A1, B1, A2, B2, H: Real): Real;

begin
  Pyramid3Volume := (1 / 3) * H * (A2 * B2 + Sqrt(A1 * B1 * A2 * B2) + A1 * B1);
end;

function Cone2Volume(R1, R2, H: Real): Real;

begin
  Cone2Volume := (1 / 3) * H * Pi * (Sqr(R1) + R1 * R2 + Sqr(R2));
end;

{ --------------------------------------------- }
{ Functions to calculate the solid surface area }
{ --------------------------------------------- }

function CubeSurface(A: Real): Real;

begin
  CubeSurface := 6 * Sqr(A);
end;

function CuboidSurface(A, B, C: Real): Real;

begin
  CuboidSurface := 2 * (A * B + A * C + B * C);
end;

function CylinderSurface(R, H: Real): Real;

begin
  CylinderSurface := 2 * Sqr(R) * Pi + 2 * R * Pi * H;
end;

function ConeSurface(R, S: Real): Real;

begin
  ConeSurface := Pi * R * S + Sqr(R) * Pi;
end;

function SphereSurface(R: Real): Real;

begin
  SphereSurface := 4 * Sqr(R) * Pi;
end;

function Pyramid1Surface(A, HS: Real): Real;

begin
  Pyramid1Surface := 2 * A * HS + Sqr(A);
end;

function Pyramid2Surface(A, B, HA, HB: Real): Real;

begin
  Pyramid2Surface := A * HA + B * HB + A * B;
end;

function Pyramid3Surface(A1, B1, A2, B2, HA, HB: Real): Real;

begin
  Pyramid3Surface := (B1 + B2) * HB + (A1 + A2) * HA + A1 * B1 + A2 * B2;
end;

function Cone2Surface(R1, R2, M: Real): Real;

begin
  Cone2Surface := Pi * M * (R1 + R2) + Sqr(R1) * Pi + Sqr(R2) * Pi;
end;

{**********}
{* TfGeo2 *}
{**********}

{ Application start: Set default values and reset variables and form controls }

procedure TfGeo2.FormCreate(Sender: TObject);

begin
  sExercise := 'volume'; iSolids := 9; bPythagoras := False;
  NewTest(iQuestion, iCorrect, iFalse);
  Randomize;
end;

{ Menu item "Test > Neu": Start new test }

procedure TfGeo2.mTestNewClick(Sender: TObject);

begin
  NewTest(iQuestion, iCorrect, iFalse);
end;

{ Menu item "Test > Verlassen": Exit the application }

procedure TfGeo2.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Berechnung > Volumen": Exercises = volume calculations }

procedure TfGeo2.mSetExercVolumeClick(Sender: TObject);

begin
  if not mSetExercVolume.Checked then begin
    mSetExercVolume.Checked := True;
    mSetExercArea.Checked := False;
    sExercise := 'volume';
    laExercise.Caption := 'Volumen';
  end;
end;

{ Menu item "Einstellungen > Berechnung > Oberfläche": Exercises = surface area calculations }

procedure TfGeo2.mSetExercAreaClick(Sender: TObject);

begin
  if not mSetExercArea.Checked then begin
    mSetExercArea.Checked := True;
    mSetExercVolume.Checked := False;
    sExercise := 'area';
    laExercise.Caption := 'Oberfläche';
  end;
end;

{ Menu item "Einstellungen > Aufgaben > Grundkörper": Exercises = 6 basic solids }

procedure TfGeo2.mSetLevel1Click(Sender: TObject);

begin
  if not mSetLevel1.Checked then begin
    mSetLevel1.Checked := True;
    mSetLevel2.Checked := False;
    mSetLevel3.Checked := False;
    iSolids := 6;
    bPythagoras := False;
  end;
end;

{ Menu item "Einstellungen > Aufgaben > Alle Körper": Exercises = all 9 solids }

procedure TfGeo2.mSetLevel2Click(Sender: TObject);

begin
  if not mSetLevel2.Checked then begin
    mSetLevel2.Checked := True;
    mSetLevel1.Checked := False;
    mSetLevel3.Checked := False;
    iSolids := 9;
    bPythagoras := False;
  end;
end;

{ Menu item "Einstellungen > Aufgaben > Pythagoras": Exercises = all solids with Pythagoras required questions enabled }

procedure TfGeo2.mSetLevel3Click(Sender: TObject);

begin
  if not mSetLevel3.Checked then begin
    mSetLevel3.Checked := True;
    mSetLevel1.Checked := False;
    mSetLevel2.Checked := False;
    iSolids := 9;
    bPythagoras := True;
  end;
end;

{ Menu item "Hilfe > Hilfe": Display Help text }

procedure TfGeo2.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Hilfe > Über": Display About text }

procedure TfGeo2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if Popup.Visible then
      Popup.Visible := False
  else begin
    S := 'Erstellen von Mathematik-Aufgaben zur Berechnung von Volumen and Oberfläche ';
    S += 'von geometrischen Körpern.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, Juli, 2018.';
    Popup.Title := 'Geometrie.';
    Popup.Text := S;
    Popup.Visible := True;
  end;
end;

{ Button "Frage": Generate a new question }

procedure TfGeo2.btQuestionClick(Sender: TObject);

var
  A, B, C, A1, A2, B1, B2, S, M, H, HA, HB, HS, R, R1, R2: Real;
  Filename: string;

begin
  A := 0; B := 0; C := 0; A1 := 0; A2 := 0; B1 := 0; B2 := 0; S := 0; H := 0; HA := 0; HB := 0; R := 0; R1 := 0; R2 := 0;
  edAnswer.Text := ''; edEvaluation.Text := '';
  // First check is there was an unanswered question before. If so, increment number of false answers
  if bNoAnswer then begin
    Inc(iFalse);
    edFalse.Text := IntToStr(iFalse);
    DisplaySuccess(iQuestion, iCorrect);
  end;
  // New question generation
  Inc(iQuestion); edQuestion.Text := IntToStr(iQuestion);
  iSolid := Random(iSolids) + 1;                                               // a random surface from the 6 resp. 9 available
  stSolid.Caption := Solids[iSolid];
  Filename := 'solids/s' + IntToStr(iSolid) + '.jpg';
  Filename := StringReplace(Filename, 'ü', 'ue', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ä', 'ae', [rfReplaceAll]);
  DoDirSeparators (Filename);                                                  // directory separators for current platform
  imSolid.Picture.LoadFromFile(Filename);                                      // display the pic of the surface chosen
  laV1.Visible := True; edV1.Visible := True;                                  // first variable always visible; others will be set visible if effectively used
  laV2.Visible := False; edV2.Visible := False;
  laV3.Visible := False; edV3.Visible := False;
  laV4.Visible := False; edV4.Visible := False;
  laV5.Visible := False; edV5.Visible := False;
  laV6.Visible := False; edV6.Visible := False;
  // Generate question and do calculation depending on solid chosen and exercise type selected
  case iSolid of
     1: begin
          CubeQuestion(bPythagoras, A);
          if sExercise = 'volume' then
            rAnswer := CubeVolume(A)
          else
            rAnswer := CubeSurface(A);
        end;
     2: begin
          CuboidQuestion(bPythagoras, A, B, C);
          if sExercise = 'volume' then
            rAnswer := CuboidVolume(A, B, C)
          else
            rAnswer := CuboidSurface(A, B, C);
        end;
     3: begin
          CylinderQuestion(R, H);
          if sExercise = 'volume' then
            rAnswer := CylinderVolume(R, H)
          else
            rAnswer := CylinderSurface(R, H);
        end;
     4: begin
          ConeQuestion(sExercise, bPythagoras, R, H, S);
          if sExercise = 'volume' then
            rAnswer := ConeVolume(R, H)
          else begin
            rAnswer := ConeSurface(R, S);
          end;
        end;
     5: begin
          SphereQuestion(R);
          if sExercise = 'volume' then
            rAnswer := SphereVolume(R)
          else
            rAnswer := SphereSurface(R);
        end;
     6: begin
          Pyramid1Question(sExercise, bPythagoras, A, H, HS);
          if sExercise = 'volume' then
            rAnswer := Pyramid1Volume(A, H)
          else begin
            rAnswer := Pyramid1Surface(A, HS);
          end;
        end;
     7: begin
          Pyramid2Question(sExercise, bPythagoras, A, B, H, HA, HB);
          if sExercise = 'volume' then
            rAnswer := Pyramid2Volume(A, B, H)
          else begin
            rAnswer := Pyramid2Surface(A, B, HA, HB);
          end;
        end;
     8: begin
          Pyramid3Question(sExercise, bPythagoras, A1, B1, A2, B2, H, HA, HB);
          if sExercise = 'volume' then
            rAnswer := Pyramid3Volume(A1, B1, A2, B2, H)
          else begin
            rAnswer := Pyramid3Surface(A1, B1, A2, B2, HA, HB);
          end;
        end;
     9: begin
          Cone2Question(sExercise, bPythagoras, R1, R2, H, M);
          if sExercise = 'volume' then
            rAnswer := Cone2Volume(R1, R2, H)
          else begin
            rAnswer := Cone2Surface(R1, R2, M);
          end;
        end;
  end;
  rAnswer := Round(1000 * rAnswer) / 1000;                                     // result rounded to 3 fracional digits
  // Some variables to be set for program continuation
  btAnswer.Enabled := True;                                                    // enable Answer button
  bNoAnswer := True;                                                           // actual question has not yet been answered
  edAnswer.SetFocus;                                                           // focus the Answer field
end;

{ Button "Antwort": Check user's answer }

procedure TfGeo2.btAnswerClick(Sender: TObject);

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

