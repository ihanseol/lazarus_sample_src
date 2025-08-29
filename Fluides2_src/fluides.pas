{**************************************}
{* Main unit for Fluides2 application *}
{**************************************}

unit fluides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, Math;

type
  TFluid = record
    Name: string;
    Density: Real;
  end;
  TData = array[0..11] of Real;
  TGivenData = array[0..11] of Boolean;
  {************}
  { TfFluides2 }
  {************}
  TfFluides2 = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExercise1, mExercise2, mExercise3, mExercise4: TMenuItem;
    mExercise5, mExercise6, mExercise7, mExerciseExit: TMenuItem;
    mOptions, mOptionsG, mOptionsG980, mOptionsG981, mOptionsG100: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    Shape1: TShape;
    StaticText1: TStaticText;
    edDetails: TMemo;
    imExercise: TImage;
    Label1, Label2, Label3, Label7, Label8, Label9, Label10: TLabel;
    laPressure1, laPressure2, laEntry, laExit, laSpeed1, laSpeed2: TLabel;
    laSup1, laSup2, laSup3, laSup4, laSup5: TLabel;
    laUPressure1, laUPressure2, laUEntry, laUExit, laUSpeed1, laUSpeed2: TLabel;
    laUSup1, laUSup2, laUSup3, laUSup4, laUSup5: TLabel;
    edFluidName, edFluidDensity, edPressure1, edPressure2: TEdit;
    edEntry, edExit, edSpeed1, edSpeed2: TEdit;
    edSup1, edSup2, edSup3, edSup4, edSup5: TEdit;
    sgEval: TStringGrid;
    imEval: TImage;
    btQuestion: TButton;
    btShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExercise1Click(Sender: TObject);
    procedure mExercise2Click(Sender: TObject);
    procedure mExercise3Click(Sender: TObject);
    procedure mExercise4Click(Sender: TObject);
    procedure mExercise5Click(Sender: TObject);
    procedure mExercise6Click(Sender: TObject);
    procedure mExercise7Click(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsG980Click(Sender: TObject);
    procedure mOptionsG981Click(Sender: TObject);
    procedure mOptionsG100Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    iExType, iQuestion, iCorrect: Integer;
    aData, aUserData: TData;
    aGivenData: TGivenData;
    edData: array[0..11] of TEdit;
  end;

const
  PAtm = 1E+5;
  Fluids: array[0..2] of TFluid = (
    (Name: 'eau'; Density: 1E+3),
    (Name: 'essence'; Density: 0.7E+3),
    (Name: 'huile'; Density: 0.85E+3)
  );
  SUB_1 = #$E2#$82#$81; SUB_2 = #$E2#$82#$82; SUB_3 = #$E2#$82#$83;
  SUP_2 = #$C2#$B2;

var
  fFluides2: TfFluides2;
  iG: Real;

implementation

{$R *.lfm}

{ Format number for the string-grid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S <> '' then
    SN += S
  else
    SN := ' ' + SN;
  GFormat := SN;
end;

{ Format number (depending on significant decimal digits) }

function RFormat(R: Real): string;

var
  F: Integer;

begin
  if Int(R) = R then
    F := 0
  else if Int(10 * R) = 10 * R then
    F := 1
  else if Int(100 * R) = 100 * R then
    F := 2
  else if R < 1E-4 then
    F := 5
  else if R < 1E-3 then
    F := 4
  else
    F := 3;                                                                              // default (except for "very small" numbers)
  Result := FloatToStrF(R, ffFixed, 0, F);
end;

{ Clear form fields; reset values of data arrays }

procedure ClearForm(var DataFields: array of TEdit; var Data, UserData: TData; var GData: TGivenData; ClearAll: Boolean);

var
  I: Integer;

begin
  fFluides2.edFluidName.Text := '';
  DataFields[0].ReadOnly := True; DataFields[0].TabStop := False; DataFields[0].Color := clCream;
  DataFields[0].Text := ''; GData[0] := True;
  for I := 1 to 11 do begin
    DataFields[I].Text := '';
    DataFields[I].ReadOnly := False; DataFields[I].TabStop := True; DataFields[I].Color := clDefault;
    Data[I] := -1; UserData[I] := 0;
    GData[I] := False;
  end;
  fFluides2.imEval.Picture.Clear;
  fFluides2.btQuestion.Caption := 'Question';
  if ClearAll then begin
    // If this Boolean is True, clear the evaluation grid
    for I := 0 to 3 do
      fFluides2.sgEval.Cells[1, I] := '';
  end;
end;

{ Create a Boolean array, indicating the parameters that are given by the program }

Procedure FillGivenData(B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11: Boolean; var Data: TGivenData);

begin
  Data[1] := B1; Data[2] := B2; Data[3] := B3; Data[4]  := B4;  Data[5]  := B5; Data[6] := B6;
  Data[7] := B7; Data[8] := B8; Data[9] := B9; Data[10] := B10; Data[11] := B11;
end;

{ Fill in the form fields (values giveb by the program) }

procedure FillForm(GData: TGivenData; Data: TData; var FData: array of TEdit);

var
  I: Integer;

begin
  for I := 1 to 11 do begin
    if GData[I] then begin
      // Field containing a value given by the program
      FData[I].Text := RFormat(Data[I]);
      FData[I].ReadOnly := True; FData[I].TabStop := False;
      FData[I].Color := clCream;
    end;
  end;
end;

{ Prepare for a new exercise series }

procedure NewExercise(ExType: Integer; DetailsText: string; var DetailsField: TMemo; var DataFields: array of TEdit;
  var Data, UserData: TData; var GData: TGivenData; out Question, Correct: Integer);

var
  Filename: string;

begin
  DetailsField.Clear;
  DetailsField.Lines.AddText(DetailsText);
  Filename := 'fluides' + IntTosTr(ExType) + '.jpg'; fFluides2.imExercise.Picture.LoadFromFile(Filename);
  ClearForm(DataFields, Data, UserData, GData, True);
  fFluides2.laPressure1.Caption := 'Pression P' + SUB_1; fFluides2.laPressure2.Caption := 'Pression P' + SUB_2;
  fFluides2.laSpeed1.Caption := 'Vitesse V' + SUB_1;     fFluides2.laSpeed2.Caption := 'Vitesse V' + SUB_2;
  fFluides2.laPressure1.Visible := True; fFluides2.edPressure1.Visible := True; fFluides2.laUPressure1.Visible := True;
  fFluides2.laPressure2.Visible := True; fFluides2.edPressure2.Visible := True; fFluides2.laUPressure2.Visible := True;
  fFluides2.laEntry.Visible := True; fFluides2.edEntry.Visible := True; fFluides2.laUEntry.Visible := True;
  fFluides2.laExit.Visible := True; fFluides2.edExit.Visible := True; fFluides2.laUExit.Visible := True;
  fFluides2.laSpeed1.Visible := True; fFluides2.edSpeed1.Visible := True; fFluides2.laUSpeed1.Visible := True;
  fFluides2.laSpeed2.Visible := True; fFluides2.edSpeed2.Visible := True; fFluides2.laUSpeed2.Visible := True;
  fFluides2.laSup1.Visible := False; fFluides2.edSup1.Visible := False; fFluides2.laUSup1.Visible  := False;
  fFluides2.laSup2.Visible := False; fFluides2.edSup2.Visible := False; fFluides2.laUSup2.Visible := False;
  fFluides2.laSup3.Visible := False; fFluides2.edSup3.Visible := False; fFluides2.laUSup3.Visible := False;
  fFluides2.laSup4.Visible := False; fFluides2.edSup4.Visible := False; fFluides2.laUSup4.Visible := False;
  fFluides2.laSup5.Visible := False; fFluides2.edSup5.Visible := False; fFluides2.laUSup5.Visible := False;
  // Reset evaluation counters
  Question := 0; Correct := 0;
end;

{ Exercise type 1 question generation: "Circulation d’un fluide dans une conduite" }

procedure Exercise1(Density: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  ExType2, I: Integer;
  P1, P2, R1, R2, S1, S2, V1, V2, V2V1, Alpha, L: Real;
  S: string;

begin
  ExType2 := Random(3);
  S := 'Un fluide, supposé parfait et à une pression donnée, est acheminé dans une conduite en position horizontale. ';
  if ExType2 > 0 then begin
    S += 'La vitesse du fluide est augmenté par la présence d''un convergent avec des section d''entrée et de sortie de rayon connu. ';
    S += 'Calculez la vitesse du fluide à la sortie de la conduite.';
    fFluides2.laSup1.Visible := False; fFluides2.edSup1.Visible := False; fFluides2.laUSup1.Visible  := False;
    fFluides2.laSup2.Visible := False; fFluides2.edSup2.Visible := False; fFluides2.laUSup2.Visible := False;
  end
  else begin
    S += 'On veut accélérer le fluide de sorte que sa vitesse soit multipliée par 4. Pour cela, la conduite comporte un convergent ';
    S += 'caractérisé par l’angle α. Calculer la longueur du convergent.';
    fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible  := True;
    fFluides2.laSup1.Caption := 'Angle α'; fFluides2.laUSup1.Caption := '°';
    fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
    fFluides2.laSup2.Caption := 'Longueur l'; fFluides2.laUSup2.Caption := 'mm';
  end;
  Details.Lines.Clear; Details.Lines.AddText(S);
  // Random entry pressure (exit pressure = Patm)
  P1 := ((Random(101) + 200) / 100) * 1E+5; P2 := PAtm;
  // Random entry speed
  V1 := (Random(31) + 20) / 10;
  // Random radiuses resp. random speed multiplication factor
  if ExType2 > 0 then begin
    fFluides2.laEntry.Caption := 'Rayon R' + SUB_1; fFluides2.laExit.Caption := 'Rayon R' + SUB_2;
    fFluides2.laUEntry.Caption := 'mm'; fFluides2.laUExit.Caption := 'mm';
    // 2 random radiuses
    repeat
      R1 := Random(11) + 10;
      R2 := Random(6) + 5;
    until R1 - R2 >= 2;
    // Speed multiplication factor and V2 calculated
    V2V1 := Sqr(R1 / R2);
    V2 := Sqrt((2 * Sqr(V2V1) / (Sqr(V2V1) - 1))  * (P1 - P2) / Density); V1 := V2 / V2V1;
  end
  else begin
    // Random speed multiplication factor
    V2V1 := Random(4) + 2; V2 := V2V1 * V1;
    for I := 2 to 5 do
      Details.Text := StringReplace(Details.Text, 'par ' + IntToStr(I), 'par ' + IntToStr(Round(V2V1)), []);
    // Random radius 1, radius 2 calculated
    if Random(3) > 0 then begin
      // Display: radiuses
      fFluides2.laEntry.Caption := 'Rayon R' + SUB_1; fFluides2.laExit.Caption := 'Rayon R' + SUB_2;
      fFluides2.laUEntry.Caption := 'mm'; fFluides2.laUExit.Caption := 'mm';
      R1 := Random(11) + 10;
      R2 := R1 / Sqrt(V2 / V1);
    end
    else begin
      // Display: sections
      fFluides2.laEntry.Caption := 'Section S' + SUB_1; fFluides2.laExit.Caption := 'Section S' + SUB_2;
      fFluides2.laUEntry.Caption := 'mm' + SUP_2; fFluides2.laUExit.Caption := 'mm' + SUP_2;
      S1 := (Random(91) + 30) * 10;
      S2 := S1 / (V2 / V1);
      R1 := Sqrt(Pi * S1); R2 := Sqrt(Pi * S2);
    end;
    // Random angle; length calculated
    Alpha := Random(21) + 5;
    L := (R1 - R2) / Tan(DegToRad(Alpha));
  end;
  // Fill Boolean array for actually given values
  if ExType2 > 0 then
    FillGivenData(True, True, True, True, False, False, False, False, False, False, False, GData)
  else
    FillGivenData(True, True, True, False, True, False, True, False, False, False, False, GData);
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5;
  if LeftStr(fFluides2.laEntry.Caption, 5) = 'Rayon' then begin
    Data[3] := R1; Data[4] := R2;
  end
  else begin
    Data[3] := S1; Data[4] := S2;
  end;
  Data[5] := V1; Data[6] := V2; Data[7] := Alpha; Data[8] := L;
end;

{ Exercise type 2 question generation: "Vidange d'un réservoir par un orifice" }

procedure Exercise2(G: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  P1, P2, D1, D2, R1, R2, V1, V2, H, Q2, T, TH, TM: Real;
  S: string;

begin
  ExType2 := Random(3);
  S := 'Un réservoir cylindrique, ouvert à l''air libre, contient un fluide, supposé parfait, qui s''écoule par un petit orifice, situé à sa base. ';
  if ExType2 <> 2 then
    S += 'Comme D est beaucoup plus grand que d, on peut considérer que le niveau dans le réservoir ne varie que très lentement. ';
  if ExType2 = 1 then begin
    S += 'Connaissant le débit volumique Q' + SUB_2 +  ' en sortie, on veut déterminer à quelle hauteur de la surface du fluide se trouve l''orifice.'
  end
  else begin
    S += 'On veut déterminer la vitesse d''écoulement V' + SUB_2 + ' et le débit volumique Q' + SUB_2 + ' en sortie de l''orifice, ainsi que ';
    S += '(supposant ce débit constant) le temps de vidage du réservoir.';
  end;
  Details.Lines.Clear; Details.Lines.AddText(S);
  fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible := True;
  fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
  if ExType2 <> 1 then begin
    fFluides2.laSup3.Visible := True; fFluides2.edSup3.Visible := True; fFluides2.laUSup3.Visible := True;
    fFluides2.laSup3.Caption := 'Temps t'; fFluides2.laUSup3.Caption := 'hh:mm';
  end
  else  begin
    fFluides2.laSup3.Visible := False; fFluides2.edSup3.Visible := False; fFluides2.laUSup3.Visible := False;
  end;
  // Pressures = Patm
  P1 := PAtm; P2 := PAtm;
  // Random diameters
  repeat
    D1 := Random(5) + 2; D2 := (Random(11) + 10);
    R1 := D1 / 2; R2 := 1E-3 * D2 / 2;
    // Random height
    H := (Random(7) + 2) / 2;
    // Velocities and flow rates calculated
    if ExType2 <> 2 then begin
      // Calculation simplification: if D1 >> D2, fluid height in tank varies very slowly
      V1 := 0;
      V2 := Sqrt(2 * G * H);
      Q2 := V2 * Pi * Sqr(R2);
      if ExType2 = 1 then begin
        // When Q2 is given, use round number for it (need to recalculate V2 and H)
        Q2 := Round(1000 * Q2) / 1000;
        V2 := Q2 / (Pi * Sqr(R2));
        H := Sqr(V2) / (2 * G);
      end;
    end
    else begin
      // "Real" calculation (considering variation of fluid height in tank)
      V2 := Sqrt( (2 * G * H) / (1 - intpower(R2 / R1, 4) ) );
      V1 := Sqr(R2 / R1) * V2;
      Q2 := V2 * Pi * Sqr(R2);
    end;
    // Calculation of drainage time
    if Q2 < 0.001 then
      T := 0
    else
      T := Pi * Sqr(R1) * H / Q2;
  until (T <= 5 * 3600) and (Q2 >= 0.001);
  fFluides2.laEntry.Caption := 'Diamètre D'; fFluides2.laExit.Caption := 'Diamètre d';
  fFluides2.laUEntry.Caption := 'm'; fFluides2.laUExit.Caption := 'mm';
  fFluides2.laSup1.Caption := 'Hauteur H'; fFluides2.laUSup1.Caption := 'm';
  fFluides2.laSup2.Caption := 'Débit Q' + SUB_2; fFluides2.laUSup2.Caption := 'L/s';
  // Fill Boolean array for actually given values
  if ExType2 = 1 then
    FillGivenData(False, False, True, True, False, False, False, True, False, False, False, GData)
  else
    FillGivenData(False, False, True, True, False, False, True, False, False, False, False, GData);
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5;
  Data[3] := D1; Data[4] := D2;
  Data[5] := V1; Data[6] := V2;
  Data[7] := H; Data[8] := 1E+3 * Q2;
  T /= 3600;
  TH := Int(T); TM := (T - TH) * 60 / 100;
  Data[9] := Round(100 * (TH + TM)) / 100;
end;

{ Exercise type 3 question generation: "Vidange d'un réservoir par un siphon" }

procedure Exercise3(Density, G: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  P1, P2, P3, D1, D2, R2, V1, V2, H, H2, H2Max, Q2: Real;
  S: string;

begin
  ExType2 := Random(2);
  S := 'Un siphon de diamètre d est alimenté par un réservoir de grandes dimensions par rapport à d (le niveau du fluide dans le réservoir ';
  S += 'variant très lentement) et contenant un fluide, supposé parfait. Le réservoir et le siphon sont supposés ouverts à l’atmosphère. ';
  if ExType2 = 0 then begin
    S += 'On veut déterminer la vitesse d''écoulement V' + SUB_2 + ' et le débit volumique Q' + SUB_2 + ' en sortie du siphon. Quelle est ';
    S += 'la pression P' + SUB_3 + ' au point B?';
  end
  else begin
    S += 'Connaissant le débit volumique Q' + SUB_2 + ' en sortie, on veut déterminer à quelle hauteur H de la surface du fluide se trouve ';
    S += 'la sortie du siphon. Quelle est la valeur maximale que peut prendre la hauteur h?'
  end;
  Details.Lines.Clear; Details.Lines.AddText(S);
  if ExType2 = 1 then begin
    fFluides2.laSpeed1.Visible := False; fFluides2.edSpeed1.Visible := False; fFluides2.laUSpeed1.Visible := False;
    fFluides2.laSpeed2.Visible := False; fFluides2.edSpeed2.Visible := False; fFluides2.laUSpeed2.Visible := False;
  end
  else begin
    fFluides2.laSpeed1.Visible := True; fFluides2.edSpeed1.Visible := True; fFluides2.laUSpeed1.Visible := True;
    fFluides2.laSpeed2.Visible := True; fFluides2.edSpeed2.Visible := True; fFluides2.laUSpeed2.Visible := True;
  end;
  fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible := True;
  fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
  fFluides2.laSup3.Visible := True; fFluides2.edSup3.Visible := True; fFluides2.laUSup3.Visible := True;
  // Pressures = Patm
  P1 := PAtm; P2 := PAtm;
  repeat
    // Random diameters and heights
    D1 := Random(5) + 2; D2 := (Random(11) + 10);
    R2 := 1E-3 * D2 / 2;
    H := (Random(7) + 4) / 2;
    H2 := (Random(31) + 20) / 100;
    // Velocities and flow rate calculation
    V1 := 0;
    V2 := Sqrt(2 * G * H);
    Q2 := V2 * Pi * Sqr(R2);
    if ExType2 = 1 then begin
      // When Q2 is given, use round value for it (needs recalculation of V2 and H)
      Q2 := Round(1000 * Q2) / 1000;
      V2 := Q2 / (Pi * Sqr(R2));
      H := Sqr(V2) / (2 * G);
    end;
    // Calculation of P3 and max, height h
    P3 := P2 - G * Density * (H + H2);
    H2Max := PAtm / (G * Density) - H;
  until (H2 < H2Max) and (Q2 >= 0.001);
  fFluides2.laEntry.Caption := 'Diamètre D'; fFluides2.laExit.Caption := 'Diamètre d';
  fFluides2.laUEntry.Caption := 'm'; fFluides2.laUExit.Caption := 'mm';
  fFluides2.laSup1.Caption := 'Hauteur H'; fFluides2.laUSup1.Caption := 'm';
  fFluides2.laUSup2.Caption := 'm';
  fFluides2.laSup3.Caption := 'Débit Q' + SUB_2; fFluides2.laUSup3.Caption := 'L/s';
  if ExType2 = 0 then begin
    fFluides2.laPressure1.Caption := 'Pression P' + SUB_2;
    fFluides2.laPressure2.Caption := 'Pression P' + SUB_3;
    fFluides2.laSup2.Caption := 'Hauteur h'
  end
  else begin
    fFluides2.laPressure1.Caption := 'Pression P' + SUB_1;
    fFluides2.laPressure2.Caption := 'Pression P' + SUB_2;
    fFluides2.laSup2.Caption := 'h maximal';
  end;
  // Fill Boolean array for actually given values
  if ExType2 = 0 then
    FillGivenData(False, False, True, True, False, False, True, True, False, False, False, GData)
  else
    FillGivenData(False, False, True, True, False, False, False, False, True, False, False, GData);
  // Store all values in array
  if Extype2 = 0 then begin
    Data[1] := P2 / 1E+5; Data[2] := P3 / 1E+5;
  end
  else begin
    Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5;
  end;
  Data[3] := D1; Data[4] := D2;
  Data[5] := V1; Data[6] := V2;
  Data[7] := H;
  if ExType2 = 0 then
    Data[8] := H2
  else
    Data[8] := H2Max;
  Data[9] := 1E+3 * Q2;
end;

{ Exercise type 4 question generation: "Remplissage d'un bassin à l'aide d'une pompe" }

procedure Exercise4(Density, G: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  ExType2, DTop: Integer;
  P1, P2, D, H1, H2, V1, V2, Q, Eta, P: Real;
  S: string;

begin
  ExType2 := Random(2);
  S := 'Un bassin est rempli en pompant de l''eau (supposé fluide parfait) à travers une conduite de diamètre d à partir d''un lac. ';
  S += 'On suppose que la vitesse d’aspiration est égale à la vitesse de refoulement. ';
  if ExType2 = 0 then begin
    S += 'On veut déterminer la vitesse d''écoulement V' + SUB_2 + ' et, en négligeant toutes les pertes de charge, la puissance ';
    S += 'électrique de la pompe.';
  end
  else
    S += 'On veut déterminer le débit volumique Q' + SUB_2 + '  et, en négligeant toutes les pertes de charge, la puissance utile de la pompe.';
  Details.Lines.Clear; Details.Lines.AddText(S);
  fFluides2.laExit.Visible := False; fFluides2.edExit.Visible := False; fFluides2.laUExit.Visible := False;
  fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible := True;
  fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
  fFluides2.laSup3.Visible := True; fFluides2.edSup3.Visible := True; fFluides2.laUSup3.Visible := True;
  if ExType2 = 0 then begin
    DTop := 40;
    fFluides2.laSup4.Visible := True; fFluides2.edSup4.Visible := True;
  end
  else begin
    DTop := 0;
    fFluides2.laSup4.Visible := False; fFluides2.edSup4.Visible := False;
  end;
  fFluides2.laSup5.Visible := True; fFluides2.edSup5.Visible := True; fFluides2.laUSup5.Visible := True;
  // Position "Puissance" controls depending on exercise type (nicer look)
  fFluides2.laSup5.Top := fFluides2.laSpeed1.Top + DTop;
  fFluides2.edSup5.Top := fFluides2.edSpeed1.Top + DTop;
  fFluides2.laUSup5.Top := fFluides2.laUSpeed1.Top + DTop;
  // Pressions = Patm
  P1 := PAtm; P2 := PAtm;
  // Random heights
  H1 := -((Random(26) + 50)) / 10; H2 := Random(21) + 20;
  if ExType2 = 0 then begin
    // Random flow rate and diameter; velocity calculated
    repeat
      Q := ((Random(61) + 40) / 10) / 1000;
      D := Random(201) + 150;
      V2 := 4 * Q / (Pi * Sqr(D * 1E-3)); V1 := V2;
    until (V2 >= 0.25)  and (V2 <= 0.5);
    // Electric power calculation (with random η)
    Eta := (Random(11) + 75) / 100;
    P := (Q * Density * G * (Abs(H1) + H2)) / Eta;
  end
  else begin
    repeat
      // Random velocity and diameter; flow rate calculated
      V2 := (Random(26) + 25) / 100; V1 := V2;
      D := Random(201) + 150;
      Q := V2 * (Pi * Sqr(D * 1E-3) / 4);
    until (Q >= 4E-3) and (Q <= 10E-3);
    // Used power calculation
    P := Q * Density * G * (Abs(H1) + H2);
  end;
  fFluides2.laEntry.Caption := 'Diamètre d';
  fFluides2.laSup1.Caption := 'Niveau Z' + SUB_1; fFluides2.laUSup1.Caption := 'm';
  fFluides2.laSup2.Caption := 'Niveau Z' + SUB_2; fFluides2.laUSup2.Caption := 'm';
  fFluides2.laSup3.Caption := 'Débit Q' + SUB_2; fFluides2.laUSup3.Caption := 'L/s';
  if ExType2 = 0 then
    fFluides2.laSup5.Caption :=  'Puissance Pe'
  else
    fFluides2.laSup5.Caption :=  'Puissance Pu';
  // Fill Boolean array for actually given values
  if ExType2 = 0 then
    FillGivenData(True, True, True, False, False, False, True, True, True, True, False, GData)
  else
    FillGivenData(True, True, True, False, True, False, True, True, False, False, False, GData);
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5;
  Data[3] := D; Data[5] := V1; Data[6] := V2;
  Data[7] := H1; Data[8] := H2;
  Data[9] := Q * 1000;
  if ExType2 = 0 then
    Data[10] := Eta;
  Data[11] := P * 1E-3;
end;

{ Exercise type 5 question generation: "Mesure de la vitesse d'écoulement d'un fluide (Pitot)" }

procedure Exercise5(Density, G: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  P1, P2, D, H, V, Q: Real;
  S: string;

begin
  S := 'Mesure de la vitesse d''écoulement d''un fluide, supposé parfait, dans une conduite à l''aide de deux tubes de diamètre d ';
  S += 'plongeant dans le liquide. Le tube débouchant en A est orienté face au courant, celui débouchant en B le long des lignes de courant. ';
  S += 'C''est d''après ce principe que fonctionne le tube de Pitot. En supposant que l''écoulement est permanent, ';
  S += 'calculer la vitesse d''écoulement en A et en B, ainsi que le débit volumique et massique du fluide';
  Details.Lines.Clear; Details.Lines.AddText(S);
  fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible := True;
  fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
  fFluides2.laSup3.Visible := True; fFluides2.edSup3.Visible := True; fFluides2.laUSup3.Visible := True;
  // Pressions = Patm
  P1 := PAtm; P2 := PAtm;
  // Random height; calculation of velocity
  H := (Random(901) + 100) / 10;
  V := Sqrt(2 * G * H * 1E-3);
  // Random diameter; calculation of flow rate
  D := Random(51) + 25;
  Q := Pi * Sqr(D * 1E-3) / 4 * V;
  fFluides2.laSpeed1.Caption := 'Vitesse en A'; fFluides2.laSpeed2.Caption := 'Vitesse en B';
  fFluides2.laEntry.Caption := 'Diamètre d'; fFluides2.laUEntry.Caption := 'mm';
  fFluides2.laExit.Visible := False; fFluides2.edExit.Visible := False; fFluides2.laUExit.Visible := False;
  fFluides2.laSup1.Caption := 'Hauteur h'; fFluides2.laUSup1.Caption := 'mm';
  fFluides2.laSup2.Caption := 'Débit Qv'; fFluides2.laUSup2.Caption := 'L/s';
  fFluides2.laSup3.Caption := 'Débit Qm'; fFluides2.laUSup3.Caption := 'kg/s';
  // Fill Boolean array for actually given values
  FillGivenData(True, True, True, False, False, False, True, False, False, False, False, GData);
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5;
  Data[3] := D;
  Data[5] := V; Data[6] := V;
  Data[7] := H;
  Data[8] := Q * 1000;
  Data[9] := Density * Q * 1000 * 1E-3;
end;

{ Exercise type 6 question generation: "Mesure du débit d'un fluide: Tube de Venturi" }

procedure Exercise6(G: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  P1, P2, D1, D2, S1, S2, Alpha, H, V1, V2, Q: Real;
  S: string;

begin
  ExType2 := Random(2);
  S := 'Mesure de la vitesse d''écoulement d''un fluide, supposé parfait, dans un tube de Venturi, constitué d'' une conduite principale A de ';
  S += 'diamètre D et subissant un étranglement de diamètre d en B. Deux tubes plongent dans la conduite ayant leur extrémité dans A resp. dans B. ';
  if ExType2 = 1 then
    S += 'Sachant que la section de la conduite A vaut #alpha# fois celle de l''étranglement B et lisant le dénivellation h, '
  else
    S += 'Lisant le dénivellation h, ';
  S += 'calculer la vitesse du fluide dans les parties A et B du tube et le débit volumique du fluide.';
  Details.Lines.Clear; Details.Lines.AddText(S);
  fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible := True;
  fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
  // Pressions = Patm
  P1 := PAtm; P2 := PAtm;
  // Random height
  H := (Random(901) + 100) / 10;
  // Random diameters
  repeat
    D1 := Random(51) + 25; D2 := Random(51) + 25;
  until D1 >= 1.5 * D2;
  // Surface ratio calculation
  S1 := Pi * Sqr(D1 * 1E-3) / 4; S2 := Pi * Sqr(D2 * 1E-3) / 4; Alpha := S1 / S2;
  if ExType2 = 1 then begin
    // When surface ratio is given, use round value for it (need to recalculate not-given diameter)
    Alpha := Int(10 * Alpha) / 10;
    S2 := S1 / Alpha;
    D2 := Sqrt(4 * S2 / Pi) * 1E+3;
    Details.Text := StringReplace(Details.Text, '#alpha#', RFormat(Alpha), []);
  end;
  // Calculate velocities and flow rate
  V1 := Sqrt((2 * G * H * 1E-3) / (Sqr(Alpha) - 1));
  V2 := V1 * (S1 / S2);
  Q := Pi * Sqr(D1 * 1E-3) / 4 * V1;
  fFluides2.laEntry.Caption := 'Diamètre D'; fFluides2.laUEntry.Caption := 'mm';
  fFluides2.laExit.Caption := 'Diamètre d';  fFluides2.laUExit.Caption := 'mm';
  fFluides2.laSup1.Caption := 'Hauteur h'; fFluides2.laUSup1.Caption := 'mm';
  fFluides2.laSpeed1.Caption := 'Vitesse en A'; fFluides2.laSpeed2.Caption := 'Vitesse en B';
  fFluides2.laSup2.Caption := 'Débit Q'; fFluides2.laUSup2.Caption := 'L/s';
  // Fill Boolean array for actually given values
  if ExType2 = 0 then
    FillGivenData(True, True, True, True, False, False, True, False, False, False, False, GData)
  else
    FillGivenData(True, True, True, False, False, False, True, False, False, False, False, GData);
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5;
  Data[3] := D1; Data[4] := D2;
  Data[7] := H;
  Data[5] := V1; Data[6] := V2;
  Data[8] := Q * 1000;
end;

{ Exercise type 7 question generation: "Mesure du débit d'un fluide: Tube de Venturi vertical" }

procedure Exercise7(Density, G: Real; var Details: TMemo; var Data: TData; var GData: TGivenData);

var
  PA, PB, PA2, PB2, D1, D2, ZA, ZB, ZA2, ZB2, Alpha, VA, VB: Real;
  S: string;

begin
  S := 'Mesure de la vitesse d''écoulement d''un fluide, supposé parfait, dans un tube de Venturi vertical, constitué d'' une conduite ';
  S += 'principale A de diamètre D et subissant un étranglement de diamètre d en B. Deux tubes plongent dans la conduite ayant leur extrémité ';
  S += 'dans A resp. dans B. Supposant que le fluide circule de bas en haut et que la pression au niveau des surfaces libres est égale à Patm. ';
  S += 'Mesurant les niveaux Z dans les tubes piézimétriques gradués, calculer la pression aux points A et B, ainsi que la vitesse ';
  S += 'd''écoulement dans les partie A et B du tube.';
  Details.Lines.Clear; Details.Lines.AddText(S);
  fFluides2.laSup1.Visible := True; fFluides2.edSup1.Visible := True; fFluides2.laUSup1.Visible := True;
  fFluides2.laSup2.Visible := True; fFluides2.edSup2.Visible := True; fFluides2.laUSup2.Visible := True;
  fFluides2.laSup3.Visible := True; fFluides2.edSup3.Visible := True; fFluides2.laUSup3.Visible := True;
  fFluides2.laSup4.Visible := True; fFluides2.edSup4.Visible := True; fFluides2.laUSup4.Visible := True;
  // Venturi Pressions = Patm
  PA2 := PAtm; PB2 := PAtm;
  repeat
    // Random levels
    ZA := 0; ZB := (Random(26) + 25) / 100;
    ZA2 := (Random(201) + 200) / 100;  ZB2 := (Random(201) + 200) / 100;
    // Calculate tubes' pression
    PA := PA2 + Density * G * (ZA2 - ZA); PB := PB2 + Density * G * (ZB2 - ZB);
  until (ZA2 - ZB2 >= 0.5) and (PA - PB >= 0.05) and ((PA - PB) / Density > G * (ZB - ZA));
  repeat
    // Random diameters
    D1 := Random(51) + 25; D2 := Random(51) + 25;
  until D1 >= 1.5 * D2;
  // Calculate velocities
  Alpha := Sqr(D1 / D2);
  VB := Sqrt((2 / (Sqr(Alpha) - 1)) * (((PA - PB) / Density) + (G * (ZA - ZB))));
  VA := VB * (1 / Alpha);
  fFluides2.laPressure1.Caption := 'Pression en A'; fFluides2.laPressure2.Caption := 'Pression en B';
  fFluides2.laSpeed1.Caption := 'Vitesse en A'; fFluides2.laSpeed2.Caption := 'Vitesse en B';
  fFluides2.laEntry.Caption := 'Diamètre D'; fFluides2.laUEntry.Caption := 'mm';
  fFluides2.laExit.Caption := 'Diamètre d';  fFluides2.laUExit.Caption := 'mm';
  fFluides2.laSup1.Caption := 'Niveau ZA'; fFluides2.laUSup1.Caption := 'm';
  fFluides2.laSup2.Caption := 'Niveau ZB'; fFluides2.laUSup2.Caption := 'm';
  fFluides2.laSup3.Caption := 'Niveau ZA'''; fFluides2.laUSup3.Caption := 'm';
  fFluides2.laSup4.Caption := 'Niveau ZB'''; fFluides2.laUSup4.Caption := 'm';
  // Fill Boolean array for actually given values
  FillGivenData(False, False, True, True, False, False, True, True, True, True, False, GData);
  // Store all values in array
  Data[1] := PA / 1E+5; Data[2] := PB / 1E+5;
  Data[3] := D1; Data[4] := D2;
  Data[5] := VA; Data[6] := VB;
  Data[7] := ZA; Data[8] := ZB;
  Data[9] := ZA2; Data[10] := ZB2;
end;

{************}
{ TfFluides2 }
{************}

{ Application start: Initialisation }

procedure TfFluides2.FormCreate(Sender: TObject);

begin
  // Create an array containing the form edit fields
  edData[0] := edFluidDensity;
  edData[1] := edPressure1; edData[2] := edPressure2; edData[3] := edEntry; edData[4] := edExit;
  edData[5] := edSpeed1; edData[6] := edSpeed2; edData[7] := edSup1; edData[8] := edSup2;
  edData[9] := edSup3; edData[10] := edSup4; edData[11] := edSup5;
  // Apply subscripts
  laPressure1.Caption := StringReplace(laPressure1.Caption, '1', SUB_1, []);
  laPressure2.Caption := StringReplace(laPressure2.Caption, '2', SUB_2, []);
  laEntry.Caption := StringReplace(laEntry.Caption, '1', SUB_1, []);
  laExit.Caption := StringReplace(laExit.Caption, '2', SUB_2, []);
  laSpeed1.Caption := StringReplace(laSpeed1.Caption, '1', SUB_1, []);
  laSpeed2.Caption := StringReplace(laSpeed2.Caption, '2', SUB_2, []);
  // Acceleration due to gravity default value
  iG := 9.8;
  // Start-up exercise: Type 1 questions
  mExercise1.Click;
  // Start random number generator
  Randomize;
end;

{ Menu item "Exercice > Circulation d’un fluide dans une conduite": Prepare for type 1 exercises }

procedure TfFluides2.mExercise1Click(Sender: TObject);

var
  S: string;

begin
  iExType := 1;
  S := 'Un fluide, supposé parfait et à une pression donnée, est acheminé dans une conduite en position horizontale. ';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Vidange d'un réservoir par un orifice": Prepare for type 2 exercises }

procedure TfFluides2.mExercise2Click(Sender: TObject);

var
  S: string;

begin
  iExType := 2;
  S := 'Un réservoir cylindrique contient un fluide, supposé parfait, qui s''écoule par un petit orifice, situé à sa base. ';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Vidange d'un réservoir par un siphon": Prepare for type 3 exercises }

procedure TfFluides2.mExercise3Click(Sender: TObject);

var
  S: string;

begin
  iExType := 3;
  S := 'Un siphon de diamètre d est alimenté par un réservoir de grandes dimensions par rapport à d (le niveau du fluide dans le réservoir ';
  S += 'variant très lentement) et contenant un fluide, supposé parfait.';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Remplissage d'un bassin à l'aide d'une pompe": Prepare for type 4 exercises }

procedure TfFluides2.mExercise4Click(Sender: TObject);

var
  S: string;

begin
  iExType := 4;
  S := 'Un bassin est rempli en pompant de d''eau (supposé fluide parfait) à travers une conduite de diamètre d à partir d''un lac.';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Mesure de la vitesse d'écoulement d'un fluide (Pitot)": Prepare for type 5 exercises }

procedure TfFluides2.mExercise5Click(Sender: TObject);

var
  S: string;

begin
  iExType := 5;
  S := 'Mesure de la vitesse d''écoulement d''un fluide, supposé parfait, dans une conduite à l''aide de deux tubes plongeant dans le liquide.';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Mesure du débit d'un fluide: Tube de Venturi": Prepare for type 6 exercises }

procedure TfFluides2.mExercise6Click(Sender: TObject);

var
  S: string;

begin
  iExType := 6;
  S := 'Mesure de la vitesse d''écoulement d''un fluide, supposé parfait, dans un tube de Venturi, constitué d'' une conduite principale A de ';
  S += 'diamètre D et subissant un étranglement de diamètre d en B.';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Mesure du débit d'un fluide: Tube de Venturi vertical": Prepare for type 7 exercises }

procedure TfFluides2.mExercise7Click(Sender: TObject);

var
  S: string;

begin
  iExType := 7;
  S := 'Mesure de la vitesse d''écoulement d''un fluide, supposé parfait, dans un tube de Venturi vertical, constitué d'' une conduite ';
  S += 'principale A de diamètre D et subissant un étranglement de diamètre d en B.';
  NewExercise(iExType, S, edDetails, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfFluides2.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Acc. pesanteur (g) > ...": Select value for g }

procedure TfFluides2.mOptionsG980Click(Sender: TObject);

begin
  mOptionsG980.Checked := True;
  mOptionsG981.Checked := False;
  mOptionsG100.Checked := False;
  iG := 9.8;
end;

procedure TfFluides2.mOptionsG981Click(Sender: TObject);

begin
  mOptionsG980.Checked := False;
  mOptionsG981.Checked := True;
  mOptionsG981.Checked := False;
  iG := 9.81;
end;

procedure TfFluides2.mOptionsG100Click(Sender: TObject);

begin
  mOptionsG980.Checked := False;
  mOptionsG981.Checked := False;
  mOptionsG981.Checked := True;
  iG := 10;
end;

{ Menu item "Aide > Info programme": Display program about }

procedure TfFluides2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physique: Dynamique des fluides.' + LineEnding;
  S += 'Générateur d''exercices (niveau débutant).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, juillet-septembre 2020.';
  MessageDlg('Information "Fluides2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Réponse": Generate new question resp. check user's answers }

procedure TfFluides2.btQuestionClick(Sender: TObject);

var
  F1, I: Integer;
  OK: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    ClearForm(edData, aData, aUserData, aGivenData, False);                              // clear form fields; reset data array values
    // Random fluid (or water)
    if iExType in [1, 2, 3, 5] then
      F1 := Random(2)
    else
      F1 := 0;
    // Fill in fluid name and density
    edFluidName.Text := Fluids[F1].Name; edFluidDensity.Text := FloatToStr(Fluids[F1].Density);
    // Generate the exercise question
    case iExType of
      1: Exercise1(Fluids[F1].Density, edDetails, aData, aGivenData);
      2: Exercise2(iG, edDetails, aData, aGivenData);
      3: Exercise3(Fluids[F1].Density, iG, edDetails, aData, aGivenData);
      4: Exercise4(Fluids[F1].Density, iG, edDetails, aData, aGivenData);
      5: Exercise5(Fluids[F1].Density, iG, edDetails, aData, aGivenData);
      6: Exercise6(iG, edDetails, aData, aGivenData);
      7: Exercise7(Fluids[F1].Density, iG, edDetails, aData, aGivenData);
    end;
    // Fill in the values given by the program
    FillForm(aGivenData, aData, edData);
    // Increment the question counter
    Inc(iQuestion);
    // Next button push will be a user answer
    btQuestion.Caption := 'Réponse';
    // Disable "Résultat" button: User must give some answer before using it
    btShow.Enabled := False;
  end
  // Button "Réponse": Check user's answer
  else begin
    // Read user values from the form
    for I := 1 to 11 do begin
      if not edData[I].ReadOnly then begin
        // Values entered by the user
        if edData[I].Text = '' then
          aUserData[I] := -1
        else begin
          if (iExType = 2) and (I = 9) then begin
            // Transform hh:mm to real number H,M (resp. H.M)
            if (Length(edData[9].Text) = 4) and (Copy(edData[9].Text, 2, 1) = ':') then
              aUserData[I] := StrToFloat(LeftStr(edData[I].Text, 1)) + StrToFloat(RightStr(edData[I].Text, 2)) / 100
            else if (Length(edData[9].Text) = 5) and (Copy(edData[9].Text, 3, 1) = ':') then
              aUserData[I] := StrToFloat(LeftStr(edData[I].Text, 2)) + StrToFloat(RightStr(edData[I].Text, 2)) / 100;
          end
          else begin
            aUserData[I] := StrToFloat(edData[I].Text);
          end;
        end;
      end;
    end;
    OK := True;
    // Check user values: If one of them is false, the overal answer is false
    for I := 1 to 11 do begin
      if edData[I].Visible then begin;
        if not edData[I].ReadOnly then begin
          // Check user values only (not those given by the program)
          if StrToFloat(RFormat(aUserData[I])) <> StrToFloat(RFormat(aData[I])) then begin
            edData[I].Color := clRed;                                                    // mark the field as false answer
            OK := False;                                                                 // overal answer is false
          end;
        end;
      end;
    end;
    if OK then begin
      // Overal answer is correct
      Inc(iCorrect);
      imEval.Picture.LoadFromFile('correct.png');
    end
    else begin
      // Overal answer is false
      imEval.Picture.LoadFromFile('false.png');
      btShow.Enabled := True;                                                            // give user possibility to view correct answers
    end;
    imEval.Visible := True;
    // Update evaluation counters
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be a question
    btQuestion.Caption := 'Question';
  end;
end;

{ Button "Show": Display correct answers }

procedure TfFluides2.btShowClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 11 do begin
    if edData[I].Color = clRed then begin
      // Only for false user values
      edData[I].Color := clYellow;
      edData[I].Text := RFormat(aData[I]);
      if (iExType = 2) and (I = 9) then begin
        // Transform "decimal time" to hh:mm
        edData[I].Text := StringReplace(edData[I].Text, ',', ':', []);
        edData[I].Text := StringReplace(edData[I].Text, '.', ':', []);
      end;
    end;
  end;
end;

end.

