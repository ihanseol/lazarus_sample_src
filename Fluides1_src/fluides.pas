{**************************************}
{* Main unit for Fluides1 application *}
{**************************************}

unit fluides;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids;

type
  TFluid = record
    Name: string;
    Density: Real;
  end;
  TData = array[0..11] of Real;
  TGivenData = array[0..11] of Boolean;
  {************}
  { TfFluides1 }
  {************}
  TfFluides1 = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseTubes, mExerciseUTubeLiq2, mExerciseUTubeLiq3a: TMenuItem;
    mExerciseUTubeLiq3b, mExerciseExit: TMenuItem;
    mOptions, mOptionsG, mOptionsG980, mOptionsG981: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edDetails: TMemo;
    Label1, Label2, Label3, Label4: TLabel;
    Label5, Label6, Label7, Label8: TLabel; Label9: TLabel;
    imExercise: TImage;
    edFluidName1, edFluidDensity1, edFluidName2, edFluidDensity2: TEdit;
    laPressure1, laPressure2, laPressure3, laPressure4: TLabel;
    edPressure1, edPressure2, edPressure3, edPressure4: TEdit;
    laUPressure1, laUPressure2, laUPressure3, laUPressure4: TLabel;
    laHeight1, laHeight2, laHeight3, laHeight4, laHeight5, laHeight6, laHeight7: TLabel;
    edHeight1, edHeight2, edHeight3, edHeight4, edHeight5, edHeight6, edHeight7: TEdit;
    laUHeight1, laUHeight2, laUHeight3, laUHeight4, laUHeight5, laUHeight6, laUHeight7: TLabel;
    laAlcool, laHg: TLabel;
    sgEval: TStringGrid;
    imEval: TImage;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseTubesClick(Sender: TObject);
    procedure mExerciseUTubeLiq2Click(Sender: TObject);
    procedure mExerciseUTubeLiq3aClick(Sender: TObject);
    procedure mExerciseUTubeLiq3bClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsG980Click(Sender: TObject);
    procedure mOptionsG981Click(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iExType, iQuestion, iCorrect: Integer;
    aData, aUserData: TData;
    aGivenData: TGivenData;
    edData: array[0..11] of TEdit;
  end;

const
  Fluids: array[0..4] of TFluid = (
    (Name: 'mercure'; Density: 13.546E+3),
    (Name: 'eau'; Density: 1E+3),
    (Name: 'essence'; Density: 0.7E+3),
    (Name: 'huile'; Density: 0.85E+3),
    (Name: 'alcool'; Density: 0.8E+3)
  );

var
  fFluides1: TfFluides1;
  G: Real;

implementation

{$R *.lfm}

{ Format numbers for the stringgrid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N) + S;
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  GFormat := SN;
end;

{ Clear form fields; reset values of data arrays }

procedure ClearForm(var DataFields: array of TEdit; var Data, UserData: TData; var GData: TGivenData; ClearAll: Boolean);

var
  I: Integer;

begin
  DataFields[0].ReadOnly := True; DataFields[0].TabStop := False; DataFields[0].Color := clCream; GData[0] := True;
  fFluides1.edFluidName1.Text := ''; fFluides1.edFluidName2.Text := '';
  fFluides1.edFluidDensity1.Text := ''; fFluides1.edFluidDensity2.Text := '';
  fFluides1.laAlcool.Visible := False;
  for I := 1 to 11 do begin
    DataFields[I].Text := '';
    DataFields[I].ReadOnly := False;
    DataFields[I].TabStop := True;
    DataFields[I].Color := clDefault;
    Data[I] := -1; UserData[I] := 0;
    GData[I] := False;
  end;
  fFluides1.imEval.Picture.Clear;
  fFluides1.btQuestion.Caption := 'Question';
  if ClearAll then begin
    // If this Boolean is True, clear the evaluation grid
    for I := 0 to 3 do
      fFluides1.sgEval.Cells[1, I] := '';
  end;
end;

{ Create a Boolean array, indicating the parameters that are given by the program }

Procedure FillGivenData(B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11: Boolean; var Data: TGivenData);

begin
  Data[1] := B1; Data[2] := B2; Data[3] := B3; Data[4] := B4; Data[5] := B5; Data[6] := B6;
  Data[7] := B7; Data[8] := B8; Data[9] := B9; Data[10] := B10; Data[11] := B11;
end;

{ Fill in the form fields }

procedure FillForm(GData: TGivenData; Data: TData; var FData: array of TEdit);

var
  I: Integer;

begin
  for I := 1 to 11 do begin
    if GData[I] then begin
      // Field containing a value given by the program
      FData[I].Text := FloatToStrF(Data[I], ffFixed, 0, 3);
      FData[I].ReadOnly := True; FData[I].TabStop := False;
      FData[I].Color := clCream;
    end;
  end;
end;

{ Prepare for a new exercise series }

procedure NewExercise(ExType: Integer; ExDetails: string; var DataFields: array of TEdit;
  var Data, UserData: TData; var GData: TGivenData; out Question, Correct: Integer);

var
  Filename: string;

begin
  fFluides1.edDetails.Clear;
  fFluides1.edDetails.Lines.AddText(ExDetails);
  Filename := 'fluides' + IntTosTr(ExType) + '.jpg';
  fFluides1.imExercise.Picture.LoadFromFile(Filename);
  ClearForm(DataFields, Data, UserData, GData, True);
  fFluides1.laHeight5.Visible := False; fFluides1.laHeight6.Visible := False; fFluides1.laHeight7.Visible := False;
  fFluides1.edHeight5.Visible := False; fFluides1.edHeight6.Visible := False; fFluides1.edHeight7.Visible := False;
  fFluides1.laUHeight5.Visible := False; fFluides1.laUHeight6.Visible := False; fFluides1.laUHeight7.Visible := False;
  fFluides1.laHg.Visible := False;
  // Show fields and labels, depending on exercise type
  case ExType of
    1: begin
         fFluides1.laPressure4.Visible := False; fFluides1.edPressure4.Visible := False; fFluides1.laUPressure4.Visible := False;
         fFluides1.laHeight3.Visible := True; fFluides1.laHeight4.Visible := True;
         fFluides1.edHeight3.Visible := True; fFluides1.edHeight4.Visible := True;
         fFluides1.laUHeight3.Visible := True; fFluides1.laUHeight4.Visible := True;
         fFluides1.laPressure1.Caption := 'Pressure PA'; fFluides1.laPressure2.Caption := 'Pressure PB'; fFluides1.laPressure3.Caption := 'Pressure PC';
         fFluides1.laHeight1.Caption := 'Hauteur h1'; fFluides1.laHeight2.Caption := 'Hauteur h2';
         fFluides1.laHeight3.Caption := 'Hauteur ZD'; fFluides1.laHeight4.Caption := 'Hauteur ZE';
       end;
    2: begin
         fFluides1.laPressure4.Visible := False; fFluides1.edPressure4.Visible := False; fFluides1.laUPressure4.Visible := False;
         fFluides1.laHeight3.Visible := True; fFluides1.laHeight4.Visible := False;
         fFluides1.edHeight3.Visible := True; fFluides1.edHeight4.Visible := False;
         fFluides1.laUHeight3.Visible := True; fFluides1.LaUHeight4.Visible := False;
         fFluides1.laHeight5.Visible := True; fFluides1.laHeight6.Visible := True;
         fFluides1.edHeight5.Visible := True; fFluides1.edHeight6.Visible := True;
         fFluides1.edHeight5.Visible := True; fFluides1.edHeight6.Visible := True;
         fFluides1.laUHeight5.Visible := True; fFluides1.laUHeight6.Visible := True;
         fFluides1.laPressure1.Caption := 'Pressure P1'; fFluides1.laPressure2.Caption := 'Pressure P2'; fFluides1.laPressure3.Caption := 'Pressure P3';
         fFluides1.laHeight1.Caption := 'Hauteur Z1'; fFluides1.laHeight2.Caption := 'Hauteur Z2'; fFluides1.laHeight3.Caption := 'Hauteur Z3';
         fFluides1.laHeight5.Caption := 'Hauteur h'; fFluides1.laHeight6.Caption := 'Hauteur h´';
       end;
    3: begin
         fFluides1.laPressure4.Visible := True; fFluides1.edPressure4.Visible := True; fFluides1.laUPressure4.Visible := True;
         fFluides1.laHeight3.Visible := False; fFluides1.laHeight4.Visible := False;
         fFluides1.edHeight3.Visible := False; fFluides1.edHeight4.Visible := False;
         fFluides1.laUHeight3.Visible := False; fFluides1.laUHeight4.Visible := False;
         fFluides1.laPressure1.Caption := 'Pressure P1'; fFluides1.laPressure2.Caption := 'Pressure P2';
         fFluides1.laPressure3.Caption := 'Pressure P3'; fFluides1.laPressure4.Caption := 'Pressure P4';
         fFluides1.laHeight1.Caption := 'Hauteur h1'; fFluides1.laHeight2.Caption := 'Hauteur h2';
       end;
    4: begin
         fFluides1.laPressure4.Visible := True; fFluides1.edPressure4.Visible := True; fFluides1.laUPressure4.Visible := True;
         fFluides1.laHeight3.Visible := True; fFluides1.laHeight4.Visible := True;
         fFluides1.edHeight3.Visible := True; fFluides1.edHeight4.Visible := True;
         fFluides1.laUHeight3.Visible := True; fFluides1.laUHeight4.Visible := True;
         fFluides1.laPressure1.Caption := 'Pressure P0'; fFluides1.laPressure2.Caption := 'Pressure P1';
         fFluides1.laPressure3.Caption := 'Pressure P2'; fFluides1.laPressure4.Caption := 'Pressure P3';
         fFluides1.laHeight1.Caption := 'Hauteur Z0'; fFluides1.laHeight2.Caption := 'Hauteur Z1';
         fFluides1.laHeight3.Caption := 'Hauteur Z2'; fFluides1.laHeight4.Caption := 'Hauteur Z3';
         fFluides1.laHeight5.Caption := 'h1 (liquide 1)'; fFluides1.laHeight6.Caption := 'h2 (liquide2)'; fFluides1.laHeight7.Caption := 'h3 (mercure)';
         fFluides1.laHeight5.Visible := True; fFluides1.laHeight6.Visible := True; fFluides1.laHeight7.Visible := True;
         fFluides1.edHeight5.Visible := True; fFluides1.edHeight6.Visible := True; fFluides1.edHeight7.Visible := True;
         fFluides1.laUHeight5.Visible := True; fFluides1.laUHeight6.Visible := True; fFluides1.laUHeight7.Visible := True; fFluides1.laHg.Visible := True;
       end;
  end;
  // Reset evaluation counters
  Question := 0; Correct := 0;
end;

{ Type 1 question generation: "Réservoir avec tubes piézométriques" }

procedure ExerciseTubes(Density1, Density2: Real; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  PA, PB, PC, PD, H1, H2, ZD, ZE: Real;
  // PE: Real;

begin
  // Atmospheric pressure
  PA := 1E+5; PD := 1E+5; // PE := 1E+5; (value not used)
  // Random heights for the 2 fluids
  repeat
    H1 := (Random(61) + 20) / 100; H2 := (Random(61) + 20) / 100;
  until H1 > H2;                                                               // to be adequate with image heights
  // Calculate other values
  PB := PA + Density1 * G * H1; PC := PB + Density2 * G * H2;
  ZD := (PC - PD) / (Density2 * G); ZE := H1 + H2;
  // Random question subtaype, determining which values the program will show (resp. which ones the user has to calculate)
  ExType2 := Random(3);
  if ExType2 in [0, 1] then begin
    // H1 and H2 given (values calculated are ok)
    FillGivenData(False, False, False, False, True, True, False, False, False, False, False, GData);
  end
  else begin
    // Recalculate values for PB and PC given
    PB /= 1E+5; PC /= 1E+5;
    PB := Round(1000 * PB) / 1000; PC := Round(1000 * PC) / 1000;
    PB *= 1E+5; PC *= 1E+5;
    H1 := (PB - PA) / (Density1 * G); H2 := (PC - PB) / (Density2 * G);
    ZD := (PC - PD) / (Density2 * G); ZE := H1 + H2;
    FillGivenData(False, True, True, False, False, False, False, False, False, False, False, GData);
  end;
  // Store all values in array
  Data[1] := PA / 1E+5; Data[2] := PB / 1E+5; Data[3] := PC / 1E+5;
  Data[5] := 100 * H1; Data[6] := 100 * H2; Data[7] := 100 * ZD; Data[8] := 100 * ZE;
end;

{ Type 2 question generation: "Tube en U: 2 liquides (et 1 gaz)" }

procedure ExerciseUTubeLiq2(Density1, Density2: Real; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  P1, P2, P3, H1, H2, Z1, Z2, Z3: Real;

begin
  // Atmospheric pressure
  P1 := 1E+5;
  // Random heights for the 2 fluids and Z1 (arbitrarily) depending on them
  repeat
    H1 := Random(61) + 20; H2 := Random(61) + 20;
  until H1 > H2;                                                               // to be adequate with image heights
  Z1 := H1 + (Random(Round(H1 / 2) + 1) + Round(H1 / 2));
  H1 /= 100; H2 /= 100; Z1 /= 100;
  // Calculate other values
  P2 := P1 + Density1 * G * H1;
  P3 := P2 - Density2 * G * H2;
  Z2 := Z1 - H1; Z3 := Z2 + H2;
  // Random question subtaype, determining which values the program will show (resp. which ones the user has to calculate)
  ExType2 := Random(4);
  if ExType2 in [0, 1] then begin
    // H1 and H2 given (values calculated are ok)
    FillGivenData(False, False, False, False, True, False, False, False, True, True, False, GData);
  end
  else if ExType2 = 2 then begin
    // Recalculate values for P2 and P3 given
    P2/= 1E+5; P3 /= 1E+5;
    P2 := Round(1000 * P2) / 1000; P3 := Round(1000 * P3) / 1000;
    P2 *= 1E+5; P3 *= 1E+5;
    H1 := (P2 - P1) / (Density1 * G);
    H2 := (P2 - P3) / (Density2 * G);
    Z2 := Z1 - H1; Z3 := Z2 + H2;
    FillGivenData(False, True, True, False, True, False, False, False, False, False, False, GData)
  end
  else begin
    // Recalculate values for Z2 and Z3 given
    Z2 := Round(100 * Z2) / 100; Z3 := Round(100 * Z3) / 100;                  // use cm integer values
    H1 := Z1 - Z2; H2 := Z3 - Z2;
    P2 := P1 + Density1 * G * H1;
    P3 := P2 - Density2 * G * H2;
    FillGivenData(False, False, False, False, True, True, True, False, False, False, False, GData);
  end;
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5; Data[3] := P3 / 1E+5;
  Data[5] := 100 * Z1; Data[6] := 100 * Z2; Data[7] := 100 * Z3; Data[9] := 100 * H1; Data[10] := 100 * H2;
end;

{ Type 3 question generation: "Tube en U: 3 liquides (même base)" }

procedure ExerciseUTubeLiq3a(Fluid: string; Density1, Density2: Real; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  P1, P2, P3, P4, H1, H2, Density, DensityMin, DensityMax: Real;

begin
  // Atmospheric pressure
  P1 := 1E+5; P4 := 1E+5;
  // If fluid 1 is alcool or oil, special question: Determination of the fluid's density
  if (Fluid = 'alcool') or (Fluid = 'huile') then begin
    if Fluid = 'alcool' then begin
      DensityMin := 0.40E+3; DensityMax := 0.80E+3;                            // alcool density limits
      fFluides1.laAlcool.Visible := True;
    end
    else begin
      DensityMin := 0.82E+3; DensityMax := 0.90E+3;                            // oil density limits
    end;
    // Random heights for the 2 liquids
    // Note: Height consistency with graph may be false in this case; adding a "H1 > H2" condition to the
    //       "until" statement could make the program hang as calculated liquid 1 density may be greater than
    //       liquid 2 density (-> to adapt in a future version...)
    repeat
      H1 := (Random(100) + 10) / 100; H2 := (Random(100) + 10) / 100;
      Density := Density2 * (H2 / H1);                                         // alcool resp. oil density
    until (Density >= DensityMin) and (Density <= DensityMax)              ;   // to ensure valid density
    // Calculate pressures
    P2 := P1 + Density * G * H1; P3 := P2;
    // Given values for this type of question
    FillGivenData(False, False, False, False, True, True, False, False, False, False, False, GData);
    // Fluid 1 density has to be the value generated here
    Data[0] := Density;
  end
  // "Normal" case
  else begin
    // Random fluid heights
    repeat
      H1 := (Random(91) + 10) / 100; H2 := (Random(91) + 10) / 100;
    until H1 > H2;                                                             // to be adequate with image heights
    // Calculate pressures
    P2 := P1 + Density1 * G * H1; P3 := P2;
    // Random question subtaype, determining which values the program will show (resp. which ones the user has to calculate)
    ExType2 := Random(4);
    if ExType2 = 0 then begin
      // Recalculate heights for P2 given
      P2/= 1E+5;
      P2 := Round(1000 * P2) / 1000; P3 := P2;
      P2 *= 1E+5; P3 := P2;
      H1 := (P2 - P1) / (Density1 * G);
      H2 := (P3 - P1) / (Density2 * G);
      FillGivenData(False, True, False, False, False, False, False, False, False, False, False, GData);
    end
    else if ExType2 = 1 then begin
      // Recalculate heights for P3 given
      P3/= 1E+5;
      P3 := Round(1000 * P3) / 1000; P2 := P3;
      P3 *= 1E+5; P2 := P3;
      H1 := (P2 - P1) / (Density1 * G);
      H2 := (P3 - P1) / (Density2 * G);
      FillGivenData(False, False, True, False, False, False, False, False, False, False, False, GData);
    end
    else begin
      // H1 and H2 given: calculated values are ok
      FillGivenData(False, False, False, False, True, True, False, False, False, False, False, GData);
    end;
    // Fluid 1 density is the original value (passed as argument)
    Data[0] := Density1;
  end;
  // Store all values in array
  Data[1] := P1 / 1E+5; Data[2] := P2 / 1E+5; Data[3] := P3 / 1E+5; Data[4] := P4 / 1E+5;
  Data[5] := 100 * H1; Data[6] := 100 * H2;
end;

{ Type 4 question generation: "Tube en U: 3 liquides (base différente)" }

procedure ExerciseUTubeLiq3b(Density1, Density2: Real; var Data: TData; var GData: TGivenData);

var
  ExType2: Integer;
  P0, P1, P2, P3, H1, H2, H3, Z0, Z1, Z2, Z3: Real;

begin
  // Atmospheric pressure
  P0 := 1E+5; P3 := 1E+5;
  // Random values for the 4 Z-value heights
  repeat
    Z0 := Random(71) + 80; Z3 := Random(51) + 50;
    Z2 := Random(51) + 25; Z1 := Random(41) + 10;
  until (Z0 - Z3 >= 5) and (Z3 - Z2 >= 20) and (Z2 - Z1 >= 5) and (Z2 - Z1 <= 20);  // to be adequate with image heights
  Z0 /= 100; Z1 /= 100; Z2 /= 100; Z3 /= 100;
  // Calculate all other values
  H1 := Z0 - Z1; H2 := Z3 - Z2; H3 := Z2 - Z1;
  P1 := P0 + Density1 * G * H1;
  P2 := P3 + Density2 * G * H2;
  // Random question subtaype, determining which values the program will show (resp. which ones the user has to calculate)
  ExType2 := Random(3);
  fFluides1.laHeight1.Caption := 'Hauteur Z0'; fFluides1.laHeight2.Caption := 'Hauteur Z1';
  fFluides1.laHeight3.Caption := 'Hauteur Z2'; fFluides1.laHeight4.Caption := 'Hauteur Z3';
  fFluides1.laHeight4.Visible := True; fFluides1.edHeight4.Visible := True; fFluides1.laUHeight4.Visible := True;
  if ExType2 = 0 then begin
    // Recalculate values for Z0, H1, H2, H3 given
    H1 := Round(100 * H1) / 100; H2 := Round(100 * H2) / 100; H3 := Round(100 * H3) / 100;  // use cm integer values
    Z1 := Z0 - H1; Z2 := H3 + Z1; Z3 := H2 + Z2;
    P1 := P0 + Density1 * G * H1;
    P2 := P3 + Density2 * G * H2;
    FillGivenData(False, False, False, False, True, False, False, False, True, True, True, GData);
  end
  else if ExType2 = 1 then begin
    // Z0, Z1, Z2, Z3 given: calculated values are ok
    FillGivenData(False, False, False, False, True, True, True, True, False, False, False, GData);
  end
  else begin
    // Recalculate values for Z0, P1 and P2 given
      P1 /= 1E+5; P2 /= 1E+5;
      P1 := Round(1000 * P1) / 1000; P2 := Round(1000 * P2) / 1000;
      P1 *= 1E+5; P2 *= 1E+5;
      H1 := (P1 - P0) / (Density1 * G);
      H2 := (P2 - P3) / (Density2 * G);
      H3 := (P1 - P2) / (Fluids[0].Density * G);
      Z1 := Z0 - H1; Z2 := H3 + Z1; Z3 := H2 + Z2;
    FillGivenData(False, True, True, False, True, False, False, False, False, False, False, GData);
  end;
  // Store all values in array
  Data[1] := P0 / 1E+5; Data[2] := P1 / 1E+5; Data[3] := P2 / 1E+5; Data[4] := P3 / 1E+5;
  Data[5] := 100 * Z0; Data[6] := 100 * Z1; Data[7] := 100 * Z2; Data[8] := 100 * Z3;
  Data[9] := 100 * H1; Data[10] := 100 * H2; Data[11] := 100 * H3;
end;

{************}
{ TfFluides1 }
{************}

{ Application start: Initialisation }

procedure TfFluides1.FormCreate(Sender: TObject);

begin
  // Create an array containing the form edit fields
  edData[0] := edFluidDensity1;
  edData[1] := edPressure1; edData[2] := edPressure2; edData[3] := edPressure3; edData[4] := edPressure4;
  edData[5] := edHeight1; edData[6] := edHeight2; edData[7] := edHeight3; edData[8] := edHeight4;
  edData[9] := edHeight5; edData[10] := edHeight6; edData[11] := edHeight7;
  // Acceleration due to gravity default value
  G := 9.8;
  // Start-up exercise: Type 3 questions
  mExerciseUTubeLiq3a.Click;
  // Random number generator
  Randomize;
end;

{ Menu item "Exercice > Réservoir avec tubes piézométriques": New type 1 exercise }

procedure TfFluides1.mExerciseTubesClick(Sender: TObject);

var
  S: string;

begin
    iExType := 1;
    S := 'Un réservoir, équipé de deux tubes piézométriques, contient deux liquides non miscibles: "liquide 1" en haut et ';
    S += '"liquide 2" en bas. La pression au-dessus de la surface libre (PA) et celles au-dessus de la surface des liquides ';
    S += 'dans les tubes (PD et PE) sont égales à la pression atmosphérique.';
    NewExercise(iExType, S, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Tube en U: 2 liquides (et 1 gaz)": New type 2 exercise }

procedure TfFluides1.mExerciseUTubeLiq2Click(Sender: TObject);

var
  S: string;

begin
  iExType := 2;
  S := 'Un tube en U fermé à une extrémité contient deux liquides non miscibles: "liquide 1" entre les surfaces (1) et (2) ';
  S += 'et "liquide 2" entre les surfaces (2) et (3). La pression au-dessus de la surface libre (P1) est égale à la pression ';
  S += 'atmosphérique. La branche fermée emprisonne un gaz à une pression P3.';
  NewExercise(iExType, S, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Tube en U: 3 liquides (même base)": New type 3 exercise }

procedure TfFluides1.mExerciseUTubeLiq3aClick(Sender: TObject);

var
  S: string;

begin
  iExType := 3;
  S := 'Un tube en U contient du mercure sur une hauteur de quelques centimètres. Dans la branche gauche, on verse un liquide ';
  S += '"liquide 1". Puis, on verse dans la branche droite un liquide "liquide 2" jusqu''à ce que les deux surfaces du mercure ';
  S += 'reviennent dans un même plan horizontal. Les pressions au-dessus de la surface des deux liquides (P1 et P4) sont égales ';
  S += 'à la pression atmosphérique.';
  NewExercise(iExType, S, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Tube en U: 3 liquides (base différente)": New type 4 exercise }

procedure TfFluides1.mExerciseUTubeLiq3bClick(Sender: TObject);

var
  S: string;

begin
  iExType := 4;
  S := 'Un tube en U contient en bas du mercure (masse volumique = 13546 kg/m³). Dans la branche gauche, on verse une quantité ';
  S += 'donnée d''un liquide "liquide 1", dans la branche droite une quantité donnée d''un liquide "liquide 2". Les bases des deux ';
  S += 'colonnes de liquide restent différentes (les deux surfaces du mercure ne reviennent pas dans un même plan horizontal). Les ';
  S += 'pressions au-dessus de la surface des deux liquides (P0 et P3) sont égales à la pression atmosphérique.';
  NewExercise(iExType, S, edData, aData, aUserData, aGivenData, iQuestion, iCorrect);
end;

{ Menu item "Exercice > Quitter": Exit application }

procedure TfFluides1.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Acc. pesanteur (g) > ...": Select value for g }

procedure TfFluides1.mOptionsG980Click(Sender: TObject);

begin
  mOptionsG980.Checked := True;
  mOptionsG981.Checked := False;
  G := 9.8;
end;

procedure TfFluides1.mOptionsG981Click(Sender: TObject);

begin
  mOptionsG980.Checked := False;
  mOptionsG981.Checked := True;
  G := 9.81;
end;

{ Menu item "Aide > Info programme": Display program about }

procedure TfFluides1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physique: Statique des fluides.' + LineEnding;
  S += 'Générateur d''exercices (niveau débutant).' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, avril-mai 2019.';
  MessageDlg('Information "Fluides1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Réponse": Generate new question resp. check user's answers }

procedure TfFluides1.btQuestionClick(Sender: TObject);

var
  F1, F2, FT, I: Integer;
  OK: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    ClearForm(edData, aData, aUserData, aGivenData, False);                    // clear form fields; reset data array values
    // Random fluids
    if iExType in [1, 2] then begin
      // Type 1 and 2 questions: 1 of the fluids must be Hg or H2O
      repeat
        F1 := Random(5);
        if F1 >= 2 then
          F2 := Random(2)
        else
          F2 := Random(5);
      until F1 <> F2;
      // Fluid with greater density must be below the other one
      if Fluids[F1].Density > Fluids[F2].Density then begin
        FT := F1; F1 := F2; F2 := FT;
      end;
      aGivenData[0] := True;                                                   // density of fluid 1 is given by the program
    end
    else begin
      // Type 2 and 3 questions: Random fluids
      repeat
        F1 := Random(4) + 1; F2 := Random(4) + 1;
      until F1 <> F2;
    end;
    // Fill in fluid names and densities
    edFluidName1.Text := Fluids[F1].Name; edFluidDensity1.Text := FloatToStr(Fluids[F1].Density);
    edFluidName2.Text := Fluids[F2].Name; edFluidDensity2.Text := FloatToStr(Fluids[F2].Density);
    // Special case for type 3 exercise:
    // If fluid 1 is alcool or oil, consider to let user calculate its density
    if iExType = 3 then begin
      if (Fluids[F1].Name = 'alcool') or (Fluids[F1].Name = 'huile') then begin
        // Make corresponding field editable
        edFluidDensity1.ReadOnly := False; edFluidDensity1.TabStop := True; edFluidDensity1.Color := clDefault;
        edFluidDensity1.Text := '';
      end;
    end;
    // Generate the exercise question
    case iExType of
      1: ExerciseTubes(Fluids[F1].Density, Fluids[F2].Density, aData, aGivenData);
      2: ExerciseUTubeLiq2(Fluids[F1].Density, Fluids[F2].Density, aData, aGivenData);
      3: ExerciseUTubeLiq3a(Fluids[F1].Name, Fluids[F1].Density, Fluids[F2].Density, aData, aGivenData);
      4: ExerciseUTubeLiq3b(Fluids[F1].Density, Fluids[F2].Density, aData, aGivenData);
    end;
    // Fill in the values given by the program
    FillForm(aGivenData, aData, edData);
    // Increment the question counter
    Inc(iQuestion);
    // Next button push will be a user answer
    btQuestion.Caption := 'Réponse';
  end
  // Button "Réponse": Check user's answer
  else begin
    // Read user values from the form
    for I := 0 to 11 do begin
      if edData[I].ReadOnly then
        // Values given by the program
        aUserData[I] := -1
      else begin
        // Values entered by the user
        if edData[I].Text = '' then
          aUserData[I] := 0
        else
          aUserData[I] := StrToFloat(edData[I].Text);
      end;
    end;
    OK := True;
    // Check user values: If one of them is false, the overal answer is false
    for I := 0 to 11 do begin
      if edData[I].Visible then begin;
        if not edData[I].ReadOnly then begin
          // Check user values only (not those given by the program)
          if Round(1000 * aUserData[I]) <> Round(1000 * aData[I]) then begin
            // User answer (taken with 3 decimal digits) is false
            edData[I].Text := FloatToStrF(aData[I], ffFixed, 0, 3);            // display correct value
            edData[I].Color := clRed;                                          // mark the field as false answer
            OK := False;                                                       // overal answer is false
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
    end;
    imEval.Visible := True;
    // Update evaluation values
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be a question
    btQuestion.Caption := 'Question';
  end;
end;

end.

