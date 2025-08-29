{************************************}
{* Main unit for Colors application *}
{************************************}

unit colors_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, colors_help;

type
  TLightColor = record
    Name: string;
    Symbol: Char;
    Colour: TColor;
  end;
  TDone = record
    Color1, Color2: Char;
  end;
  {**********}
  { TfColors }
  {**********}
  TfColors = class(TForm)
    mMenu: TMainMenu;
    mExercise, mExerciseNew, mExerciseExit: TMenuItem;
    mOptions, mOptionsLearning, mOptionsOperations, mOptionsOperationsAdd, mOptionsOperationsSub, mOptionsNoEqu, mOptionsAbsorbedColor: TMenuItem;
    mHelp, mHelpPhysics, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edQuestion, Memo1: TMemo;
    imDiagram, imBulbAdd1, imBulbAdd2, imBulbSub, imEval: TImage;
    Label9, laQuestion, laColors1, laColors2, laColor1, laColor2: TLabel;
    laColorLeft, laColorCenter, laColorRight, laEquation, laAbsorbed, laTransmitted, laEval: TLabel;
    edColor1, edColor2, edColorLeft, edColorCenter, edColorRight, edEquation, edAbsorbed, edTransmitted: TEdit;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mExerciseNewClick(Sender: TObject);
    procedure mExerciseExitClick(Sender: TObject);
    procedure mOptionsLearningClick(Sender: TObject);
    procedure mOptionsOperationsAddClick(Sender: TObject);
    procedure mOptionsOperationsSubClick(Sender: TObject);
    procedure mOptionsNoEquClick(Sender: TObject);
    procedure mOptionsAbsorbedColorClick(Sender: TObject);
    procedure mHelpPhysicsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    iQuestionsTemp, iQuestions, iQuestion, iQuestionAdd, iQuestionSub, iCorrect: Integer;
    sOperationsTemp, sOperations: string;
    cOperation: Char;
    bLearningTemp, bLearning, bNoEquationTemp, bNoEquation: Boolean;
    Bitmap: TBitmap;
    rdBulb1Color, rdBulb2Color, rdFilterColor, rdComplementary, rdAddColor, rdSubColor: TLightColor;
    aDoneAdd, aDoneSub: array of TDone;
  end;

const
  QuestionsAdd = 18; QuestionsSub = 42;
  LightColors: array[0..10] of TLightColor = (
    (Name: 'red'; Symbol: 'R'; Colour: clRed),
    (Name: 'green'; Symbol: 'G'; Colour: clLime),
    (Name: 'blue'; Symbol: 'B'; Colour: clBlue),
    (Name: 'cyan'; Symbol: 'C'; Colour: clAqua),
    (Name: 'magenta'; Symbol: 'M'; Colour: clFuchsia),
    (Name: 'yellow'; Symbol: 'Y'; Colour: clYellow),
    (Name: 'white'; Symbol: 'W'; Colour: clWhite),
    (Name: 'black'; Symbol: 'X'; Colour: clBlack),
    (Name: 'pink'; Symbol: 'r'; Colour: $8080FF),
    (Name: 'pale green'; Symbol: 'g'; Colour: $80FF80),
    (Name: 'pale blue'; Symbol: 'b'; Colour: $FF8080)
  );

var
  fColors: TfColors;

implementation

{$R *.lfm}

{ Format numbers in evaluation grid (right-alignment) }

function GFormat(N: Integer; S: string): string;

var
  GF: string;

begin
  GF := IntToStr(N);
  if N < 10 then
    GF := '  ' + GF
  else if N < 100 then
    GF := ' ' + GF;
  if S = '' then
    GF := ' ' + GF
  else
    GF += S;                                                                   // this is for the % sign
  Result := GF;
end;

{ Get complementary color (of given primary/secondary color) }

function ColorComplementary(Colour: TLightColor): TLightColor;

var
  C, I: Integer;
  Compl: Char;

begin
  case Colour.Symbol of
    'R': Compl := 'C';
    'G': Compl := 'M';
    'B': Compl := 'Y';
    'C': Compl := 'R';
    'M': Compl := 'G';
    'Y': Compl := 'B';
  end;
  for I := 0 to 10 do begin
    if Compl = LightColors[I].Symbol then
      C := I;
  end;
  Result := LightColors[C];
end;

{ Get sum of two colors }

function ColorAddition(Color1, Color2: TLightColor): TLightColor;

// The function does the addition for the following cases:
//   primary + primary
//   secondary + secondary
//   white + primary or primary + white

var
  C, I: Integer;
  Add: Char;

begin
  // One of the 2 colors is white
  if Color1.Symbol = 'W' then
    Add := LowerCase(Color2.Symbol)
  else if Color2.Symbol = 'W' then
    Add := LowerCase(Color1.Symbol)
  // Other cases
  else begin
    // Two (different) primary colors
    case Color1.Symbol of
      'R': begin
        case Color2.Symbol of
          'G': Add := 'Y';
          'B': Add := 'M';
        end;
      end;
      'G': begin
        case Color2.Symbol of
          'R': Add := 'Y';
          'B': Add := 'C';
        end;
      end;
      'B': begin
        case Color2.Symbol of
          'R': Add := 'M';
          'G': Add := 'C';
        end;
      end;
      // Two (different) secondary colors
      'M': begin
        case Color2.Symbol of
          'Y': Add := 'r';
          'C': Add := 'b';
        end;
      end;
      'Y': begin
        case Color2.Symbol of
          'M': Add := 'r';
          'C': Add := 'g';
        end;
      end;
      'C': begin
        case Color2.Symbol of
          'M': Add := 'b';
          'Y': Add := 'g';
        end;
      end;
    end;
  end;
  for I := 0 to 10 do begin
    if Add = LightColors[I].Symbol then
      C := I;
  end;
  Result := LightColors[C];
end;

{ Get difference of two colors }

function ColorSubtraction(Color1, Color2: TLightColor): TLightColor;

// The function does the subtration for the following cases:
//   primary - primary/secondary
//   secondary - primary/secondary
//   white - primary/secondary

var
  C, I: Integer;
  Colors: array[0..1] of string;

begin
  Colors[0] := Color1.Symbol; Colors[1] := Color2.Symbol;
  // Get components of secondary color or white
  for I := 0 to 1 do begin
    case Colors[I][1] of
      'C': Colors[I] := 'GB';
      'M': Colors[I] := 'RB';
      'Y': Colors[I] := 'RG';
      'W': Colors[I] := 'RGB';
    end;
  end;
  // Remove all color components, being part of the color to be subtracted
  for I := 0 to Length(Colors[1]) do
    Colors[0] := StringReplace(Colors[0], Colors[1][I], '', []);
  // Restore color as 1-character symbol (using X for no light = black)
  if Colors[0] = '' then
    Colors[0] := 'X'
  else if Colors[0] = 'GB' then
    Colors[0] := 'C'
  else if Colors[0] = 'RB' then
    Colors[0] := 'M'
  else if Colors[0] = 'RG' then
    Colors[0] := 'Y';
  // Find corresponding color record
  for I := 0 to 10 do begin
    if Colors[0][1] = LightColors[I].Symbol then
      C := I;
  end;
  Result := LightColors[C];
end;

{ Create color addition equation }

function ColorAdditionEquation(Color1, Color2, Color3: TLightColor): string;

var
  I: Integer;
  Colors: array[0..2] of string;

begin
  Colors[0] := Color1.Symbol; Colors[1] := Color2.Symbol; Colors[2] := Color3.Symbol;
  for I := 0 to 2 do begin
    case Colors[I][1] of
      'C': Colors[I] := 'GB';
      'M': Colors[I] := 'RB';
      'Y': Colors[I] := 'RG';
      'W': Colors[I] := 'RGB';
      'r': Colors[I] := 'WR';
      'g': Colors[I] := 'WG';
      'b': Colors[I] := 'WB';
    end;
    if Length(Colors[I]) = 2 then
      Colors[I] := '(' + Colors[I][1] + '+' + Colors[I][2] + ')'
    else if Length(Colors[I]) = 3 then
      Colors[I] := '(' + Colors[I][1] + '+' + Colors[I][2] + '+' + Colors[I][3] + ')';
  end;
  Result := Colors[0] + ' + ' + Colors[1] + ' = ' + Colors[2] + ' = ' + Color3.Symbol;
end;

{ Create color subtraction equation }

function ColorSubtractionEquation(Color1, Color2, Color3: TLightColor): string;

var
  I: Integer;
  Equ: string;
  Colors: array[0..2] of string;

begin
  Colors[0] := Color1.Symbol; Colors[1] := Color2.Symbol; Colors[2] := Color3.Symbol;
  for I := 0 to 2 do begin
    case Colors[I][1] of
      'C': Colors[I] := 'GB';
      'M': Colors[I] := 'RB';
      'Y': Colors[I] := 'RG';
      'W': Colors[I] := 'RGB';
    end;
    if Length(Colors[I]) = 2 then
      Colors[I] := '(' + Colors[I][1] + '+' + Colors[I][2] + ')'
    else if Length(Colors[I]) = 3 then
      Colors[I] := '(' + Colors[I][1] + '+' + Colors[I][2] + '+' + Colors[I][3] + ')';
  end;
  Equ := Colors[0] + ' - ' + Colors[1] + ' = ' + Colors[2];
  if Color3.Symbol <> Colors[2] then
    Equ += ' = ' + Color3.Symbol;
  Result := Equ;
end;

{ Clear the diagram area by displaying a black rectangle }

procedure ClearDiagram(var Diagram: TImage);

begin
  Diagram.Picture.Bitmap.Canvas.Pen.Width := 1;
  Diagram.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Diagram.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  Diagram.Picture.Bitmap.Canvas.Rectangle(0, 0, Diagram.Width, Diagram.Height);
end;

{ Clear form (edit fields, diagram, evaluation grid, bulbs) }

procedure ClearForm(var Diagram: TImage);

var
  I: Integer;

begin
  fColors.imBulbAdd1.Visible := False; fColors.imBulbAdd2.Visible := False; fColors.imBulbSub.Visible := False;
  ClearDiagram(Diagram);
  fColors.edColor1.Text := ''; fColors.edColor2.Text := '';
  fColors.edColorLeft.Text := ''; fColors.edColorRight.Text := ''; fColors.edColorCenter.Text := '';
  fColors.edAbsorbed.Text := ''; fColors.edTransmitted.Text := ''; fColors.edEquation.Text := '';
  for I := 0 to 3 do
    fColors.sgEval.Cells[1, I] := '';
  fColors.imEval.Visible := False;
end;

{ Draw color addition diagram }

procedure DrawDiagramAddition(Bulb1, Bulb2, Addition: TLightColor; var Diagram: TImage; Show: Boolean);

var
  Filename: string;
  PaperColor1, PaperColor2, PaperColorC: TColor;

begin
  ClearDiagram(Diagram);
  // Display color of paper parts depends on case: question or solution shown
  if Show then begin
    PaperColor1 := Bulb1.Colour; PaperColor2 := Bulb2.Colour; PaperColorC := Addition.Colour;
  end
  else begin
    PaperColor1 := clBlack; PaperColor2 := clBlack; PaperColorC := clBlack;
  end;
  // Show the two bulbs (actual colors)
  Filename := './bulbs/bulb_' + Bulb1.Name + '.jpg'; DoDirSeparators(Filename);
  fColors.imBulbAdd1.Picture.LoadFromFile(Filename);
  Filename := './bulbs/bulb_' + Bulb2.Name + '.jpg'; DoDirSeparators(Filename);
  fColors.imBulbAdd2.Picture.LoadFromFile(Filename);
  Diagram.Picture.Bitmap.Canvas.Pen.Color := clCream;
  // Draw the left and right paper parts
  if Show then begin
    // Solution shown:
    // The left and right parts, illuminated by a single bulb, have two different colors
    // Simple way, to avoid overlapping: draw 2 ellipses with half long axis, one behind the other
    Diagram.Picture.Bitmap.Canvas.Brush.Color := PaperColor1;
    Diagram.Picture.Bitmap.Canvas.EllipseC(Diagram.Width div 2 - 120, Diagram.Height - 100, 120, 75);
    Diagram.Picture.Bitmap.Canvas.Brush.Color := PaperColor2;
    Diagram.Picture.Bitmap.Canvas.EllipseC(Diagram.Width div 2 + 120, Diagram.Height - 100, 120, 75);
  end
  else begin
    // Question (solution not shown)
    // The left and right parts, illuminated by a single bulb, drawn in black
    // Simply draw 1 ellipse with full long axis
    Diagram.Picture.Bitmap.Canvas.Brush.Color := clBlack;
    Diagram.Picture.Bitmap.Canvas.EllipseC(Diagram.Width div 2, Diagram.Height - 100, 240, 75);
  end;
  // Draw the central paper part (illuminated by both bulbs)
  // Draw with sum color if solution is shown, black if question
  Diagram.Picture.Bitmap.Canvas.Brush.Color := PaperColorC;
  Diagram.Picture.Bitmap.Canvas.EllipseC(Diagram.Width div 2, Diagram.Height - 100, 120, 75);
  // Draw lines, delimiting the light beam emitted by the two bulbs
  // Use color of the bulb, if solution is shown, cream otherwise
  Diagram.Picture.Bitmap.Canvas.Pen.Color := Bulb1.Colour;
  Diagram.Picture.Bitmap.Canvas.Line(50, Diagram.Height - 100, 230, 72);
  Diagram.Picture.Bitmap.Canvas.Line(230, 72, 410, Diagram.Height - 100);
  Diagram.Picture.Bitmap.Canvas.Pen.Color := Bulb2.Colour;
  Diagram.Picture.Bitmap.Canvas.Line(170, Diagram.Height - 100, 340, 72);
  Diagram.Picture.Bitmap.Canvas.Line(340, 72, 530, Diagram.Height - 100);
end;

{ Draw color subtraction diagram }

procedure DrawDiagramSubtraction(Bulb, Filter, Subtraction: TLightColor; var Diagram: TImage; Show: Boolean);

var
  Filename: string;
  LightColor1, LightColor2, FilterColor: TColor;

begin
  ClearDiagram(Diagram);
  // Display color of transmitted light and filter depends on case: question or solution shown
  LightColor1 := Bulb.Colour;
  if Show then begin
    // Solution shown: Transmitted light: difference color; filter: pigment color or absorbed color (depending on user settings)
    LightColor2 := Subtraction.Colour;
    if fColors.mOptionsAbsorbedColor.Checked then
      FilterColor := ColorComplementary(Filter).Colour
    else
      FilterColor := Filter.Colour;
  end
  else begin
    // Question (solution not shown): Transmitted light: cream; filter: pigment color
    LightColor2 := clCream; FilterColor := Filter.Colour;
  end;
  // Show the bulb (actual color)
  Filename := './bulbs/bulb2_' + Bulb.Name + '.jpg'; DoDirSeparators(Filename);
  fColors.imBulbSub.Picture.LoadFromFile(Filename);
  // Draw the filter
  Diagram.Picture.Bitmap.Canvas.Pen.Color := clCream;
  Diagram.Picture.Bitmap.Canvas.Brush.Style := bsDiagCross;
  Diagram.Picture.Bitmap.Canvas.Brush.Color := FilterColor;
  Diagram.Picture.Bitmap.Canvas.Rectangle(Diagram.Width div 2 - 20, Diagram.Height div 2 - 150, Diagram.Width div 2 + 20, Diagram.Height div 2 + 150);
  // Draw the light beam, emitted by the bulb
  Diagram.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  Diagram.Picture.Bitmap.Canvas.Brush.Color := LightColor1;
  Diagram.Picture.Bitmap.Canvas.Rectangle(85, Diagram.Height div 2 - 7, Diagram.Width div 2 - 24, Diagram.Height div 2 + 8);
  // Draw the transmitted light beam (except, if there is no light transmitted)
  if LightColor2 <> clBlack then begin
    Diagram.Picture.Bitmap.Canvas.Brush.Color := LightColor2;
    Diagram.Picture.Bitmap.Canvas.Rectangle(Diagram.Width div 2 + 24, Diagram.Height div 2 - 7, Diagram.Width div 2 + 150, Diagram.Height div 2 + 8);
  end;
end;

{**********}
{ TfColors }
{**********}

{ Application start: Initialisation }

procedure TfColors.FormCreate(Sender: TObject);

begin
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := imDiagram.Width;
  Bitmap.Height := imDiagram.Height;
  //Assign the bitmap to the image component (the drawing surface)
  imDiagram.Picture.Graphic := Bitmap;
  // Variables initialisation
  iQuestionsTemp := QuestionsAdd + QuestionsSub; sOperationsTemp := '+-';
  bLearningTemp := False; bNoEquationTemp := False;
  // Start random number generator for exercises
  Randomize;
  // Prepare for a new exercise
  mExerciseNew.Click;
end;

{ Menu item "Exercise > New": Prepare for a new exercise or learning session }

procedure TfColors.mExerciseNewClick(Sender: TObject);

begin
  // User options now becoming active
  iQuestions := iQuestionsTemp; sOperations := sOperationsTemp;
  bLearning := bLearningTemp; bNoEquation := bNoEquationTemp;
  // Variables reset
  iQuestion := 0; iQuestionAdd := 0; iQuestionSub := 0; iCorrect := 0;
  SetLength(aDoneAdd, 0); SetLength(aDoneSub, 0);
  ClearForm(imDiagram);
  // Show or hide form fields (depending on exercise/learning mode)
  if bLearning then begin
    laQuestion.Caption := 'Example'; btQuestion.Caption := 'Example'; btShow.Visible := False;
    laEval.Visible := False; sgEval.Visible := False;
    edColorLeft.ReadOnly := True; edColorCenter.ReadOnly := True; edColorRight.ReadOnly := True;
    edColorLeft.TabStop := False; edColorCenter.TabStop := False; edColorRight.TabStop := False;
    edAbsorbed.ReadOnly := True; edTransmitted.ReadOnly := True; edEquation.ReadOnly := True;
    edAbsorbed.TabStop := False; edTransmitted.TabStop := False; edEquation.TabStop := False;
  end
  else begin
    laQuestion.Caption := 'Question'; btQuestion.Caption := 'Question'; btShow.Visible := True; btShow.Enabled := False;
    laEval.Visible := True; sgEval.Visible := True;
    edColorLeft.ReadOnly := False; edColorCenter.ReadOnly := False; edColorRight.ReadOnly := False;
    edColorLeft.TabStop := True; edColorCenter.TabStop := True; edColorRight.TabStop := True;
    edAbsorbed.ReadOnly := False; edTransmitted.ReadOnly := False; edEquation.ReadOnly := False;
    edAbsorbed.TabStop := True; edTransmitted.TabStop := True; edEquation.TabStop := True;
  end;
  btQuestion.Enabled := True;
end;

{ Menu item "Exercise > Exit": Exit application }

procedure TfColors.mExerciseExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Learning mode": Toggle exercise/learning mode }

procedure TfColors.mOptionsLearningClick(Sender: TObject);

begin
  if mOptionsLearning.Checked then
    mOptionsLearning.Checked := False
  else
    mOptionsLearning.Checked := True;
  bLearningTemp := mOptionsLearning.Checked;
end;

{ Menu items "Options > Color operations > ...": Choose color operations (addition or/and subtraction) to be used }

procedure TfColors.mOptionsOperationsAddClick(Sender: TObject);

begin
  iQuestionsTemp := 0; sOperationsTemp := '';
  if mOptionsOperationsAdd.Checked then begin
    mOptionsOperationsAdd.Checked := False;
    if mOptionsOperationsSub.Checked then begin
      sOperationsTemp := '-'; iQuestionsTemp := QuestionsSub;
    end;
  end
  else begin
    mOptionsOperationsAdd.Checked := True;
    if mOptionsOperationsSub.Checked then begin
      sOperationsTemp := '+-'; iQuestionsTemp := QuestionsAdd + QuestionsSub;
    end
    else begin
      sOperationsTemp := '+'; iQuestionsTemp := QuestionsAdd;
    end;
  end;
end;

procedure TfColors.mOptionsOperationsSubClick(Sender: TObject);

begin
  iQuestionsTemp := 0; sOperationsTemp := '';
  if mOptionsOperationsSub.Checked then begin
    mOptionsOperationsSub.Checked := False;
    if mOptionsOperationsAdd.Checked then begin
      sOperationsTemp := '+'; iQuestionsTemp := QuestionsAdd;
    end;
  end
  else begin
    mOptionsOperationsSub.Checked := True;
    if mOptionsOperationsAdd.Checked then begin
      sOperationsTemp := '+-'; iQuestionsTemp := QuestionsAdd + QuestionsSub;
    end
    else begin
      sOperationsTemp := '-'; iQuestionsTemp := QuestionsSub;
    end;
  end;
end;

{ Menu item "Options > Do not evaluate equation": Toggle to eveluate or not the color addition/subtraction equation }

procedure TfColors.mOptionsNoEquClick(Sender: TObject);

begin
  if mOptionsNoEqu.Checked then
    mOptionsNoEqu.Checked := False
  else
    mOptionsNoEqu.Checked := True;
  bNoEquationTemp := mOptionsNoEqu.Checked;
end;

{ Menu item "Options > Show absorbed color": Toggle to display filter with pigment color or absorbed color }

procedure TfColors.mOptionsAbsorbedColorClick(Sender: TObject);

begin
  if mOptionsAbsorbedColor.Checked then
    mOptionsAbsorbedColor.Checked := False
  else
    mOptionsAbsorbedColor.Checked := True;
end;

{ Menu item "Help > Physics help": Display color physics help text }

procedure TfColors.mHelpPhysicsClick(Sender: TObject);

begin
  fHelp.laTitle.Caption := 'Physics of colors help.';
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('physics.txt');
  fHelp.Show;
end;

{ Menu item "Help > Application help": Display "Colors" application usage text }

procedure TfColors.mHelpHelpClick(Sender: TObject);

begin
  fHelp.laTitle.Caption := '"Colors" application help.';
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display "Colors" application about }

procedure TfColors.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics exercises: Addition and subtraction of colors.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February-March 2021.';
  MessageDlg('About "Colors"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Example/Answer": Display new color addition/subtraction example resp. check user answer }

procedure TfColors.btQuestionClick(Sender: TObject);

var
  B1, B2, I: Integer;
  Equation, S: string;
  OK, ExEnd: Boolean;

begin
  // Button "Question/Example": Display new color addition/subtraction example
  if (btQuestion.Caption = 'Question') or (btQuestion.Caption = 'Example') then begin
    if sOperations = '' then begin
      MessageDlg('Invalid options', 'There is no color operation selected!', mtError, [mbOK], 0);
    end
    else begin
      // Generate (random) example (among those wanted)
      Inc(iQuestion);
      laQuestion.Caption := btQuestion.Caption + ' ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ':';
      // Choose operation (addition or subtraction)
      if Length(sOperations) = 1 then
        cOperation := sOperations[1]                                           // user selected + or -
      else
        cOperation := Copy(sOperations, Random(2) + 1, 1)[1];                  // user selected + and -
      // If all examples for one operation have been done, must use the other one
      if (cOperation = '+-') and (iQuestionAdd = QuestionsAdd) then
        cOperation := '-'
      else if (cOperation = '+-') and (iQuestionSub = QuestionsSub) then
        cOperation := '+';
      // Generate color addition example
      if cOperation = '+' then begin
        Inc(iQuestionAdd);
        laColors1.Caption := 'Bulb colors:'; laColors2.Caption := 'Paper areas colors:';
        laColor1.Caption := 'Bulb 1'; laColor2.Caption := 'Bulb 2';
        laColorLeft.Visible := True; laColorCenter.Visible := True; laColorRight.Visible := True;
        edColorLeft.Visible := True; edColorCenter.Visible := True; edColorRight.Visible := True;
        laAbsorbed.Visible := False; laTransmitted.Visible := False;
        edAbsorbed.Visible := False; edTransmitted.Visible := False;
        // Random colors for the two bulbs (but with some conditions)
        repeat
          OK := True;
          B1 := Random(7); B2 := Random(7);
          // Bulb1 = white => bulb2 = primary
          if (B1 = 6) and (B2 <> 0) and (B2 <> 1) and (B2 <> 2) then
            OK := False
          // Bulb1 = primary => bulb2 = other primary or white
          else if (B1 = 0) and (B2 <> 1) and (B2 <> 2) and (B2 <> 6) then
            OK := False
          else if (B1 = 1) and (B2 <> 0) and (B2 <> 2) and (B2 <> 6) then
            OK := False
          else if (B1 = 2) and (B2 <> 0) and (B2 <> 1) and (B2 <> 6) then
            OK := False
          // Bulb1 = secondary => bulb2 = other secondary
          else if (B1 = 3) and (B2 <> 4) and (B2 <> 5) then
            OK := False
          else if (B1 = 4) and (B2 <> 3) and (B2 <> 5) then
            OK := False
          else if (B1 = 5) and (B2 <> 3) and (B2 <> 4) then
            OK := False
          // Each example (color combination) used only once
          else begin
            if Length(aDoneAdd) > 1 then begin
              for I := 0 to Length(aDoneAdd) - 1 do begin
                if (aDoneAdd[I].Color1 = LightColors[B1].Symbol) and (aDoneAdd[I].Color2 = LightColors[B2].Symbol) then
                  OK := False;
              end;
            end;
          end;
        until OK;
        // Mark actual color combination as done
        SetLength(aDoneAdd, Length(aDoneAdd) + 1);
        aDoneAdd[Length(aDoneAdd) - 1].Color1 := LightColors[B1].Symbol;
        aDoneAdd[Length(aDoneAdd) - 1].Color2 := LightColors[B2].Symbol;
        // Display descriptive text for addition
        S := 'Two  lights are arranged above a white  sheet of paper.  When the lights are turned on,  they illuminate the entire sheet of paper.  ';
        S += 'Depending on which colors are emitted by the bulbs,  the paper will appear a different color. Determine the colors that will appear ';
        S += 'on the different parts of the sheet of paper.';
        edQuestion.Lines.Clear;
        edQuestion.Lines.AddText(S);
        // The two bulb colors
        imBulbAdd1.Visible := True; imBulbAdd2.Visible := True; imBulbSub.Visible := False;
        rdBulb1Color := LightColors[B1];
        rdBulb2Color := LightColors[B2];
        // Calculate the sum color
        rdAddColor := ColorAddition(LightColors[B1], LightColors[B2]);
        // Display the two bulb colors; clear other edit fields
        edColor1.Text := rdBulb1Color.Symbol; edColor2.Text := rdBulb2Color.Symbol;
        edColorLeft.Text := ''; edColorCenter.Text := ''; edColorRight.Text := ''; edEquation.Text := '';
        // Draw the addition diagram
        DrawDiagramAddition(rdBulb1Color, rdBulb2Color, rdAddColor, imDiagram, False);
        // Set the focus (depending on exercise/learning mode)
        if bLearning then
          btQuestion.SetFocus
        else
          edColorLeft.SetFocus;
      end
      // Generate color subtraction example
      else begin
        Inc(iQuestionSub);
        laColors1.Caption := 'Bulb and filter colors:'; laColors2.Caption := 'Absorbed and transmitted colors:';
        laColor1.Caption := 'Bulb'; laColor2.Caption := 'Filter';
        laColorLeft.Visible := False; laColorCenter.Visible := False; laColorRight.Visible := False;
        edColorLeft.Visible := False; edColorCenter.Visible := False; edColorRight.Visible := False;
        laAbsorbed.Visible := True; laTransmitted.Visible := True;
        edAbsorbed.Visible := True; edTransmitted.Visible := True;
        // Random colors for the bulb and the filter (but with some conditions)
        repeat
          OK := True;
          B1 := Random(7); B2 := Random(6);                                    // bulb = primary, secondary or white; filter = primary or secondary
          // Each example (color combination) used only once
          if Length(aDoneSub) > 1 then begin
            for I := 0 to Length(aDoneSub) - 1 do begin
              if (aDoneSub[I].Color1 = LightColors[B1].Symbol) and (aDoneSub[I].Color2 = LightColors[B2].Symbol) then
                OK := False;
            end;
          end;
        until OK;
        // Mark actual color combination as done
        SetLength(aDoneSub, Length(aDoneSub) + 1);
        aDoneSub[Length(aDoneSub) - 1].Color1 := LightColors[B1].Symbol;
        aDoneSub[Length(aDoneSub) - 1].Color2 := LightColors[B2].Symbol;
        // Display descriptive text for subtraction
        S := 'A light  bulb shines on a filter of a given color.  Depending on the color, emitted by the  bulb and the color of the filter,  ';
        S += 'the filter will appear in a different color. Determine the color absorbed and the color transmitted by the filter.';
        edQuestion.Lines.Clear;
        edQuestion.Lines.AddText(S);
        // Bulb and filter colors
        imBulbAdd1.Visible := False; imBulbAdd2.Visible := False; imBulbSub.Visible := True;
        rdBulb1Color := LightColors[B1];
        rdFilterColor := LightColors[B2];
        // Calculate color, absorbed by filter (= the one to be used in subtraction operation)
        rdComplementary := ColorComplementary(LightColors[B2]);
        // Calculate difference color
        rdSubColor := ColorSubtraction(LightColors[B1], rdComplementary);
        // Display bulb and filter color; clear other edit fields
        edColor1.Text := rdBulb1Color.Symbol; edColor2.Text := rdFilterColor.Symbol;
        edAbsorbed.Text := ''; edTransmitted.Text := ''; edEquation.Text := '';
        // Draw subtraction diagraam
        DrawDiagramSubtraction(rdBulb1Color, rdFilterColor, rdSubColor, imDiagram, False);
        // Set the focus (depending on exercise/learning mode)
        if bLearning then
          btQuestion.SetFocus
        else
          edAbsorbed.SetFocus;
      end;
      fColors.imEval.Visible := False;
      // Learning mode: Stop example generation, if all examples have been done
      // In exercise mode, this will be tested, after the user has entered a solution
      if bLearning then begin
        btShow.Visible := True; btShow.Enabled := True; btShow.Click; btShow.Visible := False;
        ExEnd := False;
        if (sOperations = '+') and (iQuestion = QuestionsAdd) then
          ExEnd := True
        else if (sOperations = '-') and (iQuestion = QuestionsSub) then
          ExEnd := True
        else if iQuestion = QuestionsAdd + QuestionsSub then
          ExEnd := True;
        // All examples done: End of learning session; otherwise, wait for user asking for next example
        if ExEnd then begin
          MessageDlg('Examples end', 'All examples have been shown.', mtInformation, [mbOK], 0);
          btQuestion.Enabled := False;
        end;
      end
      // Exercise mode: Wait for user answer
      else begin
        btQuestion.Caption := 'Answer'; btShow.Enabled := False;
      end;
    end;
  end
  // Button "Answer": Check user answer and update evaluation counters
  else begin
    Equation := StringReplace(edEquation.Text, ' ', '', [rfReplaceAll]);
    OK := True;
    // Check user answer for question is a color addition
    if cOperation = '+' then begin
      if (edColorLeft.Text = '') or (edColorRight.Text = '') or (edColorCenter.Text = '') then
        OK := False
      else if (edColorLeft.Text[1] <> rdBulb1Color.Symbol) or (edColorRight.Text[1] <> rdBulb2Color.Symbol) then
        OK := False
      else if edColorCenter.Text[1] <> rdAddColor.Symbol then
        OK := False
      else if not bNoEquation and (Equation <> StringReplace(ColorAdditionEquation(rdBulb1Color, rdBulb2Color, rdAddColor), ' ', '', [rfReplaceAll])) then
        OK := False;
    end
    // Check user answer for question is a color subtraction
    else begin
      if (edAbsorbed.Text = '') or (edTransmitted.Text = '') then
        OK := False
      else if (edAbsorbed.Text[1] <> rdComplementary.Symbol) or (edTransmitted.Text[1] <> rdSubColor.Symbol) then
        OK := False
      else if not bNoEquation and (Equation <> StringReplace(ColorSubtractionEquation(rdBulb1Color, rdComplementary, rdSubColor), ' ', '', [rfReplaceAll])) then
        OK := False;
    end;
    imEval.Visible := True;
    // Correct answer
    if OK then begin
      Inc(iCorrect);
      imEval.Picture.LoadFromFile('correct.png');
    end
    // False answer
    else begin
      imEval.Picture.LoadFromFile('false.png');
    end;
    // Update evaluation counters
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * iCorrect / iQuestion), '%');
    // Stop example generation, if all questions have been done
    ExEnd := False;
    if (sOperations = '+') and (iQuestion = QuestionsAdd) then
      ExEnd := True
    else if (sOperations = '-') and (iQuestion = QuestionsSub) then
      ExEnd := True
    else if iQuestion = QuestionsAdd + QuestionsSub then
      ExEnd := True;
    btQuestion.Caption := 'Question'; btShow.Enabled := True;
    // All examples done: End of exercise; otherwise, wait for user asking for next question
    if ExEnd then begin
      MessageDlg('Exercise end', 'All questions have been done.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Button "Show": Display and draw the correct solution }

procedure TfColors.btShowClick(Sender: TObject);

begin
  if cOperation = '+' then begin
    // Color addition
    DrawDiagramAddition(rdBulb1Color, rdBulb2Color, rdAddColor, imDiagram, True);
    edColorLeft.Text := rdBulb1Color.Symbol;
    edColorRight.Text := rdBulb2Color.Symbol;
    edColorCenter.Text := rdAddColor.Symbol;
    edEquation.Text := ColorAdditionEquation(rdBulb1Color, rdBulb2Color, rdAddColor);
  end
  else begin
    // Color subtraction
    DrawDiagramSubtraction(rdBulb1Color, rdFilterColor, rdSubColor, imDiagram, True);
    edAbsorbed.Text := rdComplementary.Symbol;
    edTransmitted.Text := rdSubColor.Symbol;
    edEquation.Text := ColorSubtractionEquation(rdBulb1Color, rdComplementary, rdSubColor);
  end;
  // Next main button push will be for a new question
  if not bLearning then begin
    imEval.Visible := False;
    btQuestion.Caption := 'Question';
  end;
end;

end.

