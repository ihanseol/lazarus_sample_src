{******************************************}
{* Main unit for ZueleRaetsel application *}
{******************************************}

unit raetsel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids;

type
  TRowNumbers = array[0..2, 0..5] of Integer;
  TNumberShapes = array[0..3, 0..5] of TShape;
  TQuestion = record
    QNumbers: array[0..2] of Integer;
    QShapes: array[0..2] of TShapeType;
    QColors: array[0..2] of TColor;
  end;
  {***********}
  { TfRaetsel }
  {***********}
  TfRaetsel = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings, mSettingsLevel, mSettingsShapes, mSettingsShapesShapes, mSettingsShapesColors: TMenuItem;
    mSettingsLevel1, mSettingsLevel2, mSettingsLevel3, mSettingsLevel4: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    laOperator00, laOperator01, laOperator03, laOperator20, laOperator10, laOperator11: TLabel;
    laOperator13, laOperator21, laOperator23, laOperator30, laOperator31, laOperator33: TLabel;
    shN00, shN01, shN02, shN04, shN05, shN06: TShape;
    shN10, shN11, shN12, shN14, shN15, shN16: TShape;
    shN20, shN21, shN22, shN24, shN25, shN26: TShape;
    shN30, shN31, shN32, shN34, shN35, shN36: TShape;
    edResult0, edResult1, edResult2, edResult3: TEdit;
    shShape0, shShape1, shShape2: TShape;
    edAnswer0, edAnswer1, edAnswer2, edAnswerR: TEdit;
    imEval0, imEval1, imEval2, imEval3: TImage;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsLevel1Click(Sender: TObject);
    procedure mSettingsLevel2Click(Sender: TObject);
    procedure mSettingsLevel3Click(Sender: TObject);
    procedure mSettingsLevel4Click(Sender: TObject);
    procedure mSettingsShapesShapesClick(Sender: TObject);
    procedure mSettingsShapesColorsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    iLevel, iLevelTemp, iResult, iQuestion, iCorrect: Integer;
    aRowNumbers: TRowNumbers;
    aLastRowNumbers: array[0..5] of Integer;
    dQuestion: TQuestion;
    shNumberShapes: TNumberShapes;
    shShapes: array[0..2] of TShape;
    edAnswers, edResults: array[0..3] of TEdit;
    imEvals: array[0..3] of TImage;
  end;

const
  aColors: array[0..5] of TColor = (
    clRed, clBlue, clLime, clYellow, clFuchsia, clAqua
  );
  aShapeTypes: array[0..5] of TShapeType = (
    stSquare, stCircle, stTriangle, stStar, stDiamond, stTriangleDown
  );

var
  fRaetsel: TfRaetsel;

implementation

{$R *.lfm}

{ Format numbers for grid (right-align) }

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
    SN := ' ' + SN
  else
    SN += S;                                                                   // this is for '%' sign
  Result := SN;
end;

{ Fill-in TQuestion record = create number shape for given row and column }

procedure CreateNumberShape(Question: TQuestion; Row, Col, IX, ColLeft, PLeft: Integer; var NumberShapes: TNumberShapes; var RowNumbers: TRowNumbers);

begin
  NumberShapes[Row, Col].Visible := True;
  NumberShapes[Row, Col].Shape := Question.QShapes[IX];
  NumberShapes[Row, Col].Brush.Color := Question.QColors[IX];
  RowNumbers[Row, Col] := Question.QNumbers[IX];
  NumberShapes[Row, ColLeft].Left := 8 + ColLeft * 192 + PLeft;                // different left position for case with 1 resp. 2 shapes in column
end;

{***********}
{ TfRaetsel }
{***********}

{ Application start: Initialisation }

procedure TfRaetsel.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create arrays with shapes, edit fields and "evaluation" images
  shNumberShapes[0, 0] := shN00; shNumberShapes[0, 1] := shN01; shNumberShapes[0, 2] := shN02;
  shNumberShapes[0, 3] := shN04; shNumberShapes[0, 4] := shN05; shNumberShapes[0, 5] := shN06;
  shNumberShapes[1, 0] := shN10; shNumberShapes[1, 1] := shN11; shNumberShapes[1, 2] := shN12;
  shNumberShapes[1, 3] := shN14; shNumberShapes[1, 4] := shN15; shNumberShapes[1, 5] := shN16;
  shNumberShapes[2, 0] := shN20; shNumberShapes[2, 1] := shN21; shNumberShapes[2, 2] := shN22;
  shNumberShapes[2, 3] := shN24; shNumberShapes[2, 4] := shN25; shNumberShapes[2, 5] := shN26;
  shNumberShapes[3, 0] := shN30; shNumberShapes[3, 1] := shN31; shNumberShapes[3, 2] := shN32;
  shNumberShapes[3, 3] := shN34; shNumberShapes[3, 4] := shN35; shNumberShapes[3, 5] := shN36;
  edResults[0] := edResult0; edResults[1] := edResult1; edResults[2] := edResult2; edResults[3] := edResult3;
  shShapes[0] := shShape0; shShapes[1] := shShape1; shShapes[2] := shShape2;
  edAnswers[0] := edAnswer0; edAnswers[1] := edAnswer1; edAnswers[2] := edAnswer2; edAnswers[3] := edAnswerR;
  imEvals[0] := imEval0; imEvals[1] := imEval1; imEvals[2] := imEval2; imEvals[3] := imEval3;
  // Clear "evaluation" pictures
  for I := 0 to 3 do
    imEvals[I].Picture.Clear;
  // Start random number generator
  Randomize;
  // Start new Level 1 game
  iLevelTemp := 1;
  mGameNew.Click;
end;

{ Menu item "Rätsel > Nei": Start new game }

procedure TfRaetsel.mGameNewClick(Sender: TObject);

var
  I, J: Integer;

begin
  iLevel := iLevelTemp;                                                        // level selected now becomes active
  // Display "blank" gaming board
  for I := 0 to 3 do begin
    for J := 0 to 2 do begin
      shNumberShapes[I, J].Shape := stRectangle; shNumberShapes[I, J].Brush.Color := clWhite;
      shNumberShapes[I, J].Left := 8 + J * 192 + 27;
    end;
    for J := 3 to 5 do
      shNumberShapes[I, J].Visible := False;
    if I < 3 then
      edResults[I].Text := '';
    edAnswers[I].Text := '';
  end;
  // Reset evaluation counters
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  iQuestion := 0; iCorrect := 0;
  // Set buttons for game start
  btQuestion.Caption := 'Start';
  btShow.Enabled := False;
end;

{ Menu item "Rätsel > Verloossen": Exit application }

procedure TfRaetsel.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Astellungen > Schwieregkeet > ...": Select game level }

procedure TfRaetsel.mSettingsLevel1Click(Sender: TObject);

begin
  mSettingsLevel1.Checked := True; mSettingsLevel2.Checked := False; mSettingsLevel3.Checked := False; mSettingsLevel4.Checked := False;
  iLevelTemp := 1;
end;

procedure TfRaetsel.mSettingsLevel2Click(Sender: TObject);

begin
  mSettingsLevel1.Checked := False; mSettingsLevel2.Checked := True; mSettingsLevel3.Checked := False; mSettingsLevel4.Checked := False;
  iLevelTemp := 2;
end;

procedure TfRaetsel.mSettingsLevel3Click(Sender: TObject);

begin
  mSettingsLevel1.Checked := False; mSettingsLevel2.Checked := False; mSettingsLevel3.Checked := True; mSettingsLevel4.Checked := False;
  iLevelTemp := 3;
end;

procedure TfRaetsel.mSettingsLevel4Click(Sender: TObject);

begin
  mSettingsLevel1.Checked := False; mSettingsLevel2.Checked := False; mSettingsLevel3.Checked := False; mSettingsLevel4.Checked := True;
  iLevelTemp := 4;
end;

{ Menu items "Astellungen > Zueleformen > ...": Select how to differentiate the 3 shapes (shape or/and color) }

procedure TfRaetsel.mSettingsShapesShapesClick(Sender: TObject);

begin
  if mSettingsShapesShapes.Checked then
    mSettingsShapesShapes.Checked := False
  else
    mSettingsShapesShapes.Checked := True;
end;

procedure TfRaetsel.mSettingsShapesColorsClick(Sender: TObject);

begin
  if mSettingsShapesColors.Checked then
    mSettingsShapesColors.Checked := False
  else
    mSettingsShapesColors.Checked := True;
end;

{ Menu item "Hëllef > Iwwer": Display application about }

procedure TfRaetsel.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Zuelerätsel:' + LineEnding;
  S += 'Wéi eng Ziffere verstoppe sech hannert dee verschiddene Formen?' + LineEnding + LineEnding;
  S += 'Versioun 1.0, © allu, Mee 2020.';
  MessageDlg('Iwwer "Zuelerätsel"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Weider/Äntwert": Generate new question resp. check user answers }

procedure TfRaetsel.btQuestionClick(Sender: TObject);

const
  Equ2ShapeCounts: array[0..4, 0..2] of Integer = (
    (1, 1, 1),
    (1, 1, 2),
    (1, 2, 1),
    (1, 2, 2),
    (2, 2, 1)
  );
  Equ3ShapeCounts: array[0..6, 0..2] of Integer = (
    (1, 1, 1),
    (1, 1, 2),
    (2, 1, 1),
    (2, 1, 2),
    (1, 2, 1),
    (1, 2, 2),
    (2, 2, 1)
  );

var
  Answer, Res, R, N, N1, N2, EX1, EX2, EX3, I, J, J1, J2, J3: Integer;
  S: string;
  OK, Correct, MustBeSame, MustBeDifferent: Boolean;
  LastRowNumbers, ThisRowNumbers: array[0..3] of Integer;

begin
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Weider') then begin
    if not mSettingsShapesShapes.Checked and not mSettingsShapesColors.Checked then begin
      S := 'Et si keng Zueleformen Astellunge gemeet!' + LineEnding + ' Op "verschidde Formen" gesat.';
      MessageDlg('Auswielfehler', S, mtWarning, [mbOK], 0);
      mSettingsShapesShapes.Click;
    end;
    Inc(iQuestion);
    // Hide second group of shapes (and make visible as needed later)
    for I := 0 to 3 do begin
      for J := 3 to 5 do
        shNumberShapes[I, J].Visible := False
    end;
    // Clear answer fields and "evaluation" images
    for I := 0 to 3 do begin
      edAnswers[I].Text := '';
      imEvals[I].Picture.Clear;
    end;
    // Reset result-calculation numbers
    for I := 0 to 5 do
      aLastRowNumbers[I] := 0;
    // Generate the 3 dQuestion records (the 3 shapes' data)
    for I := 0 to 2 do begin
      repeat
        OK := True;
        // Number, that they stand for (1 - 9)
        dQuestion.QNumbers[I] := Random(9) + 1;
        // Shape type: random or circle
        if mSettingsShapesShapes.Checked then
          dQuestion.QShapes[I] := aShapeTypes[Random(6)]
        else
          dQuestion.QShapes[I] := stCircle;
        // Shape color: random or blue
        if mSettingsShapesColors.Checked then
          dQuestion.QColors[I] := aColors[Random(6)]
        else
          dQuestion.QColors[I] := clBlue;
        // Numbers, shape types and colors must be unique!
        if I > 0 then begin
          for J := 0 to I - 1 do begin
            if dQuestion.QNumbers[I] = dQuestion.QNumbers[J] then
              OK := False
            else if mSettingsShapesShapes.Checked and (dQuestion.QShapes[I] = dQuestion.QShapes[J]) then
              OK := False
            else if mSettingsShapesColors.Checked and (dQuestion.QColors[I] = dQuestion.QColors[J]) then
              OK := False
          end;
        end;
      until OK;
    end;
    // "Result" row with all 3 shapes, placed at random positions
    repeat
      LastRowNumbers[0] := Random(3); LastRowNumbers[1] := Random(3); LastRowNumbers[2] := Random(3);
    until (LastRowNumbers[0] <> LastRowNumbers[1]) and (LastRowNumbers[0] <> LastRowNumbers[2]) and (LastRowNumbers[1] <> LastRowNumbers[2]);
    // Level 1 questions
    // -----------------
    if iLevel = 1 then begin
      for I := 0 to 2 do begin
        for J := 0 to 2 do
          CreateNumberShape(dQuestion, I, J, I, J, 27, shNumberShapes, aRowNumbers);
      end;
    end
    // Level 2 and 3 questions
    // -----------------------
    else if (iLevel = 2) or (iLevel = 3) then begin
      // One (random positioned) row with 3 same shapes ("first"-row shapes)
      R := Random(3);                                                          // random position of "first"-row shapes row
      ThisRowNumbers[0] := Random(3);                                          // random shape for this row
      for J := 0 to 2 do
        CreateNumberShape(dQuestion, R, J, ThisRowNumbers[0], J, 27, shNumberShapes, aRowNumbers);
      if iLevel = 3 then begin
        // Level 3: Add or not the column's 2nd shape (same as "first"-row shapes)
        for J := 3 to 5 do begin
          if Random(2) = 0 then
            CreateNumberShape(dQuestion, R, J, ThisRowNumbers[0], J - 3, 0, shNumberShapes, aRowNumbers);
        end;
      end;
      // Fill-in of the two remaining rows
      repeat
        ThisRowNumbers[1] := Random(3); ThisRowNumbers[2] := Random(3);
      until (ThisRowNumbers[1] <> ThisRowNumbers[0]) and (ThisRowNumbers[2] <> ThisRowNumbers[0]) and (ThisRowNumbers[1] <> ThisRowNumbers[2]);
      MustBeSame := False;                                                     // will be set true, if the two not-"first"-row shapes must be identical
      MustBeDifferent := False;                                                // will be set true, if the two not-"first"-row shapes must be different
      for I := 0 to 2 do begin
        if I <> R then begin
          // Only applies to the two remaining rows (one being already filled)
          repeat
            J1 := Random(3); J2 := Random(3); J3 := Random(3);                 // random shape positions within the actual row
          until (J1 <> J2) and (J1 <> J3) and (J2 <> J3);
          // Case 1: Two "first"-row shapes + one random (different from this one)
          if Random(2) = 0 then begin
            // If both rows are case 1, the not-"first"-row shapes in the two rows have to be different!
            if MustBeDifferent then
              N1 := ThisRowNumbers[2]                                          // second row
            else
              N1 := ThisRowNumbers[1];                                         // first row
            // Create the shapes for the actual row
            CreateNumberShape(dQuestion, I, J1, ThisRowNumbers[0], J1, 27, shNumberShapes, aRowNumbers);
            CreateNumberShape(dQuestion, I, J2, ThisRowNumbers[0], J2, 27, shNumberShapes, aRowNumbers);
            CreateNumberShape(dQuestion, I, J3, N1, J3, 27, shNumberShapes, aRowNumbers);
            if iLevel = 3 then begin
              // Level 3: Add or not the column's 2nd shape
              if Random(2) = 0 then begin
                // "First"-row shape added to one (random) of the not-"first"-row shapes rows
                if Random(2) = 0 then
                  J := J1
                else
                  J := J2;
                CreateNumberShape(dQuestion, I, J + 3, ThisRowNumbers[0], J, 0, shNumberShapes, aRowNumbers);
              end;
              if Random(2) = 0 then begin
                // One random shape added to "first"-row shapes row
                N1 := ThisRowNumbers[Random(3)];
                CreateNumberShape(dQuestion, I, J3 + 3, N1, J3, 0, shNumberShapes, aRowNumbers);
              end;
            end;
            MustBeDifferent := True;                                           // if another row of this case, must use the shape not used here!
          end
          // Case 2: One "first"-row shape + two random (different from this one)
          else begin
            // This ensures that, if there has been a case 1 before, the shapes here will be different form the one used there
            // (otherwise could be that one shape wouldn't be used at all)
            if MustBeDifferent then begin
              N1 := ThisRowNumbers[2]; N2 := ThisRowNumbers[1];
            end
            else begin
              N1 := ThisRowNumbers[1]; N2 := ThisRowNumbers[2];
            end;
            // If both rows are case 2, the two not-"first"-row shapes in this row must be identical
            if MustBeSame then begin
              if Random(2) = 0 then
                N2 := N1
              else
                N1 := N2;
            end;
            CreateNumberShape(dQuestion, I, J3, ThisRowNumbers[0], J3, 27, shNumberShapes, aRowNumbers);
            CreateNumberShape(dQuestion, I, J1, N1, J1, 27, shNumberShapes, aRowNumbers);
            CreateNumberShape(dQuestion, I, J2, N2, J2, 27, shNumberShapes, aRowNumbers);
            if iLevel = 3 then begin
              // Level 3: Add or not the column's 2nd shape
              for J := 3 to 5 do begin
                if Random(3) = 0 then begin
                  if MustBeSame then begin
                    // Simple way to avoid 1 resp. different shapes where 0 resp. 1 different shapes have to be...
                    if Random(2) = 0 then
                      N1 := ThisRowNumbers[0];
                  end
                  else begin
                    // Any of the 3 shapes will be ok in this case
                    N1 := Random(3);
                  end;
                  CreateNumberShape(dQuestion, I, J, N1, J - 3, 0, shNumberShapes, aRowNumbers);
                end;
              end;
            end;
            MustBeSame := True;                                                // if another row of this case, must use two same shapes in that row!
          end;
        end;
      end;
    end
    // Level 4 questions
    // -----------------
    else begin
      repeat
        ThisRowNumbers[0] := Random(3); ThisRowNumbers[1] := Random(3); ThisRowNumbers[2] := Random(3);
      until (ThisRowNumbers[1] <> ThisRowNumbers[0]) and (ThisRowNumbers[2] <> ThisRowNumbers[0]) and (ThisRowNumbers[1] <> ThisRowNumbers[2]);
      // Case 1: System of 3 equations with 3 variables
      if Random(3) = 0 then begin
        EX1 := Random(5);                                    // random item of equation-array for first row
        EX2 := EX1; EX3 := EX1;                              // these variables will be used to ensure uniqueness of equation-array items
        for I := 0 to 2 do begin
          // Each row with all 3 shapes present
          for J := 0 to 2 do begin
            N := ThisRowNumbers[J];
            CreateNumberShape(dQuestion, I, J, N, J, 27, shNumberShapes, aRowNumbers);
            if Equ3ShapeCounts[EX1, J] = 2 then begin
              // "Coefficient" = 2: Unhide 2nd column's shape
              CreateNumberShape(dQuestion, I, J + 3, N, J, 0, shNumberShapes, aRowNumbers);
            end;
          end;
          // Equation-array item for next row (must be different from those used before!)
          repeat
            EX1 := Random(5);
          until (EX1 <> EX2) and (EX1 <> EX3);
          EX3 := EX2; EX2 := EX1;
        end;
      end
      // Case 2: System of 2 equations with 2 variables, plus one row with all 3 shapes present
      else begin
        // Randomly positioned row with all 3 shapes present
        R := Random(3);
        for J := 0 to 2 do begin
          CreateNumberShape(dQuestion, R, J, ThisRowNumbers[J], J, 27, shNumberShapes, aRowNumbers);
          if Random(3) = 0 then begin
            // For each column: Add or not any random shape
            CreateNumberShape(dQuestion, R, J + 3, ThisRowNumbers[Random(3)], J, 0, shNumberShapes, aRowNumbers);
          end;
        end;
        // Complete the two remaining rows (2-by-2 system)
        repeat
          N1 := ThisRowNumbers[Random(3)]; N2 := ThisRowNumbers[Random(3)];    // two random (and different) shapes
        until N1 <> N2;
        EX1 := Random(5);                                                      // random item of equation-array for first row
        EX2 := EX1;                                                            // this variable will be used to ensure uniqueness of equation-array items
        for I := 0 to 2 do begin
          if I <> R then begin
            // Only applies to the remaining rows (one row already being done)
            for J := 0 to 2 do begin
              // Use one of the two shapes once, the other twice
              if J = 0 then
                N := N1
              else
                N := N2;
              CreateNumberShape(dQuestion, I, J, N, J, 27, shNumberShapes, aRowNumbers);
              if Equ2ShapeCounts[EX1, J] = 2 then begin
                // "Coefficient" = 2: Unhide 2nd column's shape
                CreateNumberShape(dQuestion, I, J + 3, N, J, 0, shNumberShapes, aRowNumbers);
              end;
            end;
            // Equation-array item for next row (must be different from the one used before!)
            repeat
              EX1 := Random(5);
            until EX1 <> EX2;
          end;
        end;
      end;
    end;
    // Calculate and didplay row results
    for I := 0 to 2 do begin
      Res := aRowNumbers[I, 0];
      if shNumberShapes[I, 3].Visible then
        Res += aRowNumbers[I, 3];
      for J := 0 to 1 do begin
        Res += aRowNumbers[I, J + 1];
        if shNumberShapes[I, J + 1 + 3].Visible then
          Res += aRowNumbers[I, J + 1 + 3];
      end;
      edResults[I].Text := IntToStr(Res);
    end;
    // Create shapes of last row ("result row"): All 3 shapes present
    for J := 0 to 2 do begin
      CreateNumberShape(dQuestion, 3, J, LastRowNumbers[J], J, 27, shNumberShapes, aRowNumbers);
      if (iLevel <> 1) and ((iLevel = 4) or (Random(3) = 0)) then begin
        // Add or not the second column's shape (always same as first)
        CreateNumberShape(dQuestion, 3, J + 3, LastRowNumbers[J], J, 0, shNumberShapes, aRowNumbers);
      end;
    end;
    // Display the 3 actual shapes (near user answer input fields)
    for I := 0 to 2 do begin
      shShapes[I].Shape := dQuestion.QShapes[I];
      shShapes[I].Brush.Color := dQuestion.QColors[I];
    end;
    // Set buttons for user answer
    edAnswer0.SetFocus;
    btQuestion.Caption := 'Änwert';
    btShow.Enabled := False;
  end
  // Button "Äntwert": Check user answer
  else begin
    Correct := True;
    // Check value of each of the 3 shapes
    for I := 0 to 2 do begin
      if edAnswers[I].Text = '' then
        Answer := -1
      else
        Answer := StrToInt(edAnswers[I].Text);
      if Answer = dQuestion.QNumbers[I] then begin
        imEvals[I].Picture.LoadFromFile('correct.png');
      end
      else begin
        imEvals[I].Picture.LoadFromFile('false.png');
        Correct := False;
      end;
    end;
    // Calculate result of last row and check versus value entered by user
    iResult := aLastRowNumbers[0];
    for I := 0 to 4 do
      iResult += aLastRowNumbers[I + 1];
    if edAnswerR.Text = '' then
      Answer := -1
    else
      Answer := StrToInt(edAnswerR.Text);
    if Answer = iResult then begin
      imEvals[3].Picture.LoadFromFile('correct.png');
    end
    else begin
      imEvals[3].Picture.LoadFromFile('False.png');
      Correct := False;
    end;
    // Update evaluation counters
    if Correct then
      Inc(iCorrect);
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // In case of false answer, give user possibility to view the correct one
    if not Correct then
      btShow.Enabled := True;
    // Set button to "next question"
    btQuestion.Caption := 'Weider';
  end;
end;

{ Button "Léisung": Show the solution }

procedure TfRaetsel.btShowClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to 2 do
    edAnswers[I].Text := IntToStr(dQuestion.QNumbers[I]);
  edAnswerR.Text := IntToStr(iResult);
  btShow.Enabled := False;
end;

end.

