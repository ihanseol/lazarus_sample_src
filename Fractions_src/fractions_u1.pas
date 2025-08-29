{***************************************}
{* Main unit for Fractions application *}
{***************************************}

unit fractions_u1;

// Version history:
//  Version 1.0 (September 2018): Original program
//  Version 1.0.1 (December 2023)
//    - Usage of different colors (better contrast with numbers)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, Grids, fractions_u2;

type
  TGrid = array[1 .. 8, 1 .. 8] of TShape;
  TGridFractions = array[1 .. 8, 1 .. 8] of TLabel;
  TFraction = record
    Numerator, Denominator: Integer;
  end;
  TFractions = array[1 .. 8, 1 .. 8] of TFraction;
  TFractionCounts = array[1 .. 9, 2 .. 10] of Integer;
  TAnswer = array of TFraction;
  {***************}
  {* TfFractions *}
  {***************}
  TfFractions = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsQuestions: TMenuItem;
    mSettingsGrid, mSettingsGrid3, mSettingsGrid4, mSettingsGrid5, mSettingsGrid6, mSettingsGrid8: TMenuItem;
    mSettingsOpMax, mSettingsOpMax2, mSettingsOpMax3, mSettingsOpMax4, MenuItem1, mSettingsOpMaxAlways: TMenuItem;
    mSettingsDenominators, mSettingsDenominators2, mSettingsDenominators3: TMenuItem;
    mSettingsNoHalfs, mSettingsNoTenths: TMenuItem;
    mSettingsColor: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    shGrid11, shGrid12, shGrid13, shGrid14, shGrid15, shGrid16, shGrid17, shGrid18: TShape;
    shGrid21, shGrid22, shGrid23, shGrid24, shGrid25, shGrid26, shGrid27, shGrid28: TShape;
    shGrid31, shGrid32, shGrid33, shGrid34, shGrid35, shGrid36, shGrid37, shGrid38: TShape;
    shGrid41, shGrid42, shGrid43, shGrid44, shGrid45, shGrid46, shGrid47, shGrid48: TShape;
    shGrid51, shGrid52, shGrid53, shGrid54, shGrid55, shGrid56, shGrid57, shGrid58: TShape;
    shGrid61, shGrid62, shGrid63, shGrid64, shGrid65, shGrid66, shGrid67, shGrid68: TShape;
    shGrid71, shGrid72, shGrid73, shGrid74, shGrid75, shGrid76, shGrid77, shGrid78: TShape;
    shGrid81, shGrid82, shGrid83, shGrid84, shGrid85, shGrid86, shGrid87, shGrid88: TShape;
    edAnswer: TEdit;
    imEval: TImage;
    stTitle: TStaticText;
    memoInstructions, memoGInstructions: TMemo;
    StaticText2: TStaticText;
    sgEval: TStringGrid;
    btQuestion: TButton;
    btUndo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsGrid3Click(Sender: TObject);
    procedure mSettingsGrid4Click(Sender: TObject);
    procedure mSettingsGrid5Click(Sender: TObject);
    procedure mSettingsGrid6Click(Sender: TObject);
    procedure mSettingsGrid8Click(Sender: TObject);
    procedure mSettingsOpMax2Click(Sender: TObject);
    procedure mSettingsOpMax3Click(Sender: TObject);
    procedure mSettingsOpMax4Click(Sender: TObject);
    procedure mSettingsOpMaxAlwaysClick(Sender: TObject);
    procedure mSettingsDenominators2Click(Sender: TObject);
    procedure mSettingsDenominators3Click(Sender: TObject);
    procedure mSettingsNoHalfsClick(Sender: TObject);
    procedure mSettingsNoTenthsClick(Sender: TObject);
    procedure mSettingsColorClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btUndoClick(Sender: TObject);
    procedure shGrid11MouseDown(Sender: TObject);
    procedure shGrid12MouseDown(Sender: TObject);
    procedure shGrid13MouseDown(Sender: TObject);
    procedure shGrid14MouseDown(Sender: TObject);
    procedure shGrid15MouseDown(Sender: TObject);
    procedure shGrid16MouseDown(Sender: TObject);
    procedure shGrid17MouseDown(Sender: TObject);
    procedure shGrid18MouseDown(Sender: TObject);
    procedure shGrid21MouseDown(Sender: TObject);
    procedure shGrid22MouseDown(Sender: TObject);
    procedure shGrid23MouseDown(Sender: TObject);
    procedure shGrid24MouseDown(Sender: TObject);
    procedure shGrid25MouseDown(Sender: TObject);
    procedure shGrid26MouseDown(Sender: TObject);
    procedure shGrid27MouseDown(Sender: TObject);
    procedure shGrid28MouseDown(Sender: TObject);
    procedure shGrid31MouseDown(Sender: TObject);
    procedure shGrid32MouseDown(Sender: TObject);
    procedure shGrid33MouseDown(Sender: TObject);
    procedure shGrid34MouseDown(Sender: TObject);
    procedure shGrid35MouseDown(Sender: TObject);
    procedure shGrid36MouseDown(Sender: TObject);
    procedure shGrid37MouseDown(Sender: TObject);
    procedure shGrid38MouseDown(Sender: TObject);
    procedure shGrid41MouseDown(Sender: TObject);
    procedure shGrid42MouseDown(Sender: TObject);
    procedure shGrid43MouseDown(Sender: TObject);
    procedure shGrid44MouseDown(Sender: TObject);
    procedure shGrid45MouseDown(Sender: TObject);
    procedure shGrid46MouseDown(Sender: TObject);
    procedure shGrid47MouseDown(Sender: TObject);
    procedure shGrid48MouseDown(Sender: TObject);
    procedure shGrid51MouseDown(Sender: TObject);
    procedure shGrid52MouseDown(Sender: TObject);
    procedure shGrid53MouseDown(Sender: TObject);
    procedure shGrid54MouseDown(Sender: TObject);
    procedure shGrid55MouseDown(Sender: TObject);
    procedure shGrid56MouseDown(Sender: TObject);
    procedure shGrid57MouseDown(Sender: TObject);
    procedure shGrid58MouseDown(Sender: TObject);
    procedure shGrid61MouseDown(Sender: TObject);
    procedure shGrid62MouseDown(Sender: TObject);
    procedure shGrid63MouseDown(Sender: TObject);
    procedure shGrid64MouseDown(Sender: TObject);
    procedure shGrid65MouseDown(Sender: TObject);
    procedure shGrid66MouseDown(Sender: TObject);
    procedure shGrid67MouseDown(Sender: TObject);
    procedure shGrid68MouseDown(Sender: TObject);
    procedure shGrid71MouseDown(Sender: TObject);
    procedure shGrid72MouseDown(Sender: TObject);
    procedure shGrid73MouseDown(Sender: TObject);
    procedure shGrid74MouseDown(Sender: TObject);
    procedure shGrid75MouseDown(Sender: TObject);
    procedure shGrid76MouseDown(Sender: TObject);
    procedure shGrid77MouseDown(Sender: TObject);
    procedure shGrid78MouseDown(Sender: TObject);
    procedure shGrid81MouseDown(Sender: TObject);
    procedure shGrid82MouseDown(Sender: TObject);
    procedure shGrid83MouseDown(Sender: TObject);
    procedure shGrid84MouseDown(Sender: TObject);
    procedure shGrid85MouseDown(Sender: TObject);
    procedure shGrid86MouseDown(Sender: TObject);
    procedure shGrid87MouseDown(Sender: TObject);
    procedure shGrid88MouseDown(Sender: TObject);
  private
    iQuestions, iQuestionsTemp, iQuestion, iCorrect, iFalse, iGridSize, iGridSizeTemp: Integer;
    iMaxOperands, iOperands, iDenominators, iQDenominators: Integer;
    bOpMaxAlways, bNoHalfs, bNoTenths, bGridColored: Boolean;
    aFractions: TFractions;
    aGrid: TGrid;
    aGridFractions: TGridFractions;
    aAnswer: TAnswer;
  end;

const
  Instructions1 = 'Add #o fractions in order to get a result of 1.';
  Instructions2 = 'There must be at least #d different denominators!';

var
  fFractions: TfFractions;

implementation

{$R *.lfm}

{ Greatest Common Divisor }

function GCD (N1, N2: Integer): Integer;

var
  N0, NR : INTEGER;

begin
  N0 := 1;
  repeat
    NR := N1 mod N2;
    if NR = 0 then
      N0 := N2
    else begin
      N1 := N2;
      N2 := NR;
    end;
  until NR = 0;
  GCD := N0;
end;

{ Least Common Multiple }

function LCM (N1, N2: Integer): Integer;

var
  G : Integer;

begin
  G := GCD(N1, N2);
  LCM := (N1 * N2) div G;
end;

{ Addition of two fractions }

procedure FractionAdd(F1, F2: TFraction; out R: TFraction);

begin
  R.Denominator := LCM(F1.Denominator, F2.Denominator);
  R.Numerator := (F1.Numerator * (R.Denominator div F1.Denominator)) + (F2.Numerator * (R.Denominator div F2.Denominator));
end;

{ Test if a fraction is equal to 1 }

function FractionIsOne(F: Tfraction): Boolean;

var
  IsOne: Boolean;

begin
  if F.Numerator = F.Denominator then
    IsOne := True
  else
    IsOne := False;
  FractionIsOne := IsOne;
end;

{ Format numbers in evaluation grid (right-alignment) }

function GridFormat(N: Integer; S: string): string;

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
    GF += S;
  GridFormat := GF;
end;

{ Create the fractions grid }

procedure CreateGrid(GridSize: Integer; Grid: TGrid; var Fractions: TFractions; GFractions: TGridFractions; Colored: Boolean);

const
  Colours: array[2 .. 10] of TColor = (
    $D7EBFA, $F0FFF0, $98FB98, $2FFFAD, $8CE6F0, $00D7FF, $00FFFF, $507FFF, $CBC0FF
  );

var
  I, J: Integer;
  Colour: TColor;

begin
  for J := 1 to 8 do begin
    for I := 1 to 8 do begin
      // Do this for squares included in actual grid
      if (J <= GridSize) and (I <= GridSize) then begin
        if Fractions[J, I].Numerator = 0 then
          // No fraction actually (is the case when a new game has been selected )
          GFractions[J, I].Caption := ''
        else begin
          // Normal case
          GFractions[J, I].Caption := IntToStr(Fractions[J, I].Numerator) + '/' + IntToStr(Fractions[J, I].Denominator);
          if Fractions[J, I].Denominator < 10 then
            GFractions[J, I].Caption := ' ' + GFractions[J, I].Caption;        // used to center the fraction in the square
        end;
        Grid[J, I].Visible := True;
        if not Colored or (Fractions[J, I].Numerator = 0) then
          // All squares lime, if colored squares not selected or no fraction (start of new game)
          Colour := clLime
        else
          // Square color dpending on fraction's denominator
          Colour := Colours[Fractions[J, I].Denominator];
        Grid[J, I].Brush.Color := Colour;
        GFractions[J, I].Visible := True;
      end
      // Do this for squares not included in actual grid
      else begin
        Grid[J, I].Visible := False;
        GFractions[J, I].Visible := False;
      end;
    end;
  end;
end;

{ Display game instructions }

procedure DisplayInstructions(Operands, Denominators: Integer; DisplayOperands: Boolean);

var
  S: string;

begin
  S := Instructions1 + ' ' + Instructions2;
  if DisplayOperands then
    S := StringReplace(S, '#o', IntToStr(Operands), [])
  else
    S := StringReplace(S, '#o ', '', []);
  if not DisplayOperands then
    S := StringReplace(S, '#d', IntToStr(Denominators), [])
  else begin
    if (Operands = 2) or ((Operands = 3) and (Denominators = 3)) then
      S := StringReplace(S, 'at least #d', IntToStr(Denominators), [])
    else
      S := StringReplace(S, '#d', IntToStr(Denominators), [])
  end;
  fFractions.memoInstructions.Clear;
  fFractions.memoInstructions.Lines.AddText(S);
end;

{ Reset all fractions (for given grid) to 0; clear the fraction lables }

procedure ResetFractions(N: Integer; out Fractions: TFractions; GridFractions: TGridFractions);

var
  I, J: Integer;


begin
  for J := 1 to N do begin
    for I := 1 to N do begin
      Fractions[J, I].Numerator := 0;
      Fractions[J, I].Denominator := 1;
      GridFractions[J, I].Caption := '';
      GridFractions[J, I].Transparent := True;
      GridFractions[J, I].Font.Color := clDefault;
      GridFractions[J, I].Font.Style := [];
    end;
  end;
end;

{ Prepare for starting a new game }

procedure NewGame(QuestionsTemp, GridSizeTemp, Denominators: Integer; out Questions, Question, QCorrect, QFalse, GridSize: Integer; Grid: TGrid; out Fractions: TFractions; GFractions: TGridFractions);

var
  I: Integer;

begin
  Questions := QuestionsTemp; GridSize := GridSizeTemp;                        // game parameters chosen now becoming active
  Question := 0; QCorrect := 0; QFalse := 0;
  ResetFractions(8, Fractions, GFractions);                                    // set all fractions to 0
  CreateGrid(GridSize, Grid, Fractions, GFractions, False);                    // create the (empty) grid
  fFractions.edAnswer.Width := (GridSize - 1) * 50;
  fFractions.edAnswer.Text := '';
  fFractions.imEval.Left := 10 + GridSize * 50 - 28;
  if GridSize = 3 then begin                                                   // default edit field size would be to small for 4 fractions...
    fFractions.edAnswer.Width := fFractions.edAnswer.Width + 50;
    fFractions.imEval.Left := fFractions.imEval.Left + 50;
  end;
  fFractions.imEval.Visible := False;
  DisplayInstructions(0, Denominators, False);                                 // display game instructions
  for I := 0 to 3 do
    fFractions.sgEval.Cells[1, I] := '';
  fFractions.btQuestion.Caption := 'Question';
end;

{ Select fraction (as indicated by the square that has been clicked by user) }

procedure FractionSelect(Y, X: Integer; var Answer: TAnswer; var Fractions: TFractions; GridFractions: TGridFractions);

begin
  // Squares having only to "react" if a user answer is awaited
  if fFractions.btQuestion.Caption = 'Answer' then begin
    // Max. of fractions being used
    if Length(Answer) < 4 then begin
      // Select the fraction, unless it already has been selected
      if GridFractions[Y, X].Font.Style = [] then begin
        SetLength(Answer, Length(Answer) + 1);
        // Store the fraction value in array
        Answer[Length(Answer) - 1].Numerator := Fractions[Y, X].Numerator;
        Answer[Length(Answer) - 1].Denominator := Fractions[Y, X].Denominator;
        // Highlight the fraction on the grid
        GridFractions[Y, X].Color := clBlack;
        GridFractions[Y, X].Font.Color := clWhite;
        GridFractions[Y, X].Font.Style := [fsBold];
        // Add fraction to answer field
        if fFractions.edAnswer.Text <> '' then
          fFractions.edAnswer.Text := fFractions.edAnswer.Text + ' + ';
        fFractions.edAnswer.Text := fFractions.edAnswer.Text + Trim(GridFractions[Y, X].Caption);
      end
      // Display message if fraction already has been used
      else
        MessageDlg('Invalid selection', 'You already have chosen this fraction!', mtError, [mbOK], 0);
    end
    else
      MessageDlg('Invalid selection', 'You may not choose more than 4 fractions!', mtError, [mbOK], 0);
  end;
end;

{***************}
{* TfFractions *}
{***************}

{ Application start: Initialisation }

procedure TfFractions.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  // Create array with grid squares
  aGrid[1, 1] := shGrid11; aGrid[1, 2] := shGrid12; aGrid[1, 3] := shGrid13; aGrid[1, 4] := shGrid14;
  aGrid[1, 5] := shGrid15; aGrid[1, 6] := shGrid16; aGrid[1, 7] := shGrid17; aGrid[1, 8] := shGrid18;
  aGrid[2, 1] := shGrid21; aGrid[2, 2] := shGrid22; aGrid[2, 3] := shGrid23; aGrid[2, 4] := shGrid24;
  aGrid[2, 5] := shGrid25; aGrid[2, 6] := shGrid26; aGrid[2, 7] := shGrid27; aGrid[2, 8] := shGrid28;
  aGrid[3, 1] := shGrid31; aGrid[3, 2] := shGrid32; aGrid[3, 3] := shGrid33; aGrid[3, 4] := shGrid34;
  aGrid[3, 5] := shGrid35; aGrid[3, 6] := shGrid36; aGrid[3, 7] := shGrid37; aGrid[3, 8] := shGrid38;
  aGrid[4, 1] := shGrid41; aGrid[4, 2] := shGrid42; aGrid[4, 3] := shGrid43; aGrid[4, 4] := shGrid44;
  aGrid[4, 5] := shGrid45; aGrid[4, 6] := shGrid46; aGrid[4, 7] := shGrid47; aGrid[4, 8] := shGrid48;
  aGrid[5, 1] := shGrid51; aGrid[5, 2] := shGrid52; aGrid[5, 3] := shGrid53; aGrid[5, 4] := shGrid54;
  aGrid[5, 5] := shGrid55; aGrid[5, 6] := shGrid56; aGrid[5, 7] := shGrid57; aGrid[5, 8] := shGrid58;
  aGrid[6, 1] := shGrid61; aGrid[6, 2] := shGrid62; aGrid[6, 3] := shGrid63; aGrid[6, 4] := shGrid64;
  aGrid[6, 5] := shGrid65; aGrid[6, 6] := shGrid66; aGrid[6, 7] := shGrid67; aGrid[6, 8] := shGrid68;
  aGrid[7, 1] := shGrid71; aGrid[7, 2] := shGrid72; aGrid[7, 3] := shGrid73; aGrid[7, 4] := shGrid74;
  aGrid[7, 5] := shGrid75; aGrid[7, 6] := shGrid76; aGrid[7, 7] := shGrid77; aGrid[7, 8] := shGrid78;
  aGrid[8, 1] := shGrid81; aGrid[8, 2] := shGrid82; aGrid[8, 3] := shGrid83; aGrid[8, 4] := shGrid84;
  aGrid[8, 5] := shGrid85; aGrid[8, 6] := shGrid86; aGrid[8, 7] := shGrid87; aGrid[8, 8] := shGrid88;
  // Create array with newly created label objects
  for J := 1 to 8 do begin
    for I := 1 to 8 do begin
      aGridFractions[J, I] := TLabel.Create(aGridFractions[J, I]);
      aGridFractions[J, I].Parent := Self;
      aGridFractions[J, I].Font.Size := 9;
      aGridFractions[J, I].Caption := '';
      aGridFractions[J, I].Left := aGrid[J, I].Left + 14;
      aGridFractions[J, I].Top := aGrid[J, I].Top + 17;
    end;
  end;
  // Default startup values
  iQuestionsTemp := 10; iGridSizeTemp := 5; iMaxOperands := 3; iDenominators := 2;
  bOpMaxAlways := False; bNoHalfs := False; bNoTenths := False; bGridColored := False;
  SetLength(aAnswer, 0);
  // Prepare new game
  mGameNew.Click;
  // Start random number generator
  Randomize;
end;

{ Menu item "Game > New" : Start new game }

procedure TfFractions.mGameNewClick(Sender: TObject);

begin
  NewGame(iQuestionsTemp, iGridSizeTemp, iDenominators, iQuestions, iQuestion, iCorrect, iFalse, iGridSize, aGrid, aFractions, aGridFractions);
end;

{ Menu item "Game > Exit" : Exit application }

procedure TfFractions.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of questions...": Get number of questions from user }

procedure TfFractions.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Fraction addition', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
end;

{ Menu items "Settings > Grid size > ...": Choose grid size (number of fractions displayed) }

procedure TfFractions.mSettingsGrid3Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := True;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 3;
end;

procedure TfFractions.mSettingsGrid4Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := True;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 4;
end;

procedure TfFractions.mSettingsGrid5Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := True;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 5;
end;

procedure TfFractions.mSettingsGrid6Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := True;
  mSettingsGrid8.Checked := False;
  iGridSizeTemp := 6;
end;

procedure TfFractions.mSettingsGrid8Click(Sender: TObject);

begin
  mSettingsGrid3.Checked := False;
  mSettingsGrid4.Checked := False;
  mSettingsGrid5.Checked := False;
  mSettingsGrid6.Checked := False;
  mSettingsGrid8.Checked := True;
  iGridSizeTemp := 8;
end;

{ Menu items "Settings > Number of operands > ...": Choose maximum number of operands (fractions) }

procedure TfFractions.mSettingsOpMax2Click(Sender: TObject);

begin
  if mSettingsDenominators3.Checked then begin
    // Not possible to have 3 denominators with 2 operands: Auto-correct settings
    MessageDlg('Inconsistent selection', 'Number of different denominators reset to 2!', mtWarning, [mbOK], 0);
    mSettingsDenominators2.Checked := True;
    mSettingsDenominators3.Checked := False;
    iDenominators := 2;
  end;
  mSettingsOpMax2.Checked := True;
  mSettingsOpMax3.Checked := False;
  mSettingsOpMax4.Checked := False;
  iMaxOperands := 2;
end;

procedure TfFractions.mSettingsOpMax3Click(Sender: TObject);

begin
  mSettingsOpMax2.Checked := False;
  mSettingsOpMax3.Checked := True;
  mSettingsOpMax4.Checked := False;
  iMaxOperands := 3;
end;

procedure TfFractions.mSettingsOpMax4Click(Sender: TObject);

begin
  mSettingsOpMax2.Checked := False;
  mSettingsOpMax3.Checked := False;
  mSettingsOpMax4.Checked := True;
  iMaxOperands := 4;
end;

procedure TfFractions.mSettingsOpMaxAlwaysClick(Sender: TObject);

// If this option is checked the selected maximum will always be used, otherwise number of operands will be 2 .. max

begin
  if mSettingsOpMaxAlways.Checked then
    mSettingsOpMaxAlways.Checked := False
  else
    mSettingsOpMaxAlways.Checked := True;
  bOpMaxAlways := mSettingsOpMaxAlways.Checked;
end;

{ Menu items "Settings > Different denominators > ...": Choose minimum number of different denominators }

procedure TfFractions.mSettingsDenominators2Click(Sender: TObject);

begin
  mSettingsDenominators2.Checked := True;
  mSettingsDenominators3.Checked := False;
  iDenominators := 2;
end;

procedure TfFractions.mSettingsDenominators3Click(Sender: TObject);

begin
  if mSettingsOpMax2.Checked then
    // Not possible to have 3 denominators with 2 operands: Error message
    MessageDlg('Invalid selection', 'You can''t have 3 denominators with 2 operands!', mtError, [mbOK], 0)
  else begin
    mSettingsDenominators2.Checked := False;
    mSettingsDenominators3.Checked := True;
    iDenominators := 3;
  end;
end;

{ Menu item "Settings > Exclude halfs": Choose if halfs will be available or not }

procedure TfFractions.mSettingsNoHalfsClick(Sender: TObject);

begin
  if mSettingsNoHalfs.Checked then
    mSettingsNoHalfs.Checked := False
  else
    mSettingsNoHalfs.Checked := True;
  bNoHalfs := mSettingsNoHalfs.Checked;
end;

{ Menu item "Settings > Exclude tenths": Choose if tenths will be available or not }

procedure TfFractions.mSettingsNoTenthsClick(Sender: TObject);

begin
  if mSettingsNoTenths.Checked then
    mSettingsNoTenths.Checked := False
  else
    mSettingsNoTenths.Checked := True;
  bNoTenths := mSettingsNoTenths.Checked;
end;

{ Menu item "Settings > Colored fractions": Choose if squares will be colored depending on fraction's denominator or not }

procedure TfFractions.mSettingsColorClick(Sender: TObject);

begin
  if mSettingsColor.Checked then
    mSettingsColor.Checked := False
  else
    mSettingsColor.Checked := True;
  bGridColored := mSettingsColor.Checked;
end;

{ Menu item "Help > Help": Display application help }

procedure TfFractions.mHelpHelpClick(Sender: TObject);

begin
  fHelp.ShowModal;
end;

{ Menu item "Help > About": Display application about }

procedure TfFractions.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics game: Addition of fractions.' + LineEnding;
  S += 'Version 1.0.1, © allu, November 2018 - December 2023.';
  MessageDlg('About "Fractions"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new grid resp. check user answer}

procedure TfFractions.btQuestionClick(Sender: TObject);

var
  N, D, Max, I, J: Integer;
  Ok, OkAll: Boolean;
  FResult, F1, F2, F3, F4, FR2, FR3, FR4: TFraction;
  FractionCounts: TFractionCounts;
  DiffDenominators: array of Integer;

begin
  // Button "Question": Generate new grid
  if btQuestion.Caption = 'Question' then begin
    // Still questions left
    if iQuestion < iQuestions then begin
      Inc(iQuestion);
      ResetFractions(iGridSize, aFractions, aGridFractions);
      SetLength(aAnswer, 0);
      for J := 1 to 9 do begin
        for I := 2 to 10 do begin
          FractionCounts[J, I] := 0;
        end;
      end;
      // Choose number of operands
      if bOpMaxAlways then
        iOperands := iMaxOperands
      else
        iOperands := Random(iMaxOperands - 1) + 2;
      // Adapt number of denominators if necessary
      iQDenominators := iDenominators;
      if iQDenominators > iOperands then
        iQDenominators := iOperands;
      // Display instructions (with actual question parameters)
      DisplayInstructions(iOperands, iQDenominators, True);
      // Generate a valid answer
      repeat
        OkAll := True;
        // 1st and 2nd fraction
        repeat
          Ok := True;
          F1.Numerator := Random(9) + 1; F1.Denominator := Random(9) + 2;
          if (F1.Numerator >= F1.Denominator) or (bNoHalfs and (F1.Denominator / F1.Numerator = 2)) or (bNoTenths and (F1.Denominator = 10)) then
            Ok := False;
        until Ok;
        repeat
          Ok := True;
          F2.Numerator := Random(9) + 1; F2.Denominator := Random(9) + 2;
          if (F2.Numerator >= F2.Denominator) or (bNoHalfs and (F2.Denominator / F2.Numerator = 2)) or (bNoTenths and (F2.Denominator = 10)) then
            Ok := False;
        until Ok;
        // 3rd fraction
        if (iOperands = 3) or (iOperands = 4) then begin
          repeat
            Ok := True;
            F3.Numerator := Random(9) + 1; F3.Denominator := Random(9) + 2;
            if (F3.Numerator >= F3.Denominator) or (bNoHalfs and (F3.Denominator / F3.Numerator = 2)) or (bNoTenths and (F3.Denominator = 10)) then
              Ok := False;
          until Ok;
        end;
        // 4th fraction
        if iOperands = 4 then begin
          repeat
            Ok := True;
            F4.Numerator := Random(9) + 1; F4.Denominator := Random(9) + 2;
            if (F4.Numerator >= F4.Denominator) or (bNoHalfs and (F4.Denominator / F4.Numerator = 2)) or (bNoTenths and (F4.Denominator = 10)) then
              Ok := False;
          until Ok;
        end;
        // Check if fractions fullfill actual game settings conditions
        FractionAdd(F1, F2, FR2);
        if iOperands = 2 then begin
          // Addition result must be 1 and denominators different
          if not FractionIsOne(FR2) or (F1.Denominator = F2.Denominator) then
            OkAll := False;
        end
        else if (iOperands = 3) or (iOperands = 4) then begin
          FractionAdd(FR2, F3, FR3);
          if iOperands = 3 then begin
            // Addition result must be 1 and number of different denominators must be at least 2 resp. 3
            if iQDenominators = 2 then begin
              if not FractionIsOne(FR3) or ((F1.Denominator = F2.Denominator) and (F1.Denominator = F3.Denominator)) then
                OkAll := False;
            end
            else begin
              if not FractionIsOne(FR3) or ((F1.Denominator = F2.Denominator) or (F1.Denominator = F3.Denominator) or (F2.Denominator = F3.Denominator)) then
                OkAll := False;
            end;
          end
          else if iOperands = 4 then begin
            // Addition result must be 1 and number of different denominators must be at least 2 resp. 3
            FractionAdd(FR3, F4, FR4);
            if iQDenominators = 2 then begin
              if not FractionIsOne(FR4) or ((F1.Denominator = F2.Denominator) and (F1.Denominator = F3.Denominator) and (F1.Denominator = F4.Denominator)) then
                OkAll := False;
            end
            else begin
              if not FractionIsOne(FR4) or ((F1.Denominator = F2.Denominator) and (F1.Denominator = F3.Denominator))
                                        or ((F1.Denominator = F2.Denominator) and (F1.Denominator = F4.Denominator))
                                        or ((F1.Denominator = F3.Denominator) and (F1.Denominator = F4.Denominator))
                                        or ((F2.Denominator = F3.Denominator) and (F2.Denominator = F4.Denominator))
                                        or ((F1.Denominator = F2.Denominator) and (F3.Denominator = F4.Denominator))
                                        or ((F1.Denominator = F3.Denominator) and (F2.Denominator = F4.Denominator))
                                        or ((F1.Denominator = F4.Denominator) and (F2.Denominator = F3.Denominator)) then
                OkAll := False;
            end;
          end;
        end;
      until OkAll;
      // Randomly place these fractions (that give one - of possibly more - solutions) onto the grid
      J := Random(iGridSize) + 1; I := Random(iGridSize) + 1;
      aFractions[J, I].Numerator := F1.Numerator;
      aFractions[J, I].Denominator := F1.Denominator;
      FractionCounts[F1.Numerator, F1.Denominator] := 1;
      repeat
        J := Random(iGridSize) + 1; I := Random(iGridSize) + 1;
      until aFractions[J, I].Numerator = 0;
      aFractions[J, I].Numerator := F2.Numerator;
      aFractions[J, I].Denominator := F2.Denominator;
      FractionCounts[F2.Numerator, F2.Denominator] += 1;
      if (iOperands = 3) or (iOperands = 4) then begin
        repeat
          J := Random(iGridSize) + 1; I := Random(iGridSize) + 1;
        until aFractions[J, I].Numerator = 0;
        aFractions[J, I].Numerator := F3.Numerator;
        aFractions[J, I].Denominator := F3.Denominator;
        FractionCounts[F3.Numerator, F3.Denominator] += 1;
      end;
      if iOperands = 4 then begin
        repeat
          J := Random(iGridSize) + 1; I := Random(iGridSize) + 1;
        until aFractions[J, I].Numerator = 0;
        aFractions[J, I].Numerator := F4.Numerator;
        aFractions[J, I].Denominator := F4.Denominator;
        FractionCounts[F4.Numerator, F4.Denominator] += 1;
      end;
      // Generate random fractions to fill the rest of the grid
      if iGridSize = 8 then
        Max := 3                                                               // each fraction only twice, except if grid = 8x8 (otherwise may be not enought fractions)
      else
        Max := 2;
      for J := 1 to iGridSize do begin
        for I := 1 to iGridSize do begin
          if aFractions[J, I].Numerator = 0 then begin
            // Fill in if empty only (answer-fractions determined before may be here)
            repeat
              OK := True;
              N := Random(9) + 1; D := Random(9) + 2;
              // Random numbers, but there are conditions!
              if N >= D then
                // Numerator must be less than denominator
                Ok := False
              else if bNoHalfs and (D / N = 2) then
                // There mustn't be halfs if game settings tell so
                Ok := False
              else if bNoTenths and (D = 10) then
                // There mustn't be tenths if game settings tell so
                Ok := False
              else if FractionCounts[N, D] = Max then
                // Each fraction only twice (or three times for 8x8 grid)
                Ok := False
              else begin
                // Fill fraction into the grid
                FractionCounts[N, D] += 1;
                aFractions[J, I].Numerator := N;
                aFractions[J, I].Denominator := D;
              end;
            until Ok;
          end;
        end;
      end;
      // Create the grid with generated fractions
      CreateGrid(iGridSize, aGrid, aFractions, aGridFractions, bGridColored);
      // Clear answer field
      edAnswer.Text := ''; imEval.Visible := False;
      // Next button push will be the user's answer
      btQuestion.Caption := 'Answer';
    end;
  end
  // Button "Answer": Check user answer
  else begin
    // Still answers left
    if iQuestion <= iQuestions then begin
      OkAll := False;
      // If user entered correct number of operands, calculate the addition result
      if Length(aAnswer) = iOperands then begin
        FResult.Numerator := 0; FResult.Denominator := 1;
        for I := 0 to Length(aAnswer) - 1 do
          FractionAdd(FResult, aAnswer[I], FResult);
      end;
      // Check if the addition result is 1
      if FractionIsOne(FResult) then
        OkAll := True;
      if OkAll then begin
        // If addition result is 1, check if minimum of different denominators is respected
        SetLength(DiffDenominators, 1);
        DiffDenominators[0] := aAnswer[0].Denominator;
        for I := 1 to Length(aAnswer) - 1 do begin
          Ok := True;
          for J := 0 to Length(DiffDenominators) - 1 do begin
            if aAnswer[I].Denominator = DiffDenominators[J] then
              Ok := False;
          end;
          if Ok then begin
            SetLength(DiffDenominators, Length(DiffDenominators) + 1);
            DiffDenominators[Length(DiffDenominators) - 1] := aAnswer[I].Denominator;
          end;
        end;
        if Length(DiffDenominators) < iQDenominators then
          OkAll := False;
      end;
      // User's answer is correct
      if OkAll then begin
        Inc(iCorrect);
        imEval.Picture.LoadFromFile('correct.png');
      end
      // User's answer isn't correct (for actual game parameters)
      else begin
        Inc(iFalse);
        imEval.Picture.LoadFromFile('false.png');
      end;
      imEval.Visible := True;
      // Update evoluation
      sgEval.Cells[1, 0] := GridFormat(iQuestion, '');
      sgEval.Cells[1, 1] := GridFormat(iCorrect, '');
      sgEval.Cells[1, 2] := GridFormat(iFalse, '');
      sgEval.Cells[1, 3] := GridFormat(Round(100 * (iCorrect / iQuestion)), '%');
      // All questiosn done: Display message
      if iQuestion = iQuestions then begin
        MessageDlg('End of exercice', 'All questions have been done!', mtInformation, [mbOK], 0);
        Inc(iQuestion);
      end;
      // Next button push will be another question
      btQuestion.Caption := 'Question';
    end;
  end;
end;

{ Button "Undo": Unselect the last fraction selected }

procedure TfFractions.btUndoClick(Sender: TObject);

var
  I, J, P: Integer;
  S: string;

begin
  if btQuestion.Caption = 'Answer' then begin
    // Button reactive only if user answer awaited
    if Length(aAnswer) > 0 then begin
      // Remove fraction from answer field
      S := edAnswer.Text;
      P := Pos(' + ', S);
      if P = 0 then
        edAnswer.Text := ''
      else
        edAnswer.Text := LeftStr(S, P - 1);
      // Update the grid (remove highlighting)
      for J := 1 to iGridSize do begin
        for I := 1 to iGridSize do begin
          if (aAnswer[Length(aAnswer) - 1].Numerator = aFractions[J, I].Numerator) and (aAnswer[Length(aAnswer) - 1].Denominator = aFractions[J, I].Denominator) then begin
            aGridFractions[J, I].Transparent := True;
            aGridFractions[J, I].Font.Color := clDefault;
            aGridFractions[J, I].Font.Style := [];
          end;
        end;
      end;
      // Remove fraction from user's answer array
      SetLength(aAnswer, Length(aAnswer) - 1);
    end;
  end;
end;

{ Methods handling click on squares 1,1 to 8,8 }

procedure TfFractions.shGrid11MouseDown(Sender: TObject);

begin
  FractionSelect(1, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid12MouseDown(Sender: TObject);

begin
  FractionSelect(1, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid13MouseDown(Sender: TObject);

begin
  FractionSelect(1, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid14MouseDown(Sender: TObject);

begin
  FractionSelect(1, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid15MouseDown(Sender: TObject);

begin
  FractionSelect(1, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid16MouseDown(Sender: TObject);

begin
  FractionSelect(1, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid17MouseDown(Sender: TObject);

begin
  FractionSelect(1, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid18MouseDown(Sender: TObject);

begin
  FractionSelect(1, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid21MouseDown(Sender: TObject);

begin
  FractionSelect(2, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid22MouseDown(Sender: TObject);

begin
  FractionSelect(2, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid23MouseDown(Sender: TObject);

begin
  FractionSelect(2, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid24MouseDown(Sender: TObject);

begin
  FractionSelect(2, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid25MouseDown(Sender: TObject);

begin
  FractionSelect(2, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid26MouseDown(Sender: TObject);

begin
  FractionSelect(2, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid27MouseDown(Sender: TObject);

begin
  FractionSelect(2, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid28MouseDown(Sender: TObject);

begin
  FractionSelect(2, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid31MouseDown(Sender: TObject);

begin
  FractionSelect(3, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid32MouseDown(Sender: TObject);

begin
  FractionSelect(3, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid33MouseDown(Sender: TObject);

begin
  FractionSelect(3, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid34MouseDown(Sender: TObject);

begin
  FractionSelect(3, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid35MouseDown(Sender: TObject);

begin
  FractionSelect(3, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid36MouseDown(Sender: TObject);

begin
  FractionSelect(3, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid37MouseDown(Sender: TObject);

begin
  FractionSelect(3, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid38MouseDown(Sender: TObject);

begin
  FractionSelect(3, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid41MouseDown(Sender: TObject);

begin
  FractionSelect(4, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid42MouseDown(Sender: TObject);

begin
  FractionSelect(4, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid43MouseDown(Sender: TObject);

begin
  FractionSelect(4, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid44MouseDown(Sender: TObject);

begin
  FractionSelect(4, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid45MouseDown(Sender: TObject);

begin
  FractionSelect(4, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid46MouseDown(Sender: TObject);

begin
  FractionSelect(4, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid47MouseDown(Sender: TObject);

begin
  FractionSelect(4, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid48MouseDown(Sender: TObject);

begin
  FractionSelect(4, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid51MouseDown(Sender: TObject);

begin
  FractionSelect(5, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid52MouseDown(Sender: TObject);

begin
  FractionSelect(5, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid53MouseDown(Sender: TObject);

begin
  FractionSelect(5, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid54MouseDown(Sender: TObject);

begin
  FractionSelect(5, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid55MouseDown(Sender: TObject);

begin
  FractionSelect(5, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid56MouseDown(Sender: TObject);

begin
  FractionSelect(5, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid57MouseDown(Sender: TObject);

begin
  FractionSelect(5, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid58MouseDown(Sender: TObject);

begin
  FractionSelect(5, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid61MouseDown(Sender: TObject);

begin
  FractionSelect(6, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid62MouseDown(Sender: TObject);

begin
  FractionSelect(6, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid63MouseDown(Sender: TObject);

begin
  FractionSelect(6, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid64MouseDown(Sender: TObject);

begin
  FractionSelect(6, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid65MouseDown(Sender: TObject);

begin
  FractionSelect(6, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid66MouseDown(Sender: TObject);

begin
  FractionSelect(6, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid67MouseDown(Sender: TObject);

begin
  FractionSelect(6, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid68MouseDown(Sender: TObject);

begin
  FractionSelect(6, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid71MouseDown(Sender: TObject);

begin
  FractionSelect(7, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid72MouseDown(Sender: TObject);

begin
  FractionSelect(7, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid73MouseDown(Sender: TObject);

begin
  FractionSelect(7, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid74MouseDown(Sender: TObject);

begin
  FractionSelect(7, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid75MouseDown(Sender: TObject);

begin
  FractionSelect(7, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid76MouseDown(Sender: TObject);

begin
  FractionSelect(7, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid77MouseDown(Sender: TObject);

begin
  FractionSelect(7, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid78MouseDown(Sender: TObject);

begin
  FractionSelect(7, 8, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid81MouseDown(Sender: TObject);

begin
  FractionSelect(8, 1, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid82MouseDown(Sender: TObject);

begin
  FractionSelect(8, 2, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid83MouseDown(Sender: TObject);

begin
  FractionSelect(8, 3, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid84MouseDown(Sender: TObject);

begin
  FractionSelect(8, 4, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid85MouseDown(Sender: TObject);

begin
  FractionSelect(8, 5, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid86MouseDown(Sender: TObject);

begin
  FractionSelect(8, 6, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid87MouseDown(Sender: TObject);

begin
  FractionSelect(8, 7, aAnswer, aFractions, aGridFractions);
end;

procedure TfFractions.shGrid88MouseDown(Sender: TObject);

begin
  FractionSelect(8, 8, aAnswer, aFractions, aGridFractions);
end;

end.

