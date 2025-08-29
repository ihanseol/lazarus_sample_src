{***************************************}
{* Main unit for Fruitquiz application *}
{***************************************}

unit fquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids, Menus, LazUTF8;

const
  iTotalFruits  = 50;
  iGroup1Fruits = 15;
  iGroup2Fruits = 30;
  iGroup3Fruits = 50;

type
  TFruit = record
    Name, Name2, Name3, Shortname: string;
    Pics, Group: Integer;
  end;
  TFruits = array[0..iTotalFruits - 1] of TFruit;
  {*************}
  { TfFruitquiz }
  {*************}
  TfFruitquiz = class(TForm)
    MainMenu1: TMainMenu;
    mQuiz, mQuizNew, mQuizExit: TMenuItem;
    mOptions, mOptionsLang, mOptionsLangEng, mOptionsLangGer: TMenuItem;
    mOptionsFruits, mOptionsFruits1, mOptionsFruits2, mOptionsFruits3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laFruit: TLabel;
    imFruit1, imFruit2, imFruit3, imFruit4: TImage;
    imFruit5, imFruit6, imFruit7, imFruit8: TImage;
    Shape1, Shape2, Shape3, Shape4: TShape;
    Shape5, Shape6, Shape7, Shape8: TShape;
    shSel1, shSel2, shSel3, shSel4: TShape;
    Label2, laFruits: TLabel;
    sgEval: TStringGrid;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizNewClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsLangEngClick(Sender: TObject);
    procedure mOptionsLangGerClick(Sender: TObject);
    procedure mOptionsFruits1Click(Sender: TObject);
    procedure mOptionsFruits2Click(Sender: TObject);
    procedure mOptionsFruits3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure imFruit1Click(Sender: TObject);
    procedure imFruit2Click(Sender: TObject);
    procedure imFruit3Click(Sender: TObject);
    procedure imFruit4Click(Sender: TObject);
    procedure imFruit5Click(Sender: TObject);
    procedure imFruit6Click(Sender: TObject);
    procedure imFruit7Click(Sender: TObject);
    procedure imFruit8Click(Sender: TObject);
  private
    iGroup0, iGroup, iFruits, iFruit, iCorrect, iPic: Integer;
    sLanguage0, sLanguage, sFruit, sFruitENG: string;
    bAnswer: Boolean;
    aFruitsEng, aFruitsGer, aFruits: TFruits;
    aFruitsDone: array[0..iTotalFruits - 1] of Boolean;
    imFruits: array[0..7] of TImage;
    shSelection: array[0..3] of TShape;
  end;

var
  fFruitquiz: TfFruitquiz;

implementation

{$R *.lfm}

{ Format number for the grid (right-align) }

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
    SN := SN + S;                                                              // % sign
  Result := SN;
end;

{ Extract supplementary fruit names }

procedure GetNames(S: string; out ShortNameEng, ShortNameGer, SupplNameENG, SupplNameGer: string);

begin
  ShortNameEng := ''; ShortNameGer := ''; SupplNameENG := ''; SupplNameGer := '';
  if LeftStr(S, 1) = '*' then begin
    // Name given is shortname
    UTF8Delete(S, 1, 1);
    if LeftStr(S, 1) = 'e' then
      ShortNameEng := UTF8Copy(S, 2, UTF8Length(S))
    else if LeftStr(S, 1) = 'g' then
      ShortNameGer := UTF8Copy(S, 2, UTF8Length(S));
  end
  else begin
    // Name given is second/third name
    if LeftStr(S, 1) = 'e' then
      SupplNameENG := UTF8Copy(S, 2, UTF8Length(S))
    else if LeftStr(S, 1) = 'g' then
      SupplNameGER := UTF8Copy(S, 2, UTF8Length(S));
  end;
end;

{ Read fruit data from text file }

procedure ReadFruits(out FruitsEng, FruitsGer: TFruits);

var
  N, P: Integer;
  Filename, Line, S: string;
  InFile: Text;

begin
  Filename := GetCurrentDir + '/data/fruits.txt'; DoDirSeparators(Filename);
  Assign(InFile, Filename); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := UTF8Trim(Line);
    if Line <> '' then begin
      Inc(N);
      if N <= iTotalFruits then
        FruitsEng[N - 1].Name := UTF8Trim(UTF8Copy(Line, 1, 15));              // cols  1-15: English fruit name
        FruitsGer[N - 1].Name := UTF8Trim(UTF8Copy(Line, 16, 15));             // cols 16-30: German fruit name
        FruitsEng[N - 1].Name2 := ''; FruitsEng[N - 1].Name3 := '';
        FruitsGer[N - 1].Name2 := ''; FruitsGer[N - 1].Name3 := '';
        FruitsEng[N - 1].ShortName := ''; FruitsGer[N - 1].ShortName := '';
        if Length(Line) > 20 then begin                                        // cols 35-: supplementary names
          // Read supplementary fruit names
          S := UTF8Trim(UTF8Copy(Line, 35, UTF8Length(Line)));
          P := UTF8Pos(',', S);                                                // second and third item separated by comma
          if P = 0 then begin
            // One single name item given
            GetNames(S, FruitsEng[N - 1].ShortName, FruitsGer[N - 1].ShortName, FruitsEng[N - 1].Name2, FruitsGer[N - 1].Name2);
          end
          else begin
            // Two name items given
            GetNames(UTF8Copy(S, 1, P - 1), FruitsEng[N - 1].ShortName, FruitsGer[N - 1].ShortName, FruitsEng[N - 1].Name2, FruitsGer[N - 1].Name2);
            GetNames(UTF8Copy(S, P + 1, UTF8Length(S)), FruitsEng[N - 1].ShortName, FruitsGer[N - 1].ShortName, FruitsEng[N - 1].Name3, FruitsGer[N - 1].Name3);
            // Avoid Name3 set, if Name2 is empty (-> set Name2)
            if FruitsEng[N - 1].Name2 = '' then begin
              FruitsEng[N - 1].Name2 := FruitsEng[N - 1].Name3;
              FruitsEng[N - 1].Name3 := '';
            end;
            if FruitsGer[N - 1].Name2 = '' then begin
              FruitsGer[N - 1].Name2 := FruitsGer[N - 1].Name3;
              FruitsGer[N - 1].Name3 := '';
            end;
          end;
        end;
        FruitsEng[N - 1].Pics := StrToInt(UTF8Copy(Line, 31, 1));              // col 31: number of fruit pictures
        FruitsGer[N - 1].Pics := StrToInt(UTF8Copy(Line, 31, 1));
        FruitsEng[N - 1].Group := StrToInt(UTF8Copy(Line, 33, 1));             // col 33: fruit group indicator
        FruitsGer[N - 1].Group := StrToInt(UTF8Copy(Line, 33, 1));
    end;
  end;
  Close(InFile);
end;

{ Select fruit image clicked on as user answer }

procedure FruitSelect(PicClicked, PicCorrect: Integer; var MayAnswer: Boolean; var FruitsIm: array of TImage;
  var SelShapes: array of TShape; Fruits, GroupFruits: Integer; var Correct: Integer);

var
  I: Integer;
  Colour: TColor;

begin
  if MayAnswer then begin
    // Proceed only if an answer is to be given (i.e. after a new question has been asked)
    if PicClicked = PicCorrect then begin
      // Correct picture clicked: Draw green border
      Colour := clLime;
      Inc(Correct);
    end
    else begin
      // Wrong picture clicked: Draw red border
      Colour := clRed;
    end;
    // Color the border and make it visible around picture clicked on
    SelShapes[0].Left := FruitsIm[PicClicked].Left; SelShapes[0].Top := FruitsIm[PicClicked].Top;
    SelShapes[1].Left := FruitsIm[PicClicked].Left; SelShapes[1].Top := FruitsIm[PicClicked].Top + FruitsIm[PicClicked].Height - 5;
    SelShapes[2].Left := FruitsIm[PicClicked].Left; SelShapes[2].Top := FruitsIm[PicClicked].Top;
    SelShapes[3].Left := FruitsIm[PicClicked].Left + FruitsIm[PicClicked].Width - 5; SelShapes[3].Top := FruitsIm[PicClicked].Top;
    for I := 0 to 3 do begin
      SelShapes[I].Brush.Color := Colour;
      SelShapes[I].Visible := True;
    end;
    // Fill in evaluation grid
    fFruitquiz.sgEval.Cells[1, 0] := GFormat(Fruits, '');
    fFruitquiz.sgEval.Cells[1, 1] := GFormat(Correct, '');
    fFruitquiz.sgEval.Cells[1, 2] := GFormat(Fruits - Correct, '');
    fFruitquiz.sgEval.Cells[1, 3] := GFormat(Round(100 * (Correct / Fruits)), '%');
    if Fruits < GroupFruits then begin
      // Still fruits left: Enable the "Question" button
      fFruitquiz.btStart.Enabled := True; MayAnswer := False;
    end
    else begin
      // All fruits done: End of quiz message
      fFruitquiz.btStart.Enabled := False; MayAnswer := False;
      MessageDlg('Fruitquiz', 'All fruits of this group have been shown. End of the quiz.', mtInformation, [mbOK], 0);
    end;
  end;
end;

{*************}
{ TfFruitquiz }
{*************}

{ Application start: Initialisation }

procedure TfFruitquiz.FormCreate(Sender: TObject);

begin
  // Create arrays with fruit images and selection-border shapes
  imFruits[0] := imFruit1; imFruits[1] := imFruit2; imFruits[2] := imFruit3; imFruits[3] := imFruit4;
  imFruits[4] := imFruit5; imFruits[5] := imFruit6; imFruits[6] := imFruit7; imFruits[7] := imFruit8;
  shSelection[0] := shSel1; shSelection[1] := shSel2; shSelection[2] := shSel3; shSelection[3] := shSel4;
  // Read fruits data from file
  ReadFruits(aFruitsEng, aFruitsGer);
  // Start a new quiz (with group 1 fruits, names in English)
  Randomize;
  sLanguage0 := 'ENG'; iGroup0 := 1;
  mQuizNew.Click;
end;

{ Menu item "Quiz > New": Start a new quiz }

procedure TfFruitquiz.mQuizNewClick(Sender: TObject);

var
  I: Integer;

begin
  sLanguage := sLanguage0; iGroup := iGroup0;                                  // user selections becomes active now
  bAnswer := False;                                                            // disable images clicked-on event
  if sLanguage = 'ENG' then
    laFruit.Caption := 'Which of these pictures shows ...?'
  else
    laFruit.Caption := 'Welches dieser Bilder zeigt ...?';
  // Total number of fruits (questions) for actual quiz
  case iGroup of
    1: iFruits := iGroup1Fruits;
    2: iFruits := iGroup2Fruits;
    3: iFruits := iGroup3Fruits;
  end;
  // Get English or German fruit names (depending on user selection)
  for I := 0 to iTotalFruits - 1 do begin
    if sLanguage = 'ENG' then
      aFruits[I] := aFruitsEng[I]
    else
      aFruits[I] := aFruitsGer[I];
    aFruitsDone[I] := False;                                                   // set all fruits to "not yet done"
  end;
  // Variables initialisation and form controls reset
  iFruit := 0; iCorrect := 0;
  for I := 0 to 7 do
    imFruits[I].Picture.Clear;
  for I := 0 to 3 do
    shSelection[I].Visible := False;
  laFruits.Visible := False;
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  // Enable "Start" button (-> user can make generate a new question)
  btStart.Enabled := True;
end;

{ Menu item "Quiz > Exit": Exit application }

procedure TfFruitquiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Language > ...": Fruit names language selection }

procedure TfFruitquiz.mOptionsLangEngClick(Sender: TObject);

begin
  mOptionsLangEng.Checked := True; mOptionsLangGer.Checked := False;
  sLanguage0 := 'ENG';
end;

procedure TfFruitquiz.mOptionsLangGerClick(Sender: TObject);

begin
  mOptionsLangEng.Checked := False; mOptionsLangGer.Checked := True;
  sLanguage0 := 'DEU';
end;

{ Menu items "Settings > Fruit group > ...": Fruit group (number of fruits) selection }

procedure TfFruitquiz.mOptionsFruits1Click(Sender: TObject);

begin
  mOptionsFruits1.Checked := True; mOptionsFruits2.Checked := False; mOptionsFruits3.Checked := False;
  iGroup0 := 1;
end;

procedure TfFruitquiz.mOptionsFruits2Click(Sender: TObject);

begin
  mOptionsFruits1.Checked := False; mOptionsFruits2.Checked := True; mOptionsFruits3.Checked := False;
  iGroup0 := 2;
end;

procedure TfFruitquiz.mOptionsFruits3Click(Sender: TObject);

begin
  mOptionsFruits1.Checked := False; mOptionsFruits2.Checked := False; mOptionsFruits3.Checked := True;
  iGroup0 := 3;
end;

{ Menu item "Help > Help": Display (short) application help text }

procedure TfFruitquiz.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Push the button "Question" to ask for a new fruit. To answer, click onto the picture that shows this fruit. ';
  S += 'A green border indicates a correct, a red border a false answer.' + LineEnding;
  S += 'In the "Settings" menu, you can select the fruit names language and the fruit group (number of fruits/questions). Use "New" in ';
  S += 'the "Quiz" menu to start a new quiz.';
  MessageDlg('"Fruitquiz" Help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfFruitquiz.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Fruitquiz for children:' + LineEnding;
  S += 'Guess on which of the 8 pictures displayed, the given fruit is shown.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, October-December 2021.';
  MessageDlg('About "Fruitquiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question" pushed: Generate new question (ask for new fruit) }

procedure TfFruitquiz.btStartClick(Sender: TObject);

var
  F, F0, R, I, J: Integer;
  Filename, S: string;
  OK: Boolean;
  FShown: array[0..7] of Integer;

begin
  Inc(iFruit);
  laFruits.Caption := 'Fruit ' + IntToStr(iFruit) + '/' + IntToStr(iFruits); laFruits.Visible := True;
  for I := 0 to 3 do
    shSelection[I].Visible := False;
  // Random fruit (within actual group and not yet asked)
  repeat
    F0 := Random(iTotalFruits);
  until (aFruits[F0].Group <= iGroup) and (not aFruitsDone[F0]);
  aFruitsDone[F0] := True;                                                     // mark this fruit as "done"
  sFruit := aFruits[F0].Name;                                                  // fruit name (English or German)
  sFruitEng := aFruitsEng[F0].Name;                                            // fruit name (English)
  // Randomly choose one of the possibly 3 fruit names (for display in question text)
  R := 0;
  if (aFruits[F0].Name2 <> '') and (aFruits[F0].Name3 <> '') then
    R := Random(3)
  else if aFruits[F0].Name2 <> '' then
    R := Random(2);
  case R of
    0: S := sFruit;
    1: S := aFruits[F0].Name2;
    2: S := aFruits[F0].Name3;
  end;
  if sLanguage = 'ENG' then
    laFruit.Caption := 'Which of these pictures shows the fruit '
  else
    laFruit.Caption := 'Welches dieser Bilder zeigt die Frucht ';
  laFruit.Caption := laFruit.Caption + UTF8UpperCase(S) + '?';
  // Randomly choose 7 fruit images
  for I := 0 to 7 do begin
    FShown[I] := 0;
    repeat
      OK := False;
      F := Random(iTotalFruits);
      if (F <> F0) and (aFruits[F].Group <= iGroup) then begin
        // Fruits must be different from the one asked for and be part of actual fruit group
        OK := True;
        if (aFruits[F].Shortname <> '') and (aFruits[F].Shortname = aFruits[F0].Name) then begin
          // Do not use fruits that have a shortname that equals the name of the fruit asked for
          // This avoids displaying for example a watermelon if the question concerns melons
          OK := False;
        end
        else if I > 0 then begin
          // Each fruit should be displayed only once (no duplicates)
          for J := 0 to I - 1 do begin
            if F = FShown[J] then
              OK := False;
          end;
        end;
      end;
    until OK;
    // Display fruit's picture (if there are several pics, randomly choose one of them)
    Filename := GetCurrentDir + '/data/' + UTF8LowerCase(aFruitsEng[F].Name);  // filenames are English fruit names!
    Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
    R := Random(aFruits[F].Pics) + 1;
    if R >= 2 then
      Filename += IntToStr(R);
    Filename += '.jpg'; DoDirSeparators(Filename);
    imFruits[I].Picture.LoadFromFile(Filename);
    FShown[I] := F;                                                            // save fruit chosen (for duplicates test)
  end;
  // Now place the fruit asked for at a random position within the picture-grid
  iPic := Random(8);
  Filename := GetCurrentDir + '/data/' + UTF8LowerCase(sFruitEng);             // filename is English fruit name!
  R := Random(aFruits[F0].Pics) + 1;
  if R >= 2 then
    Filename += IntToStr(R);
  Filename += '.jpg'; DoDirSeparators(Filename);
  Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
  imFruits[iPic].Picture.LoadFromFile(Filename);
  // Disable the "Question" button and enable action when users click a picture (-> users can give their answer)
  bAnswer := True; btStart.Enabled := False;
end;

{ Click on one of the 8 fruit images: Select this fruit as user's answer }

procedure TfFruitquiz.imFruit1Click(Sender: TObject);

begin
  FruitSelect(0, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit2Click(Sender: TObject);

begin
  FruitSelect(1, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit3Click(Sender: TObject);

begin
  FruitSelect(2, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit4Click(Sender: TObject);

begin
  FruitSelect(3, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit5Click(Sender: TObject);

begin
  FruitSelect(4, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit6Click(Sender: TObject);

begin
  FruitSelect(5, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit7Click(Sender: TObject);

begin
  FruitSelect(6, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

procedure TfFruitquiz.imFruit8Click(Sender: TObject);

begin
  FruitSelect(7, iPic, bAnswer, imFruits, shSelection, iFruit, iFruits, iCorrect);
end;

end.

