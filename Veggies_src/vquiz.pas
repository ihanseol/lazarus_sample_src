{*************************************}
{* Main unit for Veggies application *}
{*************************************}

unit vquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Grids, Menus, LazUTF8;

const
  iTotalVeggies  = 50;
  iGroup1Veggies = 15;
  iGroup2Veggies = 30;
  iGroup3Veggies = 50;

type
  TVeggie = record
    Name, Name2: string;
    Group: Integer;
  end;
  TVeggies = array[0..iTotalVeggies - 1] of TVeggie;
  {***********}
  { TfVeggies }
  {***********}
  TfVeggies = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizNew, mQuizExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsVeggies, mOptionsVeggies1, mOptionsVeggies2, mOptionsVeggies3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laQuestions, laQuestion, Label2: TLabel;
    imVeggie1, imVeggie2, imVeggie3, imVeggie4: TImage;
    imVeggie5, imVeggie6, imVeggie7, imVeggie8: TImage;
    Shape1, Shape2, Shape3, Shape4: TShape;
    Shape5, Shape6, Shape7, Shape8: TShape;
    shSel1, shSel2, shSel3, shSel4: TShape;
    sgEval: TStringGrid;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuizNewClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsVeggies1Click(Sender: TObject);
    procedure mOptionsVeggies2Click(Sender: TObject);
    procedure mOptionsVeggies3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure imVeggie1Click(Sender: TObject);
    procedure imVeggie2Click(Sender: TObject);
    procedure imVeggie3Click(Sender: TObject);
    procedure imVeggie4Click(Sender: TObject);
    procedure imVeggie5Click(Sender: TObject);
    procedure imVeggie6Click(Sender: TObject);
    procedure imVeggie7Click(Sender: TObject);
    procedure imVeggie8Click(Sender: TObject);
  private
    iGroup0, iGroup, iVeggies, iVeggie, iCorrect, iPic: Integer;
    bAnswer: Boolean;
    aVeggies: TVeggies;
    aVeggiesDone: array[0..iTotalVeggies - 1] of Boolean;
    imVeggies: array[0..7] of TImage;
    shSelection: array[0..3] of TShape;
  end;

var
  fVeggies: TfVeggies;

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

{ Get vegetable picture filepath (from vegetable name) }

function GetFilename(Filename: string): string;

begin
  Filename := GetCurrentDir + '/pics/' + UTF8LowerCase(Filename);
  Filename := StringReplace(Filename, 'ä', 'ae', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ö', 'oe', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ü', 'ue', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ß', 'ss', [rfReplaceAll]);
  Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
  Filename += '.jpg'; DoDirSeparators(Filename);
  Result := Filename;
end;

{ Read vegetables data from text file }

procedure ReadVeggies(out Veggies: TVeggies);

var
  N: Integer;
  Filename, Line: string;
  InFile: Text;

begin
  Filename := GetCurrentDir + '/' + 'veggies.txt'; DoDirSeparators(Filename);
  Assign(InFile, Filename); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := UTF8Trim(Line);
    if Line <> '' then begin
      Inc(N);
      if N <= iTotalVeggies then begin
        Veggies[N - 1].Name := UTF8Trim(UTF8Copy(Line, 1, 20));                // cols  1-20: vegetable name
        Veggies[N - 1].Name2 := UTF8Trim(UTF8Copy(Line, 21, 20));              // cols 21-40: other vegetable name
        Veggies[N - 1].Group := StrToInt(UTF8Copy(Line, 41, 1));               // col 42: vegetable group indicator
      end;
    end;
  end;
  Close(InFile);
end;

{ Select vegetable image clicked-on as user answer }

procedure VeggieSelect(PicClicked, PicCorrect: Integer; var MayAnswer: Boolean; var VeggiesIm: array of TImage;
  var SelShapes: array of TShape; Question, Questions: Integer; var Correct: Integer);

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
    // Color the border and make it visible around picture clicked-on
    SelShapes[0].Left := VeggiesIm[PicClicked].Left; SelShapes[0].Top := VeggiesIm[PicClicked].Top;
    SelShapes[1].Left := VeggiesIm[PicClicked].Left; SelShapes[1].Top := VeggiesIm[PicClicked].Top + VeggiesIm[PicClicked].Height - 5;
    SelShapes[2].Left := VeggiesIm[PicClicked].Left; SelShapes[2].Top := VeggiesIm[PicClicked].Top;
    SelShapes[3].Left := VeggiesIm[PicClicked].Left + VeggiesIm[PicClicked].Width - 5; SelShapes[3].Top := VeggiesIm[PicClicked].Top;
    for I := 0 to 3 do begin
      SelShapes[I].Brush.Color := Colour;
      SelShapes[I].Visible := True;
    end;
    // Fill in evaluation grid
    fVeggies.sgEval.Cells[1, 0] := GFormat(Question, '');
    fVeggies.sgEval.Cells[1, 1] := GFormat(Correct, '');
    fVeggies.sgEval.Cells[1, 2] := GFormat(Question - Correct, '');
    fVeggies.sgEval.Cells[1, 3] := GFormat(Round(100 * (Correct / Question)), '%');
    if Question < Questions then begin
      // Still vegetables left: Enable the "Frage" button
      fVeggies.btStart.Enabled := True; MayAnswer := False;
    end
    else begin
      // All vegetables done: End of quiz message
      fVeggies.btStart.Enabled := False; MayAnswer := False;
      MessageDlg('Veggies', 'Alle Gemüse dieser Gruppe wurden gezeigt. Ende des Quizes.', mtInformation, [mbOK], 0);
    end;
  end;
end;

{***********}
{ TfVeggies }
{***********}

{ Application start: Initialisation }

procedure TfVeggies.FormCreate(Sender: TObject);

begin
  // Create arrays with vegetable images and selection-border shapes
  imVeggies[0] := imVeggie1; imVeggies[1] := imVeggie2; imVeggies[2] := imVeggie3; imVeggies[3] := imVeggie4;
  imVeggies[4] := imVeggie5; imVeggies[5] := imVeggie6; imVeggies[6] := imVeggie7; imVeggies[7] := imVeggie8;
  shSelection[0] := shSel1; shSelection[1] := shSel2; shSelection[2] := shSel3; shSelection[3] := shSel4;
  // Read vegetable data from file
  ReadVeggies(aVeggies);
  // Start a new quiz (with group 1 vegetables)
  Randomize;
  iGroup0 := 1;
  mQuizNew.Click;
end;

{ Menu item "Quiz > Neu": Start a new quiz }

procedure TfVeggies.mQuizNewClick(Sender: TObject);

var
  I: Integer;

begin
  iGroup := iGroup0;                                                           // user selection becomes active now
  bAnswer := False;                                                            // disable images onClick event
  laQuestion.Caption := 'Welches dieser Bilder zeigt ...?';
  // Total number of vegetables (questions) for actual quiz
  case iGroup of
    1: iVeggies := iGroup1Veggies;
    2: iVeggies := iGroup2Veggies;
    3: iVeggies := iGroup3Veggies;
  end;
  // Set all vegetables to "not yet done"
  for I := 0 to iTotalVeggies - 1 do
    aVeggiesDone[I] := False;
  // Variables initialisation and form controls reset
  iVeggie := 0; iCorrect := 0;
  for I := 0 to 7 do
    imVeggies[I].Picture.Clear;
  for I := 0 to 3 do
    shSelection[I].Visible := False;
  laQuestions.Visible := False;
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  // Enable "Start" button (-> user can make generate a new question)
  btStart.Enabled := True;
end;

{ Menu item "Quiz > Verlassen": Exit application }

procedure TfVeggies.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Einstellungen > Gemüsegruppe > ...": Vegetable group (number of vegetables asked) selection }

procedure TfVeggies.mOptionsVeggies1Click(Sender: TObject);

begin
  mOptionsVeggies1.Checked := True; mOptionsVeggies2.Checked := False; mOptionsVeggies3.Checked := False;
  iGroup0 := 1;
end;

procedure TfVeggies.mOptionsVeggies2Click(Sender: TObject);

begin
  mOptionsVeggies1.Checked := False; mOptionsVeggies2.Checked := True; mOptionsVeggies3.Checked := False;
  iGroup0 := 2;
end;

procedure TfVeggies.mOptionsVeggies3Click(Sender: TObject);

begin
  mOptionsVeggies1.Checked := False; mOptionsVeggies2.Checked := False; mOptionsVeggies3.Checked := True;
  iGroup0 := 3;
end;

{ Menu item "Hilfe > Hilfe": Display (short) application help text }

procedure TfVeggies.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Den Druckknopf "Frage" betätigen um ein neues Gemüse zu erfragen. Um zu antworten, auf das Bild, das dieses Gemüse zeigt, klicken. ';
  S += 'Ein grüner Rand signalisiert eine richtige, ein roter Rand eine falsche Antwort.' + LineEnding;
  S += 'Im Menü "Einstellungen" kannst du die Gemüsegruppe (die Anzahl der Fragen) auswählen. Benutze "Neu" im Menü "Quiz" um ';
  S += 'einen neuen Quiz zu starten.';
  MessageDlg('"Veggies" Hilfe', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfVeggies.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Gemüse-Bilderquiz für Kinder:' + LineEnding;
  S += 'Errate auf welchem der 8 angezeigten Bilder das erfragte Gemüse abgebildet ist.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Juni 2022.';
  MessageDlg('Über "Veggies"', S, mtInformation, [mbOK], 0);
end;

{ Button "Frage" pushed: Generate new question (ask new vegetable) }

procedure TfVeggies.btStartClick(Sender: TObject);

var
  V, V0, R, I, J: Integer;
  Filename, S: string;
  OK: Boolean;
  VShown: array[0..7] of Integer;

begin
  Inc(iVeggie);
  laQuestions.Caption := 'Frage ' + IntToStr(iVeggie) + '/' + IntToStr(iVeggies); laQuestions.Visible := True;
  for I := 0 to 3 do
    shSelection[I].Visible := False;
  // Random vegetable (within actual group and not yet asked)
  repeat
    V0 := Random(iTotalVeggies);
  until (aVeggies[V0].Group <= iGroup) and (not aVeggiesDone[V0]);
  aVeggiesDone[V0] := True;                                                    // mark this vegetable as "done"
  // Randomly choose one of the possibly 2 vegetable names (for display in question text)
  R := 0;
  if aVeggies[V0].Name2 <> '' then
    R := Random(2);
  if R = 0 then
    S := aVeggies[V0].Name
  else
    S := aVeggies[V0].Name2;
  laQuestion.Caption := 'Welches dieser Bilder zeigt das Gemüse ';
  laQuestion.Caption := laQuestion.Caption + UTF8UpperCase(S) + '?';
  // Randomly choose 8 vegetable images
  for I := 0 to 7 do begin
    VShown[I] := 0;
    repeat
      OK := False;
      V := Random(iTotalVeggies);
      if (V <> V0) and (aVeggies[V].Group <= iGroup) then begin
        // Vegetable must be different from the one asked for and be part of actual vegetable group
        OK := True;
        // Vegetables must be unique
        for J := 0 to I - 1 do begin
          if V = VShown[J] then
            OK := False;
        end;
      end;
    until OK;
    // Display vegetable picture
    Filename := GetFilename(aVeggies[V].Name);
    imVeggies[I].Picture.LoadFromFile(Filename);
    VShown[I] := V;                                                            // save vegetable chosen (for duplicates test)
  end;
  // Now place the vegetable asked for at a random position within the picture-grid (replacing one of the pics there)
  iPic := Random(8);
  Filename := GetFilename(aVeggies[V0].Name);
  imVeggies[iPic].Picture.LoadFromFile(Filename);
  // Disable the "Frage" button and enable pictures onClick event (-> users can give their answer)
  btStart.Enabled := False; bAnswer := True;
end;

{ Click on one of the 8 vegetable images: Select this vegetable as user's answer }

procedure TfVeggies.imVeggie1Click(Sender: TObject);

begin
  Veggieselect(0, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie2Click(Sender: TObject);

begin
  Veggieselect(1, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie3Click(Sender: TObject);

begin
  Veggieselect(2, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie4Click(Sender: TObject);

begin
  Veggieselect(3, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie5Click(Sender: TObject);

begin
  Veggieselect(4, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie6Click(Sender: TObject);

begin
  Veggieselect(5, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie7Click(Sender: TObject);

begin
  Veggieselect(6, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

procedure TfVeggies.imVeggie8Click(Sender: TObject);

begin
  Veggieselect(7, iPic, bAnswer, imVeggies, shSelection, iVeggie, iVeggies, iCorrect);
end;

end.

