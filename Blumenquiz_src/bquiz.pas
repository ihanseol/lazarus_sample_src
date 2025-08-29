{****************************************}
{* Main unit for Blumenquiz application *}
{****************************************}

unit bquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, Grids, LazUTF8, LCLIntf, flowers;

type
  TFlower = record
    Name, SimpleName, SciName: string;
    Levels: array[0..2] of Boolean;
    UseColors: Boolean;
    Colors: array[0..5] of Boolean;
    UseSeasons: Boolean;
    Seasons: array[0..3] of Boolean;
    UseSci: Boolean;
    AltNames: array of string;
  end;
  TFlowers = array of TFlower;
  {*********}
  { TfBQuiz }
  {*********}
  TfBQuiz = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuiz1, mQuiz2, mQuiz3, mQuiz4, mQuiz5, mQuiz6, mQuiz7, mQuiz8, mQuizExit: TMenuItem;
    mOptions, mOptionsQuestions, mOptionsNamesSimple, mOptionsNamesShort, mOptionsGray: TMenuItem;
    mOptionsFlowers, mOptionsFlowers20, mOptionsFlowers50, mOptionsFlowersAll: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    laQuestion, Label1: TLabel;
    edQuestion: TEdit;
    cobNames: TComboBox;
    cbList: TCheckBox;
    edName: TEdit;
    imFlower: TImage;
    cbPicture: TCheckBox;
    cbFlower1, cbFlower2, cbFlower3, cbFlower4, cbFlower5, cbFlower6: TCheckBox;
    cbColor1, cbColor2, cbColor3, cbColor4, cbColor5, cbColor6: TCheckBox;
    cbSeason1, cbSeason2, cbSeason3, cbSeason4: TCheckBox;
    sgEval: TStringGrid;
    imEval: TImage;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mQuiz1Click(Sender: TObject);
    procedure mQuiz2Click(Sender: TObject);
    procedure mQuiz3Click(Sender: TObject);
    procedure mQuiz4Click(Sender: TObject);
    procedure mQuiz5Click(Sender: TObject);
    procedure mQuiz6Click(Sender: TObject);
    procedure mQuiz7Click(Sender: TObject);
    procedure mQuiz8Click(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsQuestionsClick(Sender: TObject);
    procedure mOptionsFlowers20Click(Sender: TObject);
    procedure mOptionsFlowers50Click(Sender: TObject);
    procedure mOptionsFlowersAllClick(Sender: TObject);
    procedure mOptionsNamesSimpleClick(Sender: TObject);
    procedure mOptionsNamesShortClick(Sender: TObject);
    procedure mOptionsGrayClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure cbListChange(Sender: TObject);
  private
    iQuiz, iFlower, iColor, iSeason, iAltName, iLevel, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sNames, sColor: string;
    aFlowers: TFlowers;
    aFlowersDone: array of Boolean;
    aFlowersSel: array[0..5] of Integer;
    cbFlowers: array[0..5] of TCheckBox;
    cbColors: array[0..5] of TCheckBox;
    cbSeasons: array[0..3] of TCheckBox;
  end;

var
  fBQuiz: TfBQuiz;

implementation

{$R *.lfm}

{ Check if a string is entirely composed of spaces }

function IsAllSpaces(S: string): Boolean;

var
  I: Integer;
  ItIs: Boolean;

begin
  ItIs := True;
  I := 1;
  while ItIs and (I <= UTF8Length(S)) do begin
    if UTF8Copy(S, I, 1) <> ' ' then
      ItIs := False;
    Inc(I);
  end;
  Result := ItIs;
end;

{ Format integer number for grid display (right-align) }

function GFormat(N: Integer; S: string): string;

var
  NF: string;

begin
  NF := ' ' + IntToStr(N);
  if N < 10 then
    NF := '  ' + NF
  else if N < 100 then
    NF := ' ' + NF;
  if S = '' then
    NF := ' ' + NF
  else
    NF += S;                                                                   // this is for the "%" sign
  Result := NF;
end;

{ Get specific name of flower: long, short, simplified or scientific }

function GetFlowerName(Flower: TFlower; NameUsed: string): string;

var
  FlowerName: string;

begin
  if NameUsed = 'sci' then
    FlowerName := Flower.SciName
  else begin
    if NameUsed = 'simple' then
      FlowerName := Flower.SimpleName
    else
      FlowerName := Flower.Name;
    if (NameUsed = 'simple') or (NameUsed = 'short') then begin
      // Remove "common" words from name
      FlowerName := StringReplace(FlowerName, 'Echter', '', []);
      FlowerName := StringReplace(FlowerName, 'Gemeiner', '', []);
      FlowerName := StringReplace(FlowerName, 'Gewöhnlicher', '', []);
      FlowerName := StringReplace(FlowerName, 'Echtes', '', []);
      FlowerName := StringReplace(FlowerName, 'Gemeines', '', []);
      FlowerName := StringReplace(FlowerName, 'Gewöhnliches', '', []);
      FlowerName := StringReplace(FlowerName, 'Echte', '', []);
      FlowerName := StringReplace(FlowerName, 'Gemeine', '', []);
      FlowerName := StringReplace(FlowerName, 'Gewöhnliche', '', []);
    end;
  end;
  Result := FlowerName;
end;

{ Get flower picture file name }

function GetFilename(FlowerName: string; Colour: string): string;

const
  Path = './pics/';

var
  Filename: string;

begin
  Filename := FlowerName;
  Filename := StringReplace(Filename, 'ä', 'ae', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ö', 'oe', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ü', 'ue', [rfReplaceAll]);
  Filename := StringReplace(Filename, 'ß', 'ss', [rfReplaceAll]);
  Filename := StringReplace(Filename, ' ', '_', [rfReplaceAll]);
  if Colour = 'gray' then
    // Gray-scaled pics are colored pics filenames plus "2"
    Filename += '2';
  Filename := Path + Filename + '.jpg'; DoDirSeparators(Filename);
  Result := Filename;
end;

{ Read flower data from text file and fill TFlowers record }

procedure ReadFlowers(out Flowers: TFlowers);

var
  SStart, SEnd, N, M, I, P: Integer;
  Line, AllAltNames: string;
  ColorCode: Char;
  InFile: Text;

begin
  SetLength(Flowers, 0);
  Assign(InFile, 'Blumen.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := UTF8Trim(Line);
    if Line <> '' then begin
      Inc(N); SetLength(Flowers, N);
      with Flowers[N - 1] do begin
        // Long name and simplified name
        Name := UTF8Trim(UTF8Copy(Line, 1, 30));
        SimpleName := UTF8Trim(UTF8Copy(Line, 41, 20));
        if IsAllSpaces(SimpleName) then
          SimpleName := Name;
        // Scientific name (do not use "special" names, such as hybrids)
        P := UTF8Pos('×', UTF8Copy(Line, 76, 30));
        if P <> 0 then
          UseSci := False
        else begin
          P := UTF8Pos('sect.', UTF8Copy(Line, 76, 30));
          if P <> 0 then
            UseSci := False
          else begin
            UseSci := True;
            SciName := UTF8Trim(UTF8Copy(Line, 76, 30));
          end;
        end;
        // Level (corresponding to number of flowers used in quiz)
        for I := 0 to 2 do begin
          Levels[I] := False;
          if UTF8Copy(Line, 31 + I, 1) = IntToStr(I + 1) then
            Levels[I] := True;
        end;
        // Flower color
        if UTF8Copy(Line, 61, 1) = '-' then
          // Do not use color, if color-code starts with "-"
          UseColors := False
        else begin
          UseColors := True;
          for I := 0 to 5 do
            Colors[I] := False;
          for I := 0 to 3 do begin
            // There may be from 1 to 3 flowercodes; set the corresponding element in the
            // flower's colors array to True
            ColorCode := UTF8Copy(Line, 61 + I, 1)[1];
            if (ColorCode = 'b') or (ColorCode = 'v') then begin
              Colors[2] := True; Colors[3] := True;                            // for blue-violet, set both "blue" and "violet" element to True
            end
            else begin
              case ColorCode of
                          'W', 'w': Colors[0] := True;                         // "weiß" and "weißlich"
                          'G', 'g': Colors[1] := True;                         // "gelb" and "gelblich"
                          'B': Colors[2] := True;                              // "blau"
                          'V': Colors[3] := True;                              // "violet"
                'R', 'r', 'P': Colors[4] := True;                              // "rot", "rosa" and "purpur"
                          'O': Colors[5] := True;                              // "orange"
              end;
            end;
          end;
        end;
        // Flowering seasons
        UseSeasons := False;
        for I := 0 to 3 do
          Seasons[I] := False;
        for I := 0 to 1 do begin
          // There may be 2 flowering months periods entries in the file; for each of them,
          // set the corresponding element in the flower's seasons array to True
          if not IsAllSpaces(UTF8Copy(Line, 66 + I * 5, 4)) then begin
            UseSeasons := True;
            SStart := StrToInt(UTF8Copy(Line, 66 + I * 5, 2));
            SEnd := StrToInt(UTF8Copy(Line, 68 + I * 5, 2));
            if (SStart < 3) or (SStart = 12) then
              Seasons[3] := True;
            if (SStart < 6) and (SEnd >= 3) then
              Seasons[0] := True;
            if (SStart < 9) and (SEnd >= 6) then
              Seasons[1] := True;
            if (SStart < 12) and (SEnd >= 9) then
              Seasons[2] := True;
          end;
        end;
        // Alternate names
        SetLength(AltNames, 0);
        if UTF8Length(Line) > 106 then begin
          AllAltNames := UTF8Trim(UTF8Copy(Line, 106, UTF8Length(Line))); M := 0;
          while AllAltNames <> '' do begin
            // Extract individual alternate names (separated by "," in the file) and fill in array
            P := UTF8Pos(', ', AllAltNames);
            if P = 0 then begin
              if LeftStr(AllAltNames, 1) <> '-' then begin                     // ignore alternate names starting with "-"
                Inc(M); SetLength(AltNames, M);
                AltNames[M - 1] := AllAltNames;
              end;
              AllAltNames := '';
            end
            else begin
              if LeftStr(UTF8Copy(AllAltNames, 1, P - 1), 1) <> '-' then begin // ignore alternate names starting with "-"
                Inc(M); SetLength(AltNames, M);
                AltNames[M - 1] := UTF8Copy(AllAltNames, 1, P - 1);
              end;
              UTF8Delete(AllAltNames, 1, P + 1);
            end;
          end;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ New quiz: Prepare the form (showing/hiding controls as applicable) }

procedure NewQuiz(Quiz: Integer; Colour: string; var Seasons, Colors, Flowers: array of TCheckBox);

const
  Quizes: array[1..8] of string = (
    'Blumen-Bilder (Name)', 'Blumen-Bilder (Auswahl)', 'Blütenfarben (Farbe)', 'Blütenfarben (Blumen)',
    'Blütezeit (Jahreszeit)', 'Blütezeit (Blumen)', 'Systematik (wissenschaftlich)', 'Systematik (Alternativnamen)'
  );

var
  I: Integer;

begin
  fBQuiz.stTitle.Caption := 'Blumenquiz: ' + Quizes[Quiz] + '.';
  // Set all to default and then adapt depending on quiz type
  for I := 0 to 3 do
    Seasons[I].Visible := False;
  for I := 0 to 5 do begin
    Flowers[I].Visible := False;
    Colors[I].Visible := False;
  end;
  fBQuiz.cobNames.Visible := False; fBQuiz.edName.Visible := False; fBQuiz.cbList.Visible := False;
  fBQuiz.imFlower.Visible := True; fBQuiz.cbPicture.Visible := False;
  case Quiz of
    // Flower-pictures quiz (name)
    1: begin
      fBQuiz.edQuestion.Text := 'Raten, wie eine bestimmte Blume heißt.';
      fBQuiz.cbList.Visible := True;
      if fBQuiz.cbList.Checked then begin
        // Answer from list: show combobox
        fBQuiz.cobNames.Visible := True; fBQuiz.cobNames.Clear;
      end
      else begin
        // Manual answer: Show edit field
        fBQuiz.edName.Visible := True; fBQuiz.edName.Text := '';
      end;
    end;
    // Flower-pictures quiz (flowers)
    2: begin
      fBQuiz.edQuestion.Text := 'Raten, welche Blume einen bestimmten Namen hat.';
    end;
    // Flower-color quiz (color)
    3: begin
      fBQuiz.edQuestion.Text := 'Raten, welche Farbe die Blüte einer bestimmten Blume hat.';
      for I := 0 to 5 do begin
        // Show color-selection checkboxes
        Colors[I].Visible := True; Colors[I].Checked := False;
      end;
      fBQuiz.cbPicture.Visible := True; Colour := 'gray';
    end;
    // Flower-color quiz (flowers)
    4: begin
      fBQuiz.edQuestion.Text := 'Raten, welche Blumen Blüten von einer bestimmten Farbe haben.';
      for I := 0 to 5 do begin
        // Show flower-selection checkboxes (names will be filled in during quiz)
        Flowers[I].Visible := True; Flowers[I].Checked := False;
        Flowers[I].Caption := 'Blume ' + IntToStr(I + 1);
      end;
      fBQuiz.cbPicture.Visible := True; Colour := 'gray';
    end;
    // Flowering-season quiz (season)
    5: begin
      fBQuiz.edQuestion.Text := 'Raten, in welcher Jahreszeit eine bestimmte Blumen blüht.';
      for I := 0 to 3 do begin
        // Show season-selection checkboxes
        Seasons[I].Visible := True; Seasons[I].Checked := False;
      end;
      fBQuiz.cbPicture.Visible := True;
    end;
    // Flowering-season quiz (flowers)
    6: begin
      fBQuiz.edQuestion.Text := 'Raten, welche Blumen in einer bestimmten Jahreszeit blühen.';
      for I := 0 to 5 do begin
        // Show flower-selection checkboxes (names will be filled in during quiz)
        Flowers[I].Visible := True; Flowers[I].Checked := False;
        Flowers[I].Caption := 'Blume ' + IntToStr(I + 1);
      end;
      fBQuiz.cbPicture.Visible := True;
    end;
    // Scientific names quiz
    7: begin
      fBQuiz.edQuestion.Text := 'Raten, wie der wissenschaftliche Name einer Blume lautet.';
      fBQuiz.cbList.Visible := True;
      if fBQuiz.cbList.Checked then begin
        // Answer from list: show combobox
        fBQuiz.cobNames.Visible := True; fBQuiz.cobNames.Clear;
      end
      else begin
        // Manual answer: show edit field
        fBQuiz.edName.Visible := True; fBQuiz.edName.Text := '';
      end;
      fBQuiz.cbPicture.Visible := True;
    end;
    // Alternate names quiz
    8: begin
      fBQuiz.edQuestion.Text := 'Raten, welche Blume mit einem bestimmten Namen bezeichnet wird.';
      fBQuiz.edName.Visible := True; fBQuiz.edName.Text := '';                 // show edit field (always manual answer)
      fBQuiz.cbPicture.Visible := True;  fBQuiz.cbPicture.Checked := False;
    end;
  end;
  // Load defalt picture (colored or black/white)
  fBQuiz.imFlower.Picture.LoadFromFile(GetFilename('Blumen', Colour));
  // Clear evaluation grid
  for I := 0 to 3 do
    fBQuiz.sgEval.Cells[1, I] := '';
  fBQuiz.imEval.Visible := False;
  // Give user access to "Einstellungen" menu again
  fBQuiz.mOptions.Enabled := True;
  // Re-enable "Start" button
  fBQuiz.btQuestion.Caption := 'Start'; fBQuiz.btQuestion.Enabled := True;
end;

{ Fill flowernames combobox with 10 (partially) random flower names }

procedure FillCobFlowers(var Flowers: TFlowers; NameUsed: string; Quiz, Level, Flower: Integer);

var
  FX, I, J: Integer;
  Temp: string;
  OK: Boolean;
  Used: array[0..9] of Integer;
  CBFlowers: array[0..9] of string;

begin
  fBQuiz.cobNames.Clear;
  for I := 0 to 9 do
    Used[I] := -1;
  // Fill temporary array with 10 flower names
  for I := 0 to 9 do begin
    if I = 0 then
      // Fill-in the flower passed as argument (this is the one corr. to the correct answer)
      FX := Flower
    else begin
      // Fill-in a random flower (that must however satisfy some conditions)
      repeat
        OK := True;
        FX := Random(Length(Flowers));
        if not Flowers[FX].Levels[Level] then
          // For all quizes, flower must be one of those for actual level
          OK := False
        else if (Quiz = 7) and (not Flowers[FX].UseSci) then
          // For scintific names quiz, flower must not be one of those with "special name"
          OK := False
        else begin
          // Be sure not to fill in duplicates
          for J := 0 to I - 1 do begin
            if FX = Used[J] then
              OK := False;
          end;
        end;
      until OK;
    end;
    Used[I] := FX;                                                             // remember flowers already used
    CBFlowers[I] := GetFlowerName(Flowers[FX], NameUsed);                      // store the flowername into temporary array
  end;
  // Sort the flowernames array alphabetically
  for I := 0 to 8 do begin
    for J := I + 1 to 9 do begin
      if CBFlowers[J] < CBFlowers[I] then begin
        Temp := CBFlowers[I]; CBFlowers[I] := CBFlowers[J]; CBFlowers[J] := Temp;
      end;
    end;
  end;
  // Fill in the combobox
  for I := 0 to 9 do begin
    if (I = 0) or (CBFlowers[I] <> CBFlowers[I - 1]) then                      // eliminate duplicates, that may ocuur when simplified names are used
      fBQuiz.cobNames.Items.AddText(CBFlowers[I]);
  end;
end;

{ Fill flower images (on fFlowers form) with 6 random flower pictures) }

procedure FillImgFlowers(var Flowers: TFlowers; NamesUsed, Colour: string; Quiz, Level, Flower: Integer;
  DisplayPic, DisplayName: Boolean; out FlowersSel: array of Integer);

var
  FX, I, J, I1, I2: Integer;
  OK: Boolean;
  CBFlowers, CBFlowers2: array[0..5] of string;

begin
  for I := 0 to 5 do
    FlowersSel[I] := -1;
  for I := 0 to 5 do begin
    if (Flower <> -1) and (I = 0) then
      // If there is a flower passed as argument (the one that actually is the correct solution), fill it in here
      FX := Flower
    else begin
      // Otherwise fill-in a random flower (that must however satisfy some conditions, depending on actual quiz type
      repeat
        OK := True;
        FX := Random(Length(Flowers));
        if not Flowers[FX].Levels[Level] then
          // For all quizes, flower must be one of those for actual level
          OK := False
        else if (Quiz = 4) and (not Flowers[FX].UseColors) then
          // For flower color quiz, flower must be one of those with colors defined
          OK := False
        else if (Quiz = 6) and (not Flowers[FX].UseSeasons) then
          // For flowering season quiz, flower must be one of those with flowering season defined
          OK := False
        else begin
          // Be sure not to fill in duplicates
          for J := 0 to I - 1 do begin
            if FX = FlowersSel[J] then
              OK := False;
          end;
        end;
      until OK;
    end;
    FlowersSel[I] := FX;                                                       // save index of flower into array (will be returned and used for answer check!)
  end;
  if Flower <> -1 then begin
    // As the flower passed is just filled in at pos. 0, must "shuffle" to give it a random position
    for I := 1 to 50 do begin
      repeat
        I1 := Random(5); I2 := Random(5);
      until I1 <> I2;
      FX := FlowersSel[I1]; FlowersSel[I1] := FlowersSel[I2]; FlowersSel[I2] := FX;
    end;
  end;
  // Fill in flower names and/or pics as applicable
  for I := 0 to 5 do begin
    CBFlowers[I] := GetFlowerName(Flowers[FlowersSel[I]], NamesUsed);
    CBFlowers2[I] := Flowers[FlowersSel[I]].Name;
    if DisplayPic then begin
      // Flower picture has to be displayed
      fFlowers.imFlowers[I].Visible := True;
      fFlowers.imFlowers[I].Picture.LoadFromFile(GetFileName(CBFlowers2[I], Colour));
    end;
    if DisplayName then begin
      // Flower name has to be displayed
      if (Quiz in [4, 6]) and (not DisplayPic) then
        fBQuiz.cbFlowers[I].Caption := ' ' + CBFlowers[I];                     // display name on main form
      fFlowers.cbFlowers[I].Caption := ' ' + CBFlowers[I];                     // display name on fFlowers form
    end
    else
      // With quiz 2, no fill-in of the names, of course
      fFlowers.cbFlowers[I].Caption := ' Blume ' + IntToStr(I + 1);            // display "Blume" on fFlowers form
    fBQuiz.cbFlowers[I].Checked := False;
    fFlowers.cbFlowers[I].Checked := False;
  end;
end;

{*********}
{ TfBQuiz }
{*********}

{ Application start: Initialisation }

procedure TfBQuiz.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create arrays with check boxes
  cbFlowers[0] := cbFlower1; cbFlowers[1] := cbFlower2; cbFlowers[2] := cbFlower3;
  cbFlowers[3] := cbFlower4; cbFlowers[4] := cbFlower5; cbFlowers[5] := cbFlower6;
  cbColors[0] := cbColor1; cbColors[1] := cbColor2; cbColors[2] := cbColor3;
  cbColors[3] := cbColor4; cbColors[4] := cbColor5; cbColors[5] := cbColor6;
  cbSeasons[0] := cbSeason1; cbSeasons[1] := cbSeason2; cbSeasons[2] := cbSeason3; cbSeasons[3] := cbSeason4;
  // Re-arrange controls on the form (where they are displayed "suitable for the programmer")
  for I := 0 to 1 do begin
    cbFlowers[I].Top := 128; cbFlowers[I + 2].Top := 168; cbFlowers[I + 4].Top := 208;
  end;
  for I := 0 to 2 do begin
    cbColors[I].Top := 128; cbColors[I + 3].Top := 168;
  end;
  cobNames.Top := 128; edName.Top := 128; cbList.Top := 130;
  fBQuiz.Height := 348;
  // Read flowers data from file
  ReadFlowers(aFlowers);
  SetLength(aFlowersDone, Length(aFlowers));
  // Start-up values
  iQuestionsTemp := 20; iLevel := 0; sColor := 'color';
  // Start random number generator
  Randomize;
  // Start application with Quiz 1
  mQuiz1.Click;
end;

{ Menu items "Quiz > ...": Start the corr. quiz }

procedure TfBQuiz.mQuiz1Click(Sender: TObject);

begin
  iQuiz := 1; mOptionsNamesSimple.Enabled := True;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz2Click(Sender: TObject);

begin
  iQuiz := 2; mOptionsNamesSimple.Enabled := True;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz3Click(Sender: TObject);

begin
  iQuiz := 3;
  mOptionsNamesSimple.Checked := False; mOptionsNamesSimple.Enabled := False;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz4Click(Sender: TObject);

begin
  iQuiz := 4;
  mOptionsNamesSimple.Checked := False; mOptionsNamesSimple.Enabled := False;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz5Click(Sender: TObject);

begin
  iQuiz := 5;
  mOptionsNamesSimple.Checked := False; mOptionsNamesSimple.Enabled := False;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz6Click(Sender: TObject);

begin
  iQuiz := 6;
  mOptionsNamesSimple.Checked := False; mOptionsNamesSimple.Enabled := False;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz7Click(Sender: TObject);

begin
  iQuiz := 7;
  mOptionsNamesSimple.Checked := False; mOptionsNamesSimple.Enabled := False;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

procedure TfBQuiz.mQuiz8Click(Sender: TObject);

begin
  iQuiz := 8;
  mOptionsNamesSimple.Checked := False; mOptionsNamesSimple.Enabled := False;
  NewQuiz(iQuiz, sColor, cbSeasons, cbColors, cbFlowers);
end;

{ Menu item "Quiz > Verlassen": Exit application }

procedure TfBQuiz.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Quizfragen": User entry of number of quiz questions }

procedure TfBQuiz.mOptionsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Blumenquiz', 'Anzahl der Fragen', IntToStr(iQuestionsTemp));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);                                             // value entered will be adapted, when "Start" button pushed
end;

{ Menu items "Einstellungen > Blumenliste > ...": Selection of number of flowers to be used with actual quiz (= level) }

procedure TfBQuiz.mOptionsFlowers20Click(Sender: TObject);

begin
  mOptionsFlowers20.Checked := True; mOptionsFlowers50.Checked := False; mOptionsFlowersAll.Checked := False;
  iLevel := 0;
end;

procedure TfBQuiz.mOptionsFlowers50Click(Sender: TObject);

begin
  mOptionsFlowers20.Checked := False; mOptionsFlowers50.Checked := True; mOptionsFlowersAll.Checked := False;
  iLevel := 1;
end;

procedure TfBQuiz.mOptionsFlowersAllClick(Sender: TObject);

begin
  mOptionsFlowers20.Checked := False; mOptionsFlowers50.Checked := False; mOptionsFlowersAll.Checked := True;
  iLevel := 2;
end;

{ Menu item "Einstellungen > Kurze Namen": Toggle usage of short names or not }

procedure TfBQuiz.mOptionsNamesShortClick(Sender: TObject);

begin
  if mOptionsNamesShort.Checked then
    mOptionsNamesShort.Checked := False
  else
    mOptionsNamesShort.Checked := True;
end;

{ Menu item "Einstellungen > Kurze Namen": Toggle usage of simplified names or not }

procedure TfBQuiz.mOptionsNamesSimpleClick(Sender: TObject);

begin
  if mOptionsNamesSimple.Checked then
    mOptionsNamesSimple.Checked := False
  else
    mOptionsNamesSimple.Checked := True;
end;

{ Menu item "Einstellungen > S/w Bilder": Toggle usage of colored or gray-scaled pictures }

procedure TfBQuiz.mOptionsGrayClick(Sender: TObject);

begin
  if mOptionsGray.Checked then begin
    mOptionsGray.Checked := False;
    sColor := 'color';
  end
  else begin
    mOptionsGray.Checked := True;
    sColor := 'gray';
  end;
end;

{ Menu item "Hilfe > Hilfe": Display application help (HTML file opened in browser) }

procedure TfBQuiz.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Hilfe > Über": Display aplication about }

procedure TfBQuiz.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Quiz über die bekanntesten europäischen Blumen:' + LineEnding;
  S += 'Blumen erkennen, Blütenfarbe, Blütezeit, deutsche und wissenschaftliche Namen.' + LineEnding + LineEnding;
  S += 'Version 1.0.1, © allu, Oktober 2020 - April 2021.';
  MessageDlg('Über "Blumenquiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Frage/Antwort": Generate new question resp. check user answer }

procedure TfBQuiz.btQuestionClick(Sender: TObject);

const
  Max: array[0..2] of Integer = (
    20, 50, 90
  );

var
  I: Integer;
  Colour, Answer, UAnswer: string;
  OK, Correct: Boolean;

begin
  // Button "Start/Frage": Generate new question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Frage') then begin
    // Button "Start": Initialize the quiz
    if btQuestion.Caption = 'Start' then begin
      fBQuiz.mOptions.Enabled := False;                                        // block user access to "Einstellungen" menu
      // Determine number of questions to be used for actual quiz
      // This one depends on: value entered by user, number of flowers for this level, flowers not to be considered for given quiz
      // Start with setting number of questions to number of flowers for this level
      iQuestions := Max[iLevel];
      // Decrease this number for each flower (of this level!) that has not to be considered for a given quiz
      if iQuiz = 3 then begin
        // Flower color quiz: color must be defined
        for I := 0 to Length(aFlowers) - 1 do begin
          if aFlowers[I].Levels[iLevel] and not aFlowers[I].UseColors then
            Dec(iQuestions);
        end;
      end
      else if iQuiz = 5 then begin
        // Flowering season quiz: season must be defined
        for I := 0 to Length(aFlowers) - 1 do begin
          if aFlowers[I].Levels[iLevel] and not aFlowers[I].UseSeasons then
            Dec(iQuestions);
        end;
      end
      else if iQuiz = 7 then begin
        // Scientific names quiz: not using flowers with "special" scientific names
        for I := 0 to Length(aFlowers) - 1 do begin
          if aFlowers[I].Levels[iLevel] and not aFlowers[I].UseSci then
            Dec(iQuestions);
        end;
      end
      else if iQuiz = 8 then begin
        // Alternate names quiz: can only use flowers that actually have alternate name (and is one of those not marked as to be not considered)
        for I := 0 to Length(aFlowers) - 1 do begin
          if aFlowers[I].Levels[iLevel] and (Length(aFlowers[I].AltNames) = 0) then
            Dec(iQuestions);
        end
      end;
      // If the question number thus determined (= max possible for this quiz) is greater than the number entered by the user, use this last number
      if iQuestions > iQuestionsTemp then
        iQuestions := iQuestionsTemp;
      // Check settings to determine which flower name ("normal", short, simple) has to be used in this quiz
      if mOptionsNamesSimple.Checked then
        sNames := 'simple'
      else begin
        if mOptionsNamesShort.Checked then
          sNames := 'short'
        else
          sNames := 'long';
      end;
      // Initialize variables
      iQuestion := 0; iCorrect := 0;
      for I := 0 to Length(aFlowersDone) - 1 do
        aFlowersDone[I] := False;
      cbList.Visible := False; cbPicture.Visible := False;
    end;
    // From here code is executed for "Start" as well as for "Frage"...
    Inc(iQuestion);
    laQuestion.Caption := 'Frage ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + '.';
    // Choose a random flower (among those not yet chosen), that verifies conditions for thiy type of quiz
    repeat
      OK := True;
      iFlower := Random(Length(aFlowers));
      if aFlowersDone[iFlower] then
        // Flower has already been used
        OK := False
      else if not aFlowers[iFlower].Levels[iLevel] then
        // Flower must be of actual level
        OK := False
      else if (iQuiz in [3, 4]) and (not aFlowers[iFlower].UseColors) then
        // For flower color quizes, color must be defined
        OK := False
      else if (iQuiz in [5, 6]) and (not aFlowers[iFlower].UseSeasons) then
        // For flowering season quizes, season must be defined
        OK := False
      else if (iQuiz = 7) and (not aFlowers[iFlower].UseSci) then
        // For scientific names quiz, name must not be "special"
        OK := False
      else if (iQuiz = 8) and (Length(aFlowers[iFlower].AltNames) = 0) then
        // For alternate names quiz, alternate name must exist (and not be marked not to be considered)
        OK := False;
    until OK;
    aFlowersDone[iFlower] := True;                                             // mark this flower as "used in this quiz"
    // Generate the question (depending on quiz type)
    if iQuiz = 1 then begin
      // Flower picture quiz (name)
      edQuestion.Text := 'Wie heißt die Blume auf dem Bild?';
      if cbList.Checked then begin
        FillCobFlowers(aFlowers, sNames, iQuiz, iLevel, iFlower);
      end
      else
        edName.Text := '';
      imFlower.Visible := True; imFlower.Picture.LoadFromFile(GetFilename(aFlowers[iFlower].Name, sColor));
    end
    else if iQuiz = 2 then begin
      // Flower picture quiz (flowers)
      edQuestion.Text := 'Welche der Blumen auf den 6 Bildern heißt "' + GetFlowerName(aFlowers[iFlower], sNames) + '"?';
      fFlowers.laQuestion.Caption := 'Welche dieser Blumen heißt "' + GetFlowerName(aFlowers[iFlower], sNames) + '"?';
      FillImgFlowers(aFlowers, sNames, sColor, iQuiz, iLevel, iFlower, True, False, aFlowersSel);
      fFlowers.ShowModal;                                                      // flower pics to select from on fFlowers form
    end
    else if iQuiz = 3 then begin
      // Flowers color quiz (color)
      edQuestion.Text := 'Welche Farbe haben die Blüten der Blume "' + aFlowers[iFlower].Name + '"?';
      for I := 0 to 5 do
        cbColors[I].Checked := False;
      if cbPicture.Checked then begin
        imFlower.Visible := True; imFlower.Picture.LoadFromFile(GetFilename(aFlowers[iFlower].Name, 'gray'));
      end
      else
        imFlower.Visible := False;
    end
    else if iQuiz = 4 then begin
      // Flowers color quiz (flowers)
      iColor := Random(6);                                                     // random color
      Colour := LowerCase(cbColors[iColor].Caption);
      if RightStr(Colour, 1) <> 'e' then
        Colour += 'e';
      edQuestion.Text := 'Welche der 6 angegebenen Blumen hat ' + Colour + ' Blüten?';
      fFlowers.laQuestion.Caption := 'Welche dieser Blumen hat ' + Colour + ' Blüten?';
      FillImgFlowers(aFlowers, sNames, 'gray', iQuiz, iLevel, -1, cbPicture.Checked, True, aFlowersSel);
      if cbPicture.Checked then begin
        // Display (black/white) pictures on fFlowers form and show this form only if selected to show pictures
        for I := 0 to 5 do
          fFlowers.cbFlowers[I].Checked := False;
        fFlowers.ShowModal;
        // Fill-in main form data, as is on fFlowers form
        for I := 0 to 5 do begin
          cbFlowers[I].Caption := fFlowers.cbFlowers[I].Caption;
          cbFlowers[I].Checked := fFlowers.cbFlowers[I].Checked;
        end;
      end;
    end
    else if iQuiz = 5 then begin
      // Flowering seasons quiz (season)
      edQuestion.Text := 'In welcher Jahreszeit blüht die Blume "' + aFlowers[iFlower].Name + '"?';
      for I := 0 to 3 do
        cbSeasons[I].Checked := False;
      if cbPicture.Checked then begin
        imFlower.Visible := True; imFlower.Picture.LoadFromFile(GetFilename(aFlowers[iFlower].Name, sColor));
      end
      else
        imFlower.Visible := False;
    end
    else if iQuiz = 6 then begin
      // Flowering season quiz (flowers)
      iSeason := Random(10) div 3;                                             // just 1/3 probability of other seasons for winter
      edQuestion.Text := 'Welche der 6 angegebenen Blumen blühen im' + cbSeasons[iSeason].Caption + '?';
      fFlowers.laQuestion.Caption := 'Welche dieser Blumen blühen im' + cbSeasons[iSeason].Caption + '?';
      FillImgFlowers(aFlowers, sNames, sColor, iQuiz, iLevel, -1, cbPicture.Checked, True, aFlowersSel);
      if cbPicture.Checked then begin
        // Display pictures on fFlowers form and show this form only if selected to show pictures
        for I := 0 to 5 do
          fFlowers.cbFlowers[I].Checked := False;
        fFlowers.ShowModal;
        for I := 0 to 5 do begin
          // Fill-in main form data, as is on fFlowers form
          cbFlowers[I].Caption := fFlowers.cbFlowers[I].Caption;
          cbFlowers[I].Checked := fFlowers.cbFlowers[I].Checked;
        end;
      end;
    end
    else if iQuiz = 7 then begin
      // Scientific names quiz
      edQuestion.Text := 'Wie lautet der wissenschaftliche Name der Blume "' + aFlowers[iFlower].Name + '"?';
      if cbList.Checked then
        FillCobFlowers(aFlowers, 'sci', iQuiz, iLevel, iFlower)
      else
        edName.Text := '';
      if cbPicture.Checked then begin                                          // Display flower picture (if so selected)
        imFlower.Visible := True; imFlower.Picture.LoadFromFile(GetFilename(aFlowers[iFlower].Name, sColor));
      end
      else
        imFlower.Visible := False;
    end
    else begin
      // Altername names quiz
      iAltName := Random(Length(aFlowers[iFlower].AltNames));
      edQuestion.Text := 'Welche Blume bezeichnet man auch als "' + aFlowers[iFlower].AltNames[iAltName] + '"?';
      edName.Text := '';
      if cbPicture.Checked then begin                                          // Display flower picture (if so selected)
        imFlower.Visible := True; imFlower.Picture.LoadFromFile(GetFilename(aFlowers[iFlower].Name, sColor));
      end
      else
        imFlower.Visible := False;
    end;
    imEval.Visible := False;
    // Next button push will be to check user answer
    btQuestion.Caption := 'Antwort';
    if (iQuiz = 2) or ((iQuiz in [4, 6]) and (cbPicture.Checked)) then
      // Answer has been given on fFlowers form: automatically check the answer (as user pushed already "Antwort" on this form)
      btQuestion.Click;
  end
  // Button "Antwort": Check user answer
  else begin
    Correct := True;
    if iQuiz in [1, 7, 8] then begin
      // For these quizes, the answer is a flower name
      case iQuiz of
        1: Answer := GetFlowerName(aFlowers[iFlower], sNames);
        7: Answer := GetFlowerName(aFlowers[iFlower], 'sci');
        8: Answer := GetFlowerName(aFlowers[iFlower], sNames);
      end;
      // The user flower name may be a manual entry or a combobox selection
      if edName.Visible then
        UAnswer := edName.Text
      else
        UAnswer := cobNames.Text;
      // Answer is correct, if the two names are equal
      if UAnswer <> Answer then begin
        Correct := False;
        if iQuiz = 1 then begin
          // This allows to enter the full name with "simplified names" selected
          if UAnswer = GetFlowerName(aFlowers[iFlower], 'long') then
            Correct := True;
        end;
      end;
    end
    else begin
      if iQuiz = 2 then begin
        // For this quiz, the answer is correct if the user checked the picture corr. to the flower name given
        // Check is done by using the array containing the indexes of the 6 flowers generated in the FillImgFlowers procedure
        for I := 0 to 5 do begin
          if fFlowers.cbFlowers[I].Checked and (aFlowersSel[I] <> iFlower) then
            Correct := False
          else if not fFlowers.cbFlowers[I].Checked and (aFlowersSel[I] = iFlower) then
            Correct := False;
        end;
      end
      else if iQuiz = 3 then begin
        // For this quiz, the answer is correct if the user checked the colors, that effectively are those of the flower given
        for I := 0 to 5 do begin
          if aFlowers[iFlower].Colors[I] and not cbColors[I].Checked then
            Correct := False
          else if not aFlowers[iFlower].Colors[I] and cbColors[I].Checked then
            Correct := False;
        end;
      end
      else if iQuiz = 4 then begin
        // For this quiz, the answer is correct if the user checked the picture corr. to the flower that has one of its colors equal to the color given
        // Check is done by using the array containing the indexes of the 6 flowers generated in the FillImgFlowers procedure
        // Knowing all these flowers indexes, easy to determine if they have the given color set to True
        for I := 0 to 5 do begin
          if aFlowers[aFlowersSel[I]].Colors[iColor] and not cbFlowers[I].Checked then
            Correct := False
          else if not aFlowers[aFlowersSel[I]].Colors[iColor] and cbFlowers[I].Checked then
            Correct := False;
        end;
      end
      else if iQuiz = 5 then begin
        // For this quiz, the answer is correct if the user checked the seasons, that effectively are those of the flower given
        for I := 0 to 3 do begin
          if aFlowers[iFlower].Seasons[I] and not cbSeasons[I].Checked then
            Correct := False
          else if not aFlowers[iFlower].Seasons[I] and cbSeasons[I].Checked then
            Correct := False;
        end;
      end
      else if iQuiz = 6 then begin
        // For this quiz, the answer is correct if the user checked the picture corr. to the flower that has one of its seasons equal to the season given
        // Check is done by using the array containing the indexes of the 6 flowers generated in the FillImgFlowers procedure
        // Knowing all these flowers indexes, easy to determine if they have the given season set to True
        for I := 0 to 5 do begin
          if aFlowers[aFlowersSel[I]].Seasons[iSeason] and not cbFlowers[I].Checked then
            Correct := False
          else if not aFlowers[aFlowersSel[I]].Seasons[iSeason] and cbFlowers[I].Checked then
            Correct := False;
        end;
      end;
    end;
    // Question evaluation
    if Correct then begin
      Inc(iCorrect);
      imEval.Picture.LoadFromFile('correct.png');
    end
    else begin
      imEval.Picture.LoadFromFile('false.png');
    end;
    imEval.Visible := True;
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be to generate another question
    btQuestion.Caption := 'Frage';
    if iQuestion = iQuestions then begin
      // All questions done: End of quiz
      MessageDlg('Blumenquiz', 'Alle Fragen gestellt. Ende des Quizes.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ User selection to select answer from list or to enter it manually ("Auswahlliste" checkbox changes) }

procedure TfBQuiz.cbListChange(Sender: TObject);

begin
  if cbList.Checked then begin
    cobNames.Visible := True; edName.Visible := False;                         // show the combobox for list selection
  end
  else begin                                                                   // show the edit field for manual entry
    cobNames.Visible := False; edName.Visible := True;
  end;
end;

end.

