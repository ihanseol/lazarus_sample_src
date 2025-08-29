{*************************************}
{* Main unit for Chinese application *}
{*************************************}

unit chinese_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, LazUTF8;

type
  TCharacter = record
    Traditional, Simplified, Pinyin: string;
    English: array of string;
    Lists: array[1..9] of Boolean;
  end;
  TCharacters = array of TCharacter;
  TIntArray = array of Integer;
  TBoolArray = array of Boolean;
  {***********}
  { TfChinese }
  {***********}
  TfChinese = class(TForm)
    mMenu: TMainMenu;
    mTest, mTest1, mTest2, mTest3, mTest4, mTestExit: TMenuItem;
    mOptions, mOptionsChinese, mOptionsChinese1, mOptionsChinese2, mOptionsChinese3: TMenuItem;
    mOptionsChars, mOptionsChars20, mOptionsChars50, mOptionsChars100, mOptionsCharsAll: TMenuItem;
    MenuItem1, mOptionsCharsA, mOptionsCharsN, mOptionsCharsV: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, laQuestion: TLabel;
    shChar0, shChar1, shChar2, shChar3, shChar4, shChar5, shChar6, shChar7, shChar8, shChar9: TShape;
    stChar0, stChar1, stChar2, stChar3, stChar4, stChar5, stChar6, stChar7, stChar8, stChar9: TStaticText;
    stWord0, stWord1, stWord2, stWord3, stWord4, stWord5, stWord6, stWord7, stWord8, stWord9: TStaticText;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mTest1Click(Sender: TObject);
    procedure mTest2Click(Sender: TObject);
    procedure mTest3Click(Sender: TObject);
    procedure mTest4Click(Sender: TObject);
    procedure mTestExitClick(Sender: TObject);
    procedure mOptionsChinese1Click(Sender: TObject);
    procedure mOptionsChinese2Click(Sender: TObject);
    procedure mOptionsChinese3Click(Sender: TObject);
    procedure mOptionsChars20Click(Sender: TObject);
    procedure mOptionsChars50Click(Sender: TObject);
    procedure mOptionsChars100Click(Sender: TObject);
    procedure mOptionsCharsAllClick(Sender: TObject);
    procedure mOptionsCharsAClick(Sender: TObject);
    procedure mOptionsCharsNClick(Sender: TObject);
    procedure mOptionsCharsVClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure shChar1MouseDown(Sender: TObject);
    procedure shChar2MouseDown(Sender: TObject);
    procedure shChar3MouseDown(Sender: TObject);
    procedure shChar4MouseDown(Sender: TObject);
    procedure shChar5MouseDown(Sender: TObject);
    procedure shChar6MouseDown(Sender: TObject);
    procedure shChar7MouseDown(Sender: TObject);
    procedure shChar8MouseDown(Sender: TObject);
    procedure shChar9MouseDown(Sender: TObject);
    procedure stChar1Click(Sender: TObject);
    procedure stChar2Click(Sender: TObject);
    procedure stChar3Click(Sender: TObject);
    procedure stChar4Click(Sender: TObject);
    procedure stChar5Click(Sender: TObject);
    procedure stChar6Click(Sender: TObject);
    procedure stChar7Click(Sender: TObject);
    procedure stChar8Click(Sender: TObject);
    procedure stChar9Click(Sender: TObject);
    procedure stWord1Click(Sender: TObject);
    procedure stWord2Click(Sender: TObject);
    procedure stWord3Click(Sender: TObject);
    procedure stWord4Click(Sender: TObject);
    procedure stWord5Click(Sender: TObject);
    procedure stWord6Click(Sender: TObject);
    procedure stWord7Click(Sender: TObject);
    procedure stWord8Click(Sender: TObject);
    procedure stWord9Click(Sender: TObject);
  private
    iTest, iListTemp, iList, iChar, iXChar, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sChineseTemp, sChinese: string;
    aCharacters: TCharacters;
    aList: TIntArray;
    aDone: TBoolArray;
    stChars, stWords: array[0..8] of TStaticText;
    shChars: array[0..8] of TShape;
  end;

const
  clGold = $00D7FF;

var
  fChinese: TfChinese;

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

{ Read chinese characters data from text file }

procedure ReadCharacters(out Characters: TCharacters);

var
  N, E, L, I, P: Integer;
  Line, Eng, S: string;
  InFile: Text;

begin
  SetLength(Characters, 0); N := 0;
  Assign(InFile, 'chinese.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(INFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Characters, N);
      // Store character data in record, being element of all characters array
      with Characters[N - 1] do begin
        Traditional := UTF8Trim(UTF8Copy(Line, 6, 5));
        Simplified := UTF8Trim(UTF8Copy(Line, 11, 5));
        Pinyin := UTF8Trim(UTF8Copy(Line, 16, 10));
        // Store English translations (separated by comma in text file) into array, part of the character's record
        S := UTF8Trim(UTF8Copy(Line, 26, UTF8Length(Line))); E := 0;
        S := StringReplace(S, ', ', ',', [rfReplaceAll]);
        while S <> '' do begin
          P := UTF8Pos(',', S);
          if P = 0 then begin
            Eng := S; S := '';
          end
          else begin
            Eng := UTF8Copy(S, 1, P - 1);
            UTF8Delete(S, 1, P);
          end;
          Inc(E); SetLength(English, E);
          English[E - 1] := Eng;
        end;
        // If actual character is part of a given characters-selection-list, store a true value in corr. list array
        for I := 1 to 9 do
          Lists[I] := False;
        // 20, 50 and 100 characters lists
        L := StrToInt(UTF8Copy(Line, 1, 1));
        if L <> 4 then begin
          Lists[3] := True;
          if L in [1, 2] then
            Lists[2] := True;
          if L = 1 then
            Lists[1] := True;
        end;
        L := StrToInt(UTF8Copy(Line, 2, 1));
        if L <> 4 then begin
          Lists[6] := True;
          if L in [1, 2] then
            Lists[5] := True;
          if L = 1 then
            Lists[4] := True;
        end;
        // Adjectives, nouns and verbs lists
        if UTF8Copy(Line, 3, 1) = 'A' then
          Lists[9] := True
        else if UTF8Copy(Line, 3, 1) = 'N' then
          Lists[7] := True
        else if UTF8Copy(Line, 3, 1) = 'V' then
          Lists[8] := True;
      end;
    end;
  end;
  Close(InFile);
end;

{ Create list with the characters, that should be included in the test (user selection in the "Options" menu) }

procedure GetList(var Characters: TCharacters; LX: Integer; out List: TIntArray; out Done: TBoolArray);

var
  N, I: Integer;

// The list array will not include the character data itself, but an index pointing to the characters in the all-characters-array
// The identical length Done array will be used to keep track of characters already asked during the test

begin
  SetLength(List, 0); N := 0;
  for I := 0 to Length(Characters) - 1 do begin
    if (LX = -1) or Characters[I].Lists[LX] then begin
      Inc(N); SetLength(List, N);
      List[N - 1] := I;
    end;
  end;
  SetLength(Done, Length(List));
  for I := 0 to Length(Done) - 1 do
    Done[I] := False;
end;

{ Prepare for a new test }

procedure NewTest(Test, ListTemp, QuestionsTemp: Integer; ChineseTemp: string; out List, Questions: Integer; out Chinese: string;
  var Characters: TCharacters; out ListCharacters: TIntArray; out Done: TBoolArray;
  var Chars, Words: array of TStaticText; var Shapes: array of TShape);

var
  I: Integer;

begin
  // Make user selected values now becoming active
  Questions := QuestionsTemp; List := ListTemp; Chinese := ChineseTemp;
  fChinese.laQuestion.Caption := 'Question';
  // Create character list (as user selected)
  GetList(Characters, List, ListCharacters, Done);
  // Show/hide fields depending on actual test settings
  // (large static texts for Chinese characters, small ones for English words and pinyin)
  fChinese.stChar0.Visible := False; fChinese.stWord0.Visible := False;
  // Character/word given (at the left)
  case Test of
    1: begin
         fChinese.stWord0.Visible := True; fChinese.stWord0.Caption := 'to learn';
       end;
    2: begin
         if Chinese = 'pinyin' then begin
           fChinese.stWord0.Visible := True; fChinese.stWord0.Caption := 'xué';
         end
         else begin
           fChinese.stChar0.Visible := True;
           if Chinese = 'traditional' then
             fChinese.stChar0.Caption := '學'
           else
             fChinese.stChar0.Caption := '学';
         end;
       end;
    3: begin
         fChinese.stChar0.Visible := True;
         if Chinese = 'traditional' then
           fChinese.stChar0.Caption := '學'
         else if Chinese = 'simplified' then
           fChinese.stChar0.Caption := '学';
       end;
    4: begin
         fChinese.stWord0.Visible := True; fChinese.stWord0.Caption := 'xué';
       end;
  end;
  // Characters/word selections (at the right)
  for I := 0 to 8 do begin
    if (Test = 1) or (Test = 4) then begin
      Chars[I].Visible := True; Words[I].Visible := False;
      if Chinese = 'traditional' then
        Chars[I].Caption := '學'
      else if Chinese = 'simplified' then
        Chars[I].Caption := '学'
      else begin
        if Test = 1 then begin
          Chars[I].Visible := False; Words[I].Visible := True;
          Words[I].Caption := 'xué';
        end;
      end;
    end
    else begin
      Chars[I].Visible := False; Words[I].Visible := True;
      if Test = 2 then
        Words[I].Caption := 'to learn'
      else
        Words[I].Caption := 'xué';
    end;
    Shapes[I].Brush.Color := clYellow;
  end;
  // Clear evaluation grid
  for I := 0 to 3 do
    fChinese.sgEval.Cells[1, I] := '';
  // (Re)enable start button
  fChinese.btQuestion.Caption := 'Start'; fChinese.btQuestion.Enabled := True;
end;

{ User click on character/word: Select or unselect it (coloring the corr. shape) }

procedure UserClick(IX: Integer; var Chars: array of TShape);

var
  I: Integer;

begin
  if fChinese.btQuestion.Caption = 'Answer' then begin
    for I := 0 to 8 do begin
      if I = IX then begin
        if Chars[I].Brush.Color = clYellow then
          Chars[I].Brush.Color := clGold
        else
          Chars[I].Brush.Color := clYellow;
      end
      else
        Chars[I].Brush.Color := clYellow;
    end;
  end;
end;

{***********}
{ TfChinese }
{***********}

{ Application start: Initialisation }

procedure TfChinese.FormCreate(Sender: TObject);

begin
  // Create arrays with large and small static texts and with corr. shapes
  stChars[0] := stChar1; stChars[1] := stChar2; stChars[2] := stChar3; stChars[3] := stChar4; stChars[4] := stChar5;
  stChars[5] := stChar6; stChars[6] := stChar7; stChars[7] := stChar8; stChars[8] := stChar9;
  stWords[0] := stWord1; stWords[1] := stWord2; stWords[2] := stWord3; stWords[3] := stWord4; stWords[4] := stWord5;
  stWords[5] := stWord6; stWords[6] := stWord7; stWords[7] := stWord8; stWords[8] := stWord9;
  shChars[0] := shChar1; shChars[1] := shChar2; shChars[2] := shChar3; shChars[3] := shChar4; shChars[4] := shChar5;
  shChars[5] := shChar6; shChars[6] := shChar7; shChars[7] := shChar8; shChars[8] := shChar9;
  // Read Chinese characters data from text file
  ReadCharacters(aCharacters);
  // Start English to Chinese test with default settings
  Randomize;
  iListTemp := 1; sChineseTemp := 'traditional'; iQuestionsTemp := 20;
  mTest1.Click;
end;

{ Menu item "Test > English to Chinese": Start English word to Chinese character/pinyin test }

procedure TfChinese.mTest1Click(Sender: TObject);

begin
  iTest := 1;
  NewTest(iTest, iListTemp, iQuestionsTemp, sChineseTemp, iList, iQuestions, sChinese, aCharacters, aList, aDone, stChars, stWords, shChars);
end;

{ Menu item "Test > Chinese to English": Start Chinese character/pinyin to English word test }

procedure TfChinese.mTest2Click(Sender: TObject);

begin
  iTest := 2;
  NewTest(iTest, iListTemp, iQuestionsTemp, sChineseTemp, iList, iQuestions, sChinese, aCharacters, aList, aDone, stChars, stWords, shChars);
end;

{ Menu item "Test > Lonogram to pinyin": Start Chinese character to pinyin test }

procedure TfChinese.mTest3Click(Sender: TObject);

begin
  iTest := 3;
  if mOptionsChinese3.Checked then begin
    MessageDlg('Invalid options', 'Characters reset from Pinyin to Traditional Chinese!', mtWarning, [mbOK], 0);
    mOptionsChinese1.Click;
  end;
  NewTest(iTest, iListTemp, iQuestionsTemp, sChineseTemp, iList, iQuestions, sChinese, aCharacters, aList, aDone, stChars, stWords, shChars);
end;

{ Menu item "Test > Pinyin to lonogram": Start pinyin to Chinese character test }

procedure TfChinese.mTest4Click(Sender: TObject);

begin
  iTest := 4;
  if mOptionsChinese3.Checked then begin
    MessageDlg('Invalid options', 'Characters reset from Pinyin to Traditional Chinese!', mtWarning, [mbOK], 0);
    mOptionsChinese1.Click;
  end;
  NewTest(iTest, iListTemp, iQuestionsTemp, sChineseTemp, iList, iQuestions, sChinese, aCharacters, aList, aDone, stChars, stWords, shChars);
end;

{ Menu item "Test > Exit": Exit application }

procedure TfChinese.mTestExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Chinese > Traditionnal": Select to use traditional Chinese characters }

procedure TfChinese.mOptionsChinese1Click(Sender: TObject);

begin
  mOptionsChinese1.Checked := True; mOptionsChinese2.Checked := False; mOptionsChinese3.Checked := False;
  sChineseTemp := 'traditional';
  if iListTemp in [4..6] then
    iListTemp -= 3;
end;

{ Menu item "Options > Chinese > Simplified": Select to use simplified Chinese characters }

procedure TfChinese.mOptionsChinese2Click(Sender: TObject);

begin
  mOptionsChinese1.Checked := False; mOptionsChinese2.Checked := True; mOptionsChinese3.Checked := False;
  sChineseTemp := 'simplified';
  if iListTemp in [1..3] then
    iListTemp += 3;
end;

{ Menu item "Options > Chinese > Pinyin": Select to use pinyin instead of Chinese characters lonograms }

procedure TfChinese.mOptionsChinese3Click(Sender: TObject);

begin
  mOptionsChinese1.Checked := False; mOptionsChinese2.Checked := False; mOptionsChinese3.Checked := True;
  sChineseTemp := 'pinyin';
end;

{ Menu item "Options > Character list > 20 characters": Select to use a list of 20 ("simple looking") characters }

procedure TfChinese.mOptionsChars20Click(Sender: TObject);

begin
  mOptionsChars20.Checked := True;  mOptionsChars50.Checked := False; mOptionsChars100.Checked := False; mOptionsCharsAll.Checked := False;
  mOptionsCharsN.Checked  := False; mOptionsCharsV.Checked  := False; mOptionsCharsA.Checked   := False;
  iQuestionsTemp := 20;
  if mOptionsChinese2.Checked then
    iListTemp := 4
  else
    iListTemp := 1;
end;

{ Menu item "Options > Character list > 50 characters": Select to use a list of 50 ("not to difficult looking") characters }

procedure TfChinese.mOptionsChars50Click(Sender: TObject);

begin
  mOptionsChars20.Checked := False; mOptionsChars50.Checked := True;  mOptionsChars100.Checked := False; mOptionsCharsAll.Checked := False;
  mOptionsCharsN.Checked  := False; mOptionsCharsV.Checked  := False; mOptionsCharsA.Checked   := False;
  iQuestionsTemp := 50;
  if mOptionsChinese2.Checked then
    iListTemp := 5
  else
    iListTemp := 2;
end;

{ Menu item "Options > Character list > 100 characters": Select to use a list of 100 characters (will include "difficult looking" ones) }

procedure TfChinese.mOptionsChars100Click(Sender: TObject);

begin
  mOptionsChars20.Checked := False; mOptionsChars50.Checked := False; mOptionsChars100.Checked := True; mOptionsCharsAll.Checked := False;
  mOptionsCharsN.Checked  := False; mOptionsCharsV.Checked  := False; mOptionsCharsA.Checked   := False;
  iQuestionsTemp := 100;
  if mOptionsChinese2.Checked then
    iListTemp := 6
  else
    iListTemp := 3;
end;

{ Menu item "Options > Character list > Full list": Select to use the complete character list (300 different characters) }

procedure TfChinese.mOptionsCharsAllClick(Sender: TObject);

begin
  mOptionsChars20.Checked := False; mOptionsChars50.Checked := False; mOptionsChars100.Checked := False; mOptionsCharsAll.Checked := True;
  mOptionsCharsN.Checked  := False; mOptionsCharsV.Checked  := False; mOptionsCharsA.Checked   := False;
  iQuestionsTemp := Length(aCharacters);
  iListTemp := -1;
end;

{ Menu item "Options > Character list > Adjectives": Select to use a character list with (40) adjectives }

procedure TfChinese.mOptionsCharsAClick(Sender: TObject);

begin
  mOptionsChars20.Checked := False; mOptionsChars50.Checked := False; mOptionsChars100.Checked := False; mOptionsCharsAll.Checked := False;
  mOptionsCharsN.Checked  := False; mOptionsCharsV.Checked  := False; mOptionsCharsA.Checked   := True;
  iQuestionsTemp := 40;
  iListTemp := 9;
end;

{ Menu item "Options > Character list > Nouns": Select to use a character list with (50) nouns }

procedure TfChinese.mOptionsCharsNClick(Sender: TObject);

begin
  mOptionsChars20.Checked := False; mOptionsChars50.Checked := False; mOptionsChars100.Checked := False; mOptionsCharsAll.Checked := False;
  mOptionsCharsN.Checked  := True;  mOptionsCharsV.Checked  := False; mOptionsCharsA.Checked   := False;
  iQuestionsTemp := 50;
  iListTemp := 7;
end;

{ Menu item "Options > Character list > Verbs": Select to use a character list with (50) verbs }

procedure TfChinese.mOptionsCharsVClick(Sender: TObject);

begin
  mOptionsChars20.Checked := False; mOptionsChars50.Checked := False; mOptionsChars100.Checked := False; mOptionsCharsAll.Checked := False;
  mOptionsCharsN.Checked  := False; mOptionsCharsV.Checked  := True; mOptionsCharsA.Checked    := False;
  iQuestionsTemp := 50;
  iListTemp := 8;
end;

{ Menu item "Help > About": Display application about }

procedure TfChinese.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Language trainer:' + LineEnding;
  S += 'Learn to recognize Chinese characters and their meaning.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, April-May 2020.';
  MessageDlg('About "Chinese"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next/Answer": Generate new question resp. check user answer }

procedure TfChinese.btQuestionClick(Sender: TObject);

var
  Chr, UX, LX, I, J: Integer;
  OK: Boolean;
  Chars: array[0..8] of Integer;

begin
  // Button "Start/Next": Generate new question
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Next') then begin
    if btQuestion.Caption = 'Start' then begin
      // Reset variables at start of test
      iQuestion := 0; iCorrect := 0;
      for I := 0 to 3 do
        fChinese.sgEval.Cells[1, I] := '';
    end;
    Inc(iQuestion);
    laQuestion.Caption := 'Character ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions);
    for I := 0 to 8 do
      shChars[I].Brush.Color := clYellow;
    // Get random character (form actual list)
    repeat
      LX := Random(Length(aList));
    until not aDone[LX];                                                       // character, that has not yet been asked
    aDone[LX] := True;                                                         // mark character as asked
    iChar := aList[LX];                                                        // list array elements are indexes for characters in all-characters array
    // Display character/word/pinyin given (at the left)
    case iTest of
        1: stWord0.Caption := aCharacters[iChar].English[Random(Length(aCharacters[iChar].English))];
      2,3: if sChinese = 'traditional' then
             stChar0.Caption := aCharacters[iChar].Traditional
           else if sChinese = 'simplified' then
             stChar0.Caption := aCharacters[iChar].Simplified
           else
             stWord0.Caption := aCharacters[iChar].Pinyin;
        4: stWord0.Caption := aCharacters[iChar].Pinyin;
    end;
    // Display character/word/pinyin choices (at the right)
    repeat
      OK := True;
      iXChar := Random(9);                                                     // random position of the correct answer
      for I := 0 to 8 do begin
        Chars[I] := -1;
        if I = iXChar then begin
          // Correct answer (as all-characters array index)
          Chr := iChar;
        end
        else begin
          // Random answer (as all-characters array index)
          repeat
            OK := True;
            Chr := aList[Random(Length(aList))];
            // Eliminate doubles
            if I > 0 then begin
              for J := 0 to I - 1 do begin
                if Chr = Chars[J] then
                  OK := False;
              end;
            end;
          until OK;
        end;
        Chars[I] := Chr;
        // Display choices
        case iTest of
            2: stWords[I].Caption := aCharacters[Chr].English[Random(Length(aCharacters[Chr].English))];
          1,4: if sChinese = 'traditional' then
                 stChars[I].Caption := aCharacters[Chr].Traditional
               else if sChinese = 'simplified' then
                 stChars[I].Caption := aCharacters[Chr].Simplified
               else
                 stWords[I].Caption := aCharacters[Chr].Pinyin;
            3: stWords[I].Caption := aCharacters[Chr].Pinyin;
        end;
      end;
      // As different characters may have the same English translation, be sure there are no doubles among the choices
      for I := 1 to 8 do begin
        for J := 0 to I - 1 do begin
          if ((iTest in [1, 4]) and(sChinese = 'pinyin')) or (iTest in [2, 3]) then begin
            if stWords[J].Caption = stWords[I].Caption then
              OK := False;
          end
          else begin
            if stChars[J].Caption = stChars[I].Caption then
              OK := False;
          end;
        end;
      end;
    until OK;
    btQuestion.Caption := 'Answer';
  end
  else begin
    OK := False; UX := -1;
    // Get user answer
    for I := 0 to 8 do begin
      if shChars[I].Brush.Color = clGold then begin
        // The golden colored shape corresponds to the actual user choice
        UX := I;
        // Check user answer
        // Note, that it's necessary to get the answer as string and compare with the given character's data
        // and not only check if the user selected the correct choices position (with answer corresponding
        // to the actual character/word given. In fact, other choices among those displayed may also be correct
        // (because of characters' several meanings)
        case iTest of
            1: begin
                 if sChinese = 'pinyin' then begin
                   if stWords[I].Caption = aCharacters[iChar].Pinyin then
                     OK := True;
                 end
                 else begin
                   if (sChinese = 'traditional') and (stChars[I].Caption = aCharacters[iChar].Traditional) then
                     OK := True
                   else if (sChinese = 'simplified') and (stChars[I].Caption = aCharacters[iChar].Simplified) then
                     OK := True
                 end;
               end;
          2,3: begin
                 for J := 0 to Length(aCharacters[iChar].English) - 1 do begin
                   if stWords[I].Caption = aCharacters[iChar].English[J] then
                     OK := True;
                 end;
               end;
            4: begin
                 if (sChinese = 'traditional') and (stChars[I].Caption = aCharacters[iChar].Traditional) then
                   OK := True
                 else if (sChinese = 'simplified') and (stChars[I].Caption = aCharacters[iChar].Simplified) then
                   OK := True
               end;
        end;
      end;
    end;
    // Answer evaluation
    if OK then begin
      // Correct answer
      shChars[UX].Brush.Color := clLime;                                       // shape colored in lime -> answer is correct
      Inc(iCorrect);
    end
    else begin
      // False answer
      if UX <> -1 then                                                         // this is no answer given at all
        shChars[UX].Brush.Color := clRed;                                      // shape colored in red  -> answer is false
      shChars[iXChar].Brush.Color := clAqua;                                   // shape colored in cyan -> showing, what correct answer is
    end;
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // If all characters of actual list are done, terminate the test...
    if iQuestion = iQuestions then begin
      MessageDlg('End of test', 'All characters have been done...', mtInformation, [mbOK], 0);
      btQuestion.Caption := 'Start'; btQuestion.Enabled := False;
    end
    // ...otherwise, continue with next question
    else
      btQuestion.Caption := 'Next';
  end;
end;

{ User click on large static text fields: Select or unselect answer choice }

procedure TfChinese.stChar1Click(Sender: TObject);

begin
  UserClick(0, shChars);
end;

procedure TfChinese.stChar2Click(Sender: TObject);

begin
  UserClick(1, shChars);
end;

procedure TfChinese.stChar3Click(Sender: TObject);

begin
  UserClick(2, shChars);
end;

procedure TfChinese.stChar4Click(Sender: TObject);

begin
  UserClick(3, shChars);
end;

procedure TfChinese.stChar5Click(Sender: TObject);

begin
  UserClick(4, shChars);
end;

procedure TfChinese.stChar6Click(Sender: TObject);

begin
  UserClick(5, shChars);
end;

procedure TfChinese.stChar7Click(Sender: TObject);

begin
  UserClick(6, shChars);
end;

procedure TfChinese.stChar8Click(Sender: TObject);

begin
  UserClick(7, shChars);
end;

procedure TfChinese.stChar9Click(Sender: TObject);

begin
  UserClick(8, shChars);
end;

{ User click on small static text fields: Select or unselect answer choice }

procedure TfChinese.stWord1Click(Sender: TObject);

begin
  UserClick(0, shChars);
end;

procedure TfChinese.stWord2Click(Sender: TObject);

begin
  UserClick(1, shChars);
end;

procedure TfChinese.stWord3Click(Sender: TObject);

begin
  UserClick(2, shChars);
end;

procedure TfChinese.stWord4Click(Sender: TObject);

begin
  UserClick(3, shChars);
end;

procedure TfChinese.stWord5Click(Sender: TObject);

begin
  UserClick(4, shChars);
end;

procedure TfChinese.stWord6Click(Sender: TObject);

begin
  UserClick(5, shChars);
end;

procedure TfChinese.stWord7Click(Sender: TObject);

begin
  UserClick(6, shChars);
end;

procedure TfChinese.stWord8Click(Sender: TObject);

begin
  UserClick(7, shChars);
end;

procedure TfChinese.stWord9Click(Sender: TObject);

begin
  UserClick(8, shChars);
end;

{ User click on shapes: Select or unselect answer choice }

procedure TfChinese.shChar1MouseDown(Sender: TObject);

begin
  UserClick(0, shChars);
end;

procedure TfChinese.shChar2MouseDown(Sender: TObject);

begin
  UserClick(1, shChars);
end;

procedure TfChinese.shChar3MouseDown(Sender: TObject);

begin
  UserClick(2, shChars);
end;

procedure TfChinese.shChar4MouseDown(Sender: TObject);

begin
  UserClick(3, shChars);
end;

procedure TfChinese.shChar5MouseDown(Sender: TObject);

begin
  UserClick(4, shChars);
end;

procedure TfChinese.shChar6MouseDown(Sender: TObject);

begin
  UserClick(5, shChars);
end;

procedure TfChinese.shChar7MouseDown(Sender: TObject);

begin
  UserClick(6, shChars);
end;

procedure TfChinese.shChar8MouseDown(Sender: TObject);

begin
  UserClick(7, shChars);
end;

procedure TfChinese.shChar9MouseDown(Sender: TObject);

begin
  UserClick(8, shChars);
end;

end.

