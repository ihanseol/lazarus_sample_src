{****************************************}
{* Main unit for WordSearch application *}
{****************************************}

unit words;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, help;

type
  TAllWords = array of string;
  TFields  = array[1..12, 1..12] of TShape;
  TLetters = array[1..12, 1..12] of TLabel;
  TWords = array[1..8] of string;
  TEdWords = array[1..8] of TEdit;
  {*********}
  { TfWords }
  {*********}
  TfWords = class(TForm)
    MenuItem1: TMenuItem;
    mSettingsShow: TMenuItem;
    mSettingsDiagRL: TMenuItem;
    mSettingsDiagLR: TMenuItem;
    mSettingsInvD: TMenuItem;
    mSettingsInvV: TMenuItem;
    mSettingsInvH: TMenuItem;
    mSettingsInv: TMenuItem;
    mMenu: TMainMenu;
    mPuzzle, mPuzzleExit: TMenuItem;
    mSettings, mSettingsDiag: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    shLetter1, shLetter10, shLetter100, shLetter101, shLetter102, shLetter103, shLetter104, shLetter105, shLetter106: TShape;
    shLetter107, shLetter108, shLetter109, shLetter11, shLetter110, shLetter111, shLetter112, shLetter113, shLetter114: TShape;
    shLetter115, shLetter116, shLetter117, shLetter118, shLetter119, shLetter12, shLetter120, shLetter121, shLetter122: TShape;
    shLetter123, shLetter124, shLetter125, shLetter126, shLetter127, shLetter128, shLetter129, shLetter13, shLetter130: TShape;
    shLetter131, shLetter132, shLetter133, shLetter134, shLetter135, shLetter136, shLetter137, shLetter138, shLetter139: TShape;
    shLetter14, shLetter140, shLetter141, shLetter142, shLetter143, shLetter144, shLetter15, shLetter16, shLetter17: TShape;
    shLetter18, shLetter19, shLetter2, shLetter20, shLetter21, shLetter22, shLetter23, shLetter24, shLetter25, shLetter26: TShape;
    shLetter27, shLetter28, shLetter29, shLetter3, shLetter30, shLetter31, shLetter32, shLetter33, shLetter34, shLetter35: TShape;
    shLetter36, shLetter37, shLetter38, shLetter39, shLetter4, shLetter40, shLetter41, shLetter42, shLetter43, shLetter44: TShape;
    shLetter45, shLetter46, shLetter47, shLetter48, shLetter49, shLetter5, shLetter50, shLetter51, shLetter52, shLetter53: TShape;
    shLetter54, shLetter55, shLetter56, shLetter57, shLetter58, shLetter59, shLetter6, shLetter60, shLetter61, shLetter62: TShape;
    shLetter63, shLetter64, shLetter65, shLetter66, shLetter67, shLetter68, shLetter69, shLetter7, shLetter70, shLetter71: TShape;
    shLetter72, shLetter73, shLetter74, shLetter75, shLetter76, shLetter77, shLetter78, shLetter79, shLetter8, shLetter80: TShape;
    shLetter81, shLetter82, shLetter83, shLetter84, shLetter85, shLetter86, shLetter87, shLetter88, shLetter89, shLetter9: TShape;
    shLetter90, shLetter91, shLetter92, shLetter93, shLetter94, shLetter95, shLetter96, shLetter97, shLetter98, shLetter99: TShape;
    Label1, Label10, Label100, Label101, Label102, Label103, Label104, Label105, Label106, Label107, Label108, Label109: TLabel;
    Label11, Label110, Label111, Label112, Label113, Label114, Label115, Label116, Label117, Label118, Label119, Label12: TLabel;
    Label120, Label121, Label122, Label123, Label124, Label125, Label126, Label127, Label128, Label129, Label13, Label130: TLabel;
    Label131, Label132, Label133, Label134, Label135, Label136, Label137, Label138, Label139, Label14, Label140, Label141: TLabel;
    Label142, Label143, Label144, Label145, Label15, Label16, Label17, Label18, Label19, Label2, Label20, Label21, Label22: TLabel;
    Label23, Label24, Label25, Label26, Label27, Label28, Label29, Label3, Label30, Label31, Label32, Label33, Label34: TLabel;
    Label35, Label36, Label37, Label38, Label39, Label4, Label40, Label41, Label42, Label43, Label44, Label45, Label46: TLabel;
    Label47, Label48, Label49, Label5, Label50, Label51, Label52, Label53, Label54, Label55, Label56, Label57, Label58: TLabel;
    Label59, Label6, Label60, Label61, Label62, Label63, Label64, Label65, Label66, Label67, Label68, Label69, Label7: TLabel;
    Label70, Label71, Label72, Label73, Label74, Label75, Label76, Label77, Label78, Label79, Label8, Label80, Label81: TLabel;
    Label82, Label83, Label84, Label85, Label86, Label87, Label88, Label89, Label9, Label90, Label91, Label92, Label93: TLabel;
    Label94, Label95, Label96, Label97, Label98, Label99: TLabel;
    StaticText2: TStaticText;
    edWord1, edWord2, edWord3, edWord4, edWord5, edWord6, edWord7, edWord8, edUserWord: TEdit;
    btStart: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure mSettingsDiagLRClick(Sender: TObject);
    procedure mSettingsDiagRLClick(Sender: TObject);
    procedure mSettingsInvDClick(Sender: TObject);
    procedure mSettingsInvHClick(Sender: TObject);
    procedure mSettingsInvVClick(Sender: TObject);
    procedure mSettingsShowClick(Sender: TObject);
    procedure shLetter100MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter101MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter102MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter103MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter104MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter105MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter106MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter107MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter108MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter109MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter10MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter110MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter111MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter112MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter113MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter114MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter115MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter116MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter117MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter118MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter119MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter11MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter120MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter121MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter122MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter123MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter124MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter125MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter126MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter127MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter128MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter129MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter12MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter130MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter131MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter132MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter133MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter134MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter135MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter136MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter137MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter138MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter139MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter13MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter140MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter141MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter142MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter143MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter144MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter14MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter15MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter16MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter17MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter18MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter19MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter1MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter20MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter21MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter22MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter23MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter24MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter25MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter26MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter27MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter28MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter29MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter2MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter30MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter31MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter32MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter33MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter34MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter35MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter36MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter37MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter38MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter39MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter3MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter40MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter41MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter42MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter43MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter44MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter45MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter46MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter47MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter48MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter49MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter4MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter50MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter51MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter52MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter53MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter54MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter55MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter56MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter57MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter58MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter59MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter5MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter60MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter61MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter62MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter63MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter64MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter65MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter66MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter67MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter68MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter69MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter6MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter70MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter71MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter72MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter73MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter74MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter75MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter76MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter77MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter78MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter79MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter7MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter80MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter81MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter82MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter83MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter84MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter85MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter86MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter87MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter88MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter89MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter8MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter90MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter91MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter92MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter93MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter94MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter95MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter96MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter97MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter98MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter99MouseDown(Sender: TObject; Button: TMouseButton);
    procedure shLetter9MouseDown(Sender: TObject; Button: TMouseButton);
  private
    iWords, iPosX, iPosY, iDirX, iDirY: Integer;
    sWord: string;
    aAllWords: TAllWords;
    aWords: TWords;
    shLetters: TFields;
    laLetters: TLetters;
    edWords: TEdWords;
  end;

var
  fWords: TfWords;

implementation

{$R *.lfm}

{ Reverse a string }

function Reverse(S0: string): string;

var
  I: Integer;
  S: string;

begin
  S := '';
  for I := Length(S0) downto 1 do
    S += Copy(S0, I, 1);
  Result := S;
end;

{ Read words from text file }

procedure ReadWords(out Words: TAllWords);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  SetLength(Words, 0); N := 0;
  Assign(InFile, 'words.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (Length(Line) <= 10) then begin                        // max word length fixed to 10
      Inc(N); SetLength(Words, N);
      Words[N - 1] := Trim(Line);
    end;
  end;
  Close(InFile);
end;

{ Create a new puzzle }

procedure NewPuzzle(var AllWords: TAllWords; out DisplayWords: TWords; var WFields: TEdWords; var Fields: TFields; var FieldLetters: TLetters);

var
  Direction, Invert, X, Y, XD, YD, XO, YO, Count, I, J: Integer;
  SWord: string;
  Found, OK: Boolean;
  Letters: array[1..12, 1..12] of Char;
  Words: TWords;

// Note that the code of this proceddure is partially a mess! Sorry...

begin
  // Clear letter grid
  for I := 1 to 12 do begin
    for J := 1 to 12 do begin
      Fields[I, J].Brush.Color := clDefault;
      Fields[I, J].Brush.Style := bsSolid;
      FieldLetters[I, J].Caption := ' ';
      FieldLetters[I, J].Font.Style := [];
    end;
  end;
  // Choose 8 random words and place them onto the letter grid
  repeat
    Found := True;
    // Reset the words and letters arrays
    for I := 1 to 8 do
      Words[I] := '';
    for I := 1 to 12 do begin
      for J := 1 to 12 do begin
        Letters[I, J] := ' ';
      end;
    end;
    // Choose two (different) random words
    repeat
      Words[1] := AllWords[Random(Length(AllWords))]; Words[2] := AllWords[Random(Length(AllWords))];
    until Words[1] <> Words[2];
    // Choose a random direction (among those selected) for these first 2 words
    repeat
      OK := True;
      Direction := Random(4);
      if (Direction = 2) and (not fWords.mSettingsDiagLR.Checked) then
        OK := False;
      if (Direction = 3) and (not fWords.mSettingsDiagRL.Checked) then
        OK := False;
    until OK;
    // Place these words onto the grid
    for I := 1 to 2 do begin
      repeat
        OK := True;
        // Get a random word starting position; set X- and Y-value of word direction
        case Direction of
          0: begin
               X := Random(12 - Length(Words[I]) + 1) + 1; Y := Random(12) + 1; XD := 1; YD := 0;
             end;
          1: begin
               X := Random(12) + 1; Y := Random(12 - Length(Words[I]) + 1) + 1; XD := 0; YD := 1;
             end;
          2: begin
               X := Random(12 - Length(Words[I]) + 1) + 1; Y := Random(12 - Length(Words[I]) + 1) + 1; XD := 1; YD := 1;
             end;
          3: begin
               X := Random(12 - Length(Words[I]) + 1) + Length(Words[I]); Y := Random(12 - Length(Words[I]) + 1) + 1; XD := -1; YD := 1;
             end;
        end;
        XO := X; YO := Y;
        // Place the letters of actual word
        for J := 1 to Length(Words[I]) do begin
          if I = 1 then begin
            // First word: Just place it
            Letters[Y, X] := Words[I][J];
            X += XD; Y += YD;
          end
          else begin
            // Second word: Check if place for letter is free
            if Letters[Y, X] <> ' ' then
              OK := False;
            X += XD; Y += YD;
           end;
        end;
        // Place letters of second word only if all places are free
        if (I = 2) and OK then begin
          X := XO; Y := YO;
          for J := 1 to Length(Words[I]) do begin
            Letters[Y, X] := Words[I][J];
            X += XD; Y += YD;
          end;
        end;
        // If the second word could not be placed, repeat with new parameters
      until OK;
    end;
    for I := 1 to 2 do
      DisplayWords[I] := Words[I];
    // Place the remaining 6 words
    for I := 3 to 8 do begin
      // Choose a random direction (among those selected)
      repeat
        OK := True;
        Direction := Random(4);
        if (Direction = 2) and (not fWords.mSettingsDiagLR.Checked) then
          OK := False;
        if (Direction = 3) and (not fWords.mSettingsDiagRL.Checked) then
          OK := False;
      until OK;
      // Use a counter value to break word positioning trials if no possibility found (otherwise may get infinite loop)
      Count := 0;
      // Try positionning the words until all done or counter variable exceeds limit
      repeat
        Inc(Count);
        // Choose a random word (different from those already chosen)
        repeat
          OK := True;
          Words[I] := AllWords[Random(Length(AllWords))];
          for J := 1 to I - 1 do begin
            if Words[I] = DisplayWords[J] then
              OK := False;
          end;
        until OK;
        DisplayWords[I] := Words[I];
        // Randomly reverse the word (if corr. option selected)
        Invert := Random(2);
        if Invert = 1 then begin
          case Direction of
            0: if fWords.mSettingsInvH.Checked then
                 Words[I] := Reverse(Words[I]);                                  // inverted word: right-left display
            1: if fWords.mSettingsInvV.Checked then
                 Words[I] := Reverse(Words[I]);                                  // inverted word: down-up display
            else if fWords.mSettingsInvD.Checked then
                 Words[I] := Reverse(Words[I]);                                  // inverted word: down-up display
          end;
        end;
        // Choose random starting positions; set X- and Y-value of word direction
        case Direction of
          0: begin
               X := Random(12 - Length(Words[I]) + 1) + 1; Y := Random(12) + 1; XD := 1; YD := 0;
             end;
          1: begin
               X := Random(12) + 1; Y := Random(12 - Length(Words[I]) + 1) + 1; XD := 0; YD := 1;
             end;
          2: begin
               X := Random(12 - Length(Words[I]) + 1) + 1; Y := Random(12 - Length(Words[I]) + 1) + 1; XD := 1; YD := 1;
             end;
          3: begin
               X := Random(12 - Length(Words[I]) + 1) + Length(Words[I]); Y := Random(12 - Length(Words[I]) + 1) + 1; XD := -1; YD := 1;
             end;
        end;
        XO := X; YO := Y;
        // Try placing the word onto the letter grid
        for J := 1 to Length(Words[I]) do begin
          // If position is occupied, it has to be by same letter as the actual word's one at this position
          if (Letters[Y, X] <> ' ') and (Letters[Y, X] <> Words[I][J]) then
            OK := False
          else begin
            // Avoid word overlapping
            if (X + XD >= 1) and ((X + XD <= 12)) and (Y + YD >= 1) and ((Y + YD <= 12)) then begin
              if (Letters[Y, X] = Words[I][J]) and (Letters[Y + YD, X + XD] <> ' ') then
                OK := False;
            end;
          end;
          X += XD; Y += YD;
        end;
        // If conditions as described above are met, place the word
        if OK then begin
          X := XO; Y := YO;
          for J := 1 to Length(Words[I]) do begin
            Letters[Y, X] := Words[I][J];
            X += XD; Y += YD;
          end;
        end;
        // If the word couldn't be placed, try with a different one (but quit the loop if "security" variable exceeds limit)
      until OK or (Count > 1000);
      if Count > 1000 then
        Found := False;
    end;
    // Repeat the whole until all 8 words have been placed
  until Found;
  // Sort the words in the list
  for I := 2 to 8 do begin
    for J := 1 to I - 1 do begin
      if DisplayWords[J] > DisplayWords[I] then begin
        SWord := DisplayWords[I]; DisplayWords[I] := DisplayWords[J]; DisplayWords[J] := SWord;
      end;
    end;
  end;
  // Display the word list
  for I := 1 to 8 do begin
    WFields[I].Text := DisplayWords[I];
    WFields[I].Font.Style := [fsBold];
  end;
  // Fill the empty grid places with random letters
  for I := 1 to 12 do begin
    for J := 1 to 12 do begin
      if Letters[I, J] <> ' ' then begin
        // Show the words (if this option is selected)
        if fWords.mSettingsShow.Checked then
          Fields[I, J].Brush.Color := clWhite;
      end
      else
        Letters[I, J] := Chr(Random(26) + 65);
      FieldLetters[I, J].Caption := Letters[I, J];
    end;
  end;
end;

{ Clear the actual word (i.e. reset font style from bold to "normal") }

procedure ResetWord(var UWord: string; OX, OY, DX, DY: Integer; var FieldLetters: TLetters);

var
  X, Y, I: Integer;

begin
  if Length(UWord) > 0 then begin
    FieldLetters[OY, OX].Font.Style := [];
    if Length(UWord) > 1 then begin
      X := OX; Y := OY;
      for I := 2 to Length(UWord) do begin
        X -= DX; Y -= DY;
        FieldLetters[Y, X].Font.Style := [];
      end;
    end;
    // Also reset the actual word string variable and the corr. edit field
    UWord := ''; fWords.edUserWord.Text := '';
  end;
end;

{ Mark the actual word (i.e. highlight it by setting a color for the underlying shape) }

procedure MarkWord(UWord: string; NWords, OX, OY, DX, DY: Integer; var Fields: TFields);

const
  clOrange = $00A5FF;
  Colors: array[1..8] of TColor = (
    clRed, clBlue, clLime, clYellow, clAqua, clFuchsia, clOrange, clSkyBlue
  );

var
  X, Y, I: Integer;

begin
  if (Length(UWord) > 0) and (NWords >= 1) and (NWords <= 8) then begin
    if (Fields[OY, OX].Brush.Color <> clDefault) and (Fields[OY, OX].Brush.Color <> clWhite) then begin
      // Word croos: Use diagagonal-cross brush style (instead of solid color)
      Fields[OY, OX].Brush.Style := bsDiagCross;
    end;
    Fields[OY, OX].Brush.Color := Colors[NWords];
    if Length(UWord) > 1 then begin
      X := OX; Y := OY;
      for I := 2 to Length(UWord) do begin
        X -= DX; Y -= DY;
        if (Fields[Y, X].Brush.Color <> clDefault) and (Fields[Y, X].Brush.Color <> clWhite) then begin
          // Word croos: Use diagagonal-cross brush style (instead of solid color)
          Fields[Y, X].Brush.Style := bsDiagCross;
        end;
        Fields[Y, X].Brush.Color := Colors[NWords];                            // different color for each word
      end;
    end;
  end;
end;

{ Field clicked by user: Select the corr. letter; if right-click: select whole word }

procedure FieldSelect(N: Integer; Button: TMouseButton; var Words: TWords; var UWord: string; var NWords, OX, OY, DX, DY: Integer;
  var WFields: TEdWords; var Fields: TFields; var FieldLetters: TLetters);

var
  X, Y, FI, I: Integer;
  Mess: string;

begin
  // Only do sth if still some word(s) remaining to be found
  if NWords < 8 then begin
    // Determine row/column coordinates
    Y := (N - 1) div 12 + 1; X := N - (Y - 1) * 12;
    // Only consider the click if it is on field with letter not already part of actual word
    if FieldLetters[Y, X].Font.Style <> [fsbold] then begin
      Mess := '';
      if Length(UWord) <= 1 then begin
        if Length(UWord) = 1 then begin
          // If this is 2nd letter: Set word direction
          DX := X - OX; DY := Y - OY;
        end;
      end
      else if Length(UWord) = 10 then
        // If this is 11th letter, display error message
        Mess := 'Die max. Wortlänge von 10 Buchstaben ist bereits erreicht!'
      else begin
        // If this is 3rd .. 10th letter, check if letter clicked is well in word direction
        if (X - OX <> DX) or (Y - OY <> DY) then
          Mess := 'Das geklickte Feld gehört nicht zu den aktuellen Wortfeldern!';
      end;
      if Mess <> '' then
        MessageDlg('Fehler', Mess, mtError, [mbOK], 0)
      // If a correct letter was selected, add it to the actual word
      else begin
        UWord += FieldLetters[Y, X].Caption;
        fWords.edUserWord.Text := UWord;
        FieldLetters[Y, X].Font.Style := [fsbold];                             // use bold font style for word-letters selected
        OX := X; OY := Y;                                                      // save actual row/column values (needed for word reset and mark)
      end;
    end;
    // Right button click within the actual word selection: Select word and check if is in list
    if (Button = mbRight) and (FieldLetters[Y, X].Font.Style = [fsbold]) then begin
      FI := -1;
      for I := 1 to 8 do begin
        if UWord = Words[I] then
          FI := I;
      end;
      // Selected word is part of list words
      if FI > 0 then begin
        Words[FI] := '';
        WFields[FI].Font.Style := [fsStrikeOut];                               // strike through the word in the list
        Inc(NWords);                                                           // increment found words counter
        MarkWord(UWord, NWords, OX, OY, DX, DY, Fields);                       // highlight the word in the letter grid
        if NWords = 8 then begin
          // All words have been found
          MessageDlg('Fertig', 'Alle Wörter gefunden: Rätsel gelöst!', mtWarning, [mbOK], 0);
        end;
      end
      // Selected word isn't part of the list
      else
        MessageDlg('Fehler', 'Dieses Wort gehört nicht zu den aktuell gesuchten!', mtWarning, [mbOK], 0);
      // Reset the (found) word's font style to "normal"
      ResetWord(UWord, OX, OY, DX, DY, FieldLetters);
    end;
  end;
end;

{*********}
{ TfWords }
{*********}

{ Application start: Initialisation }

procedure TfWords.FormCreate(Sender: TObject);

begin
  // Create array with the grid shapes
  shLetters[1, 1] := shLetter1; shLetters[1, 2] := shLetter2; shLetters[1, 3] := shLetter3; shLetters[1, 4] := shLetter4; shLetters[1, 5] := shLetter5; shLetters[1, 6] := shLetter6;
  shLetters[1, 7] := shLetter7; shLetters[1, 8] := shLetter8; shLetters[1, 9] := shLetter9; shLetters[1, 10] := shLetter10; shLetters[1, 11] := shLetter11; shLetters[1, 12] := shLetter12;
  shLetters[2, 1] := shLetter13; shLetters[2, 2] := shLetter14; shLetters[2, 3] := shLetter15; shLetters[2, 4] := shLetter16; shLetters[2, 5] := shLetter17; shLetters[2, 6] := shLetter18;
  shLetters[2, 7] := shLetter19; shLetters[2, 8] := shLetter20; shLetters[2, 9] := shLetter21; shLetters[2, 10] := shLetter22; shLetters[2, 11] := shLetter23; shLetters[2, 12] := shLetter24;
  shLetters[3, 1] := shLetter25; shLetters[3, 2] := shLetter26; shLetters[3, 3] := shLetter27; shLetters[3, 4] := shLetter28; shLetters[3, 5] := shLetter29; shLetters[3, 6] := shLetter30;
  shLetters[3, 7] := shLetter31; shLetters[3, 8] := shLetter32; shLetters[3, 9] := shLetter33; shLetters[3, 10] := shLetter34; shLetters[3, 11] := shLetter35; shLetters[3, 12] := shLetter36;
  shLetters[4, 1] := shLetter37; shLetters[4, 2] := shLetter38; shLetters[4, 3] := shLetter39; shLetters[4, 4] := shLetter40; shLetters[4, 5] := shLetter41; shLetters[4, 6] := shLetter42;
  shLetters[4, 7] := shLetter43; shLetters[4, 8] := shLetter44; shLetters[4, 9] := shLetter45; shLetters[4, 10] := shLetter46; shLetters[4, 11] := shLetter47; shLetters[4, 12] := shLetter48;
  shLetters[5, 1] := shLetter49; shLetters[5, 2] := shLetter50; shLetters[5, 3] := shLetter51; shLetters[5, 4] := shLetter52; shLetters[5, 5] := shLetter53; shLetters[5, 6] := shLetter54;
  shLetters[5, 7] := shLetter55; shLetters[5, 8] := shLetter56; shLetters[5, 9] := shLetter57; shLetters[5, 10] := shLetter58; shLetters[5, 11] := shLetter59; shLetters[5, 12] := shLetter60;
  shLetters[6, 1] := shLetter61; shLetters[6, 2] := shLetter62; shLetters[6, 3] := shLetter63; shLetters[6, 4] := shLetter64; shLetters[6, 5] := shLetter65; shLetters[6, 6] := shLetter66;
  shLetters[6, 7] := shLetter67; shLetters[6, 8] := shLetter68; shLetters[6, 9] := shLetter69; shLetters[6, 10] := shLetter70; shLetters[6, 11] := shLetter71; shLetters[6, 12] := shLetter72;
  shLetters[7, 1] := shLetter73; shLetters[7, 2] := shLetter74; shLetters[7, 3] := shLetter75; shLetters[7, 4] := shLetter76; shLetters[7, 5] := shLetter77; shLetters[7, 6] := shLetter78;
  shLetters[7, 7] := shLetter79; shLetters[7, 8] := shLetter80; shLetters[7, 9] := shLetter81; shLetters[7, 10] := shLetter82; shLetters[7, 11] := shLetter83; shLetters[7, 12] := shLetter84;
  shLetters[8, 1] := shLetter85; shLetters[8, 2] := shLetter86; shLetters[8, 3] := shLetter87; shLetters[8, 4] := shLetter88; shLetters[8, 5] := shLetter89; shLetters[8, 6] := shLetter90;
  shLetters[8, 7] := shLetter91; shLetters[8, 8] := shLetter92; shLetters[8, 9] := shLetter93; shLetters[8, 10] := shLetter94; shLetters[8, 11] := shLetter95; shLetters[8, 12] := shLetter96;
  shLetters[9, 1] := shLetter97; shLetters[9, 2] := shLetter98; shLetters[9, 3] := shLetter99; shLetters[9, 4] := shLetter100; shLetters[9, 5] := shLetter101; shLetters[9, 6] := shLetter102;
  shLetters[9, 7] := shLetter103; shLetters[9, 8] := shLetter104; shLetters[9, 9] := shLetter105; shLetters[9, 10] := shLetter106; shLetters[9, 11] := shLetter107; shLetters[9, 12] := shLetter108;
  shLetters[10, 1] := shLetter109; shLetters[10, 2] := shLetter110; shLetters[10, 3] := shLetter111; shLetters[10, 4] := shLetter112; shLetters[10, 5] := shLetter113; shLetters[10, 6] := shLetter114;
  shLetters[10, 7] := shLetter115; shLetters[10, 8] := shLetter116; shLetters[10, 9] := shLetter117; shLetters[10, 10] := shLetter118; shLetters[10, 11] := shLetter119; shLetters[10, 12] := shLetter120;
  shLetters[11, 1] := shLetter121; shLetters[11, 2] := shLetter122; shLetters[11, 3] := shLetter123; shLetters[11, 4] := shLetter124; shLetters[11, 5] := shLetter125; shLetters[11, 6] := shLetter126;
  shLetters[11, 7] := shLetter127; shLetters[11, 8] := shLetter128; shLetters[11, 9] := shLetter129; shLetters[11, 10] := shLetter130; shLetters[11, 11] := shLetter131; shLetters[11, 12] := shLetter132;
  shLetters[12, 1] := shLetter133; shLetters[12, 2] := shLetter134; shLetters[12, 3] := shLetter135; shLetters[12, 4] := shLetter136; shLetters[12, 5] := shLetter137; shLetters[12, 6] := shLetter138;
  shLetters[12, 7] := shLetter139; shLetters[12, 8] := shLetter140; shLetters[12, 9] := shLetter141; shLetters[12, 10] := shLetter142; shLetters[12, 11] := shLetter143; shLetters[12, 12] := shLetter144;
  // Create array with the grid letter labels
  laLetters[1, 1] := Label1; laLetters[1, 2] := Label2; laLetters[1, 3] := Label3; laLetters[1, 4] := Label4; laLetters[1, 5] := Label5; laLetters[1, 6] := Label6;
  laLetters[1, 7] := Label7; laLetters[1, 8] := Label8; laLetters[1, 9] := Label9; laLetters[1, 10] := Label10; laLetters[1, 11] := Label11; laLetters[1, 12] := Label12;
  laLetters[2, 1] := Label13; laLetters[2, 2] := Label14; laLetters[2, 3] := Label15; laLetters[2, 4] := Label16; laLetters[2, 5] := Label17; laLetters[2, 6] := Label18;
  laLetters[2, 7] := Label19; laLetters[2, 8] := Label20; laLetters[2, 9] := Label21; laLetters[2, 10] := Label22; laLetters[2, 11] := Label23; laLetters[2, 12] := Label24;
  laLetters[3, 1] := Label25; laLetters[3, 2] := Label26; laLetters[3, 3] := Label27; laLetters[3, 4] := Label28; laLetters[3, 5] := Label29; laLetters[3, 6] := Label30;
  laLetters[3, 7] := Label31; laLetters[3, 8] := Label32; laLetters[3, 9] := Label33; laLetters[3, 10] := Label34; laLetters[3, 11] := Label35; laLetters[3, 12] := Label36;
  laLetters[4, 1] := Label37; laLetters[4, 2] := Label38; laLetters[4, 3] := Label39; laLetters[4, 4] := Label40; laLetters[4, 5] := Label41; laLetters[4, 6] := Label42;
  laLetters[4, 7] := Label43; laLetters[4, 8] := Label44; laLetters[4, 9] := Label45; laLetters[4, 10] := Label46; laLetters[4, 11] := Label47; laLetters[4, 12] := Label48;
  laLetters[5, 1] := Label49; laLetters[5, 2] := Label50; laLetters[5, 3] := Label51; laLetters[5, 4] := Label52; laLetters[5, 5] := Label53; laLetters[5, 6] := Label54;
  laLetters[5, 7] := Label55; laLetters[5, 8] := Label56; laLetters[5, 9] := Label57; laLetters[5, 10] := Label58; laLetters[5, 11] := Label59; laLetters[5, 12] := Label60;
  laLetters[6, 1] := Label61; laLetters[6, 2] := Label62; laLetters[6, 3] := Label63; laLetters[6, 4] := Label64; laLetters[6, 5] := Label65; laLetters[6, 6] := Label66;
  laLetters[6, 7] := Label67; laLetters[6, 8] := Label68; laLetters[6, 9] := Label69; laLetters[6, 10] := Label70; laLetters[6, 11] := Label71; laLetters[6, 12] := Label72;
  laLetters[7, 1] := Label73; laLetters[7, 2] := Label74; laLetters[7, 3] := Label75; laLetters[7, 4] := Label76; laLetters[7, 5] := Label77; laLetters[7, 6] := Label78;
  laLetters[7, 7] := Label79; laLetters[7, 8] := Label80; laLetters[7, 9] := Label81; laLetters[7, 10] := Label82; laLetters[7, 11] := Label83; laLetters[7, 12] := Label84;
  laLetters[8, 1] := Label85; laLetters[8, 2] := Label86; laLetters[8, 3] := Label87; laLetters[8, 4] := Label88; laLetters[8, 5] := Label89; laLetters[8, 6] := Label90;
  laLetters[8, 7] := Label91; laLetters[8, 8] := Label92; laLetters[8, 9] := Label93; laLetters[8, 10] := Label94; laLetters[8, 11] := Label95; laLetters[8, 12] := Label96;
  laLetters[9, 1] := Label97; laLetters[9, 2] := Label98; laLetters[9, 3] := Label99; laLetters[9, 4] := Label100; laLetters[9, 5] := Label101; laLetters[9, 6] := Label102;
  laLetters[9, 7] := Label103; laLetters[9, 8] := Label104; laLetters[9, 9] := Label105; laLetters[9, 10] := Label106; laLetters[9, 11] := Label107; laLetters[9, 12] := Label108;
  laLetters[10, 1] := Label109; laLetters[10, 2] := Label110; laLetters[10, 3] := Label111; laLetters[10, 4] := Label112; laLetters[10, 5] := Label113; laLetters[10, 6] := Label114;
  laLetters[10, 7] := Label115; laLetters[10, 8] := Label116; laLetters[10, 9] := Label117; laLetters[10, 10] := Label118; laLetters[10, 11] := Label119; laLetters[10, 12] := Label120;
  laLetters[11, 1] := Label121; laLetters[11, 2] := Label122; laLetters[11, 3] := Label123; laLetters[11, 4] := Label124; laLetters[11, 5] := Label125; laLetters[11, 6] := Label126;
  laLetters[11, 7] := Label127; laLetters[11, 8] := Label128; laLetters[11, 9] := Label129; laLetters[11, 10] := Label130; laLetters[11, 11] := Label131; laLetters[11, 12] := Label132;
  laLetters[12, 1] := Label133; laLetters[12, 2] := Label134; laLetters[12, 3] := Label135; laLetters[12, 4] := Label136; laLetters[12, 5] := Label137; laLetters[12, 6] := Label138;
  laLetters[12, 7] := Label139; laLetters[12, 8] := Label140; laLetters[12, 9] := Label141; laLetters[12, 10] := Label142; laLetters[12, 11] := Label143; laLetters[12, 12] := Label144;
  // Create array with the 8 words edit fields
  edWords[1] := edWord1; edWords[2] := edWord2; edWords[3] := edWord3; edWords[4] := edWord4;
  edWords[5] := edWord5; edWords[6] := edWord6; edWords[7] := edWord7; edWords[8] := edWord8;
  // Read the word list
  SetLength(aAllWords, 0); ReadWords(aAllWords);
  // Start random number generator
  Randomize;
  // Push the "Neu" button (generate new puzzle)
  btStart.Click;
end;

{ Menu item "Rätsel > Verlassen": Exit the application }

procedure TfWords.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Diagonale Wörter erlauben > Von links nach rechts": Allow or not left-right diagonal words }

procedure TfWords.mSettingsDiagLRClick(Sender: TObject);

begin
  if mSettingsDiagLR.Checked then
    mSettingsDiagLR.Checked := False
  else
    mSettingsDiagLR.Checked := True;
  if mSettingsDiagLR.Checked or mSettingsDiagRL.Checked then
    mSettingsInvD.Enabled := True
  else begin
    mSettingsInvD.Checked := False;
    mSettingsInvD.Enabled := False;
  end;
end;

{ Menu item "Einstellungen > Diagonale Wörter erlauben > Von rechts nach links": Allow or not right-left diagonal words }

procedure TfWords.mSettingsDiagRLClick(Sender: TObject);

begin
  if mSettingsDiagRL.Checked then
    mSettingsDiagRL.Checked := False
  else
    mSettingsDiagRL.Checked := True;
  if mSettingsDiagLR.Checked or mSettingsDiagRL.Checked then
    mSettingsInvD.Enabled := True
  else begin
    mSettingsInvD.Checked := False;
    mSettingsInvD.Enabled := False;
  end;
end;

{ Menu item "Einstellungen > Invertierte Wörter erlauben > Horizontal": Allow or not inverted horizontal words }

procedure TfWords.mSettingsInvHClick(Sender: TObject);

begin
  if mSettingsInvH.Checked then
    mSettingsInvH.Checked := False
  else
    mSettingsInvH.Checked := True;
end;

{ Menu item "Einstellungen > Invertierte Wörter erlauben > Vertikal": Allow or not inverted vertical words }

procedure TfWords.mSettingsInvVClick(Sender: TObject);

begin
  if mSettingsInvV.Checked then
    mSettingsInvV.Checked := False
  else
    mSettingsInvV.Checked := True;
end;

{ Menu item "Einstellungen > Invertierte Wörter erlauben > Diagonal": Allow or not inverted diagonal words }

procedure TfWords.mSettingsInvDClick(Sender: TObject);

begin
  if mSettingsInvD.Checked then
    mSettingsInvD.Checked := False
  else
    mSettingsInvD.Checked := True;
end;

{ Menu item "Einstellungen > Wörter anzeigen": Show words (test option...) }

procedure TfWords.mSettingsShowClick(Sender: TObject);

begin
  if mSettingsShow.Checked then
    mSettingsShow.Checked := False
  else
    mSettingsShow.Checked := True;
end;

{ Menu "Hilfe > Hilfe": Display program help }

procedure TfWords.mHelpHelpClick(Sender: TObject);

begin
  if not fHelp.Visible then
    fHelp.Show
  else
    fHelp.Close;
end;

{ Menu "Hilfe > Über": Display program about }

procedure TfWords.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Wörterrätsel: Wortsuche im Buchstabensalat.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, April-September 2019.';
  MessageDlg('Über "WordSearch"', S, mtInformation, [mbOK], 0);
end;

{ Button "Neu": Generate a new puzzle }

procedure TfWords.btStartClick(Sender: TObject);

begin
  sWord := ''; iWords := 0;
  NewPuzzle(aAllWords, aWords, edWords, shLetters, laLetters);
end;

{ Button "Löschen": Reset actual word }

procedure TfWords.btClearClick(Sender: TObject);

begin
  ResetWord(sWord, iPosX, iPosY, iDirX, iDirY, laLetters);
end;

{ MouseDown events (left/right-click) on grid squares: Select letter/word }

procedure TfWords.shLetter100MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(100, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter101MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(101, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter102MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(102, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter103MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(103, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter104MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(104, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter105MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(105, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter106MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(106, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter107MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(107, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter108MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(108, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter109MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(109, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter10MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(10, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter110MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(110, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter111MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(111, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter112MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(112, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter113MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(113, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter114MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(114, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter115MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(115, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter116MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(116, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter117MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(117, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter118MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(118, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter119MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(119, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter11MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(11, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter120MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(120, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter121MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(121, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter122MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(122, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter123MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(123, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter124MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(124, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter125MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(125, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter126MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(126, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter127MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(127, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter128MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(128, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter129MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(129, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter12MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(12, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter130MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(130, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter131MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(131, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter132MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(132, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter133MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(133, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter134MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(134, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter135MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(135, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter136MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(136, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter137MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(137, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter138MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(138, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter139MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(139, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter13MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(13, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter140MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(140, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter141MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(141, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter142MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(142, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter143MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(143, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter144MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(144, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter14MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(14, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter15MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(15, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter16MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(16, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter17MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(17, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter18MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(18, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter19MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(19, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter1MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(1, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter20MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(20, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter21MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(21, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter22MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(22, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter23MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(23, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter24MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(24, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter25MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(25, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter26MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(26, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter27MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(27, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter28MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(28, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter29MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(29, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter2MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(2, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter30MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(30, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter31MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(31, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter32MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(32, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter33MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(33, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter34MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(34, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter35MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(35, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter36MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(36, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter37MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(37, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter38MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(38, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter39MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(39, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter3MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(3, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter40MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(40, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter41MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(41, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter42MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(42, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter43MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(43, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter44MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(44, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter45MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(45, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter46MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(46, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter47MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(47, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter48MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(48, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter49MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(49, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter4MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(4, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter50MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(50, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter51MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(51, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter52MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(52, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter53MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(53, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter54MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(54, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter55MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(55, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter56MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(56, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter57MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(57, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter58MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(58, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter59MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(59, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter5MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(5, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter60MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(60, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter61MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(61, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter62MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(62, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter63MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(63, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter64MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(64, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter65MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(65, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter66MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(66, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter67MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(67, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter68MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(68, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter69MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(69, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter6MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(6, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter70MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(70, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter71MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(71, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter72MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(72, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter73MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(73, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter74MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(74, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter75MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(75, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter76MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(76, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter77MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(77, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter78MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(78, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter79MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(79, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter7MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(7, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter80MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(80, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter81MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(81, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter82MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(82, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter83MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(83, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter84MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(84, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter85MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(85, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter86MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(86, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter87MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(87, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter88MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(88, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter89MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(89, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter8MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(8, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter90MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(90, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter91MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(91, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter92MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(92, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter93MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(93, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter94MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(94, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter95MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(95, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter96MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(96, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter97MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(97, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter98MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(98, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter99MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(99, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

procedure TfWords.shLetter9MouseDown(Sender: TObject; Button: TMouseButton);

begin
  FieldSelect(9, Button, aWords, sWord, iWords, iPosX, iPosY, iDirX, iDirY, edWords, shLetters, laLetters);
end;

end.

