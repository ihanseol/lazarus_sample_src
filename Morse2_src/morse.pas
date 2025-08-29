{************************************}
{* Main unit for Morse2 application *}
{************************************}

unit morse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, Grids, StdCtrls, translation, test, help, common;

type
  {**********}
  { TfMorse2 }
  {**********}
  TfMorse2 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileText, mFileMorse, mFileTest1, mFileTest2, mFileExit: TMenuItem;
    mSettings, mSettingsLDash, mSettingsLWSep: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3: TLabel;
    sgLetters1, sgLetters2, sgNumbers, sgMarks: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure mFileTextClick(Sender: TObject);
    procedure mFileMorseClick(Sender: TObject);
    procedure mFileTest1Click(Sender: TObject);
    procedure mFileTest2Click(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsLDashClick(Sender: TObject);
    procedure mSettingsLWSepClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
  private
    bDisplayed, bLongerDash, bLongerWordSep: Boolean;
    aMorseAlphabet: TMorseAlphabet;
  end;

var
  fMorse2: TfMorse2;

implementation

{$R *.lfm}

{ Read morse alphabet from file and fill it into an array }

procedure ReadMorseAlphabet(out MorseAlphabet: TMorseAlphabet);

// The TMorseAlphabet array contains the morse character (or '?' if does not exist) for characters with Ascii codes 32-90

var
  I: Integer;
  Ch: Char;
  Line: string;
  InFile: Text;

begin
  for I := 32 to 90 do
    MorseAlphabet[I] := '?';
  Assign(InFile, 'morse.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Ch := LeftStr(Line, 1)[1];
      MorseAlphabet[Ord(Ch)] := RightStr(Line, Length(Line) - 2);
    end;
  end;
  Close(InFile);
end;

{ Display the morse alphabet (fill the string-grids) }

procedure DisplayMorseAlphabet(var MorseAlphabet: TMorseAlphabet);

var
  I, J: Integer;

begin
  for I := 32 to 90 do begin
    if (I <= 47) or ((I >= 58) and (I <= 64)) then begin
      // Punctuation marks
      if I = 32 then
        J := 0;
      if MorseAlphabet[I] <> '?' then begin
        Inc(J);
        fMorse2.sgMarks.Cells[0, J] := '  ' + Chr(I);
        fMorse2.sgMarks.Cells[1, J] := MorseAlphabet[I];
      end;
    end
    else if I <= 57 then begin
      // Numbers
      if I = 48 then
        J := 0;
      Inc(J);
      fMorse2.sgNumbers.Cells[0, J] := '  ' + Chr(I);
      fMorse2.sgNumbers.Cells[1, J] := MorseAlphabet[I];
    end
    else begin
      // Letters
      if (I = 65) or (I = 78) then
        J := 0;
      Inc(J);
      if I <= 77 then begin
        // Letters first part
        fMorse2.sgLetters1.Cells[0, J] := '  ' + Chr(I);
        fMorse2.sgLetters1.Cells[1, J] := MorseAlphabet[I];
      end
      else begin
        // Letters second part
        fMorse2.sgLetters2.Cells[0, J] := '  ' + Chr(I);
        fMorse2.sgLetters2.Cells[1, J] := MorseAlphabet[I];
      end;
    end;
  end;
end;

{ Clear the morse alphabet string-grids }

procedure ClearMorseAlphabet;

var
  I: Integer;

begin
  for I := 1 to 13 do begin
    fMorse2.sgLetters1.Cells[0, I] := '';
    fMorse2.sgLetters1.Cells[1, I] := '';
    fMorse2.sgLetters2.Cells[0, I] := '';
    fMorse2.sgLetters2.Cells[1, I] := '';
  end;
  for I := 1 to 10 do begin
    fMorse2.sgNumbers.Cells[0, I] := '';
    fMorse2.sgNumbers.Cells[1, I] := '';
  end;
  for I := 1 to 15 do begin
    fMorse2.sgMarks.Cells[0, I] := '';
    fMorse2.sgMarks.Cells[1, I] := '';
  end;
end;

{**********}
{ TfMorse2 }
{**********}

{ Application start: Read morse code from file and display it }

procedure TfMorse2.FormCreate(Sender: TObject);

begin
  ReadMorseAlphabet(aMorseAlphabet);
  DisplayMorseAlphabet(aMorseAlphabet); bDisplayed := True;
  bLongerDash := False; bLongerWordSep := False;
end;

{ Main window getting focus: Display morse alphabet (if is not displayed) }

procedure TfMorse2.FormMouseEnter(Sender: TObject);

// This automatically displays the morse code after a knowledge test (during which it was hidden)

begin
  if not bDisplayed then begin
    DisplayMorseAlphabet(aMorseAlphabet);
    bDisplayed := True;
  end;
end;

{ Menu item "File > Text conversion": Convert text to morse code }

procedure TfMorse2.mFileTextClick(Sender: TObject);

begin
  // Set variables on fTranslation form and display this form
  fTranslation.sTranslation := 'text';
  fTranslation.bLDash := bLongerDash; fTranslation.bLWordSep := bLongerWordSep;
  fTranslation.aMorseAlphabet := aMorseAlphabet;
  fTranslation.btPlay.Visible := True; fTranslation.btPlay2.Visible := False;
  fTranslation.edBox1.Lines.Clear; fTranslation.edBox2.Lines.Clear;
  fTranslation.ShowModal;
end;

{ Menu item "File > Morse code conversion": Convert morse code to text }

procedure TfMorse2.mFileMorseClick(Sender: TObject);

begin
  // Set variables on fTranslation form and display this form
  fTranslation.sTranslation := 'morse';
  fTranslation.bLDash := bLongerDash; fTranslation.bLWordSep := bLongerWordSep;
  fTranslation.aMorseAlphabet := aMorseAlphabet;
  fTranslation.btPlay2.Visible := True; fTranslation.btPlay.Visible := False;
  fTranslation.edBox1.Lines.Clear; fTranslation.edBox2.Lines.Clear;
  fTranslation.ShowModal;
end;

{ Menu item "File > Character to morse test": Start 'character to morse code' knowledge test }

procedure TfMorse2.mFileTest1Click(Sender: TObject);

begin
  // Hide morse alphabet during test
  ClearMorseAlphabet; bDisplayed := False;
  // Set variables on fTest form and display this form
  fTest.sTranslation := 'char';
  fTest.bLDash := bLongerDash;
  fTest.aMorseAlphabet := aMorseAlphabet;
  fTest.ShowModal;
end;

{ Menu item "File > Morse to character test": Start 'morse code to character' knowledge test }

procedure TfMorse2.mFileTest2Click(Sender: TObject);

begin
  // Hide morse alphabet during test
  ClearMorseAlphabet; bDisplayed := False;
  // Set variables on fTest form and display this form
  fTest.sTranslation := 'morse';
  fTest.bLDash := bLongerDash;
  fTest.aMorseAlphabet := aMorseAlphabet;
  fTest.ShowModal;
end;

{ Menu item "File > Exit": Exit application }

procedure TfMorse2.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Longer dash duration": Choose or not longer dash duration }

procedure TfMorse2.mSettingsLDashClick(Sender: TObject);

begin
  if mSettingsLDash.Checked then
    mSettingsLDash.Checked := False
  else
    mSettingsLDash.Checked := True;
  bLongerDash := mSettingsLDash.Checked;                                       // this also increases character separation duration
end;

{ Menu item "Settings > Longer word separation": Choose or not longer word separation duration }

procedure TfMorse2.mSettingsLWSepClick(Sender: TObject);

begin
  if mSettingsLWSep.Checked then
    mSettingsLWSep.Checked := False
  else
    mSettingsLWSep.Checked := True;
  bLongerWordSep := mSettingsLWSep.Checked;
end;

{ Menu item "Help > Help": Display help text }

procedure TfMorse2.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display about text }

procedure TfMorse2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'International morse code.' + LineEnding;
  S += 'Text and morse code translation, morse audio output and morse code knowledge tests.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, January-March 2019.';
  MessageDlg('About "Morse2"', S, mtInformation, [mbOK], 0);
end;

end.

