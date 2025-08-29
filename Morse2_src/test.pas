{**********************************************}
{* Knowledge test unit for Morse2 application *}
{**********************************************}

unit test;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, ExtCtrls, common;

type
  TMorseAlphabetDone = array[32 .. 90] of Boolean;
  {********}
  { TfTest }
  {********}
  TfTest = class(TForm)
    stTitle: TStaticText;
    cbLetters, cbNumbers, cbMarks: TCheckBox;
    cbDisplay, cbAudio: TCheckBox;
    cbRepeat: TCheckBox;
    Label1, Label2: TLabel;
    edChar, edMorse: TEdit;
    edEval: TEdit;
    stEval: TStringGrid;
    btQuestion: TButton;
    btClose: TButton;
    tiMorseCode: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure tiMorseCodeTimer(Sender: TObject);
  private
    iQuestion, iCorrect, iChar, iPause: Integer;
    sMorseChar, sAnswer: string;
    bMorseAlphabet: TMorseAlphabetDone;
  public
    sTranslation: string;
    bLDash: Boolean;
    aMorseAlphabet: TMorseAlphabet;
  end;

var
  fTest: TfTest;

implementation

{$R *.lfm}

{ Format numbers for grid (right-align) }

function GFormat(N: Integer; Ch: string): string;

var
  S: string;

begin
  S := ' ' + IntToStr(N);
  if Ch = '' then
    S := ' ' + S;
  if N < 100 then
    S := ' ' + S;
  if N < 10 then
    S := ' ' + S;
  if Ch <> '' then
    S += Ch;
  GFormat := S;
end;

{********}
{ TfTest }
{********}

{ Form getting focus: Set controls depending on test done }

procedure TfTest.FormActivate(Sender: TObject);

var
  I: Integer;

begin
  // Character to morse knowledge test
  if sTranslation = 'char' then begin
    stTitle.Caption := 'Character to morse test.';
    edChar.TabStop := False;
    edChar.ReadOnly := True;
    edChar.Color := clCream;
    edMorse.TabStop := True;
    edMorse.ReadOnly := False;
    edMorse.Color := clDefault;
  end
  // Morse to character knowledge test
  else begin
    stTitle.Caption := 'Morse to character test.';
    edChar.TabStop := True;
    edChar.ReadOnly := False;
    edChar.Color := clDefault;
    edMorse.TabStop := False;
    edMorse.ReadOnly := True;
    edMorse.Color := clCream;
  end;
  // Mark all characters (that have a morse code) as not yet done
  for I := 32 to 90 do
    bMorseAlphabet[I] := False;
  bMorseAlphabet[32] := True; bMorseAlphabet[35] := True; bMorseAlphabet[36] := True; bMorseAlphabet[37] := True; bMorseAlphabet[42] := True;
  bMorseAlphabet[59] := True; bMorseAlphabet[60] := True; bMorseAlphabet[61] := True; bMorseAlphabet[62] := True;
  // Reset evaluation counters
  iQuestion := 0; iCorrect := 0;
  // Clear the form
  edChar.Text := ''; edMorse.Text := ''; edEval.Text := ''; edEval.Color := clCream;
  for I := 0 to 3 do
    stEval.Cells[1, I] := '';
  btQuestion.Caption := 'Start';
  btQuestion.Enabled := True;
end;

{ Button "Start/Next/Answer": Generate a random character/morse code resp. check user answer }

procedure TfTest.btQuestionClick(Sender: TObject);

var
  N, P: Integer;
  UserAnswer: string;
  OK: Boolean;

begin
  tiMorseCode.Enabled := False;                                                // disable the timer (will stop audio)
  // Button "Start/Next": Generate a random character resp. morse code
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Next') then begin
    // Proceed if there is at least one character group selected
    if cbLetters.Checked or cbNumbers.Checked or cbMarks.Checked then begin
      if sTranslation = 'char' then
        // In char to morse test, char is of course always displayed
        cbDisplay.Checked := True;
      // Proceed if at least one output option is selected
      if cbDisplay.Checked or cbAudio.Checked then begin
        N := 0;
        // Get random character (among those selected)
        repeat
          OK := True; Inc(N);
          iChar := Random(59) + 32;
          if aMorseAlphabet[iChar] = '?' then
            OK := False
          else if bMorseAlphabet[iChar] then
            OK := False
          else begin
            if not cbLetters.Checked and (iChar >= 65) then
              OK := False;
            if not cbNumbers.Checked and ((iChar >= 48) and (iChar <= 57)) then
              OK := False;
            if not cbMarks.Checked and ((iChar <= 47) or ((iChar >= 58) and (iChar <= 64))) then
              OK := False;
          end;
        until OK or (N > 1000);                                                // N > 1000: did not find any characters (-> because all done)
        // If a character not yet done has been found, do the question
        if OK then begin
          Inc(iQuestion);
          bMorseAlphabet[iChar] := True;                                       // mark character as being done
          edMorse.Text := ''; edChar.Text := '';
          if sTranslation = 'char' then begin
            // Character to morse test
            edChar.Text := Chr(iChar);
            sAnswer := aMorseAlphabet[iChar];
            edMorse.SetFocus;
          end
          else begin
            // Morse to character test
            if cbDisplay.Checked then
              // Morse code to find as dots and dashes display
              edMorse.Text := aMorseAlphabet[iChar];
            if cbAudio.Checked then begin
              // Morse code to find as audio signal
              sMorseChar := aMorseAlphabet[iChar];
              iPause := 0; tiMorseCode.Enabled := True;                        // enabling the timer with Pause set to 0 will play the morse code
            end;
            sAnswer := Chr(iChar);
            edChar.SetFocus;
          end;
          edEval.Text := '';
          btQuestion.Caption := 'Answer';
        end
        // If no character not yet done has been found, display 'end of test' message
        else begin
          MessageDlg('End of test', 'All test questions have been done!', mtInformation, [mbOK], 0);
          btQuestion.Enabled := False;
        end;
      end
      // Error message if no display option has been selected
      else
        MessageDlg('Selection error', 'Select one at least of display, audio!', mtError, [mbOK], 0);
    end
    // Error message if no character group has been selected
    else
      MessageDlg('Selection error', 'Select one at least of letters, numbers, punctuation marks!', mtError, [mbOK], 0);
  end
  // Button = "Answer": Check user's answer
  else begin
    if sTranslation = 'char' then
      // Character to morse test
      UserAnswer := edMorse.Text
    else
      // Morse to character test
      UserAnswer := UpperCase(edChar.Text);
    // Correct user answer
    if UserAnswer = sAnswer then begin
      Inc(iCorrect);
      edEval.Text := 'This is correct!';
      edEval.Color := clCream;
    end
    // False user answer
    else begin
      edEval.Text := 'False! Correct answer is: ' + sAnswer;
      edEval.Color := clRed;
      if cbRepeat.Checked then
        // Re-add not known character to list (if this option is selected)
        bMorseAlphabet[iChar] := False;
    end;
    if cbAudio.Checked then begin
      // Play the (correct) morse character
      sMorseChar := sAnswer;
      iPause := 0; tiMorseCode.Enabled := True;
    end;
    // Update evaluation grid
    stEval.Cells[1, 0] := GFormat(iQuestion, '');
    stEval.Cells[1, 1] := GFormat(iCorrect, '');
    stEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    P := Round(100 * (iCorrect / iQuestion));
    stEval.Cells[1, 3] := GFormat(P, '%');
    btQuestion.Caption := 'Next';
  end;
end;

{ Button "Close": Close the knowledge test window }

procedure TfTest.btCloseClick(Sender: TObject);

begin
  tiMorseCode.Enabled := False;                                                // stop audio signal
  Close;
end;

{ Timer routine to play the morse character }

procedure TfTest.tiMorseCodeTimer(Sender: TObject);

// Cf. timer routine in "translation" unit for detailed description

var
  Pause: Integer;

begin
  // Pause set to 0: Play the morse character
  if iPause = 0 then begin
    PlayMorseChar(sMorseChar, bLDash, False, Pause);                           // play dot/dash and return the pause to apply
    // All dots/dashes of the morse character have been played
    if sMorseChar = '' then
      tiMorseCode.Enabled := False;
  end
  // Pause set to value <> 0: This is time to pause left (and decremented as time passes)
  else
    iPause -= 5;
end;

end.

