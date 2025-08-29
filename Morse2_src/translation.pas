{******************************************}
{* Conversion unit for Morse2 application *}
{******************************************}

unit translation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, common;

type
  {***************}
  { TfTranslation }
  {***************}
  TfTranslation = class(TForm)
    stTitle: TStaticText;
    edDetails: TMemo;
    edBox1, edBox2: TMemo;
    btTranslate: TButton;
    btPlay: TButton;
    btPlay2: TButton;
    btLoad: TButton;
    btClear: TButton;
    btSave: TButton;
    btClose: TButton;
    tiMorseCode: TTimer;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btTranslateClick(Sender: TObject);
    procedure btPlayClick(Sender: TObject);
    procedure btPlay2Click(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure tiMorseCodeTimer(Sender: TObject);
  private
    iPause: Integer;
    sDir: string;
    sMorseCode: AnsiString;
  public
    sTranslation: string;
    bLDash, bLWordSep: Boolean;
    aMorseAlphabet: TMorseAlphabet;
  end;

var
  fTranslation: TfTranslation;

implementation

{$R *.lfm}

{***************}
{ TfTranslation }
{***************}

{ Application start: Initialisation }

procedure TfTranslation.FormCreate(Sender: TObject);

begin
  sDir := GetCurrentDir;
end;

{ Form getting focus: Set controls depending on conversion done }

procedure TfTranslation.FormActivate(Sender: TObject);

var
  S: AnsiString;

begin
  if sTranslation = 'text' then begin
    stTitle.Caption := 'Text to morse code conversion.';
    S := 'Enter your text (or get it from a file, using the "Load" button) in the left box and push the "Convert" button ';
    S += 'to convert it to morse code. Use the "Play" button for morse code audio output. Text and corresponding morse ';
    S += 'code may be stored into a file, using the "Save" button.';
    edDetails.Lines.Clear;
    edDetails.Lines.AddText(S);
  end
  else begin
    stTitle.Caption := 'Morse code to text conversion.';
    S := 'Enter your morse code (or get it from a file, using the "Load" button) in the left box, using "." for dot and "-" ';
    S += 'for dash. Separate morse characters by a space; separate words by slash + space. ';
    S += 'Push the "Convert" button to convert the morse code to text. Use the "Play" button for morse code audio output. ';
    S += 'Text and corresponding morse code may be stored into a file, using the "Save" button.';
    edDetails.Lines.Clear;
    edDetails.Lines.AddText(S);
  end;
  btPlay.Caption := 'Play'; btPlay2.Caption := 'Play';
end;

{ Button "Convert": Convert text to morse code or vice-versa }

procedure TfTranslation.btTranslateClick(Sender: TObject);

// Conversion is done by routines located in the "common" unit

var
  S: AnsiString;

begin
  tiMorseCode.Enabled := False;
  if edBox1.Lines.Count > 0 then begin
    S := edBox1.Lines.Text;
    edBox2.Lines.Clear;
    if sTranslation = 'text' then
      edBox2.Lines.AddText(TextToMorse(S, aMorseAlphabet))
    else
      edBox2.Lines.AddText(MorseToText(S, aMorseAlphabet))
  end;
end;

{ Buttons "Play/Stop": Play morse code resp. stop audio signal }

procedure TfTranslation.btPlayClick(Sender: TObject);

// The morse code is played by activating the timer (the timer routine calling the playback sub in the "common" unit)

var
  I: Integer;
  Mess: string;

begin
  // Button "Play": Play the morse code
  if btPlay.Caption = 'Play' then begin
    if edBox2.Lines.Count > 0 then begin
      sMorseCode := '';
      for I := 0 to edBox2.Lines.Count - 1 do
        sMorseCode += edBox2.Lines[I] + LineEnding;
      AdjustMorseCode(sMorseCode, Mess);
      if Mess <> '' then
        // Don't do any playback if morse code contains error
        MessageDlg('Data error', Mess, mtError, [mbOK], 0)
      else begin
        btPlay.Caption := 'Stop';
        // Start the timer (= start audio playback)
        iPause := 0; tiMorseCode.Enabled := True;
      end;
    end;
  end
  // Button "Stop": Stop audio playback
  else begin
    tiMorseCode.Enabled := False;
    btPlay.Caption := 'Play';
  end;
end;

procedure TfTranslation.btPlay2Click(Sender: TObject);

var
  I: Integer;
  Mess: string;

begin
  if btPlay2.Caption = 'Play' then begin;
    if edBox1.Lines.Count > 0 then begin
      sMorseCode := '';
      for I := 0 to edBox1.Lines.Count - 1 do
        sMorseCode += edBox1.Lines[I] + LineEnding;
      AdjustMorseCode(sMorseCode, Mess);
      if Mess <> '' then
        MessageDlg('Data error', Mess, mtError, [mbOK], 0)
      else begin
        btPlay2.Caption := 'Stop';
        iPause := 0; tiMorseCode.Enabled := True;
      end;
    end;
  end
  else begin
    tiMorseCode.Enabled := False;
    btPlay2.Caption := 'Play';
  end;
end;

{ Button "Clear": Clear text/morse code boxes }

procedure TfTranslation.btClearClick(Sender: TObject);

begin
  edBox1.Lines.Clear;
  edBox2.Lines.Clear;
end;

{ Button "Load": Load text/morse code from file }

procedure TfTranslation.btLoadClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpen.InitialDir := sDir; Filename := '';
  // Set filters depending on what you want to load
  if sTranslation = 'text' then
    dlgOpen.Filter := 'Text files|*.txt'
  else
    dlgOpen.Filter := 'Morse code files|*.mrs';
  dlgOpen.Filter := dlgOpen.Filter + '|All files|*.*';
  // Load the text/morse code in left box
  if dlgOpen.Execute then begin
    FileName := dlgOpen.Filename;
    edBox1.Lines.LoadFromFile(FileName);
    sDir := ExtractFilePath(FileName);
    edBox2.Lines.Clear;
  end;
end;

{ Button "Save": Save text and morse code to files }

procedure TfTranslation.btSaveClick(Sender: TObject);

var
  Filename, S: string;

begin
  if edBox1.Lines.Count > 0 then begin
    dlgSave.InitialDir := sDir; Filename := '';
    // Set filter, depending on what conversion you actually do
    if sTranslation = 'text' then
      dlgSave.Filter := 'Text file|*.txt'
    else
      dlgSave.Filter := 'Morse code files|*.mrs';
    // Save content of both boxes to files
    if dlgSave.Execute then begin
      Filename := dlgSave.Filename;
      edBox1.Lines.SaveToFile(Filename);
      if edBox2.Lines.Count = 0 then begin
        // Display message if saving was done without doing conversion (2nd file will be empty)
        S := 'No conversion done: ';
        if sTranslation = 'text' then
          S += 'Morse code file will be empty!'
        else
          S += 'Text file will be empty!';
        MessageDlg('Empty file', S, mtWarning, [mbOK], 0);
      end;
      if sTranslation = 'text' then
        Filename := StringReplace(Filename, '.txt', '.mrs', [])
      else
        Filename := StringReplace(Filename, '.mrs', '.txt', []);
      edBox2.Lines.SaveToFile(Filename);
      sDir := ExtractFilePath(FileName);
      Filename := StringReplace(ExtractFilename(Filename), ExtractFileExt(Filename), '', []);
      S := 'Text file name = ' + Filename + '.txt' + LineEnding + 'Morse code file name = ' + Filename + '.mrs';
      MessageDlg('Files successfully saved', S, mtInformation, [mbOK], 0);
    end;
  end;
end;

{ Button "Close": Close the conversion window }

procedure TfTranslation.btCloseClick(Sender: TObject);

begin
  tiMorseCode.Enabled := False;                                                // stop audio playback
  Close;
end;

{ Timer routine: Play morse code }

procedure TfTranslation.tiMorseCodeTimer(Sender: TObject);

// The timer routine uses 2 variables: sMorseCode = a string containing the morse code; iPause = pause (delay) to do after a dot/dash has been played
// When iPause = 0 (at startup and every time subsequent decrements give so), the morse code string is passed to the PlayMorseChar routine located in
// the "common" unit. One dot/dash is played and, together with eventual separator characters, removed from the string. The pause to be applied
// is returned as iPause and the following calls to the timer routine decrement this variable. When 0, the next dot/dash is played, and so on, until
// the morse code string is empty.

begin
  // iPause = 0: Play dot/dash
  if iPause = 0 then begin
    while (sMorseCode <> '') and (sMorseCode[1] = '#') do
      // Eliminate any linebreaks (coded as '#'; AdjustMorseCode routine)
      Delete(sMorseCode, 1, 1);
    // Play the dot/dash (calling PlayMorseChar in the "common" unit)
    // iPause now contains the delay to be applied before playing the next dot/dash
    if sMorseCode <> '' then begin
      PlayMorseChar(sMorseCode, bLDash, bLWordSep, iPause);
    end
    // All code has been played: Stop the timer
    else begin
      tiMorseCode.Enabled := False;
      btPlay.Caption := 'Play';
    end;
  end;
  // Decrement pause (delay) by value = equal to timer interval
  iPause -= 5;
end;

end.

