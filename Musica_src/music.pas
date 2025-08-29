{************************************}
{* Main unit for Musica application *}
{************************************}

unit music;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus, LCLType, Crt, help;

type
  TNotes = array[0..11, 0..3] of Word;
  TPianoKeys = array[0..3, 0..11] of TShape;
  {**********}
  { TfMusica }
  {**********}
  TfMusica = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsInput, mOptionsInputMouse, mOptionsInputKeyb: TMenuItem;
    mOptionsKeyboard, mOptionsKeyboard1, mOptionsKeyboard2, mOptionsKeyboard3: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1, StaticText2: TStaticText;
    Shape1, Shape2, Shape3, Shape4, Shape5, Shape6, Shape7, Shape8: TShape;
    shC1, shE2, shF2, shG2, shA2, shB2, shCs1, shDs1, shFs1, shGs1: TShape;
    shAs1, shD1, shCs2, shDs2, shFs2, shGs2, shAs2, shC3, shD3, shE3: TShape;
    shF3, shG3, shE1, shA3, shB3, shCs3, shDs3, shFs3, shGs3, shAs3: TShape;
    shC4, shD4, shE4, shF1, shF4, shG4, shA4, shB4, shCs4, shDs4: TShape;
    shFs4, shGs4, shAs4, shG1, shA1, shB1, shC2, shD2: TShape;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsInputMouseClick(Sender: TObject);
    procedure mOptionsInputKeybClick(Sender: TObject);
    procedure mOptionsKeyboard1Click(Sender: TObject);
    procedure mOptionsKeyboard2Click(Sender: TObject);
    procedure mOptionsKeyboard3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word);
    procedure shA1MouseDown(Sender: TObject);
    procedure shA1MouseUp(Sender: TObject);
    procedure shA2MouseDown(Sender: TObject);
    procedure shA2MouseUp(Sender: TObject);
    procedure shA3MouseDown(Sender: TObject);
    procedure shA3MouseUp(Sender: TObject);
    procedure shA4MouseDown(Sender: TObject);
    procedure shA4MouseUp(Sender: TObject);
    procedure shAs1MouseDown(Sender: TObject);
    procedure shAs1MouseUp(Sender: TObject);
    procedure shAs2MouseDown(Sender: TObject);
    procedure shAs2MouseUp(Sender: TObject);
    procedure shAs3MouseDown(Sender: TObject);
    procedure shAs3MouseUp(Sender: TObject);
    procedure shAs4MouseDown(Sender: TObject);
    procedure shAs4MouseUp(Sender: TObject);
    procedure shB1MouseDown(Sender: TObject);
    procedure shB1MouseUp(Sender: TObject);
    procedure shB2MouseDown(Sender: TObject);
    procedure shB2MouseUp(Sender: TObject);
    procedure shB3MouseDown(Sender: TObject);
    procedure shB3MouseUp(Sender: TObject);
    procedure shB4MouseDown(Sender: TObject);
    procedure shB4MouseUp(Sender: TObject);
    procedure shC1MouseDown(Sender: TObject);
    procedure shC1MouseUp(Sender: TObject);
    procedure shC2MouseDown(Sender: TObject);
    procedure shC2MouseUp(Sender: TObject);
    procedure shC3MouseDown(Sender: TObject);
    procedure shC3MouseUp(Sender: TObject);
    procedure shC4MouseDown(Sender: TObject);
    procedure shC4MouseUp(Sender: TObject);
    procedure shCs1MouseDown(Sender: TObject);
    procedure shCs1MouseUp(Sender: TObject);
    procedure shCs2MouseDown(Sender: TObject);
    procedure shCs2MouseUp(Sender: TObject);
    procedure shCs3MouseDown(Sender: TObject);
    procedure shCs3MouseUp(Sender: TObject);
    procedure shCs4MouseDown(Sender: TObject);
    procedure shCs4MouseUp(Sender: TObject);
    procedure shD1MouseDown(Sender: TObject);
    procedure shD1MouseUp(Sender: TObject);
    procedure shD2MouseDown(Sender: TObject);
    procedure shD2MouseUp(Sender: TObject);
    procedure shD3MouseDown(Sender: TObject);
    procedure shD3MouseUp(Sender: TObject);
    procedure shD4MouseDown(Sender: TObject);
    procedure shD4MouseUp(Sender: TObject);
    procedure shDs1MouseDown(Sender: TObject);
    procedure shDs1MouseUp(Sender: TObject);
    procedure shDs2MouseDown(Sender: TObject);
    procedure shDs2MouseUp(Sender: TObject);
    procedure shDs3MouseDown(Sender: TObject);
    procedure shDs3MouseUp(Sender: TObject);
    procedure shDs4MouseDown(Sender: TObject);
    procedure shDs4MouseUp(Sender: TObject);
    procedure shE1MouseDown(Sender: TObject);
    procedure shE1MouseUp(Sender: TObject);
    procedure shE2MouseDown(Sender: TObject);
    procedure shE2MouseUp(Sender: TObject);
    procedure shE3MouseDown(Sender: TObject);
    procedure shE3MouseUp(Sender: TObject);
    procedure shE4MouseDown(Sender: TObject);
    procedure shE4MouseUp(Sender: TObject);
    procedure shF1MouseDown(Sender: TObject);
    procedure shF1MouseUp(Sender: TObject);
    procedure shF2MouseDown(Sender: TObject);
    procedure shF2MouseUp(Sender: TObject);
    procedure shF3MouseDown(Sender: TObject);
    procedure shF3MouseUp(Sender: TObject);
    procedure shF4MouseDown(Sender: TObject);
    procedure shF4MouseUp(Sender: TObject);
    procedure shFs1MouseDown(Sender: TObject);
    procedure shFs1MouseUp(Sender: TObject);
    procedure shFs2MouseDown(Sender: TObject);
    procedure shFs2MouseUp(Sender: TObject);
    procedure shFs3MouseDown(Sender: TObject);
    procedure shFs3MouseUp(Sender: TObject);
    procedure shFs4MouseDown(Sender: TObject);
    procedure shFs4MouseUp(Sender: TObject);
    procedure shG1MouseDown(Sender: TObject);
    procedure shG1MouseUp(Sender: TObject);
    procedure shG2MouseDown(Sender: TObject);
    procedure shG2MouseUp(Sender: TObject);
    procedure shG3MouseDown(Sender: TObject);
    procedure shG3MouseUp(Sender: TObject);
    procedure shG4MouseDown(Sender: TObject);
    procedure shG4MouseUp(Sender: TObject);
    procedure shGs1MouseDown(Sender: TObject);
    procedure shGs1MouseUp(Sender: TObject);
    procedure shGs2MouseDown(Sender: TObject);
    procedure shGs2MouseUp(Sender: TObject);
    procedure shGs3MouseDown(Sender: TObject);
    procedure shGs3MouseUp(Sender: TObject);
    procedure shGs4MouseDown(Sender: TObject);
    procedure shGs4MouseUp(Sender: TObject);
  private
    iNoteKey: Integer;
    wKeybKey: Word;
    bPlay: Boolean;
    shKeys: TPianoKeys;
  end;

const
  // 4-octaves note frequencies. Some values are 1-2 Hz different from calculated value. No idea why,
  // but tones with an odd frequency values do not play propperly using the Crt Sound procedure
  aNotes: TNotes = (
    (262, 524, 1046, 2094),
    (278, 554, 1108, 2218),
    (294, 588, 1176, 2352),
    (312, 622, 1244, 2488),
    (330, 660, 1318, 2636),
    (350, 698, 1396, 2792),
    (370, 740, 1480, 2960),
    (392, 784, 1568, 3136),
    (416, 832, 1662, 3324),
    (440, 880, 1760, 3520),
    (466, 932, 1864, 3728),
    (494, 988, 1976, 3952)
  );

var
  fMusica: TfMusica;

implementation

{$R *.lfm}

{ Push note key: Highlight or reset the key's color }

procedure PushKey(NoteKey: Integer);

// The value passed corresponds to the position of the key (1-48)
// A negative value is used, when the color has to be reset (to white resp. black)

var
  I, J: Integer;

begin
  // 2-dimensional array row and column index calculation
  I := (Abs(NoteKey) - 1) div 12;
  J := (Abs(NoteKey) - 1) mod 12;
  // Highlight or reset key color
  if NoteKey >= 0 then begin
    // Highlight the key (aqua or blue)
    if fMusica.shKeys[I, J].Brush.Color = clWhite then
      fMusica.shKeys[I, J].Brush.Color := clAqua
    else
      fMusica.shKeys[I, J].Brush.Color := clBlue;
  end
  else begin
    // Un-highlight the key (white or black)
    if fMusica.shKeys[I, J].Brush.Color = clAqua then
      fMusica.shKeys[I, J].Brush.Color := clWhite
    else
      fMusica.shKeys[I, J].Brush.Color := clBlack;
  end;
end;

{ Play a given note (of given octave) }

procedure PlayNote(Octave, Note: Integer);

// The values passed correpond to the indexes of he 2-dimensional note frequencies array

begin
  Sound(aNotes[Note, Octave]);
end;

{ Play resp. stop playing a given note }

procedure PlayKeybNote(NoteKey: Integer);

// The value passed corresponds to the notes numbered from 1 to 48
// Negative values indicate that the note playing has to be stopped

var
  Octave, Note: Integer;

begin
  // Highlight resp. reset the note key's color
  PushKey(NoteKey);
  // Stop playing the note actually playing
  NoSound;
  // If the procedure is called to play a note, do so
  if NoteKey >= 0 then begin
    // Octave and note index determination
    Dec(NoteKey);
    Octave := NoteKey div 12;
    Note   := NoteKey mod 12;
    // Play the note
    PlayNote(Octave, Note);
  end;
end;

{**********}
{ TfMusica }
{**********}

{ Application start: Initialisation }

procedure TfMusica.FormCreate(Sender: TObject);

begin
  // Create 2-dimensional array with note key shapes
  shKeys[0, 0] := shC1;  shKeys[0, 1] := shCs1; shKeys[0, 2]  := shD1;  shKeys[0, 3]  := shDs1;
  shKeys[0, 4] := shE1;  shKeys[0, 5] := shF1;  shKeys[0, 6]  := shFs1; shKeys[0, 7]  := shG1;
  shKeys[0, 8] := shGs1; shKeys[0, 9] := shA1;  shKeys[0, 10] := shAs1; shKeys[0, 11] := shB1;
  shKeys[1, 0] := shC2;  shKeys[1, 1] := shCs2; shKeys[1, 2]  := shD2;  shKeys[1, 3]  := shDs2;
  shKeys[1, 4] := shE2;  shKeys[1, 5] := shF2;  shKeys[1, 6]  := shFs2; shKeys[1, 7]  := shG2;
  shKeys[1, 8] := shGs2; shKeys[1, 9] := shA2;  shKeys[1, 10] := shAs2; shKeys[1, 11] := shB2;
  shKeys[2, 0] := shC3;  shKeys[2, 1] := shCs3; shKeys[2, 2]  := shD3;  shKeys[2, 3]  := shDs3;
  shKeys[2, 4] := shE3;  shKeys[2, 5] := shF3;  shKeys[2, 6]  := shFs3; shKeys[2, 7]  := shG3;
  shKeys[2, 8] := shGs3; shKeys[2, 9] := shA3;  shKeys[2, 10] := shAs3; shKeys[2, 11] := shB3;
  shKeys[3, 0] := shC4;  shKeys[3, 1] := shCs4; shKeys[3, 2]  := shD4;  shKeys[3, 3]  := shDs4;
  shKeys[3, 4] := shE4;  shKeys[3, 5] := shF4;  shKeys[3, 6]  := shFs4; shKeys[3, 7]  := shG4;
  shKeys[3, 8] := shGs4; shKeys[3, 9] := shA4;  shKeys[3, 10] := shAs4; shKeys[3, 11] := shB4;
  // Init variables
  wKeybKey := VK_UNDEFINED;                                                    // Keyboard key pressed
  iNoteKey := -1;                                                              // Corresponding note (numbered from 1-48)
  bPlay := False;                                                              // Sound control variable (cf. further down)
end;

{ Menu item "File > Exit": Exit application }

procedure TfMusica.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Input > ...": Select mouse or keyboard as notes input }

procedure TfMusica.mOptionsInputMouseClick(Sender: TObject);

begin
  mOptionsInputMouse.Checked := True; mOptionsInputKeyb.Checked := False;
  wKeybKey := VK_UNDEFINED; iNoteKey := -1; bPlay := False;
end;

procedure TfMusica.mOptionsInputKeybClick(Sender: TObject);

begin
  mOptionsInputKeyb.Checked := True; mOptionsInputMouse.Checked := False;
  wKeybKey := VK_UNDEFINED; iNoteKey := -1; bPlay := True;
end;

{ Menu items "Options > Keyboard > ...": Select keyboard layout (Qwertz, Qwerty, Azerty) }

procedure TfMusica.mOptionsKeyboard1Click(Sender: TObject);

// By choosing the value corresponding to the user's keyboard layout, it is possible to use
// the keys located at the same position on the keyboard independently of the keyboard used
// Ex: With a Qwertz keyboard, C is played with Y, for Qwerty with Z, for Azerty with W, i.e.
//     the left-most letter-key in the row above the space bar

begin
  mOptionsKeyboard1.Checked := True; mOptionsKeyboard2.Checked := False; mOptionsKeyboard3.Checked := False;
end;

procedure TfMusica.mOptionsKeyboard2Click(Sender: TObject);

begin
  mOptionsKeyboard1.Checked := False; mOptionsKeyboard2.Checked := True; mOptionsKeyboard3.Checked := False;
end;

procedure TfMusica.mOptionsKeyboard3Click(Sender: TObject);

begin
  mOptionsKeyboard1.Checked := False; mOptionsKeyboard2.Checked := False; mOptionsKeyboard3.Checked := True;
end;

{ Menu item "Help > Help": Display application help }

procedure TfMusica.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfMusica.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := '(Very simple) music application.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March 2022.';
  MessageDlg('About "Musica"', S, mtInformation, [mbOK], 0);
end;

{ Keystroke event handler (key pressed): Start playing a note }

procedure TfMusica.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

var
  Octave, Note, KeybNote: Integer;

begin
  // Do only if note input is set to "keyboard"
  if mOptionsInputKeyb.Checked then begin
    // Start playing the note only, if hasn't already been started
    // The bPlay variable controls the sound: If it has value True (and only then), the sound is turned on and the variable
    // is set to False until the user releases the keyboard key (= stops playing the note). This is necessary, because a
    // keyboard key held down continuously retriggers the onKeyDown event. As I stop the sound before starting it with another
    // note, this would continously turn the sound on and off resulting in a tone interrupted by a clicking noise each time
    // the event is retriggered. As the procedure, setting the sound on, is disabled until the user releases the key pressed,
    // a proper continuous tone will be played.
    if bPlay then begin
      Note := -1;
      // Determine the note (numbered from 0 to 11) corresponding to the keyboard key pressed
      // For C and B, the key varies with the keyboard layout selected in the Options menu
      case Key of
        VK_Y: if moptionsKeyboard1.Checked then Note := 0;
        VK_Z: if moptionsKeyboard2.Checked then Note := 0;
        VK_W: if moptionsKeyboard3.Checked then Note := 0;
        VK_S: Note := 1;
        VK_X: Note := 2;
        VK_D: Note := 3;
        VK_C: Note := 4;
        VK_V: Note := 5;
        VK_G: Note := 6;
        VK_B: Note := 7;
        VK_H: Note := 8;
        VK_N: Note := 9;
        VK_J: Note := 10;
        VK_M: if moptionsKeyboard1.Checked or moptionsKeyboard2.Checked then Note := 11;
        VK_LCL_Comma: if moptionsKeyboard3.Checked then Note := 11;
      end;
      // Proceed only if the key pressed actually is a "note" key
      if Note <> -1 then begin
        wKeybKey := Key;                                                       // save the VK value of the key pressed
        // Determine the octave of the note played (depending on SHIFT/CTRL pressed by user)
        if (ssShift in Shift) and (ssCtrl in Shift) then
          Octave := 3
        else if ssCtrl in Shift then
          Octave := 2
        else if ssShift in Shift then
          Octave := 1
        else
          Octave := 0;
        // Play the note corresponding to the "note" and "octave" keys pressed
        KeybNote := Octave * 12 + Note + 1;
        iNoteKey := KeybNote;
        PlayKeybNote(KeybNote);
        // Disable the execution of the sound on procedure until the user releases the keyboard key pressed
        bPlay := False;
      end
      // Key pressed is not a "note" key
      else begin
        wKeybKey := VK_Unknown;
        iNoteKey := -1;
      end;
    end;
  end;
end;

{ Keystroke event handler (key released): Stop playing the note actually playing }

procedure TfMusica.FormKeyUp(Sender: TObject; var Key: Word);

begin
  // Do only if note input is set to "keyboard"
  if mOptionsInputKeyb.Checked then begin
    // Do only if a note key has been pressed (sound is actually on)
    if (wKeybKey <> VK_Undefined) and (wKeybKey <> VK_Unknown) and (Key = wKeybKey) and (iNoteKey <> -1) then
      // Stop playing the note (the negative note value is used to reset the note key's color)
      PlayKeybNote(-iNoteKey);
    // Re-eneble the sound on procedure (next onKeyDown will start playing a note)
    bPlay := True;
  end;
end;

{ Mouse down on note key shapes: Start playing the corresponding note }
{ Mouse up from note key shapes: Stop playing the corresponding note }

procedure TfMusica.shC1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(1);
end;

procedure TfMusica.shC1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-1);
end;

procedure TfMusica.shCs1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(2);
end;

procedure TfMusica.shCs1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-2);
end;

procedure TfMusica.shD1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(3);
end;

procedure TfMusica.shD1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-3);
end;

procedure TfMusica.shDs1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(4);
end;

procedure TfMusica.shDs1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-4);
end;

procedure TfMusica.shE1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(5);
end;

procedure TfMusica.shE1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-5);
end;

procedure TfMusica.shF1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(6);
end;

procedure TfMusica.shF1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-6);
end;

procedure TfMusica.shFs1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(7);
end;

procedure TfMusica.shFs1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-7);
end;

procedure TfMusica.shG1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(8);
end;

procedure TfMusica.shG1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-8);
end;

procedure TfMusica.shGs1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(9);
end;

procedure TfMusica.shGs1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-9);
end;

procedure TfMusica.shA1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(10);
end;

procedure TfMusica.shA1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-10);
end;

procedure TfMusica.shAs1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(11);
end;

procedure TfMusica.shAs1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-11);
end;

procedure TfMusica.shB1MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(12);
end;

procedure TfMusica.shB1MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-12);
end;

procedure TfMusica.shC2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(13);
end;

procedure TfMusica.shC2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-13);
end;

procedure TfMusica.shCs2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(14);
end;

procedure TfMusica.shCs2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-14);
end;

procedure TfMusica.shD2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(15);
end;

procedure TfMusica.shD2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-15);
end;

procedure TfMusica.shDs2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(16);
end;

procedure TfMusica.shDs2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-16);
end;

procedure TfMusica.shE2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(17);
end;

procedure TfMusica.shE2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-17);
end;

procedure TfMusica.shF2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(18);
end;

procedure TfMusica.shF2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-18);
end;

procedure TfMusica.shFs2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(19);
end;

procedure TfMusica.shFs2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-19);
end;

procedure TfMusica.shG2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(20);
end;

procedure TfMusica.shG2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-20);
end;

procedure TfMusica.shGs2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(21);
end;

procedure TfMusica.shGs2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-21);
end;

procedure TfMusica.shA2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(22);
end;

procedure TfMusica.shA2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-22);
end;

procedure TfMusica.shAs2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(23);
end;

procedure TfMusica.shAs2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-23);
end;

procedure TfMusica.shB2MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(24);
end;

procedure TfMusica.shB2MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-24);
end;

procedure TfMusica.shC3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(25);
end;

procedure TfMusica.shC3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-25);
end;

procedure TfMusica.shCs3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(26);
end;

procedure TfMusica.shCs3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-26);
end;

procedure TfMusica.shD3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(27);
end;

procedure TfMusica.shD3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-27);
end;

procedure TfMusica.shDs3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(28);
end;

procedure TfMusica.shDs3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-28);
end;

procedure TfMusica.shE3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(29);
end;

procedure TfMusica.shE3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-29);
end;

procedure TfMusica.shF3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(30);
end;

procedure TfMusica.shF3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-30);
end;

procedure TfMusica.shFs3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(31);
end;

procedure TfMusica.shFs3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-31);
end;

procedure TfMusica.shG3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(32);
end;

procedure TfMusica.shG3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-32);
end;

procedure TfMusica.shGs3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(33);
end;

procedure TfMusica.shGs3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-33);
end;

procedure TfMusica.shA3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(34);
end;

procedure TfMusica.shA3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-34);
end;

procedure TfMusica.shAs3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(35);
end;

procedure TfMusica.shAs3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-35);
end;

procedure TfMusica.shB3MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(36);
end;

procedure TfMusica.shB3MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-36);
end;

procedure TfMusica.shC4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(37);
end;

procedure TfMusica.shC4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-37);
end;

procedure TfMusica.shCs4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(38);
end;

procedure TfMusica.shCs4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-38);
end;

procedure TfMusica.shD4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(39);
end;

procedure TfMusica.shD4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-39);
end;

procedure TfMusica.shDs4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(40);
end;

procedure TfMusica.shDs4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-40);
end;

procedure TfMusica.shE4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(41);
end;

procedure TfMusica.shE4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-41);
end;

procedure TfMusica.shF4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(42);
end;

procedure TfMusica.shF4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-42);
end;

procedure TfMusica.shFs4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(43);
end;

procedure TfMusica.shFs4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-43);
end;

procedure TfMusica.shG4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(44);
end;

procedure TfMusica.shG4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-44);
end;

procedure TfMusica.shGs4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(45);
end;

procedure TfMusica.shGs4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-45);
end;

procedure TfMusica.shA4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(46);
end;

procedure TfMusica.shA4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-46);
end;

procedure TfMusica.shAs4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(47);
end;

procedure TfMusica.shAs4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-47);
end;

procedure TfMusica.shB4MouseDown(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(48);
end;

procedure TfMusica.shB4MouseUp(Sender: TObject);

begin
  if mOptionsInputMouse.Checked then
    PlayKeybNote(-48);
end;

end.

