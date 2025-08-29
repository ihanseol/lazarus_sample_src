{*********************************************}
{* Main unit for Adventskalender application *}
{*********************************************}

unit advent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type
  {**********}
  { TfAdvent }
  {**********}
  TfAdvent = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mSettings, mSettingsHints: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imBackground, imPicture: TImage;
    edMess: TMemo;
    shDoor1, shDoor2, shDoor3, shDoor4, shDoor5, shDoor6: TShape;
    shDoor7, shDoor8, shDoor9, shDoor10, shDoor11, shDoor12: TShape;
    shDoor13, shDoor14, shDoor15, shDoor16, shDoor17, shDoor18: TShape;
    shDoor19, shDoor20, shDoor21, shDoor22, shDoor23, shDoor24: TShape;
    stDoor1, stDoor2, stDoor3, stDoor4, stDoor5, stDoor6: TStaticText;
    stDoor7, stDoor8, stDoor9, stDoor10, stDoor11, stDoor12: TStaticText;
    stDoor13, stDoor14, stDoor15, stDoor16, stDoor17, stDoor18: TStaticText;
    stDoor19, stDoor20, stDoor21, stDoor22, stDoor23, stDoor24: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsHintsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure stDoor10DblClick(Sender: TObject);
    procedure stDoor11DblClick(Sender: TObject);
    procedure stDoor12DblClick(Sender: TObject);
    procedure stDoor13DblClick(Sender: TObject);
    procedure stDoor14DblClick(Sender: TObject);
    procedure stDoor15DblClick(Sender: TObject);
    procedure stDoor16DblClick(Sender: TObject);
    procedure stDoor17DblClick(Sender: TObject);
    procedure stDoor18DblClick(Sender: TObject);
    procedure stDoor19DblClick(Sender: TObject);
    procedure stDoor1DblClick(Sender: TObject);
    procedure stDoor20DblClick(Sender: TObject);
    procedure stDoor21DblClick(Sender: TObject);
    procedure stDoor22DblClick(Sender: TObject);
    procedure stDoor23DblClick(Sender: TObject);
    procedure stDoor24DblClick(Sender: TObject);
    procedure stDoor2DblClick(Sender: TObject);
    procedure stDoor3DblClick(Sender: TObject);
    procedure stDoor4DblClick(Sender: TObject);
    procedure stDoor5DblClick(Sender: TObject);
    procedure stDoor6DblClick(Sender: TObject);
    procedure stDoor7DblClick(Sender: TObject);
    procedure stDoor8DblClick(Sender: TObject);
    procedure stDoor9DblClick(Sender: TObject);
    procedure edMessClick(Sender: TObject);
    procedure imPictureClick(Sender: TObject);
  private
    iDay, iMonth, iYear: Word;
    shDoors: array[0..23] of TShape;
    stDoors: array[0..23] of TStaticText;
  public

  end;

var
  fAdvent: TfAdvent;

implementation

{$R *.lfm}

{ One byte hexadecimal to decimal conversion }

function HexToDec(Hex: string): Byte;

var
  D0, D1: Byte;
  First, Last: Char;

begin
  First := LeftStr(Hex, 1)[1]; Last := RightStr(Hex, 1)[1];
  if First in ['0'..'9'] then
    D1 := StrToInt(First)
  else
    D1 := Ord(First) - Ord('A') + 10;
  if Last in ['0'..'9'] then
    D0 := StrToInt(Last)
  else
    D0 := Ord(Last) - Ord('A') + 10;
  Result := 16 * D1 + D0;
end;

{ Enable or disable hint display }

procedure SetHints(Doors: array of TStaticText; Status: Boolean);

var
  I: Integer;

begin
  for I := 0 to 23 do
    Doors[I].ShowHint := Status;
end;

{ Create text file for actual day }

procedure CreateTextFile(Filename: string; Day: Integer);

var
  OutFile: Text;

begin
  Assign(OutFile, Filename); Rewrite(OutFile);
  case Day of
    1: begin
        Writeln(OutFile, 'Jede Schneeflocke ist ein Kunstwerk für sich.');
        Writeln(OutFile, 'Jede ist einzigartig, doch wirkt so unscheinbar.');
        Writeln(OutFile, 'Nur zusammen zeigen sie ihre ganze Stärke.');
    end;
    2: begin
        Writeln(OutFile, 'Die Winterwelt zieht uns in ihren Bann,');
        Writeln(OutFile, 'die Luft so rein und kalt und klar,');
        Writeln(OutFile, 'die Welt hält kurz den Atem an –');
        Writeln(OutFile, 'in der schönsten Zeit im Jahr.');
        Writeln(OutFile, 'Die Schneeflöckchen im sanften Tanz,');
        Writeln(OutFile, 'Ruhe und Frieden für den einen Moment,');
        Writeln(OutFile, 'mit Kerzenschein und Lichterglanz');
        Writeln(OutFile, 'kommt die stille Zeit – Advent!');
    end;
    5: begin
        Writeln(OutFile, 'Jedes Jahr aufs Neue wundere ich mich:');
        Writeln(OutFile, 'Mein Weihnachtsstrumpf bleibt stetig leer.');
        Writeln(OutFile, 'Es geht mir völlig gegen den Strich');
        Writeln(OutFile, 'bin ich doch immer lieb und brav – wie unfair!');
        Writeln(OutFile, 'Ich überlege hin und überlege her');
        Writeln(OutFile, 'und wundere mich gar sehr.');
        Writeln(OutFile, 'Dann irgendwann kommt es mir doch:');
        Writeln(OutFile, 'In der Socke ist ein Loch!');
    end;
    6: begin
        Writeln(OutFile, 'Lieber, guter Weihnachtsmann,');
        Writeln(OutFile, 'zieh die langen Stiefel an,');
        Writeln(OutFile, 'kämme deinen weißen Bart,');
        Writeln(OutFile, 'mach‘ dich auf die Weihnachtsfahrt.');
        Writeln(OutFile, 'Komm‘ doch auch in unser Haus,');
        Writeln(OutFile, 'packe die Geschenke aus.');
        Writeln(OutFile, 'Ach, erst das Sprüchlein wolltest du?');
        Writeln(OutFile, 'Ja, ich kann es, hör mal zu:');
        Writeln(OutFile, 'Lieber, guter Weihnachtsmann,');
        Writeln(OutFile, 'guck mich nicht so böse an.');
        Writeln(OutFile, 'Stecke deine Rute ein,');
        Writeln(OutFile, 'will auch immer artig sein!');
    end;
    7: begin
        Writeln(OutFile, 'O Tannenbaum, o Tannenbaum,');
        Writeln(OutFile, 'wie grün sind deine Blätter.');
        Writeln(OutFile, 'Du grünst nicht nur zur Sommerzeit,');
        Writeln(OutFile, 'nein auch im Winter, wenn es schneit.');
        Writeln(OutFile, 'O Tannenbaum, o Tannenbaum,');
        Writeln(OutFile, 'wie grün sind deine Blätter!');
    end;
    8: begin
        Writeln(OutFile, 'Wenn Kekse auf dem Tische stehen');
        Writeln(OutFile, 'und Tannenkränze schmücken Kerzen,');
        Writeln(OutFile, 'wenn selbst die Lauten in sich gehen,');
        Writeln(OutFile, 'vergessen sind das Leid, die Schmerzen,');
        Writeln(OutFile, 'wenn kleine Gaben uns entzücken');
        Writeln(OutFile, 'und Nikolaus die Stiefel füllt,');
        Writeln(OutFile, 'wenn Eiszapfen die Fenster schmücken');
        Writeln(OutFile, 'und weiße Pracht das Land verhüllt,');
        Writeln(OutFile, 'das ist die Zeit, die jeder kennt,');
        Writeln(OutFile, 'der unvergleichliche Advent.');
    end;
    9: begin
        Writeln(OutFile, 'Der Tannenwald ist nicht nur schön,');
        Writeln(OutFile, 'er stärkt auch das Immunsystem.');
        Writeln(OutFile, 'Und schon ein Baum im Domizil,');
        Writeln(OutFile, 'gibt Weihnachten und Neujahr Stil!');
    end;
    12: begin
        Writeln(OutFile, 'Zeit für Liebe und Gefühl,');
        Writeln(OutFile, 'heute bleibt’s nur draußen kühl.');
        Writeln(OutFile, 'Kerzenschein und Plätzchenduft,');
        Writeln(OutFile, 'Weihnachten liegt in der Luft.');
    end;
    13: begin
        Writeln(OutFile, 'Der Weihnachtstisch ist öd und leer,');
        Writeln(OutFile, 'die Kinder schauen träg daher.');
        Writeln(OutFile, 'Da lässt der Vater einen krachen,');
        Writeln(OutFile, 'die Kinder fangen an zu lachen.');
        Writeln(OutFile, 'So kann man auch mit kleinen Sachen,');
        Writeln(OutFile, 'den Kindern eine Freude machen.');
    end;
    14: begin
        Writeln(OutFile, 'Bald kommt das liebe Weihnachtsfest');
        Writeln(OutFile, 'auf das ich mich so freue,');
        Writeln(OutFile, 'zuerst sind alle sehr gestresst,');
        Writeln(OutFile, 'ein jedes Jahr auf`s Neue.');
        Writeln(OutFile, 'Doch wie verwandelt nach der Andacht,');
        Writeln(OutFile, 'wenn alle singen: “Stille Nacht”');
        Writeln(OutFile, 'wenn brennen alle Kerzen,');
        Writeln(OutFile, 'dann wird`s ganz still im Herzen.');
    end;
    15: begin
        Writeln(OutFile, 'Man sieht die Adventskränze weit und breit,');
        Writeln(OutFile, 'und weiß genau bis Weihnachten ist nicht mehr weit.');
        Writeln(OutFile, 'Ihr werdet sehen wie schnell die Tage verrinnen');
        Writeln(OutFile, 'Ihr werdet sehen wie schnell die Tage verrinnen');
    end;
    16: begin
        Writeln(OutFile, 'Wenn Glöcklein dröhnen, Engel brüllen,');
        Writeln(OutFile, 'mit Radau die Ohren füllen,');
        Writeln(OutFile, 'wenn Radau ist überall,');
        Writeln(OutFile, 'und es mir mit lautem Knall');
        Writeln(OutFile, 'Flocken auf die Rübe schneit,');
        Writeln(OutFile, 'heissa dann ist Weihnachtszeit!');
    end;
    19: begin
        Writeln(OutFile, 'Die Feuerwehr, die Feuerwehr,');
        Writeln(OutFile, 'die hat es zum Advent sehr schwer.');
        Writeln(OutFile, 'Sobald die ersten Kerzen brennen,');
        Writeln(OutFile, 'sieht man auch schon den Löschtrupp rennen.');
        Writeln(OutFile, 'Sirenen stehen selten still,');
        Writeln(OutFile, 'ja der Advent macht, was er will.');
    end;
    20: begin
        Writeln(OutFile, 'Kerzenschein und Tannenduft,');
        Writeln(OutFile, 'rote Backen, kalte Luft.');
        Writeln(OutFile, 'Glockenläuten, Kinderlachen,');
        Writeln(OutFile, 'Äpfel, Wein und süße Sachen.');
        Writeln(OutFile, 'Das Christkind ist schon nicht mehr weit,');
        Writeln(OutFile, 'es beschert uns eine glückliche Zeit.');
        Writeln(OutFile, 'Lasst uns freuen und besinnlich sein,');
        Writeln(OutFile, 'der Zauber der Weihnacht');
        Writeln(OutFile, 'macht uns alle wieder klein.');
    end;
    21: begin
        Writeln(OutFile, 'Am Tannenbaum ein Liedchen singen,');
        Writeln(OutFile, 'was wird der Weihnachtsabend bringen,');
        Writeln(OutFile, 'für jene, die zu viel verloren,');
        Writeln(OutFile, 'vom Glück waren sie nicht auserkoren.');
        Writeln(OutFile, 'Hell, so leuchten unsere Kerzen,');
        Writeln(OutFile, 'lasst die Wärme in die Herzen.');
        Writeln(OutFile, 'Ladet einsame Menschen zu Euch ein,');
        Writeln(OutFile, 'sie sollten heute nicht alleine sein.');
    end;
    22: begin
        Writeln(OutFile, 'Leise rieselt der Schnee');
        Writeln(OutFile, 'still und starr liegt der See,');
        Writeln(OutFile, 'weihnachtlich glänzet der Wald:');
        Writeln(OutFile, 'Freue dich, Christkind kommt bald!');
    end;
    23: begin
        Writeln(OutFile, 'Morgen, Kinder, wird’s was geben,');
        Writeln(OutFile, 'morgen werden wir uns freun!');
        Writeln(OutFile, 'Welch ein Jubel, welch ein Leben');
        Writeln(OutFile, 'wird in unserm Hause sein!');
        Writeln(OutFile, 'Einmal werden wir noch wach,');
        Writeln(OutFile, 'heißa dann ist Weihnachtstag!');
    end;
  end;
  Close(OutFile);
end;

{ Create picture file for actual day }

procedure CreatePicFile(OutFilename: string; Day: Integer);

// The pictures are located in the "res" directory, but with the first 16 bytes missing. Picture viewers will not
// recognize any valid format and the user will not be able to view the picture - until the door is opened and a
// new file, where the missing bytes have been added, has been created.

const
  PicBytes: array[1..24] of string = (
    '', '',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 01 2C',
    '', '', '', '', '',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 48',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 60',
    '', '', '', '', '',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 B4',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 60',
    '', '', '', '', '',
    'FF D8 FF E0 00 10 4A 46 49 46 00 01 01 01 00 B4'
  );

var
  N: Byte;
  I: Integer;
  InFilename, Bytes: string;
  InFile, OutFile: file of Byte;

begin
  InFilename := GetCurrentDir + '/res/' + IntToStr(Day) + '.dat';
  Assign(InFile, InFilename); Reset(InFile);
  Assign(OutFile, OutFilename); Rewrite(OutFile);
  Bytes := StringReplace(PicBytes[Day], ' ', '', [rfReplaceAll]);
  // Write the 16 missing bytes from the hard coded array
  for I := 0 to 15 do begin
    Write(OutFile, HexToDec(Copy(Bytes, 2 * I + 1, 2)));
  end;
  // Write the resting bytes from the file in the "res" directory
  while not EoF(InFile) do begin
    Read(InFile, N);
    Write(OutFile, N);
  end;
  Close(InFile); Close(OutFile);
end;

{ Show Advent saying text for given day }

procedure ShowText(Filename: string);

// The text is read from the file, that has been created in the "texte" directory
// and displayed horizontally and vertically centered in the TMemo object

var
  N, E, I: Integer;
  Line: string;
  Lines: array of string;
  InFile: Text;

begin
  // Read text from file
  N := 0; SetLength(Lines, 0);
  Assign(InFile, Filename); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Lines, N);
      Lines[N - 1] := Trim(Line);
    end;
  end;
  // Display the text (fill in the TMemo object)
  E := (11 - Length(Lines)) div 2;                                             // number of leading blank lines to vertically center the text
  fAdvent.edMess.Visible := True;
  fAdvent.edMess.Lines.Clear;
  for I := 1 to E do
    fAdvent.edMess.Lines.Add(' ');
  for I := 0 to Length(Lines) - 1 do
    fAdvent.edMess.Lines.Add(Lines[I]);
  Close(InFile);
end;

{ Show Advent picture for given day }

procedure ShowPic(Filename: string);

// The picture is loaded from the file, that has been created in the "bilder" directory
// To make it better visible the calendar background picture is hidden
// As objects like statictexts are not covered up by pictures, the day number statictexts
// at the center of the calendar (where the picture is placed) should be hidden, too.

begin
  // Display the picture (from file)
  fAdvent.imPicture.Picture.LoadFromFile(Filename);
  fAdvent.imPicture.Visible := True;
  // Hide background (to make picture better visible)
  fAdvent.imBackground.Visible := False;
  // Hide some day number statictexts (looks nicer...)
  fAdvent.stDoor2.Visible := False;  fAdvent.stDoor6.Visible := False;  fAdvent.stDoor18.Visible := False;
  fAdvent.stDoor20.Visible := False; fAdvent.stDoor21.Visible := False; fAdvent.stDoor24.Visible := False;
end;

{ User click on a door, that contains an Advent saying }

procedure TextDoor(Doors: array of TShape; MM, DD, Day: Integer);

var
  Filename: string;

begin
  Filename := GetCurrentDir + '/texte/tag' + IntToStr(Day) + '.txt'; DoDirSeparators(Filename);
  // If a file for this door exists (has been created the corr. day), show the saying
  if FileExists(Filename) then
    ShowText(Filename)
  // If there isn't a file, create one IF it's the actual day's door that has been clicked
  else begin
    if (MM = 12) and (DD = Day) then begin
      CreateTextFile(Filename, Day);
      Doors[Day - 1].Brush.Color := clLime;
      ShowText(Filename);
    end;
  end;
end;

{ User click on a door, that contains an Advent picture }

procedure PicDoor(Doors: array of TShape; MM, DD, Day: Integer);

var
  Filename: string;

begin
  Filename := GetCurrentDir + '/bilder/tag' + IntToStr(Day) + '.jpg'; DoDirSeparators(Filename);
  // If a file for this door exists (has been created the corr. day), show the picture
  if FileExists(Filename) then
    ShowPic(Filename)
  // If there isn't a file, create one IF it's the actual day's door that has been clicked
  else begin
    if (MM = 12) and (DD = Day) then begin
      CreatePicFile(Filename, Day);
      Doors[Day - 1].Brush.Color := clLime;
      ShowPic(Filename);
    end;
  end;
end;

{**********}
{ TfAdvent }
{**********}

{ Application start: Initialization }

procedure TfAdvent.FormCreate(Sender: TObject);

var
  I: Integer;
  Dir, F1, F2: string;

begin
  DecodeDate(Date, iYear, iMonth, iDay);
  // Create arrays with shapes and statictexts
  shDoors[0] := shDoor1; shDoors[1] := shDoor2; shDoors[2] := shDoor3; shDoors[3] := shDoor4;
  shDoors[4] := shDoor5; shDoors[5] := shDoor6; shDoors[6] := shDoor7; shDoors[7] := shDoor8;
  shDoors[8] := shDoor9; shDoors[9] := shDoor10; shDoors[10] := shDoor11; shDoors[11] := shDoor12;
  shDoors[12] := shDoor13; shDoors[13] := shDoor14; shDoors[14] := shDoor15; shDoors[15] := shDoor16;
  shDoors[16] := shDoor17; shDoors[17] := shDoor18; shDoors[18] := shDoor19; shDoors[19] := shDoor20;
  shDoors[20] := shDoor21; shDoors[21] := shDoor22; shDoors[22] := shDoor23; shDoors[23] := shDoor24;
  stDoors[0] := stDoor1; stDoors[1] := stDoor2; stDoors[2] := stDoor3; stDoors[3] := stDoor4;
  stDoors[4] := stDoor5; stDoors[5] := stDoor6; stDoors[6] := stDoor7; stDoors[7] := stDoor8;
  stDoors[8] := stDoor9; stDoors[9] := stDoor10; stDoors[10] := stDoor11; stDoors[11] := stDoor12;
  stDoors[12] := stDoor13; stDoors[13] := stDoor14; stDoors[14] := stDoor15; stDoors[15] := stDoor16;
  stDoors[16] := stDoor17; stDoors[17] := stDoor18; stDoors[18] := stDoor19; stDoors[19] := stDoor20;
  stDoors[20] := stDoor21; stDoors[21] := stDoor22; stDoors[22] := stDoor23; stDoors[23] := stDoor24;
  SetHints(stDoors, mSettingsHints.Checked);
  // Create "texte" and "bilder" subdirectories
  Dir := GetCurrentDir + '/texte'; DoDirSeparators(Dir);
  if not DirectoryExists(Dir) then
    CreateDir(Dir);
  Dir := GetCurrentDir + '/bilder'; DoDirSeparators(Dir);
  if not DirectoryExists(Dir) then
    CreateDir(Dir);
  // Set color and hint of the 24 doors
  for I := 1 to 24 do begin
    F1 := GetCurrentDir + '/texte/tag' + IntToStr(I) + '.txt'; DoDirSeparators(F1);
    F2 := GetCurrentDir + '/bilder/tag' + IntToStr(I) + '.jpg'; DoDirSeparators(F2);
    // If there is a file for a door, door has already been opened (green shape)
    if FileExists(F1) or FileExists(F2) then begin
      shDoors[I - 1].Brush.Color := clLime;
      stDoors[I - 1].Enabled := True;
      stDoors[I - 1].Hint := 'Klicken um zu schauen, was sich hinter dieser Tür befindet';
    end
    // If there isn't a file, door are still closed
    else begin
      // If actual date precedes date of the door, door may be opened (yellow shape)
      if (iMonth < 12) or ((iMonth = 12) and (iDay <= I)) then begin
        shDoors[I - 1].Brush.Color := clYellow;
        stDoors[I - 1].Enabled := True;
        stDoors[I - 1].Hint := 'Klicken um diese Tür zu öffnen';
      end
      // If actual date succeeds date of the door, door may never be opened (red shape)
      else begin
        shDoors[I - 1].Brush.Color := clRed;
        stDoors[I - 1].Enabled := False;
        stDoors[I - 1].Hint := 'Diese Tür kann leider nicht mehr geöffnet werden...';
      end;
    end;
  end;
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfAdvent.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Hints anzeigen": Toggle show resp. hide the hints }

procedure TfAdvent.mSettingsHintsClick(Sender: TObject);

begin
  if mSettingsHints.Checked then
    mSettingsHints.Checked := False
  else
    mSettingsHints.Checked := True;
  SetHints(stDoors, mSettingsHints.Checked);
end;

{ Menu item "Hilfe > Hilfe": Display short application help text }

procedure TfAdvent.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Um eine Tür zu öffnen oder das, was sich dahinter befindet anzuschauen, bitte einen der grünen oder ';
  S += 'gelben Kreise doppelklicken. Die Türen können nur an dem entsprechenden Tag geöffnet werden. Türen, die man ';
  S += 'vergaß zu öffnen, bleiben für immer zu (roter Kreis). Um einen Text oder ein Bild zu schließen, einfach ';
  S += 'darin klicken. Der Inhalt hinter den geöffneten Türen wird als Dateien in den Ordnern "texte" und "bilder" abgelegt.';
  MessageDlg('Hilfe zu "Adventskalender"', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfAdvent.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Elektronischer Adventskalender in Free Pascal.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, November 2022.';
  MessageDlg('Über "Adventskalender"', S, mtInformation, [mbOK], 0);
end;

{ Days statictexts double-clicked by user: Open door, show content (or do nothing) }

procedure TfAdvent.stDoor1DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 1);
end;

procedure TfAdvent.stDoor2DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 2);
end;

procedure TfAdvent.stDoor3DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 3);
end;

procedure TfAdvent.stDoor4DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 4);
end;

procedure TfAdvent.stDoor5DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 5);
end;

procedure TfAdvent.stDoor6DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 6);
end;

procedure TfAdvent.stDoor7DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 7);
end;

procedure TfAdvent.stDoor8DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 8);
end;

procedure TfAdvent.stDoor9DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 9);
end;

procedure TfAdvent.stDoor10DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 10);
end;

procedure TfAdvent.stDoor11DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 11);
end;

procedure TfAdvent.stDoor12DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 12);
end;

procedure TfAdvent.stDoor13DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 13);
end;

procedure TfAdvent.stDoor14DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 14);
end;

procedure TfAdvent.stDoor15DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 15);
end;

procedure TfAdvent.stDoor16DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 16);
end;

procedure TfAdvent.stDoor17DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 17);
end;

procedure TfAdvent.stDoor18DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 18);
end;

procedure TfAdvent.stDoor19DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 19);
end;

procedure TfAdvent.stDoor20DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 20);
end;

procedure TfAdvent.stDoor21DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 21);
end;

procedure TfAdvent.stDoor22DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 22);
end;

procedure TfAdvent.stDoor23DblClick(Sender: TObject);

begin
  TextDoor(shDoors, iMonth, iDay, 23);
end;

procedure TfAdvent.stDoor24DblClick(Sender: TObject);

begin
  PicDoor(shDoors, iMonth, iDay, 24);
end;

{ Advent saying TMemo object clicked by user: Hide the object }

procedure TfAdvent.edMessClick(Sender: TObject);

begin
  edMess.Lines.Clear;
  edMess.Visible := False;
end;

{ Advent picture clicked by user: Hide the picture }

procedure TfAdvent.imPictureClick(Sender: TObject);

begin
  imPicture.Picture.Clear;
  imPicture.Visible := False;
  // Make the background picture visible again
  fAdvent.imBackground.Visible := True;
  // Make the statictexts, hidden when showing the picture, visible again
  fAdvent.stDoor2.Visible := True;  fAdvent.stDoor6.Visible := True;  fAdvent.stDoor18.Visible := True;
  fAdvent.stDoor20.Visible := True; fAdvent.stDoor21.Visible := True; fAdvent.stDoor24.Visible := True;
end;

end.

