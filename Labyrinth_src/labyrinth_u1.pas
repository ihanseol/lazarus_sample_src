{*******************************************}
{* Main unit for the Labyrinth application *}
{*******************************************}

unit labyrinth_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, LCLType, IniFiles, labyrinth_u2;

type
  TLabyrinth = array[1..55, 1..100] of Char;
  TGridPos = record
    Row, Col: Integer;
  end;
  TEntries = array[1..10] of TGridPos;
  TConnections = array of TGridPos;
  TEntryLabels = array[1..10] of TLabel;
  {*************}
  { TfLabyrinth }
  {*************}
  TfLabyrinth = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileExit: TMenuItem;
    mSettings, mSettingsLabyrinth, mSettingsShow: TMenuItem;
    mSettingsLabyrinth1, mSettingsLabyrinth2, mSettingsLabyrinth3, mSettingsLabyrinthR: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    imLabyrinth: TImage;
    laEntry1, laEntry2, laEntry3, laEntry4, laEntry5: TLabel;
    laEntry6, laEntry7, laEntry8, laEntry9, laEntry10: TLabel;
    edHelp: TEdit;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsLabyrinth1Click(Sender: TObject);
    procedure mSettingsLabyrinth2Click(Sender: TObject);
    procedure mSettingsLabyrinth3Click(Sender: TObject);
    procedure mSettingsLabyrinthRClick(Sender: TObject);
    procedure mSettingsShowClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure laEntry10Click(Sender: TObject);
    procedure laEntry1Click(Sender: TObject);
    procedure laEntry2Click(Sender: TObject);
    procedure laEntry3Click(Sender: TObject);
    procedure laEntry4Click(Sender: TObject);
    procedure laEntry5Click(Sender: TObject);
    procedure laEntry6Click(Sender: TObject);
    procedure laEntry7Click(Sender: TObject);
    procedure laEntry8Click(Sender: TObject);
    procedure laEntry9Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word);
  private
    iImageWidth, iImageHeight, iLabyrinthSel, iLabyrinth, iUserEntry, iEntry, iExit, iRow, iCol: Integer;
    bStarted, bEnded: Boolean;
    iColour: TColor;
    aLabyrinth: TLabyrinth;
    aEntries: TEntries;
    laEntries: TEntryLabels;
    Bitmap: TBitmap;
  end;

const
  NLabyrinths = 3;
  clOrange = $000080FF; clMauve = $00FF8080;
  Colours: array['a'..'t'] of TColor = (
    clPurple, clMauve, clPurple, clMauve, clPurple, clMauve, clFuchsia, clBlue, clBlue, clFuchsia,
    clWhite, clGreen, clGray, clTeal, clAqua, clOrange, clSkyBlue, clLime, clSilver, clOlive
  );

var
  fLabyrinth: TfLabyrinth;
  ConfFile: TINIFile;

implementation

{$R *.lfm}

{ Clean labyrinth by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fLabyrinth.imLabyrinth.Picture.Bitmap.Canvas.Clear;
  fLabyrinth.imLabyrinth.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fLabyrinth.imLabyrinth.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Read labyrinth data from text file }

procedure ReadLabyrinth(L: Integer; out Labyrinth: TLabyrinth);

var
  Row, Col: Integer;
  Filename, Line: string;
  InFile: Text;

begin
  Filename := 'labyrinth' + IntToStr(L) + '.txt';
  Assign(InFile, Filename); Reset(InFile);
  Row := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);                                                                // one file line = data for 1 grid row
    Inc(Row);
    for Col := 1 to 100 do begin
      Labyrinth[Row, Col] := Line[Col];
    end;
  end;
  Close(InFile);
end;

{ Mouse click on one of the 10 labyrinth entry labels }

procedure EntryClicked(EntryClicked: Integer; var Entry: Integer; var Entries: TEntryLabels);

begin
  if Entry <> 0 then begin
    Entries[Entry].Color := clForm;                                                      // reset label color of previously selected entry
    Entries[Entry].Font.Color := clDefault;
  end;
  Entries[EntryClicked].Color := clRed;                                                  // set label color of actually selected entry
  Entries[EntryClicked].Font.Color := clWhite;
  Entry := EntryClicked;
  fLabyrinth.edHelp.Text := 'Push the Start button to begin';
end;

{*************}
{ TfLabyrinth }
{*************}

{ Application start: Initialisation }

procedure TfLabyrinth.FormCreate(Sender: TObject);

begin
  iImageWidth := imLabyrinth.Width; iImageHeight := imLabyrinth.Height;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the labyrinth image component (the drawing surface)
  imLabyrinth.Picture.Graphic := Bitmap;
  // Create array with "entry" labels
  laEntries[1] := laEntry1; laEntries[2] := laEntry2; laEntries[3] := laEntry3; laEntries[4] := laEntry4; laEntries[5]  := laEntry5;
  laEntries[6] := laEntry6; laEntries[7] := laEntry7; laEntries[8] := laEntry8; laEntries[9] := laEntry9; laEntries[10] := laEntry10;
  // Create a new labyrinth
  Randomize;
  iLabyrinthSel := 1; mFileNew.Click;
end;

{ Menu item "File > New": Create a new labyrinth }

procedure TfLabyrinth.mFileNewClick(Sender: TObject);

const
  // Entry labels for flipped labyrinths
  HFlip: array[1..10] of Integer = (
    3, 2, 1, 6, 5, 4, 9, 10, 7, 8
  );
  VFlip: array[1..10] of Integer = (
    4, 5, 6, 1, 2, 3, 8, 7, 10, 9
  );
  HVFlip: array[1..10] of Integer = (
    6, 5, 4, 3, 2, 1, 10, 9, 8, 7
  );

var
  NJunctions, NConnections, Flip, Keep, L, T, P, I, J, K: Integer;
  Filename, Junction, Path, Connection, S: string;
  Temp: Char;
  Reset: Boolean;
  Colour: TColor;
  Junctions: array of string;
  JunctionChars: array of Char;
  Connections: TConnections;
  AllConnections: array[1..10] of TConnections;
  ConnectionJunctions: array[1..10] of Char;

begin
  // Clean the labyrinth
  GraphClean(iImageWidth, iImageHeight);
  iUserEntry := 0; bStarted := False; bEnded := False;
  for I := 1 to 10 do begin
    laEntries[I].Color := clForm;
    laEntries[I].Font.Color := clDefault;
  end;
  // Load random or selected labyrinth
  if iLabyrinthSel = 0 then
    iLabyrinth := Random(NLabyrinths) + 1
  else
    iLabyrinth := iLabyrinthSel;
  ReadLabyrinth(iLabyrinth, aLabyrinth);
  // Open configuration file for selected labyrinth
  ConfFile.Free; Filename := 'labyrinth' + IntToStr(iLabyrinth) + '.cnf';
  ConfFile := TINIFile.Create(Filename);
  // Choose random entry and exit
  iEntry := Random(10) + 1;
  repeat
    iExit := Random(10) + 1;
  until iExit <> iEntry;
  if iEntry > iExit then begin                                                           // lowest number as entry point
    P := iEntry; iEntry := iExit; iExit := P;
  end;
  // Read path (for actual entry/exit path) from configuration file
  Path := 'Path_' + IntToStr(iEntry) + '_' + IntToStr(iExit);
  NJunctions := StrToInt(ConfFile.ReadString(Path, 'Junctions', ''));
  SetLength(Junctions, NJunctions); SetLength(JunctionChars, NJunctions);
  for I := 1 to NJunctions do begin
    Junctions[I - 1] := ConfFile.ReadString(Path, 'Junction' + IntToStr(I), '');
    if LeftStr(Junctions[I - 1], 1) > RightStr(Junctions[I - 1], 1) then                 // lowest character first (for correct "Junctions" reading)
      Junctions[I - 1] := RightStr(Junctions[I - 1], 1) + LeftStr(Junctions[I - 1], 1);
  end;
  // Read junction characters (for actual path junctions) from configuration file
  for I := 0 to NJunctions - 1 do begin
    Junction := 'Junction_' + Junctions[I];
    S := ConfFile.ReadString(Junction, 'Character', '');
    JunctionChars[I] := S[1];
  end;
  // Remove all labyrinth junctions, except those being part of the path
  for I := 1 to 55 do begin
    for J := 1 to 100 do begin
      if aLabyrinth[I, J] in ['1'..'9', 'U'..'Z'] then begin
        Reset := True;
        for K := 0 to NJunctions - 1 do begin
          if aLabyrinth[I, J] = JunctionChars[K] then
            Reset := False;
        end;
        if Reset then
          aLabyrinth[I, J] := '.';
      end;
    end;
  end;
  // Read all connection data from config file
  for I := 1 to 10 do begin
    Connection := 'Connection_' + IntToStr(I);
    S := ConfFile.ReadString(Connection, 'Junction', '');
    ConnectionJunctions[I] := S[1];
    NConnections := StrToInt(ConfFile.ReadString(Connection, 'Connections', ''));
    SetLength(Connections, NConnections);
    for J := 1 to NConnections do begin
      S := ConfFile.ReadString(Connection, 'Connection' + IntToStr(J), '');
      P := Pos(',', S);
      Connections[J - 1].Row := StrToInt(LeftStr(S, P - 1));
      Connections[J - 1].Col := StrToInt(RightStr(S, Length(S) - P));
    end;
    AllConnections[I] := Connections;
  end;
  // Adapt labyrinth connections
  for I := 1 to 10 do begin
    Connections := AllConnections[I];
    // For entry and exit parts, keep 1 single (randomly selected) connection
    if (I = iEntry) or (i = iExit) then begin
      Keep := Random(Length(Connections));
      for J := 0 to Length(Connections) - 1 do begin
        if J <> Keep then
          aLabyrinth[Connections[J].Row, Connections[J].Col] := '.';
      end;
    end
    // For other parts, remove or keep all connections
    else begin
      Reset := False;
      for J := 0 to Length(Junctions) - 1 do begin
        if (ConnectionJunctions[I] = Junctions[J][1]) or (ConnectionJunctions[I] = Junctions[J][2]) then
          // Connections communicating with actual entry-exit path have to be removed
          Reset := True;
      end;
      for J := 0 to Length(Connections) - 1 do begin
        if Reset then
          aLabyrinth[Connections[J].Row, Connections[J].Col] := '.';
      end;
    end;
  end;
  // Flip labyrinth (or not)
  Flip := Random(4);
  if (Flip = 1) or (Flip = 3) then begin
    // Horizontal or horizontal+vertical flip
    for I := 1 to 55 do begin
      for J := 1 to 50 do begin
        Temp := aLabyrinth[I, J];
        aLabyrinth[I, J] := aLabyrinth[I, 100 - J + 1]; aLabyrinth[I, 100 - J + 1] := Temp;
      end;
    end;
  end;
  if (Flip = 2) or (Flip = 3) then begin
    // Vertical or horizontal+vertical flip
    for I := 1 to 27 do begin
      for J := 1 to 100 do begin
        Temp := aLabyrinth[I, J];
        aLabyrinth[I, J] := aLabyrinth[55 - I + 1, J]; aLabyrinth[55 - I + 1, J] := Temp;
      end;
    end;
  end;
  // Draw the labyrinth
  for I := 1 to 55 do begin
    for J := 1 to 100 do begin
      if mSettingsShow.Checked then begin
        // Show labyrinth parts, junctions and connections using different colors
        if aLabyrinth[I, J] in ['a' .. 't'] then
          Colour := Colours[aLabyrinth[I, J]]
        else if aLabyrinth[I, J] in ['A' .. 'T'] then
          Colour := clYellow
        else if aLabyrinth[I, J] in ['1' .. '9', 'U' .. 'Z'] then
          Colour := clRed
        else
          Colour := clBlack;
      end
      else begin
        // Display whole labyrinth in white
        if aLabyrinth[I, J] = '.' then
          Colour := clBlack
        else
          Colour := clWhite;
      end;
      imLabyrinth.Picture.Bitmap.Canvas.Pen.Color := Colour;
      imLabyrinth.Picture.Bitmap.Canvas.Brush.Color := Colour;
      imLabyrinth.Picture.Bitmap.Canvas.Rectangle((J - 1) * 10, (I - 1) * 10, J * 10, I * 10);
    end;
  end;
  // Get entry point coordinates
  K := 0;
  for J := 1 to 100 do begin
    if aLabyrinth[1, J] <> '.' then begin
      Inc(K); aEntries[K].Row := 1; aEntries[K].Col := J;
    end;
  end;
  for J := 1 to 100 do begin
    if aLabyrinth[55, J] <> '.' then begin
      Inc(K); aEntries[K].Row := 55; aEntries[K].Col := J;
    end;
  end;
  for I := 1 to 55 do begin
    if aLabyrinth[I, 1] <> '.' then begin
      Inc(K); aEntries[K].Row := I; aEntries[K].Col := 1;
    end;
  end;
  for I := 1 to 55 do begin
    if aLabyrinth[I, 100] <> '.' then begin
      Inc(K); aEntries[K].Row := I; aEntries[K].Col := 100;
    end;
  end;
  // Move entry labels to correct (proper) position
  for I := 1 to 10 do begin
    if I <= 3 then
      J := -1
    else if I <=6 then
      J := 1;
    if I <= 3 then begin
      L := 10 * (aEntries[I].Col - 1) + 40;
      T := 10 * (aEntries[I].Row - 1) + 15;
    end
    else if I <= 6 then begin
      L := 10 * (aEntries[I].Col - 1) + 40;
      T := 10 * (aEntries[I].Row - 1) + 65;
    end
    else if I <= 8 then begin
      L := 10 * (aEntries[I].Col - 1) + 15;
      T := 10 * (aEntries[I].Row - 1) + 40;
    end
    else begin
      L := 10 * (aEntries[I].Col - 1) + 65;
      T := 10 * (aEntries[I].Row - 1) + 40;
    end;
    laEntries[I].Left := L; laEntries[I].Top  := T;
  end;
  // Correct entry and exit (if there was a flip)
  case Flip of
    1: begin
         iEntry := HFlip[iEntry]; iExit := HFlip[iExit];
       end;
    2: begin
        iEntry := VFlip[iEntry]; iExit := VFlip[iExit];
       end;
    3: begin
        iEntry := HVFlip[iEntry]; iExit := HVFlip[iExit];
       end;
  end;
  // Display help message
  edHelp.Visible := True;
  edHelp.Text := 'Select an entry (by clicking its number)';
  // Set button caption
  btStart.Enabled := True; btStart.Caption := 'Start';
end;

{ Menu item "File > Exit": Exit the application }

procedure TfLabyrinth.mFileExitClick(Sender: TObject);

begin
  ConfFile.Free;
  Close;
end;

{ Menu items "Settings > Labyrinth > ...": Select labyrinth to be used }

procedure TfLabyrinth.mSettingsLabyrinth1Click(Sender: TObject);

begin
  mSettingsLabyrinth1.Checked := True;  mSettingsLabyrinth2.Checked := False;
  mSettingsLabyrinth3.Checked := False; mSettingsLabyrinthR.Checked := False;
  iLabyrinthSel := 1;
end;

procedure TfLabyrinth.mSettingsLabyrinth2Click(Sender: TObject);

begin
  mSettingsLabyrinth1.Checked := False; mSettingsLabyrinth2.Checked := True;
  mSettingsLabyrinth3.Checked := False; mSettingsLabyrinthR.Checked := False;
  iLabyrinthSel := 2;
end;

procedure TfLabyrinth.mSettingsLabyrinth3Click(Sender: TObject);

begin
  mSettingsLabyrinth1.Checked := False; mSettingsLabyrinth2.Checked := False;
  mSettingsLabyrinth3.Checked := True;  mSettingsLabyrinthR.Checked := False;
  iLabyrinthSel := 3;
end;

procedure TfLabyrinth.mSettingsLabyrinthRClick(Sender: TObject);

begin
  mSettingsLabyrinth1.Checked := False; mSettingsLabyrinth2.Checked := False;
  mSettingsLabyrinth3.Checked := False; mSettingsLabyrinthR.Checked := True;
  iLabyrinthSel := 0;
end;

{ Menu item "Settings > Show labyrinth": Toggle show/don't show labyrinth }

procedure TfLabyrinth.mSettingsShowClick(Sender: TObject);

begin
  if mSettingsShow.Checked then
    mSettingsShow.Checked := False
  else
    mSettingsShow.Checked := True;
end;

{ Menu item "Help > Help": Dispaly program help }

procedure TfLabyrinth.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Dispaly program about }

procedure TfLabyrinth.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Game: Find your way through the labyrinth!' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, July-October 2019.';
  MessageDlg('About "Labyrinth"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Reset": Start game resp. reset entry selection }

procedure TfLabyrinth.btStartClick(Sender: TObject);

begin
  // Button "Start": Start the game
  if btStart.Caption = 'Start' then begin
    if iUserEntry <> 0 then begin
      iRow := aEntries[iUserEntry].Row; iCol := aEntries[iUserEntry].Col;                // start position
      if mSettingsShow.Checked then
        iColour := Colours[LowerCase(aLabyrinth[iRow, iCol])]                            // remember color used
      else
        iColour := clWhite;
      // Draw the red square (being moved through the labyrinth)
      imLabyrinth.Picture.Bitmap.Canvas.Pen.Color := clRed;
      imLabyrinth.Picture.Bitmap.Canvas.Brush.Color := clRed;
      imLabyrinth.Picture.Bitmap.Canvas.Rectangle((iCol - 1) * 10, (iRow - 1) * 10, iCol * 10, iRow * 10);
      // Display help message
      edHelp.Text := 'Move the red square through the labyrinth, using the arrow keys';
      // Set button caption
      bStarted := True; btStart.Caption := 'Reset';
    end
    else
      MessageDlg('Labyrinth', 'Error: No entry selected!', mtError, [mbOK], 0);
  end
  // Button "Reset": Reset actual entry selection (and prepare for selection of another one)
  else begin
    laEntries[iUserEntry].Color := clForm;
    laEntries[iUserEntry].Font.Color := clDefault;
    // Redraw starting position with color saved before
    imLabyrinth.Picture.Bitmap.Canvas.Pen.Color := iColour;
    imLabyrinth.Picture.Bitmap.Canvas.Brush.Color := iColour;
    imLabyrinth.Picture.Bitmap.Canvas.Rectangle((iCol - 1) * 10, (iRow - 1) * 10, iCol * 10, iRow * 10);
    // Display help message
    edHelp.Visible := True;
    edHelp.Text := 'Select an entry (by clicking its number)';
    // Set button caption
    bStarted := False; iUserEntry := 0;
    btStart.Enabled := True; btStart.Caption := 'Start';
  end;
end;

{ Mouse click on labyrinth entry labels }

procedure TfLabyrinth.laEntry1Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(1, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry2Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(2, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry3Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(3, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry4Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(4, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry5Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(5, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry6Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(6, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry7Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(7, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry8Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(8, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry9Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(9, iUserEntry, laEntries);
end;

procedure TfLabyrinth.laEntry10Click(Sender: TObject);

begin
  if not bStarted then
    EntryClicked(10, iUserEntry, laEntries);
end;

{ Keyboard keystroke catching routine: Move 1 field through the labyrinth depending on key pressed }

procedure TfLabyrinth.FormKeyDown(Sender: TObject; var Key: Word);

var
  NRow, NCol, DRow, DCol, IX: Integer;
  ArrowKey: Boolean;

begin
  if bStarted and not bEnded then begin
    // Catch keyboard codes only if the game is running
    edHelp.Visible := False;                                                             // to avoid flickering and cursor movement in the edit field
    // Check which (arrow) key has been pressed
    ArrowKey := True;
    case Key of
      VK_LEFT:  begin DRow :=  0; DCol := -1; end;
      VK_RIGHT: begin DRow :=  0; DCol :=  1; end;
      VK_DOWN:  begin DRow :=  1; DCol :=  0; end;
      VK_UP:    begin DRow := -1; DCol :=  0; end;
      else ArrowKey := False;
    end;
    // If arrow key pressed, move the red square
    if ArrowKey then begin
      NRow := iRow + DRow; NCol := iCol + DCol;
      if (NRow >= 1) and (NRow <= 55) and (NCol >= 1) and (NCol <= 100) then begin
        if aLabyrinth[NRow, NCol] <> '.' then begin
          // Reset color of actual square (the position, that you now will leave)
          if mSettingsShow.Checked then begin
            // Colored display if "Show labyrinth" is selected
            if aLabyrinth[iRow, iCol] in ['a' .. 't'] then
              iColour := Colours[LowerCase(aLabyrinth[iRow, iCol])]
            else if aLabyrinth[iRow, iCol] in ['A' .. 'T'] then
              iColour := clYellow
            else if aLabyrinth[iRow, iCol] in ['1' .. '9', 'U' .. 'Z'] then
              iColour := clRed;
          end
          else begin
            // All white squares otherwise
            iColour := clWhite;
          end;
          imLabyrinth.Picture.Bitmap.Canvas.Pen.Color := iColour;
          imLabyrinth.Picture.Bitmap.Canvas.Brush.Color := iColour;
          imLabyrinth.Picture.Bitmap.Canvas.Rectangle((iCol - 1) * 10, (iRow - 1) * 10, iCol * 10, iRow * 10);
          // Display red square (the square that moves through the labyrinth) at the position that you now will go to
          iCol := NCol; iRow := NRow;
          imLabyrinth.Picture.Bitmap.Canvas.Pen.Color := clRed;
          imLabyrinth.Picture.Bitmap.Canvas.Brush.Color := clRed;
          imLabyrinth.Picture.Bitmap.Canvas.Rectangle((iCol - 1) * 10, (iRow - 1) * 10, iCol * 10, iRow * 10);
          // Exit has been reached: End of game
          if iUserEntry = iEntry then
            IX := iExit
          else
            IX := iEntry;
          if (iRow = aEntries[IX].Row) and (iCol = aEntries[IX].Col) then begin
            bEnded := True;
            laEntries[IX].Color := clRed;
            laEntries[IX].Font.Color := clWhite;
            MessageDlg('Game over', 'Great! You have found your way through the labyrinth!', mtInformation, [mbOK], 0);
            btStart.Caption := 'Start'; btStart.Enabled := False;
          end;
        end;
      end;
    end;
  end;
end;

end.

