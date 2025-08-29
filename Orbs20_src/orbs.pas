{************************************}
{* Main unit for Orbs20 application *}
{************************************}

unit orbs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, highscores;

type
  TColumn = record
    Orbs, Total: Integer;
    Available: Boolean;
  end;
  TColumns = array[0..3] of TColumn;
  {**********}
  { TfOrbs20 }
  {**********}
  TfOrbs20 = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mSettings, mSettingsTime, mSettingsTime1, mSettingsTime2, mSettingsTime4, mSettingsHi: TMenuItem;
    mHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edDescription: TMemo;
    shArrow1, shArrow2, shArrow3, shArrow4: TShape;
    shNextCol: TShape;
    shCol1, shCol2, shCol3, shCol4: TShape;
    Shape2, Shape3, Shape4, Shape5: TShape;
    edTotal1, edTotal2, edTotal3, edTotal4: TEdit;
    shColBust1, shColBust2, shColBust3, shColBust4: TShape;
    Label1, Label3, Label4, Label5, Label6: TLabel;
    edName, edHighscore, edScore, edTime: TEdit;
    shNextOrb: TShape;
    laNextOrb: TLabel;
    btStart: TButton;
    tiOrbs20: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsTime1Click(Sender: TObject);
    procedure mSettingsTime2Click(Sender: TObject);
    procedure mSettingsTime4Click(Sender: TObject);
    procedure mSettingsHiClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure shArrow1MouseDown(Sender: TObject);
    procedure shArrow2MouseDown(Sender: TObject);
    procedure shArrow3MouseDown(Sender: TObject);
    procedure shArrow4MouseDown(Sender: TObject);
    procedure edNameEditingDone(Sender: TObject);
    procedure tiOrbs20Timer(Sender: TObject);
  private
    iNextOrb, iNextCol, iTime, iT, iAvailable: Integer;
    bHighScore: Boolean;
    aColumns: TColumns;
    shOrbs: array[1..20] of TShape;
    laOrbs: array[1..20] of TLabel;
    edTotals: array[0..3] of TEdit;
    shColBust: array[0..3] of TShape;
  end;

const
  clOrange = $000080FF; clMauve = $00FF8080;
  Colors: array[1..9] of TColor = (
    clAqua, clMauve, clBlue, clGreen, clLime, clYellow, clOrange, clRed, clFuchsia
  );

var
  fOrbs20: TfOrbs20;

implementation

{ Read current user's highscore }

procedure ReadHighScore(var Name: string; out HighScore: Integer);

var
  LastHi, P: Integer;
  Line, Last: string;
  InFile: Text;

begin
  HighScore := -1;
  Assign(InFile, 'hi.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := Trim(Line);
    if Line <> '' then begin
      P := Pos(',', Line);
      if LeftStr(Line, P - 1) = 'last' then
        Last := Copy(Line, P + 1, Length(Line));
      if LeftStr(Line, P - 1) = Last then
        LastHi := StrToInt(Copy(Line, P + 1, Length(Line)));
      if LeftStr(Line, P - 1) = Name then
        HighScore := StrToInt(Copy(Line, P + 1, Length(Line)));
    end;
  end;
  if HighScore = -1 then begin
    // Start application (no user name passed) with user from last program run
    if Name = '' then begin
      Name := Last; HighScore := LastHi;
    end
    else
      HighScore := 0;
  end;
  Close(InFile);
end;

{ Write current user's highscore }

procedure WriteHighScore(Name: string; Score: Integer; var Done: Boolean);

var
  HighScore, N, P: Integer;
  Line: string;
  Found: Boolean;
  InFile, OutFile: Text;

begin
  // Create temporary file with updated highscore from actual highscore file
  Assign(InFile, 'hi.txt'); Reset(InFile);
  Assign(OutFile, 'hi.tmp'); Rewrite(OutFile);
  N := 0; Found := False;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := Trim(Line);
    if Line <> '' then begin
      P := Pos(',', Line);
      if LeftStr(Line, P - 1) = 'last' then
        Writeln(OutFile, 'last,' + Name)
      else begin
        Inc(N);
        if LeftStr(Line, P - 1) = Name then begin
          HighScore := StrToInt(Copy(Line, P + 1, Length(Line)));
          // Check if actual score > as user's (old) highscore
          if Score > HighScore then
            HighScore := Score;
          Writeln(OutFile, Name + ',' + IntToStr(HighScore));
          fOrbs20.edHighscore.Text := IntToStr(HighScore);
          Found := True;
        end
        else
          Writeln(OutFile, Line);
      end;
    end;
  end;
  // User not yet in list: Create new list entry
  if not Found then begin
    if N = 25 then
      // List (arbitrarily) limited to 25 users
      MessageDlg('Highscores', 'Cannot add highscore: Table full!', mtError, [mbOK], 0)
    else
      // New user entry
      Writeln(OutFile, Name + ',' + IntToStr(Score));
  end;
  Close(InFile); Close(OutFile);
  // Overwrite actual highscore file with (updated highscore) data from temorary file
  Assign(InFile, 'hi.tmp'); Reset(InFile);
  Assign(OutFile, 'hi.txt'); Rewrite(OutFile);
  while not EoF(InFile) do begin
    Readln(INFile, Line);
    Writeln(OutFile, Line);
  end;
  Close(InFile); Close(OutFile);
  Done := True;                                                                // update is done; no need to ask and save at application exit
  DeleteFile('hi.tmp');                                                        // delete the temporary file
end;

{ Fill highscore table (string grid) with values from actual highscores file }

procedure FillHighScores;

var
  N, L, I, J, P: Integer;
  Line, S: string;
  InFile: Text;

begin
  for I := 0 to 1 do begin
    for J := 1 to 25 do begin
      fHighScores.sgHighScores.Cells[I, J] := '';
    end;
  end;
  Assign(InFile, 'hi.txt'); Reset(InFile); N := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line); Line := Trim(Line);
    if Line <> '' then begin
      P := Pos(',', Line);
      if LeftStr(Line, P - 1) <> 'last' then begin
        Inc(N);
        fHighScores.sgHighScores.Cells[0, N] := LeftStr(Line, P - 1);
        fHighScores.sgHighScores.Cells[1, N] := Copy(Line, P + 1, Length(Line));
      end;
    end;
  end;
  Close(InFile);
  // Sort table on highscore value
  for I := 1 to N - 1 do begin
    for J := I + 1 to N do begin
      if StrToInt(fHighScores.sgHighScores.Cells[1, J]) > StrToInt(fHighScores.sgHighScores.Cells[1, I]) then begin
        S := fHighScores.sgHighScores.Cells[0, I]; fHighScores.sgHighScores.Cells[0, I] := fHighScores.sgHighScores.Cells[0, J]; fHighScores.sgHighScores.Cells[0, J] := S;
        S := fHighScores.sgHighScores.Cells[1, I]; fHighScores.sgHighScores.Cells[1, I] := fHighScores.sgHighScores.Cells[1, J]; fHighScores.sgHighScores.Cells[1, J] := S;
      end;
    end;
  end;
  // Align highscore values in grid cell
  for I := 1 to N do begin
    S := fHighScores.sgHighScores.Cells[1, I]; L := Length(S);
    for J := 1 to 6 - L do
      S := ' ' + S;
    fHighScores.sgHighScores.Cells[1, I] := S;
  end;
end;

{ Update highscore file from table (string grid) }

procedure UpdateHighScores;

var
  I, P: Integer;
  Last, User, UHigh, Line: string;
  Found: Boolean;
  FH: Text;

begin
  Assign(FH, 'hi.txt'); Reset(FH);
  User := fOrbs20.edName.Text; UHigh := '0'; Last := ''; Found := False;
  // Find name of last user (in file)
  while not EoF(FH) and (Last = '')do begin
    Readln(FH, Line); Line := Trim(Line);
    if Line <> '' then begin
      P := Pos(',', Line);
      if LeftStr(Line, P - 1) = 'last' then
        Last := Copy(Line, P + 1, Length(Line));
    end;
  end;
  Close(FH);
  // Find last user and actual user in highscore table (string grid)
  for I := 1 to 25 do begin
    if fHighScores.sgHighScores.Cells[0, I] = Last then
      Found := True;
    if fHighScores.sgHighScores.Cells[0, I] = User then
      UHigh := Trim(fHighScores.sgHighScores.Cells[1, I]);
  end;
  // If last user no more exists, start next program run with default user "Orb"
  if not Found then
    Last := 'Orb';
  // Overwrite highscores file with actual table data
  Rewrite(FH);
  Writeln(FH, 'last,' + Last);
  for I := 1 to 25 do begin
    if fHighScores.sgHighScores.Cells[0, I] <> '' then
      Writeln(FH, fHighScores.sgHighScores.Cells[0, I], ',', Trim(fHighScores.sgHighScores.Cells[1, I]));
  end;
  Close(FH);
  fOrbs20.edHighscore.Text := UHigh;                                           // if entry for current user has been deleted, this value will be 0 now
  MessageDlg('Highscores', 'Highscore table has been changed!', mtInformation, [mbOK], 0);
end;

{ Drop orb into selected column }

procedure DropOrb(NextOrb, NextCol: Integer; var Columns: TColumns; GTime: Integer; var T: Integer;
  var Totals: array of TEdit; var ColBust: array of TShape; var NAvailable: Integer; var HSDone: Boolean);

var
  Orb, I: Integer;

begin
  // Movee the "star" to column position
  fOrbs20.shNextCol.Left := 16 + NextCol * 175 + (25 - 12);
  // Update number of orbs and column total
  Columns[NextCol].Orbs += 1; Columns[NextCol].Total += NextOrb;
  // Determine index of orb shape to be used
  Orb := NextCol * 5 + Columns[NextCol].Orbs;
  // Place the orb shape (with its label) into the selected column and make it visible
  fOrbs20.shOrbs[Orb].Brush.Color := fOrbs20.shNextOrb.Brush.Color;
  fOrbs20.shOrbs[Orb].Left := 16 + NextCol * 175 + 5;
  fOrbs20.shOrbs[Orb].Top := fOrbs20.shCol1.Top + fOrbs20.shCol1.Height - (Columns[NextCol].Orbs) * 40 - 5;
  fOrbs20.laOrbs[Orb].Caption := fOrbs20.laNextOrb.Caption;
  fOrbs20.laOrbs[Orb].Left := fOrbs20.shOrbs[Orb].Left + 15;
  fOrbs20.laOrbs[Orb].Top := fOrbs20.shOrbs[Orb].Top + 9;
  fOrbs20.shOrbs[Orb].Visible := True; fOrbs20.laOrbs[Orb].Visible := True;
  // Column total > 20 or column full: Change column indicator color: lime - yellow - orange - red (no more available)
  if (Columns[NextCol].Total > 20) or ((Columns[NextCol].Orbs = 5) and (Columns[NextCol].Total <> 20)) then begin
    if ColBust[NextCol].Brush.Color = clLime then
      ColBust[NextCol].Brush.Color := clYellow
    else if ColBust[NextCol].Brush.Color = clYellow then
      ColBust[NextCol].Brush.Color := clOrange
    else
      ColBust[NextCol].Brush.Color := clRed;
  end;
  // Column has become unavailable
  if ColBust[NextCol].Brush.Color = clRed then begin
    for I := NextCol * 5 + 1 to NextCol * 5 + 5 do begin
      fOrbs20.shOrbs[I].Visible := False; fOrbs20.laOrbs[I].Visible := False;  // remove all column orbs
    end;
    Columns[NextCol].Available := False;
    Dec(NAvailable);
  end
  // Column may still be used
  else begin
    // Column total > 20, column full or column total = 20: Remove orbs (and score if total = 20)
    if (Columns[NextCol].Orbs = 5) or (Columns[NextCol].Total >= 20) then begin
      if Columns[NextCol].Total = 20 then begin
        // Update user score
        fOrbs20.edScore.Text := IntToStr(StrToInt(fOrbs20.edScore.Text) + 1);
        HSDone := False;                                                       // this may change highscore; so mark to rewrite file at application exit
      end;
      // Remove column orbs
      Columns[NextCol].Orbs := 0; Columns[NextCol].Total := 0;
      for I := NextCol * 5 + 1 to NextCol * 5 + 5 do begin
        fOrbs20.shOrbs[I].Visible := False; fOrbs20.laOrbs[I].Visible := False;
      end;
    end;
  end;
  // Column total value (new total or 0 if orbs removed)
  Totals[NextCol].Text := IntToStr(Columns[NextCol].Total);
  // Reset time counter for next orb
  T := GTime;
  // No more columns available: Game over message and rewrite highscore file
  if NAvailable = 0 then begin
    fOrbs20.tiOrbs20.Enabled := False;
    MessageDlg('Game over', 'No more column available: End of game!', mtError, [mbOK], 0);
    WriteHighScore(fOrbs20.edName.Text, StrToInt(fOrbs20.edScore.Text), HSDone);
    fOrbs20.btStart.Caption := 'Start'; fOrbs20.btStart.Enabled := False;
  end;
end;

{$R *.lfm}

{**********}
{ TfOrbs20 }
{**********}

{ Application start: Initialisation }

procedure TfOrbs20.FormCreate(Sender: TObject);

var
  I, HighScore: Integer;
  UName: string;

begin
  // Create 20 orbs, 5 per column; position as needed and set visible during runtime
  for I := 1 to 20 do begin
    shOrbs[I] := TShape.Create(fOrbs20.shOrbs[I]);
    shOrbs[I].Parent  := Self;
    shOrbs[I].Shape:= stCircle;
    shOrbs[I].Width   := shNextOrb.Width;
    shOrbs[I].Height  := shNextOrb.Height;
    shOrbs[I].Visible := False;
    laOrbs[I] := TLabel.Create(fOrbs20.laOrbs[I]);
    laOrbs[I].Parent  := Self;
    laOrbs[I].Font.Style := [fsBold];
    laOrbs[I].Visible := False;
  end;
  // Create arrays with totals edit fields and column value busted shapes
  edTotals[0]  := edTotal1;   edTotals[1]  := edTotal2;
  edTotals[2]  := edTotal3;   edTotals[3]  := edTotal4;
  shColBust[0] := shColBust1; shColBust[1] := shColBust2;
  shColBust[2] := shColBust3; shColBust[3] := shColBust4;
  // Initialise and start a new game
  Randomize;
  UName := ''; ReadHighScore(UName, HighScore);                                // get name and highscore of last-run user
  edName.Text := UName; edHighScore.Text := IntToStr(HighScore);
  iTime := 2000; mGameNew.Click;                                               // start new game
end;

{ Menu item "Game > New": Start new game }

procedure TfOrbs20.mGameNewClick(Sender: TObject);

var
  I: Integer;

begin
  tiOrbs20.Enabled := False;
  // Reset variables
  iAvailable := 4;
  bHighScore := True;                                                          // mark highscores as "up to date"
  for I := 0 to 3 do begin
    aColumns[I].Orbs := 0;
    aColumns[I].Total := 0;
    aColumns[I].Available := True;
  end;
  // Remove all orbs
  for I := 1 to 20 do begin
    shOrbs[I].Visible := False;
    laOrbs[I].Visible := False;
  end;
  // Reset field values on form
  edScore.Text := '0';
  for I := 0 to 3 do begin
    edTotals[I].Text := '0';
    shColBust[I].Brush.Color := clLime;
  end;
  // Re-enable "Start" button
  btStart.Enabled := True; btStart.Caption := 'Start';
end;

{ Menu item "Game > Exit": Exit the application }

procedure TfOrbs20.mGameExitClick(Sender: TObject);

var
  Ret: Integer;

begin
  // If highscores haven't yet be saved, ask to do so
  if not bHighScore then begin
    if StrToInt(edScore.Text) > StrToInt(edHighScore.Text) then begin
      Ret := MessageDlg('Orbs20 highscores', 'Do you want to save your score before exiting the program ?', mtWarning, [mbYes, mbNo], 0);
      if Ret = mrYes then
        WriteHighScore(edName.Text, StrToInt(edScore.Text), bHighScore);
    end;
  end;
  Close;
end;

{ Menu item "Settings > Time interval > ...": Choose time interval (before orb is dropped) }

procedure TfOrbs20.mSettingsTime1Click(Sender: TObject);

begin
  mSettingsTime1.Checked := True;
  mSettingsTime2.Checked := False;
  mSettingsTime4.Checked := False;
  iTime := 1000;
end;

procedure TfOrbs20.mSettingsTime2Click(Sender: TObject);

begin
  mSettingsTime1.Checked := False;
  mSettingsTime2.Checked := True;
  mSettingsTime4.Checked := False;
  iTime := 2000;
end;

procedure TfOrbs20.mSettingsTime4Click(Sender: TObject);

begin
  mSettingsTime1.Checked := False;
  mSettingsTime2.Checked := False;
  mSettingsTime4.Checked := True;
  iTime := 4000;
end;

{ Menu item "Settings > Highscores": Show highscores window; rewrite highscores if table was changed }

procedure TfOrbs20.mSettingsHiClick(Sender: TObject);

begin
  if fHighScores.Visible then
    fHighScores.Close
  else begin
    FillHighScores;
    fHighScores.ShowModal;
    // If highscore table (string grid values) has been changed, rewrite the highscores file
    if fHighScores.bChanged then
      UpdateHighScores;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfOrbs20.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Game:' + LineEnding;
  S += 'Drop numbered orbs into columns to total 20.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March-June 2019.';
  MessageDlg('About "Orbs20"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Pause/Resume": Start/resume resp. pause the game }

procedure TfOrbs20.btStartClick(Sender: TObject);

begin
  // Button "Start/Resume": Start/resume the game
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Resume') then begin
    iT := iTime; tiOrbs20.Enabled := True;                                     // enable timer
    btStart.Caption := 'Pause';
  end
  // Button "Pause": Pause the game
  else begin
    tiOrbs20.Enabled := False;
    btStart.Caption := 'Resume';
  end;
end;

{ "Column arrow clicked" routines: Drop the orb into selected column (if it is still available) }

procedure TfOrbs20.shArrow1MouseDown(Sender: TObject);

begin
  if aColumns[0].Available then begin
    iNextCol := 0;
    DropOrb(iNextOrb, iNextCol, aColumns, iTime, iT, edTotals, shColBust, iAvailable, bHighScore);
  end;
end;

procedure TfOrbs20.shArrow2MouseDown(Sender: TObject);

begin
  if aColumns[1].Available then begin
    iNextCol := 1;
    DropOrb(iNextOrb, iNextCol, aColumns, iTime, iT, edTotals, shColBust, iAvailable, bHighScore);
  end;
end;

procedure TfOrbs20.shArrow3MouseDown(Sender: TObject);

begin
  if aColumns[2].Available then begin
    iNextCol := 2;
    DropOrb(iNextOrb, iNextCol, aColumns, iTime, iT, edTotals, shColBust, iAvailable, bHighScore);
  end;
end;

procedure TfOrbs20.shArrow4MouseDown(Sender: TObject);

begin
  if aColumns[3].Available then begin
    iNextCol := 3;
    DropOrb(iNextOrb, iNextCol, aColumns, iTime, iT, edTotals, shColBust, iAvailable, bHighScore);
  end;
end;

{ Timer routine: Decrement time and drop orb if it reaches 0 }

procedure TfOrbs20.tiOrbs20Timer(Sender: TObject);

begin
  if iT mod 100 = 0 then
    // Display time each 0.1 sec
    edTime.Text := FloatToStrF(iT / 1000, ffFixed, 0, 1);
  // Time counter = time interval value means that a new random orb has to be generated
  if iT = iTime then begin
    iNextOrb := Random(9) + 1;                                                 // random orb value
    shNextOrb.Brush.Color := Colors[iNextOrb];                                 // orb color (depending on value)
    laNextOrb.Caption := IntToStr(iNextOrb);                                   // orb label (indicating its value)
    // Random column (among those still available)
    repeat
      iNextCol := Random(4);
    until aColumns[iNextCol].Available;
    // Display the orb above the column (where it will drop if user does not change the column by clicking one of the arrows)
    shNextCol.Left := 16 + iNextCol * 175 + (25 - 12);
    shNextCol.Visible := True;
  end;
  // Decrement time and drop the orb if time is over
  iT -= 100;
  if iT = 0 then begin
    if aColumns[iNextCol].Available then
      DropOrb(iNextOrb, iNextCol, aColumns, iTime, iT, edTotals, shColBust, iAvailable, bHighScore);
  end;
end;

{ User name changed: Re-read highscore value and start new game }

procedure TfOrbs20.edNameEditingDone(Sender: TObject);

var
  HighScore: Integer;
  UName: string;

begin
  UName := edName.Text;
  ReadHighScore(UName, HighScore);
  edHighscore.Text := IntToStr(HighScore);
  mGameNew.Click;                                                              // start new game
end;

end.

