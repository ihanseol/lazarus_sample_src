{***************************************}
{* Main unit for Dividers2 application *}
{***************************************}

unit divider2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, Grids, keys, help;

type
  TScores = array[1..2] of Integer;
  TExclusions = array of Integer;
  {*************}
  { TfDividers2 }
  {*************}
  TfDividers2 = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameSimple, mGameAddition, mGameSubtraction, mGameExit: TMenuItem;
    mSettings: TMenuItem;
    mSettingsNMax, mSettingsNMax100, mSettingsNMax1000: TMenuItem;
    mSettingsMax, mSettingsMax10, mSettingsMax20, mSettingsMax50, mSettingsMax100: TMenuItem;
    mSettingsExclusions: TMenuItem;
    mSettingsExclusions2, mSettingsExclusions5, mSettingsExclusions10, mSettingsExclusions20: TMenuItem;
    mSettingsExclusions25, mSettingsExclusions50, mSettingsExclusions100: TMenuItem;
    mSettingsTime, mSettingsTime1000, mSettingsTime2000, mSettingsTime5000: TMenuItem;
    mSettingsDPenalty, mSettingsAnimate, mSettingsKeys: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    edInstructions, edInstructions2: TMemo;
    laDivider, laNumber: TLabel;
    edDivider, edNumber: TEdit;
    Shape1: TShape;
    shNumber: TShape;
    stNumber: TStaticText;
    Label1, Label2: TLabel;
    edPlayer1, edPlayer2: TEdit;
    sgScoring: TStringGrid;
    btStart: TButton;
    tiDivider: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameSimpleClick(Sender: TObject);
    procedure mGameAdditionClick(Sender: TObject);
    procedure mGameSubtractionClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mSettingsNMax100Click(Sender: TObject);
    procedure mSettingsNMax1000Click(Sender: TObject);
    procedure mSettingsMax10Click(Sender: TObject);
    procedure mSettingsMax20Click(Sender: TObject);
    procedure mSettingsMax50Click(Sender: TObject);
    procedure mSettingsMax100Click(Sender: TObject);
    procedure mSettingsExclusions2Click(Sender: TObject);
    procedure mSettingsExclusions5Click(Sender: TObject);
    procedure mSettingsExclusions10Click(Sender: TObject);
    procedure mSettingsExclusions20Click(Sender: TObject);
    procedure mSettingsExclusions25Click(Sender: TObject);
    procedure mSettingsExclusions50Click(Sender: TObject);
    procedure mSettingsExclusions100Click(Sender: TObject);
    procedure mSettingsTime1000Click(Sender: TObject);
    procedure mSettingsTime2000Click(Sender: TObject);
    procedure mSettingsTime5000Click(Sender: TObject);
    procedure mSettingsAnimateClick(Sender: TObject);
    procedure mSettingsDPenaltyClick(Sender: TObject);
    procedure mSettingsKeysClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure tiDividerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure edPlayer1Change(Sender: TObject);
    procedure edPlayer2Change(Sender: TObject);
  private
    iNMax, iNMaxTemp, iMax, iMaxTemp, iTime, iTimeTemp, iDivider, iOpNumber, iNumber, iDTime, iWTime: Integer;
    aCorrect, aFalse, aScore: TScores;
    sGame: string;
    cKey1, cKey2: Char;
    bSolution: Boolean;
    aExclusions, aExclusionsTemp: TExclusions;
  end;

var
  fDividers2: TfDividers2;

implementation

{$R *.lfm}

{ Format numbers in scoring grid (right-alignment) }

function GridFormat(N: Integer): string;

var
  GF: string;

begin
  GF := IntToStr(N);
  if Length(GF) < 2 then
    GF := '   ' + GF
  else if Length(GF) < 3 then
    GF := '  ' + GF
  else if Length(GF) < 4 then
    GF := ' ' + GF;
  GridFormat := '   ' + GF;
end;

{ Prepare for starting a new game }

procedure NewGame(NMaxTemp, MaxTemp, GTimeTemp: Integer; ExclusionsTemp: TExclusions;
  out QCorrect, QFalse, QScore: TScores; out NMax, Max, GTime: Integer; out Exclusions: TExclusions);

var
  I, J: Integer;

begin
  fDividers2.tiDivider.Enabled := False;
  Max := MaxTemp; NMax := NMaxTemp; GTime := GTimeTemp;                        // game parameters chosen now becoming active
  Exclusions := ExclusionsTemp;
  for I := 1 to 2 do begin
    QCorrect[I] := 0; QFalse[I] := 0; QScore[I] := 0;
  end;
  fDividers2.edDivider.Text := ''; fDividers2.edNumber.Text := '';
  // Clear scoring grid
  for J := 1 to 2 do begin
    for I := 1 to 3 do begin
      fDividers2.sgScoring.Cells[J, I] := '';
    end;
  end;
  fDividers2.btStart.Caption := 'Start';
end;

{ Update list of dividers to be excluded }

procedure ExcludeDivider(Divider: Integer; Exclude: Boolean; var Exclusions: TExclusions);

// Variable Exclude=True means that divider is to exclude (= to be added to the list); Exclude=False means, it must be removed from the list

var
  I: Integer;
  IsInList: Boolean;
  OldExclusions: TExclusions;

begin
  OldExclusions := Exclusions; IsInList := False;
  // Check if divider is actually in the exclusions list
  for I := 0 to Length(OldExclusions) - 1 do begin
    if Divider = OldExclusions[I] then
      IsInList := True;
  end;
  // If devider not to be excluded is in the list, remove it
  if not Exclude and IsInList then begin
    SetLength(Exclusions, 0);
    for I := 0 to Length(OldExclusions) - 1 do begin
      if OldExclusions[I] <> Divider then begin
        SetLength(Exclusions, Length(Exclusions) + 1);
        Exclusions[Length(Exclusions) - 1] := OldExclusions[I];
      end;
    end;
  end
  // If devider to be excluded isn't in the list, add it
  else if Exclude and not IsInList then begin
    SetLength(Exclusions, Length(Exclusions) + 1);
    Exclusions[Length(Exclusions) - 1] := Divider;
  end;
end;

{*************}
{ TfDividers2 }
{*************}

{ Application start: Initialisation }

procedure TfDividers2.FormCreate(Sender: TObject);

begin
  // Default startup values
  sGame := 'simple';
  iNMaxTemp := 100; iMaxTemp := 10; iTimeTemp := 1000;
  cKey1 := 'Y'; cKey2 := 'M';
  SetLength(aExclusionsTemp, 0);
  // Start random number generator
  Randomize;
  // Prepare new "simple" game
  mGameSimple.Click;
end;

{ Menu item "Game > Simple game" : Prepare for a new simple game }

procedure TfDividers2.mGameSimpleClick(Sender: TObject);

begin
  sGame := 'simple';
  stTitle.Caption := 'Maths game:  Dividers (simple)';
  laNumber.Font.Style := []; laNumber.Caption := 'Operation number';
  laNumber.Enabled := False; edNumber.Enabled := False;
  edInstructions.Lines.Clear;
  edInstructions.Lines.AddText('Among the numbers displayed, find those, that have a given divider!');
  NewGame(iNMaxTemp, iMaxTemp, iTimeTemp, aExclusionsTemp, aCorrect, aFalse, aScore, iNMax, iMax, iTime, aExclusions);
end;

{ Menu item "Game > Addition game" : Prepare for a new addition game }

procedure TfDividers2.mGameAdditionClick(Sender: TObject);

begin
  sGame := 'addition';
  stTitle.Caption := 'Maths game:  Dividers (addition)';
  laNumber.Font.Style := [fsBold]; laNumber.Caption := 'Addition number';
  laNumber.Enabled := True; edNumber.Enabled := True;
  edInstructions.Lines.Clear;
  edInstructions.Lines.AddText('Among the numbers displayed, find those, that, plus a given number, have a given divider!');
  NewGame(iNMaxTemp, iMaxTemp, iTimeTemp, aExclusionsTemp, aCorrect, aFalse, aScore, iNMax, iMax, iTime, aExclusions);
end;

{ Menu item "Game > Subtraction game" : Prepare for a new subtraction game }

procedure TfDividers2.mGameSubtractionClick(Sender: TObject);

begin
  sGame := 'subtraction';
  stTitle.Caption := 'Maths game:  Dividers (subtraction)';
  edInstructions.Lines.Clear;
  edInstructions.Lines.AddText('Among the numbers displayed, find those, that, minus a given number, have a given divider!');
  laNumber.Font.Style := [fsBold]; laNumber.Caption := 'Subtraction number';
  laNumber.Enabled := True; edNumber.Enabled := True;
  NewGame(iNMaxTemp, iMaxTemp, iTimeTemp, aExclusionsTemp, aCorrect, aFalse, aScore, iNMax, iMax, iTime, aExclusions);
end;

{ Menu item "Game > Exit" : Exit application }

procedure TfDividers2.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Max. number > ...": Choose maximum value for displayed numbers }

procedure TfDividers2.mSettingsNMax100Click(Sender: TObject);

begin
  mSettingsNMax100.Checked := True;
  mSettingsNMax1000.Checked := False;
  iNMaxTemp := 100;
  if mSettingsMax50.Checked or mSettingsMax100.Checked then begin
    // Using dividers up to 50/100 for max. number = 100 makes no sense
    MessageDlg('Invalid settings', 'Actual value of max. divider can''t be used with max. number = 100! Max. divider will be reset to 20.', mtWarning, [mbOK], 0);
    mSettingsMax50.Checked := False; mSettingsMax100.Checked := False;
    mSettingsMax20.Click;                                                      // this not only checks max = 20, but also disables not accurate exclusions
  end;
  mSettingsMax50.Enabled := False; mSettingsMax100.Enabled := False;
end;

procedure TfDividers2.mSettingsNMax1000Click(Sender: TObject);

begin
  mSettingsNMax100.Checked := False;
  mSettingsNMax1000.Checked := True;
  iNMaxTemp := 1000;
  mSettingsMax50.Enabled := True; mSettingsMax100.Enabled := True;
end;

{ Menu items "Settings > Max. divider > ...": Choose maximum value for divider }

procedure TfDividers2.mSettingsMax10Click(Sender: TObject);

begin
  mSettingsMax10.Checked := True;
  mSettingsMax20.Checked := False;
  mSettingsMax50.Checked := False;
  mSettingsMax100.Checked := False;
  iMaxTemp := 10;
  // Disable inaccurate exclusions
  mSettingsExclusions20.Checked := False;
  mSettingsExclusions25.Checked := False;
  mSettingsExclusions50.Checked := False;
  mSettingsExclusions100.Checked := False;
  mSettingsExclusions20.Enabled := False;
  mSettingsExclusions25.Enabled := False;
  mSettingsExclusions50.Enabled := False;
  mSettingsExclusions100.Enabled := False;
end;

procedure TfDividers2.mSettingsMax20Click(Sender: TObject);

begin
  mSettingsMax10.Checked := False;
  mSettingsMax20.Checked := True;
  mSettingsMax50.Checked := False;
  mSettingsMax100.Checked := False;
  iMaxTemp := 20;
  // Enable/disable exclusions (as are acuurate or not)
  mSettingsExclusions25.Checked := False;
  mSettingsExclusions50.Checked := False;
  mSettingsExclusions100.Checked := False;
  mSettingsExclusions20.Enabled := True;
  mSettingsExclusions25.Enabled := False;
  mSettingsExclusions50.Enabled := False;
  mSettingsExclusions100.Enabled := False;
end;

procedure TfDividers2.mSettingsMax50Click(Sender: TObject);

begin
  mSettingsMax10.Checked := False;
  mSettingsMax20.Checked := False;
  mSettingsMax50.Checked := True;
  mSettingsMax100.Checked := False;
  iMaxTemp := 50;
  // Enable/disable exclusions (as are acuurate or not)
  mSettingsExclusions100.Checked := False;
  mSettingsExclusions20.Enabled := True;
  mSettingsExclusions25.Enabled := True;
  mSettingsExclusions50.Enabled := True;
  mSettingsExclusions100.Enabled := False;
end;

procedure TfDividers2.mSettingsMax100Click(Sender: TObject);

begin
  mSettingsMax10.Checked := False;
  mSettingsMax20.Checked := False;
  mSettingsMax50.Checked := False;
  mSettingsMax100.Checked := True;
  iMaxTemp := 100;
  // Enable/disable exclusions (as are acuurate or not)
  mSettingsExclusions20.Enabled := True;
  mSettingsExclusions25.Enabled := True;
  mSettingsExclusions50.Enabled := True;
  mSettingsExclusions100.Enabled := TRue;
end;

{ Menu items "Settings > Exclusions > ...": Choose which dividers should be excluded (as to easy...) }

procedure TfDividers2.mSettingsExclusions2Click(Sender: TObject);

begin
  if mSettingsExclusions2.Checked then
    mSettingsExclusions2.Checked := False
  else
    mSettingsExclusions2.Checked := True;
  ExcludeDivider(2, mSettingsExclusions2.Checked, aExclusionsTemp);
end;

procedure TfDividers2.mSettingsExclusions5Click(Sender: TObject);

begin
  if mSettingsExclusions5.Checked then
    mSettingsExclusions5.Checked := False
  else
    mSettingsExclusions5.Checked := True;
  ExcludeDivider(5, mSettingsExclusions5.Checked, aExclusionsTemp);
end;

procedure TfDividers2.mSettingsExclusions10Click(Sender: TObject);

begin
  if mSettingsExclusions10.Checked then
    mSettingsExclusions10.Checked := False
  else
    mSettingsExclusions10.Checked := True;
  ExcludeDivider(10, mSettingsExclusions10.Checked, aExclusionsTemp);
end;

procedure TfDividers2.mSettingsExclusions20Click(Sender: TObject);

begin
  if mSettingsExclusions20.Checked then
    mSettingsExclusions20.Checked := False
  else
    mSettingsExclusions20.Checked := True;
  ExcludeDivider(20, mSettingsExclusions20.Checked, aExclusionsTemp);
end;

procedure TfDividers2.mSettingsExclusions25Click(Sender: TObject);

begin
  if mSettingsExclusions25.Checked then
    mSettingsExclusions25.Checked := False
  else
    mSettingsExclusions25.Checked := True;
  ExcludeDivider(25, mSettingsExclusions25.Checked, aExclusionsTemp);
end;

procedure TfDividers2.mSettingsExclusions50Click(Sender: TObject);

begin
  if mSettingsExclusions50.Checked then
    mSettingsExclusions50.Checked := False
  else
    mSettingsExclusions50.Checked := True;
  ExcludeDivider(50, mSettingsExclusions50.Checked, aExclusionsTemp);
end;

procedure TfDividers2.mSettingsExclusions100Click(Sender: TObject);

begin
  if mSettingsExclusions100.Checked then
    mSettingsExclusions100.Checked := False
  else
    mSettingsExclusions100.Checked := True;
  ExcludeDivider(100, mSettingsExclusions100.Checked, aExclusionsTemp);
end;

{ Menu items "Settings > Max. time > ...": Choose maximum display time }

procedure TfDividers2.mSettingsTime1000Click(Sender: TObject);

begin
  mSettingsTime1000.Checked := True;
  mSettingsTime2000.Checked := False;
  mSettingsTime5000.Checked := False;
  iTimeTemp := 1000;                                                           // time in msec
end;

procedure TfDividers2.mSettingsTime2000Click(Sender: TObject);

begin
  mSettingsTime1000.Checked := False;
  mSettingsTime2000.Checked := True;
  mSettingsTime5000.Checked := False;
  iTimeTemp := 2000;
end;

procedure TfDividers2.mSettingsTime5000Click(Sender: TObject);

begin
  mSettingsTime1000.Checked := False;
  mSettingsTime2000.Checked := False;
  mSettingsTime5000.Checked := True;
  iTimeTemp := 5000;
end;

{ Menu item "Settings > Double penalty": Choose penalty (-1 or -2) for false answer }

procedure TfDividers2.mSettingsDPenaltyClick(Sender: TObject);

begin
  if mSettingsDPenalty.Checked then
    mSettingsDPenalty.Checked := False
  else
    mSettingsDPenalty.Checked := True;
end;

{ Menu item "Settings > Animate numbers": Choose to animate or not the display numbers }

procedure TfDividers2.mSettingsAnimateClick(Sender: TObject);

begin
  if mSettingsAnimate.Checked then
    mSettingsAnimate.Checked := False
  else
    mSettingsAnimate.Checked := True;
end;

{ Menu item "Settings > Player keys...": Choose (enter) player answer keys }

procedure TfDividers2.mSettingsKeysClick(Sender: TObject);

begin
  // Pass actual values to fKeys form
  fKeys.laPlayer1.Caption := edPlayer1.Text;
  fKeys.laPlayer2.Caption := edPlayer2.Text;
  fKeys.sButton := 'cancel';
  // Show the fKeys form to enter the new player keys
  fKeys.ShowModal;
  // If user hit OK button, get new player keys from fKeys form
  if fKeys.sButton = 'ok' then begin
    cKey1 := Uppercase(fKeys.edKey1.Text)[1];
    cKey2 := Uppercase(fKeys.edKey2.Text)[1];
    // Update the instructions text
    edInstructions2.Lines.Clear;
    edInstructions2.Lines.AddText('To select a number, press your answer key; ' + edPlayer1.Text + ': "' + cKey1 + '", ' +  edPlayer2.Text + ': "' + cKey2 + '".');
  end;
end;

{ Menu item "Help > Help": Display application help }

procedure TfDividers2.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfDividers2.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics game for 2 players: Find numbers with a given divider.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2019.';
  MessageDlg('About "Dividers2"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Restart/Stop": Start resp. stop the game }

procedure TfDividers2.btStartClick(Sender: TObject);

begin
  if (btStart.Caption = 'Start') or (btStart.Caption = 'Restart') then begin
    // Button "Start": Start the game
    iDTime := iTime; iWTime := -100;                                           // iDTime = number display time (as chosen); iWTime = waiting time until next display
    iNumber := 1; iDivider := 3;                                               // these values avoid that player scores without a correct solution displayed
    tiDivider.Enabled := True;                                                 // activate the timer (timer routine will display the numbers)
    btStart.Caption := 'Stop';
  end
  else begin
    // Button "Stop": Stop the game
    tiDivider.Enabled := False;                                                // deactivate the timer
    btStart.Caption := 'Restart';                                              // game will continue at the stage where it was stopped
  end;
end;

{ Game timer routine }

procedure TfDividers2.tiDividerTimer(Sender: TObject);

// The timer routine displays the numbers (during selected time, with random wait-time between 2 displays) until timer is disabled
// This happens: 1. when user pushes "Stop"; 2. when a new game is started; 3. when a player key is stroked

const
  clOrange = $0080FF; clMauve = $FF8080;
  Colors: array[0..9] of TColor = (
    clRed, clBlue, clGreen, clLime, clYellow, clFuchsia, clAqua, clOrange, clMauve, clCream
  );

var
  Count, X, Y, I: Integer;
  OK, IsSolution: Boolean;

begin
  // iDTime > 0 indicates that number display time isn't yet over
  if iDTime > 0 then begin
    // iDTime = selected iTime indicated that a new number (plus divider and operation number, if any) has to be generated and displayed
    if iDTime = iTime then begin
      // Generate divider
      repeat
        OK := True;
        iDivider := Random(iMax - 1) + 2;
        // Divider must not be one of those excluded
        for I := 0 to Length(aExclusions) - 1 do begin
          if iDivider = aExclusions[I] then
            OK := False;
        end;
      until OK;
      edDivider.Text := IntToStr(iDivider);
      if sGame <> 'simple' then begin
        // Generate addition/subtraction number
        iOpNumber := Random(10) + 1;
        edNumber.Text := IntToStr(iOpNumber);
      end;
      // Generate display number: 1/3 should be correct solutions
      if Random(3) = 0 then
        bSolution := True
      else
        bSolution := False;
      Count := 0;
      repeat
        iNumber := Random(iNMax - iDivider + 1) + iDivider;
        IsSolution := False;
        if (sGame = 'simple') and (iNumber mod iDivider = 0) then
          IsSolution := True
        else if (sGame = 'addition') and ((iNumber + iOpNumber) mod iDivider = 0) then
          IsSolution := True
        else if (sGame = 'subtraction') and ((iNumber - iOpNumber) mod iDivider = 0) then
          IsSolution := True;
        OK := False;
        if bSolution = IsSolution then
          OK := True;
        Inc(Count);                                                            // "security varible" to avoid infinite loops
      until OK or (Count > 1000);
      // Move the shape and its number to center of box
      shNumber.Left := 100; shNumber.Top := 366;
      stNumber.Left := 100; stNumber.Top := 381;
      // Make the shape and the number visible
      stNumber.Visible := True; shNumber.Visible := True;
      // Get the shape a random color
      shNumber.Brush.Color := Colors[Random(10)];
      stNumber.Caption := IntToStr(iNumber);
    end;
    if mSettingsAnimate.Checked then begin
      // Randomly move the shape and its number leftwards, rightwards, upwards, downwards
      X := 20 * (Random(3) - 1); Y := 20 * (Random(3) - 1);
      if (shNumber.Left + X > 20) and (shNumber.Left + X < 180) then begin
        shNumber.Left := shNumber.Left + X;
        stNumber.Left := stNumber.Left + X;
      end;
      if (shNumber.Top + Y > 310) and (shNumber.Top + Y < 420) then begin
        shNumber.Top := shNumber.Top + Y;
        stNumber.Top := stNumber.Top + Y;
      end;
    end;
    // Decrease display time by timer interval value (100 msec)
    iDTime -= 100;
  end
  // iDTime = 0 indicates that number display time is over, i.e. that time actually is waiting time
  else begin
    // iWTime = -100 indicates that waiting time starts now
    if iWTime = -100 then begin
      // Choose a random waiting time
      iWTime := Random(751) + 250;
      // Be sure, players can't score during waiting time
      iNumber := 1; iDivider := 3;
      // Make the shape and its number invisible
      stNumber.Visible := False; shNumber.Visible := False;
    end
    // iWTime <= 0 (and <> -100) indicates that waiting time is over now
    else if iWTime <= 0 then begin
      // Reaset display time to selected value
      iDTime := iTime;
      // Reset waiting time in order to indicate a waiting time start, when display time will be 0
      // iWTime is set to 0 here, as 100 will be subtracted below (thus getting the "start of waiting time" value of -100)
      iWTime := 0;
    end;
    // Dercrease waiting time by timer interval value
    iWTime -= 100;
  end;
end;

{ Keyboard character-key-pressed handler }

procedure TfDividers2.FormKeyPress(Sender: TObject; var Key: char);

var
  P, J: Integer;

begin
  // Proceed only if the game is actually running
  if btStart.Caption = 'Stop' then begin
    // If key pressed is one of the player keys, check if solution is correct and update scores
    if (Uppercase(Key) = cKey1) or (Uppercase(Key) = cKey2) then begin
      tiDivider.Enabled := False;                                              // this ends the number display by the timer routine
      iNumber := 1; iDivider := 3;                                             // this avoids further scoring
      if Uppercase(Key) = cKey1 then
        P := 1
      else
        P := 2;
      // Solution is correct: increase the score
      if bSolution then begin
        Inc(aCorrect[P]);
        Inc(aScore[P]);
      end
      // Solution is false: apply penalty
      else begin
        Inc(aFalse[P]);
        Dec(aScore[P]);
        if mSettingsDPenalty.Checked then
          Dec(aScore[P]);
      end;
      // Update the scoring grid
      for J := 1 to 2 do begin
        sgScoring.Cells[J, 1] := GridFormat(aCorrect[J]);
        sgScoring.Cells[J, 2] := GridFormat(aFalse[J]);
        sgScoring.Cells[J, 3] := GridFormat(aScore[J]);
      end;
      // Reinit display and waiting time
      iDTime := iTime; iWTime := -100;
      // Reactivate the timer (number display continuing)
      tiDivider.Enabled := True;
    end;
  end;
end;

{ Update player 1 name if user edits the field }

procedure TfDividers2.edPlayer1Change(Sender: TObject);

begin
  sgScoring.Cells[1, 0] := edPlayer1.Text;
  edInstructions2.Lines.Clear;
  edInstructions2.Lines.AddText('To select a number, press your answer key; ' + edPlayer1.Text + ': "' + cKey1 + '", ' +  edPlayer2.Text + ': "' + cKey2 + '".');
end;

{ Update player 2 name if user edits the field }

procedure TfDividers2.edPlayer2Change(Sender: TObject);

begin
  sgScoring.Cells[2, 0] := edPlayer2.Text;
  edInstructions2.Lines.Clear;
  edInstructions2.Lines.AddText('To select a number, press your answer key; ' + edPlayer1.Text + ': "' + cKey1 + '", ' +  edPlayer2.Text + ': "' + cKey2 + '".');
end;

end.

