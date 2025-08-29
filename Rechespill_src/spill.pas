{****************************************}
{* Main unit for Rechespill application *}
{****************************************}

unit spill;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, ColorBox, LCLIntf;

type
  TAddNumbers = record
    X, Y: Integer;
  end;
  {**********}
  { TfRSpill }
  {**********}
  TfRSpill = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameNew, mGameExit: TMenuItem;
    mOptionsPlayers, mOptionsPlayers1, mOptionsPlayers2, mOptionsPlayers3, mOptionsPlayers4: TMenuItem;
    mOptions, mOptionsMistake, mOptionsMistake1, mOptionsMistake2, mOptionsMistake3: TMenuItem;
    mOptionsArrival, mOptionsArrivalPass, mOptionsArrivalWait: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Shape1: TShape;
    shField1, shField2, shField3, shField4, shField5, shField6: TShape;
    shField7, shField8, shField9, shField10, shField11, shField12: TShape;
    shField13, shField14, shField15, shField16, shField17, shField18: TShape;
    shField19, shField20, shField21, shField22, shField23, shField24: TShape;
    shField25, shField26, shField27, shField28, shField29, shField30: TShape;
    shPlayer1, shPlayer2, shPlayer3, shPlayer4: TShape;
    shSum1, shSum2, shSum3, shSum4, shSum5, shSum6, shSum7: TShape;
    shSum8, shSum9, shSum10, shSum11, shSum12, shSum13, shSum14: TShape;
    shSum15, shSum16, shSum17, shSum18, shSum19, shSum20, shSum21: TShape;
    shSum22, shSum23, shSum24, shSum25, shSum26, shSum27, shSum28: TShape;
    shSum29, shSum30, shSum31, shSum32, shSum33, shSum34, shSum35: TShape;
    shSum36, shSum37, shSum38, shSum39, shSum40, shSum41, shSum42: TShape;
    stSum1, stSum2, stSum3, stSum4, stSum5, stSum6, stSum7: TStaticText;
    stSum8, stSum9, stSum10, stSum11, stSum12, stSum13, stSum14: TStaticText;
    stSum15, stSum16, stSum17, stSum18, stSum19, stSum20, stSum21: TStaticText;
    stSum22, stSum23, stSum24, stSum25, stSum26, stSum27, stSum28: TStaticText;
    stSum29, stSum30, stSum31, stSum32, stSum33, stSum34, stSum35: TStaticText;
    stSum36, stSum37, stSum38, stSum39, stSum40, stSum41, stSum42: TStaticText;
    edName1, edName2, edName3, edName4, edPlayer1, edPlayer2, edPlayer3, edPlayer4: TEdit;
    shColor1, shColor2, shColor3, shColor4: TShape;
    cobColors: TColorBox;
    laDice: TLabel;
    imDice: TImage;
    btDice: TButton;
    laPlayer, laColors, laCalc, laMove, laResult, laMoveBack: TLabel;
    edCalc, edMove, edResult, edMoveBack: TEdit;
    btStart: TButton;
    tiRSpill: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mGameNewClick(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mOptionsPlayers1Click(Sender: TObject);
    procedure mOptionsPlayers2Click(Sender: TObject);
    procedure mOptionsPlayers3Click(Sender: TObject);
    procedure mOptionsPlayers4Click(Sender: TObject);
    procedure mOptionsArrivalPassClick(Sender: TObject);
    procedure mOptionsArrivalWaitClick(Sender: TObject);
    procedure mOptionsMistake1Click(Sender: TObject);
    procedure mOptionsMistake2Click(Sender: TObject);
    procedure mOptionsMistake3Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDiceClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure edName1EditingDone(Sender: TObject);
    procedure edName2EditingDone(Sender: TObject);
    procedure edName3EditingDone(Sender: TObject);
    procedure edName4EditingDone(Sender: TObject);
    procedure cobColorsChange(Sender: TObject);
    procedure shColor1MouseDown(Sender: TObject);
    procedure shColor2MouseDown(Sender: TObject);
    procedure shColor3MouseDown(Sender: TObject);
    procedure shColor4MouseDown(Sender: TObject);
    procedure stSum1Click(Sender: TObject);
    procedure stSum2Click(Sender: TObject);
    procedure stSum3Click(Sender: TObject);
    procedure stSum4Click(Sender: TObject);
    procedure stSum5Click(Sender: TObject);
    procedure stSum6Click(Sender: TObject);
    procedure stSum7Click(Sender: TObject);
    procedure stSum8Click(Sender: TObject);
    procedure stSum9Click(Sender: TObject);
    procedure stSum10Click(Sender: TObject);
    procedure stSum11Click(Sender: TObject);
    procedure stSum12Click(Sender: TObject);
    procedure stSum13Click(Sender: TObject);
    procedure stSum14Click(Sender: TObject);
    procedure stSum15Click(Sender: TObject);
    procedure stSum16Click(Sender: TObject);
    procedure stSum17Click(Sender: TObject);
    procedure stSum18Click(Sender: TObject);
    procedure stSum19Click(Sender: TObject);
    procedure stSum20Click(Sender: TObject);
    procedure stSum21Click(Sender: TObject);
    procedure stSum22Click(Sender: TObject);
    procedure stSum23Click(Sender: TObject);
    procedure stSum24Click(Sender: TObject);
    procedure stSum25Click(Sender: TObject);
    procedure stSum26Click(Sender: TObject);
    procedure stSum27Click(Sender: TObject);
    procedure stSum28Click(Sender: TObject);
    procedure stSum29Click(Sender: TObject);
    procedure stSum30Click(Sender: TObject);
    procedure stSum31Click(Sender: TObject);
    procedure stSum32Click(Sender: TObject);
    procedure stSum33Click(Sender: TObject);
    procedure stSum34Click(Sender: TObject);
    procedure stSum35Click(Sender: TObject);
    procedure stSum36Click(Sender: TObject);
    procedure stSum37Click(Sender: TObject);
    procedure stSum38Click(Sender: TObject);
    procedure stSum39Click(Sender: TObject);
    procedure stSum40Click(Sender: TObject);
    procedure stSum41Click(Sender: TObject);
    procedure stSum42Click(Sender: TObject);
    procedure tiRSpillTimer(Sender: TObject);
  private
    iPlayers, iRound, iPlayer, iDiceValue, iCalcResult, iColSelPlayer, iDiceRoll, iSumBar: Integer;
    aPlayerNames: array[0..3] of string;
    aPlayerColors: array[0..3] of TColor;
    aAdditions: array[0..29] of TAddNumbers;
    aPlayerFields: array[0..3] of Integer;
    shFields: array[0..29] of TShape;
    stFields: array[0..29] of TStaticText;
    edPlayerNums, edPlayerNames: array[0..3] of TEdit;
    shPlayers, shPlayerColors: array[0..3] of TShape;
    stSums: array[0..41] of TStaticText;
    shSumBars: array[0..167] of TShape;
  end;

const
  DefaultPlayerNames: array[0..3] of string = (
    'Spiller 1', 'Spiller 2', 'Spiller 3', 'Spiller 4'
  );
  DefaultPlayerColors: array[0..3] of TColor = (
    clRed, clBlue, clLime, clYellow
  );

var
  fRSpill: TfRSpill;

implementation

{$R *.lfm}

{ Check player name }

function PlayerNameOK(Players: Integer; Name: string; var NamesFields: array of TEdit): Boolean;

var
  N, I: Integer;
  OK: Boolean;

begin
  if Name = '' then begin
    // Must not be null
    OK := False;
  end
  else begin
    // Must be unique
    N := 0; OK := True;
    for I := 0 to Players - 1 do begin
      if NamesFields[I].Text = Name then
        Inc(N);
    end;
    if N <> 1 then
      OK := False;
  end;
  if not OK then
    MessageDlg('Rechespill', 'Dee Numm ass net valabel! Setzen zréck op dee vu virdrun...', mtWarning, [mbOK], 0);
  Result := OK;
end;

{ Show color selection box }

procedure ColorSelectShow(Colour: TColor);

begin
  fRSpill.laColors.Visible := True; fRSpill.cobColors.Visible := True;
  fRSpill.cobColors.Selected := Colour;
end;

{ Display or hide player names and colors }

procedure AdaptPlayers(Players: Integer; PlayerNames: array of string; PlayerColors: array of TColor;
  var PlayerNumsFields, PlayerNamesFields: array of TEdit; var PlayerShapes, PlayerColorsShapes: array of TShape);

var
  I: Integer;

begin
  for I := 0 to 3 do begin
    if I < Players then begin
      PlayerNumsFields[I].Visible := True; PlayerNamesFields[I].Visible := True;
      PlayerShapes[I].Visible := True; PlayerColorsShapes[I].Visible := True;
      PlayerNamesFields[I].Text := PlayerNames[I];
      PlayerShapes[I].Brush.Color := PlayerColors[I];
      PlayerColorsShapes[I].Brush.Color := PlayerColors[I];
    end
    else begin
      PlayerNumsFields[I].Visible := False; PlayerNamesFields[I].Visible := False;
      PlayerShapes[I].Visible := False; PlayerColorsShapes[I].Visible := False;
    end;
  end;
end;

{ Move actual player's figure to given field }

procedure MovePlayer(Player, Field: Integer; var FieldShapes, PlayerShapes: array of TShape);

begin
  case Player of
    0: begin
      PlayerShapes[Player].Left := FieldShapes[Field].Left + 10;
      PlayerShapes[Player].Top := FieldShapes[Field].Top + 5;
    end;
    1: begin
      PlayerShapes[Player].Left := FieldShapes[Field].Left + 45;
      PlayerShapes[Player].Top := FieldShapes[Field].Top + 5;
    end;
    2: begin
      PlayerShapes[Player].Left := FieldShapes[Field].Left + 10;
      PlayerShapes[Player].Top := FieldShapes[Field].Top + 40;
    end;
    3: begin
      PlayerShapes[Player].Left := FieldShapes[Field].Left + 45;
      PlayerShapes[Player].Top := FieldShapes[Field].Top + 40;
    end;
  end;
end;

{ Start gaming sequence for actual player (dice button will be enabled) }

procedure StartPlayer(Round, Player: Integer; var PlayerNames: array of TEdit);

var
  I: Integer;

begin
  for I := 0 to 3 do
    PlayerNames[I].Color := clDefault;
  fRSpill.laPlayer.Caption := 'Ronn ' + IntToStr(Round) + ': ' + UpperCase(PlayerNames[Player].Text);
  PlayerNames[Player].Color := clHighLight;
  fRSpill.laDice.Font.Color := clHighLight; fRSpill.laDice.Font.Style := [fsBold];
  fRSpill.laMove.Font.Color := clDefault;
  fRSpill.laMoveBack.Visible := True; fRSpill.edMoveBack.Visible := True;
  fRSpill.laMoveBack.Font.Color := clDefault; fRSpill.laMoveBack.Font.Style := [];
  fRSpill.edCalc.Text := ''; fRSpill.edResult.Text := ''; fRSpill.edMove.Text := ''; fRSpill.edMoveBack.Text := '';
  fRSpill.btDice.Enabled := True;                                              // game will continue with player pushing the dice button
end;

{ Strike through sumfield clicked, check user answer (for sumfield clicked), move back if wrong answer }

procedure CheckAdditionResult(var Player, Round: Integer; Players, ResultField, DiceValue, CalcResult: Integer; var SumBar: Integer;
  PlayerColor: TColor; var PlayerFields: array of Integer; var SumLabels: array of TStaticText;
  var FieldShapes, PlayerShapes, SumBarShapes: array of TShape);

var
  NewField, I, J: Integer;
  OK, StOut, AllStOut: Boolean;

begin
  if fRSpill.laResult.Font.Color = clHighLight then begin
    OK := True;
    // Check if actual player has already stroken through this sumfield
    for I := 0 to SumBar do begin
      if (SumBarShapes[I].Left = SumLabels[ResultField].Left) and (SumBarShapes[I].Top = SumLabels[ResultField].Top + Player * 8 + 5) then
        OK := False;
    end;
    // This sumfield has not yet been stroken through by actual player
    if OK then begin
      if CalcResult = StrToInt(SumLabels[ResultField].Caption) then
        fRSpill.edResult.Text := 'Richteg Äntwert'
      else
        fRSpill.edResult.Text := 'Falsch Äntwert';
      // Strike through sumfield clicked
      SumBarShapes[SumBar].Left := SumLabels[ResultField].Left;
      SumBarShapes[SumBar].Top := SumLabels[ResultField].Top + Player * 8 + 5;
      SumBarShapes[SumBar].Pen.Color := PlayerColor;
      SumBarShapes[SumBar].Brush.Color := PlayerColor;
      SumBarShapes[SumBar].Visible := True;
      Inc(SumBar);
      fRSpill.laMove.Font.Color := clDefault; fRSpill.laCalc.Font.Color := clDefault;
      fRSpill.laResult.Font.Color := clDefault; fRSpill.laResult.Font.Style := [];
      // If answer is correct, player's figure stays on actual game field, otherwise it has to be moved backward
      NewField := PlayerFields[Player];
      if CalcResult = StrToInt(SumLabels[ResultField].Caption) then begin
        // Correct addition result has been clicked
        fRSpill.laMoveBack.Visible := False; fRSpill.edMoveBack.Visible := False;
      end
      else begin
        // Wrong addition result has been clicked
        fRSpill.laMoveBack.Visible := True; fRSpill.edMoveBack.Visible := True;
        fRSpill.laMoveBack.Font.Color := clHighLight;
        // The number of fields to move back depends on actual game setiings
        if fRSpill.mOptionsMistake1.Checked then begin
          // Move back 2 fields
          NewField := PlayerFields[Player] - 2;
          if NewField < 0 then
            NewField := PlayerFields[Player] - 1;
          fRSpill.edMoveBack.Text := '2 Felder zréckréckelen';
        end
        else if fRSpill.mOptionsMistake2.Checked then begin
          // Move back 3 fields
          NewField := PlayerFields[Player] - 3;
          if NewField < 0 then
            NewField := PlayerFields[Player] - 2;
            if NewField < 0 then
              NewField := PlayerFields[Player] - 1;
          fRSpill.edMoveBack.Text := '3 Felder zréckréckelen';
        end
        else if fRSpill.mOptionsMistake3.Checked then begin
          // Move back to field, where player was before rolling the dice
          NewField := PlayerFields[Player] - DiceValue;
          fRSpill.edMoveBack.Text := 'Op d''Feld vu virdrun zréckréckelen';
        end;
        if NewField < 0 then                                                   // can't move beyond the Start field
          NewField := 0;
      end;
      // Move the player's figure to definitive position
      PlayerFields[Player] := NewField;
      MovePlayer(Player, PlayerFields[Player], FieldShapes, PlayerShapes);
      // Prepare for next player to play
      Inc(Player);
      if Player > Players - 1 then begin
        Player := 0; Inc(Round);
      end;
      fRSpill.btStart.Caption := 'Weider'; fRSpill.btStart.Enabled := True;    // game will continue with player hitting Next button
    end
    // This sumfield has already been stroken through by actual player
    else begin
      // Check if all fields that have the same sum value as the one clicked by the player have already been stroken through by actual player
      AllStOut := True;
      for I := 0 to Length(SumLabels) - 1 do begin
        if SumLabels[I].Caption = SumLabels[ResultField].Caption then begin
          StOut := False;
          for J := 0 to SumBar do begin
            if (SumBarShapes[J].Left = SumLabels[I].Left) and (SumBarShapes[J].Top = SumLabels[I].Top + Player * 8 + 5) then
              StOut := True;
          end;
          if not StOut then
            AllStOut := False;
        end;
      end;
      if AllStOut then begin
        // All these fields are stroken through: Player has to move back to field before the dice roll
        MessageDlg('Rechespill', 'All Felder mat deem Resultat si schon durechgestrach!', mtInformation, [mbOK], 0);
        PlayerFields[Player] := PlayerFields[Player] - DiceValue;
        MovePlayer(Player, PlayerFields[Player], FieldShapes, PlayerShapes);
        fRSpill.laMove.Font.Color := clDefault; fRSpill.laCalc.Font.Color := clDefault;
        fRSpill.laResult.Font.Color := clDefault; fRSpill.laResult.Font.Style := [];
        if CalcResult = StrToInt(SumLabels[ResultField].Caption) then begin
          // If correct answer field was clicked, the player may roll the dice again
          fRSpill.edMoveBack.Text := 'Zréck a nach eemol wierfelen';
          fRSpill.laMoveBack.Visible := True; fRSpill.edMoveBack.Visible := True;
          fRSpill.btStart.Caption := 'Weider'; fRSpill.btStart.Enabled := True;
        end
        else begin
          // If false answer field was clicked, the game continues with the next player
          Inc(Player);
          if Player > Players - 1 then begin
            Player := 0; Inc(Round);
          end;
          fRSpill.edMoveBack.Text := 'Op d''Feld vu virdrun zréckréckelen';
          fRSpill.laMoveBack.Visible := True; fRSpill.edMoveBack.Visible := True;
          fRSpill.btStart.Caption := 'Weider'; fRSpill.btStart.Enabled := True;
        end
      end
      else begin
        // There are other fields with this sum: Do nothing and continue game with player clicking another sumfield
        MessageDlg('Rechespill', 'Dat Resultat ass schon durechgestrach!', mtWarning, [mbOK], 0);
      end;
    end;
  end;
end;

{**********}
{ TfRSpill }
{**********}

{ Application start: Initialisation }

procedure TfRSpill.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // Create array with game fields
  shFields[0]  := shField1;  shFields[1]  := shField2;  shFields[2]  := shField3;  shFields[3]  := shField4;  shFields[4]  := shField5;
  shFields[5]  := shField6;  shFields[6]  := shField7;  shFields[7]  := shField8;  shFields[8]  := shField9;  shFields[9]  := shField10;
  shFields[10] := shField11; shFields[11] := shField12; shFields[12] := shField13; shFields[13] := shField14; shFields[14] := shField15;
  shFields[15] := shField16; shFields[16] := shField17; shFields[17] := shField18; shFields[18] := shField19; shFields[19] := shField20;
  shFields[20] := shField21; shFields[21] := shField22; shFields[22] := shField23; shFields[23] := shField24; shFields[24] := shField25;
  shFields[25] := shField26; shFields[26] := shField27; shFields[27] := shField28; shFields[28] := shField29; shFields[29] := shField30;
  // Create static-text objects as labels for game fields (and put them into an array)
  for I := 0 to 29 do begin
    stFields[I] := TStaticText.Create(fRSpill.stFields[I]);
    stFields[I].Parent := Self;
    stFields[I].Width := shFields[I].Width - 10;
    stFields[I].Height := 30;
    stFields[I].Alignment := taCenter;
    stFields[I].Left := shFields[I].Left + 5;
    stFields[I].Top := shFields[I].Top + 25;
  end;
  // Create arrays with player figure shapes, sequence numbers and names, color shapes
  shPlayers[0] := shPlayer1; shPlayers[1] := shPlayer2; shPlayers[2] := shPlayer3; shPlayers[3] := shPlayer4;
  edPlayerNums[0] := edPlayer1; edPlayerNums[1] := edPlayer2; edPlayerNums[2] := edPlayer3; edPlayerNums[3] := edPlayer4;
  edPlayerNames[0] := edName1; edPlayerNames[1] := edName2; edPlayerNames[2] := edName3; edPlayerNames[3] := edName4;
  shPlayerColors[0] := shColor1; shPlayerColors[1] := shColor2; shPlayerColors[2] := shColor3; shPlayerColors[3] := shColor4;
  // Create array with sumfield static-texts
  stSums[0]  := stSum1;  stSums[1]  := stSum2;  stSums[2]  := stSum3;  stSums[3]  := stSum4;  stSums[4]  := stSum5;
  stSums[5]  := stSum6;  stSums[6]  := stSum7;  stSums[7]  := stSum8;  stSums[8]  := stSum9;  stSums[9]  := stSum10;
  stSums[10] := stSum11; stSums[11] := stSum12; stSums[12] := stSum13; stSums[13] := stSum14; stSums[14] := stSum15;
  stSums[15] := stSum16; stSums[16] := stSum17; stSums[17] := stSum18; stSums[18] := stSum19; stSums[19] := stSum20;
  stSums[20] := stSum21; stSums[21] := stSum22; stSums[22] := stSum23; stSums[23] := stSum24; stSums[24] := stSum25;
  stSums[25] := stSum26; stSums[26] := stSum27; stSums[27] := stSum28; stSums[28] := stSum29; stSums[29] := stSum30;
  stSums[30] := stSum31; stSums[31] := stSum32; stSums[32] := stSum33; stSums[33] := stSum34; stSums[34] := stSum35;
  stSums[35] := stSum36; stSums[36] := stSum37; stSums[37] := stSum38; stSums[38] := stSum39; stSums[39] := stSum40;
  stSums[40] := stSum41; stSums[41] := stSum42;
  // Create shape objects to be used to strike out the sumfields
  for I := 0 to 167 do begin
    shSumBars[I] := TShape.Create(fRSpill.shSumBars[I]);
    shSumBars[I].Parent := Self;
    shSumBars[I].Width := 36;
    shSumBars[I].Height := 3;
    shSumBars[I].Visible := False;
  end;
  // Default startup values
  iPlayers := 2;
  aPlayerNames := DefaultPlayerNames;
  aPlayerColors := DefaultPlayerColors;
  Randomize;
  // Start a new game
  mGameNew.Click;
end;

{ Menu item "Spill > Neit Spill": Start a new game }

procedure TfRSpill.mGameNewClick(Sender: TObject);

var
  X, Y, I, J, K1, K2: Integer;
  OK: Boolean;
  SumCount: array[11..30] of Integer;

begin
  // Clear fields, reset variables
  for I := 0 to 3 do begin
    aPlayerFields[I] := 0; MovePlayer(I, aPlayerFields[I], shFields, shPlayers);
    edPlayerNames[I].ReadOnly := False; edPlayerNames[I].TabStop := True;
    edPlayerNames[I].Color := cldefault;
  end;
  laDice.Font.Color := clDefault; laDice.Font.Style := [];
  laCalc.Font.Color := clDefault;
  laResult.Font.Color := clDefault; laResult.Font.Style := [];
  laMoveBack.Font.Color := clDefault; laMoveBack.Font.Style := [];
  edCalc.Text := ''; edResult.Text := ''; edMoveBack.Text := '';
  for I := 0 to 29 do begin
    aAdditions[I].X := 0;
    aAdditions[I].Y := 0;
  end;
  for I := 11 to 30 do
    SumCount[I] := 0;
  // Generate additions
  for I := 1 to 28 do begin
    repeat
      OK := True;
      // The following code results in pseudo-random addition operands, being within given limits and given values being excluded
      if I <= 8 then begin
        X := I + 6;
        if X = 10 then
          X := 15;
        Y := X;
      end
      else begin
        repeat
          X := Random(13) + 7; Y := Random(13) + 7;
        until (X <> Y) and (X + Y <= 30) and (X <> 10) and (Y <> 10);
      end;
      for J := 0 to I - 1 do begin
        if ((X = aAdditions[J].X) and (Y = aAdditions[J].Y)) or ((X = aAdditions[J].Y) and (Y = aAdditions[J].X)) then
          OK := False;
      end;
      // The following code limits the number of times, the same addition result may be used
      if OK then begin
        if (X + Y >= 24) and (X + Y <= 28) then begin
          if SumCount[X + Y] > 2 then
            OK := False;
        end
        else if (X + Y >= 29) and (X + Y <= 30) then begin
          if SumCount[X + Y] > 0 then
            OK := False;
        end
        else begin
          if SumCount[X + Y] > 1 then
            OK := False;
        end;
      end;
    until OK;
    aAdditions[I].X := X; aAdditions[I].Y := Y;
    SumCount[X + Y] += 1;
  end;
  // The game fields have been filled from Start to Destination; now shuffle the whole (random field positions)
  for I := 1 to 50 do begin
    K1 := Random(28) + 1; K2 := Random(28) + 1;
    X := aAdditions[K1].X; Y := aAdditions[K1].Y;
    aAdditions[K1].X := aAdditions[K2].X;
    aAdditions[K1].Y := aAdditions[K2].Y;
    aAdditions[K2].X := X; aAdditions[K2].Y := Y;
  end;
  // Fill-in the additions
  for I := 0 to 29 do begin
    if I = 0 then
      stFields[I].Caption := 'START'
    else if I = 29 then
      stFields[I].Caption := 'ZIEL'
    else
      stFields[I].Caption := IntToStr(aAdditions[I].X) + ' + ' + IntToStr(aAdditions[I].Y);
  end;
  mOptions.Enabled := True;
  btStart.Caption := 'Start'; btStart.Enabled := True;                         // game is started by pushing the Start button
end;

{ Menu item "Spill > Verloossen": Exit application }

procedure TfRSpill.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Astellungen > Spiller > ...": Select number of players }

procedure TfRSpill.mOptionsPlayers1Click(Sender: TObject);

begin
  mOptionsPlayers1.Checked := True; mOptionsPlayers2.Checked := False; mOptionsPlayers3.Checked := False; mOptionsPlayers4.Checked := False;
  iPlayers := 1;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

procedure TfRSpill.mOptionsPlayers2Click(Sender: TObject);

begin
  mOptionsPlayers1.Checked := False; mOptionsPlayers2.Checked := True; mOptionsPlayers3.Checked := False; mOptionsPlayers4.Checked := False;
  iPlayers := 2;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

procedure TfRSpill.mOptionsPlayers3Click(Sender: TObject);

begin
  mOptionsPlayers1.Checked := False; mOptionsPlayers2.Checked := False; mOptionsPlayers3.Checked := True; mOptionsPlayers4.Checked := False;
  iPlayers := 3;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

procedure TfRSpill.mOptionsPlayers4Click(Sender: TObject);

begin
  mOptionsPlayers1.Checked := False; mOptionsPlayers2.Checked := False; mOptionsPlayers3.Checked := False; mOptionsPlayers4.Checked := True;
  iPlayers := 4;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

{ Menu items "Astellungen > Rechefeeler > ...": Select how many fields the player has to move backward if answer is wrong }

procedure TfRSpill.mOptionsMistake1Click(Sender: TObject);

begin
  mOptionsMistake1.Checked := True; mOptionsMistake2.Checked := False; mOptionsMistake3.Checked := False;
end;

procedure TfRSpill.mOptionsMistake2Click(Sender: TObject);

begin
  mOptionsMistake1.Checked := False; mOptionsMistake2.Checked := True; mOptionsMistake3.Checked := False;
end;

procedure TfRSpill.mOptionsMistake3Click(Sender: TObject);

begin
  mOptionsMistake1.Checked := False; mOptionsMistake2.Checked := False; mOptionsMistake3.Checked := True;
end;

{ Menu items "Astellungen > Am Zil ukommen > ...": Select if must move onto arrival field or may pass beyond }

procedure TfRSpill.mOptionsArrivalWaitClick(Sender: TObject);

begin
  mOptionsArrivalWait.Checked := True; mOptionsArrivalPass.Checked := False;
end;

procedure TfRSpill.mOptionsArrivalPassClick(Sender: TObject);

begin
  mOptionsArrivalWait.Checked := False; mOptionsArrivalPass.Checked := True;
end;

{ Menu item "Hëllef > Hëllef": Open webbrowser with help text displayed }

procedure TfRSpill.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Hëllef > Info": Display application about }

procedure TfRSpill.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Reche-Wierfelspill.' + LineEnding;
  S += 'E Wierfelspill fir Kanner, bei deem een op all Feld, wou een hikënnt, eng Rechenaufgab léise muss.' + LineEnding + LineEnding;
  S += 'Versioun 1.0, © allu, November 2020 - Juni 2021.';
  MessageDlg('Info "Rechespill"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next" pushed: Start game and/or continue with player actually set as playing next }

procedure TfRSpill.btStartClick(Sender: TObject);

var
  I: Integer;

begin
  if btStart.Caption = 'Start' then begin
    // Some initialisations at game start
    for I := 0 to 167 do
      shSumBars[I].Visible := False;
    for I := 0 to 3 do begin
      edPlayerNames[I].ReadOnly := True; edPlayerNames[I].TabStop := False;
    end;
    iRound := 1; iPlayer := 0; iSumBar := 0;
  end;
  // Start gaming sequence for actual player (this will enable the Dice button...)
  StartPlayer(iRound, iPlayer, edPlayerNames);
  // Options disabled during game
  mOptions.Enabled := False;
  // Start button disabled until this player's gaming sequence is terminated
  btStart.Enabled := False;
end;

{ Button "Wierfel" pushed: Roll the dice (code for this in the timer routine) }

procedure TfRSpill.btDiceClick(Sender: TObject);

begin
  iDiceRoll := 0;
  tiRSpill.Enabled := True;                                                    // enable the timer (game continues with code in timer routine)
end;

{ Dice roll timer routine }

procedure TfRSpill.tiRSpillTimer(Sender: TObject);

// This routine not only rolls the dice, but also moves the player's figure according to the dice value
// It also determines if the player has reached the arrival field (end of game in this case)

var
  NewField: Integer;
  Filename: string;
  MustWait, GameOver: Boolean;

begin
  Inc(iDiceRoll);
  if iDiceRoll <= 10 then begin
    // Roll the dice
    iDiceValue := Random(6) + 1;
    Filename := './dices/' + 'dice' + IntToStr(iDiceValue) + '.jpg'; DoDirSeparators(Filename);
    imDice.Picture.LoadFromFile(Filename);
  end
  else begin
    // Dice has been rolled: Disable timer routine, move player and set controls in order to continue game accordingly
    tiRSpill.Enabled := False; MustWait := False; GameOver := False;
    NewField := aPlayerFields[iPlayer] + iDiceValue;
    if NewField >= 29 then begin
      // Player has moved onto arrival field or beyond
      if NewField = 29 then
        GameOver := True                                                       // player wins
      else begin
        // If player moved beyond the arrival field, must wait or wins, depending on game settings
        if mOptionsArrivalPass.Checked then begin
          NewField := 29;                                                      // arrival field
          GameOver := True;                                                    // player wins
        end
        else begin
          NewField -= iDiceValue;                                              // field, where player was before dice roll
          MustWait := True;                                                    // player must wait
        end;
      end;
    end;
    // Move the player according to dice value (and considering arrival field rules)
    aPlayerFields[iPlayer] := NewField;
    MovePlayer(iPlayer, aPlayerFields[iPlayer], shFields, shPlayers);
    btDice.Enabled := False;
    laDice.Font.Color := clDefault; laDice.Font.Style := [];
    if GameOver then begin
      // If the player has reached the arrival field, end the game
      MessageDlg('Enn vum Spill', 'Gewënner ass: ' + aPlayerNames[iPlayer], mtInformation, [mbOK], 0);
    end
    else begin
      if MustWait then begin
        // If the player cannot move, continue with next player
        edMove.Text := 'Kann net réckelen!';
        Inc(iPlayer);
        if iPlayer > iPlayers - 1 then
          iPlayer := 0;
        btStart.Caption := 'Weider'; btStart.Enabled := True;                  // game will continue when Next button is pushed
      end
      else begin
        // Normal case: Player has to resolve the addition question
        // Game will continue when one of the sumfields is clicked
        edMove.Text := IntToStr(iDiceValue);
        if iDiceValue = 1 then
          edMove.Text := edMove.Text + ' Feld'
        else
          edMove.Text := edMove.Text + ' Felder';
        iCalcResult := aAdditions[aPlayerFields[iPlayer]].X + aAdditions[aPlayerFields[iPlayer]].Y;
        edCalc.Text := IntToStr(aAdditions[aPlayerFields[iPlayer]].X) + ' + ' + IntToStr(aAdditions[aPlayerFields[iPlayer]].Y) + ' = ?';
        laCalc.Font.Color := clHighLight; laMove.Font.Color := clHighLight; laResult.Font.Color := clHighLight; laResult.Font.Style := [fsBold];
      end;
    end;
  end;
end;

{ Mouse click on one of the sumfields: Check addition result (and move player's figure backward if wrong answer) }

procedure TfRSpill.stSum1Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 0, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum2Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 1, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum3Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 2, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum4Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 3, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum5Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 4, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum6Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 5, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum7Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 6, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum8Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 7, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum9Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 8, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum10Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 9, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum11Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 10, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum12Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 11, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum13Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 12, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum14Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 13, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum15Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 14, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum16Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 15, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum17Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 16, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum18Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 17, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum19Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 18, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum20Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 19, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer], aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum21Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 20, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer], aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum22Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 21, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum23Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 22, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum24Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 23, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum25Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 24, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum26Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 25, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum27Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 26, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum28Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 27, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum29Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 28, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum30Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 29, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum31Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 30, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum32Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 31, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum33Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 32, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum34Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 33, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum35Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 34, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum36Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 35, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum37Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 36, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum38Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 37, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum39Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 38, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum40Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 39, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum41Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 40, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

procedure TfRSpill.stSum42Click(Sender: TObject);

begin
  CheckAdditionResult(iPlayer, iRound, iPlayers, 41, iDiceValue, iCalcResult, iSumBar, aPlayerColors[iPlayer],
    aPlayerFields, stSums, shFields, shPlayers, shSumBars);
end;

{ Mouse click on one of the color squares: Display the color selection box (where user may choose a new color for his figure) }

procedure TfRSpill.shColor1MouseDown(Sender: TObject);

begin
  if btStart.Enabled and (btStart.Caption = 'Start') then begin
    iColSelPlayer := 0;
    ColorSelectShow(aPlayerColors[iColSelPlayer]);
  end;
end;

procedure TfRSpill.shColor2MouseDown(Sender: TObject);

begin
  if btStart.Enabled and (btStart.Caption = 'Start') then begin
    iColSelPlayer := 1;
    ColorSelectShow(aPlayerColors[iColSelPlayer]);
  end;
end;

procedure TfRSpill.shColor3MouseDown(Sender: TObject);

begin
  if btStart.Enabled and (btStart.Caption = 'Start') then begin
    iColSelPlayer := 2;
    ColorSelectShow(aPlayerColors[iColSelPlayer]);
  end;
end;

procedure TfRSpill.shColor4MouseDown(Sender: TObject);

begin
  if btStart.Enabled and (btStart.Caption = 'Start') then begin
    iColSelPlayer := 3;
    ColorSelectShow(aPlayerColors[iColSelPlayer]);
  end;
end;

{ New color selection (value in color box changed): Attrib new color to player's figure }

procedure TfRSpill.cobColorsChange(Sender: TObject);

var
  N, I: Integer;
  OK: Boolean;

begin
  N := 0; OK := True;
  shPlayerColors[iColSelPlayer].Brush.Color := cobColors.Selected;
  // Check if this color isn't already used
  for I := 0 to iPlayers - 1 do begin
    if shPlayerColors[I].Brush.Color = cobColors.Selected then
      Inc(N);
  end;
  if N <> 1 then
    OK := False;
  if OK then begin
    // Figure's color may be changed
    aPlayerColors[iColSelPlayer] := cobColors.Selected;
  end
  else begin
    // Figure's color can't be changed
    MessageDlg('Rechespill', 'Déi Faarf ass net valabel! Setzen zréck op déi vu virdrun...', mtWarning, [mbOK], 0);
    cobColors.Selected := aPlayerColors[iColSelPlayer];
  end;
  laColors.Visible := False; cobColors.Visible := False;                       // hide color box again
  // Do (figure color) adaptions for actual player
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

{ Player name input (corr. edit field changed): Check name validity }

procedure TfRSpill.edName1EditingDone(Sender: TObject);

begin
  if PlayerNameOK(iPlayers, edName1.Text, edPlayerNames) then
    aPlayerNames[0] := edName1.Text;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

procedure TfRSpill.edName2EditingDone(Sender: TObject);

begin
  if PlayerNameOK(iPlayers, edName2.Text, edPlayerNames) then
    aPlayerNames[1] := edName2.Text;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

procedure TfRSpill.edName3EditingDone(Sender: TObject);

begin
  if PlayerNameOK(iPlayers, edName3.Text, edPlayerNames) then
    aPlayerNames[2] := edName3.Text;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

procedure TfRSpill.edName4EditingDone(Sender: TObject);

begin
  if PlayerNameOK(iPlayers, edName4.Text, edPlayerNames) then
    aPlayerNames[3] := edName4.Text;
  AdaptPlayers(iPlayers, aPlayerNames, aPlayerColors, edPlayerNums, edPlayerNames, shPlayers, shPlayerColors);
end;

end.

