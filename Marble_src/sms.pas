{************************************}
{* Main unit for Marble application *}
{************************************}

unit sms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus, StdCtrls, help;

type
  TBoard = array[0..6, 0..6] of TImage;
  TMarbles = array[0..6, 0..6] of TShape;
  {**********}
  { TfMarble }
  {**********}
  TfMarble = class(TForm)
    mMenu: TMainMenu;
    mGame, mGame1, mGame2, mGame3, mGame4: TMenuItem;
    mGame5, mGame6, mGame7, mGame8, mGameExit: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Shape1: TShape;
    Label1, Label2: TLabel;
    edTotal, edLeft: TEdit;
    imSlot1, imSlot10, imSlot11, imSlot12, imSlot13, imSlot14, imSlot15: TImage;
    imSlot16, imSlot17, imSlot18, imSlot19, imSlot2, imSlot20, imSlot21: TImage;
    imSlot22, imSlot23, imSlot24, imSlot25, imSlot26, imSlot27, imSlot28: TImage;
    imSlot29, imSlot3, imSlot30, imSlot31, imSlot32, imSlot33, imSlot34: TImage;
    imSlot35, imSlot36, imSlot37, imSlot38, imSlot39, imSlot4, imSlot40: TImage;
    imSlot41, imSlot42, imSlot43, imSlot44, imSlot45, imSlot46, imSlot47: TImage;
    imSlot48, imSlot49, imSlot5, imSlot6, imSlot7, imSlot8, imSlot9: TImage;
    shMarble1, shMarble10,shMarble11, shMarble12, shMarble13, shMarble14, shMarble15: TShape;
    shMarble16, shMarble17, shMarble18, shMarble19, shMarble2, shMarble20, shMarble21: TShape;
    shMarble22, shMarble23, shMarble24, shMarble25, shMarble26, shMarble27, shMarble28: TShape;
    shMarble29, shMarble3, shMarble30, shMarble31, shMarble32, shMarble33, shMarble34: TShape;
    shMarble35, shMarble36, shMarble37, shMarble38, shMarble39, shMarble4, shMarble40: TShape;
    shMarble41, shMarble42, shMarble43, shMarble44, shMarble45, shMarble46, shMarble47: TShape;
    shMarble48, shMarble49, shMarble5, shMarble6, shMarble7, shMarble8, shMarble9: TShape;
    procedure FormCreate(Sender: TObject);
    procedure mGame1Click(Sender: TObject);
    procedure mGame2Click(Sender: TObject);
    procedure mGame3Click(Sender: TObject);
    procedure mGame4Click(Sender: TObject);
    procedure mGame5Click(Sender: TObject);
    procedure mGame6Click(Sender: TObject);
    procedure mGame7Click(Sender: TObject);
    procedure mGame8Click(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure imSlot10Click(Sender: TObject);
    procedure imSlot11Click(Sender: TObject);
    procedure imSlot12Click(Sender: TObject);
    procedure imSlot13Click(Sender: TObject);
    procedure imSlot14Click(Sender: TObject);
    procedure imSlot15Click(Sender: TObject);
    procedure imSlot16Click(Sender: TObject);
    procedure imSlot17Click(Sender: TObject);
    procedure imSlot18Click(Sender: TObject);
    procedure imSlot19Click(Sender: TObject);
    procedure imSlot1Click(Sender: TObject);
    procedure imSlot20Click(Sender: TObject);
    procedure imSlot21Click(Sender: TObject);
    procedure imSlot22Click(Sender: TObject);
    procedure imSlot23Click(Sender: TObject);
    procedure imSlot24Click(Sender: TObject);
    procedure imSlot25Click(Sender: TObject);
    procedure imSlot26Click(Sender: TObject);
    procedure imSlot27Click(Sender: TObject);
    procedure imSlot28Click(Sender: TObject);
    procedure imSlot29Click(Sender: TObject);
    procedure imSlot2Click(Sender: TObject);
    procedure imSlot30Click(Sender: TObject);
    procedure imSlot31Click(Sender: TObject);
    procedure imSlot32Click(Sender: TObject);
    procedure imSlot33Click(Sender: TObject);
    procedure imSlot34Click(Sender: TObject);
    procedure imSlot35Click(Sender: TObject);
    procedure imSlot36Click(Sender: TObject);
    procedure imSlot37Click(Sender: TObject);
    procedure imSlot38Click(Sender: TObject);
    procedure imSlot39Click(Sender: TObject);
    procedure imSlot3Click(Sender: TObject);
    procedure imSlot40Click(Sender: TObject);
    procedure imSlot41Click(Sender: TObject);
    procedure imSlot42Click(Sender: TObject);
    procedure imSlot43Click(Sender: TObject);
    procedure imSlot44Click(Sender: TObject);
    procedure imSlot45Click(Sender: TObject);
    procedure imSlot46Click(Sender: TObject);
    procedure imSlot47Click(Sender: TObject);
    procedure imSlot48Click(Sender: TObject);
    procedure imSlot49Click(Sender: TObject);
    procedure imSlot4Click(Sender: TObject);
    procedure imSlot5Click(Sender: TObject);
    procedure imSlot6Click(Sender: TObject);
    procedure imSlot7Click(Sender: TObject);
    procedure imSlot8Click(Sender: TObject);
    procedure imSlot9Click(Sender: TObject);
    procedure shMarble10MouseDown(Sender: TObject);
    procedure shMarble11MouseDown(Sender: TObject);
    procedure shMarble12MouseDown(Sender: TObject);
    procedure shMarble13MouseDown(Sender: TObject);
    procedure shMarble14MouseDown(Sender: TObject);
    procedure shMarble15MouseDown(Sender: TObject);
    procedure shMarble16MouseDown(Sender: TObject);
    procedure shMarble17MouseDown(Sender: TObject);
    procedure shMarble18MouseDown(Sender: TObject);
    procedure shMarble19MouseDown(Sender: TObject);
    procedure shMarble1MouseDown(Sender: TObject);
    procedure shMarble20MouseDown(Sender: TObject);
    procedure shMarble21MouseDown(Sender: TObject);
    procedure shMarble22MouseDown(Sender: TObject);
    procedure shMarble23MouseDown(Sender: TObject);
    procedure shMarble24MouseDown(Sender: TObject);
    procedure shMarble25MouseDown(Sender: TObject);
    procedure shMarble26MouseDown(Sender: TObject);
    procedure shMarble27MouseDown(Sender: TObject);
    procedure shMarble28MouseDown(Sender: TObject);
    procedure shMarble29MouseDown(Sender: TObject);
    procedure shMarble2MouseDown(Sender: TObject);
    procedure shMarble30MouseDown(Sender: TObject);
    procedure shMarble31MouseDown(Sender: TObject);
    procedure shMarble32MouseDown(Sender: TObject);
    procedure shMarble33MouseDown(Sender: TObject);
    procedure shMarble34MouseDown(Sender: TObject);
    procedure shMarble35MouseDown(Sender: TObject);
    procedure shMarble36MouseDown(Sender: TObject);
    procedure shMarble37MouseDown(Sender: TObject);
    procedure shMarble38MouseDown(Sender: TObject);
    procedure shMarble39MouseDown(Sender: TObject);
    procedure shMarble3MouseDown(Sender: TObject);
    procedure shMarble40MouseDown(Sender: TObject);
    procedure shMarble41MouseDown(Sender: TObject);
    procedure shMarble42MouseDown(Sender: TObject);
    procedure shMarble43MouseDown(Sender: TObject);
    procedure shMarble44MouseDown(Sender: TObject);
    procedure shMarble45MouseDown(Sender: TObject);
    procedure shMarble46ChangeBounds(Sender: TObject);
    procedure shMarble46MouseDown(Sender: TObject);
    procedure shMarble47MouseDown(Sender: TObject);
    procedure shMarble48MouseDown(Sender: TObject);
    procedure shMarble49MouseDown(Sender: TObject);
    procedure shMarble4MouseDown(Sender: TObject);
    procedure shMarble5MouseDown(Sender: TObject);
    procedure shMarble6MouseDown(Sender: TObject);
    procedure shMarble7MouseDown(Sender: TObject);
    procedure shMarble8MouseDown(Sender: TObject);
    procedure shMarble9MouseDown(Sender: TObject);
  private
    iGame, iRow, iCol: Integer;
    bFirst, bGameOver: Boolean;
    imBoard: TBoard;
    shMarbles: TMarbles;
  end;

var
  fMarble: TfMarble;

implementation

{$R *.lfm}

{ Reset the board (for currently selected game) }

procedure BoardReset(var Board: TBoard; var Marbles: TMarbles; Game: Integer; out First, GameOver: Boolean);

const
  Games: array[1..8] of string = (
    'Classic', 'Diamomd', 'The Cross', 'Crossbow', 'On the Edge', 'French Classic', 'The Square', 'Longbow'
  );
  NMarbles: array[1..8] of Integer = (
    32, 12, 9, 10, 32, 36, 35, 14
  );

var
  I, J: Integer;

begin
  fMarble.stTitle.Caption := 'Super Marble: ' + Games[Game] + '.';
  fMarble.edTotal.Text := IntToStr(NMarbles[Game]);
  fMarble.edLeft.Text := fMarble.edTotal.Text;
  First := True; GameOver := False;
  // Default layout, modified further down...
  for I := 0 to 6 do begin
    for J := 0 to 6 do begin
      Board[I, J].Visible := True;
      if (Game = 4) or (Game = 8) then begin
        Board[0, 3].Picture.LoadFromFile('goal.jpg'); Board[0, 3].Hint := 'Goal';
        Board[2, 1].Picture.LoadFromFile('slot.jpg'); Board[2, 1].Hint := '';
        Board[2, 2].Picture.LoadFromFile('slot.jpg'); Board[2, 2].Hint := '';
        Board[3, 3].Picture.LoadFromFile('slot.jpg'); Board[3, 3].Hint := '';
      end
      else if Game = 5 then begin
        Board[0, 3].Picture.LoadFromFile('slot.jpg'); Board[0, 3].Hint := '';
        Board[2, 1].Picture.LoadFromFile('goal.jpg'); Board[2, 1].Hint := 'Goal';
        Board[2, 2].Picture.LoadFromFile('slot.jpg'); Board[2, 2].Hint := '';
        Board[3, 3].Picture.LoadFromFile('slot.jpg'); Board[3, 3].Hint := '';
      end
      else if Game = 7 then begin
        Board[0, 3].Picture.LoadFromFile('slot.jpg'); Board[0, 3].Hint := '';
        Board[2, 1].Picture.LoadFromFile('slot.jpg'); Board[2, 1].Hint := '';
        Board[2, 2].Picture.LoadFromFile('goal.jpg'); Board[2, 2].Hint := 'Goal';
        Board[3, 3].Picture.LoadFromFile('slot.jpg'); Board[3, 3].Hint := '';
      end
      else begin
        Board[0, 3].Picture.LoadFromFile('slot.jpg'); Board[0, 3].Hint := '';
        Board[2, 1].Picture.LoadFromFile('slot.jpg'); Board[2, 1].Hint := '';
        Board[2, 2].Picture.LoadFromFile('slot.jpg'); Board[2, 2].Hint := '';
        Board[3, 3].Picture.LoadFromFile('goal.jpg'); Board[3, 3].Hint := 'Goal';
      end;
      Marbles[I, J].Visible := True;
      Marbles[I, J].Brush.Color := clRed;
    end;
  end;
  // Show/hide given slots (board cells) or marbles (depending on game selected)
  if Game <> 7 then begin
    for I := 0 to 1 do begin
      for J := 0 to 1 do begin
        Board[I, J].Visible := False;       Board[I + 5, J].Visible := False;
        Board[I, J + 5].Visible := False;   Board[I + 5, J + 5].Visible := False;
        Marbles[I, J].Visible := False;     Marbles[I + 5, J].Visible := False;
        Marbles[I, J + 5].Visible := False; Marbles[I + 5, J + 5].Visible := False;
      end;
    end;
    if Game = 6 then begin
      Board[1, 1].Visible := True;   Board[1, 5].Visible := True;
      Board[5, 1].Visible := True;   Board[5, 5].Visible := True;
      Marbles[1, 1].Visible := True; Marbles[1, 5].Visible := True;
      Marbles[5, 1].Visible := True; Marbles[5, 5].Visible := True;
    end;
  end
  else begin
    for I := 0 to 6 do begin
      Board[I, 6].Visible := False;   Board[6, I].Visible := False;
      Marbles[I, 6].Visible := False; Marbles[6, I].Visible := False;
    end;
  end;
  if Game = 1 then begin
    Marbles[3, 3].Visible := False;
  end
  else if Game = 2 then begin
    for I := 0 to 6 do begin
      for J := 0 to 6 do begin
        if (I < 2) or (J < 2) or (I > 4) or (J > 4) or ((I = 3) and (J = 3)) then
          Marbles[I, J].Visible := False;
      end;
    end;
    Marbles[3, 1].Visible := True; Marbles[3, 5].Visible := True;
    Marbles[1, 3].Visible := True; Marbles[5, 3].Visible := True;
  end
  else if Game = 3 then begin
    for I := 0 to 6 do begin
      for J := 0 to 6 do begin
        if (I = 0) or (I = 6) or (J = 0) or (J = 6) or ((I <> 3) and (J <> 3)) then
          Marbles[I, J].Visible := False;
      end;
    end;
  end
  else if Game = 4 then begin
    for I := 0 to 6 do begin
      for J := 0 to 6 do begin
        if (I < 3) or (I > 4) or (J = 0) or (J = 6) or ((I = 3) and (J = 3)) then
          Marbles[I, J].Visible := False;
      end;
    end;
    Marbles[5, 3].Visible := True;
  end
  else if Game = 5 then begin
    Marbles[2, 1].Visible := False;
  end
  else if (Game = 5) or (Game = 7) then begin
    Marbles[2, 2].Visible := False;
  end
  else if Game = 6 then begin
    Marbles[3, 3].Visible := False;
  end
  else if Game = 8 then begin
    for I := 0 to 4 do begin
      for J := 2 to 4 do begin
        Marbles[I, J].Visible := False;
      end;
    end;
    Marbles[3, 3].Visible := True;  Marbles[4, 3].Visible := True;
    Marbles[2, 1].Visible := False; Marbles[2, 5].Visible := False;
    Marbles[4, 0].Visible := False; Marbles[4, 6].Visible := False;
    Marbles[6, 2].Visible := False; Marbles[6, 4].Visible := False;
  end;
end;

{ Check if game is over (no more move possible) }

function CheckGameOver(var Board: TBoard; var Marbles: TMarbles): Boolean;

var
  I, J: Integer;
  GameOver: Boolean;

begin
  GameOver := True;
  for I := 0 to 6 do begin
    for J := 0 to 6 do begin
      if Marbles[I, J].Visible then begin
        if (I <= 4) and (Board[I + 1, J].Visible) and (Board[I + 2, J].Visible) then begin
          if (Marbles[I + 1, J].Visible) and (not Marbles[I + 2, J].Visible) then
            GameOver := False;
        end;
        if (I >= 2) and (Board[I - 1, J].Visible) and (Board[I - 2, J].Visible) then begin
          if (Marbles[I - 1, J].Visible) and (not Marbles[I - 2, J].Visible) then
            GameOver := False;
        end;
        if (J <= 4) and (Board[I, J + 1].Visible) and (Board[I, J + 2].Visible) then begin
          if (Marbles[I, J + 1].Visible) and (not Marbles[I, J + 2].Visible) then
            GameOver := False;
        end;
        if (J >= 2) and (Board[I, J - 1].Visible) and (Board[I, J - 2].Visible) then begin
          if (Marbles[I, J - 1].Visible) and (not Marbles[I, J - 2].Visible) then
            GameOver := False;
        end;
      end;
    end;
  end;
  Result := GameOver;
end;

{ Select marble, deselect marble, or move previously selected marble to actually clicked slot}

procedure MarbleMove(var Board: TBoard; var Marbles: TMarbles; Row, Col: Integer; out OldRow, OldCol: Integer; var First, GameOver: Boolean);

var
  RRow, RCol: Integer;
  Valid: Boolean;

begin
  if not GameOver then begin
    if First and Marbles[Row, Col].Visible then begin
      // First click = Select the marble to move
      Marbles[Row, Col].Brush.Color := clLime;
      OldRow := Row; OldCol := Col;
      First := False;
    end
    else begin
      // Second click = deselect previously selected marble, resp. move marble
      if (Row <> OldRow) or (Col <> OldCol) then begin
        // Move marble (if it actually is possible to move this marble)
        Valid := False;
        if not Marbles[Row, Col].Visible then begin
          // If destination slot is empty, check if moving the marble to there is a valid move
          if ((Row = OldRow) and (Abs(Col - OldCol) = 2)) then begin
            if (Col > OldCol) and (Marbles[Row, Col - 1].Visible) then begin
              RRow := Row; RCol := Col - 1;
              Valid := True;
            end
            else if (Col < OldCol) and (Marbles[Row, Col + 1].Visible) then begin
              RRow := Row; RCol := Col + 1;
              Valid := True;
            end;
          end
          else if ((Col = OldCol) and (Abs(Row - OldRow) = 2)) then begin
            if (Row > OldRow) and (Marbles[Row - 1, Col].Visible) then begin
              RRow := Row - 1; RCol := Col;
              Valid := True;
            end
            else if (Row < OldRow) and (Marbles[Row + 1, Col].Visible) then begin
              RRow := Row + 1; RCol := Col;
              Valid := True;
            end;
          end;
          if Valid then begin
            // Move the marble; remove the marble jumped over from the board
            Marbles[OldRow, OldCol].Brush.Color := clRed;
            Marbles[OldRow, OldCol].Visible := False;
            Marbles[Row, Col].Visible := True;
            Marbles[RRow, RCol].Visible := False;
            fMarble.edLeft.Text := IntToStr(StrToInt(fMarble.edLeft.Text) - 1);
            First := True;
            if StrToInt(fMarble.edLeft.Text) = 1 then begin
              // One single marble left
              GameOver := True;
              if Board[Row, Col].Hint = 'Goal' then begin
                // Remaining marble is in the goal
                MessageDlg('Super Marble', 'Congratulations! You win!', mtInformation, [mbOK], 0);
              end
              else begin
                // Remaining marble is not in the goal
                MessageDlg('Super Marble', 'The remaining marble should be in the goal... You loose!', mtInformation, [mbOK], 0);
              end;
            end
            else begin
              // More than 1 marble left; continue game unless none of the remaining marbles may be moved
              GameOver := CheckGameOver(Board, Marbles);
              if GameOver then
                MessageDlg('Super Marble', 'There should be a single marble left... You loose!', mtInformation, [mbOK], 0);
            end;
          end;
        end;
      end
      else begin
        // Deselect previously selected marble
        Marbles[Row, Col].Brush.Color := clRed;
        First := True;
      end;
    end;
  end;
end;

{**********}
{ TfMarble }
{**********}

{ Application start: Initialization }

procedure TfMarble.FormCreate(Sender: TObject);

begin
  // Create array with slot images
  imBoard[0, 0] := imSlot1;  imBoard[0, 1] := imSlot2;  imBoard[0, 2] := imSlot3;
  imBoard[0, 3] := imSlot4;  imBoard[0, 4] := imSlot5;  imBoard[0, 5] := imSlot6;  imBoard[0, 6] := imSlot7;
  imBoard[1, 0] := imSlot8;  imBoard[1, 1] := imSlot9;  imBoard[1, 2] := imSlot10;
  imBoard[1, 3] := imSlot11; imBoard[1, 4] := imSlot12; imBoard[1, 5] := imSlot13; imBoard[1, 6] := imSlot14;
  imBoard[2, 0] := imSlot15; imBoard[2, 1] := imSlot16; imBoard[2, 2] := imSlot17;
  imBoard[2, 3] := imSlot18; imBoard[2, 4] := imSlot19; imBoard[2, 5] := imSlot20; imBoard[2, 6] := imSlot21;
  imBoard[3, 0] := imSlot22; imBoard[3, 1] := imSlot23; imBoard[3, 2] := imSlot24;
  imBoard[3, 3] := imSlot25; imBoard[3, 4] := imSlot26; imBoard[3, 5] := imSlot27; imBoard[3, 6] := imSlot28;
  imBoard[4, 0] := imSlot29; imBoard[4, 1] := imSlot30; imBoard[4, 2] := imSlot31;
  imBoard[4, 3] := imSlot32; imBoard[4, 4] := imSlot33; imBoard[4, 5] := imSlot34; imBoard[4, 6] := imSlot35;
  imBoard[5, 0] := imSlot36; imBoard[5, 1] := imSlot37; imBoard[5, 2] := imSlot38;
  imBoard[5, 3] := imSlot39; imBoard[5, 4] := imSlot40; imBoard[5, 5] := imSlot41; imBoard[5, 6] := imSlot42;
  imBoard[6, 0] := imSlot43; imBoard[6, 1] := imSlot44; imBoard[6, 2] := imSlot45;
  imBoard[6, 3] := imSlot46; imBoard[6, 4] := imSlot47; imBoard[6, 5] := imSlot48; imBoard[6, 6] := imSlot49;
  // Create array with marble shapes
  shMarbles[0, 0] := shMarble1;  shMarbles[0, 1] := shMarble2;  shMarbles[0, 2] := shMarble3;
  shMarbles[0, 3] := shMarble4;  shMarbles[0, 4] := shMarble5;  shMarbles[0, 5] := shMarble6;  shMarbles[0, 6] := shMarble7;
  shMarbles[1, 0] := shMarble8;  shMarbles[1, 1] := shMarble9;  shMarbles[1, 2] := shMarble10;
  shMarbles[1, 3] := shMarble11; shMarbles[1, 4] := shMarble12; shMarbles[1, 5] := shMarble13; shMarbles[1, 6] := shMarble14;
  shMarbles[2, 0] := shMarble15; shMarbles[2, 1] := shMarble16; shMarbles[2, 2] := shMarble17;
  shMarbles[2, 3] := shMarble18; shMarbles[2, 4] := shMarble19; shMarbles[2, 5] := shMarble20; shMarbles[2, 6] := shMarble21;
  shMarbles[3, 0] := shMarble22; shMarbles[3, 1] := shMarble23; shMarbles[3, 2] := shMarble24;
  shMarbles[3, 3] := shMarble25; shMarbles[3, 4] := shMarble26; shMarbles[3, 5] := shMarble27; shMarbles[3, 6] := shMarble28;
  shMarbles[4, 0] := shMarble29; shMarbles[4, 1] := shMarble30; shMarbles[4, 2] := shMarble31;
  shMarbles[4, 3] := shMarble32; shMarbles[4, 4] := shMarble33; shMarbles[4, 5] := shMarble34; shMarbles[4, 6] := shMarble35;
  shMarbles[5, 0] := shMarble36; shMarbles[5, 1] := shMarble37; shMarbles[5, 2] := shMarble38;
  shMarbles[5, 3] := shMarble39; shMarbles[5, 4] := shMarble40; shMarbles[5, 5] := shMarble41; shMarbles[5, 6] := shMarble42;
  shMarbles[6, 0] := shMarble43; shMarbles[6, 1] := shMarble44; shMarbles[6, 2] := shMarble45;
  shMarbles[6, 3] := shMarble46; shMarbles[6, 4] := shMarble47; shMarbles[6, 5] := shMarble48; shMarbles[6, 6] := shMarble49;
  // Start with "Classic" game
  mGame1.Click;
end;

{ Menu item "Game > Classic": Start "Classic" game }

procedure TfMarble.mGame1Click(Sender: TObject);

begin
  iGame := 1;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);

end;

{ Menu item "Game > Diamond": Start "Diamond" game }

procedure TfMarble.mGame2Click(Sender: TObject);

begin
  iGame := 2;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > The Cross": Start "The Cross" game }

procedure TfMarble.mGame3Click(Sender: TObject);

begin
  iGame := 3;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > Crossbow": Start "Crossbow" game }

procedure TfMarble.mGame4Click(Sender: TObject);

begin
  iGame := 4;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > On the Edge": Start "On the Edge" game }

procedure TfMarble.mGame5Click(Sender: TObject);

begin
  iGame := 5;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > French Classic": Start "French Classic" game }

procedure TfMarble.mGame6Click(Sender: TObject);

begin
  iGame := 6;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > The Square": Start "The Square" game }

procedure TfMarble.mGame7Click(Sender: TObject);

begin
  iGame := 7;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > Longbow": Start "Longbow" game }

procedure TfMarble.mGame8Click(Sender: TObject);

begin
  iGame := 8;
  BoardReset(imBoard, shMarbles, iGame, bFirst, bGameOver);
end;

{ Menu item "Game > Exit": Exit application }

procedure TfMarble.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display application help }

procedure TfMarble.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfMarble.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Super Marble.' + LineEnding;
  S += 'A game that consists in eliminating all but one marbles with the remaining marble being located in the goal.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, December 2023.';
  MessageDlg('About "Marble"', S, mtInformation, [mbOK], 0);
end;

{ User click on slot image: Perform "moving marble" action }

procedure TfMarble.imSlot1Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot2Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot3Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot4Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot5Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot6Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot7Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot8Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot9Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot10Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot11Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot12Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot13Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot14Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot15Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot16Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot17Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot18Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot19Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot20Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot21Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot22Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot23Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot24Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot25Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot26Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot27Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot28Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot29Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot30Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot31Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot32Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot33Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot34Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot35Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot36Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot37Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot38Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot39Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot40Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot41Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot42Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot43Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot44Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot45Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot46Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot47Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot48Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.imSlot49Click(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 6, iRow, iCol, bFirst, bGameOver);
end;

{ User click on marble shape: Perform "moving marble" action }

procedure TfMarble.shMarble1MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble2MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble3MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble4MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble5MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble6MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble7MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 0, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble8MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble9MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble10MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble11MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble12MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble13MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble14MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 1, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble15MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble16MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble17MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble18MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble19MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble20MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble21MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 2, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble22MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble23MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble24MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble25MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble26MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble27MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble28MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 3, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble29MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble30MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble31MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble32MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble33MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble34MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble35MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 4, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble36MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble37MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble38MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble39MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble40MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble41MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble42MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 5, 6, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble43MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 0, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble44MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 1, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble45MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 2, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble46ChangeBounds(Sender: TObject);
begin

end;

procedure TfMarble.shMarble46MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 3, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble47MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 4, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble48MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 5, iRow, iCol, bFirst, bGameOver);
end;

procedure TfMarble.shMarble49MouseDown(Sender: TObject);

begin
  MarbleMove(imBoard, shMarbles, 6, 6, iRow, iCol, bFirst, bGameOver);
end;

end.

