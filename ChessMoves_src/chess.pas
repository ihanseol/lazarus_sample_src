{****************************************}
{* Main unit for ChessMoves application *}
{****************************************}

unit chess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, position, help;

type
  // Invalid king moves
  TCkPos = record
    KingCol, KingRow: Integer;
    CheckPiece: string;
    CheckCol, CheckRow: Integer;
  end;
  TCheckPos = array[1..8] of TCkPos;
  // Invalid pieces moves
  TCk2Pos = record
    CheckPiece: string;
    PieceCol, PieceRow: Integer;
    MoveCol, MoveRow: Integer;
  end;
  TCheck2Pos = array[1..8] of TCk2Pos;
  // Pieces moves with checkmate
  TCkmatePos = record
    CheckPiece: string;
    PieceCol, PieceRow: Integer;
    CheckCol, CheckRow: Integer;
  end;
  TCheckmatePos = array[1..8] of TCkmatePos;
  // Chess pieces description (16 for each player)
  TPiece = record
    Name: string;
    Value: Integer;
    Col, Row: Integer;
  end;
  TPieces = array[1..16] of TPiece;
  // Chess board field description (8x8)
  TField = record
    FieldColour: Char;
    Piece: string;
    PieceColour: Char;
    PiecePic: TImage;
  end;
  TBoard = array[1..8, 1..8] of TField;
  // Chess piece images (16 for each player)
  TPieceImages = array[1..16] of TImage;
  // Piece or field selection (highlighting) shapes
  TSelectShapes = array[1..32, 1..4] of TShape;
  // Chess piece move description
  TMove = record
    Col, Row: Integer;
    Capture, Castling, Check: Boolean;
  end;
  TMoves = array of TMove;
  {*********}
  { TfChess }
  {*********}
  TfChess = class(TForm)
    mMenu: TMainMenu;
    mChess: TMenuItem;
    mChessSetup, mChessPieces, mChessCastling: TMenuItem;
    mChessBegin, mChessGame, mChessExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsColor, mOptionsColorWhite, mOptionsColorBlack: TMenuItem;
    mOptionsMess, mOptionsErrors: TMenuItem;
    mHelp: TMenuItem;
    mHelpQuick, mHelpChess, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    Label1, laHints: TLabel;
    imBoard: TImage;
    imField1, imField2, imField3, imField4, imField5, imField6, imField7, imField8: TImage;
    imField9, imField10, imField11, imField12, imField13, imField14, imField15, imField16: TImage;
    imField17, imField18, imField19, imField20, imField21, imField22, imField23, imField24: TImage;
    imField25, imField26, imField27, imField28, imField29, imField30, imField31, imField32: TImage;
    imField33, imField34, imField35, imField36, imField37, imField38, imField39, imField40: TImage;
    imField41, imField42, imField43, imField44, imField45, imField46, imField47, imField48: TImage;
    imField49, imField50, imField51, imField52, imField53, imField54, imField55, imField56: TImage;
    imField57, imField58, imField59, imField60, imField61, imField62, imField63, imField64: TImage;
    imPiecesKing, imPiecesQueen, imPiecesRook1, imPiecesRook2: TImage;
    imPiecesKnight1, imPiecesKnight2, imPiecesBishop1, imPiecesBishop2: TImage;
    imPiecesPion1, imPiecesPion2, imPiecesPion3, imPiecesPion4: TImage;
    imPiecesPion5, imPiecesPion6, imPiecesPion7, imPiecesPion8: TImage;
    edInstructions, edInstructions2, edHint: TMemo;
    btShow: TButton;
    btHint: TButton;
    btPawns: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mChessSetupClick(Sender: TObject);
    procedure mChessPiecesClick(Sender: TObject);
    procedure mChessCastlingClick(Sender: TObject);
    procedure mChessBeginClick(Sender: TObject);
    procedure mChessGameClick(Sender: TObject);
    procedure mChessExitClick(Sender: TObject);
    procedure mOptionsColorWhiteClick(Sender: TObject);
    procedure mOptionsColorBlackClick(Sender: TObject);
    procedure mOptionsMessClick(Sender: TObject);
    procedure mOptionsErrorsClick(Sender: TObject);
    procedure mHelpQuickClick(Sender: TObject);
    procedure mHelpChessClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btHintClick(Sender: TObject);
    procedure btPawnsClick(Sender: TObject);
    procedure imField10Click(Sender: TObject);
    procedure imField11Click(Sender: TObject);
    procedure imField12Click(Sender: TObject);
    procedure imField13Click(Sender: TObject);
    procedure imField14Click(Sender: TObject);
    procedure imField15Click(Sender: TObject);
    procedure imField16Click(Sender: TObject);
    procedure imField17Click(Sender: TObject);
    procedure imField18Click(Sender: TObject);
    procedure imField19Click(Sender: TObject);
    procedure imField1Click(Sender: TObject);
    procedure imField20Click(Sender: TObject);
    procedure imField21Click(Sender: TObject);
    procedure imField22Click(Sender: TObject);
    procedure imField23Click(Sender: TObject);
    procedure imField24Click(Sender: TObject);
    procedure imField25Click(Sender: TObject);
    procedure imField26Click(Sender: TObject);
    procedure imField27Click(Sender: TObject);
    procedure imField28Click(Sender: TObject);
    procedure imField29Click(Sender: TObject);
    procedure imField2Click(Sender: TObject);
    procedure imField30Click(Sender: TObject);
    procedure imField31Click(Sender: TObject);
    procedure imField32Click(Sender: TObject);
    procedure imField33Click(Sender: TObject);
    procedure imField34Click(Sender: TObject);
    procedure imField35Click(Sender: TObject);
    procedure imField36Click(Sender: TObject);
    procedure imField37Click(Sender: TObject);
    procedure imField38Click(Sender: TObject);
    procedure imField39Click(Sender: TObject);
    procedure imField3Click(Sender: TObject);
    procedure imField40Click(Sender: TObject);
    procedure imField41Click(Sender: TObject);
    procedure imField42Click(Sender: TObject);
    procedure imField43Click(Sender: TObject);
    procedure imField44Click(Sender: TObject);
    procedure imField45Click(Sender: TObject);
    procedure imField46Click(Sender: TObject);
    procedure imField47Click(Sender: TObject);
    procedure imField48Click(Sender: TObject);
    procedure imField49Click(Sender: TObject);
    procedure imField4Click(Sender: TObject);
    procedure imField50Click(Sender: TObject);
    procedure imField51Click(Sender: TObject);
    procedure imField52Click(Sender: TObject);
    procedure imField53Click(Sender: TObject);
    procedure imField54Click(Sender: TObject);
    procedure imField55Click(Sender: TObject);
    procedure imField56Click(Sender: TObject);
    procedure imField57Click(Sender: TObject);
    procedure imField58Click(Sender: TObject);
    procedure imField59Click(Sender: TObject);
    procedure imField5Click(Sender: TObject);
    procedure imField60Click(Sender: TObject);
    procedure imField61Click(Sender: TObject);
    procedure imField62Click(Sender: TObject);
    procedure imField63Click(Sender: TObject);
    procedure imField64Click(Sender: TObject);
    procedure imField6Click(Sender: TObject);
    procedure imField7Click(Sender: TObject);
    procedure imField8Click(Sender: TObject);
    procedure imField9Click(Sender: TObject);
    procedure imPiecesBishop1Click(Sender: TObject);
    procedure imPiecesBishop2Click(Sender: TObject);
    procedure imPiecesKingClick(Sender: TObject);
    procedure imPiecesKnight1Click(Sender: TObject);
    procedure imPiecesKnight2Click(Sender: TObject);
    procedure imPiecesPion1Click(Sender: TObject);
    procedure imPiecesPion2Click(Sender: TObject);
    procedure imPiecesPion3Click(Sender: TObject);
    procedure imPiecesPion4Click(Sender: TObject);
    procedure imPiecesPion5Click(Sender: TObject);
    procedure imPiecesPion6Click(Sender: TObject);
    procedure imPiecesPion7Click(Sender: TObject);
    procedure imPiecesPion8Click(Sender: TObject);
    procedure imPiecesQueenClick(Sender: TObject);
    procedure imPiecesRook1Click(Sender: TObject);
    procedure imPiecesRook2Click(Sender: TObject);
  private
    iPieceCol, iPieceRow, iPieceSelect, iPiecesSel: Integer;
    sOption, sSelect: string;
    cColour: Char;
    bShowHint: Boolean;
    aBoard: TBoard;
    aWhite, aBlack: TPieces;
    aCheckWhite, aCheckBlack: TCheckPos;
    aCheck2White, aCheck2Black: TCheck2Pos;
    aCheckmateWhite, aCheckmateBlack: TCheckmatePos;
    imPieces: TPieceImages;
    shSelect: TSelectShapes;
  end;

const
  PieceNames: array[1..6] of string = (
    'king', 'queen', 'rook', 'knight', 'bishop', 'pawn'
  );
  PieceValues: array[1..6] of Integer = (                                      // actually not used
    0, 7, 4, 3, 3, 1
  );
  AllPieces: array[1..16] of string = (
    'king', 'queen', 'rook', 'rook', 'knight', 'knight', 'bishop', 'bishop',
    'pawn', 'pawn', 'pawn', 'pawn', 'pawn', 'pawn', 'pawn', 'pawn'
  );

var
  fChess: TfChess;

implementation

{$R *.lfm}

{ Get piece name (for given piece symbol) }

function GetPieceName(Symbol: Char): string;

var
  Piece: string;

begin
  case Symbol of
    'k': Piece := 'king';
    'q': Piece := 'queen';
    'r': Piece := 'rook';
    'n': Piece := 'knight';
    'b': Piece := 'bishop';
    'p': Piece := 'pawn';
  end;
  Result := Piece;
end;

{ Get piece colour (for given color symbol) }

function GetPieceColour(Symbol: Char; Opponent: Boolean): string;

var
  Colour: string;

begin
  if Opponent then begin
    // Opponent's piece color
    if Symbol = 'w' then
      Colour := 'black'
    else
      Colour := 'white';
  end
  else begin
    // Player's piece color
    if Symbol = 'w' then
      Colour := 'white'
    else
      Colour := 'black';
  end;
  Result := Colour;
end;

{ Get board column (as integer) }

function GetColumn(SCol: Char): Integer;

var
  Col: Integer;

begin
  Col := Ord(SCol) - Ord('a') + 1;
  if not (Col in [1..8]) then
    Col := -1;
  Result := Col;
end;

{ Get board row (as integer) }

function GetRow(SRow: Char): Integer;

var
  Row, Code: Integer;

begin
  Val(SRow, Row, Code);
  if Code <> 0 then
    Row := -1;
  Result := Row;
end;

{ Clear the chess board }

procedure ClearBoard(var Board: TBoard);

// The sub removes all piece images from the board; in the board array, the pieces' name is set to an empty string
// The sub also removes all shapes, used to highlight the fields

var
  I, J: Integer;
  Filename: string;

begin
  // Clear the board
  for I := 1 to 8 do begin
    for J := 1 to 8 do begin
      Board[I, J].Piece := '';
      Filename := './res/empty_' + Board[I, J].FieldColour + '.jpg'; DoDirSeparators(Filename);
      Board[I, J].PiecePic.Picture.LoadFromFile(Filename);
    end;
  end;
  // Remove "highlight shapes" (by making them "invisible")
  for I := 1 to 32 do begin
    for J := 1 to 4 do begin
      fChess.shSelect[I, J].Visible := False;
    end;
  end;
end;

{ Setup the chess board (with actual pieces) }

procedure DisplayBoard(var Board: TBoard; var White, Black: TPieces);

// The sub displays the piece images and also updates the board array

var
  C, R, I: Integer;
  Filename: string;

begin
  ClearBoard(Board);                                                           // clear the board
  for I := 1 to 16 do begin
    if (White[I].Col in [1..8]) and (White[I].Row in [1..8]) then begin
      // If this white piece is actually on the board, place it at its actual position
      C := White[I].Col; R := White[I].Row;
      Board[C, R].Piece := White[I].Name;
      Board[C, R].PieceColour := 'w';
      Filename := './res/' + White[I].Name + '_w' + Board[C, R].FieldColour + '.jpg'; DoDirSeparators(Filename);
      Board[C, R].PiecePic.Picture.LoadFromFile(Filename);
    end;
  end;
  for I := 1 to 16 do begin
    if (Black[I].Col in [1..8]) and (Black[I].Row in [1..8]) then begin
      // If this black piece is actually on the board, place it at its actual position
      C := Black[I].Col; R := Black[I].Row;
      Board[C, R].Piece := Black[I].Name;
      Board[C, R].PieceColour := 'b';
      Filename := './res/' + Black[I].Name + '_b' + Board[C, R].FieldColour + '.jpg'; DoDirSeparators(Filename);
      Board[C, R].PiecePic.Picture.LoadFromFile(Filename);
    end;
  end;
end;

{ Display "player pieces" (of given color) }

procedure PlayerPiecesDisplay(var Pieces: TPieceImages; Colour: Char);

var
  I: Integer;
  Filename: string;
  FieldColour: Char;

begin
  if Colour = 'w' then
    FieldColour := 'b'
  else
    FieldColour := 'w';
  for I := 1 to 16 do begin
    Filename := './res/' + AllPieces[I] + '_' + Colour + FieldColour + '.jpg'; DoDirSeparators(Filename);
    Pieces[I].Picture.LoadFromFile(Filename); Pieces[I].Enabled := True;
  end;
end;

{ Fill-in the "player pieces" arrays }

procedure PlayerPiecesFill(var White, Black: TPieces);

var
  I: Integer;

begin
  White[1].Col := 5; Black[1].Col := 5;
  White[2].Col := 4; Black[2].Col := 4;
  White[3].Col := 1; Black[3].Col := 1; White[4].Col := 8; Black[4].Col := 8;
  White[5].Col := 2; Black[5].Col := 2; White[6].Col := 7; Black[6].Col := 7;
  White[7].Col := 3; Black[7].Col := 3; White[8].Col := 6; Black[8].Col := 6;
  for I := 1 to 8 do begin
    White[I].Row := 1; Black[I].Row := 8;
  end;
  for I := 9 to 16 do begin
    White[I].Col := I - 8; Black[I].Col := I - 8;
    White[I].Row := 2; Black[I].Row := 7;
  end;
end;

{ Piece or board field selection (highlighting) }

procedure HighLight(SelectShapes: TSelectShapes; IX: Integer; PieceImage: TImage; Colour: TColor);

begin
  SelectShapes[IX, 1].Left := PieceImage.Left;       SelectShapes[IX, 1].Top := PieceImage.Top;
  SelectShapes[IX, 2].Left := PieceImage.Left;       SelectShapes[IX, 2].Top := PieceImage.Top + 56;
  SelectShapes[IX, 3].Left := PieceImage.Left;       SelectShapes[IX, 3].Top := PieceImage.Top;
  SelectShapes[IX, 4].Left := PieceImage.Left + 56;  SelectShapes[IX, 4].Top := PieceImage.Top;
  SelectShapes[IX, 1].Pen.Color := Colour;           SelectShapes[IX, 1].Brush.Color := Colour;
  SelectShapes[IX, 2].Pen.Color := Colour;           SelectShapes[IX, 2].Brush.Color := Colour;
  SelectShapes[IX, 3].Pen.Color := Colour;           SelectShapes[IX, 3].Brush.Color := Colour;
  SelectShapes[IX, 4].Pen.Color := Colour;           SelectShapes[IX, 4].Brush.Color := Colour;
  SelectShapes[IX, 1].Visible := True;               SelectShapes[IX, 2].Visible := True;
  SelectShapes[IX, 3].Visible := True;               SelectShapes[IX, 4].Visible := True;
end;

{ Piece or board field deselection (unhighlighting) }

procedure UnHighlight(SelectShapes: TSelectShapes; IX: Integer);

var
  I: Integer;

begin
  for I := 1 to 4 do
    SelectShapes[IX, I].Visible := False;                                      // simply hide the shapes
end;

{ Get pieces and special moves info for given game position }

procedure GamePosition(SPieces: TPosPieces; SCheck, SCheck2, SCheckmate: TPosCheck; var White, Black: TPieces;
  out CheckWhite, CheckBlack: TCheckPos; out Check2White, Check2Black: TCheck2Pos; out CheckmateWhite, CheckmateBlack: TCheckmatePos);

// Transformation of the string arrays to arrays of records

var
  Col, Row, I, JW, JB: Integer;
  Colour: Char;
  Check: TCkPos;
  Check2: TCk2Pos;
  Checkmate: TCkmatePos;

begin
  for I := 1 to 16 do begin
    White[I].Col := 0; White[I].Row := 0;
    Black[I].Col := 0; Black[I].Row := 0;
  end;
  // Fill-in arrays with white and black pieces and their position on the board
  for I := 1 to 32 do begin
    Col := GetColumn(Copy(SPieces[I], 2, 1)[1]); Row := GetRow(RightStr(SPieces[I], 1)[1]);
    if (Col in [1..8]) and (Row in [1..8]) then begin
      if I <= 16 then begin
        White[I].Col := Col;
        White[I].Row := Row;
      end
      else begin
        Black[I - 16].Col := Col;
        Black[I - 16].Row := Row;
      end;
    end;
  end;
  // Clear special moves arrays
  for I := 1 to 8 do begin
    CheckWhite[I].CheckPiece := ''; CheckBlack[I].CheckPiece := '';
    Check2White[I].CheckPiece := ''; Check2Black[I].CheckPiece := '';
    CheckmateWhite[I].CheckPiece := ''; CheckmateBlack[I].CheckPiece := '';
  end;
  // Fill "forbidden king moves" array
  JW := 0; JB := 0;
  for I := 1 to 8 do begin
    if SCheck[I] <> '' then begin
      Colour := LeftStr(SCheck[I], 1)[1];
        Check.KingCol := GetColumn(Copy(SCheck[I], 2, 1)[1]); Check.KingRow := GetRow(Copy(SCheck[I], 3, 1)[1]);
      Check.CheckPiece := GetPieceName(Copy(SCheck[I], 4, 1)[1]);
      Check.CheckCol := GetColumn(Copy(SCheck[I], 5, 1)[1]); Check.CheckRow := GetRow(Copy(SCheck[I], 6, 1)[1]);
        if Colour = 'w' then begin
        Inc(JW);
        CheckWhite[JW] := Check;
      end
      else begin
        Inc(JB);
        CheckBlack[JB] := Check;
      end;
    end;
  end;
  // Fill "forbidden piece moves" array
  JW := 0; JB := 0;
  for I := 1 to 8 do begin
    if SCheck2[I] <> '' then begin
      Colour := LeftStr(SCheck2[I], 1)[1];
      Check2.CheckPiece := GetPieceName(Copy(SCheck2[I], 2, 1)[1]);
      Check2.PieceCol := GetColumn(Copy(SCheck2[I], 3, 1)[1]); Check2.PieceRow := GetRow(Copy(SCheck2[I], 4, 1)[1]);
      Check2.MoveCol := GetColumn(Copy(SCheck2[I], 5, 1)[1]);  Check2.MoveRow  := GetRow(Copy(SCheck2[I], 6, 1)[1]);
      if Colour = 'w' then begin
        Inc(JW);
        Check2White[JW] := Check2;
      end
      else begin
        Inc(JB);
        Check2Black[JB] := Check2;
      end;
    end;
  end;
  // Fill "checkmate moves" array
  JW := 0; JB := 0;
  for I := 1 to 8 do begin
    if SCheckmate[I] <> '' then begin
      Colour := LeftStr(SCheckmate[I], 1)[1];
      Checkmate.CheckPiece := GetPieceName(Copy(SCheckmate[I], 2, 1)[1]);
      Checkmate.PieceCol := GetColumn(Copy(SCheckmate[I], 3, 1)[1]); Checkmate.PieceRow := GetRow(Copy(SCheckmate[I], 4, 1)[1]);
      Checkmate.CheckCol := GetColumn(Copy(SCheckmate[I], 5, 1)[1]); Checkmate.CheckRow := GetRow(Copy(SCheckmate[I], 6, 1)[1]);
      if Colour = 'w' then begin
        Inc(JW);
        CheckmateWhite[JW] := Checkmate;
      end
      else begin
        Inc(JB);
        CheckmateBlack[JB] := Checkmate;
      end;
    end;
  end;
end;

{ Check "valid moves" array to determine those, that would put the own king into check }

procedure ForbiddenPieceMoves(Col, Row: Integer; var Check2Pos: TCheck2Pos; var ValidMoves: TMoves);

// These forbidden moves remain in the "valid moves" array, but are marked
// by setting the field column and row to a negative value

var
  I, J: Integer;

begin
  for I := 0 to Length(ValidMoves) - 1 do begin
    for J := 1 to 8 do begin
      if Check2Pos[J].CheckPiece <> '' then begin
        if (Col = Check2Pos[J].PieceCol) and (Row = Check2Pos[J].PieceRow) and
          (ValidMoves[I].Col = Check2Pos[J].MoveCol) and (ValidMoves[I].Row = Check2Pos[J].MoveRow) then begin
            ValidMoves[I].Col := -ValidMoves[I].Col; ValidMoves[I].Row := -ValidMoves[I].Row;
        end;
      end;
    end;
  end;
end;

{ Add piece move to the "valid moves" array }

procedure UpdateMoves(Col, Row: Integer; Capture, Castling, Check: Boolean; var Moves: TMoves);

begin
  SetLength(Moves, Length(Moves) + 1);
  Moves[Length(Moves) - 1].Col := Col;
  Moves[Length(Moves) - 1].Row := Row;
  Moves[Length(Moves) - 1].Capture := Capture;
  Moves[Length(Moves) - 1].Castling := Castling;
  Moves[Length(Moves) - 1].Check := Check;
end;

{ ---------------------------------------------------- Chess pieces moves ---------------------------------------------------- }

{ Get valid moves of a king placed at a given position }

procedure GetKingMoves(Board: TBoard; KingColour: Char; Col, Row: Integer; var CheckPos: TCheckPos; var ValidMoves: TMoves);

var
  NewCol, NewRow, I, J: Integer;
  IsValid, Capture: Boolean;

begin
  for NewCol := Col - 1 to Col + 1 do begin
    for NewRow := Row - 1 to Row + 1 do begin
      if (NewCol >= 1) and (NewCol <= 8) and (NewRow >= 1) and (NewRow <= 8) and ((NewCol <> Col) or (NewRow <> Row)) then begin
        // Valid king moves
        IsValid := True; Capture := False;
        if Board[NewCol, NewRow].Piece <> '' then begin
          // Move field contains a piece
          if Board[NewCol, NewRow].PieceColour <> KingColour then begin
            // Piece = opponent's piece
            if Board[NewCol, NewRow].Piece = 'king' then
              // Piece = opponent's king: can't move there
              IsValid := False
            else
              // Piece = other opponent's piece: capture
              Capture := True
          end
          else
            // Piece = own piece: can't move there
            IsValid := False;
        end;
        // Add the move to the "valid moves" array
        if IsValid then
          UpdateMoves(NewCol, NewRow, Capture, False, False, ValidMoves);
      end;
    end;
  end;
  // King castling moves
  if (KingColour = 'w') and (Col = 5) and (Row = 1) then begin
    if (Board[8, 1].Piece = 'rook') and (Board[8, 1].PieceColour = KingColour) and (Board[6, 1].Piece = '') and (Board[7, 1].Piece = '') then
      UpdateMoves(7, 1, False, True, False, ValidMoves)
    else if (Board[1, 1].Piece = 'rook') and (Board[1, 1].PieceColour = KingColour) and (Board[2, 1].Piece = '') and (Board[3, 1].Piece = '') and (Board[4, 1].Piece = '') then
      UpdateMoves(3, 1, False, True, False, ValidMoves)
  end
  else if (KingColour = 'b') and (Col = 5) and (Row = 8) then begin
    if (Board[8, 8].Piece = 'rook') and (Board[8, 8].PieceColour = KingColour) and (Board[6, 8].Piece = '') and (Board[7, 8].Piece = '') then
      UpdateMoves(7, 8, False, True, False, ValidMoves)
    else if (Board[1, 8].Piece = 'rook') and (Board[1, 8].PieceColour = KingColour) and (Board[2, 8].Piece = '') and (Board[3, 8].Piece = '') and (Board[4, 8].Piece = '') then
      UpdateMoves(3, 8, False, True, False, ValidMoves)
  end;
  // Valid moves include forbidden moves, i.e. moves/captures that would move the king into check
  // Let these as elements of the "valid moves" array, but set field column and row to a negative value
  for I := 0 to Length(ValidMoves) - 1 do begin
    for J := 1 to 8 do begin
      if CheckPos[J].CheckPiece <> '' then begin
        if (ValidMoves[I].Col = CheckPos[J].KingCol) and (ValidMoves[I].Row = CheckPos[J].KingRow) then begin
          ValidMoves[I].Col := -ValidMoves[I].Col; ValidMoves[I].Row := -ValidMoves[I].Row;
        end;
      end;
    end;
  end;
end;

{ Get valid moves of a rook placed at a given position }

procedure GetRookMoves(Board: TBoard; RookColour: Char; Col, Row: Integer; var Check2Pos: TCheck2Pos; var ValidMoves: TMoves);

var
  NewCol, NewRow, I: Integer;
  IsValid, Capture, Check: Boolean;

begin
  for I := 1 to 4 do begin
    NewCol := Col; NewRow := Row;
    repeat
      // Move the rook horizontally or vertically as far as allowed (i.e. until board border or presence of another piece)
      case I of
        // The 2 horizontals and 2 verrticals
        1: NewCol += 1;
        2: NewCol -= 1;
        3: NewRow += 1;
        4: NewRow -= 1;
      end;
      IsValid := False;
      if (NewCol >= 1) and (NewCol <= 8) and (NewRow >= 1) and (NewRow <= 8) and ((NewCol <> Col) or (NewRow <> Row)) then begin
        // Valid rook move
        IsValid := True; Capture := False; Check := False;
        if Board[NewCol, NewRow].Piece <> '' then begin
          // Move field contains a piece
          if Board[NewCol, NewRow].PieceColour <> RookColour then begin
            // Piece = opponent's piece
            if Board[NewCol, NewRow].Piece = 'king' then begin
              // Piece = opponent's king: check
              IsValid := False;
              Check := True;
            end
            else
              // Piece = other opponent's piece: capture
              Capture := True
          end
          else
            // Piece = own piece: can't move there
            IsValid := False;
        end;
        // Add the move to the "valid moves" array
        if IsValid or Check then
          UpdateMoves(NewCol, NewRow, Capture, False, Check, ValidMoves);
      end;
    until not IsValid or Capture or Check;
  end;
  // Check if moving the piece would set the own king into check
  ForbiddenPieceMoves(Col, Row, Check2Pos, ValidMoves);
end;

{ Get valid moves of a bishop placed at a given position }

procedure GetBishopMoves(Board: TBoard; BishopColour: Char; Col, Row: Integer; var Check2Pos: TCheck2Pos; var ValidMoves: TMoves);

var
  NewCol, NewRow, I: Integer;
  IsValid, Capture, Check: Boolean;

begin
  for I := 1 to 4 do begin
    NewCol := Col; NewRow := Row;
    repeat
      // Move the bishop diagonally as far as allowed (i.e. until board border or presence of another piece)
      case I of
        // The 4 diagonals
        1: begin NewCol += 1; NewRow += 1; end;
        2: begin NewCol += 1; NewRow -= 1; end;
        3: begin NewCol -= 1; NewRow += 1; end;
        4: begin NewCol -= 1; NewRow -= 1; end;
      end;
      IsValid := False;
      if (NewCol >= 1) and (NewCol <= 8) and (NewRow >= 1) and (NewRow <= 8) and ((NewCol <> Col) or (NewRow <> Row)) then begin
        // Valid bishop move
        IsValid := True; Capture := False; Check := False;
        if Board[NewCol, NewRow].Piece <> '' then begin
          // Move field contains a piece
          if Board[NewCol, NewRow].PieceColour <> BishopColour then begin
            // Piece = opponent's piece
            if Board[NewCol, NewRow].Piece = 'king' then begin
              // Piece = opponent's king: check
              IsValid := False;
              Check := True;
            end
            else
              // Piece = other opponent's piece: capture
              Capture := True
          end
          else
            // Piece is own piece: can't move there
            IsValid := False;
        end;
        // Add the move to the "valid moves" array
        if IsValid or Check then
          UpdateMoves(NewCol, NewRow, Capture, False, Check, ValidMoves);
      end;
    until not IsValid or Capture or Check;
  end;
  // Check if moving the piece would set the own king into check
  ForbiddenPieceMoves(Col, Row, Check2Pos, ValidMoves);
end;

{ Get valid moves of a queen placed at a given position }

procedure GetQueenMoves(Board: TBoard; QueenColour: Char; Col, Row: Integer; var Check2Pos: TCheck2Pos; var ValidMoves: TMoves);

begin
  GetRookMoves(Board, QueenColour, Col, Row, Check2Pos, ValidMoves);           // horizontal and vertical moves
  GetBishopMoves(Board, QueenColour, Col, Row, Check2Pos, ValidMoves);         // diagonal moves
end;

{ Get valid moves of a knight placed at a given position }

procedure GetKnightMoves(Board: TBoard; KnightColour: Char; Col, Row: Integer; var Check2Pos: TCheck2Pos; var ValidMoves: TMoves);

var
  NewCol, NewRow: Integer;
  IsValid, Capture, Check: Boolean;

begin
  for NewCol := Col - 2 to Col + 2 do begin
    for NewRow := Row - 2 to Row + 2 do begin
      if (NewCol >= 1) and (NewCol <= 8) and (NewRow >= 1) and (NewRow <= 8) and ((NewCol <> Col) or (NewRow <> Row)) then begin
        if ((Abs(Col - NewCol) = 1) and (Abs(Row - NewRow) = 2)) or ((Abs(Col - NewCol) = 2) and (Abs(Row - NewRow) = 1)) then begin
          // Valid knight move
          IsValid := True; Capture := False; Check := False;
          if Board[NewCol, NewRow].Piece <> '' then begin
            // Move field contains a piece
            if Board[NewCol, NewRow].PieceColour <> KnightColour then begin
              // Piece is an opponent's piece
              if Board[NewCol, NewRow].Piece = 'king' then begin
                // Piece = opponent's king: check
                IsValid := False;
                Check := True;
              end
              else
                // Piece = other opponent's piece: capture
                Capture := True
            end
            else
              // Piece is own piece: can't move there
              IsValid := False;
          end;
          if IsValid or Check then
            // Add the move to the "valid moves" array
            UpdateMoves(NewCol, NewRow, Capture, False, Check, ValidMoves);
        end;
      end;
    end;
  end;
  // Check if moving the piece would set the own king into check
  ForbiddenPieceMoves(Col, Row, Check2Pos, ValidMoves);
end;

{ Get valid moves of a pawn placed at a given position }

procedure GetPawnMoves(Board: TBoard; PawnColour: Char; Col, Row: Integer; var Check2Pos: TCheck2Pos; var ValidMoves: TMoves);

var
  NewRow, NewCol, Row1, Row2: Integer;
  IsValid, Capture, Check: Boolean;

begin
  if PawnColour = 'w' then begin
    Row1 := Row; Row2 := Row1 + 2;
  end
  else begin
    Row1 := Row - 2; Row2 := Row;
  end;
  for NewCol := Col - 1 to Col + 1 do begin
    for NewRow := Row1 to Row2 do begin
      if (NewCol >= 1) and (NewCol <= 8) and (NewRow >= 1) and (NewRow <= 8) and ((NewCol <> Col) or (NewRow <> Row)) then begin
        IsValid := True; Capture := False; Check := False;
        if Board[NewCol, NewRow].Piece = '' then begin
          // Empty move field: Pawn may move straight forward
          if NewCol <> Col then
            IsValid := False
          else begin
            if PawnColour = 'w' then begin
              if Row = 2 then begin
                // White pawn on row 2 may move 1 or 2 fields
                if (NewRow - Row <> 1) and (NewRow - Row <> 2) then
                  IsValid := False
              end
              else begin
                // White pawn on other rows may move 1 single field
                if NewRow - Row <> 1 then
                  IsValid := False
              end;
            end
            else begin
              if Row = 7 then begin
                // Black pawn on row 7 may move 1 or 2 fields
                if (NewRow - Row <> -1) and (NewRow - Row <> -2) then
                  IsValid := False
              end
              else begin
                // Black pawn on other rows may move 1 single field
                if NewRow - Row <> -1 then
                  IsValid := False
              end;
            end;
          end;
        end
        else begin
          // Non-empty move field: Pawn may capture (an opponent piece) diagonally
          if Abs(NewCol - Col) <> 1 then
            // Move must be one field to the left or to the right
            IsValid := False
          else if (PawnColour = 'w') and (NewRow - Row <> 1) then
            // White pawn move must be one field in 1->8 direction
            IsValid := False
          else if (PawnColour = 'b') and (NewRow - Row <> -1) then
            // Black pawn move must be one field in 8->1 direction
            IsValid := False
          else if Board[NewCol, NewRow].PieceColour = PawnColour then
            // You can't capture your own pieces
            IsValid := False
          else begin
            // If the piece on the move field is the opponent's king, it's check
            // otherwise the pawn may capture the piece
            if Board[NewCol, NewRow].Piece = 'king' then begin
              IsValid := False;
              Check := True;
            end
            else
              Capture := True
          end;
        end;
        // Add the move to the "valid moves" array
        if IsValid or Check then
          UpdateMoves(NewCol, NewRow, Capture, False, Check, ValidMoves);
      end;
    end;
  end;
  // Check if moving the piece would set the own king into check
  ForbiddenPieceMoves(Col, Row, Check2Pos, ValidMoves);
end;

{ ---------------------------------------------------------------------------------------------------------------------------- }

{ Determine all valid (incl. forbidden) moves for a given peace starting from a given position }

procedure GetValidMoves(Board: TBoard; Col, Row: Integer; var CheckWhite, CheckBlack: TCheckPos; var Check2White, Check2Black: TCheck2Pos;
  out ValidMoves: TMoves);

var
  Colour: Char;
  Check2: TCheck2Pos;

begin
  SetLength(ValidMoves, 0);
  Colour := Board[Col, Row].PieceColour;
  // Get all valid moves, depending on the piece kind
  if Board[Col, Row].Piece = 'king' then begin
    if Colour = 'w' then
      GetKingMoves(Board, Colour, Col, Row, CheckWhite, ValidMoves)
    else
      GetKingMoves(Board, Colour, Col, Row, CheckBlack, ValidMoves);
  end
  else begin
    if Colour = 'w' then
      Check2 := Check2White
    else
      Check2 := Check2Black;
    if Board[Col, Row].Piece = 'queen' then
      GetQueenMoves(Board, Colour, Col, Row, Check2, ValidMoves)
    else if Board[Col, Row].Piece = 'rook' then
      GetRookMoves(Board, Colour, Col, Row, Check2, ValidMoves)
    else if Board[Col, Row].Piece = 'bishop' then
      GetBishopMoves(Board, Colour, Col, Row, Check2, ValidMoves)
    else if Board[Col, Row].Piece = 'knight' then
      GetKnightMoves(Board, Colour, Col, Row, Check2, ValidMoves)
    else
      GetPawnMoves(Board, Colour, Col, Row, Check2, ValidMoves);
  end;
end;

{ Move a given chess piece starting from a given field }

procedure PieceMove(Board: TBoard; PCol, PRow, Col, Row: Integer; var CheckWhite, CheckBlack: TCheckPos;
  var Check2White, Check2Black: TCheck2Pos; var CheckmateWhite, CheckmateBlack: TCheckmatePos);

var
  I, J: Integer;
  SColour: string;
  IsValid, IsCheck, IsCheck2, IsCastling, IsCheckmate: Boolean;
  Colour: TColor;
  ValidMoves: TMoves;
  CheckmatePos: TCheckmatePos;
  Field: TField;

begin
  // Get all valid moves for this piece
  GetValidMoves(Board, PCol, PRow, CheckWhite, CheckBlack, Check2White, Check2Black, ValidMoves);
  IsValid := False; IsCastling := False; IsCheck := False; IsCheck2 := False;
  // Analyze the valid moves (detecting special and forbidden moves)
  for I := 0 to Length(ValidMoves) - 1 do begin
    if (Col = Abs(ValidMoves[I].Col)) and (Row = Abs(ValidMoves[I].Row)) then begin
      // User clicked on a valid (possibly forbidden field)
      if (Col = ValidMoves[I].Col) and (Row = ValidMoves[I].Row) then begin
        // User clicked on a valid (perhaps special, but not forbidden field)
        IsValid := True;
        // Moving to this field may be move to empty field, castling move, or opponent's piece capture
        if ValidMoves[I].Castling then
          IsCastling := True;
        if ValidMoves[I].Capture then
          Colour := clYellow
        else
          Colour := clLime;
      end
      else begin
        // User clicked on a forbidden field (these fields are included in the "valid moves" array, but column and row values have been set negative)
        ValidMoves[I].Col := -ValidMoves[I].Col; ValidMoves[I].Row := -ValidMoves[I].Row;
        if Board[PCol, PRow].Piece = 'king' then
          // King trying to move to a field, where it would be in check
          IsCheck := True
        else
          // Moving the piece would set the king into check
          IsCheck2 := True;
      end;
    end;
  end;
  // Highlight invalid (and forbidden) moves using red shapes
  if not IsValid then
    Colour := clRed;
  // Remove all highlighting markers
  for I := 3 to 32 do
    Unhighlight(fChess.shSelect, I);
  // Highlight the field, where the piece moved (or tried to move) to
  Highlight(fChess.shSelect, 2, Board[Col, Row].PiecePic, Colour);
  // Display special resp. forbidden moves messages
  if (Board[PCol, PRow].Piece = 'king') and IsCheck then begin
    if not fChess.mOptionsErrors.Checked then
      MessageDlg('Invalid move', 'The king may never move himself into check!', mtError, [mbOK], 0);
  end
  else if (Board[PCol, PRow].Piece = 'king') and IsCastling then begin
    if not fChess.mOptionsErrors.Checked then
      MessageDlg('Special move', 'This move starts the castling procedure!', mtInformation, [mbOK], 0)
  end
  else if (Board[PCol, PRow].Piece <> 'king') and IsCheck2 then begin
    if not fChess.mOptionsErrors.Checked and IsCheck2 then begin
      if Board[PCol, PRow].PieceColour = 'w' then
        SColour := 'white'
      else
        SColour := 'black';
      MessageDlg('Invalid move', 'This move would put the ' + SColour + ' king into check!', mtError, [mbOK], 0);
    end;
  end
  else if IsValid then begin
    if not fChess.mOptionsMess.Checked and (Board[PCol, PRow].Piece = 'pawn') and (Board[PCol, PRow].PieceColour = 'w') and (Row = 8) then
      MessageDlg('Promotion', 'A white pawn arriving at row 8 will be promoted!', mtInformation, [mbOK], 0)
    else if not fChess.mOptionsMess.Checked and (Board[PCol, PRow].Piece = 'pawn') and (Board[PCol, PRow].PieceColour = 'b') and (Row = 1) then
      MessageDlg('Promotion', 'A black pawn arriving at row 1 will be promoted!', mtInformation, [mbOK], 0)
    else begin
      // Determine if the move done sets the opponent's king into check
      // This is actually done by recalculating the valid moves for the moved piece from its position after the initial move
      Field := Board[Col, Row];                                                // save actual field values
      Board[Col, Row] := Board[PCol, PRow];
      GetValidMoves(Board, Col, Row, CheckWhite, CheckBlack, Check2White, Check2Black, ValidMoves);
      // For all valid moves, check if there is one that would put the opponent's king into check
      for I := 0 to Length(ValidMoves) - 1 do begin
        if ValidMoves[I].Check then begin
          // The opponent's king actually is set into check
          if Board[Col, Row].PieceColour = 'w' then begin
            CheckmatePos := CheckmateWhite;                                    // array with checkmate values for white piece moving (thus for black king)
            SColour := 'black';
          end
          else begin
            CheckmatePos := CheckmateBlack;                                    // array with checkmate values for black piece moving (thus for white king)
            SColour := 'white';
          end;
          if not fChess.mOptionsMess.Checked then begin
            IsCheckmate := False;
            // Determine if piece position is element of checkmate positions array
            for J := 1 to 8 do begin
              if CheckmatePos[J].CheckPiece <> '' then begin
                if (PCol = CheckmatePos[J].PieceCol) and (PRow = CheckmatePos[J].PieceRow) and
                  (Col = CheckmatePos[J].CheckCol) and (Row = CheckmatePos[J].CheckRow) then
                    // The move done by the user is a checkmate position
                    IsCheckmate := True;
              end;
            end;
            // Display check resp. checkmate message
            if IsCheckmate then
              MessageDlg('Check', 'This move puts the ' + SColour + ' king into checkmate!', mtInformation, [mbOK], 0)
            else
              MessageDlg('Check', 'This move puts the ' + SColour + ' king into check!', mtInformation, [mbOK], 0);
          end;
        end;
      end;
      Board[Col, Row] := Field;                                                // restore field values
    end;
  end;
end;

{ User click on a given chess board field }

procedure FieldClicked(var Board: TBoard; var White, Black: TPieces; Option: string; var Select: string;
  Piece, Col, Row: Integer; var CheckWhite, CheckBlack: TCheckPos; var Check2White, Check2Black: TCheck2Pos;
  var CheckmateWhite, CheckmateBlack: TCheckmatePos; var PiecesSel: Integer; out PCol, PRow: Integer; var ShowHint: Boolean);

// Action to be taken depends on actual feature (var: Option) and actual situation (var: Select)

var
  IX, I: Integer;
  Filename, SColour, Side: string;
  Colour, Colour2: Char;
  ShapeColour: TColor;
  Pieces: TPieces;

begin
  if (Option = 'begin') or (Option = 'position') then begin
    // "Game opening" and "Game position"
    // ----------------------------------
    if Select = 'piece' then begin
      // Piece selection
      if Board[Col, Row].Piece <> '' then begin                                // there must be a piece at this field
        Highlight(fChess.shSelect, 1, Board[Col, Row].PiecePic, clBlue);       // highlight the piece's field
        UnHighlight(fChess.shSelect, 2);                                       // unhighlight the last move-to field
        PCol := Col; PRow := Row;
        Select := 'field';                                                     // next click will be a field selection
      end;
    end
    else begin
      // Field selection
      if (Col = PCol) and (Row = PRow) then begin
        // User clicked on the field with the actually selected piece: Unselect the piece and remove all move highlightings
        for I := 1 to 32 do
          UnHighlight(fChess.shSelect, I);
        PCol := 0; PRow := 0;
        Select := 'piece';                                                     // next click will be a piece selection
        fChess.edHint.Clear;
        ShowHint := True;                                                      // clear the hints
      end
      else begin
        // User clicked on any other field: Move the actually selected piece to this field
        PieceMove(Board, PCol, PRow, Col, Row, CheckWhite, CheckBlack, Check2White, Check2Black, CheckmateWhite, CheckmateBlack );
      end;
    end;
  end
  else if Option = 'moves' then begin
    // "Piece movement"
    // ----------------
    if fChess.mOptionsColorWhite.Checked then
      Colour := 'w'
    else
      Colour := 'b';
    if Select <> '' then begin
      if Select = 'piece' then begin
        // Piece selection
        if (Board[Col, Row].Piece = '') and (Piece <> 0) then begin
          // There is a piece selected and the board field is empty: Place the selected piece onto the board (if position is valid)
          if (PCol > 0) and (PRow > 0) then begin
            // Remove the piece actually on the board
            Board[PCol, PRow].Piece := '';
            Filename := './res/' + 'empty_' + Board[PCol, PRow].FieldColour + '.jpg'; DoDirSeparators(Filename);
            Board[PCol, PRow].PiecePic.Picture.LoadFromFile(Filename);
          end;
          // Place the selected piece onto the board or display error message
          if not fChess.mOptionsErrors.Checked and (Piece > 8) and (Colour = 'w') and (Row = 1) then
            // Invalid position: White pawns at row 1
            MessageDlg('Invalid position', 'A white pawn can never be at row 1!', mtError, [mbOK], 0)
          else if not fChess.mOptionsErrors.Checked and (Piece > 8) and (Colour = 'b') and (Row = 8) then
            // Invalid position: Black pawns at row 8
            MessageDlg('Invalid position', 'A black pawn can never be at row 8!', mtError, [mbOK], 0)
          else begin
            // Valid positions: Place the actual piece onto the board
            Board[Col, Row].Piece := AllPieces[Piece];
            Board[Col, Row].PieceColour := Colour;
            Filename := './res/' + AllPieces[Piece] + '_' + Colour + Board[Col, Row].FieldColour + '.jpg'; DoDirSeparators(Filename);
            Board[Col, Row].PiecePic.Picture.LoadFromFile(Filename);
            // Information messages for pawn promotion
            if not fChess.mOptionsMess.Checked and (Piece > 8) and (Colour = 'w') and (Row = 8) then
              MessageDlg('Promotion', 'A white pawn arrived at row 8 will be promoted!', mtInformation, [mbOK], 0)
            else if not fChess.mOptionsMess.Checked and (Piece > 8) and (Colour = 'b') and (Row = 1) then
              MessageDlg('Promotion', 'A black pawn arrived at row 1 will be promoted!', mtInformation, [mbOK], 0);
            PCol := Col; PRow := Row;
            Select := 'field';                                                 // next click will be field selection
          end;
        end;
      end
      else begin
        // Field selection
        if (Col = PCol) and (Row = PRow) then begin
          // User clicked on the field with the actually selected piece: Remove the piece and remove all move highlightings
          Board[Col, Row].Piece := '';
          Filename := './res/' + 'empty_' + Board[Col, Row].FieldColour + '.jpg'; DoDirSeparators(Filename);
          Board[Col, Row].PiecePic.Picture.LoadFromFile(Filename);
          for I := 2 to 32 do                                                  // first element of highlight shapes = "player pieces" selection
            UnHighlight(fChess.shSelect, I);
          PCol := 0; PRow := 0;
          Select := 'piece';                                                   // next click will be a piece selection
        end
        else
          // User clicked on any other field: Move the actual piece to that field
          PieceMove(Board, PCol, PRow, Col, Row, CheckWhite, CheckBlack, Check2White, Check2Black, CheckmateWhite, CheckmateBlack);
      end;
    end;
  end
  else if Option = 'setup' then begin
    // "Chessboard setup"
    // ------------------
    if Select = 'field' then begin                                             // select variable will be set to "done" if setup is terminated
      // Setup will be for white or black (as user has selected)
      if fChess.mOptionsColorWhite.Checked then begin
        Colour := 'w'; Pieces := White; Colour2 := 'b';
      end
      else begin
        Colour := 'b'; Pieces := Black; Colour2 := 'w';
      end;
      // Error message if piece is placed outside the 2 first resp. 2 last rows
      if not fChess.mOptionsErrors.Checked and not (Row in [1, 2, 7, 8]) then
        MessageDlg('Invalid position', 'At board setup, all pieces must be on rows 1-2 resp. 7-8!', mtError, [mbOK], 0)
      else if not fChess.mOptionsErrors.Checked and (Colour = 'w') and (not (Row in [1, 2])) then
        MessageDlg('Invalid position', 'At board setup, all white pieces must be on rows 1-2!', mtError, [mbOK], 0)
      else if not fChess.mOptionsErrors.Checked and (Colour = 'b') and (not (Row in [7, 8])) then
        MessageDlg('Invalid position', 'At board setup, all black pieces must be on rows 7-8!', mtError, [mbOK], 0)
      // Valid position: Place the actual piece there
      else begin
        if (Board[Col, Row].Piece = '') and (Piece <> 0) then begin            // there must be a piece selected and the board field must be empty
          IX := -1;
          for I := 1 to 16 do begin
            if (Col = Pieces[I].Col) and (Row = Pieces[I].Row) then
              IX := I;
          end;
          if IX <> -1 then begin
            // Place the piece onto the board or mark the field as "invalid position"
            if Pieces[IX].Name = AllPieces[Piece] then begin
              // Correct position for actual piece: Place the piece to the clicked field
              Board[Col, Row].Piece := AllPieces[Piece];
              Board[Col, Row].PieceColour := Colour;
              Filename := './res/' + AllPieces[Piece] + '_' + Colour + Board[Col, Row].FieldColour + '.jpg'; DoDirSeparators(Filename);
              Board[Col, Row].PiecePic.Picture.LoadFromFile(Filename);
              // Remove the actual "player piece" and its highlighting
              Filename := './res/' + 'empty_' + Colour2 + '.jpg'; DoDirSeparators(Filename);
              UnHighlight(fChess.shSelect, 1);
              fChess.imPieces[Piece].Picture.LoadFromFile(Filename); fChess.imPieces[Piece].Enabled := False;
              // Remove last highlighting (invalid position) on the board
              if (PCol > 0) and (PRow > 0) then
                UnHighlight(fChess.shSelect, 2);
              Inc(PiecesSel);                                                  // counter of pieces set up so far
              if PiecesSel = 16 then begin
                // All pieces have been setup
                SColour := GetPieceColour(Colour, False);
                if not fChess.mOptionsMess.Checked then
                  MessageDlg('Setup done', 'You have successfully set up the ' + SColour + ' pieces!', mtInformation, [mbOK], 0);
                Inc(PiecesSel);                                                // this makes sure that the message is displayed only once
              end;
              Select := 'piece';                                               // next click will be a piece selection
            end
            else begin
              // Invalid position for actual piece: Highlight the field with red markers
              Highlight(fChess.shSelect, 2, Board[Col, Row].PiecePic, clRed);
            end;
          end;
        end;
      end;
    end;
    PCol := Col; PRow := Row;                                                  // save actual field's column and row
  end
  else if Option = 'castling' then begin
    // "Castling"
    // ----------
    if Select = 'king' then begin
      // King selection
      if Board[Col, Row].Piece = 'king' then begin
        // User has selected the king: Highlight the king's field
        Highlight(fChess.shSelect, 1, Board[Col, Row].PiecePic, clBlue);
        PCol := Col; PRow := Row;                                              // save actual king position
        Select := 'field';                                                     // next click will be a field selection
      end
      else if Board[Col, Row].Piece = 'rook' then begin
        // User has selected a rook: Mark the selection as invalid
        Highlight(fChess.shSelect, 1, Board[Col, Row].PiecePic, clRed);
        if not fChess.mOptionsErrors.Checked then
          MessageDlg('Invalid move', 'Castling starts with moving the king!', mtError, [mbOK], 0);
      end;
    end
    else if Select = 'rook' then begin
      // Rook selection
      if Board[Col, Row].Piece = 'rook' then begin
        // User has selected the rook: Move it if it is the correct one for actual castling side
        if (fChess.mOptionsColorWhite.Checked and (Board[7, 1].Piece = 'king')) or (fChess.mOptionsColorBlack.Checked and (Board[7, 8].Piece = 'king')) then
          Side := 'kingside'
        else if (fChess.mOptionsColorWhite.Checked and (Board[3, 1].Piece = 'king')) or (fChess.mOptionsColorBlack.Checked and (Board[3, 8].Piece = 'king')) then
          Side := 'queenside';
        if ((Side = 'kingside') and (Col = 8)) or ((Side = 'queenside') and (Col = 1)) then begin
          // The rook selected is the correct one
          PCol := Col; PRow := Row;
          Select := 'field';
          ShapeColour := clBlue;
        end
        else begin
          if not fChess.mOptionsErrors.Checked then
            MessageDlg('Invalid move', 'You must move the other rook for ' + Side + ' castling', mtError, [mbOK], 0);
          ShapeColour := clRed;
        end;
        // Highlight the rook's field (in red if it is the bad rook)
        Highlight(fChess.shSelect, 1, Board[Col, Row].PiecePic, ShapeColour);
      end
      else if Board[Col, Row].Piece = 'king' then begin
        // User has selected the king: Mark the selection as invalid
        Highlight(fChess.shSelect, 1, Board[Col, Row].PiecePic, clRed);
        if not fChess.mOptionsErrors.Checked then
          MessageDlg('Invalid move', 'The king has already been moved!', mtError, [mbOK], 0);
      end;
    end
    else if Select = 'field' then begin
      // Field selection
      Colour := Board[PCol, PRow].PieceColour;
      if (PCol = 5) and (Board[Col, Row].Piece = '') then begin
        // Actual piece selected = king: Move the king to the clicked field or mark the field as "invalid position"
        if (((Colour = 'w') and (Row = 1)) or ((Colour = 'b') and (Row = 8))) and ((Col = 3) or (Col = 7)) then begin
          // Correct king position: Move the king
          Board[PCol, PRow].Piece := '';
          Filename := './res/' + 'empty_' + Board[PCol, PRow].FieldColour + '.jpg'; DoDirSeparators(Filename);
          Board[PCol, PRow].PiecePic.Picture.LoadFromFile(Filename);
          Board[Col, Row].Piece := 'king';
          Filename := './res/' + 'king_' + Colour + Board[Col, Row].FieldColour + '.jpg'; DoDirSeparators(Filename);
          Board[Col, Row].PiecePic.Picture.LoadFromFile(Filename);
          // Remove highlighting shapes
          Unhighlight(fChess.shSelect, 1); Unhighlight(fChess.shSelect, 2);
          // Display castling information
          if not fChess.mOptionsMess.Checked then begin
            if (Col = 7) then
              MessageDlg('Castling', 'This move starts kingside castling', mtInformation, [mbOK], 0)
            else
              MessageDlg('Castling', 'This move starts queenside castling', mtInformation, [mbOK], 0);
          end;
          Select := 'rook';                                                    // next click will be rook selection
        end
        else begin
          // Bad king position: Highlight the clicked field with red shapes
          Highlight(fChess.shSelect, 2, Board[Col, Row].PiecePic, clRed);
        end;
      end
      else if Board[Col, Row].Piece = '' then begin
        // Actual piece selected = rook: Move the rook to the clicked field or mark the field as "invalid position"
        if ((PCol = 8) and (((Colour = 'w') and (Row = 1)) or ((Colour = 'b') and (Row = 8))) and (Col = 6)) or
           ((PCol = 1) and (((Colour = 'w') and (Row = 1)) or ((Colour = 'b') and (Row = 8))) and (Col = 4)) then begin
          // Correct rook position: Move the rook
          Board[PCol, PRow].Piece := '';
          Filename := './res/' + 'empty_' + Board[PCol, PRow].FieldColour + '.jpg'; DoDirSeparators(Filename);
          Board[PCol, PRow].PiecePic.Picture.LoadFromFile(Filename);
          Board[Col, Row].Piece := 'rook';
          Filename := './res/' + 'rook_' + Colour + Board[Col, Row].FieldColour + '.jpg'; DoDirSeparators(Filename);
          Board[Col, Row].PiecePic.Picture.LoadFromFile(Filename);
          // Remove highlighting shapes
          Unhighlight(fChess.shSelect, 1); Unhighlight(fChess.shSelect, 2);
          // Display castling information
          if not fChess.mOptionsMess.Checked then begin
            if (Col = 6) then
              MessageDlg('Castling', 'This move ends kingside castling', mtInformation, [mbOK], 0)
            else
              MessageDlg('Castling', 'This move ends queenside castling', mtInformation, [mbOK], 0);
          end;
          Select := 'done';                                                    // castling move is terminated
        end
        else begin
          // Bad rook position: Highlight the field with red markers
          Highlight(fChess.shSelect, 2, Board[Col, Row].PiecePic, clRed);
        end;
      end;
    end;
  end;
end;

{ User click on a given "player piece" }

procedure PieceClicked(Option: string; var Select: string; Pieces: TPieceImages; Piece: Integer; var PieceSel: Integer; var ShowHint: Boolean);

begin
  if (Option = 'setup') or ((Option = 'moves') and (Select = 'piece')) then begin
    if Piece <> PieceSel then begin
      HighLight(fChess.shSelect, 1, Pieces[Piece], clBlue);
      PieceSel := Piece;
      if Option = 'setup' then begin
        Select := 'field';
      end
      else begin
        Select := 'piece';
        fChess.edHint.Clear; ShowHint := True;                                 // clear hints
      end;
    end
  end;
end;

{*********}
{ TfChess }
{*********}

{ Application start: Initialisation }

procedure TfChess.FormCreate(Sender: TObject);

var
  C, R, I, J: Integer;
  Colour: Char;
  Filename: string;

begin
  // Create shape objects (for marking selected field/piece)
  for I := 1 to 32 do begin                                                    // 32 markers,
    for J := 1 to 4 do begin                                                   // each of them consisting of 4 elements
      shSelect[I, J] := TShape.Create(fChess.shSelect[I, J]);
      shSelect[I, J].Parent := Self;
      if (J = 1) or (J = 2) then begin
        shSelect[I, J].Width := 60;
        shSelect[I, J].Height := 4;
      end
      else begin
        shSelect[I, J].Width := 4;
        shSelect[I, J].Height := 60;
      end;
      if I = 1 then begin                                                      // the first marker will be used for piece selection
        shSelect[I, J].Brush.Color := clBlue;
        shSelect[I, J].Pen.Color := clBlue;
      end;
      shSelect[I, J].Visible := False;
    end;
  end;
  // Create array with piece images
  imPieces[1] := imPiecesKing; imPieces[2] := imPiecesQueen; imPieces[3] := imPiecesRook1; imPieces[4] := imPiecesRook2;
  imPieces[5] := imPiecesKnight1; imPieces[6] := imPiecesKnight2; imPieces[7] := imPiecesBishop1; imPieces[8] := imPiecesBishop2;
  imPieces[9] := imPiecesPion1; imPieces[10] := imPiecesPion2; imPieces[11] := imPiecesPion3; imPieces[12] := imPiecesPion4;
  imPieces[13] := imPiecesPion5; imPieces[14] := imPiecesPion6; imPieces[15] := imPiecesPion7; imPieces[16] := imPiecesPion8;
  // Init chess board: define images for empty field or chess piece picture
  aBoard[1, 1].PiecePic := imField1; aBoard[2, 1].PiecePic := imField2; aBoard[3, 1].PiecePic := imField3; aBoard[4, 1].PiecePic := imField4;
  aBoard[5, 1].PiecePic := imField5; aBoard[6, 1].PiecePic := imField6; aBoard[7, 1].PiecePic := imField7; aBoard[8, 1].PiecePic := imField8;
  aBoard[1, 2].PiecePic := imField9; aBoard[2, 2].PiecePic := imField10; aBoard[3, 2].PiecePic := imField11; aBoard[4, 2].PiecePic := imField12;
  aBoard[5, 2].PiecePic := imField13; aBoard[6, 2].PiecePic := imField14; aBoard[7, 2].PiecePic := imField15; aBoard[8, 2].PiecePic := imField16;
  aBoard[1, 3].PiecePic := imField17; aBoard[2, 3].PiecePic := imField18; aBoard[3, 3].PiecePic := imField19; aBoard[4, 3].PiecePic := imField20;
  aBoard[5, 3].PiecePic := imField21; aBoard[6, 3].PiecePic := imField22; aBoard[7, 3].PiecePic := imField23; aBoard[8, 3].PiecePic := imField24;
  aBoard[1, 4].PiecePic := imField25; aBoard[2, 4].PiecePic := imField26; aBoard[3, 4].PiecePic := imField27; aBoard[4, 4].PiecePic := imField28;
  aBoard[5, 4].PiecePic := imField29; aBoard[6, 4].PiecePic := imField30; aBoard[7, 4].PiecePic := imField31; aBoard[8, 4].PiecePic := imField32;
  aBoard[1, 5].PiecePic := imField33; aBoard[2, 5].PiecePic := imField34; aBoard[3, 5].PiecePic := imField35; aBoard[4, 5].PiecePic := imField36;
  aBoard[5, 5].PiecePic := imField37; aBoard[6, 5].PiecePic := imField38; aBoard[7, 5].PiecePic := imField39; aBoard[8, 5].PiecePic := imField40;
  aBoard[1, 6].PiecePic := imField41; aBoard[2, 6].PiecePic := imField42; aBoard[3, 6].PiecePic := imField43; aBoard[4, 6].PiecePic := imField44;
  aBoard[5, 6].PiecePic := imField45; aBoard[6, 6].PiecePic := imField46; aBoard[7, 6].PiecePic := imField47; aBoard[8, 6].PiecePic := imField48;
  aBoard[1, 7].PiecePic := imField49; aBoard[2, 7].PiecePic := imField50; aBoard[3, 7].PiecePic := imField51; aBoard[4, 7].PiecePic := imField52;
  aBoard[5, 7].PiecePic := imField53; aBoard[6, 7].PiecePic := imField54; aBoard[7, 7].PiecePic := imField55; aBoard[8, 7].PiecePic := imField56;
  aBoard[1, 8].PiecePic := imField57; aBoard[2, 8].PiecePic := imField58; aBoard[3, 8].PiecePic := imField59; aBoard[4, 8].PiecePic := imField60;
  aBoard[5, 8].PiecePic := imField61; aBoard[6, 8].PiecePic := imField62; aBoard[7, 8].PiecePic := imField63; aBoard[8, 8].PiecePic := imField64;
  // Init chess board: Alternate black and white empty fileds
  Colour := 'w';
  for C := 1 to 8 do begin
    if Colour = 'b' then
      Colour := 'w'
    else
      Colour := 'b';
    for R := 1 to 8 do begin
      aBoard[C, R].FieldColour := Colour;
      aBoard[C, R].Piece := '';
      Filename := './res/empty_' + Colour + '.jpg'; DoDirSeparators(Filename);
      aBoard[C, R].PiecePic.Picture.LoadFromFile(Filename);
      if Colour = 'b' then
        Colour := 'w'
      else
        Colour := 'b';
    end;
  end;
  // Init players' pieces
  for I := 1 to 16 do begin
    case I of
         1: J := 1;
         2: J := 2;
      3, 4: J := 3;
      5, 6: J := 4;
      7, 8: J := 5;
       else J := 6;
    end;
    aWhite[I].Name := PieceNames[J]; aBlack[I].Name := PieceNames[J];
    aWhite[I].Value := PieceValues[J]; aBlack[I].Value := PieceValues[J];
    aWhite[I].Col := 0; aBlack[I].Col := 0;
    aWhite[I].Row := 0; aBlack[I].Row := 0;
  end;
  // Init "hint" variable
  bShowHint := True;                                                           // if variable = true, pushing "Hint" button will show the hint, otherwise will hide them
  // Start with piece moves feature (white pieces)
  cColour := 'w'; mChessPieces.Click;
end;

{ Menu item "Chess > Chessboard setup": Run "chessboard setup" feature }

procedure TfChess.mChessSetupClick(Sender: TObject);

var
  S: string;

begin
  sOption := 'setup';
  sSelect := 'piece';                                                          // first action will be to select one of the "player pieces"
  iPiecesSel := 0;                                                             // index for piece selection (0 = none)
  iPieceSelect := 0;                                                           // pieces selected counter
  btPawns.Visible := True;                                                     // "Pawns" button available for this feature
  // Display title and instructions
  stTitle.Caption := 'Chessboard setup.';
  edInstructions.Lines.Clear; edInstructions2.Lines.Clear;
  edInstructions.Lines.AddText('Try to set up the pieces on the chess board.');
  S := 'Click a piece and when it is highlighted (blue), click the field on the board, where you intend to place it. ';
  S += 'If the position is invalid, the field will be shown in red.';
  edInstructions2.Lines.AddText(S);
  // Hide setup hints
  laHints.Caption := 'Chessboard setup hints.';
  edHint.Clear;
  laHints.Caption := 'Chessboard setup hints.';
  edHint.Clear;
  bShowHint := True;                                                           // pushing "Hints" will show the setup hints
  // Fill-in the white and black "player pieces" arrays
  PlayerPiecesFill(aWhite, aBlack);
  // Display the "player pieces" (of actual color)
  PlayerPiecesDisplay(imPieces, cColour);
  // Clear the board
  ClearBoard(aBoard);
end;

{ Menu item "Chess > Piece movements": Run "piece movements" feature }

procedure TfChess.mChessPiecesClick(Sender: TObject);

var
  S: string;

begin
  sOption := 'moves';
  sSelect := 'piece';                                                          // first action will be to select one of the "player pieces"
  iPieceSelect := 0;                                                           // index for piece selection (0 = none)
  iPieceCol := 0; iPieceRow := 0;                                              // column and row of actual piece
  btPawns.Visible := False;
  // Display title and instructions
  stTitle.Caption := 'Chess pieces movements.';
  edInstructions.Lines.Clear; edInstructions2.Lines.Clear;
  edInstructions.Lines.AddText('Try to correctly move the chess pieces.');
  S := 'Click a piece and when it is highlighted (blue), click the field on the board, where you want to place it. ';
  S += 'Then, click the field, where you intend to move it. Valid moves are shown in green, invalid ones in red. ';
  S += 'Click the piece on the board to remove it.';
  edInstructions2.Lines.AddText(S);
  // Hide hints
  laHints.Caption := 'Chess pieces move hints.';
  edHint.Clear;
  bShowHint := True;                                                           // pushing "Hints" will show the hints
  // Display the "player pieces" (of actual color)
  PlayerPiecesDisplay(imPieces, cColour);
  // Clear the board
  ClearBoard(aBoard);
end;

{ Menu item "Chess > Castling": Run "castling" feature }

procedure TfChess.mChessCastlingClick(Sender: TObject);

var
  I: Integer;
  S: string;

begin
  sOption := 'castling';
  sSelect := 'king';                                                           // first action will be to move the king
  iPieceSelect := 0;                                                           // index for piece selection (0 = none)
  iPieceCol := 0; iPieceRow := 0;                                              // column and row of actual piece
  btPawns.Visible := False;
  // Display title and instructions
  stTitle.Caption := 'The "castling" procedure.';
  edInstructions.Lines.Clear; edInstructions2.Lines.Clear;
  edInstructions.Lines.AddText('Try kingside or queenside castling.');
  S := 'Click the king and when it is highlighted (blue), click the field, where you intend to move it. ';
  S += 'Then do the same for one of the rooks. Valid moves are executed, invalid ones are shown in red. To ';
  S += 'restart, choose "Castling" in the "Chess" menu again. ';
  edInstructions2.Lines.AddText(S);
  // Hide hints
  laHints.Caption := 'Castling hints.';
  edHint.Clear;
  bShowHint := True;                                                           // pushing "Hints" will show the hints
  // Castling pieces (for actual color)
  for I := 1 to 16 do begin
    aWhite[I].Col := 0; aWhite[I].Row := 0;
    aBlack[I].Col := 0; aBlack[I].Row := 0;
  end;
  if mOptionsColorWhite.Checked then begin
    aWhite[1].Col := 5; aWhite[1].Row := 1;
    aWhite[3].Col := 1; aWhite[3].Row := 1;
    aWhite[4].Col := 8; aWhite[4].Row := 1;
  end
  else begin
    aBlack[1].Col := 5; aBlack[1].Row := 8;
    aBlack[3].Col := 1; aBlack[3].Row := 8;
    aBlack[4].Col := 8; aBlack[4].Row := 8;
  end;
  // Display the "player pieces" (of actual color)
  PlayerPiecesDisplay(imPieces, cColour);                                      // just displayed (not used)
  // Display castling board setup (for actual color)
  DisplayBoard(aBoard, aWhite, aBlack);
end;

{ Menu item "Chess > Game opening": Run "game opening" feature }

procedure TfChess.mChessBeginClick(Sender: TObject);

var
  S: string;

begin
  sOption := 'begin';
  sSelect := 'piece';                                                          // first action will be to select a piece (on the board)
  iPieceSelect := 0;                                                           // index for piece selection (0 = none)
  iPieceCol := 0; iPieceRow := 0;                                              // column and row of actual piece
  btPawns.Visible := False;
  // Display title and instructions
  stTitle.Caption := 'Game opening moves.';
  edInstructions.Lines.Clear; edInstructions2.Lines.Clear;
  edInstructions.Lines.AddText('Try the moves at the opening of the game. There are only 20 for each color!');
  S := 'Click a piece and when it is highlighted (blue), click the field, where you intend to move it. ';
  S += 'Valid moves are shown in green, invalid ones in red. To unselect the piece, click it again. ';
  edInstructions2.Lines.AddText(S);
  // Hide hints
  laHints.Caption := 'Chess pieces move hints.';
  edHint.Clear;
  bShowHint := True;                                                           // pushing "Hints" will show the hints
  // Fill-in the white and black "player pieces" arrays
  PlayerPiecesFill(aWhite, aBlack);
  // Display the "player pieces" (of actual color)
  PlayerPiecesDisplay(imPieces, cColour);                                      // just displayed (not used)
  // Display chess board (with all pieces at game begin position)
  DisplayBoard(aBoard, aWhite, aBlack);
end;

{ Menu item "Chess > Game position": Run "game position" feature }

procedure TfChess.mChessGameClick(Sender: TObject);

var
  S: string;
  Pieces: TPosPieces;
  Check, Check2, Checkmate: TPosCheck;

begin
  sOption := 'position';
  sSelect := 'piece';                                                          // first action will be to select a piece (on the board)
  iPieceSelect := 0;                                                           // index for piece selection (0 = none)
  iPieceCol := 0; iPieceRow := 0;                                              // column and row of actual piece
  btPawns.Visible := False;
  // Display title and instructions
  stTitle.Caption := 'Game position moves.';
  edInstructions.Lines.Clear; edInstructions2.Lines.Clear;
  edInstructions.Lines.AddText('Try the moves at a given position of the game.');
  S := 'Click a piece and when it is highlighted (blue), click the field, where you intend to move it. ';
  S += 'Valid moves are shown in green, valid moves with piece capture in yellow, invalid ones, including ';
  S += 'those, that would move/set the king into check, in red. To unselect the piece, click it again.';
  edInstructions2.Lines.AddText(S);
  // Hide hints
  laHints.Caption := 'Chess pieces move hints.';
  edHint.Clear;
  bShowHint := True;                                                           // pushing "Hints" will show the hints
  // User selection of one of the 6 predefined game positions
  fPosition.ShowModal;                                                         // show game position selection form
  // Get game position and "special moves" data from "fPosition" form
  Pieces := fPosition.aPieces;
  Check := fPosition.aCheck; Check2 := fPosition.aCheck2; Checkmate := fPosition.aCheckmate;
  // Get game position data into arrays of records (for simpler access to it)
  GamePosition(Pieces, Check, Check2, Checkmate, aWhite, aBlack, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack);
  // Display the "player pieces" (of actual color)
  PlayerPiecesDisplay(imPieces, cColour);                                      // just displayed (not used)
  // Display the actual position board setup
  DisplayBoard(aBoard, aWhite, aBlack);
end;

{ Menu item "Chess > Exit": Exit application }

procedure TfChess.mChessExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Choose your color > White": Set player color to "white" }

procedure TfChess.mOptionsColorWhiteClick(Sender: TObject);

begin
  mOptionsColorWhite.Checked := True;
  mOptionsColorBlack.Checked := False;
  cColour := 'w';
  PlayerPiecesDisplay(imPieces, cColour);
  if sOption = 'castling' then
    mChessCastling.Click                                                       // set up white pieces
  else if (sOption = 'setup') or (sOption = 'moves') then
    ClearBoard(aBoard);                                                        // remove all pieces from board
end;

{ Menu item "Options > Choose your color > Black": Set player color to "black" }

procedure TfChess.mOptionsColorBlackClick(Sender: TObject);

begin
  mOptionsColorWhite.Checked := False;
  mOptionsColorBlack.Checked := True;
  cColour := 'b';
  PlayerPiecesDisplay(imPieces, cColour);
  if sOption = 'castling' then
    mChessCastling.Click                                                       // set up black pieces
  else if (sOption = 'setup') or (sOption = 'moves') then
    ClearBoard(aBoard);                                                        // remove all pieces from board
end;

{ Menu item "Options > Disable info messages": Toggle display or not of info messages }

procedure TfChess.mOptionsMessClick(Sender: TObject);

begin
  if mOptionsMess.Checked then
    mOptionsMess.Checked := False
  else
    mOptionsMess.Checked := True;
end;

{ Menu item "Options > Disable error messages": Toggle display or not of error messages }

procedure TfChess.mOptionsErrorsClick(Sender: TObject);

begin
  if mOptionsErrors.Checked then
    mOptionsErrors.Checked := False
  else
    mOptionsErrors.Checked := True;
end;

{ Menu item "Help > Quick start": Display quick start help text }

procedure TfChess.mHelpQuickClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.Memo1.Clear;
    fHelp.stTitle.Caption := '"ChessMoves" quick start.';
    fHelp.Memo1.Lines.LoadFromFile('quick.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > Chess help": Display chess game help text }

procedure TfChess.mHelpChessClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.Memo1.Clear;
    fHelp.stTitle.Caption := 'Chess game help.';
    fHelp.Memo1.Lines.LoadFromFile('chess.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > Program help": Display program usage help text }

procedure TfChess.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else begin
    fHelp.Memo1.Clear;
    fHelp.stTitle.Caption := '"ChessMoves" help.';
    fHelp.Memo1.Lines.LoadFromFile('help.txt');
    fHelp.Show;
  end;
end;

{ Menu item "Help > About": Display program about }

procedure TfChess.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Interactive absolute beginners introduction to chess:' + LineEnding;
  S += 'Learn how to set up the chess board and how to move the chess pieces.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, March-December 2019.';
  MessageDlg('About "ChessMoves"', S, mtInformation, [mbOK], 0);
end;

{ Button "Show": Show (all) valid moves }

procedure TfChess.btShowClick(Sender: TObject);

const
  MediumSpringGreen = $9AFA00;

var
  I: Integer;
  Filename, S: string;
  BColour: Char;
  Colour: TColor;
  ValidMoves: TMoves;

begin
  // "Chess board setup": Show opening board setup (for both colors)
  // ---------------------------------------------------------------
  if sOption = 'setup' then begin
    iPieceSelect := 0;
    // Remove "player piece" highlighting
    shSelect[1, 1].Visible := False; shSelect[1, 2].Visible := False;
    shSelect[1, 3].Visible := False; shSelect[1, 4].Visible := False;
    // Remove all "player pieces"
    if mOptionsColorWhite.Checked then
      BColour := 'b'
    else
      BColour := 'w';
    for I := 1 to 16 do begin
      Filename := './res/' + 'empty_' + BColour + '.jpg'; DoDirSeparators(Filename);
      imPieces[I].Picture.LoadFromFile(Filename); imPieces[I].Enabled := False;
    end;
    // Display both colors pieces
    PlayerPiecesFill(aWhite, aBlack);
    DisplayBoard(aBoard, aWhite, aBlack);
  end
  // "Castling": Show both kingside and queenside castling (depending on actual color)
  // ---------------------------------------------------------------------------------
  else if sOption = 'castling' then begin
    if mOptionsColorWhite.Checked then begin
      // Kingside castling for white, queenside castling for black
      aWhite[1].Col := 7; aWhite[1].Row := 1; aWhite[3].Col := 1; aWhite[3].Row := 1;
      aWhite[4].Col := 6; aWhite[4].Row := 1; aBlack[1].Col := 3; aBlack[1].Row := 8;
      aBlack[3].Col := 4; aBlack[3].Row := 8; aBlack[4].Col := 8; aBlack[4].Row := 8;
      S := 'Showing kingside castling with white pieces and queenside castling with black pieces...';
    end
    else begin
      // Kingside castling for black, queenside castling for white
      aWhite[1].Col := 3; aWhite[1].Row := 1; aWhite[3].Col := 4; aWhite[3].Row := 1;
      aWhite[4].Col := 8; aWhite[4].Row := 1; aBlack[1].Col := 7; aBlack[1].Row := 8;
      aBlack[3].Col := 1; aBlack[3].Row := 8; aBlack[4].Col := 6; aBlack[4].Row := 8;
      S := 'Showing queenside castling with white pieces and kingside castling with black pieces...';
    end;
    edHint.Clear; edHint.Lines.AddText(S);
    bShowHint := False;
    // Display castling board setup
    DisplayBoard(aBoard, aWhite, aBlack);
    sSelect := 'done';                                                         // this "blocks" all actions, when a piece is clicked
  end
  // "Pieces movements", "Game opening" and "Game position": Show all valid moves for selected piece
  else begin
    if (iPieceCol in [1..8]) and (iPieceRow in [1..8]) then begin
      if aBoard[iPieceCol, iPieceRow].Piece <> '' then begin
        // Remove last move highlighting
        shSelect[2, 1].Visible := False; shSelect[2, 2].Visible := False;
        shSelect[2, 3].Visible := False; shSelect[2, 4].Visible := False;
        GetValidMoves(aBoard, iPieceCol, iPieceRow, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, ValidMoves);
        // Highlight all valid moves (incl. forbidden moves)
        for I := 0 to Length(ValidMoves) - 1 do begin
          if (ValidMoves[I].Col < 0) and (ValidMoves[I].Row < 0) then begin
            // Forbidden moves are included in the "valid moves" array,
            // but the piece's column and row has been set to a negative value
            ValidMoves[I].Col := -ValidMoves[I].Col; ValidMoves[I].Row := -ValidMoves[I].Row;
            Colour := clRed;                                                   // show forbidden moves with red highlighting
          end
          else if ValidMoves[I].Capture then
            // Moving to this field = capturing the opponent's piece
            Colour := clYellow                                                 // show captures with yellow highlighting
          else begin
            // Moving to an empty field
            if aBoard[iPieceCol, iPieceRow].Piece = 'queen' then begin
              // Moving the queen
              if (ValidMoves[I].Col <> iPieceCol) and (ValidMoves[I].Row <> iPieceRow) then
                Colour := MediumSpringGreen                                    // show diagonal queen moves in spring green
              else
                Colour := clLime;                                              // show horizontal queen moves in lime green
            end
            else if ValidMoves[I].Castling then
              // Moving the king to a "castling field"
              Colour := MediumSpringGreen                                      // show king "castling move" in spring green
            else
              Colour := clLime;                                                // show all other moves in lime green
          end;
          // Show the highlighting shapes
          shSelect[I + 3, 1].Left := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Left;
          shSelect[I + 3, 1].Top := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Top;
          shSelect[I + 3, 2].Left := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Left;
          shSelect[I + 3, 2].Top := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Top + 56;
          shSelect[I + 3, 3].Left := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Left;
          shSelect[I + 3, 3].Top := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Top;
          shSelect[I + 3, 4].Left := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Left + 56;
          shSelect[I + 3, 4].Top := aBoard[ValidMoves[I].Col, ValidMoves[I].Row].PiecePic.Top;
          shSelect[I + 3, 1].Brush.Color := Colour; shSelect[I + 3, 2].Brush.Color := Colour;
          shSelect[I + 3, 3].Brush.Color := Colour; shSelect[I + 3, 4].Brush.Color := Colour;
          shSelect[I + 3, 1].Pen.Color := Colour; shSelect[I + 3, 2].Pen.Color := Colour;
          shSelect[I + 3, 3].Pen.Color := Colour; shSelect[I + 3, 4].Pen.Color := Colour;
          shSelect[I + 3, 1].Visible := True; shSelect[I + 3, 2].Visible := True;
          shSelect[I + 3, 3].Visible := True; shSelect[I + 3, 4].Visible := True;
        end;
      end;
    end;
  end;
end;

{ Button "Hints": Display context based hints (depending on actual feature and piece selectd) }

procedure TfChess.btHintClick(Sender: TObject);

const
  MoveRules: array[1..6] of string = (
    // Pieces moves rules
    'The king can only move one square in any direction - up, down, to the sides, and diagonally.',
    'The queen can move as far as she wants in any one straight direction - forward, backward, sideways, or diagonally (in fact a combination of the rook and bishop moves).',
    'The rooks may move as far as they want, but only forward, backward, and to the sides.',
    'The knights move going two squares in one direction, and then one more move at a 90 degree angle, just like the shape of an “L” (being allowed to jump over other pieces).',
    'The bishops may move as far as they want, but only diagonally. This means: Each bishop starts on one color (light or dark) and must always stay on that color.',
    'The pawns can only move forward one square at a time, except for their very first move where they can move forward two squares.'
  );

var
  Piece, I: Integer;
  S: string;

begin
  if bShowHint then begin
    if sOption = 'setup' then begin
      // "Chess board setup"
      // -------------------
      S := 'The 2nd/7th row is filled with pawns; the other pieces are placed on the 1st/8th row. The rooks go in the corners, ';
      S += 'then the knights next to them, followed by the bishops. ';
      S += 'The king is placed on the "e" column, the queen on the "d" column, with the rule saying that the queen always goes on her ';
      S += 'own matching color (white queen on white, black queen on black).';
      edHint.Clear; edHint.Lines.AddText(S);
      bShowHint := False;
    end
    else if sOption = 'castling' then begin
      // "Castling"
      // ----------
      S := 'Castling consists in moving the king two squares over to one side and then move the rook from that side''s ';
      S += 'corner to right next to the king on the opposite side. For further details, please, see the Chess help text.';
      edHint.Clear;  edHint.Lines.AddText(S);
      bShowHint := False;
    end
    else begin
      // "Piece movements", "Game opening" and "Game position"
      // -----------------------------------------------------
      S := ''; Piece := -1;
      if (sOption = 'begin') or (sOption = 'position') then begin
        // Determine index of piece currently selected
        if (iPieceCol <> 0) and (iPieceRow <> 0) then begin
          for I := 1 to 9 do begin
            if aBoard[iPieceCol, iPieceRow].Piece = AllPieces[I] then
             iPieceSelect := I; Piece := 0;
          end;
        end;
        if (Piece = -1) and ((edHint.Lines.Count = 2) or (edHint.Lines.Count = 3)) then begin
          // If no piece selected, clear the hints. Do not do this, if the actual hint = the general rules
          // Doing so allows to display the general rules at "startup" and after selecting a piece, adding
          // the piece's move rules by one single button push (otherwise, the hint would be first cleared)
          edHint.Clear;
          bShowHint := True;
        end
        else begin
          // Display general move rules
          S := 'General rules: You may not capture your own pieces.';
          if Piece = -1 then
            // Display this only if no piece selected
            S += ' Except with the knight, you may not jump over other pieces.';
          if (sOption = 'position') and (aBoard[iPieceCol, iPieceRow].Piece <> 'king') then
            // Display this only for "Game position" feature and if the selected piece is not the king
            S += ' You cannot move a piece, if this sets your king into check.';
          S += LineEnding;
          if ((sOption = 'begin') and (Piece <> -1)) or ((sOption = 'position') and (aBoard[iPieceCol, iPieceRow].Piece = 'king')) then
            // Singular for "rule" if there is only one
            S := StringReplace(S, 'rules', 'rule', []);
        end;
      end;
      // Display move rules for actually selected piece
      if ((sOption = 'moves') and (iPieceSelect <> 0)) or ((sOption <> 'moves') and (Piece <> -1)) then begin
        case iPieceSelect of
             1: Piece := 1;
             2: Piece := 2;
          3, 4: Piece := 3;
          5, 6: Piece := 4;
          7, 8: Piece := 5;
          else  Piece := 6;
        end;
        S += MoveRules[Piece];
        // Some "special rules" to add
        if ((sOption = 'begin') or (sOption = 'position')) and (Piece <> 4) then
          S += ' You may not jump over other pieces.';
        if (sOption = 'position') and (Piece = 6) then
          S += ' Pawns can only capture one square diagonally in front of them.'
        else if (sOption = 'position') and (Piece = 1) then
          S += ' The king may never move himself into check.'
      end;
      edHint.Clear; edHint.Lines.AddText(S);
      if not (((sOption = 'begin') or (sOption = 'position')) and (Piece = -1)) then
        bShowHint := False;
    end;
  end
  else begin
    // Clear the hints
    edHint.Clear;
    bShowHint := True;
  end;
end;

{ Button "Pawns": Move the 8 pawns of actual color to their opening position }

procedure TfChess.btPawnsClick(Sender: TObject);

// The "Pawns" button is only visible with the "Game opening" feature

var
  Row, Col, Piece, I: Integer;
  Filename: string;
  PColour, BColour: Char;

begin
  iPieceSelect := 0;
  // Remove last move highlighting
  shSelect[1, 1].Visible := False; shSelect[1, 2].Visible := False;
  shSelect[1, 3].Visible := False; shSelect[1, 4].Visible := False;
  if mOptionsColorWhite.Checked then begin
    PColour := 'w'; BColour := 'b'; Row := 2;
  end
  else begin
    PColour := 'b'; BColour := 'w'; Row := 7;
  end;
  // Display the 8 pawns (and remove them from the "player pieces"
  for I := 1 to 8 do begin
    Piece := I + 8; Col := I;
    Filename := './res/' + 'empty_' + BColour + '.jpg'; DoDirSeparators(Filename);
    imPieces[Piece].Picture.LoadFromFile(Filename); imPieces[Piece].Enabled := False;
    aBoard[Col, Row].Piece := AllPieces[Piece];
    aBoard[Col, Row].PieceColour := PColour;
    Filename := './res/' + AllPieces[Piece] + '_' + PColour + aBoard[Col, Row].FieldColour + '.jpg'; DoDirSeparators(Filename);
    aBoard[Col, Row].PiecePic.Picture.LoadFromFile(Filename);
    Inc(iPiecesSel);
  end;
  sSelect := 'piece';
end;

{ User click on one of the 16 "player pieces" }

procedure TfChess.imPiecesKingClick(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 1, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesQueenClick(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 2, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesRook1Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 3, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesRook2Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 4, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesKnight1Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 5, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesKnight2Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 6, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesBishop1Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 7, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesBishop2Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 8, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion1Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 9, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion2Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 10, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion3Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 11, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion4Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 12, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion5Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 13, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion6Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 14, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion7Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 15, iPieceSelect, bShowHint);
end;

procedure TfChess.imPiecesPion8Click(Sender: TObject);

begin
  PieceClicked(sOption, sSelect, imPieces, 16, iPieceSelect, bShowHint);
end;

{ User click on one of the 64 chess board fields }

procedure TfChess.imField1Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField2Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField3Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField4Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField5Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField6Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField7Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField8Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 1, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField9Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField10Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField11Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField12Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField13Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField14Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField15Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField16Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 2, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField17Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField18Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField19Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField20Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField21Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField22Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField23Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField24Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 3, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField25Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField26Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField27Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField28Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField29Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField30Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField31Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField32Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 4, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField33Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField34Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField35Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField36Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField37Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField38Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField39Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField40Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 5, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField41Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField42Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField43Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField44Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField45Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField46Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField47Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField48Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 6, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField49Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField50Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField51Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField52Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField53Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField54Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField55Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField56Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 7, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField57Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 1, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField58Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 2, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField59Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 3, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField60Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 4, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField61Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 5, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField62Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 6, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField63Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 7, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

procedure TfChess.imField64Click(Sender: TObject);

begin
  FieldClicked(aBoard, aWhite, aBlack, sOption, sSelect, iPieceSelect, 8, 8, aCheckWhite, aCheckBlack, aCheck2White, aCheck2Black, aCheckmateWhite, aCheckmateBlack, iPiecesSel, iPieceCol, iPieceRow, bShowHint);
end;

end.

