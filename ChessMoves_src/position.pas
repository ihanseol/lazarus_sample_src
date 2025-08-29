{***********************************************************}
{* Game position selection unit for ChessMoves application *}
{***********************************************************}

// Note:
// Each game position is discribed by 4 constant strings:
//   SPieces:    the pieces positions on the board
//   SCheck:     invalid king moves, because the king would move into check
//   SCheck2:    invalid piece moves, because the own king would be set into check
//   SCheckmate: piece moves, that set the adversery king checkmate
// These strings are transformed to arrays of strings (element = 1 move), that are passed
// to the "chess" unit (that will transform them to arrays of records, used by the different subs)

unit position;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TPosCheck  = array[1..8] of string;
  TPosPieces = array[1..32] of string;
  {************}
  { TfPosition }
  {************}
  TfPosition = class(TForm)
    Label1: TLabel;
    rbPosOp1, rbPosOp2: TRadioButton;
    rbPosEnd1, rbPosEnd2: TRadioButton;
    rbPosMid1, rbPosMid2: TRadioButton;
    btSelect: TButton;
    procedure btSelectClick(Sender: TObject);
  // Variables read by the "chess" unit
  public
    aPieces: TPosPieces;
    aCheck, aCheck2, aCheckmate: TPosCheck;
  end;

var
  fPosition: TfPosition;

implementation

{$R *.lfm}

{************}
{ TfPosition }
{************}

{ Button "Select": Select one of the 6 game positions }

procedure TfPosition.btSelectClick(Sender: TObject);

const
  SPieces: array[1..6] of string = (
    'kg1qd1ra1rf1nb1nf3bc1bc4pa2pb2pc2p--pe4pf2pg2ph2ke8qd8ra8rh8nc6nf6bc8bd4pa7pb7pc7pd7pe5pf7pg7ph7',
    'ke1qd1ra1rh1nc3nf3bc1bb5pa2pb2pc2pd4pe5pf4pg2ph2ke8qd8ra8rh8nb8ng4bd7bg7pa7pb7pc5pd6pe7pf7pg6ph7',
    'kb1qg4rg1r--n--n--b--b--pa2pb2pc3p--p--pf2p--ph4ke8q--rd7rf8nd8n--bb5b--pa6pb7p--p--p--p--p--p--',
    'ka1q--rb1ra7n--n--b--b--pa3p--p--p--pe5p--p--ph4kc5q--rc2re2n--n--b--b--p--p--pc6pd5p--p--p--p--',
    'kb1qe2rd1rh1nc7n--bf4bg2pa2pb2pc2p--p--pf2p--ph4kf8qd8rc8rh8nc6nc5ba4be7pa6pb7p--pd5pe6pf7p--p--',
    'ke1qf3ra1rh1nc7nd5b--bf1pa3pb2pc2p--pe4pf2pg2ph2kf7qd8ra8rh8nb4nf6bc8bf8pa7pb7p--pd6pe5pe3pg7ph7'
  );
  SCheck: array[1..6] of string = (
    '', 'wf2ng4', '', 'wa2rc2wb2rc2bb6rb1bb5rb1bb4rb1bd6pe5', 'be8nc7', 'wd2pe3be8nc7be7nd5be6nc7'
  );
  SCheck2: array[1..6] of string = (
    '', '', '', '', '', 'bnf6d5bnf6d7bnf6e4bnf6e8bnf6g4bnf6g8bnf6h5'
  );
  SCheckmate: array[1..6] of string = (
    '', '', '', 'brc2a2', '', ''
  );

var
  IX, I: Integer;

begin
  if rbPosOp1.Checked then
    IX := 1
  else if rbPosOp2.Checked then
    IX := 2
  else if rbPosEnd1.Checked then
    IX := 3
  else if rbPosEnd2.Checked then
    IX := 4
  else if rbPosMid1.Checked then
    IX := 5
  else if rbPosMid2.Checked then
    IX := 6;
  // Pieces array (order: K, Q, R, R, N, B, p...)
  for I := 1 to 32 do
    aPieces[I] := Copy(SPieces[IX], (I - 1) * 3 + 1, 3);
  // Special piece moves (see note above) arrays
  for I := 1 to 8 do begin
    aCheck[I] := ''; aCheck2[I] := ''; aCheckmate[I] := '';
    if I * 6 <= Length(SCheck[IX]) then
      aCheck[I] := Copy(SCheck[IX], (I - 1) * 6 + 1, 6);
    if I * 6 <= Length(SCheck2[IX]) then
      aCheck2[I] := Copy(SCheck2[IX], (I - 1) * 6 + 1, 6);
    if I * 6 <= Length(SCheckmate[IX]) then
      aCheckmate[I] := Copy(SCheckmate[IX], (I - 1) * 6 + 1, 6);
  end;
  Close;
end;

end.

