{*****************************************}
{* Main unit for MagicSquare application *}
{*****************************************}

unit sqmagic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus, PopupNotifier;

type
  TOperators  = array of Char;
  TFields = array[1..5, 1..5] of Integer;
  TEditFields = array[1..5, 1..5] of TEdit;
  { TfSqMagic }
  TfSqMagic = class(TForm)
    mMenu: TMainMenu;
    mGame, mGameExit: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Shape1,  Shape2,  Shape3,  Shape4,  Shape5: TShape;
    Shape7,  Shape8,  Shape9,  Shape10, Shape11: TShape;
    Shape13, Shape14, Shape15, Shape16, Shape17: TShape;
    Shape19, Shape20, Shape21, Shape22, Shape23: TShape;
    Shape25, Shape26, Shape27, Shape28, Shape29: TShape;
    ed11N, ed12O, ed13N, ed14E, ed15N: TEdit;
    ed21O, ed23O, ed25O: TEdit;
    ed31N, ed32O, ed33N, ed34E, ed35N: TEdit;
    ed41E, ed43E, ed45E: TEdit;
    ed51N, ed52O, ed53N, ed54E, ed55N: TEdit;
    cgOperators: TCheckGroup;
    edResult: TEdit;
    btQuestion: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mGameExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    aOperators: TOperators;
    aField: TFields;
    aEdit: TEditFields;
  end;

const
  AllOperators: array[0..1] of Char = ('+', '-');

var
  fSqMagic: TfSqMagic;

implementation

{$R *.lfm}

{ Get list of operators: +, -, or both }

procedure OperatorsList(out Operators: TOperators);

var
  I, J: Integer;

begin
  SetLength(Operators, 0); J := 0;
  for I := 0 to 1 do begin
    if fSqMagic.cgOperators.Checked[I] then begin
      SetLength(Operators, J + 1);
      Operators[J] := AllOperators[I];
      Inc(J);
    end;
  end;
end;

{ **********}
{ TfSqMagic }
{ **********}

{ Application start: initialisation }

procedure TfSqMagic.FormCreate(Sender: TObject);

begin
  // Create arrays with the magic square's numbers and operators edit fields as elements
  aEdit[1, 1] := ed11N; aEdit[1, 2] := ed12O; aEdit[1, 3] := ed13N; aEdit[1, 5] := ed15N;
  aEdit[2, 1] := ed21O; aEdit[2, 3] := ed23O; aEdit[2, 5] := ed25O;
  aEdit[3, 1] := ed31N; aEdit[3, 2] := ed32O; aEdit[3, 3] := ed33N; aEdit[3, 5] := ed35N;
  aEdit[5, 1] := ed51N; aEdit[5, 2] := ed52O; aEdit[5, 3] := ed53N; aEdit[5, 5] := ed55N;
  cgOperators.Checked[0] := True;                                              // use '+' on startup
  Randomize;
end;

{ Menu item "Spill > Verloossen": Exit application }

procedure TfSqMagic.mGameExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Hëllef > Info": Display program about }

procedure TfSqMagic.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Visible := False
  else begin
    S := 'Rechespill matt Additioun a Sustraktioun vun Zuele vun 0 bis 100. ';
    S += 'An éischter Linn fir Primärschüler geduecht.' + Chr(13) + Chr(13);
    S += 'Versioun 1.0, © allu, Juli, 2018.';
    pnAbout.Text := S;
    pnAbout.Visible := True;
  end;
end;

{ Button "Fro/Äntwert": Create new magic square / check user's solution }

procedure TfSqMagic.btQuestionClick(Sender: TObject);

const
  FieldsFilled: array[0..2, 0..1] of Byte = (
    (1, 3), (1, 5), (3, 5)
  );

var
  N, Row2, Col2, Row2BlankCol, Col2BlankRow: Integer;
  C1, C2, I, J, K: Integer;
  Oprator: Char;
  OK: Boolean;
  CCount: array[1..5] of Integer;

begin
  // Create new magic square
  if btQuestion.Caption = 'Fro' then begin
    OperatorsList(aOperators); N := Length(aOperators);                        // get list of operators
    // Proceed if at least 1 operator has been selected
    if N > 0 then begin
      // Clear variables and edit fields
      edResult.Text := '';
      for I := 1 to 5 do begin
        if (I <> 2) and (I <> 4) then begin
          for J := 1 to 5 do begin
            if (J <> 2) and (J <> 4) then begin
              aField[I, J] := 0;
              aEdit[I, J].Text := '';
              aEdit[I, J].Color := clDefault;
            end;
          end;
        end;
      end;
      // Choose 1 of the operators available (= selected)
      Oprator := aOperators[Random(N)];
      // Fill-in of the operator in the operator fields of the magic square
      for I := 1 to 5 do begin
        if I mod 2 = 1 then
          aEdit[2, I].Text := Oprator;
      end;
      for I := 1 to 5 do begin
        if I mod 2 = 1 then
          aEdit[I, 2].Text := Oprator;
      end;
      // Choose 3 random numbers, placed in fields (1, 5), (5, 1) or (5, 3), and (1, 1),
      // calculate the value of fields (1, 3) and (3, 1), (3, 3), (3, 5) and (5, 5)
      // but keep the values only if all of them are positive and less or equal to 100
      repeat
        aField[1, 5] := Random(100) + 1;
        for J := 1 to 3 do begin
          if J <> 2 then
            aField[5, J] := Random(100) + 1;
        end;
        if aEdit[5, 2].Text = '+' then
          aField[5, 5] := aField[5, 1] + aField[5, 3]
        else
          aField[5, 5] := aField[5, 1] - aField[5, 3];
        if aEdit[2, 5].Text = '+' then
          aField[3, 5] := aField[5, 5] - aField[1, 5]
        else
          aField[3, 5] := aField[1, 5] - aField[5, 5];
        aField[1, 1] := Random(100) + 1;
        if aEdit[1, 2].Text = '+' then
          aField[1, 3] := aField[1, 5] - aField[1, 1]
        else
          aField[1, 3] := aField[1, 1] - aField[1, 5];
        if aEdit[2, 1].Text = '+' then
          aField[3, 1] := aField[5, 1] - aField[1, 1]
        else
          aField[3, 1] := aField[1, 1] - aField[5, 1];
        if aEdit[3, 2].Text = '+' then
          aField[3, 3] := aField[3, 5] - aField[3, 1]
        else
          aField[3, 3] := aField[3, 1] - aField[3, 5];
      until (aField[3, 5] >= 0) and (aField[3, 5] <= 100)
        and (aField[5, 5] >= 0) and (aField[5, 5] <= 100)
        and (aField[1, 3] >= 0) and (aField[1, 3] <= 100)
        and (aField[3, 1] >= 0) and (aField[3, 1] <= 100)
        and (aField[3, 3] >= 0) and (aField[3, 3] <= 100);
      // Displaying 4 numbers is normally enough to solve the puzzle, except when they're all on 1 row and 1 column
      // Thus: repeat randomly keeping 4 numbers (maximum 2 per row/column) until the corresponding puzzle is solvable
      repeat
        // Clear the number fields of the magic square
        for I := 1 to 5 do begin
          CCount[I] := 0;
          if I mod 2 = 1 then begin
            for J := 1 to 5 do begin
              if J mod 2 = 1 then begin
                aEdit[I, J].Text := '';
                aEdit[I, J].ReadOnly := False;
                aEdit[I, J].TabStop := True;
              end;
            end;
          end;
        end;
        OK := True;
        // Random row with 2 numbers displayed (other rows will be just 1)
        repeat
          K := Random(5) + 1;
        until K mod 2 = 1;
        Row2 := K;                                                             // save row with 2 numbers displayed (needed below)
        // Fill-in 1 number per row (except for the row that will get 2)
        for I := 1 to 5 do begin
          if (I <> 2) and (I <> 4) then begin
            J := Random(3);
            C1 := FieldsFilled[J, 0];
            aEdit[I, C1].Text := IntToStr(aField[I, C1]);
            aEdit[I, C1].ReadOnly := True;
            aEdit[I, C1].TabStop := False;
            Inc(CCount[C1]);
            // Row that will have 2 numbers displayed
            if I = K then begin
              C2 := FieldsFilled[J, 1];
              aEdit[I, C2].Text := IntToStr(aField[I, C2]);
              aEdit[I, C2].ReadOnly := True;
              aEdit[I, C2].TabStop := False;
              Inc(CCount[C2]);
            end;
          end;
        end;
        // Rows are ok this way, but not necessarily the columns:
        // check if there isn't a column with 3 numbers displayed
        for I := 1 to 5 do begin
          if CCount[I] = 2 then
            Col2 := I                                                          // save column with 2 numbers displayed (needed below)
          else if CCount[I] = 3 then
            OK := False;
        end;
        if OK then begin
          // And finally, check if with the numbers displayed this way, the puzzle is solvable (by just doing the calculations)
          for J := 1 to 5 do begin
            if (J <> 2) and (J <> 4) then begin
              if aEdit[Row2, J].Text = '' then
                Row2BlankCol := J;                                             // column of blank field in 2-numbers-displayed row
            end;
          end;
          for I := 1 to 5 do begin
            if (I <> 2) and (I <> 4) then begin
              if aEdit[I, Col2].Text = '' then
                Col2BlankRow := I;                                             // row of blank field in 2-numbers-displayed column
            end;
          end;
          if (Row2 = Col2BlankRow) and (Col2 = Row2BlankCol) then              // if one same blank field, all numbers are in 1 row and 1 column
            OK := False;
        end;
      until OK;
      // Next button push will be to check user's answer
      btQuestion.Caption := 'Äntwert';
    end
    // No operation selected: display error message
    else begin
      MessageDlg('Ongëlteg Donnéeën', '''t ass keng Rechenoperatioun (+ oder -) ausgewielt!', mtError, [mbOK], 0);
    end;
  end
  // Check if user's solution of the puzzle is correct
  else begin
    OK := True;
    // Check all number fields; if 1 of them isn't correct, the puzzle has not been solved correctly
    for I := 1 to 5 do begin
      if (I <> 2) and (I <> 4) then begin
        for J := 1 to 5 do begin
          if (J <> 2) and (J <> 4) then begin
            // Field not filled-in or containing false value
            if (aEdit[I, J].Text = '') or (StrToInt(aEdit[I, J].Text) <> aField[I, J]) then begin
              OK := False;
              aEdit[I, J].Text := IntToStr(aField[I, J]);
              aEdit[I, J].Color := clRed;
            end;
          end;
        end;
      end;
    end;
    // Display if solution is correct or false
    if OK then begin
      edResult.Font.Color := clLime;
      edResult.Text := 'OK!';
    end
    else begin
      edResult.Font.Color := clRed;
      edResult.Text := 'Feeler!';
    end;
    // Next button push will be generation of another magic square
    btQuestion.Caption := 'Fro';
  end;
end;

end.

