{*******************************************}
{* Main unit for NumberPyramid application *}
{*******************************************}

unit pyramid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus;

type
  {***********}
  { TfPyramid }
  {***********}
  TfPyramid = class(TForm)
    mMenu: TMainMenu;
    mPuzzle, mPuzzleExit, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edRules: TMemo;
    Image1: TImage;
    edN22, edN32, edN33, edN21, edN42, edN43, edN44, edN51: TEdit;
    edN52, edN53, edN55, edN54, edN41, edN11, edN31: TEdit;
    Label1, Label2, Label3: TLabel;
    edX, edY, edZ: TEdit;
    btAction: TButton;
    btReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mPuzzleExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
  private
    iSumType, iX, iY, iZ: Integer;
    aNums: array[0..4, 0..4] of Integer;
    edNums: array[0..4, 0..4] of TEdit;
  end;

var
  fPyramid: TfPyramid;

implementation

{$R *.lfm}

{***********}
{ TfPyramid }
{***********}

{ Application start: Create array with edit fiels (number input) }

procedure TfPyramid.FormCreate(Sender: TObject);

begin
  // Incomplete array (using "nil" for non existing edit fields)
  edNums[0, 0] := edN11; edNums[0, 1] := nil;   edNums[0, 2] := nil;   edNums[0, 3] := nil;   edNums[0, 4] := nil;
  edNums[1, 0] := edN21; edNums[1, 1] := edN22; edNums[1, 2] := nil;   edNums[1, 3] := nil;   edNums[1, 4] := nil;
  edNums[2, 0] := edN31; edNums[2, 1] := edN32; edNums[2, 2] := edN33; edNums[2, 3] := nil;   edNums[2, 4] := nil;
  edNums[3, 0] := edN41; edNums[3, 1] := edN42; edNums[3, 2] := edN43; edNums[3, 3] := edN44; edNums[3, 4] := nil;
  edNums[4, 0] := edN51; edNums[4, 1] := edN52; edNums[4, 2] := edN53; edNums[4, 3] := edN54; edNums[4, 4] := edN55;
  Randomize;
  btAction.Click;
end;

{ Menu item "Puzzle > Exit": Exit application }

procedure TfPyramid.mPuzzleExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfPyramid.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Math puzzle: The Number Pyramid.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2024.';
  MessageDlg('About "NumberPyramid"', S, mtInformation, [mbOK], 0);
end;

{ Button "New/Check/Show" clicked: Generate new puzzle, check user solution resp. display solved puzzle }

procedure TfPyramid.btActionClick(Sender: TObject);

var
  Sum, UX, UY, UZ, I, J, R: Integer;
  OK: Boolean;

begin
  // Button "New" clicked: Generate new puzzle
  if btAction.Caption = 'New' then begin
    // Initially 3 random values; one of them will be set to the sum of the two others
    iX := Random(10) + 1; iY := Random(10) + 1; iZ := Random(10) + 1;
    // Choose randomly if X, Y, or Z should be the sum value
    iSumType := Random(3) + 1;
    case iSumType of
      1: begin
        edRules.Text := StringReplace(edRules.Text, 'middle', 'left', []);
        edRules.Text := StringReplace(edRules.Text, 'right', 'left', []);
        edRules.Text := StringReplace(edRules.Text, 'Y = X + Z', 'X = Y + Z', []);
        edRules.Text := StringReplace(edRules.Text, 'Z = X + Y', 'X = Y + Z', []);
        iX := iY + iZ;
      end;
      2: begin
        edRules.Text := StringReplace(edRules.Text, 'left', 'middle', []);
        edRules.Text := StringReplace(edRules.Text, 'right', 'middle', []);
        edRules.Text := StringReplace(edRules.Text, 'X = Y + Z', 'Y = X + Z', []);
        edRules.Text := StringReplace(edRules.Text, 'Z = X + Y', 'Y = X + Z', []);
        iY := iX + iZ;
      end;
      3: begin
        edRules.Text := StringReplace(edRules.Text, 'left', 'right', []);
        edRules.Text := StringReplace(edRules.Text, 'middle', 'right', []);
        edRules.Text := StringReplace(edRules.Text, 'X = Y + Z', 'Z = X + Y', []);
        edRules.Text := StringReplace(edRules.Text, 'Y = X + Z', 'Z = X + Y', []);
        iZ := iX + iY;
      end;
    end;
    // Clear the number arry
    for I := 0 to 4 do begin
      for J := 0 to 4 do
        aNums[I, J] := 0;
    end;
    // Clear X, Y, Z edit fields
    edX.Text := ''; edY.Text := ''; edZ.Text := '';
    // Fill the X, Y, Z values into the array
    aNums[4, 0] := iX; aNums[4, 2] := iY; aNums[4, 4] := iZ;
    // Fill the other two numbers of the last row into the array
    aNums[4, 1] := Random(20) + 1;
    aNums[4, 3] := Random(20) + 1;
    // Build the rest of the array according to the sum rule
    for I := 3 downto 0 do begin
      for J := 0 to I do begin
        aNums[I, J] := Abs(aNums[I + 1, J]) + Abs(aNums[I + 1, J + 1]);
        // Second and fourth row has to be filled in by user:
        // Mark such fields by making the array value negative
        // For now, also consider the third row as user input fields
        if I in [1..3] then begin
          aNums[I, J] := -aNums[I, J];
        end;
      end;
      // Third row: Randomly set one of the fields as given value
      if I = 2 then begin
        R := Random(3);
        aNums[2, R] := -aNums[2, R];
      end;
    end;
    // Display the pyramid values
    for I := 0 to 4 do begin
      for J := 0 to 4 do begin
        if edNums[I, J] <> nil then begin
          // Do only if the edit field actually exists
          if (I < 4) or ((I = 4) and (J in [1, 3])) then begin
            // Exclude the fields with values 'X', 'Y', and 'Z'
            if aNums[I, J] > 0 then begin
              // These field values are given
              edNums[I, J].Text := IntToStr(aNums[I, J]);
              edNums[I, J].ReadOnly := True; edNums[I, J].TabStop := False;
              edNums[I, J].Font.Color := clDefault;
            end
            else begin
              // These field values have to be filled in by the user
              edNums[I, J].Text := '';
              edNums[I, J].ReadOnly := False; edNums[I, J].TabStop := True;
              edNums[I, J].Font.Color := clFuchsia;
            end;
          end;
        end;
      end;
    end;
    btAction.Caption := 'Check'; btReset.Enabled := True;
  end
  // Button "Check": Check the user solution
  // As I'm not sure if the puzzle may have several solutions, I check the user solution
  // according to the game rules, rather than comparing with the generated pyramid values
  else if btAction.Caption = 'Check' then begin
    OK := True;
    // Check X, Y, Z values
    if (edX.Text = '') or (edY.Text = '') or (edZ.Text = '') then
      OK := False
    else if (StrToInt(edX.Text) < 0) or (StrToInt(edY.Text) < 0) or (StrToInt(edZ.Text) < 0) then
      OK := False
    else begin
      UX := StrToInt(edX.Text); UY := StrToInt(edY.Text); UZ := StrToInt(edZ.Text);
      case iSumType of
        1: if UY + UZ <> UX then OK := False;
        2: if UX + UZ <> UY then OK := False;
        3: if UX + UY <> UZ then OK := False;
      end;
    end;
    if OK then begin
      // Check pyramid values against the sums rule
      edNums[4, 0].Text := edX.Text; edNums[4, 2].Text := edY.Text; edNums[4, 4].Text := edZ.Text;
      for I := 3 downto 0 do begin
        for J := 0 to I do begin
          Sum := StrToInt(edNums[I + 1, J].Text) + StrToInt(edNums[I + 1, J + 1].Text);
          if Sum <> StrToInt(edNums[I, J].Text) then
            OK := False;
        end;
      end;
      edNums[4, 0].Text := 'X'; edNums[4, 2].Text := 'Y'; edNums[4, 4].Text := 'Z';
    end;
    // Display solution validation result
    if OK then begin
      MessageDlg('Number Pyramid"', 'Nice. You have found the correct solution!', mtInformation, [mbOK], 0);
      btAction.Caption := 'New';                 // User can start a new game
    end
    else begin
      MessageDlg('Number Pyramid"', 'Sorry, this is not the correct solution!', mtInformation, [mbOK], 0);
      btAction.Caption := 'Show';                // User can view the correct solution
    end;
    btReset.Enabled := False;
  end
  // Button "Show" pushed: Show the correct puzzle solution
  else begin
    for I := 0 to 4 do begin
      for J := 0 to 4 do begin
        if edNums[I, J] <> nil then begin
          if (I < 4) or ((I = 4) and (J in [1, 3])) then begin
            edNums[I, J].Text := IntToStr(Abs(aNums[I, J]));
          end;
        end;
      end;
    end;
    edX.Text := IntToStr(iX); edY.Text := IntToStr(iY); edZ.Text := IntToStr(iZ);
    btAction.Caption := 'New';
  end;
end;

{ Button "Reset" pushed: Clear the user input fields }

procedure TfPyramid.btResetClick(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to 4 do begin
    for J := 0 to 4 do begin
      if edNums[I, J] <> nil then begin
       if aNums[I, J] < 0 then
         edNums[I, J].Text := '';
      end;
    end;
  end;
  edX.Text := ''; edY.Text := ''; edZ.Text := '';
end;

end.

