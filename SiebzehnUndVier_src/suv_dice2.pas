{*****************************************************************************}
{* Real dice roll (= dice values input) unit for SiebzehnUndVier application *}
{*****************************************************************************}

unit suv_dice2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TAllDices  = array[0..3, 0..2] of Integer;
  TDicesEdit = array[0..3, 0..3] of TEdit;
  {*********}
  { TfDice2 }
  {*********}
  TfDice2 = class(TForm)
    StaticText1: TStaticText;
    edHelp: TMemo;
    Label1, Label2, Label3, Label4: TLabel;
    edDice11, edDice12, edDice13, edDice3S, edDice41, edDice42, edDice43, edDice4S: TEdit;
    edDice1S, edDice21, edDice22, edDice23, edDice2S, edDice31, edDice32, edDice33: TEdit;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure edDice11Change(Sender: TObject);
    procedure edDice12Change(Sender: TObject);
    procedure edDice13Change(Sender: TObject);
    procedure edDice21Change(Sender: TObject);
    procedure edDice22Change(Sender: TObject);
    procedure edDice23Change(Sender: TObject);
    procedure edDice31Change(Sender: TObject);
    procedure edDice32Change(Sender: TObject);
    procedure edDice33Change(Sender: TObject);
    procedure edDice41Change(Sender: TObject);
    procedure edDice42Change(Sender: TObject);
    procedure edDice43Change(Sender: TObject);
  private
    edDices: TDicesEdit;
  public
    iTotal: Integer;
    aAllDices: TAllDices;
  end;

var
  fDice2: TfDice2;

implementation

{$R *.lfm}

{ Real dice roll value input (for actual dice) }

procedure DiceValue(Throw, Dice: Integer; var DicesEdt: TDicesEdit; var Dices: TAllDices);

var
  Sum, I: Integer;
  OK: Boolean;

begin
  if DicesEdt[Throw, Dice].Text = '' then begin
    Dices[Throw, Dice] := 0;
  end
  else if DicesEdt[Throw, Dice].Text[1] in ['1'..'6'] then begin
    Dices[Throw, Dice] := StrToInt(DicesEdt[Throw, Dice].Text);
  end
  else begin
    Dices[Throw, Dice] := -1;
    DicesEdt[Throw, Dice].Text := '';                                          // any value not included in [1,6] will be removed
  end;
  OK := True;
  // Input fields must be filled in without letting gaps
  // The user cannot enter a value for the second dice of a throw, if there isn't a value for the first dice...
  // The user cannot enter a dice value for the second throw, if there isn't a value for the first throw...
  if Dices[Throw, Dice] > 0 then begin
    // A valid dice value has been entered by the user
    if Dice = 0 then begin
      // For the first dice of a throw, check if first values of throws before have been filled in
      if Throw > 0 then begin
        for I := 0 to Throw - 1 do begin
          if Dices[I, Dice] < 1 then
            OK := False;
        end;
      end;
    end
    else begin
      // For other dices, check if, for this throw, all dice values to the left of it have been filled in
      for I := 0 to Dice - 1 do begin
        if Dices[Throw, I] < 1 then
          OK := False;
      end;
    end;
  end;
  // If user input is as it is supposed to be, make the sum of the dice values for this throw
  if OK then begin
    Sum := 0;
    for I := 0 to 2 do begin
      if Dices[Throw, I] > 0 then
        Sum += Dices[Throw, I];
    end;
    DicesEdt[Throw, 3].Text := IntToStr(Sum);                                  // display score for this throw
  end
  // If user input is not as it should be, clear the corresponding edit field
  else begin
    Dices[Throw, Dice] := -1;
    DicesEdt[Throw, Dice].Text := '';
  end;
end;

{*********}
{ TfDice2 }
{*********}

{ Application start: Initialisation }

procedure TfDice2.FormCreate(Sender: TObject);

begin
  // Create array with dice edit fields (incl. the score fileds)
  edDices[0, 0] := edDice11; edDices[0, 1] := edDice12; edDices[0, 2] := edDice13; edDices[0, 3] := edDice1S;
  edDices[1, 0] := edDice21; edDices[1, 1] := edDice22; edDices[1, 2] := edDice23; edDices[1, 3] := edDice2S;
  edDices[2, 0] := edDice31; edDices[2, 1] := edDice32; edDices[2, 2] := edDice33; edDices[2, 3] := edDice3S;
  edDices[3, 0] := edDice41; edDices[3, 1] := edDice42; edDices[3, 2] := edDice43; edDices[3, 3] := edDice4S;
end;

{ Window show-up: Clear dice values }

procedure TfDice2.FormActivate(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to 3 do begin
    for J := 0 to 3 do begin
      edDices[I, J].Text := '';
      if J < 3 then
        aAllDices[I, J] := 0;
    end;
  end;
  edDices[0, 0].SetFocus;
end;

{ User input of dice value (change of corr. edit field): Check if value is valid and if so, store it into array }

procedure TfDice2.edDice11Change(Sender: TObject);

begin
  DiceValue(0, 0, edDices, aAllDices);
end;

procedure TfDice2.edDice12Change(Sender: TObject);

begin
  DiceValue(0, 1, edDices, aAllDices);
end;

procedure TfDice2.edDice13Change(Sender: TObject);

begin
  DiceValue(0, 2, edDices, aAllDices);
end;

procedure TfDice2.edDice21Change(Sender: TObject);

begin
  DiceValue(1, 0, edDices, aAllDices);
end;

procedure TfDice2.edDice22Change(Sender: TObject);

begin
  DiceValue(1, 1, edDices, aAllDices);
end;

procedure TfDice2.edDice23Change(Sender: TObject);

begin
  DiceValue(1, 2, edDices, aAllDices);
end;

procedure TfDice2.edDice31Change(Sender: TObject);

begin
  DiceValue(2, 0, edDices, aAllDices);
end;

procedure TfDice2.edDice32Change(Sender: TObject);

begin
  DiceValue(2, 1, edDices, aAllDices);
end;

procedure TfDice2.edDice33Change(Sender: TObject);

begin
  DiceValue(2, 2, edDices, aAllDices);
end;

procedure TfDice2.edDice41Change(Sender: TObject);

begin
  DiceValue(3, 0, edDices, aAllDices);
end;

procedure TfDice2.edDice42Change(Sender: TObject);

begin
  DiceValue(3, 1, edDices, aAllDices);
end;

procedure TfDice2.edDice43Change(Sender: TObject);

begin
  DiceValue(3, 2, edDices, aAllDices);
end;

{ Button "Fertig": Do some final check on user input and, if all ok, calculate total for all throws and close the window }

procedure TfDice2.btDoneClick(Sender: TObject);

var
  I, J: Integer;
  Mess: string;

begin
  Mess := '';
  for I := 0 to 3 do begin
    for J := 0 to 2 do begin
      if Mess = '' then begin
        if aAllDices[I, J] = -1 then                                           // dice values entry routine sets dice = -1 for anything <> 1..6 or empty
          Mess := 'Ungültige Würfelaugen'
        else if (I = 0) and (aAllDices[I, J] = 0) then
          Mess := 'Beim ersten Wurf müssen alle 3 Würfel benutzt werden'
        else if ((I = 1) or (I = 2)) and (J = 0) and (aAllDices[I, J] = 0) then
          Mess := 'Es muss mindestens 3x gewürfelt werden';
      end;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Eingabefehler', Mess + '!', mtError, [mbOK], 0)
  else begin
    // Calculate total for all 3 (resp. 4) throws
    iTotal := 0;
    for I := 0 to 3 do begin
      if edDices[I, 3].Text <> '' then
        iTotal += StrToInt(edDices[I, 3].Text);
    end;
    Close;
  end;
end;

end.

