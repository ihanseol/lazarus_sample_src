{**********************************************}
{* Real ice roll unit for Yahtzee application *}
{**********************************************}

unit yahtzee_dice2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {*********}
  { TfDice2 }
  {*********}
  TfDice2 = class(TForm)
    StaticText1: TStaticText;
    edHelp: TMemo;
    imDice1, imDice2, imDice3, imDice4, imDice5: TImage;
    edDice1, edDice2, edDice3, edDice4, edDice5: TEdit;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure edDice1Change(Sender: TObject);
    procedure edDice2Change(Sender: TObject);
    procedure edDice3Change(Sender: TObject);
    procedure edDice4Change(Sender: TObject);
    procedure edDice5Change(Sender: TObject);
  private
    edDices: array[0..4] of TEdit;
  public
    aDices: array[0..4] of 0..6;
    imDices: array[0..4] of TImage;
  end;

var
  fDice2: TfDice2;

implementation

{$R *.lfm}

{ Real dice roll value entry (for actual dice) }

procedure DiceValue(Dice: Integer; var DicesEdt: array of TEdit; var Dices: array of ShortInt; var DicesImg: array of TImage);

var
  FilePath: string;

begin
  if DicesEdt[Dice].Text = '' then begin
    // Nothing entered: Clear the dice
    Dices[Dice] := 0;
    DicesImg[Dice].Picture.Clear;
  end
  else begin
    if DicesEdt[Dice].Text[1] in ['1'..'6'] then begin
      // Dice roll value (1-6) entered: Display the dice
      Dices[Dice] := StrToInt(DicesEdt[Dice].Text);
      FilePath := './dices/' + 'dice' + IntToStr(Dices[Dice]) + '.jpg'; DoDirSeparators(FilePath);
      DicesImg[Dice].Picture.LoadFromFile(FilePath);
    end
    else begin
      // Invalid value entered: Clear the dice
      Dices[Dice] := 0;
      DicesEdt[Dice].Text := '';
      DicesImg[Dice].Picture.Clear;
    end;
  end;
end;

{*********}
{ TfDice2 }
{*********}

{ Application start: Initialisation }

procedure TfDice2.FormCreate(Sender: TObject);

begin
  // Create array with dice images
  imDices[0] := imDice1; imDices[1] := imDice2; imDices[2] := imDice3;
  imDices[3] := imDice4; imDices[4] := imDice5;
  // Create array with dice edit fields
  edDices[0] := edDice1; edDices[1] := edDice2; edDices[2] := edDice3;
  edDices[3] := edDice4; edDices[4] := edDice5;
end;

{ Window show-up: Clear dices }

procedure TfDice2.FormActivate(Sender: TObject);

var
  I: Integer;

begin
  for I := 0 to 4 do begin
    imDices[I].Picture.Clear; edDices[I].Text := ''; aDices[I] := 0;
  end;
end;

{ Button "Done": Check dice values entered and if OK, close window }

procedure TfDice2.btDoneClick(Sender: TObject);

var
  I: Integer;
  OK: Boolean;

begin
  OK := True;
  for I := 0 to 4 do begin
    if aDices[I] = 0 then                                                      // dice values entry routine sets dice = 0 for anything <> 1..6
      OK := False;
  end;
  if not OK then
    // One or more invalid dice values: Error message
    MessageDlg('Yahtzee', 'Invalid dice values!', mtError, [mbOK], 0)
  else
    // All dice values OK: Close dice roll window
    Close;
end;

{ Dice value entries (changed in edit fields): Get value for this dice from form (user entry) }

procedure TfDice2.edDice1Change(Sender: TObject);

begin
  DiceValue(0, edDices, aDices, imDices);
end;

procedure TfDice2.edDice2Change(Sender: TObject);

begin
  DiceValue(1, edDices, aDices, imDices);
end;

procedure TfDice2.edDice3Change(Sender: TObject);

begin
  DiceValue(2, edDices, aDices, imDices);
end;

procedure TfDice2.edDice4Change(Sender: TObject);

begin
  DiceValue(3, edDices, aDices, imDices);
end;

procedure TfDice2.edDice5Change(Sender: TObject);

begin
  DiceValue(4, edDices, aDices, imDices);
end;

end.

