{******************************************}
{* Dice roll unit for Yahtzee application *}
{******************************************}

unit yahtzee_dice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  {********}
  { TfDice }
  {********}
  TfDice = class(TForm)
    StaticText1: TStaticText;
    imDice1, imDice2, imDice3, imDice4, imDice5: TImage;
    shDice1, shDice2, shDice3, shDice4, shDice5: TShape;
    edHelp: TMemo;
    btReroll: TButton;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btRerollClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure imDice1Click(Sender: TObject);
    procedure imDice2Click(Sender: TObject);
    procedure imDice3Click(Sender: TObject);
    procedure imDice4Click(Sender: TObject);
    procedure imDice5Click(Sender: TObject);
  private
    iRolls: Integer;
    shDices: array[0..4] of TShape;
  public
    sSelect: string;
    bAutoClose: Boolean;
    aDices: array[0..4] of 1..6;
    imDices: array[0..4] of TImage;
  end;

var
  fDice: TfDice;

implementation

{$R *.lfm}

{ Mark the dice, that the user has selected (to reroll or to keep) }

procedure SelectDice(Dice: Integer; DiceSelect: string; Dices: array of TShape);

begin
  if Dices[Dice].Visible then
    // Unmark the dice if it had been selected before
    Dices[Dice].Visible := False
  else begin
    // Mark the dice (red=reroll, lime=keep)
    Dices[Dice].Visible := True;
    if DiceSelect = 'keep' then
      Dices[Dice].Brush.Color := clLime
    else
      Dices[Dice].Brush.Color := clRed;
  end;
end;

{********}
{ TfDice }
{********}

{ Application start: Initialisation }

procedure TfDice.FormCreate(Sender: TObject);

begin
  // Create array with dice images
  imDices[0] := imDice1; imDices[1] := imDice2; imDices[2] := imDice3;
  imDices[3] := imDice4; imDices[4] := imDice5;
  // Create array with dice selection shapes
  shDices[0] := shDice1; shDices[1] := shDice2; shDices[2] := shDice3;
  shDices[3] := shDice4; shDices[4] := shDice5;
end;

{ Window show-up: Roll the dices }

procedure TfDice.FormActivate(Sender: TObject);

var
  Dice, I: Integer;
  FilePath: string;

begin
  // Adapt help text
  if sSelect = 'keep' then
    edHelp.Text := StringReplace(edHelp.Text, 'want to reroll', 'want to keep', [])
  else
    edHelp.Text := StringReplace(edHelp.Text, 'want to keep', 'want to reroll', []);
  // Roll the 5 dices
  for I := 0 to 4 do begin
    Dice := Random(6) + 1;
    FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
    imDices[I].Picture.LoadFromFile(FilePath);
    aDices[I] := Dice;                                                          // actual dice value
    shDices[I].Visible := False;                                                // hide the selection shapes
  end;
  iRolls := 1;                                                                  // this was the first roll
  btReroll.Enabled := True;                                                     // give user access to "Reroll" button
end;

{ Button "Reroll": Reroll the dices }

procedure TfDice.btRerollClick(Sender: TObject);

var
  Dice, N, I: Integer;
  FilePath: string;

begin
  // Check if at least one dice has been selected
  N := 0;
  for I := 0 to 4 do begin
    if shDices[I].Visible then
      Inc(N);
  end;
  if ((sSelect = 'keep') and (N = 5)) or ((sSelect = 'reroll') and (N = 0)) then
    // Nothing selected to reroll or all selected to keep
    MessageDlg('Yahtzee', 'There are no dices selected for rerolling!', mtError, [mbOK], 0)
  else begin
    // At least one dice has to be rerolled
    for I := 0 to 4 do begin
      if ((sSelect = 'keep') and (not shDices[I].Visible)) or ((sSelect = 'reroll') and shDices[I].Visible) then begin
        // Reroll the dice if it has to be done so
        Dice := Random(6) + 1;
        FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
        imDices[I].Picture.LoadFromFile(FilePath);
        aDices[I] := Dice;
      end;
    end;
    Inc(iRolls);                                                               // number of times the dices have been rolled
    if iRolls >= 3 then begin
      // Dices have been rolled 3 times
      if bAutoClose then
        // Autoclose the window
        btDone.Click
      else
        // Disable the "Reroll" button (all that remains to user is to close the window manually)
        btReroll.Enabled := False;
    end;
  end;
  // Hide all selection shapes (user must do new selection before rerolling )
  for I := 0 to 4 do begin
    shDices[I].Visible := False;
  end;
end;

{ Button "Done": Close the dice rolling window }

procedure TfDice.btDoneClick(Sender: TObject);

begin
  Close;
end;

{ User-click on dice images: Select the dices }

procedure TfDice.imDice1Click(Sender: TObject);

begin
  SelectDice(0, sSelect, shDices);
end;

procedure TfDice.imDice2Click(Sender: TObject);

begin
  SelectDice(1, sSelect, shDices);
end;

procedure TfDice.imDice3Click(Sender: TObject);

begin
  SelectDice(2, sSelect, shDices);
end;

procedure TfDice.imDice4Click(Sender: TObject);

begin
  SelectDice(3, sSelect, shDices);
end;

procedure TfDice.imDice5Click(Sender: TObject);

begin
  SelectDice(4, sSelect, shDices);
end;

end.

