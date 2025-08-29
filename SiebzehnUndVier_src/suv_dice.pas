{**************************************************}
{* Dice roll unit for SiebzehnUndVier application *}
{**************************************************}

unit suv_dice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  TAllDices = array[0..3, 0..2] of Integer;
  {********}
  { TfDice }
  {********}
  TfDice = class(TForm)
    StaticText1: TStaticText;
    rbDices1, rbDices2, rbDices3: TRadioButton;
    imDice1, imDice2, imDice3: TImage;
    laTotal: TLabel;
    edScore: TEdit;
    edTotal: TEdit;
    btRoll: TButton;
    btStop: TButton;
    btDone: TButton;
    tiRoll: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btRollClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure rbDices1Change(Sender: TObject);
    procedure rbDices2Change(Sender: TObject);
    procedure rbDices3Change(Sender: TObject);
    procedure tiRollTimer(Sender: TObject);
  private
    iRolls, iRoll, iDices: Integer;
    aDices: array[0..2] of Integer;
    imDices: array[0..2] of TImage;
  public
    iTotal: Integer;
    sDiceRoll: string;
    bFirstAuto: Boolean;
    aAllDices: TAllDices;
  end;

var
  fDice: TfDice;

implementation

{$R *.lfm}

{********}
{ TfDice }
{********}

{ Application start: Initialisation }

procedure TfDice.FormCreate(Sender: TObject);

begin
  // Create array with dice images
  imDices[0] := imDice1; imDices[1] := imDice2; imDices[2] := imDice3;
end;

{ Window pop-up (form becoming active): Initialisation for this dice rolling }

procedure TfDice.FormActivate(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 0 to 2 do
    imDices[I].Picture.Clear;
  laTotal.Caption := 'Total nach einem Wurf:';
  edScore.Text := ''; edScore.Color := clDefault; edTotal.Text := ''; edTotal.Color := clDefault;
  rbDices3.Checked := True; rbDices2.Enabled := False; rbDices1.Enabled := False;
  iRolls := 0; iDices := 3;
  if sDiceRoll = 'interactive' then
    btStop.Enabled := True
  else
    btStop.Enabled := False;
  btRoll.Enabled := True;
  btDone.Enabled := False;
  for I := 0 to 3 do begin
    for J := 0 to 2 do
      aAllDices[I, J] := 0;
  end;
  // If user selected to do first throw automatically (without button push), roll the dices now
  if bFirstAuto then
    btRoll.Click;
end;

{ Button "Würfeln": Roll the dices }

procedure TfDice.btRollClick(Sender: TObject);

// This routine does nothing more than counting the number of throws and if less or equal to 4 starts the dice rolling timer
// All dice rolling code within the timer routine...

begin
  Inc(iRolls);                                                                 // number of actual throw
  if iRolls <= 4 then begin
    iRoll := 0;                                                                // counter used to stop the timer
    tiRoll.Enabled := True;
  end;
  btRoll.Enabled := False;
end;

{ Button "Stop": Stop dice rolling (interactive mode) }

procedure TfDice.btStopClick(Sender: TObject);

begin
  iRoll := -1;                                                                 // interactive mode with counter = -1 will stop the timer
end;

{ Dice rolling timer routine }

procedure TfDice.tiRollTimer(Sender: TObject);

var
  Dice, Score, I: Integer;
  FilePath: string;

begin
  if iRolls > 1 then
    laTotal.Caption := 'Total nach ' + IntToStr(iRolls) + ' Würfen:';
  Inc(iRoll);                                                                  // increment the counter that's used to stop the dices rolling
  if ((sDiceRoll = 'application') and (iRoll <= 10)) or ((sDiceRoll = 'interactive') and (iRoll <> 0)) then begin
    // Continue rolling until this has been done 10 times (normal mode) resp. stop it, if user pushed the "Stop" button (interactive mode)
    for I := 0 to 2 do
      imDices[I].Visible := False;
    // Roll the 3 dices (random dice values)
    for I := 0 to iDices - 1 do begin
      imDices[I].Visible := True;
      Dice := Random(6) + 1;
      FilePath := './dices/' + 'dice' + IntToStr(Dice) + '.jpg'; DoDirSeparators(FilePath);
      imDices[I].Picture.LoadFromFile(FilePath);
      aDices[I] := Dice;
      aAllDices[iRolls - 1, I] := Dice;
    end;
  end
  else begin
    // Dice rolling is done for this throw
    tiRoll.Enabled := False;
    Score := 0;
    // Calculate score for this throw (sum of the 3 dices)
    for I := 0 to iDices - 1 do
      Score += aDices[I];
    edScore.Text := IntToStr(Score);
    // Calculate total score (for all throws)
    if edTotal.Text = '' then
      iTotal := StrToInt(edScore.Text)
    else
      iTotal := StrToInt(edTotal.Text) + Score;
    edTotal.Text := IntToStr(iTotal);
    edTotal.Color := clDefault;
    // Highlight "exact 21" and "over 21"
    if (iTotal = 21) and (iRolls >= 3) then
      edTotal.Color := clLime
    else if iTotal > 21 then
      edTotal.Color := clRed;
    // Enable dices selection and buttons
    if (iTotal <= 21) and (iRolls <= 3) then
      btRoll.Enabled := True;                                                  // user may throw the dices another time
    rbDices1.Enabled := True; rbDices2.Enabled := True;                        // user may choose the number of dices to be used
    if (iRolls = 3) or (iTotal > 21) then
      btDone.Enabled := True;                                                  // window may be closed if 3 throws done or score exceeds 21
  end;
end;

{ Button "Fertig": Close the dice rolling window }

procedure TfDice.btDoneClick(Sender: TObject);

begin
  Close;
end;

{ User chose to use a given number of dices (change of radiobutton selected): Set number of dices variable accordingly }

procedure TfDice.rbDices1Change(Sender: TObject);

begin
  if btRoll.Enabled then begin
    if rbDices1.Checked then
      iDices := 1;
  end;
end;

procedure TfDice.rbDices2Change(Sender: TObject);

begin
  if btRoll.Enabled then begin
    if rbDices2.Checked then
      iDices := 2;
  end;
end;

procedure TfDice.rbDices3Change(Sender: TObject);

begin
  if btRoll.Enabled then begin
    if rbDices3.Checked then
      iDices := 3;
  end;
end;

end.

