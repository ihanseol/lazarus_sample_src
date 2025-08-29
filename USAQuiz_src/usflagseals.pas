{*************************************************}
{* Flag/seal select unit for USAQuiz application *}
{*************************************************}

unit usflagseals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

const
  NStates = 50;

type
  TImages = array[1..NStates] of TImage;
  {****************}
  { TfUSAFlagSeals }
  {****************}
  TfUSAFlagSeals = class(TForm)
    stTitle: TStaticText;
    laSelect: TLabel;
    imFlagSeal1, imFlagSeal2, imFlagSeal3, imFlagSeal4, imFlagSeal5, imFlagSeal6, imFlagSeal7, imFlagSeal8, imFlagSeal9: TImage;
    imFlagSeal10, imFlagSeal11, imFlagSeal12, imFlagSeal13, imFlagSeal14, imFlagSeal15, imFlagSeal16, imFlagSeal17: TImage;
    imFlagSeal18, imFlagSeal19, imFlagSeal20, imFlagSeal21, imFlagSeal22, imFlagSeal23, imFlagSeal24, imFlagSeal25: TImage;
    imFlagSeal26, imFlagSeal27, imFlagSeal28, imFlagSeal29, imFlagSeal30, imFlagSeal31, imFlagSeal32, imFlagSeal33: TImage;
    imFlagSeal34, imFlagSeal35, imFlagSeal36, imFlagSeal37, imFlagSeal38, imFlagSeal39, imFlagSeal40, imFlagSeal41: TImage;
    imFlagSeal42, imFlagSeal43, imFlagSeal44, imFlagSeal45, imFlagSeal46, imFlagSeal47, imFlagSeal48, imFlagSeal49, imFlagSeal50: TImage;
    btOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure imFlag1Click(Sender: TObject);
    procedure imFlag2Click(Sender: TObject);
    procedure imFlag3Click(Sender: TObject);
    procedure imFlag4Click(Sender: TObject);
    procedure imFlag5Click(Sender: TObject);
    procedure imFlag6Click(Sender: TObject);
    procedure imFlag7Click(Sender: TObject);
    procedure imFlag8Click(Sender: TObject);
    procedure imFlag9Click(Sender: TObject);
    procedure imFlag10Click(Sender: TObject);
    procedure imFlag11Click(Sender: TObject);
    procedure imFlag12Click(Sender: TObject);
    procedure imFlag13Click(Sender: TObject);
    procedure imFlag14Click(Sender: TObject);
    procedure imFlag15Click(Sender: TObject);
    procedure imFlag16Click(Sender: TObject);
    procedure imFlag17Click(Sender: TObject);
    procedure imFlag18Click(Sender: TObject);
    procedure imFlag19Click(Sender: TObject);
    procedure imFlag20Click(Sender: TObject);
    procedure imFlag21Click(Sender: TObject);
    procedure imFlag22Click(Sender: TObject);
    procedure imFlag23Click(Sender: TObject);
    procedure imFlag24Click(Sender: TObject);
    procedure imFlag25Click(Sender: TObject);
    procedure imFlag26Click(Sender: TObject);
    procedure imFlag27Click(Sender: TObject);
    procedure imFlag28Click(Sender: TObject);
    procedure imFlag29Click(Sender: TObject);
    procedure imFlag30Click(Sender: TObject);
    procedure imFlag31Click(Sender: TObject);
    procedure imFlag32Click(Sender: TObject);
    procedure imFlag33Click(Sender: TObject);
    procedure imFlag34Click(Sender: TObject);
    procedure imFlag35Click(Sender: TObject);
    procedure imFlag36Click(Sender: TObject);
    procedure imFlag37Click(Sender: TObject);
    procedure imFlag38Click(Sender: TObject);
    procedure imFlag39Click(Sender: TObject);
    procedure imFlag40Click(Sender: TObject);
    procedure imFlag41Click(Sender: TObject);
    procedure imFlag42Click(Sender: TObject);
    procedure imFlag43Click(Sender: TObject);
    procedure imFlag44Click(Sender: TObject);
    procedure imFlag45Click(Sender: TObject);
    procedure imFlag46Click(Sender: TObject);
    procedure imFlag47Click(Sender: TObject);
    procedure imFlag48Click(Sender: TObject);
    procedure imFlag49Click(Sender: TObject);
    procedure imFlag50Click(Sender: TObject);
  private
    iFlagSeal: Integer;
    aFlagSeals: array[1..NStates] of Integer;
    imFlagSeals: TImages;
  public
    iQuiz, iState: Integer;
  end;

var
  fUSAFlagSeals: TfUSAFlagSeals;

implementation

{$R *.lfm}

{ Select the flag/seal, the user has clicked onto }

procedure FlagSealSelect(NewFlagSeal: Integer; FlagSeals: TImages; var FlagSeal: Integer);

begin
  if FlagSeal <> 0 then
    FlagSeals[FlagSeal].Stretch := False;                                      // reset flag/seal selected before to normal size
  FlagSeal := NewFlagSeal;                                                     // actual flag/seal (the one, that the user has clicked onto)
  FlagSeals[FlagSeal].Stretch := True;                                         // enlarge actual flag/seal (to show, that it is selected)
end;

{****************}
{ TfUSAFlagSeals }
{****************}

{ Application start: Create array with images objects }

procedure TfUSAFlagSeals.FormCreate(Sender: TObject);

begin
  imFlagSeals[1] := imFlagSeal1; imFlagSeals[2] := imFlagSeal2; imFlagSeals[3] := imFlagSeal3; imFlagSeals[4] := imFlagSeal4; imFlagSeals[5] := imFlagSeal5;
  imFlagSeals[6] := imFlagSeal6; imFlagSeals[7] := imFlagSeal7; imFlagSeals[8] := imFlagSeal8; imFlagSeals[9] := imFlagSeal9; imFlagSeals[10] := imFlagSeal10;
  imFlagSeals[11] := imFlagSeal11; imFlagSeals[12] := imFlagSeal12; imFlagSeals[13] := imFlagSeal13; imFlagSeals[14] := imFlagSeal14; imFlagSeals[15] := imFlagSeal15;
  imFlagSeals[16] := imFlagSeal16; imFlagSeals[17] := imFlagSeal17; imFlagSeals[18] := imFlagSeal18; imFlagSeals[19] := imFlagSeal19; imFlagSeals[20] := imFlagSeal20;
  imFlagSeals[21] := imFlagSeal21; imFlagSeals[22] := imFlagSeal22; imFlagSeals[23] := imFlagSeal23; imFlagSeals[24] := imFlagSeal24; imFlagSeals[25] := imFlagSeal25;
  imFlagSeals[26] := imFlagSeal26; imFlagSeals[27] := imFlagSeal27; imFlagSeals[28] := imFlagSeal28; imFlagSeals[29] := imFlagSeal29; imFlagSeals[30] := imFlagSeal30;
  imFlagSeals[31] := imFlagSeal31; imFlagSeals[32] := imFlagSeal32; imFlagSeals[33] := imFlagSeal33; imFlagSeals[34] := imFlagSeal34; imFlagSeals[35] := imFlagSeal35;
  imFlagSeals[36] := imFlagSeal36; imFlagSeals[37] := imFlagSeal37; imFlagSeals[38] := imFlagSeal38; imFlagSeals[39] := imFlagSeal39; imFlagSeals[40] := imFlagSeal40;
  imFlagSeals[41] := imFlagSeal41; imFlagSeals[42] := imFlagSeal42; imFlagSeals[43] := imFlagSeal43; imFlagSeals[44] := imFlagSeal44; imFlagSeals[45] := imFlagSeal45;
  imFlagSeals[46] := imFlagSeal46; imFlagSeals[47] := imFlagSeal47; imFlagSeals[48] := imFlagSeal48; imFlagSeals[49] := imFlagSeal49; imFlagSeals[50] := imFlagSeal50;
  Randomize;
end;

{ Window show-up: Initialisation }

procedure TfUSAFlagSeals.FormActivate(Sender: TObject);

var
  I, J: Integer;
  FileDir, Filepath: string;

begin
  // Set title
  if iQuiz = 5 then begin
    stTitle.Caption := 'USA state flags.';
    laSelect.Caption := 'To select a flag';
    FileDir := './flags/';
  end
  else begin
    stTitle.Caption := 'USA state seals.';
    laSelect.Caption := 'To select a seal';
    FileDir := './seals/';
  end;
  laSelect.Caption := laSelect.Caption + ', please, click it. When done, push "OK".';
  // Reset all flag/seals indexes
  for I := 1 to NStates do
    aFlagSeals[I] := 0;
  // Randomly fill in the "grid" with the different flags/seals
  for I := 1 to NStates do begin                                               // for each flag/seal
    repeat
      J := Random(NStates) + 1;                                                // choose a random "grid" position...
    until aFlagSeals[J] = 0;                                                   // ...that must be empty, of course
    aFlagSeals[J] := I;                                                        // save flag/seal index into "grid array"
    Filepath := FileDir + IntToStr(I) + '.jpg'; DoDirSeparators(Filepath);
    imFlagSeals[J].Picture.LoadFromFile(Filepath);                             // display the flag/seal (from file)
    // All flags/seals with normal (small) size
    imFlagSeals[J].Stretch := False;
  end;
  iFlagSeal := 0;                                                              // no flag/seal actually selected
end;

{ Button "OK": Save (lastly) selected flag/seal (for retrievel by main unit) and close the window }

procedure TfUSAFlagSeals.btOKClick(Sender: TObject);

begin
  iState := aFlagSeals[iFlagSeal];                                             // state index for flag/seal selected = flag/seal filename
  Close;
end;

{ User click on one of the 50 flag/seal images: Select this flag/seal (as quiz question answer) }

procedure TfUSAFlagSeals.imFlag1Click(Sender: TObject);

begin
  FlagSealSelect(1, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag2Click(Sender: TObject);

begin
  FlagSealSelect(2, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag3Click(Sender: TObject);

begin
  FlagSealSelect(3, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag4Click(Sender: TObject);

begin
  FlagSealSelect(4, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag5Click(Sender: TObject);

begin
  FlagSealSelect(5, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag6Click(Sender: TObject);

begin
  FlagSealSelect(6, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag7Click(Sender: TObject);

begin
  FlagSealSelect(7, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag8Click(Sender: TObject);

begin
  FlagSealSelect(8, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag9Click(Sender: TObject);

begin
  FlagSealSelect(9, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag10Click(Sender: TObject);

begin
  FlagSealSelect(10, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag11Click(Sender: TObject);

begin
  FlagSealSelect(11, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag12Click(Sender: TObject);

begin
  FlagSealSelect(12, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag13Click(Sender: TObject);

begin
  FlagSealSelect(13, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag14Click(Sender: TObject);

begin
  FlagSealSelect(14, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag15Click(Sender: TObject);

begin
  FlagSealSelect(15, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag16Click(Sender: TObject);

begin
  FlagSealSelect(16, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag17Click(Sender: TObject);

begin
  FlagSealSelect(17, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag18Click(Sender: TObject);

begin
  FlagSealSelect(18, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag19Click(Sender: TObject);

begin
  FlagSealSelect(19, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag20Click(Sender: TObject);

begin
  FlagSealSelect(20, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag21Click(Sender: TObject);

begin
  FlagSealSelect(21, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag22Click(Sender: TObject);

begin
  FlagSealSelect(22, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag23Click(Sender: TObject);

begin
  FlagSealSelect(23, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag24Click(Sender: TObject);

begin
  FlagSealSelect(24, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag25Click(Sender: TObject);

begin
  FlagSealSelect(25, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag26Click(Sender: TObject);

begin
  FlagSealSelect(26, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag27Click(Sender: TObject);

begin
  FlagSealSelect(27, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag28Click(Sender: TObject);

begin
  FlagSealSelect(28, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag29Click(Sender: TObject);

begin
  FlagSealSelect(29, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag30Click(Sender: TObject);

begin
  FlagSealSelect(30, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag31Click(Sender: TObject);

begin
  FlagSealSelect(31, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag32Click(Sender: TObject);

begin
  FlagSealSelect(32, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag33Click(Sender: TObject);

begin
  FlagSealSelect(33, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag34Click(Sender: TObject);

begin
  FlagSealSelect(34, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag35Click(Sender: TObject);

begin
  FlagSealSelect(35, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag36Click(Sender: TObject);

begin
  FlagSealSelect(36, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag37Click(Sender: TObject);

begin
  FlagSealSelect(37, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag38Click(Sender: TObject);

begin
  FlagSealSelect(38, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag39Click(Sender: TObject);

begin
  FlagSealSelect(39, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag40Click(Sender: TObject);

begin
  FlagSealSelect(40, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag41Click(Sender: TObject);

begin
  FlagSealSelect(41, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag42Click(Sender: TObject);

begin
  FlagSealSelect(42, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag43Click(Sender: TObject);

begin
  FlagSealSelect(43, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag44Click(Sender: TObject);

begin
  FlagSealSelect(44, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag45Click(Sender: TObject);

begin
  FlagSealSelect(45, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag46Click(Sender: TObject);

begin
  FlagSealSelect(46, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag47Click(Sender: TObject);

begin
  FlagSealSelect(47, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag48Click(Sender: TObject);

begin
  FlagSealSelect(48, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag49Click(Sender: TObject);

begin
  FlagSealSelect(49, imFlagSeals, iFlagSeal);
end;

procedure TfUSAFlagSeals.imFlag50Click(Sender: TObject);

begin
  FlagSealSelect(50, imFlagSeals, iFlagSeal);
end;

end.

