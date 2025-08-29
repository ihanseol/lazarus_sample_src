{***********************************************}
{* Flag select unit for EuropaQuiz application *}
{***********************************************}

unit euroflags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

const
  NCountries = 47;

type
  TFlagImages = array[1..NCountries] of TImage;
  {*********}
  { TEFlags }
  {*********}
  TEFlags = class(TForm)
    Title: TStaticText;
    Label1: TLabel;
    imFlag1, imFlag2, imFlag3, imFlag4, imFlag5, imFlag6, imFlag7, imFlag8: TImage;
    imFlag9, imFlag10, imFlag11, imFlag12, imFlag13, imFlag14, imFlag15, imFlag16: TImage;
    imFlag17, imFlag18, imFlag19, imFlag20, imFlag21, imFlag22, imFlag23, imFlag24: TImage;
    imFlag25, imFlag26, imFlag27, imFlag28, imFlag29, imFlag30, imFlag31, imFlag32: TImage;
    imFlag33, imFlag34, imFlag35, imFlag36, imFlag37, imFlag38, imFlag39, imFlag40: TImage;
    imFlag41, imFlag42, imFlag43, imFlag44, imFlag45, imFlag46, imFlag47: TImage;
    btOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
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
    procedure imFlag1Click(Sender: TObject);
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
    procedure imFlag2Click(Sender: TObject);
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
    procedure imFlag3Click(Sender: TObject);
    procedure imFlag40Click(Sender: TObject);
    procedure imFlag41Click(Sender: TObject);
    procedure imFlag42Click(Sender: TObject);
    procedure imFlag43Click(Sender: TObject);
    procedure imFlag44Click(Sender: TObject);
    procedure imFlag45Click(Sender: TObject);
    procedure imFlag46Click(Sender: TObject);
    procedure imFlag47Click(Sender: TObject);
    procedure imFlag4Click(Sender: TObject);
    procedure imFlag5Click(Sender: TObject);
    procedure imFlag6Click(Sender: TObject);
    procedure imFlag7Click(Sender: TObject);
    procedure imFlag8Click(Sender: TObject);
    procedure imFlag9Click(Sender: TObject);
  private
    iFlag: Integer;
    aFlags:  array[1..NCountries] of Integer;
    imFlags: TFlagImages;
  public
    iCountry: Integer;
  end;

var
  EFlags: TEFlags;

implementation

{$R *.lfm}

{ Select the flag, the user has clicked onto }

procedure FlagSelect(NewFlag: Integer; Flags: TFlagImages; var Flag: Integer);

begin
  if Flag <> 0 then
    Flags[Flag].Stretch := False;                                              // reset flag selected before to normal size
  Flag := NewFlag;                                                             // actual flag (the one the user has clicked onto)
  Flags[Flag].Stretch := True;                                                 // enlarge actual flag (to show, that it is selected)
end;

{*********}
{ TEFlags }
{*********}

{ Application start: Create array with flag images }

procedure TEFlags.FormCreate(Sender: TObject);

begin
  imFlags[1] := imFlag1; imFlags[2] := imFlag2; imFlags[3] := imFlag3; imFlags[4] := imFlag4; imFlags[5] := imFlag5;
  imFlags[6] := imFlag6; imFlags[7] := imFlag7; imFlags[8] := imFlag8; imFlags[9] := imFlag9; imFlags[10] := imFlag10;
  imFlags[11] := imFlag11; imFlags[12] := imFlag12; imFlags[13] := imFlag13; imFlags[14] := imFlag14; imFlags[15] := imFlag15;
  imFlags[16] := imFlag16; imFlags[17] := imFlag17; imFlags[18] := imFlag18; imFlags[19] := imFlag19; imFlags[20] := imFlag20;
  imFlags[21] := imFlag21; imFlags[22] := imFlag22; imFlags[23] := imFlag23; imFlags[24] := imFlag24; imFlags[25] := imFlag25;
  imFlags[26] := imFlag26; imFlags[27] := imFlag27; imFlags[28] := imFlag28; imFlags[29] := imFlag29; imFlags[30] := imFlag30;
  imFlags[31] := imFlag31; imFlags[32] := imFlag32; imFlags[33] := imFlag33; imFlags[34] := imFlag34; imFlags[35] := imFlag35;
  imFlags[36] := imFlag36; imFlags[37] := imFlag37; imFlags[38] := imFlag38; imFlags[39] := imFlag39; imFlags[40] := imFlag40;
  imFlags[41] := imFlag41; imFlags[42] := imFlag42; imFlags[43] := imFlag43; imFlags[44] := imFlag44; imFlags[45] := imFlag45;
  imFlags[46] := imFlag46; imFlags[47] := imFlag47;
  Randomize;
end;

{ Form show-up: Initialisations }

procedure TEFlags.FormActivate(Sender: TObject);

var
  I, J: Integer;
  Filepath: string;

begin
  // Reset all flag indexes
  for I := 1 to NCountries do
    aFlags[I] := 0;
  // Randomly fill in the "grid" with the different flags
  for I := 1 to NCountries do begin                                            // for each flag
    repeat
      J := Random(NCountries) + 1;                                             // choose a random "grid" position...
    until aFlags[J] = 0;                                                       // ...that must be empty, of course
    aFlags[J] := I;                                                            // save flag index into "grid array"
    Filepath := './flags/' + IntToStr(I) + '.png'; DoDirSeparators(Filepath);
    imFlags[J].Picture.LoadFromFile(Filepath);                                 // display the flag (from file)
    // All flags with normal (small) size
    imFlags[J].Stretch := False;
  end;
  iFlag := 0;                                                                  // no flag actually selected
end;

{ Button "OK": Save (lastly) selected flag (for retrievel by main unit) and close the window}

procedure TEFlags.btOKClick(Sender: TObject);

begin
  iCountry := aFlags[iFlag];                                                   // country index for flag selected = flag filename
  Close;
end;

{ User click on one of the 47 flag images: Select this flag (as quiz question answer) }

procedure TEFlags.imFlag1Click(Sender: TObject);

begin
  FlagSelect(1, imFlags, iFlag);
end;

procedure TEFlags.imFlag2Click(Sender: TObject);

begin
  FlagSelect(2, imFlags, iFlag);
end;

procedure TEFlags.imFlag3Click(Sender: TObject);

begin
  FlagSelect(3, imFlags, iFlag);
end;

procedure TEFlags.imFlag4Click(Sender: TObject);

begin
  FlagSelect(4, imFlags, iFlag);
end;

procedure TEFlags.imFlag5Click(Sender: TObject);

begin
  FlagSelect(5, imFlags, iFlag);
end;

procedure TEFlags.imFlag6Click(Sender: TObject);

begin
  FlagSelect(6, imFlags, iFlag);
end;

procedure TEFlags.imFlag7Click(Sender: TObject);

begin
  FlagSelect(7, imFlags, iFlag);
end;

procedure TEFlags.imFlag8Click(Sender: TObject);

begin
  FlagSelect(8, imFlags, iFlag);
end;

procedure TEFlags.imFlag9Click(Sender: TObject);

begin
  FlagSelect(9, imFlags, iFlag);
end;

procedure TEFlags.imFlag10Click(Sender: TObject);

begin
  FlagSelect(10, imFlags, iFlag);
end;

procedure TEFlags.imFlag11Click(Sender: TObject);

begin
  FlagSelect(11, imFlags, iFlag);
end;

procedure TEFlags.imFlag12Click(Sender: TObject);

begin
  FlagSelect(12, imFlags, iFlag);
end;

procedure TEFlags.imFlag13Click(Sender: TObject);

begin
  FlagSelect(13, imFlags, iFlag);
end;

procedure TEFlags.imFlag14Click(Sender: TObject);

begin
  FlagSelect(14, imFlags, iFlag);
end;

procedure TEFlags.imFlag15Click(Sender: TObject);

begin
  FlagSelect(15, imFlags, iFlag);
end;

procedure TEFlags.imFlag16Click(Sender: TObject);

begin
  FlagSelect(16, imFlags, iFlag);
end;

procedure TEFlags.imFlag17Click(Sender: TObject);

begin
  FlagSelect(17, imFlags, iFlag);
end;

procedure TEFlags.imFlag18Click(Sender: TObject);

begin
  FlagSelect(18, imFlags, iFlag);
end;

procedure TEFlags.imFlag19Click(Sender: TObject);

begin
  FlagSelect(19, imFlags, iFlag);
end;

procedure TEFlags.imFlag20Click(Sender: TObject);

begin
  FlagSelect(20, imFlags, iFlag);
end;

procedure TEFlags.imFlag21Click(Sender: TObject);

begin
  FlagSelect(21, imFlags, iFlag);
end;

procedure TEFlags.imFlag22Click(Sender: TObject);

begin
  FlagSelect(22, imFlags, iFlag);
end;

procedure TEFlags.imFlag23Click(Sender: TObject);

begin
  FlagSelect(23, imFlags, iFlag);
end;

procedure TEFlags.imFlag24Click(Sender: TObject);

begin
  FlagSelect(24, imFlags, iFlag);
end;

procedure TEFlags.imFlag25Click(Sender: TObject);

begin
  FlagSelect(25, imFlags, iFlag);
end;

procedure TEFlags.imFlag26Click(Sender: TObject);

begin
  FlagSelect(26, imFlags, iFlag);
end;

procedure TEFlags.imFlag27Click(Sender: TObject);

begin
  FlagSelect(27, imFlags, iFlag);
end;

procedure TEFlags.imFlag28Click(Sender: TObject);

begin
  FlagSelect(28, imFlags, iFlag);
end;

procedure TEFlags.imFlag29Click(Sender: TObject);

begin
  FlagSelect(29, imFlags, iFlag);
end;

procedure TEFlags.imFlag30Click(Sender: TObject);

begin
  FlagSelect(30, imFlags, iFlag);
end;

procedure TEFlags.imFlag31Click(Sender: TObject);

begin
  FlagSelect(31, imFlags, iFlag);
end;

procedure TEFlags.imFlag32Click(Sender: TObject);

begin
  FlagSelect(32, imFlags, iFlag);
end;

procedure TEFlags.imFlag33Click(Sender: TObject);

begin
  FlagSelect(33, imFlags, iFlag);
end;

procedure TEFlags.imFlag34Click(Sender: TObject);

begin
  FlagSelect(34, imFlags, iFlag);
end;

procedure TEFlags.imFlag35Click(Sender: TObject);

begin
  FlagSelect(35, imFlags, iFlag);
end;

procedure TEFlags.imFlag36Click(Sender: TObject);

begin
  FlagSelect(36, imFlags, iFlag);
end;

procedure TEFlags.imFlag37Click(Sender: TObject);

begin
  FlagSelect(37, imFlags, iFlag);
end;

procedure TEFlags.imFlag38Click(Sender: TObject);

begin
  FlagSelect(38, imFlags, iFlag);
end;

procedure TEFlags.imFlag39Click(Sender: TObject);

begin
  FlagSelect(39, imFlags, iFlag);
end;

procedure TEFlags.imFlag40Click(Sender: TObject);

begin
  FlagSelect(40, imFlags, iFlag);
end;

procedure TEFlags.imFlag41Click(Sender: TObject);

begin
  FlagSelect(41, imFlags, iFlag);
end;

procedure TEFlags.imFlag42Click(Sender: TObject);

begin
  FlagSelect(42, imFlags, iFlag);
end;

procedure TEFlags.imFlag43Click(Sender: TObject);

begin
  FlagSelect(43, imFlags, iFlag);
end;

procedure TEFlags.imFlag44Click(Sender: TObject);

begin
  FlagSelect(44, imFlags, iFlag);
end;

procedure TEFlags.imFlag45Click(Sender: TObject);

begin
  FlagSelect(45, imFlags, iFlag);
end;

procedure TEFlags.imFlag46Click(Sender: TObject);

begin
  FlagSelect(46, imFlags, iFlag);
end;

procedure TEFlags.imFlag47Click(Sender: TObject);

begin
  FlagSelect(47, imFlags, iFlag);
end;

end.

