{****************************************************}
{* Flag select unit for DeutschlandQuiz application *}
{****************************************************}

unit deflags;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

const
  NLands = 16;

type
  TImages = array[1..NLands] of TImage;
  {***********}
  { TfDEFlags }
  {***********}
  TfDEFlags = class(TForm)
    stTitle: TStaticText;
    laSelect: TLabel;
    imFlag1, imFlag2, imFlag3, imFlag4, imFlag5, imFlag6, imFlag7, imFlag8, imFlag9: TImage;
    imFlag10, imFlag11, imFlag12, imFlag13, imFlag14, imFlag15, imFlag16: TImage;
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

  private
    iFlag: Integer;
    aFlags: array[1..NLands] of Integer;
    imFlags: TImages;
  public
    iQuiz, iLand: Integer;
  end;

var
  fDEFlags: TfDEFlags;

implementation

{$R *.lfm}

{ Select the flag, the user has clicked onto }

procedure FlagSelect(NewFlag: Integer; Flags: TImages; var Flag: Integer);

begin
  if Flag <> 0 then
    Flags[Flag].Stretch := False;                                              // reset flag selected before to normal size
  Flag := NewFlag;                                                             // actual flag (the one, that the user has clicked onto)
  Flags[Flag].Stretch := True;                                                 // enlarge actual flag (to show, that it is selected)
end;

{***********}
{ TfDEFlags }
{***********}

{ Application start: Create array with flag image objects }

procedure TfDEFlags.FormCreate(Sender: TObject);

begin
  imFlags[1] := imFlag1; imFlags[2] := imFlag2; imFlags[3] := imFlag3; imFlags[4] := imFlag4;
  imFlags[5] := imFlag5; imFlags[6] := imFlag6; imFlags[7] := imFlag7; imFlags[8] := imFlag8;
  imFlags[9] := imFlag9; imFlags[10] := imFlag10; imFlags[11] := imFlag11; imFlags[12] := imFlag12;
  imFlags[13] := imFlag13; imFlags[14] := imFlag14; imFlags[15] := imFlag15; imFlags[16] := imFlag16;
  //Randomize;
end;

{ Window show-up: Initialisation }

procedure TfDEFlags.FormActivate(Sender: TObject);

var
  I, J: Integer;
  FileDir, Filepath: string;

begin
  FileDir := './flags/';
  // Reset all flag indexes
  for I := 1 to NLands do
    aFlags[I] := 0;
  // Randomly fill-in the "grid" with the different flags
  for I := 1 to NLands do begin                                                // for each flag
    repeat
      J := Random(NLands) + 1;                                                 // choose a random "grid" position...
    until aFlags[J] = 0;                                                       // ...that must be empty, of course
    aFlags[J] := I;                                                            // save flag index into "grid array"
    Filepath := FileDir + IntToStr(I) + '.jpg'; DoDirSeparators(Filepath);
    imFlags[J].Picture.LoadFromFile(Filepath);                                 // display the flag (from file)
    imFlags[J].Stretch := False;                                               // all flags with normal (small) size
  end;
  iFlag := 0;                                                                  // no flag actually selected
end;

{ Button "OK": Save (lastly) selected flag (for retrievel by main unit) and close the window }

procedure TfDEFlags.btOKClick(Sender: TObject);

begin
  iLand := aFlags[iFlag];                                                      // land index for flag selected = flag filename
  Close;
end;

{ User click on one of the 16 flag images: Select this flag (as quiz question answer) }

procedure TfDEFlags.imFlag1Click(Sender: TObject);

begin
  FlagSelect(1, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag2Click(Sender: TObject);

begin
  FlagSelect(2, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag3Click(Sender: TObject);

begin
  FlagSelect(3, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag4Click(Sender: TObject);

begin
  FlagSelect(4, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag5Click(Sender: TObject);

begin
  FlagSelect(5, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag6Click(Sender: TObject);

begin
  FlagSelect(6, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag7Click(Sender: TObject);

begin
  FlagSelect(7, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag8Click(Sender: TObject);

begin
  FlagSelect(8, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag9Click(Sender: TObject);

begin
  FlagSelect(9, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag10Click(Sender: TObject);

begin
  FlagSelect(10, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag11Click(Sender: TObject);

begin
  FlagSelect(11, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag12Click(Sender: TObject);

begin
  FlagSelect(12, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag13Click(Sender: TObject);

begin
  FlagSelect(13, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag14Click(Sender: TObject);

begin
  FlagSelect(14, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag15Click(Sender: TObject);

begin
  FlagSelect(15, imFlags, iFlag);
end;

procedure TfDEFlags.imFlag16Click(Sender: TObject);

begin
  FlagSelect(16, imFlags, iFlag);
end;

end.

