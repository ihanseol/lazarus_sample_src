{*****************************************************}
{* Emblem selection unit for Luxembourg1 application *}
{*****************************************************}

unit emblems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  {***********}
  { TfEmblems }
  {***********}
  TfEmblems = class(TForm)
    Label1: TLabel;
    imEmblem1, imEmblem2, imEmblem3, imEmblem4, imEmblem5, imEmblem6: TImage;
    imEmblem7, imEmblem8, imEmblem9, imEmblem10, imEmblem11, imEmblem12: TImage;
    btOK: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure imEmblem1Click(Sender: TObject);
    procedure imEmblem2Click(Sender: TObject);
    procedure imEmblem3Click(Sender: TObject);
    procedure imEmblem4Click(Sender: TObject);
    procedure imEmblem5Click(Sender: TObject);
    procedure imEmblem6Click(Sender: TObject);
    procedure imEmblem7Click(Sender: TObject);
    procedure imEmblem8Click(Sender: TObject);
    procedure imEmblem9Click(Sender: TObject);
    procedure imEmblem10Click(Sender: TObject);
    procedure imEmblem11Click(Sender: TObject);
    procedure imEmblem12Click(Sender: TObject);
    procedure btOKClick(Sender: TObject);
  private
    iEmblem: Integer;
    imEmblems: array[0..11] of TImage;
    aEmblems: array[0..11] of Integer;
  public
    sCanton: string;
    aCantons: array[0..11] of string;
  end;

var
  fEmblems: TfEmblems;

implementation

{$R *.lfm}

{ Emblem selection: Stretch or unstretch emblem clicked by user; return emblem index }

procedure EmblemSelect(NewEmblem: Integer; var Emblems: array of TImage; var Emblem: Integer);

begin
  if Emblem <> -1 then
    Emblems[Emblem].Stretch := False;                                              // reset emblem selected before to normal size
  Emblem := NewEmblem;                                                             // actual emblem (the one the user has clicked onto)
  Emblems[Emblem].Stretch := True;                                                 // enlarge actual emblem (to show, that it is selected)
end;

{***********}
{ TfEmblems }
{***********}

{ Application start: Create array with emblem images }

procedure TfEmblems.FormCreate(Sender: TObject);

begin
  imEmblems[0] := imEmblem1;  imEmblems[1]  := imEmblem2;  imEmblems[2] := imEmblem3;
  imEmblems[3] := imEmblem4;  imEmblems[4]  := imEmblem5;  imEmblems[5] := imEmblem6;
  imEmblems[6] := imEmblem7;  imEmblems[7]  := imEmblem8;  imEmblems[8] := imEmblem9;
  imEmblems[9] := imEmblem10; imEmblems[10] := imEmblem11; imEmblems[11] := imEmblem12;
end;

{ Window show-up: Distribute emblems randomly on the image grid }

procedure TfEmblems.FormActivate(Sender: TObject);

var
  I, J: Integer;
  Filepath: string;

begin
  // Reset all emblem indexes
  for I := 0 to 11 do
    aEmblems[I] := -1;
  // Randomly fill in the "grid" with the different emblems
  for I := 0 to 11 do begin                                                         // for each emblem
    repeat
      J := Random(12);                                                              // choose a random "grid" position...
    until aEmblems[J] = -1;                                                         // ...that must be empty, of course
    aEmblems[J] := I;                                                               // save emblem index into "grid array"
    Filepath := './Wopen/' + aCantons[I] + '.jpg'; DoDirSeparators(Filepath);
    imEmblems[J].Picture.LoadFromFile(Filepath);                                    // display the emblem picture (from file)
    imEmblems[J].Stretch := False;                                                  // all emblems with normal (small) size
  end;
  iEmblem := -1;                                                                    // no emblem actually selected
  sCanton := '';                                                                    // that is: no user answer (corresponding canton) yet
end;

{ Button "OK": Save canton corr. to emblem selected (for retrieval by main form) and close the window }

procedure TfEmblems.btOKClick(Sender: TObject);

begin
  if iEmblem >= 0 then                                                              // if user actually has selected an emblem
    sCanton := aCantons[aEmblems[iEmblem]];                                         // name of canton corr. to emblem selected (= user answer to quiz question)
  Close;
end;

{ User click on emblem images: Select this emblem as answer to quiz question }

procedure TfEmblems.imEmblem1Click(Sender: TObject);

begin
  EmblemSelect(0, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem2Click(Sender: TObject);

begin
  EmblemSelect(1, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem3Click(Sender: TObject);

begin
  EmblemSelect(2, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem4Click(Sender: TObject);

begin
  EmblemSelect(3, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem5Click(Sender: TObject);

begin
  EmblemSelect(4, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem6Click(Sender: TObject);

begin
  EmblemSelect(5, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem7Click(Sender: TObject);

begin
  EmblemSelect(6, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem8Click(Sender: TObject);

begin
  EmblemSelect(7, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem9Click(Sender: TObject);

begin
  EmblemSelect(8, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem10Click(Sender: TObject);

begin
  EmblemSelect(9, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem11Click(Sender: TObject);

begin
  EmblemSelect(10, imEmblems, iEmblem);
end;

procedure TfEmblems.imEmblem12Click(Sender: TObject);

begin
  EmblemSelect(11, imEmblems, iEmblem);
end;

end.

