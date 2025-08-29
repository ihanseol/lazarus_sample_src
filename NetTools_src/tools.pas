{********************************************}
{* Tools path unit for NetTools application *}
{********************************************}

unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

const
  NTools = 11;

type
  TTool = record
    Name, Path: string;
  end;
  TTools = array[0 .. NTools - 1] of TTool;
  {*********}
  { TfTools }
  {*********}
  TfTools = class(TForm)
    sgTools: TStringGrid;
    btClose: TButton;
    btBrowse0, btBrowse1, btBrowse2, btBrowse3, btBrowse4, btBrowse5: TButton;
    btBrowse6, btBrowse7, btBrowse8, btBrowse9, btBrowse10: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btBrowse0Click(Sender: TObject);
    procedure btBrowse10Click(Sender: TObject);
    procedure btBrowse1Click(Sender: TObject);
    procedure btBrowse2Click(Sender: TObject);
    procedure btBrowse3Click(Sender: TObject);
    procedure btBrowse4Click(Sender: TObject);
    procedure btBrowse5Click(Sender: TObject);
    procedure btBrowse6Click(Sender: TObject);
    procedure btBrowse7Click(Sender: TObject);
    procedure btBrowse8Click(Sender: TObject);
    procedure btBrowse9Click(Sender: TObject);
  private
     btBrowse: array[0 .. NTools - 1] of TButton;
  public
    aTools: TTools;
  end;

var
  fTools: TfTools;

implementation

{$R *.lfm}

{ Select tool program by browsing to the executable }

procedure SelectTool(var Tools: TTools; Tool: Integer);

begin
  if fTools.dlgOpen.Execute then begin
    // If the user actually selected a file
    Tools[Tool].Path := fTools.dlgOpen.FileName;
    fTools.sgTools.Cells[1, Tool + 1] := Tools[Tool].Path;
  end;
end;

{*********}
{ TfTools }
{*********}

{ Application start: Create array with the "Browse" buttons }

procedure TfTools.FormCreate(Sender: TObject);

begin
  btBrowse[0] := btBrowse0; btBrowse[1] := btBrowse1; btBrowse[2] := btBrowse2; btBrowse[3] := btBrowse3;
  btBrowse[4] := btBrowse4; btBrowse[5] := btBrowse5; btBrowse[6] := btBrowse6; btBrowse[7] := btBrowse7;
  btBrowse[8] := btBrowse8; btBrowse[9] := btBrowse9; btBrowse[10] := btBrowse10;
end;

{ Window show-up: Fill-in the tools list }

procedure TfTools.FormActivate(Sender: TObject);

var
  I: Integer;

begin
  for I := NTools - 1 downto 0 do begin
    sgTools.Cells[0, I + 1] := aTools[I].Name;
    sgTools.Cells[1, I + 1] := aTools[I].Path;
    if sgTools.Cells[1, I + 1] = 'NOT AVAILABLE' then
      btBrowse[I].SetFocus;                                                    // focus the button of first unavailable tool
  end;
end;

{ Button "Close": Close tools path window }

procedure TfTools.btCloseClick(Sender: TObject);

begin
  Close;
end;

{ Buttons "Browse": Open FileOpen dialog to select the program for a given tool }

procedure TfTools.btBrowse0Click(Sender: TObject);

begin
  SelectTool(aTools, 0);
end;

procedure TfTools.btBrowse1Click(Sender: TObject);

begin
  SelectTool(aTools, 1);
end;

procedure TfTools.btBrowse2Click(Sender: TObject);

begin
  SelectTool(aTools, 2);
end;

procedure TfTools.btBrowse3Click(Sender: TObject);

begin
  SelectTool(aTools, 3);
end;

procedure TfTools.btBrowse4Click(Sender: TObject);

begin
  SelectTool(aTools, 4);
end;

procedure TfTools.btBrowse5Click(Sender: TObject);

begin
  SelectTool(aTools, 5);
end;

procedure TfTools.btBrowse6Click(Sender: TObject);

begin
  SelectTool(aTools, 6);
end;

procedure TfTools.btBrowse7Click(Sender: TObject);

begin
  SelectTool(aTools, 7);
end;

procedure TfTools.btBrowse8Click(Sender: TObject);

begin
  SelectTool(aTools, 8);
end;

procedure TfTools.btBrowse9Click(Sender: TObject);

begin
  SelectTool(aTools, 9);
end;

procedure TfTools.btBrowse10Click(Sender: TObject);

begin
  SelectTool(aTools, 10);
end;

end.

