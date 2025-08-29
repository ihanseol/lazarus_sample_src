{************************************************}
{* Console window unit for NetTools application *}
{************************************************}

unit cmd;

// Most code of what happens in this window (console output of tools) is part of the main unit

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Dos;

type
  {*******}
  { TfCmd }
  {*******}
  TfCmd = class(TForm)
    memoCmd: TMemo;
    btClose: TButton;
    btSave: TButton;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    sDir: string;
  end;

var
  fCmd: TfCmd;

implementation

{$R *.lfm}

{*******}
{ TfCmd }
{*******}

{ Application start: Initialisation }

procedure TfCmd.FormCreate(Sender: TObject);

begin
  sDir := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH') + '\' + 'Documents';        // Default directory to save text files = user's Document folder
end;

{ Button "Save": Save console output (= memo content) to text file }

procedure TfCmd.btSaveClick(Sender: TObject);

var
  Filename: string;

begin
  dlgSave.InitialDir := sDir; dlgSave.FileName := '';
  if dlgSave.Execute then begin
    // If the user entered a filename
    Filename := dlgSave.FileName;                                              // get this filename
    memoCmd.Lines.SaveToFile(Filename);                                        // save memo content (with this filename)
    sDir := dlgSave.InitialDir;                                                // remember last used directory
  end;
end;

{ Button "Close": Close console window }

procedure TfCmd.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

