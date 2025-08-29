unit printing7_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Printers;

type
  { TfPrinting7 }
  {=============}
  TfPrinting7 = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    edFile: TEdit;
    imPic: TImage;
    btBrowse: TButton;
    btPrint: TButton;
    btExit: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btPrintClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  private
    sDir: string;
  end;

var
  fPrinting7: TfPrinting7;

implementation

{$R *.lfm}

{ TfPrinting7 }
{=============}

{ Application start }

procedure TfPrinting7.FormCreate(Sender: TObject);

begin
  sDir := GetCurrentDir;
end;

{ Button "Browse" pushed: User selection of the image to be printed }

procedure TfPrinting7.btBrowseClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpen.InitialDir := sDir;
  dlgOpen.FileName := '';
  if dlgOpen.Execute then begin
    // User has selected a file
    Filename := dlgOpen.FileName;
    edFile.Text := FileName;
    sDir := ExtractFileDir(Filename);
    imPic.Picture.LoadFromFile(Filename);
  end;
end;

{ Button "Print" pushed: Print image to default printer }

procedure TfPrinting7.btPrintClick(Sender: TObject);

var
  WPic, HPic, XPos, YPos: Integer;

begin
  WPic := imPic.Picture.Graphic.Width;
  HPic := imPic.Picture.Graphic.Height;
  XPos := (Printer.PaperSize.Width - WPic) div 2;
  YPos := (Printer.PaperSize.Height - HPic) div 2;
  try
    Printer.BeginDoc;
    Printer.Canvas.Draw(XPos, YPos, imPic.Picture.Graphic);
  finally
    Printer.EndDoc;
    MessageDlg('Print', 'Image has successfully been printed!', mtInformation, [mbOK], 0);
  end;
end;

{ Button "Exit" pushed: Exit the application }

procedure TfPrinting7.btExitClick(Sender: TObject);

begin
  Close;
end;

end.

