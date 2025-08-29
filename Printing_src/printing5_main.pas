unit printing5_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Printers;

type
  TLines = array of AnsiString;
  { TfPrinting5 }
  {=============}
  TfPrinting5 = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    edFile: TEdit;
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
    aLines: TLines;
  end;

var
  fPrinting5: TfPrinting5;

implementation

{$R *.lfm}

{ Read text file selected by user }

procedure ReadFile(FileName: string; out Lines: TLines);

var
  N: Integer;
  InFile: Text;

begin
  Assign(InFile, FileName); Reset(InFile);
  N := 0;
  while not EoF(InFile) do begin
    Inc(N);
    SetLength(Lines, N);
    Readln(InFile, Lines[N - 1]);
  end;
  Close(InFile);
end;

{ Advance printhead 1 line; new line if bottom of page reached }

procedure AdvanceLine(Printer: TPrinter; var Y: Integer; VM, LH: Integer);

begin
  Y += LH;
  if Y >= Printer.PaperSize.Height - VM - LH then begin
    Printer.NewPage;
    Y := VM;
  end;
end;

{ TfPrinting5 }
{=============}

{ Application start }

procedure TfPrinting5.FormCreate(Sender: TObject);

begin
  sDir := GetCurrentDir;
end;

{ Button "Browse" pushed: User selection of the file to be printed }

procedure TfPrinting5.btBrowseClick(Sender: TObject);

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
    ReadFile(FileName, aLines);
  end;
end;

{ Button "Print" pushed: Print file content to default printer }

procedure TfPrinting5.btPrintClick(Sender: TObject);

const
  HMargin = 100;
  VMargin = 100;

var
  LineHeight, XPos, YPos, I, J, J0: Integer;
  Line, Line0: AnsiString;

begin
  try
    Printer.BeginDoc;
    Printer.Canvas.Font.Name  := 'Courier New';
    Printer.Canvas.Font.Size  := 10;
    Printer.Canvas.Font.Color := $000000;
    LineHeight := Round(1.2 * Abs(Printer.Canvas.TextHeight('I')));
    YPos := VMargin;
    XPos := HMargin;
    for I := 0 to Length(aLines) - 1 do begin
      // For each text line of the file
      Line := aLines[I];
      if Printer.Canvas.TextWidth(Line) <= Printer.PaperSize.Width - 2 * HMargin then begin
        // If the line fits into the page width, print it
        Printer.Canvas.TextOut(XPos, YPos, Line);
        AdvanceLine(Printer, YPos, VMargin, LineHeight);
      end
      else begin
        // If the line doesn't fit into the page width, print it over several lines
        repeat
          Line0 := '';
          J := 1; J0 := -1;
          while (J < Length(Line)) and (Printer.Canvas.TextWidth(Line0) <= Printer.PaperSize.Width - 2 * HMargin) do begin
            if Line[J] = ' ' then
              J0 := J;
            Line0 += Line[J];
            Inc(J);
          end;
          if J = Length(Line) then begin
            // Print rest of the line (now fitting on the page)
            Printer.Canvas.TextOut(XPos, YPos, Line);
            AdvanceLine(Printer, YPos, VMargin, LineHeight);
            Line := '';
          end
          else begin
            // Print the actual part of the line
            Printer.Canvas.TextOut(XPos, YPos, Copy(Line, 1, J0 - 1));
            AdvanceLine(Printer, YPos, VMargin, LineHeight);
            Delete(Line, 1, J0);
          end;
        until Line = '';
      end;
    end;
  finally
    Printer.EndDoc;
    MessageDlg('Print', 'File has successfully been printed!', mtInformation, [mbOK], 0);
  end;
end;

{ Button "Exit" pushed: Exit the application }

procedure TfPrinting5.btExitClick(Sender: TObject);

begin
  Close;
end;

end.

