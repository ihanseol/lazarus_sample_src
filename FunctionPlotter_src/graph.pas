{**********************************************}
{* Graph unit for FunctionPlotter application *}
{**********************************************}

unit graph;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Process;

type
  {*********}
  { TfGraph }
  {*********}
  TfGraph = class(TForm)
    imDraw: TImage;
    btSave: TButton;
    btClose: TButton;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    sPNGDir: string;
  public
    sPLTDir: string;
    bGraphOK: Boolean;
  end;

var
  fGraph: TfGraph;

implementation

{$R *.lfm}

{*********}
{ TfGraph }
{*********}

{ Application start: Initialization }

procedure TfGraph.FormCreate(Sender: TObject);

begin
  sPNGDir := GetCurrentDir;                                                    // directory to save PNG files
  bGraphOK := False;
end;

{ Graph window showing up: Loading the graph from generated PNG file }

procedure TfGraph.FormActivate(Sender: TObject);

begin
  btSave.Enabled := False;
  imDraw.Picture.Clear;
  if bGraphOK then begin                                                       // bGraphOK is set by the main unit
    imDraw.Picture.LoadFromFile('fplotter.png');                               // display the graph
    if sPNGDir = GetCurrentDir then                                            // if no directory to save the PNG has been specified...
      sPNGDir := sPLTDir;                                                      // ...use the one where the last PLT file has been saved
    btSave.Enabled := True;                                                    // enable "Save" button only, if the graph has been generated
  end;
end;

{ Buttton "Save" pushed: Save the graph }

procedure TfGraph.btSaveClick(Sender: TObject);

var
  Ret: Cardinal;
  Filename, S: string;
  DoOverride, OK: Boolean;

begin
  dlgSave.InitialDir := sPNGDir;
  dlgSave.FileName := '';
  if dlgSave.Execute then begin
    // User has selected a file
    Filename := dlgSave.FileName;
    sPNGDir := ExtractFileDir(Filename);                                       // save directory (to open next time at same location)
    // Override existing file only, if user wants so
    DoOverride := True;
    if FileExists(Filename) then begin
      Ret := MessageDlg('Save graph', 'File already exists. Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
      if Ret = mrNo then
        DoOverride := False;
    end;
    if DoOverride then begin
      // Save graph to file (copy the generated PNG file)
      OK := RunCommand('copy.bat', ['fplotter.png', Filename], S);
      if OK then
        MessageDlg('Save graph', 'Graph saved to ' + Filename + '.', mtInformation, [mbOK], 0)
      else
        MessageDlg('Save graph', 'File error: Graph could not be saved!' + Filename, mtError, [mbOK], 0);
    end;
  end;
end;

{ Button "Close" pushed: Close the graph window }

procedure TfGraph.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

