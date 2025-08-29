{*******************************************}
{* Zodiac signs unit of Zodiac application *}
{*******************************************}

unit zodiac_signs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, IniFiles;

type
  {*********}
  { TfSigns }
  {*********}
  TfSigns = class(TForm)
    StaticText1: TStaticText;
    sgZodiac: TStringGrid;
    Label1: TLabel;
    edDetails: TMemo;
    cobElements: TComboBox;
    cobPlanets: TComboBox;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure cobElementsChange(Sender: TObject);
    procedure cobPlanetsChange(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

var
  fSigns: TfSigns;
  DataFile: TINIFile;

implementation

{$R *.lfm}

{*********}
{ TfSigns }
{*********}

{ Window show-up (form activation): Initialisation }

procedure TfSigns.FormActivate(Sender: TObject);

var
  Filename: string;

begin
  // Open data file (as Free Pascal .INI file)
  Filename := './res/zodiac2.dat'; DoDirSeparators(Filename);
  DataFile.Free; DataFile := TINIFile.Create(Filename);
  // Clear form controls
  edDetails.Clear;
  cobElements.ItemIndex := 0; cobPlanets.ItemIndex := 0;
end;

{ User selection of an element: Display corr. details (read from data file) }

procedure TfSigns.cobElementsChange(Sender: TObject);

begin
  if cobElements.ItemIndex >= 1 then begin
    edDetails.Clear;
    edDetails.Lines.AddText(DataFile.ReadString('Elemente', cobElements.Items[cobElements.ItemIndex], ''));
    cobPlanets.ItemIndex := 0;
  end;
end;

{ User selection of a planet: Display corr. details (read from data file) }

procedure TfSigns.cobPlanetsChange(Sender: TObject);

begin
  if cobPlanets.ItemIndex >= 1 then begin
    edDetails.Clear;
    edDetails.Lines.AddText(DataFile.ReadString('Planeten', cobPlanets.Items[cobPlanets.ItemIndex], ''));
    cobElements.ItemIndex := 0;
  end;
end;

{ Button "Close": Close zodiac signs window }

procedure TfSigns.btCloseClick(Sender: TObject);

begin
  DataFile.Free;
  Close;
end;

end.

