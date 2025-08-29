{*****************************************}
{* Main unit for Display7seg application *}
{*****************************************}

unit d7seg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls,
  d7segc, d7sega, d7segbcdc, d7segbcda, d7segcount, help;

type
  {*********}
  { TfD7Seg }
  {*********}
  TfD7Seg = class(TForm)
    mMenu: TMainMenu;
    mSimul, mSimulCathode, mSimulAnode, mSimulBCDCathode, mSimulBCDAnode, mSimulCounter, mSimulExit: TMenuItem;
    mHelp, mHelpElectro, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2: TLabel;
    Memo1: TMemo;
    Image1, Image2: TImage;
    procedure mSimulCathodeClick(Sender: TObject);
    procedure mSimulAnodeClick(Sender: TObject);
    procedure mSimulBCDCathodeClick(Sender: TObject);
    procedure mSimulBCDAnodeClick(Sender: TObject);
    procedure mSimulCounterClick(Sender: TObject);
    procedure mSimulExitClick(Sender: TObject);
    procedure mHelpElectroClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
  end;

var
  fD7Seg: TfD7Seg;

implementation

{$R *.lfm}

{*********}
{ TfD7Seg }
{*********}

{ Menu item "Simulation > Common cathode display": Open the window (form) with the CC 7-segment display simulation }

procedure TfD7Seg.mSimulCathodeClick(Sender: TObject);

begin
  fD7Segc.ShowModal;
end;

{ Menu item "Simulation > Common anode display": Open the window (form) with the CA 7-segment display simulation }

procedure TfD7Seg.mSimulAnodeClick(Sender: TObject);

begin
  fD7Sega.ShowModal;
end;

{ Menu item "Simulation > Common cathode BCD decoder": Open the window (form) with the CC 7-segment display with BCD decoder simulation }

procedure TfD7Seg.mSimulBCDCathodeClick(Sender: TObject);

begin
  fD7SegBCDc.ShowModal;
end;

{ Menu item "Simulation > Common anode BCD decoder": Open the window (form) with the CA 7-segment display with BCD decoder simulation }

procedure TfD7Seg.mSimulBCDAnodeClick(Sender: TObject);

begin
  fD7SegBCDa.ShowModal;
end;

{ Menu item "Simulation > 7-segment display counter": Open the window (form) with the 7-segment display counter simulation }

procedure TfD7Seg.mSimulCounterClick(Sender: TObject);

begin
  fD7SegCounter.ShowModal;
end;

{ Menu item "Simulation > Exit": Exit application }

procedure TfD7Seg.mSimulExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Electronics help": Display electronics help text }

procedure TfD7Seg.mHelpElectroClick(Sender: TObject);

var
  S: string;

begin
  S := 'Sorry, but the electronics help text isn''t yet available... For details concerning 7-segment displays, ';
  S += 'BCD decoders and 7-segment counters, please, have a look at an electronics book, or search the Internet.';
  MessageDlg('"Display7seg" electronics help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > Application help": Display application help text }

procedure TfD7Seg.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfD7Seg.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronic circuits: 7-segment displays.' + LineEnding;
  S += 'Simulation of common cathode and common anode displays, ';
  S += 'common cathode and common anode BCD decoders, and 7-segment display counters.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June-July 2023.';
  MessageDlg('About "Display7seg"', S, mtInformation, [mbOK], 0);
end;

end.

