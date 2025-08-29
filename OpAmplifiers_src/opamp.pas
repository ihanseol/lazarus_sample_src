{******************************************}
{* Main unit for OpAmplifiers application *}
{******************************************}

unit opamp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, opamp1, opamp2, opamp3, opamp4, opamp5, opamp6, ophelp;

type
  {*********}
  { TfOpAmp }
  {*********}
  TfOpAmp = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mHelp, mHelpElectronics, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1, StaticText2, StaticText3: TStaticText;
    Memo1: TMemo;
    btInv: TButton;
    btAmp1: TButton;
    btAdd: TButton;
    btDiff: TButton;
    btComp: TButton;
    btMult: TButton;
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpElectronicsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btInvClick(Sender: TObject);
    procedure btAmp1Click(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btDiffClick(Sender: TObject);
    procedure btCompClick(Sender: TObject);
    procedure btMultClick(Sender: TObject);
  end;

const
  // LM741 data
  VCC = 15; VMin = 70E-6;
  Aol = 200000;

var
  fOpAmp: TfOpAmp;

implementation

{$R *.lfm}

{*********}
{ TfOpAmp }
{*********}

{ Menuitem "File > Exit": Exit application }

procedure TfOpAmp.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menuitem "Help > Electronics help": Display op-amp help text }

procedure TfOpAmp.mHelpElectronicsClick(Sender: TObject);

begin
  MessageDlg('Op-amp help text', 'Sorry, help text not yet available. Please, have a look at an electronics book or website...', mtInformation, [mbOK], 0);
end;

{ Menuitem "Help > Application help": Display program help text }

procedure TfOpAmp.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menuitem "Help > About": Display program about }

procedure TfOpAmp.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronics: Operational amplifiers.' + LineEnding;
  S += 'Study of common linear and non-linear op-amp application circuits.' + LineEnding + LineEnding;
  S += 'Version 1.0.1, © allu, September 2019 - September 2021.';
  MessageDlg('About "OpAmplifiers"', S, mtInformation, [mbOK], 0);
end;

{ Button "Inverting amplifier": Open window for op-amp inverting amplifier application }

procedure TfOpAmp.btInvClick(Sender: TObject);

begin
  fOpAmp2.rVCC := VCC;
  fOpAmp2.ShowModal;
end;

{ Button "Non-inverting amplifier": Open window for op-amp non-inverting amplifier application }

procedure TfOpAmp.btAmp1Click(Sender: TObject);

begin
  fOpAmp3.rVCC := VCC;
  fOpAmp3.ShowModal;
end;

{ Button "Adder circuits": Open window for op-amp 2 or 3 inputs adder circuit application }

procedure TfOpAmp.btAddClick(Sender: TObject);

begin
  fOpAmp4.rVCC := VCC;
  fOpAmp4.ShowModal;
end;

{ Button "Differential amplifier": Open window for op-amp differential amplifier application }

procedure TfOpAmp.btDiffClick(Sender: TObject);

begin
  fOpAmp5.rVCC := VCC;
  fOpAmp5.ShowModal;
end;

{ Button "Comparator": Open window for op-amp comperator application }

procedure TfOpAmp.btCompClick(Sender: TObject);

begin
  fOpAmp1.rVCC := VCC;
  fOpAmp1.rVMin := VMin;
  fOpAmp1.iAol := Aol;
  fOpAmp1.ShowModal;
end;

{ Button "Astable multivibrator": Open window for op-amp astable multivibrator application }

procedure TfOpAmp.btMultClick(Sender: TObject);

begin
  fOpAmp6.rVCC := VCC;
  fOpAmp6.ShowModal;
end;

end.

