{**********************************************}
{* Main unit for TimerCircuits555 application *}
{**********************************************}

unit tc555;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, LCLIntf, bistable, monostable, astable, common;

type
  {*********}
  { TfTC555 }
  {*********}
  TfTC555 = class(TForm)
    mMenu: TMainMenu;
    mCircuit, mCircuitExit: TMenuItem;
    mSettings, mSettingsNoWarnings, MenuItem1, MenuItem2, MenuItem3: TMenuItem;
    mSettingsBistable, mSettingsBistableReset, mSettingsBistableThreshold: TMenuItem;
    mSettingsMonostable, mSettingsMonostableNoReset, mSettingsMonostableReset, mSettingsMonostableCalcT, mSettingsMonostableCalcR: TMenuItem;
    mSettingsAstable, mSettingsAstableStd, mSettingsAstableDiode, mSettingsAstableCalcT, mSettingsAstableCalcR: TMenuItem;
    mSettingsOutput, mSettingsOutputHigh, mSettingsOutputLow, mSettingsOutput2Leds: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3: TLabel;
    Memo1, Memo2, Memo3, Memo4: TMemo;
    btBistable: TButton;
    btMonostable: TButton;
    btAstable: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mCircuitExitClick(Sender: TObject);
    procedure mSettingsBistableResetClick(Sender: TObject);
    procedure mSettingsBistableThresholdClick(Sender: TObject);
    procedure mSettingsMonostableNoResetClick(Sender: TObject);
    procedure mSettingsMonostableResetClick(Sender: TObject);
    procedure mSettingsMonostableCalcTClick(Sender: TObject);
    procedure mSettingsMonostableCalcRClick(Sender: TObject);
    procedure mSettingsAstableStdClick(Sender: TObject);
    procedure mSettingsAstableDiodeClick(Sender: TObject);
    procedure mSettingsAstableCalcRClick(Sender: TObject);
    procedure mSettingsAstableCalcTClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mSettingsOutputHighClick(Sender: TObject);
    procedure mSettingsOutputLowClick(Sender: TObject);
    procedure mSettingsOutput2LedsClick(Sender: TObject);
    procedure mSettingsNoWarningsClick(Sender: TObject);
    procedure btBistableClick(Sender: TObject);
    procedure btMonostableClick(Sender: TObject);
    procedure btAstableClick(Sender: TObject);
  private
    aStdResistors: TStdResistors;
  end;

var
  fTC555: TfTC555;

implementation

{$R *.lfm}

{ Create array with standard resistor values (from file)}

procedure ReadStdResistors(out Resistances: TStdResistors);

var
  I, J, K, N: Integer;
  Mult: Real;
  SResistance: string;
  StdValues: array[1..24] of Integer;
  ResistanceFile: Text;

begin
  // Read standard values (10Ω - 91Ω) from file
  Assign(ResistanceFile, 'resistors.txt'); Reset(ResistanceFile);
  N := 0;
  while not EoF(ResistanceFile) do begin
    Readln(ResistanceFile, SResistance);
    if SResistance <> '' then begin
      Inc(N);
      StdValues[N] := StrToInt(SResistance);
    end;
  end;
  // Create array of all staandard resistances by multiplying the file values by 0.1, 1, 10 ... 1e+5
  K := 0;
  for I := 1 to 7 do begin
    if I = 1 then
      Mult := 0.1
    else
      Mult *= 10;
    for J := 1 to N do begin
      if Mult * StdValues[J] >= 2 then begin
        Inc(K);
        Resistances[K] := Mult * StdValues[J];
      end;
    end;
  end;
  Resistances[K + 1] := 10e+6;                                                 // add 10MΩ resistance
  Close(ResistanceFile);
end;

{*********}
{ TfTC555 }
{*********}

{ Application start: Read standard resistor values from file }

procedure TfTC555.FormCreate(Sender: TObject);

begin
  ReadStdResistors(aStdResistors);
end;

{ Menu item "Circuit > Exit": Exit application }

procedure TfTC555.mCircuitExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Bistable circuits > ...": Bistable circuits options selection }

procedure TfTC555.mSettingsBistableResetClick(Sender: TObject);

begin
  mSettingsBistableReset.Checked := True; mSettingsBistableThreshold.Checked := False;
  fBistable.sResetInput := 'reset';
end;

procedure TfTC555.mSettingsBistableThresholdClick(Sender: TObject);

begin
  mSettingsBistableReset.Checked := False; mSettingsBistableThreshold.Checked := True;
  fBistable.sResetInput := 'threshold';
end;

{ Menu items "Settings > Monostable circuits > ...": Monostable circuits options selection }

procedure TfTC555.mSettingsMonostableNoResetClick(Sender: TObject);

begin
  mSettingsMonostableNoReset.Checked := True; mSettingsMonostableReset.Checked := False;
  fMonostable.bResetUsed := False;
end;

procedure TfTC555.mSettingsMonostableResetClick(Sender: TObject);

begin
  mSettingsMonostableNoReset.Checked := False; mSettingsMonostableReset.Checked := True;
  fMonostable.bResetUsed := True;
end;

procedure TfTC555.mSettingsMonostableCalcTClick(Sender: TObject);

begin
  mSettingsMonostableCalcT.Checked := True; mSettingsMonostableCalcR.Checked := False;
  fMonostable.sCalc := 'time period';
end;

procedure TfTC555.mSettingsMonostableCalcRClick(Sender: TObject);

begin
  mSettingsMonostableCalcT.Checked := False; mSettingsMonostableCalcR.Checked := True;
  fMonostable.sCalc := 'resistance';
end;

{ Menu items "Settings > Astable circuits > ...": Astable circuits options selection }

procedure TfTC555.mSettingsAstableStdClick(Sender: TObject);

begin
  mSettingsAstableStd.Checked := True; mSettingsAstableDiode.Checked := False;
  fAstable.sCircuit := 'standard';
end;

procedure TfTC555.mSettingsAstableDiodeClick(Sender: TObject);

begin
  mSettingsAstableStd.Checked := False; mSettingsAstableDiode.Checked := True;
  fAstable.sCircuit := 'diode';
end;

procedure TfTC555.mSettingsAstableCalcRClick(Sender: TObject);

begin
  mSettingsAstableCalcR.Checked := True; mSettingsAstableCalcR.Checked := False;
  fAstable.sCalc := 'resistance';
end;

procedure TfTC555.mSettingsAstableCalcTClick(Sender: TObject);

begin
  mSettingsAstableCalcR.Checked := False; mSettingsAstableCalcR.Checked := True;
  fAstable.sCalc := 'time period';
end;

{ Menu items "Settings > Output LEDs > ...": Output LEDs options selection }

procedure TfTC555.mSettingsOutputHighClick(Sender: TObject);

begin
  mSettingsOutputHigh.Checked := True; mSettingsOutputLow.Checked := False; mSettingsOutput2Leds.Checked := False;
  fBistable.bLed1Used := False; fBistable.bLed2Used := True;
  fMonostable.bLed1Used := False; fMonostable.bLed2Used := True;
  fAstable.bLed1Used := False; fAstable.bLed2Used := True;
end;

procedure TfTC555.mSettingsOutputLowClick(Sender: TObject);

begin
  mSettingsOutputHigh.Checked := False; mSettingsOutputLow.Checked := True; mSettingsOutput2Leds.Checked := False;
  fBistable.bLed1Used := True; fBistable.bLed2Used := False;
  fMonostable.bLed1Used := True; fMonostable.bLed2Used := False;
  fAstable.bLed1Used := True; fAstable.bLed2Used := False;
end;

procedure TfTC555.mSettingsOutput2LedsClick(Sender: TObject);

begin
  mSettingsOutputHigh.Checked := False; mSettingsOutputLow.Checked := False; mSettingsOutput2Leds.Checked := True;
  fBistable.bLed1Used := True; fBistable.bLed2Used := True;
  fMonostable.bLed1Used := True; fMonostable.bLed2Used := True;
  fAstable.bLed1Used := True; fAstable.bLed2Used := True;
end;

{ Menu item "Settings > Don't display warnings": Toggle warnings display on/off }

procedure TfTC555.mSettingsNoWarningsClick(Sender: TObject);

begin
  if mSettingsNoWarnings.Checked then
    mSettingsNoWarnings.Checked := False
  else
    mSettingsNoWarnings.Checked := True;
  fMonostable.bWarnings := not mSettingsNoWarnings.Checked;
  fAstable.bWarnings := not mSettingsNoWarnings.Checked;
end;

{ Menu item "Help > Help": Start web browser and display application help }

procedure TfTC555.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Help > About": Display application about }

procedure TfTC555.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronics - 555 timer IC:' + LineEnding;
  S += 'Bistable, monostable and astable circuits.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, July-December 2021.';
  MessageDlg('About "TimerCircuits555"', S, mtInformation, [mbOK], 0);
end;

{ Button "Bistable" pushed: Run bistable circuit simulation }

procedure TfTC555.btBistableClick(Sender: TObject);

begin
  fBistable.Show;
end;

{ Button "Monostable" pushed: Run monostable circuit simulation }

procedure TfTC555.btMonostableClick(Sender: TObject);

begin
  fMonostable.pStdResistors := @aStdResistors;
  fMonostable.Show;
end;

{ Button "Astable" pushed: Run astable circuit simulation }

procedure TfTC555.btAstableClick(Sender: TObject);

begin
  fAstable.pStdResistors := @aStdResistors;
  fAstable.Show;
end;

end.

