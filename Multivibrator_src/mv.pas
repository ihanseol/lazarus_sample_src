{********************************************}
{* Main unit for Multivibrators application *}
{********************************************}

unit mv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, PopupNotifier, bistable, monostable, astable;

type
  { TfMV }
  TfMV = class(TForm)
    mMenu: TMainMenu;
    mMultivibrator, mMultivibratorBistabil, mMultivibratorMonostabil, mMultivibratorAstabil, mMultivibratorExit: TMenuItem;
    mTime: TMenuItem;
    mTimeReal, mTimeSlower, mTimeFaster: TMenuItem;
    mTimeSlower1, mTimeSlower2, mTimeSlower3, mTimeSlower4, mTimeSlower5, mTimeSlower6: TMenuItem;
    mTimeFaster1, mTimeFaster2, mTimeFaster3, mTimeFaster4: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    meText1: TMemo;
    meText2: TMemo;
    btShow: TButton;
    pnHelp: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mMultivibratorBistabilClick(Sender: TObject);
    procedure mMultivibratorMonostabilClick(Sender: TObject);
    procedure mMultivibratorAstabilClick(Sender: TObject);
    procedure mMultivibratorExitClick(Sender: TObject);
    procedure mTimeRealClick(Sender: TObject);
    procedure mTimeSlower1Click(Sender: TObject);
    procedure mTimeSlower2Click(Sender: TObject);
    procedure mTimeSlower3Click(Sender: TObject);
    procedure mTimeSlower4Click(Sender: TObject);
    procedure mTimeSlower5Click(Sender: TObject);
    procedure mTimeSlower6Click(Sender: TObject);
    procedure mTimeFaster1Click(Sender: TObject);
    procedure mTimeFaster2Click(Sender: TObject);
    procedure mTimeFaster3Click(Sender: TObject);
    procedure mTimeFaster4Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
  private
    rMult: Double;
    sMvib: string;
  end;

var
  fMV: TfMV;

implementation

{$R *.lfm}

{ Set simulation time multiplicator }

procedure SimulationTime(T0: Integer; T1S, T1F: array of Boolean; out M: Double);

// For small RC time constants, you have to choose a slower simulation time to make the simulation work
// For very large time constants, you may want to choose a faster simulation time to speed up the simulation

begin
  if T0 = 3 then begin
    // Check the "Schneller" menu item
    fMV.mTimeReal.Checked := False;
    fMV.mTimeSlower.Checked := False;
    fMV.mTimeFaster.Checked := True;
  end
  else if T0 = 2 then begin
    // Check the "Langsamer" menu item
    fMV.mTimeReal.Checked := False;
    fMV.mTimeSlower.Checked := True;
    fMV.mTimeFaster.Checked := False;
  end
  else begin
    // Check the "Realzeit" menu item
    fMV.mTimeReal.Checked := True;
    fMV.mTimeSlower.Checked := False;
    fMV.mTimeFaster.Checked := False;
  end;
  // Check the corect "Langsamer" submenu item
  fMV.mTimeSlower1.Checked := T1S[0];
  fMV.mTimeSlower2.Checked := T1S[1];
  fMV.mTimeSlower3.Checked := T1S[2];
  fMV.mTimeSlower4.Checked := T1S[3];
  fMV.mTimeSlower5.Checked := T1S[4];
  fMV.mTimeSlower6.Checked := T1S[5];
  // Check the corect "Schneller" submenu item
  fMV.mTimeFaster1.Checked := T1F[0];
  fMV.mTimeFaster2.Checked := T1F[1];
  fMV.mTimeFaster3.Checked := T1F[2];
  fMV.mTimeFaster4.Checked := T1F[3];
  // Set the simulation time multiplicator depending on menu item checked
  if fMV.mTimeReal.Checked then
    M := 1
  else if fMV.mTimeSlower1.Checked then
    M := 10
  else if fMV.mTimeSlower2.Checked then
    M := 100
  else if fMV.mTimeSlower3.Checked then
    M := 1000
  else if fMV.mTimeSlower4.Checked then
    M := 1E+4
  else if fMV.mTimeSlower5.Checked then
    M := 1E+5
  else if fMV.mTimeSlower6.Checked then
    M := 1E+6
  else if fMV.mTimeFaster1.Checked then
    M := 1 / 10
  else if fMV.mTimeFaster2.Checked then
    M := 1 / 20
  else if fMV.mTimeFaster3.Checked then
    M := 1 / 50
  else if fMV.mTimeFaster4.Checked then
    M := 1 / 100;
end;

{********}
{* TfMV *}
{********}

{ Application start: Default values }

procedure TfMV.FormCreate(Sender: TObject);

begin
  sMvib := 'bistabil';
  rMult := 1;
end;

{ Menu item "Multivibrator > Bistabil": Display bistabil multivibrator description }

procedure TfMV.mMultivibratorBistabilClick(Sender: TObject);

var
  S: AnsiString;

begin
  S := 'Ein bistabiler Multivibrator (Flipflop) hat zwei stabile Zustände, zwischen denen er durch Triggersignale ';
  S += 'hin und her geschaltet werden kann. Ohne Triggersignal verharrt er in dem zuletzt erreichten Zustand. Das ';
  S += 'Schalten des hier beschriebenen bistabilen Multivibrators erfolgt durch 2 Taster, die jeweils einen der ';
  S += 'beiden Transistoren auf Masse legt und mit seinem Sperren, der andere durchsteuernd wird.';
  stTitle.Caption := 'Bistabiler Multivibrator.';
  meText2.Clear;
  meText2.Lines.AddText(S);
  sMvib := 'bistabil';
end;

{ Menu item "Multivibrator > Monostabil": Display monostabil multivibrator description }

procedure TfMV.mMultivibratorMonostabilClick(Sender: TObject);

var
  S: AnsiString;

begin
  S := 'Ein monostabiler Multivibrator (auch Univibrator, Monoflop, Zeitstufe genannt) hat einen stabilen und einen ';
  S += 'instabilen Zustand. Durch ein Triggersignal kann der monostabile Multivibrator vom stabilen in den instabilen ';
  S += 'Zustand versetzt werden und kehrt nach einer bestimmten Zeit in den stabilen Zustand zurück. Bei dem hier ';
  S += 'beschriebenen monostabilen Multivibrator erfolgt das Schalten mit einem Taster, der den mit dem RC-Glied ';
  S += 'verbundenen Transistor auf Masse legt und so mit seinem Sperren, der andere durchsteuernd wird.';
  stTitle.Caption := 'Monostabiler Multivibrator.';
  meText2.Clear;
  meText2.Lines.AddText(S);
  sMvib := 'monostabil';
end;

{ Menu item "Multivibrator > Astabil": Display astabil multivibrator description }

procedure TfMV.mMultivibratorAstabilClick(Sender: TObject);

var
  S: AnsiString;

begin
  S := 'Ein astabiler Multivibrator ist ein frei schwingender Rechteckgenerator beziehungsweise. Kippschwinger. ';
  S += 'Im Prinzip besteht er aus zwei wechselseitig gekoppelten elektronischen Schaltern, die sich gegenseitig ';
  S += 'umschalten. Nach einer frequenzbestimmenden Verzögerungszeit wird immer wieder automatisch ein erneutes ';
  S += 'gegenseitiges Umschalten ausgelöst, sodass ein periodisches Verhalten entsteht.';
  stTitle.Caption := 'Astabiler Multivibrator.';
  meText2.Clear;
  meText2.Lines.AddText(S);
  sMvib := 'astabil';
end;

{ Menu item "Multivibrator > Verlassen": Exit application }

procedure TfMV.mMultivibratorExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Simulationszeit > Realzeit": Choose real-time simulation time }

procedure TfMV.mTimeRealClick(Sender: TObject);

begin
  SimulationTime(1, [False, False, False, False, False, False], [False, False, False, False], rMult);
end;

{ Menu item "Simulationszeit > Langsamer > ...": Choose slower simulation times }

procedure TfMV.mTimeSlower1Click(Sender: TObject);

begin
  SimulationTime(2, [True, False, False, False, False, False], [False, False, False, False], rMult);
end;

procedure TfMV.mTimeSlower2Click(Sender: TObject);

begin
  SimulationTime(2, [False, True, False, False, False, False], [False, False, False, False], rMult);
end;

procedure TfMV.mTimeSlower3Click(Sender: TObject);

begin
  SimulationTime(2, [False, False, True, False, False, False], [False, False, False, False], rMult);
end;

procedure TfMV.mTimeSlower4Click(Sender: TObject);

begin
  SimulationTime(2, [False, False, False, True, False, False], [False, False, False, False], rMult);
end;

procedure TfMV.mTimeSlower5Click(Sender: TObject);

begin
  SimulationTime(2, [False, False, False, False, True, False], [False, False, False, False], rMult);
end;

procedure TfMV.mTimeSlower6Click(Sender: TObject);

begin
  SimulationTime(2, [False, False, False, False, False, True], [False, False, False, False], rMult);
end;

{ Menu item "Simulationszeit > Schneller > ...": Choose faster simulation times }

procedure TfMV.mTimeFaster1Click(Sender: TObject);

begin
  SimulationTime(3, [False, False, False, False, False, False], [True, False, False, False], rMult);
end;

procedure TfMV.mTimeFaster2Click(Sender: TObject);

begin
  SimulationTime(3, [False, False, False, False, False, False], [False, True, False, False], rMult);
end;

procedure TfMV.mTimeFaster3Click(Sender: TObject);

begin
  SimulationTime(3, [False, False, False, False, False, False], [False, False, True, False], rMult);
end;

procedure TfMV.mTimeFaster4Click(Sender: TObject);

begin
  SimulationTime(3, [False, False, False, False, False, False], [False, False, False, True], rMult);
end;

{ Menu item "Hilfe > Hilfe": Display (very short) program help text }

procedure TfMV.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Hide
  else begin
    S := 'Beim astabilen/monostabilen M. zuerst die RC-Werte eingeben und die Zeiten berechnen. Auf den ';
    S += 'Schalter S klicken, versorgt die Schaltung mit Strom. Auf die Taster Ta1/Ta2 klicken, betätigt ';
    S += 'diese (drücken und wieder loslassen) und kippt den Schaltungszustand.';
    pnHelp.Title := 'Multivibrator Hilfe.';
    pnHelp.Text := S;
    pnHelp.Show;
  end;
end;

{ Menu item "Hilfe > Über": Display program about text }

procedure TfMV.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Hide
  else begin
    S := 'Elektronik: Multivibratorschaltungen mit npn-Transistoren.' + Chr(13) + Chr(13);
    S += '© allu, Juni, 2018.';
    pnHelp.Title := 'Über Multivibrator.';
    pnHelp.Text := S;
    pnHelp.Show;
  end;
end;

{ Button "Anzeigen": Open the form with the selected multivibrator }

procedure TfMV.btShowClick(Sender: TObject);

begin
  if sMvib = 'bistabil' then begin
    fMVB.ShowModal;
  end
  else if sMvib = 'monostabil' then begin
    fMVM.rTimeMult := rMult;                                                   // simulation time multiplicator passed to the fMVM form
    fMVM.ShowModal;
  end
  else begin
    fMVA.rTimeMult := rMult;                                                   // simulation time multiplicator passed to the fMVA form
    fMVA.ShowModal;
  end;
end;

end.

