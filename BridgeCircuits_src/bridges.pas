{**********************************************************************}
{* Main unit (incl. Wheatstone bridge) for BridgeCircuits application *}
{**********************************************************************}

unit bridges;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, maxwell, hay, schering, wien, common;

type
  {***********}
  { TfBridges }
  {***********}
  TfBridges = class(TForm)
    mMenu: TMainMenu;
    mBridge, mBridgeWheatstone, mBridgeMaxwell, mBridgeHay: TMenuItem;
    mBridgeSchering, mBridgeWien, mBridgeExit: TMenuItem;
    mOptions, mOptionsR, mOptionsRKiloOhm, mOptionsROhm: TMenuItem;
    mOptionsC, mOptionsCMicro, mOptionsCNano: TMenuItem;
    mOptionsL, mOptionsLHenry, mOptionsLMilliHenry, mOptionsKeepInput: TMenuItem;
    mOptionsDecimals, mOptionsDecimals0, mOptionsDecimals1, mOptionsDecimals2, mOptionsDecimals3: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1, StaticText2: TStaticText;
    edDetails: TMemo;
    Image1: TImage;
    Label1, Label2, Label3, Label4: TLabel;
    laR1, laR2, laR3, laR4: TLabel;
    laUR1, laUR2, laUR3, laUR4: TLabel;
    edFormula, edR1, edR2, edR3, edR4: TEdit;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mBridgeWheatstoneClick(Sender: TObject);
    procedure mBridgeMaxwellClick(Sender: TObject);
    procedure mBridgeHayClick(Sender: TObject);
    procedure mBridgeScheringClick(Sender: TObject);
    procedure mBridgeWienClick(Sender: TObject);
    procedure mBridgeExitClick(Sender: TObject);
    procedure mOptionsRKiloOhmClick(Sender: TObject);
    procedure mOptionsROhmClick(Sender: TObject);
    procedure mOptionsCMicroClick(Sender: TObject);
    procedure mOptionsCNanoClick(Sender: TObject);
    procedure mOptionsLHenryClick(Sender: TObject);
    procedure mOptionsLMilliHenryClick(Sender: TObject);
    procedure mOptionsDecimals0Click(Sender: TObject);
    procedure mOptionsDecimals1Click(Sender: TObject);
    procedure mOptionsDecimals2Click(Sender: TObject);
    procedure mOptionsDecimals3Click(Sender: TObject);
    procedure mOptionsKeepInputClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
  private
    iDecimals: Integer;
    rOldMultR, rOldMultC, rOldMultL, rMultR, rMultC, rMultL: Real;
    bKeepInput: Boolean;
    aR: array[0..3] of Real;
    edR: array[0..3] of TEdit;
  end;

var
  fBridges: TfBridges;

implementation

{$R *.lfm}

{***********}
{ TfBridges }
{***********}

{ Application start: Initialisation }

procedure TfBridges.FormCreate(Sender: TObject);

var
  S: string;

begin
  // Apply subscripts
  S := edDetails.Text; edDetails.Text := ApplySubscripts(S);
  S := edFormula.Text; edFormula.Text := ApplySubscripts(S);
  S := laR1.Caption; laR1.Caption := ApplySubscripts(S);
  S := laR2.Caption; laR2.Caption := ApplySubscripts(S);
  S := laR3.Caption; laR3.Caption := ApplySubscripts(S);
  S := laR4.Caption; laR4.Caption := ApplySubscripts(S);
  // Create array with form input/output fileds
  edR[0] := edR1; edR[1] := edR2; edR[2] := edR3; edR[3] := edR4;
  // Initial unit conversion factors
  rOldMultR := 1E+3; rOldMultC := 1E-6; rOldMultL := 1E-3;
  rMultR := rOldMultR; rMultC := rOldMultC; rMultL := rOldMultL;
  // Start-up values
  iDecimals := 3; bKeepInput := True;
end;

{ Window activation: Init the Wheatstone bridge circuit }

procedure TfBridges.FormActivate(Sender: TObject);

begin
  mBridgeWheatstone.Click;
end;

{ Menu item "Bridge > Wheatstone bridge": Init the Wheatstone bridge circuit (in main window) }

procedure TfBridges.mBridgeWheatstoneClick(Sender: TObject);

begin
  if not bKeepInput then begin
    // Clear input fields (unless values have to be kept)
    edR1.Text := ''; edR2.Text := ''; edR3.Text := '';
  end;
  edR4.Text := '';                                                             // Clear result field
  edR1.SetFocus;                                                               // Focus first input field
end;

{ Menu item "Bridge > Maxwell bridge": Init the Maxwell bridge circuit (in new window) }

procedure TfBridges.mBridgeMaxwellClick(Sender: TObject);

begin
  // Pass actual settings to "Maxwell" form
  fMaxwell.rMultR := rMultR; fMaxwell.rMultC := rMultC; fMaxwell.rMultL := rMultL;
  fMaxwell.iDecimals := iDecimals; fMaxwell.bKeepInput := bKeepInput;
  // Open the "Maxwell window" (and wait until user closes it)
  fMaxwell.ShowModal;
end;

{ Menu item "Bridge > Hay bridge": Init the Hay bridge circuit (in new window) }

procedure TfBridges.mBridgeHayClick(Sender: TObject);

begin
  fHay.rMultR := rMultR; fHay.rMultC := rMultC; fHay.rMultL := rMultL;
  fHay.iDecimals := iDecimals; fHay.bKeepInput := bKeepInput;
  // Open the "Hay window" (and wait until user closes it)
  fHay.ShowModal;
end;

{ Menu item "Bridge > Schering bridge": Init the Schering bridge circuit (in new window) }

procedure TfBridges.mBridgeScheringClick(Sender: TObject);

begin
  fSchering.rMultR := rMultR; fSchering.rMultC := rMultC; fSchering.iDecimals := iDecimals;
  fSchering.bKeepInput := bKeepInput; fSchering.ShowModal;
  // Open the "Schering window" (and wait until user closes it)
end;

{ Menu item "Bridge > Wien bridge": Init the Wien bridge circuit (in new window) }

procedure TfBridges.mBridgeWienClick(Sender: TObject);

begin
  fWien.rMultR := rMultR; fWien.rMultC := rMultC;
  fWien.iDecimals := iDecimals; fWien.bKeepInput := bKeepInput;
  // Open the "Wien window" (and wait until user closes it)
  fWien.ShowModal;
end;

{ Menu item "Bridge > Exit": Exit application }

procedure TfBridges.mBridgeExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Resistance unit": Select resistance unit (kΩ or Ω) }

procedure TfBridges.mOptionsRKiloOhmClick(Sender: TObject);

var
  I: Integer;
  R: Real;

begin
  mOptionsRKiloOhm.Checked := True; mOptionsROhm.Checked := False;
  rMultR := 1E+3;                                                              // conversion factor
  laUR1.Caption := 'kΩ'; laUR2.Caption := 'kΩ';
  laUR3.Caption := 'kΩ'; laUR4.Caption := 'kΩ';
  edR4.Text := '';                                                             // clear result field
  if bKeepInput and (rMultR <> rOldMultR) then begin
    // If input values have to be kept and resistance unit was changed, transform input values to kΩ
    for I := 0 to 2 do begin
      if edR[I].Text <> '' then begin
        R := StrToFloat(edR[I].Text) / 1000;
        edR[I].Text := RFormat(R, iDecimals);
      end;
    end;
  end;
  rOldMultR := rMultR;                                                         // save actual conversion factor
end;

procedure TfBridges.mOptionsROhmClick(Sender: TObject);

var
  I: Integer;
  R: Real;

begin
  mOptionsRKiloOhm.Checked := False; mOptionsROhm.Checked := True;
  rMultR := 1;
  laUR1.Caption := 'Ω'; laUR2.Caption := 'Ω';
  laUR3.Caption := 'Ω'; laUR4.Caption := 'Ω';
  edR4.Text := '';
  if bKeepInput and (rMultR <> rOldMultR) then begin
    // If input values have to be kept and resistance unit was changed, transform input values to Ω
    for I := 0 to 2 do begin
      if edR[I].Text <> '' then begin
        R := StrToFloat(edR[I].Text) * 1000;
        edR[I].Text := RFormat(R, iDecimals);
      end;
    end;
  end;
  rOldMultR := rMultR;
end;

{ Menu items "Options > Capacitance unit": Select capacitance unit (µF or nF) }

procedure TfBridges.mOptionsCMicroClick(Sender: TObject);

begin
  mOptionsCMicro.Checked := True; mOptionsCNano.Checked := False;
  rMultC := 1E-6;
end;

procedure TfBridges.mOptionsCNanoClick(Sender: TObject);

begin
  mOptionsCMicro.Checked := False; mOptionsCNano.Checked := True;
  rMultC := 1E-9;
end;

{ Menu items "Options > Inductance unit": Select inductance unit (mH or H) }

procedure TfBridges.mOptionsLHenryClick(Sender: TObject);

begin
  mOptionsLHenry.Checked := True; mOptionsLMilliHenry.Checked := False;
  rMultL := 1;
end;

procedure TfBridges.mOptionsLMilliHenryClick(Sender: TObject);

begin
  mOptionsLHenry.Checked := False; mOptionsLMilliHenry.Checked := True;
  rMultL := 1E-3;
end;

{ Menu items "Options > Data display": Select data display precision (0, 1, 2 or 3 decimals) }

procedure TfBridges.mOptionsDecimals0Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := True;  mOptionsDecimals1.Checked := False;
  mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := False;
  iDecimals := 0;
end;

procedure TfBridges.mOptionsDecimals1Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := False; mOptionsDecimals1.Checked := True;
  mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := False;
  iDecimals := 1;
end;

procedure TfBridges.mOptionsDecimals2Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := False; mOptionsDecimals1.Checked := False;
  mOptionsDecimals2.Checked := True;  mOptionsDecimals3.Checked := False;
  iDecimals := 2;
end;

procedure TfBridges.mOptionsDecimals3Click(Sender: TObject);

begin
  mOptionsDecimals0.Checked := False; mOptionsDecimals1.Checked := False;
  mOptionsDecimals2.Checked := False; mOptionsDecimals3.Checked := True;
  iDecimals := 3;
end;

{ Menu item "Options > Keep input values": Toggle to keep or to clear user input values }

procedure TfBridges.mOptionsKeepInputClick(Sender: TObject);

begin
  if mOptionsKeepInput.Checked then
    mOptionsKeepInput.Checked := False
  else
    mOptionsKeepInput.Checked := True;
  bKeepInput := mOptionsKeepInput.Checked;
end;

{ Menu item "Help > About": Display application about }

procedure TfBridges.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronic circuits.' + LineEnding;
  S += 'Measurement bridges: Wheatstone bridge, Maxwell bridge, Hay bridge, Schering bridge, Wien bridge.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October 2022.';
  MessageDlg('About "BridgeCircuits"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculate" pushed: Calculate Wheatstone bridge resistance }

procedure TfBridges.btCalcClick(Sender: TObject);

var
  R1, R2, R3, R4: Real;
  Mess: string;

begin
  GetData(edR, 3, False, aR, Mess);                                            // read user data from form
  if Mess = '' then begin                                                      // transform resistances to actual unit
    R1 := aR[0] * rMultR; R2 := aR[1] * rMultR; R3 := aR[2] * rMultR;          // calculate R4
    R4 := (R3 * (R2 / R1)) / rMultR;                                           // display R4 (with number of decimals as selected)
    edR4.Text := RFormat(R4, iDecimals);
  end;
end;

end.

