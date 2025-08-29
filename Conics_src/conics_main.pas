{***********************************}
{* Main unit of Conics application *}
{***********************************}

unit conics_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, conics_graph;

type
  {**********}
  { TfConics }
  {**********}
  TfConics = class(TForm)
    mSettingsInversion: TMenuItem;
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsNegativeCoeff: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10: TLabel;
    rbParabola, rbCircle, rbEllipse, rbHyperbola: TRadioButton;
    edParabolaEq, edCircleEq, edEllipseEq, edHyperbolaEq: TEdit;
    edParabolaA, edCircleA, edEllipseA, edEllipseB, edHyperbolaA, edHyperbolaB: TEdit;
    edDescription: TMemo;
    cbInverse: TCheckBox;
    btDraw: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsNegativeCoeffClick(Sender: TObject);
    procedure mSettingsInversionClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure rbParabolaChange(Sender: TObject);
    procedure rbCircleChange(Sender: TObject);
    procedure rbEllipseChange(Sender: TObject);
    procedure rbHyperbolaChange(Sender: TObject);
    procedure edParabolaAChange(Sender: TObject);
    procedure edCircleAChange(Sender: TObject);
    procedure edEllipseAChange(Sender: TObject);
    procedure edEllipseBChange(Sender: TObject);
    procedure edHyperbolaAChange(Sender: TObject);
    procedure edHyperbolaBChange(Sender: TObject);
  private
    rA, rB: Real;
    sConic, sEquation: string;
  end;

var
  fConics: TfConics;

implementation

{$R *.lfm}

{ Read conic parameters from form (user entry values) }

procedure ReadParams(Conic: string; NegCoeff: Boolean; out A, B: Real; out Mess: string);

begin
  Mess := '';
  // Parabola
  if Conic = 'Parabola' then begin
    if fConics.edParabolaA.Text = '' then
      A := 0
    else
      A := StrToFloat(fConics.edParabolaA.Text);
    if A <= 0 then begin
      if A = 0 then
        Mess := 'Parabola leading coefficient must be different from 0!'
      else if not NegCoeff and (A < 0) then                                    // leading coefficient should be positive in standard form (?)
        Mess := 'Parabola leading coefficient must be greater than 0!';
      fConics.edParabolaA.SetFocus;
    end;
  end
  // Circle
  else if Conic = 'Circle' then begin
    if fConics.edCircleA.Text = '' then
      A := 0
    else
      A := StrToFloat(fConics.edCircleA.Text);
    if A <= 0 then begin
      Mess := 'Circle radius must be greater than 0!';
      fConics.edCircleA.SetFocus;
    end;
  end
  // Ellipse
  else if Conic = 'Ellipse' then begin
    if fConics.edEllipseA.Text = '' then
      A := 0
    else
      A := StrToFloat(fConics.edEllipseA.Text);
    if fConics.edEllipseB.Text = '' then
      B := 0
    else
      B := StrToFloat(fConics.edEllipseB.Text);
    if A <= 0 then begin
      Mess := 'Ellipse major axis must be greater than 0!';
      fConics.edEllipseA.SetFocus;
    end
    else if B <= 0 then begin
      Mess := 'Ellipse minor axis must be greater than 0!';
      fConics.edEllipseB.SetFocus;
    end
    else if A <= B then begin                                                  // major axis must be greater than minor axis
      Mess := 'Ellipse major axis must be greater than minor axis!';
      fConics.edEllipseA.SetFocus;
    end;
  end
  // Hyperbola
  else begin
    if fConics.edHyperbolaA.Text = '' then
      A := 0
    else
      A := StrToFloat(fConics.edHyperbolaA.Text);
    if fConics.edHyperbolaB.Text = '' then
      B := 0
    else
      B := StrToFloat(fConics.edHyperbolaB.Text);
    if A <= 0 then begin
      Mess := 'Hyperbola major axis must be greater than 0!';
      fConics.edHyperbolaA.SetFocus;
    end
    else if B <= 0 then begin
      Mess := 'Hyperbola minor axis must be greater than 0!';
      fConics.edHyperbolaB.SetFocus;
    end;
  end;
end;

{**********}
{ TfConics }
{**********}

{ Application start: Initialisation }

procedure TfConics.FormCreate(Sender: TObject);

begin
  edParabolaEq.Text := StringReplace(edParabolaEq.Text, '2', SUP_2, [rfReplaceAll]);
  edCircleEq.Text := StringReplace(edCircleEq.Text, '2', SUP_2, [rfReplaceAll]);
  edEllipseEq.Text := StringReplace(edEllipseEq.Text, '2', SUP_2, [rfReplaceAll]);
  edHyperbolaEq.Text := StringReplace(edHyperbolaEq.Text, '2', SUP_2, [rfReplaceAll]);
  sConic := 'Parabola'; sEquation := edParabolaEq.Text;
end;

{ Menu item "File > Exit": Exit application }

procedure TfConics.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Allow parabola negative leading coefficient": Toggle to allow it or not }

procedure TfConics.mSettingsNegativeCoeffClick(Sender: TObject);

begin
  if mSettingsNegativeCoeff.Checked then
    mSettingsNegativeCoeff.Checked := False
  else
    mSettingsNegativeCoeff.Checked := True;
end;

{ Menu item "Settings > Allow inversion of x-axis and y-axis": Toggle to allow it or not }

procedure TfConics.mSettingsInversionClick(Sender: TObject);

begin
  if mSettingsInversion.Checked then begin
    mSettingsInversion.Checked := False;
    cbInverse.Checked := False; cbInverse.Enabled := False;
  end
  else begin
    mSettingsInversion.Checked := True;
    cbInverse.Enabled := True;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfConics.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics:' + LineEnding;
  S += 'Standard form of conic sections.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February-June 2020.';
  MessageDlg('About "Conics"', S, mtInformation, [mbOK], 0);
end;

{ Button "Draw": Draw the conic (is actually done by conics_graph unit) }

procedure TfConics.btDrawClick(Sender: TObject);

var
  Mess: string;

begin
  ReadParams(sConic, mSettingsNegativeCoeff.Checked, rA, rB, Mess);
  if Mess = '' then begin
    fConics.btDraw.SetFocus;
    // Pass conic parameters to fGraph form
    fGraph.sConic := sConic;
    fGraph.sEquation := sEquation;
    fGraph.rA := rA; fGraph.rB := rB;
    fGraph.bInverse := cbInverse.Checked;
    // Show fGraph window (conic will be drawn with actual parameters at window show-up)
    fGraph.Show;
  end
  else
    MessageDlg('Invalid parameters', Mess, mtError, [mbOK], 0);                // invalid user entry values for actual conic
end;

{ Checking of one of the radiobuttons = selection of the corr. conic }

procedure TfConics.rbParabolaChange(Sender: TObject);

begin
  if rbParabola.Checked then begin
    sConic := 'Parabola'; sEquation := edParabolaEq.Text;
    edParabolaA.SetFocus;
    cbInverse.Visible := True;
  end;
end;

procedure TfConics.rbCircleChange(Sender: TObject);

begin
  if rbCircle.Checked then begin
    sConic := 'Circle'; sEquation := edCircleEq.Text;
    edCircleA.SetFocus;
    cbInverse.Checked := False; cbInverse.Visible := False;
  end;
end;

procedure TfConics.rbEllipseChange(Sender: TObject);

begin
  if rbEllipse.Checked then begin
    sConic := 'Ellipse'; sEquation := edEllipseEq.Text;
    edEllipseA.SetFocus;
    cbInverse.Visible := True;
  end;
end;

procedure TfConics.rbHyperbolaChange(Sender: TObject);

begin
  if rbHyperbola.Checked then begin
    sConic := 'Hyperbola'; sEquation := edHyperbolaEq.Text;
    edHyperbolaA.SetFocus;
    cbInverse.Visible := True;
  end;
end;

{ Change of one of the edit fields' content: automatically select the corr. conic }

procedure TfConics.edParabolaAChange(Sender: TObject);

begin
  rbParabola.Checked := True;
end;

procedure TfConics.edCircleAChange(Sender: TObject);

begin
  rbCircle.Checked := True;
end;

procedure TfConics.edEllipseAChange(Sender: TObject);

begin
  rbEllipse.Checked := True;
end;

procedure TfConics.edEllipseBChange(Sender: TObject);

begin
  rbEllipse.Checked := True;
end;

procedure TfConics.edHyperbolaAChange(Sender: TObject);

begin
  rbHyperbola.Checked := True;
end;

procedure TfConics.edHyperbolaBChange(Sender: TObject);

begin
  rbHyperbola.Checked := True;
end;

end.

