{************************************}
{* Main unit of Ellipse application *}
{************************************}

unit ellipse_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, ellipse_u2;

type
  {***********}
  { TfEllipse }
  {***********}
  TfEllipse = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label3, Label4, Label5, Label8, Label9, Label11, Label12, Label13: TLabel;
    rbTangent, rbCircle, rbNormal: TRadioButton;
    rgForm: TRadioGroup;
    edDescription: TMemo;
    laPointX: TLabel;
    laPointY: TLabel;
    edEllipseEq, edEllipseA, edEllipseB: TEdit;
    edPointX, edPointY, edPointAngle, edSlope: TEdit;
    btDraw: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbTangentChange(Sender: TObject);
    procedure rbNormalChange(Sender: TObject);
    procedure rgFormClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
  private
    rA, rB, rPX, rPY, rPA, rM: Real;
    sLine, sForm: string;
  end;

var
  fEllipse: TfEllipse;

implementation

{$R *.lfm}

{ Read conic parameters from form (user entry values) }

procedure ReadParams(Line, LForm: string; out A, B, PX, PY, PA, M: Real; out Mess: string);

begin
  A := 0; B := 0;
  PX := 0; PY := 0; PA := 0; M := 0;
  Mess := '';
  if fEllipse.edEllipseA.Text = '' then begin
    Mess := 'Ellipse horizontal axis is missing!';
    fEllipse.edEllipseA.SetFocus;
  end
  else begin
    A := StrToFloat(fEllipse.edEllipseA.Text);
    if A <= 0 then begin
      Mess := 'Ellipse horizontal axis must be greater than 0!';
      fEllipse.edEllipseA.SetFocus;
    end
  end;
  if Mess = '' then begin
    if fEllipse.edEllipseB.Text = '' then begin
      Mess := 'Ellipse vertical axis is missing!';
      fEllipse.edEllipseB.SetFocus;
    end
    else begin
      B := StrToFloat(fEllipse.edEllipseB.Text);
      if B <= 0 then begin
        Mess := 'Ellipse vertical axis must be greater than 0!';
        fEllipse.edEllipseB.SetFocus;
      end;
    end;
  end;
  if Mess = '' then begin
    if LForm = 'point' then begin
      if fEllipse.edPointX.Text = '' then begin
        Mess := 'Tangent point x-coordinate is missing!';
        fEllipse.edPointX.SetFocus;
      end
      else begin
        if fEllipse.edPointX.Text <> '?' then
          PX := StrToFloat(fEllipse.edPointX.Text);
        if fEllipse.edPointY.Text = '' then begin
          Mess := 'Tangent point y-coordinate is missing!';
          fEllipse.edPointY.SetFocus;
        end
        else begin
          if fEllipse.edPointY.Text <> '?' then
            PY := StrToFloat(fEllipse.edPointY.Text);
          if (fEllipse.edPointX.Text = '?') and (fEllipse.edPointY.Text = '?') then begin
            Mess := 'At least one tangent point coordinate must be a real value!';
            fEllipse.edPointX.SetFocus;
          end
          else if fEllipse.edPointX.Text = '?' then begin
            if PY > B then begin
              Mess := 'Invalid tangent point y-coordinate!';
              fEllipse.edPointY.SetFocus;
            end
            else
              PX := A * Sqrt(1 - Sqr(PY) / Sqr(B));
          end
          else if fEllipse.edPointY.Text = '?' then begin
            if PX > A then begin
              Mess := 'Invalid tangent point x-coordinate!';
              fEllipse.edPointX.SetFocus;
            end
            else
              PY := B * Sqrt(1 - Sqr(PX) / Sqr(A));
          end;
          if Mess = '' then begin
            if Abs(Sqr(PX) / Sqr(A) + Sqr(PY) / Sqr(B) - 1) > 1E-4 then begin
              Mess := 'The point given is not located on the ellipse curve!';
              fEllipse.edPointX.SetFocus;
            end;
          end;
        end;
      end;
    end
    else if LForm = 'parametric' then begin
      if fEllipse.edPointAngle.Text = '' then begin
        Mess := 'Tangent point angle is missing!';
        fEllipse.edPointAngle.SetFocus;
      end
      else
        PA := StrToFloat(fEllipse.edPointAngle.Text);
        while PA < 0 do
          PA += 360;
        while PA > 360 do
          PA -= 360;
    end
    else begin
      if fEllipse.edSlope.Text = '' then begin
        Mess := UCFirst(Line) + ' slope is missing!';
        fEllipse.edSlope.SetFocus;
      end
      else
        M := StrToFloat(fEllipse.edSlope.Text);
    end;
  end;
end;

{***********}
{ TfEllipse }
{***********}

{ Application start: Initialisation }

procedure TfEllipse.FormCreate(Sender: TObject);

begin
  // Apply sub- and superscripts
  edEllipseEq.Text := StringReplace(edEllipseEq.Text, '2', SUP_2, [rfReplaceAll]);
  laPointX.Caption := StringReplace(laPointX.Caption, '1', SUB_1, []);
  laPointY.Caption := StringReplace(laPointY.Caption, '1', SUB_1, []);
  edDescription.Text := StringReplace(edDescription.Text, '1', SUB_1, [rfReplaceAll]);
  // Start-up selections
  sLine := 'tangent'; sForm := 'point';
end;

{ Menu item "File > Exit": Exit application }

procedure TfEllipse.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfEllipse.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics:' + LineEnding;
  S += 'Tangent and normal to an ellipse.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August-September 2022.';
  MessageDlg('About "Ellipse"', S, mtInformation, [mbOK], 0);
end;

{ Button "Draw": Draw the ellipse and the tangent/normal (drawing is actually done by the ellipse_u2 unit) }

procedure TfEllipse.btDrawClick(Sender: TObject);

var
  Mess: string;

begin
  ReadParams(sLine, sForm, rA, rB, rPX, rPY, rPA, rM, Mess);
  if Mess = '' then begin
    fEllipse.btDraw.SetFocus;
    // Pass ellipse parameters to fGraph form
    fGraph.sLine := sLine; fGraph.sForm := sForm;
    fGraph.rA := rA; fGraph.rB := rB;
    if sForm = 'point' then begin
      fGraph.rPX := rPX; fGraph.rPY := rPY;
    end
    else if sForm = 'parametric' then begin
      fGraph.rPA := rPA;
    end
    else begin
      fGraph.rM := rM;
    end;
    // Show fGraph window (ellipse and tangent/normal will be drawn with actual parameters at window show-up)
    fGraph.Show;
  end
  else
    MessageDlg('Invalid parameters', Mess, mtError, [mbOK], 0);                // invalid user input values
end;

{ Line (tangent or normal) selection (radiobutton changes) }

procedure TfEllipse.rbTangentChange(Sender: TObject);

begin
  if rbTangent.Checked then
    sLine := 'tangent';
end;

procedure TfEllipse.rbNormalChange(Sender: TObject);

begin
  if rbNormal.Checked then
    sLine := 'normal';
end;

{ Line form (point, parametric, slope) selection (radiogroup changes) }

procedure TfEllipse.rgFormClick(Sender: TObject);

begin
  edPointX.Enabled := True; edPointY.Enabled := True;
  edPointAngle.Enabled := True; edSlope.Enabled := True;
  case rgForm.ItemIndex of
    0: begin
         sForm := 'point';
         edPointAngle.Text := ''; edSlope.Text := '';
         edPointAngle.Enabled := False; edSlope.Enabled := False;
    end;
    1: begin
         sForm := 'parametric';
         edPointX.Text := ''; edPointY.Text := ''; edSlope.Text := '';
         edPointX.Enabled := False; edPointY.Enabled := False; edSlope.Enabled := False;
    end;
    2: begin
         sForm := 'slope';
         edPointX.Text := ''; edPointY.Text := ''; edPointAngle.Text := '';
         edPointX.Enabled := False; edPointY.Enabled := False; edPointAngle.Enabled := False;
    end;
  end;
end;

end.

