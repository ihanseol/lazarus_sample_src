{************************************}
{* Main unit of Circles application *}
{************************************}

unit circles_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, circles_graph;

type
  {***********}
  { TfCircles }
  {***********}
  TfCircles = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsCircle, mOptionsCircleCentre, mOptionsCirclePoints: TMenuItem;
    mOptionsRound2: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label11, Label12, Label13, Label14, Label15, Label16, Label17, Label18, Label19, Label20: TLabel;
    Label21, Label22, Label23, Label24, Label25, Label26, Label27, Label28, Label29: TLabel;
    edAx, edAy, edBx, edBy: TEdit;
    edCx, edCy, edR, edEquation: TEdit;
    rbTangent, rbIntLine, rbIntCircle, rbShapes: TRadioButton;
    laFy, laLineM, laLineC: TLabel;
    edFx, edFy: TEdit;
    cobLine: TComboBox;
    edLineM, edLineC: TEdit;
    edIntCircleCx, edIntCircleCy, edIntCircleR: TEdit;
    cbSquareOut, cbSquareIn: TCheckBox;
    btDraw: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mOptionsCircleCentreClick(Sender: TObject);
    procedure mOptionsCirclePointsClick(Sender: TObject);
    procedure mOptionsRound2Click(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure rbTangentChange(Sender: TObject);
    procedure rbIntCircleChange(Sender: TObject);
    procedure rbIntLineChange(Sender: TObject);
    procedure rbShapesChange(Sender: TObject);
    procedure edFxChange(Sender: TObject);
    procedure edFyChange(Sender: TObject);
    procedure cobLineChange(Sender: TObject);
    procedure edLineMChange(Sender: TObject);
    procedure edLineCChange(Sender: TObject);
    procedure edIntCircleCxChange(Sender: TObject);
    procedure edIntCircleCyChange(Sender: TObject);
    procedure edIntCircleRChange(Sender: TObject);
    procedure cbSquareOutChange(Sender: TObject);
    procedure cbSquareInChange(Sender: TObject);
  private
    iFormat: Integer;
    rAX, rAY, rBX, rBY, rCX, rCY, rR, rFX, rFY, rLM, rLC, rCCX, rCCY, rCR: Real;
    sCalculation, sCircleEquation, sEquationLine, sEquationCircle: string;
    bLineX, bSquareIn, bSquareOut: Boolean;
  end;

var
  fCircles: TfCircles;

implementation

{$R *.lfm}

{ Read circle and calculation parameters from form (user input values) }

procedure ReadParams(Calc: string; F: Integer; out AX, AY, BX, BY, CX, CY, R, FX, FY, LM, LC, CCX, CCY, CR: Real;
  out EquCircle, EquIntLine, EquIntCircle: string; out LineX, SquareIn, SquareOut: Boolean; out Mess: string);

// The procedure also determines the circle equation and (if is actual case) the intersection line/circle equation

begin
  AX := 0; AY := 0; BX := 0; BY := 0; CX := 0; CY := 0; R := 0; FX := 0; FY := 0;
  LM := 0; LC := 0; CCX := 0; CCY := 0; CR := 0;
  EquCircle := ''; EquIntLine := ''; EquIntCircle := ''; Mess := '';
  LineX := False; SquareIn := False; SquareOut := False;
  if fCircles.mOptionsCircleCentre.Checked then begin
    // Circle defined by center and radius
    if (fCircles.edCX.Text <> '') and (fCircles.edCY.Text <> '') and (fCircles.edR.Text <> '') then begin
      CX := StrToFloat(fCircles.edCx.Text); CY := StrToFloat(fCircles.edCy.Text); R := StrToFloat(fCircles.edR.Text);
      if R <= 0 then
        Mess := 'Circle radius is invalid';
    end
    else begin
      if fCircles.edR.Text = '' then
        Mess := 'Circle radius is missing'
      else
        Mess := 'Circle center coordinates are missing or incomplete';
    end;
  end
  else if fCircles.mOptionsCirclePoints.Checked then begin
    // Circle defined by two diameter points
    if (fCircles.edAX.Text <> '') and (fCircles.edAY.Text <> '') and (fCircles.edBX.Text <> '') and (fCircles.edBY.Text <> '') then begin
      AX := StrToFloat(fCircles.edAx.Text); AY := StrToFloat(fCircles.edAy.Text);
      BX := StrToFloat(fCircles.edBx.Text); BY := StrToFloat(fCircles.edBy.Text);
      // Calculation of center coordinates and radius
      CX := (AX + BX) / 2; fCircles.edCx.Text := FloatToStr(CX);
      CY := (AY + BY) / 2; fCircles.edCy.Text := FloatToStr(CY);
      R := Sqrt(Sqr(BX - AX) + Sqr(BY - AY)) / 2; fCircles.edR.Text := FloatToStr(R);
      if R = 0 then
        Mess := 'Circle points are invalid (same coordinates)';
    end
    else begin
      Mess := 'Circle points coordinates are missing or incomplete';
    end;
  end;
  if Mess = '' then begin
    // Determine circle equation
    EquCircle := CircleEquation(CX, CY, R, F); fCircles.edEquation.Text := EquCircle;
    // Read parameters for actual calculation
    if Calc = 'tangent' then begin
      // Tangent line
      if (fCircles.edFx.Text = '') or (fCircles.edFy.Text = '') then
        Mess := 'Tangent point coordinates are missing or incomplete'
      else begin
        FX := StrToFloat(fCircles.edFx.Text); FY := StrToFloat(fCircles.edFy.Text);
        if Abs((Sqr(FX - CX) + Sqr(FY - CY)) - Sqr(R)) > 1E-4 then
          // Point given must be part of the circle, of course
          Mess := 'Tangent point is not located on given circle curve'
      end;
    end
    else if Calc = 'line' then begin
      // Intersection with a line
      if fCircles.cobLine.ItemIndex = 1 then
        LineX := True;                                                         // the LineX variable is set to True for line // Oy (x = c)
      if (fCircles.edLineM.Visible and (fCircles.edLineM.Text = '')) or (fCircles.edLineC.Text = '') then
        Mess := 'Intersection line parameters missing'
      else begin
        if fCircles.edLineM.Visible then
          LM := StrToFloat(fCircles.edLineM.Text);
        LC := StrToFloat(fCircles.edLineC.Text);
        // Determine intersection line equation
        EquIntLine := LineEquation(LM, LC, LineX, F);
      end;
    end
    else if Calc = 'circle' then begin
      // Intersection with circle
      if (fCircles.edIntCircleCx.Text = '') or (fCircles.edIntCircleCy.Text = '') or (fCircles.edIntCircleR.Text = '') then
        Mess := 'Intersection circle parameters missing'
      else begin
        CCX := StrToFloat(fCircles.edIntCircleCx.Text); CCY := StrToFloat(fCircles.edIntCircleCy.Text);
        CR := StrToFloat(fCircles.edIntCircleR.Text);
        // Determine intersection circle equation
        EquIntCircle := CircleEquation(CCX, CCY, CR, F);
      end;
    end
    else begin
      // Outer and/or inner square
      if fCircles.cbSquareIn.Checked or fCircles.cbSquareOut.Checked then begin
        SquareIn := fCircles.cbSquareIn.Checked; SquareOut := fCircles.cbSquareOut.Checked;
      end
      else
        Mess := 'Shape selection is missing';
    end;
  end;
end;

{***********}
{ TfCircles }
{***********}

{ Application start: Initialisation }

procedure TfCircles.FormCreate(Sender: TObject);

begin
  sCalculation := 'tangent'; iFormat := 3;
end;

{ Menu item "File > Exit": Exit application }

procedure TfCircles.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Circle definition > ...": Choose circle definition by center/radius or 2 diameter points }

procedure TfCircles.mOptionsCircleCentreClick(Sender: TObject);

begin
  mOptionsCircleCentre.Checked := True; mOptionsCirclePoints.Checked := False;
  edCX.ReadOnly := False; edCY.ReadOnly := False; edR.ReadOnly := False;
  edCX.TabStop := True; edCY.TabStop := True; edR.TabStop := True;
  edAX.Text := ''; edAY.Text := ''; edBX.Text := ''; edBY.Text := '';
  edAX.Enabled := False; edAY.Enabled := False; edBX.Enabled := False; edBY.Enabled := False;
end;

procedure TfCircles.mOptionsCirclePointsClick(Sender: TObject);

begin
  mOptionsCircleCentre.Checked := False; mOptionsCirclePoints.Checked := True;
  edCX.Text := ''; edCY.Text := ''; edR.Text := '';
  edCX.ReadOnly := True; edCY.ReadOnly := True; edR.ReadOnly := True;
  edCX.TabStop := False; edCY.TabStop := False; edR.TabStop := False;
  edAX.Enabled := True; edAY.Enabled := True; edBX.Enabled := True; edBY.Enabled := True;
end;

{ Menu item "Options > Round to 2 decimal digits": Choose number of decimal digits (2 or default = 3) }

procedure TfCircles.mOptionsRound2Click(Sender: TObject);

begin
  if mOptionsRound2.Checked then begin
    mOptionsRound2.Checked := False;
    iFormat := 3;
  end
  else begin
    mOptionsRound2.Checked := True;
    iFormat := 2;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfCircles.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics: Analytical Geometry - Circles.' + LineEnding;
  S += 'Determination of the tangent, the intersection with a line or a circle, the outer and inner squares.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, May-June 2021.';
  MessageDlg('About "Circles"', S, mtInformation, [mbOK], 0);
end;

{ Button "Draw": Draw the circle (and other calculated items) }

procedure TfCircles.btDrawClick(Sender: TObject);

// The drawing code is part of the circles_graph unit

var
  Mess: string;

begin
  if sCalculation <> '' then begin
    ReadParams(sCalculation, iFormat, rAX, rAY, rBX, rBY, rCX, rCY, rR, rFX, rFY, rLM, rLC, rCCX, rCCY, rCR,
      sCircleEquation, sEquationLine, sEquationCircle, bLineX, bSquareIn, bSquareOut, Mess);
    if Mess = '' then begin
      fCircles.btDraw.SetFocus;
      // Pass calculation parameters to fGraph form
      fGraph.sCalculation := sCalculation; fGraph.iFormat := iFormat; fGraph.sEquation := sCircleEquation;
      if mOptionsCirclePoints.Checked then begin
        // Circle defined by 2 diameter points
        fGraph.bCirclePoints := True;
        fGraph.rAX := rAX; fGraph.rAY := rAY; fGraph.rBX := rBX; fGraph.rBY := rBY;
      end
      else
        fGraph.bCirclePoints := False;
      // Circle parameters
      fGraph.rCX := rCX; fGraph.rCY := rCY; fGraph.rR := rR;
      // Tangent intersection line/circle parameters or shape(s) selected
      if sCalculation = 'tangent' then begin
        fGraph.rFX := StrToFloat(edFx.Text); fGraph.rFY := StrToFloat(edFy.Text);
      end
      else if sCalculation = 'line' then begin
        fGraph.bLineX := bLineX;
        fGraph.rLM := rLM; fGraph.rLC := rLC;
        fGraph.sEquationInt := sEquationLine
      end
      else if sCalculation = 'circle' then begin
        fGraph.rCCX := rCCX; fGraph.rCCY := rCCY; fGraph.rCR := rCR;
        fGraph.sEquationInt := sEquationCircle;
      end
      else begin
        fGraph.bSqIn := bSquareIn; fGraph.bSqOut := bSquareOut;
      end;
      // Show fGraph window (circle and other items will be drawn with actual parameters at window show-up)
      fGraph.Show;
    end
    else
      MessageDlg('Invalid parameters', Mess, mtError, [mbOK], 0);              // invalid user input values for actual calculation
  end
  else
    MessageDlg('Invalid parameters', 'No calculation selected!', mtError, [mbOK], 0);
end;

{ Checking of one of the radiobuttons = Selection of corr. calculation (and focus corr. first input field) }

procedure TfCircles.rbTangentChange(Sender: TObject);

begin
  if rbTangent.Checked then begin
    sCalculation := 'tangent';
    edFx.SetFocus;
  end;
end;

procedure TfCircles.rbIntLineChange(Sender: TObject);

begin
  if rbIntLine.Checked then begin
    sCalculation := 'line';
    if edLineM.Visible then
      edLineM.SetFocus
    else
      edLineC.SetFocus;
  end;
end;

procedure TfCircles.rbIntCircleChange(Sender: TObject);

begin
  if rbIntCircle.Checked then begin
    sCalculation := 'circle';
    edIntCircleCx.SetFocus;
  end;
end;

procedure TfCircles.rbShapesChange(Sender: TObject);

begin
  if rbShapes.Checked then begin
    sCalculation := 'shapes';
    cbSquareOut.SetFocus;
  end;
end;

{ Change of one of the parameter edit fields' content: Automatically select the corr. calculation (user convenience) }

procedure TfCircles.edFxChange(Sender: TObject);

begin
  if edFx.Text <> '' then
    rbTangent.Checked := True;
end;

procedure TfCircles.edFyChange(Sender: TObject);

begin
  if edFy.Text <> '' then
    rbTangent.Checked := True;
end;

procedure TfCircles.edLineMChange(Sender: TObject);

begin
  if edLineM.Text <> '' then
    rbIntLine.Checked := True;
end;

procedure TfCircles.edLineCChange(Sender: TObject);

begin
  if edLineC.Text <> '' then
    rbIntLine.Checked := True;
end;

procedure TfCircles.edIntCircleCxChange(Sender: TObject);

begin
  if edIntCircleCx.Text <> '' then
    rbIntCircle.Checked := True;
end;

procedure TfCircles.edIntCircleCyChange(Sender: TObject);

begin
  if edIntCircleCy.Text <> '' then
    rbIntCircle.Checked := True;
end;

procedure TfCircles.edIntCircleRChange(Sender: TObject);

begin
  if edIntCircleR.Text <> '' then
    rbIntCircle.Checked := True;
end;

procedure TfCircles.cbSquareInChange(Sender: TObject);

begin
  if cbSquareIn.Checked then
    rbShapes.Checked := True;
end;

procedure TfCircles.cbSquareOutChange(Sender: TObject);

begin
  if cbSquareOut.Checked then
    rbShapes.Checked := True;
end;

{ Intersection line selection combobox changes: Adapt line parameter fields for that kind of line }

procedure TfCircles.cobLineChange(Sender: TObject);

begin
  if cobLine.ItemIndex = 2 then begin
    laLineM.Visible := True; edLineM.Visible := True;
    laLineC.Top := laFy.Top + 36; edLineC.Top := edFy.Top + 36;
  end
  else begin
    laLineM.Visible := False; edLineM.Visible := False;
    laLineC.Top := laFy.Top; edLineC.Top := edFy.Top;
  end;
  rbIntCircle.Checked := True;
end;

end.

