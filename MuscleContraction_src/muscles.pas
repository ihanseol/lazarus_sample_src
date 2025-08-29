{***********************************************}
{* Main unit for MuscleContraction application *}
{***********************************************}

unit muscles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, help;

type
  TValues = array of Real;
  {***********}
  { TfMuscles }
  {***********}
  TfMuscles = class(TForm)
    mMenu: TMainMenu;
    mModel, mModelHill, mModelKinetic, mModelExit: TMenuItem;
    mSettings, mSettingsCurve, mSettingsCurveV, mSettingsCurveP, mSettingsCurveVP, mSettingsMultCurves: TMenuItem;
    mHelp, mHelpPhysio, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imGraph: TImage;
    laModel: TLabel;
    edEquation: TEdit;
    rbHillFull, rbHillNorm, rbKinetics2, rbKinetics3: TRadioButton;
    laParamA, laParamB, laParamC, laParamK: TLabel;
    laKinetics1, laKinetics2, laKGApp: TLabel;
    laFMax, laVMax, laPMax: TLabel;
    laUKinetics1, laUKinetics2, laUFMax, laUVMax, laUPMax: TLabel;
    edParamA, edParamB, edParamC, edParamK: TEdit;
    edKinetics1, edKinetics2, edKGApp: TEdit;
    edFMax, edVMax, edPMax: TEdit;
    btCompute: TButton;
    btDraw: TButton;
    btClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mModelHillClick(Sender: TObject);
    procedure mModelKineticClick(Sender: TObject);
    procedure mModelExitClick(Sender: TObject);
    procedure mSettingsCurveVClick(Sender: TObject);
    procedure mSettingsCurvePClick(Sender: TObject);
    procedure mSettingsCurveVPClick(Sender: TObject);
    procedure mSettingsMultCurvesClick(Sender: TObject);
    procedure mHelpPhysioClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btComputeClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure rbHillFullChange(Sender: TObject);
    procedure rbHillNormChange(Sender: TObject);
    procedure rbKinetics2Change(Sender: TObject);
    procedure rbKinetics3Change(Sender: TObject);
  private
    iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, iCurve: Integer;
    rFMax, rVMax, rFPMax, rVPMax, rPMax, rGFMax, rGVMax, rGPMax: Real;
    sModel, sLegendX, sLegend1Y, sLegend2Y: string;
    bHillFull, bKinetics2: Boolean;
    aLoad, aVelocity, aPower: TValues;
    Bitmap: TBitmap;
  end;

const
  SUB_DIGITS: array[1..3] of string = (
    #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83
  );
  SUP_1 = #$C2#$B9;
  SUP_Minus = #$E2#$81#$BB;

var
  fMuscles: TfMuscles;

implementation

{$R *.lfm}

{ Clean the graph by displaying a white rectangle }

procedure GraphClear(GraphImg: TImage; W, H: Integer);

begin
  GraphImg.Picture.Bitmap.Canvas.Pen.Width := 1;
  GraphImg.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  GraphImg.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  GraphImg.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Clear the form }

procedure FormClear(var GraphImg: TImage; W, H: Integer);

begin
  // Clear edit fields
  fMuscles.edParamA.Text := ''; fMuscles.edParamB.Text := ''; fMuscles.edParamC.Text := '';
  fMuscles.edKinetics1.Text := ''; fMuscles.edKinetics2.Text := ''; fMuscles.edKGApp.Text := '';
  fMuscles.edParamK.Text := '';
  fMuscles.edFMax.Text := ''; fMuscles.edVMax.Text := ''; fMuscles.edPMax.Text := '';
  // Clear the drawing surface
  GraphClear(GraphImg, W, H);
end;

{ Start a new model }

procedure NewModel(Model: string; var GraphImg: TImage; W, H: Integer);

begin
  if LeftStr(Model, 4) = 'Hill' then begin
    // Hill's muscle equation
    fMuscles.laModel.Caption := 'Hill''s muscle model.';
    fMuscles.rbHillFull.Visible := True; fMuscles.rbHillNorm.Visible := True;
    fMuscles.rbKinetics2.Visible := False; fMuscles.rbKinetics3.Visible := False;
    fMuscles.laParamA.Visible := True; fMuscles.laParamB.Visible := True; fMuscles.laParamC.Visible := True;
    fMuscles.edParamA.Visible := True; fMuscles.edParamB.Visible := True; fMuscles.edParamC.Visible := True;
    fMuscles.laKinetics1.Visible := False; fMuscles.laKinetics2.Visible := False; fMuscles.laKGApp.Visible := False;
    fMuscles.edKinetics1.Visible := False; fMuscles.edKinetics2.Visible := False; fMuscles.edKGApp.Visible := False;
    fMuscles.laUKinetics1.Visible := False; fMuscles.laUKinetics2.Visible := False;
    if Model = 'Hill1' then begin
      // "Full" form of the equation (parameters a, b, c; K calculated)
      fMuscles.edEquation.Text := '(F + a)(V + b) = c';
      fMuscles.laFMax.Caption := StringReplace(fMuscles.laFMax.Caption, 'F''max', 'Fmax', []);
      fMuscles.laVMax.Caption := StringReplace(fMuscles.laVMax.Caption, 'V''max', 'Vmax', []);
      fMuscles.laPMax.Caption := StringReplace(fMuscles.laPMax.Caption, 'P''max', 'Pmax', []);
      fMuscles.edParamA.ReadOnly := False; fMuscles.edParamB.ReadOnly := False; fMuscles.edParamC.ReadOnly := False;
      fMuscles.edParamA.TabStop := True; fMuscles.edParamB.TabStop := True; fMuscles.edParamC.TabStop := True;
      fMuscles.edParamK.ReadOnly := True; fMuscles.edParamK.TabStop := False; fMuscles.edParamK.Text := '';
      fMuscles.edParamA.Color := clDefault; fMuscles.edParamB.Color := clDefault;
      fMuscles.edParamC.Color := clDefault; fMuscles.edParamK.Color := clCream;
      fMuscles.laUFMax.Visible := True; fMuscles.laUVMax.Visible := True; fMuscles.laUPMax.Visible := True;
      fMuscles.edParamA.SetFocus;
    end
    else begin
      // Normalized form of the equation (unique parameter K)
      fMuscles.edEquation.Text := 'F'' = K(1 - V'') / (K + V'')';
      fMuscles.laFMax.Caption := StringReplace(fMuscles.laFMax.Caption, 'Fmax', 'F''max', []);
      fMuscles.laVMax.Caption := StringReplace(fMuscles.laVMax.Caption, 'Vmax', 'V''max', []);
      fMuscles.laPMax.Caption := StringReplace(fMuscles.laPMax.Caption, 'Pmax', 'P''max', []);
      fMuscles.edParamA.ReadOnly := True; fMuscles.edParamB.ReadOnly := True; fMuscles.edParamC.ReadOnly := True;
      fMuscles.edParamA.TabStop := False; fMuscles.edParamB.TabStop := False; fMuscles.edParamC.TabStop := False;
      fMuscles.edParamA.Text := ''; fMuscles.edParamB.Text := ''; fMuscles.edParamC.Text := '';
      fMuscles.edParamK.ReadOnly := False; fMuscles.edParamK.TabStop := True;
      fMuscles.edParamA.Color := clCream; fMuscles.edParamB.Color := clCream;
      fMuscles.edParamC.Color := clCream; fMuscles.edParamK.Color := clDefault;
      fMuscles.laUFMax.Visible := False; fMuscles.laUVMax.Visible := False; fMuscles.laUPMax.Visible := False;
      fMuscles.edParamK.SetFocus;
    end;
  end
  else begin
    // Cross-bridge kinetics model
    fMuscles.laModel.Caption := 'Cross-bridge kinetics model.';
    fMuscles.rbHillFull.Visible := False; fMuscles.rbHillNorm.Visible := False;
    fMuscles.rbKinetics2.Visible := True; fMuscles.rbKinetics3.Visible := True;
    fMuscles.laParamA.Visible := False; fMuscles.laParamB.Visible := False; fMuscles.laParamC.Visible := False;
    fMuscles.edParamA.Visible := False; fMuscles.edParamB.Visible := False; fMuscles.edParamC.Visible := False;
    fMuscles.laKinetics1.Visible := True; fMuscles.edKinetics1.Visible := True; fMuscles.laUKinetics1.Visible := True;
    fMuscles.edKGApp.Visible := True; fMuscles.laKGApp.Visible := True;
    if Model = 'Kinetics1' then begin
      // 2-state kinetics model
      fMuscles.edEquation.Text := 'F'' = (1 - V'') ∙ [fApp / (kV'' + fApp)]';
      fMuscles.laKinetics1.Caption := 'Attachment rate fApp';
      fMuscles.laKinetics2.Visible := False; fMuscles.edKinetics2.Visible := False; fMuscles.laUKinetics2.Visible := False;
      fMuscles.laKGApp.Top := fMuscles.laParamB.Top; fMuscles.edKGApp.Top := fMuscles.edParamB.Top;
    end
    else begin
      // 3-state kinetics model
      fMuscles.edEquation.Text := 'F'' = (1 - V'') ∙ [c' + SUB_DIGITS[1] + SUB_DIGITS[2] + 'c' + SUB_DIGITS[2] + SUB_DIGITS[3];
      fMuscles.laKinetics1.Caption := 'S' + SUB_DIGITS[1] + ' → S' + SUB_DIGITS[2] + ' transition rate c' + SUB_DIGITS[1] + SUB_DIGITS[2];
      fMuscles.edEquation.Text := fMuscles.edEquation.Text + ' / (kV'' + c' + SUB_DIGITS[1] + SUB_DIGITS[2] + 'c' + SUB_DIGITS[2] + SUB_DIGITS[3] + ')]';
      fMuscles.laKinetics2.Visible := True; fMuscles.edKinetics2.Visible := True; fMuscles.laUKinetics2.Visible := True;
      fMuscles.laKGApp.Top := fMuscles.laParamC.Top; fMuscles.edKGApp.Top := fMuscles.edParamC.Top;
    end;
    fMuscles.laFMax.Caption := StringReplace(fMuscles.laFMax.Caption, 'Fmax', 'F''max', []);
    fMuscles.laVMax.Caption := StringReplace(fMuscles.laVMax.Caption, 'Vmax', 'V''max', []);
    fMuscles.laPMax.Caption := StringReplace(fMuscles.laPMax.Caption, 'Pmax', 'P''max', []);
    fMuscles.edParamK.ReadOnly := True; fMuscles.edParamK.TabStop := False; fMuscles.edParamK.Color := clCream;
    fMuscles.laUFMax.Visible := False; fMuscles.laUVMax.Visible := False; fMuscles.laUPMax.Visible := False;
    fMuscles.edKinetics1.SetFocus;
  end;
  fMuscles.edFMax.Text := ''; fMuscles.edVMax.Text := ''; fMuscles.edPMax.Text := '';
  FormClear(GraphImg, W, H);
  fMuscles.btDraw.Enabled := False;                                            // disable "Draw" button, until "Compute" has been pushed
end;

{ Read user parameters for Hill's muscle equation model from form }

procedure FormReadHill(Model: string; out A, B, C, K: Real; out Mess: string);

begin
  Mess := ''; A := 0; B := 0; C := 0; K := 0;
  if Model = 'Hill1' then begin
    // "Full" form of Hill's equation (a, b, c must have been entered)
    if fMuscles.edParamA.Text <> '' then
      A := StrToFloat(fMuscles.edParamA.Text);
    if A <= 0 then begin
      Mess := 'Parameter a must be greater than 0';
      fMuscles.edParamA.SetFocus;
    end;
    if Mess = '' then begin
      if fMuscles.edParamB.Text <> '' then
        B := StrToFloat(fMuscles.edParamB.Text);
      if B <= 0 then begin
        Mess := 'Parameter b must be greater than 0';
        fMuscles.edParamB.SetFocus;
      end;
    end;
    if Mess = '' then begin
      if fMuscles.edParamC.Text <> '' then
        C := StrToFloat(fMuscles.edParamC.Text);
      if C <= 0 then begin
        Mess := 'Parameter c must be greater than 0';
        fMuscles.edParamC.SetFocus;
      end;
    end;
  end
  else begin
    // Normalized form of equation (K must have been entered)
    if fMuscles.edParamK.Text <> '' then
      K := StrToFloat(fMuscles.edParamK.Text);
    if K <= 0 then begin
      Mess := 'Constant K must be greater than 0';
      fMuscles.edParamK.SetFocus;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Invalid parameters', Mess + '!', mtError, [mbOK], 0);
end;

{ Read user parameters for cross-bridge kinetics model from form }

procedure FormReadKinetics(Model: string; out FApp, GAppK: Real; out Mess: string);

var
  C12, C23: Real;

begin
  Mess := ''; FApp := 0; C12 := 0; C23 := 0; GAppK := 0;
  if Model = 'Kinetics1' then begin
    if fMuscles.edKinetics1.Text <> '' then
      FApp := StrToFloat(fMuscles.edKinetics1.Text);
    if FApp <= 0 then begin
      Mess := 'Attachment rate fApp must be greater than 0';
      fMuscles.edKinetics1.SetFocus;
    end;
  end
  else begin
    if fMuscles.edKinetics1.Text <> '' then
      C12 := StrToFloat(fMuscles.edKinetics1.Text);
    if fMuscles.edKinetics2.Text <> '' then
      C23 := StrToFloat(fMuscles.edKinetics1.Text);
    fApp := C12 * C23;
    if C12 <= 0 then begin
      Mess := 'S' + SUB_DIGITS[1] + ' → S' + SUB_DIGITS[2] + ' transition rate must be greater than 0';
      fMuscles.edKinetics1.SetFocus;
    end
    else if C23 <= 0 then begin
      Mess := 'S' + SUB_DIGITS[2] + ' → S' + SUB_DIGITS[3] + ' transition rate must be greater than 0';
      fMuscles.edKinetics2.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if fMuscles.edKGApp.Text <> '' then
      GAppK := StrToFloat(fMuscles.edKGApp.Text);
    if GAppK <= 0 then begin
      Mess := 'Detachment rate constant must be greater than 0';
      fMuscles.edKGApp.SetFocus;
    end;
  end;
  if Mess <> '' then
    MessageDlg('Invalid parameters', Mess + '!', mtError, [mbOK], 0);
end;

{ Determine muscle load / shortening velocity values (Hill's model) }

procedure VelocityCalcHill(Model: string; A, B, C, K: Real; out FMax, VMax: Real; out Load, Velocity: TValues);

const
  NValues = 1000;

var
  I: Integer;

begin
  SetLength(Load, NValues); SetLength(Velocity, NValues);
  if Model = 'Hill1' then begin
    // "Full" equation
    FMax := (C - A * B) / B;
    VMax := (C - A * B) / A;
    I := 0;
    while I < NValues do begin
      Load[I] := I * FMax / NValues;
      Velocity[I] := C / (Load[I] + A) - B;
      Inc(I);
    end;
  end
  else begin
    // Normalized equation
    FMax := 1; VMax := 1;
    I := 0;
    while I < NValues do begin
      Load[I] := I * FMax / NValues;
      Velocity[I] := K * (1 - Load[I]) / (Load[I] + K);
      Inc(I);
    end;
  end;
  fMuscles.edFMax.Text := FloatToStrF(FMax, ffFixed, 0, 3);
  fMuscles.edVMax.Text := FloatToStrF(VMax, ffFixed, 0, 3);
end;

{ Determine muscle load / shortening velocity values (Kinetics model) }

procedure VelocityCalcKinetics(FApp, GAppK: Real; out FMax, VMax: Real; out Load, Velocity: TValues);

// For the 3-state kinetics model: fApp = c12∙c23 (calculated before)

const
  NValues = 1000;

var
  I: Integer;

begin
  SetLength(Load, NValues); SetLength(Velocity, NValues);
  FMax := 1; VMax := 1;
  I := 0;
  while I < NValues do begin
    Load[I] := I * FMax / NValues;
    Velocity[I] := (FApp - FApp * Load[I]) / (Load[I] * GAppK + FApp);
    Inc(I);
  end;
  fMuscles.edFMax.Text := FloatToStrF(FMax, ffFixed, 0, 3);
  fMuscles.edVMax.Text := FloatToStrF(VMax, ffFixed, 0, 3);
end;

{ Determine muscle output power for load values (and corr. velocity) }

procedure PowerCalc(var Load, Velocity: TValues; out FPMax, VPMax, PMax: Real; out Power: TValues);

var
  I: Integer;

begin
  SetLength(Power, Length(Load)); FPMax := 0; VPMax := 0; PMax := 0;
  for I := 0 to Length(Load) - 1 do begin
    Power[I] := Load[I] * Velocity[I];
    if Power[I] > PMax then begin
      PMax := Power[I];                                                        // maximum muscle output power Pmax
      FPMax := Load[I];                                                        // muscle tension for P = Pmax
      VPMax := Velocity[I];                                                    // shortening velocity for P = Pmax
    end;
  end;
  fMuscles.edPMax.Text := FloatToStrF(PMax, ffFixed, 0, 3);
end;

{ Draw graph axes }

procedure DrawAxes(GraphImg: TImage; Graph: string; XL, XR, YT, YB: Integer; LegendX, Legend1Y, Legend2Y: string);

begin
  GraphImg.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  GraphImg.Picture.Bitmap.Canvas.Pen.Width := 1;
  GraphImg.Picture.Bitmap.Canvas.Line(XL - 35, YB, XR + 60, YB);               // draw X-axis
  GraphImg.Picture.Bitmap.Canvas.Line(XL, YT - 35, XL, YB + 25);               // draw Y-axis
  if Graph = 'VP' then
    GraphImg.Picture.Bitmap.Canvas.Line(XR + 20 , YT - 35, XR + 20, YB + 25);  // draw second Y-axis
  GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlack;
  GraphImg.Picture.Bitmap.Canvas.TextOut(XR + 26, YB + 8, LegendX);            // display X-axis legend
  if Graph = 'V' then begin
    GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlue;
    GraphImg.Picture.Bitmap.Canvas.TextOut(6, 6, Legend1Y);                    // display Y-axis legend (velocity)
  end
  else if Graph = 'P' then begin
    GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
    GraphImg.Picture.Bitmap.Canvas.TextOut(6, 6, Legend1Y);                    // display Y-axis legend (power)
  end
  else begin
    GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlue;
    GraphImg.Picture.Bitmap.Canvas.TextOut(6, 6, Legend1Y);                    // display 1st Y-axis legend (velocity)
    GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
    GraphImg.Picture.Bitmap.Canvas.TextOut(XR + 26, 6, Legend2Y);              // display 2nd Y-axis legend (power)
  end;
end;

{ Draw velocity resp. power vs muscle load graph }

procedure DrawGraph(GraphImg: TImage; Graph: string; XL, XR, YT, YB: Integer;
  K: string; FMax, VMax, FPMax, VPMax, PMax: Real; Curve: Integer; var Load, Velocity, Power: TValues; var GFMax, GVMax, GPMax: Real);

const
  Colors1: array[0..5] of TColor = (
    clBlue, clGreen, $32CD32, clBlue, $800080, clFuchsia
  );
  Colors2: array[0..5] of TColor = (
    clRed, $008CFF, $B469FF, clRed, $1E69D2, $000080
  );

var
  X, Y, YVP, YMax, T, I: Integer;

begin
  // Determine max. load, velocity, power value to appear on the graph
  // This has to be done, if multiple curves is off (Curve = 0) and for the first curve if multiple curves = on (Curve = 1)
  // For subsequent curves in multiple curves mode (Curve > 1), the value calculated for the first curve has to be used
  // in order to get a correct display (note: 1st curve considered having the highest maximum values)!
  if (Curve = 0) or (Curve = 1) then begin
    // Normalized values. The only maximum, that needs to be calculated is GPmax.
    // Use 0.1 (common value for realistic parameters), with 0.01 added until GPmax >= as actual PMax
    if FMax = 1 then begin
      GFMax := 1; GVMax := 1;
      GPMax := 0.1;
      while GPMax < PMax do
        GPMax += 0.01;
    end
    // Non-normalized values. Use power of 10 for load and power, unit value for velocity;
    // with GFMax, GVMax and GPMax > FMax, VMax and PMax respectively (max. of curve being somewhat under last axis tick)
    else begin
      GFMax := (Int(FMax / 10) + 1) * 10; GVMax := Int(VMax) + 1; GPMax := (Int(PMax / 10) + 1) * 10;
    end;
  end;
  GraphImg.Picture.Bitmap.Canvas.Pen.Width := 2;
  // Draw velocity vs tension graph
  if (Graph = 'V') or (Graph = 'VP') then begin
    GraphImg.Picture.Bitmap.Canvas.Pen.Color := Colors1[Curve];
    for I := 0 to Length(Load) - 1 do begin
      X := Round((Load[I] / GFMax) * (XR - XL));
      Y := YB - Round((Velocity[I] / GVMax) * (YB - YT));
      if Velocity[I] = VPMax then
        YVP := Y;                                                              // Y value on the velocity graph for P = PMax (see below)
      if I = 0 then
        GraphImg.Picture.Bitmap.Canvas.MoveTo(XL + X, Y)                       // first curve point
      else
        GraphImg.Picture.Bitmap.Canvas.LineTo(XL + X, Y);                      // other curve points
    end;
  end;
  // Draw power vs tension graph
  if (Graph = 'P') or (Graph = 'VP') then begin
    // Determine size of power graph, i.e. the area Y-position, corr. to PMax
    if Graph = 'P' then begin
      // If only the power curve is to be drwan, use full height of display area
      YMax := YB - YT;
    end
    else begin
      // If power and velocity curves are to be drawn, place PMax at the same height as the
      // velocity, for which P = PMax (this is actually the YVP value, determined above)
      // This little trick allows to correctly show not only the load, but also the velocity, that generate PMax
      YMax := YVP;
    end;
    GraphImg.Picture.Bitmap.Canvas.Pen.Color := Colors2[Curve];
    for I := 0 to Length(Load) - 1 do begin
      X := Round((Load[I] / GFMax) * (XR - XL));
      Y := YB - Round((Power[I] / GPMax) * YMax);
      if I = 0 then
        GraphImg.Picture.Bitmap.Canvas.MoveTo(XL + X, Y)
      else
        GraphImg.Picture.Bitmap.Canvas.LineTo(XL + X, Y);
    end;
    if Graph = 'VP' then begin
      // For graph with velocity and power, draw horizontal and vertical lines, showing
      // the load and velocity generating PMax (and mark PMax on the graph)
      GraphImg.Picture.Bitmap.Canvas.Pen.Color := clRed;
      GraphImg.Picture.Bitmap.Canvas.Pen.Width := 1;
      X := Round((FPMax / GFMax) * (XR - XL));
      Y := YB - Round((PMax / GPMax) * YMax);
      GraphImg.Picture.Bitmap.Canvas.Line(XL, Y, XL + X, Y);
      GraphImg.Picture.Bitmap.Canvas.Line(XL + X, YB, XL + X, Y);
      GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
      GraphImg.Picture.Bitmap.Canvas.TextOut(XL + X - 2, Y - 30, '↓');
      GraphImg.Picture.Bitmap.Canvas.TextOut(XL + X - 15, Y - 50, 'Pmax');
    end;
  end;
  GraphImg.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  GraphImg.Picture.Bitmap.Canvas.Pen.Width := 1;
  // Draw axis ticks and display corr. values
  if (Curve = 0) or (Curve = 1) then begin
    // Normalized values
    if FMax = 1 then begin
      // X-axis: 0, 0.1, 0.2 ... 1.0
      for I := 1 to 10 do begin
        X := Round((I / 10) * (XR - XL));
        GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlack;
        GraphImg.Picture.Bitmap.Canvas.Line(XL + X, YB - 5, XL + X, YB + 5);
        GraphImg.Picture.Bitmap.Canvas.TextOut(XL + X - 11, YB + 8, FloatToStrF((I / 10), ffFixed, 0, 1));
      end;
      if (Graph = 'V') or (Graph = 'VP') then begin
        // Velocity Y-axis: 0, 0.1, 0.2 ... 1.0
        for I := 1 to 10 do begin
          Y := YB - Round((I / 10) * (YB - YT));
          GraphImg.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
          GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlue;
          GraphImg.Picture.Bitmap.Canvas.TextOut(XL - 35, Y - 11, FloatToStrF((I / 10), ffFixed, 0, 1));
        end;
      end
      else begin
        // Power Y-axis (left): 0, 0.01, 0.02 ... 0.10 ...
        T := Round(GPMax * 100);
        for I := 1 to T do begin
          Y := YB - Round((I / T) * YMax);
          GraphImg.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
          GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
          GraphImg.Picture.Bitmap.Canvas.TextOut(XL - 35, Y - 11, FloatToStrF((I / 100), ffFixed, 0, 2));
        end;
      end;
      if Graph = 'VP' then begin
        // Power Y-axis (right): 0, 0.01, 0.02 ... 0.10 ...
        T := Round(GPMax * 100);
        for I := 1 to T do begin
          Y := YB - Round((I / T) * YMax);
          GraphImg.Picture.Bitmap.Canvas.Line(XR + 15, Y, XR + 25, Y);
          GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
          GraphImg.Picture.Bitmap.Canvas.TextOut(XR + 35, Y - 11, FloatToStrF((I / 100), ffFixed, 0, 2));
        end;
      end;
    end
    // Non-normalized values
    else begin
      // X-axis: 0, 10, 20 ...
      for I := 1 to Round(GFMax / 10) do begin
        X := Round((10 * I / GFMax) * (XR - XL));
        GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlack;
        GraphImg.Picture.Bitmap.Canvas.Line(XL + X, YB - 5, XL + X, YB + 5);
        GraphImg.Picture.Bitmap.Canvas.TextOut(XL + X - 10, YB + 8, IntToStr(10 * I));
      end;
      if (Graph = 'V') or (Graph = 'VP') then begin
        // Velocity Y-axis: 0, 1, 2 ...
        for I := 1 to Round(GVMax) do begin
          Y := YB - Round((I / GVMax) * (YB - YT));
          GraphImg.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
          GraphImg.Picture.Bitmap.Canvas.Font.Color := clBlue;
          GraphImg.Picture.Bitmap.Canvas.TextOut(XL - 25, Y - 11, IntToStr(I));
        end;
      end
      else begin
        // Power Y-axis (left): 0, 10, 20 ...
        T := Round(GPMax / 10);
        for I := 1 to T do begin
          Y := YB - Round((10 * I / GPMax) * YMax);
          GraphImg.Picture.Bitmap.Canvas.Line(XL - 5, Y, XL + 5, Y);
          GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
          GraphImg.Picture.Bitmap.Canvas.TextOut(XL - 35, Y - 10, IntToStr(10 * I));
        end;
      end;
      if Graph = 'VP' then begin
        // Power Y-axis (right): 0, 10, 20 ...
        T := Round(GPMax / 10);
        for I := 1 to T do begin
          Y := YB - Round((10 * I / GPMax) * YMax);
          GraphImg.Picture.Bitmap.Canvas.Line(XR + 15, Y, XR + 25, Y);
          GraphImg.Picture.Bitmap.Canvas.Font.Color := clRed;
          GraphImg.Picture.Bitmap.Canvas.TextOut(XR + 35, Y - 11, IntToStr(10 * I));
        end;
      end;
    end;
  end;
  // Multiple curves graph: Display K values for the different curves (same color as curve)
  if fMuscles.mSettingsMultCurves.Checked then begin
    GraphImg.Picture.Bitmap.Canvas.Font.Style := [fsBold];
    if Graph = 'V' then
      GraphImg.Picture.Bitmap.Canvas.Font.Color := Colors1[Curve]
    else
      GraphImg.Picture.Bitmap.Canvas.Font.Color := Colors2[Curve];
    GraphImg.Picture.Bitmap.Canvas.TextOut(XR, YT - 35 + (Curve - 1) * 20, 'K = ' + K);
    GraphImg.Picture.Bitmap.Canvas.Font.Style := [];
  end;
end;

{***********}
{ TfMuscles }
{***********}

{ Application start: Initialisation }

procedure TfMuscles.FormCreate(Sender: TObject);

begin
  // Graph area
  iImageWidth := imGraph.Width; iImageHeight := imGraph.Height;
  iGraphLeft  := 65; iGraphRight  := iImageWidth - 100;
  iGraphTop   := 65; iGraphBottom := iImageHeight - 35;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width := iImageWidth;
  Bitmap.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imGraph.Picture.Graphic := Bitmap;
  // Move kinetics model fields to correct position
  rbKinetics2.Top := rbHillFull.Top; rbKinetics3.Top := rbHillNorm.Top;
  laKinetics1.Top := laParamA.Top; edKinetics1.Top := edParamA.Top; laUKinetics1.Top := laParamA.Top;
  laKinetics2.Top := laParamB.Top; edKinetics2.Top := edParamB.Top; laUKinetics2.Top := laParamB.Top;
  // Apply subscripts and superscripts
  laKinetics2.Caption := StringReplace(laKinetics2.Caption, 'S2', 'S' + SUB_DIGITS[2], []);
  laKinetics2.Caption := StringReplace(laKinetics2.Caption, 'S3', 'S' + SUB_DIGITS[3], []);
  laKinetics2.Caption := StringReplace(laKinetics2.Caption, 'c23', 'c' + SUB_DIGITS[2] + SUB_DIGITS[3], []);
  laUKinetics1.Caption := StringReplace(laUKinetics1.Caption, '-1', SUP_Minus + SUP_1, []);
  laUKinetics2.Caption := StringReplace(laUKinetics2.Caption, '-1', SUP_Minus + SUP_1, []);
  // Initial parameters (Hill, "full" equation)
  sModel := 'Hill1'; iCurve := -1;
  bHillFull := True; bKinetics2 := True;                                       // use these two variables to store radiobuttons state
  FormClear(imGraph, imGraph.Width, imGraph.Height);
end;

{ Menu item "Model > Hill's muscle model": Start new calculations, using Hill's muscle model }

procedure TfMuscles.mModelHillClick(Sender: TObject);

begin
  rbHillFull.Checked := False; rbHillNorm.Checked := False;
  rbKinetics2.Checked := False; rbKinetics3.Checked := False;
  if bHillFull then begin
    rbHillFull.Checked := True;
    sModel := 'Hill1';
  end
  else begin
    rbHillNorm.Checked := True;
    sModel := 'Hill2';
  end;
  NewModel(sModel, imGraph, iImageWidth, iImageHeight); iCurve := -1;
end;

{ Menu item "Model > Cross-bridge kinetics model": Start new calculations, using cross-bridge kinetics model }

procedure TfMuscles.mModelKineticClick(Sender: TObject);

begin
  rbHillFull.Checked := False; rbHillNorm.Checked := False;
  rbKinetics2.Checked := False; rbKinetics3.Checked := False;
  if bKinetics2 then begin
    rbKinetics2.Checked :=  True;
    sModel := 'Kinetics1';
  end
  else begin
    rbKinetics3.Checked :=  True;
    sModel := 'Kinetics2';
  end;
  NewModel(sModel, imGraph, iImageWidth, iImageHeight); iCurve := -1;
end;

{ Menu item "Model > Exit": Exit application }

procedure TfMuscles.mModelExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > Curve to be drawn": Select curve to be drawn: velocity, power or both }

procedure TfMuscles.mSettingsCurveVClick(Sender: TObject);

begin
  mSettingsCurveV.Checked := True; mSettingsCurveP.Checked := False; mSettingsCurveVP.Checked := False;
  mSettingsMultCurves.Enabled := True; iCurve := -1;
end;

procedure TfMuscles.mSettingsCurvePClick(Sender: TObject);

begin
  mSettingsCurveV.Checked := False; mSettingsCurveP.Checked := True; mSettingsCurveVP.Checked := False;
  mSettingsMultCurves.Enabled := True; iCurve := -1;
end;

procedure TfMuscles.mSettingsCurveVPClick(Sender: TObject);

begin
  mSettingsCurveV.Checked := False; mSettingsCurveP.Checked := False; mSettingsCurveVP.Checked := True;
  mSettingsMultCurves.Checked := False; mSettingsMultCurves.Enabled := False; iCurve := -1;
end;

{ Menu item "Settings > Multiple curves graph": Toggle display multiple curves or not }

procedure TfMuscles.mSettingsMultCurvesClick(Sender: TObject);

begin
  if mSettingsMultCurves.Checked then
    mSettingsMultCurves.Checked := False
  else
    mSettingsMultCurves.Checked := True;
  iCurve := -1;
end;

{ Menu item "Help > Physiology help": Display muscle contraction help text }

procedure TfMuscles.mHelpPhysioClick(Sender: TObject);

var
  I, J: Integer;

begin
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('physio.txt');
  // Apply subscripts
  for I := 1 to 3 do begin
    fHelp.edHelp.Text := StringReplace(fHelp.edHelp.Text, 'S' + IntToStr(I), 'S' + SUB_DIGITS[I], [rfReplaceAll]);
    for J := 1 to 3 do begin
      fHelp.edHelp.Text := StringReplace(fHelp.edHelp.Text, 'c' + IntToStr(I) + IntToStr(J), 'c' + SUB_DIGITS[I] + SUB_Digits[J], [rfReplaceAll]);
    end;
  end;
  fHelp.Show;
end;

{ Menu item "Help > Application help": Display application help text }

procedure TfMuscles.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Lines.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfMuscles.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physiology:' + LineEnding;
  S += 'Tetanized muscle contraction graphs using Hill’s equation of muscle performance ';
  S += 'resp. two- or three-state cross-bridge kinetics models.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2021.';
  MessageDlg('About "MuscleContraction"', S, mtInformation, [mbOK], 0);
end;

{ Button "Compute": Compute curve points and maxima }

procedure TfMuscles.btComputeClick(Sender: TObject);

var
  A, B, C, FApp, GAppK, K: Real;
  Mess: string;

begin
  if iCurve = -1 then begin
    // The variable iCurve set to -1 means that the drawing area has to be cleared
    GraphClear(imGraph, iImageWidth, iImageHeight);
    if mSettingsMultCurves.Checked then begin
      // First curve of multiple curves set
      iCurve := 1;
    end
    else begin
      // Single curve(s)
      iCurve := 0;
    end;
  end;
  // Hill's muscle model
  if LeftStr(sModel, 4) = 'Hill' then begin
    FormReadHill(sModel, A, B, C, K, Mess);
    if Mess = '' then begin
      VelocityCalcHill(sModel, A, B, C, K, rFMax, rVMax, aLoad, aVelocity);
      if rbHillFull.Checked then begin
        // K being calculated from a, b and c
        K := A / rFMax; edParamK.Text := FloatToStrF(K, ffFixed, 0, 3);
      end;
      PowerCalc(aLoad, aVelocity, rFPMax, rVPMax, rPMax, aPower);
      btDraw.Enabled := True;                                                  // now the user can draw the curve
    end;
  end
  // Kinetics model
  else begin
    FormReadKinetics(sModel, FApp, GAppK, Mess);
    if Mess = '' then begin
      VelocityCalcKinetics(FApp, GAppK, rFMax, rVMax, aLoad, aVelocity);
      K := FApp / GAppK; edParamK.Text := FloatToStrF(K, ffFixed, 0, 3);
      PowerCalc(aLoad, aVelocity, rFPMax, rVPMax, rPMax, aPower);
      btDraw.Enabled := True;                                                  // now the user can draw the curve
    end;
  end;
end;

{ Button "Draw": Draw the curve(s) }

procedure TfMuscles.btDrawClick(Sender: TObject);

var
  Graph: string;

begin
  // Graph legends
  sLegendX := 'F [g]';
  if mSettingsCurveV.Checked then begin
    Graph := 'V'; sLegend1Y := 'V [cm/s]'; sLegend2Y := '';
  end
  else if mSettingsCurveP.Checked then begin
    Graph := 'P'; sLegend1Y := 'P (cm·g/s)'; sLegend2Y := '';
  end
  else begin
    Graph := 'VP'; sLegend1Y := 'V [cm/s]'; sLegend2Y := 'P (cm·g/s)';
  end;
  // Draw the axes and the curve(s)
  DrawAxes(imGraph, Graph, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, sLegendX, sLegend1Y, sLegend2Y);
  DrawGraph(imGraph, Graph, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, edParamK.Text,
    rFMax, rVMax, rFPMax, rVPMax, rPMax, iCurve, aLoad, aVelocity, aPower, rGFMax, rGVMax, rGPMax);
  // Prepare for next drawing
  if mSettingsMultCurves.Checked then begin
    // Multiple curves graph: Draw another curve (max. 5)
    Inc(iCurve);
    if iCurve = 6 then
      iCurve := -1;
  end
  else begin
    // Single curve graph: Reset iCurve (will clear the drawing surface)
    iCurve := -1;
  end;
  btDraw.Enabled := False;                                                     // force user to do recalculation before drawing again
end;

{ Button "Clear": Manually clear drawing surface }

procedure TfMuscles.btClearClick(Sender: TObject);

begin
  GraphClear(imGraph, iImageWidth, iImageHeight);
  iCurve := -1;
end;

{ "Submodel" radiobuttons changes: Start new calculation }

procedure TfMuscles.rbHillFullChange(Sender: TObject);

begin
  if rbHillFull.Checked then begin
    sModel := 'Hill1';
    NewModel(sModel, imGraph, iImageWidth, iImageHeight);
    bHillFull := True;                                                         // remember radiobutton status for Hill's model
  end;
  iCurve := -1;
end;

procedure TfMuscles.rbHillNormChange(Sender: TObject);

begin
  if rbHillNorm.Checked then begin
    sModel := 'Hill2';
    NewModel(sModel, imGraph, iImageWidth, iImageHeight);
    bHillFull := False;                                                        // remember radiobutton status for Hill's model
  end;
  iCurve := -1;
end;

procedure TfMuscles.rbKinetics2Change(Sender: TObject);

begin
  if rbKinetics2.Checked then begin
    sModel := 'Kinetics1';
    NewModel(sModel, imGraph, iImageWidth, iImageHeight);
    bKinetics2 := True;                                                        // remember radiobutton status for kinetics model
  end;
  iCurve := -1;
end;

procedure TfMuscles.rbKinetics3Change(Sender: TObject);

begin
  if rbKinetics3.Checked then begin
    sModel := 'Kinetics2';
    NewModel(sModel, imGraph, iImageWidth, iImageHeight);
    bKinetics2 := False;                                                       // remember radiobutton status for kinetics model
  end;
  iCurve := -1;
end;

end.

