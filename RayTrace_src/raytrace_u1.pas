{**************************************}
{* Main unit for RayTrace application *}
{**************************************}

unit raytrace_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, PopupNotifier, raytrace_u2;

type
  { TfOptics }
  TfOptics = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsSimple, mSettingsWarning: TMenuItem;
    mHelp, mHelpOptics, mHelpProgram, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    cbDevice: TComboBox;
    laRadius1: TLabel;
    laRadius2: TLabel;
    laRefraction: TLabel;
    laObjHeight: TLabel;
    laObjDistance: TLabel;
    edRadius1: TEdit;
    edRadius2: TEdit;
    edRefraction: TEdit;
    edObjHeight: TEdit;
    edObjDistance: TEdit;
    imOptics: TImage;
    imEyeLeft: TImage;
    imEyeRight: TImage;
    edCase: TEdit;
    meRays: TMemo;
    laDevice: TLabel;
    laDeviceType: TLabel;
    laStrength: TLabel;
    laImage: TLabel;
    edDeviceType: TEdit;
    edFocus: TEdit;
    edStrength: TEdit;
    meImType: TMemo;
    edImHeight: TEdit;
    edImDistance: TEdit;
    edMagnification: TEdit;
    btTrace: TButton;
    pnAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsSimpleClick(Sender: TObject);
    procedure mSettingsWarningClick(Sender: TObject);
    procedure mHelpOpticsClick(Sender: TObject);
    procedure mHelpProgramClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btTraceClick(Sender: TObject);
    procedure cbDeviceChange(Sender: TObject);
  private
    Bitmap : TBitmap;
    rR1, rR2, rN, rHO, rDO, rF, rD, rHI, rDI, rM: Real;
  end;

var
  fOptics: TfOptics;

implementation

{$R *.lfm}

{ Clean the draw area by displaying a white rectangle }

procedure DrawareaClean(W, H: Integer);

begin
  fOptics.imOptics.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fOptics.imOptics.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Get equation of line passing through 2 given points }

procedure LineEquation(AX, AY, BX, BY: Real; var M, P: Real);

begin
  if AX = BX then
    M := 0
  else
    M := (BY - AY) / (BX - AX);
  P := AY - M * AX;
end;

{ Get intersection point of 2 given lines }

procedure LinesIntersection(M1, P1, M2, P2: Real; var Intersection: Boolean; var X, Y: Real);

// Line equations: y = m1x + p1 and y = m2x + p2

var
  Det, DetX, DetY: Real;

begin
  Det  := M1 * (-1) - (-1) * M2;
  DetX := -P1 * (-1) - (-1) * (-P2);
  DetY := M1 * (-P2) - (-P1) * M2;
  if Det = 0 then begin
    Intersection := False;
  end
  else begin
    Intersection := True;
    X := DetX / Det; Y := DetY / Det;
  end;
end;

{ Draw an arrow head at the end of a given line segment }

procedure DrawArray(X1, Y1, X2, Y2: Integer; Colour: TColor);

const
  HeadLength = 8;

var
  xBase, yBase: Integer;
  xLineDelta, yLineDelta, xLineUnitDelta, yLineUnitDelta: Real;
  xNormalDelta, yNormalDelta, xNormalUnitDelta, yNormalUnitDelta: Real;
  PColour, BColour: TColor;

begin
  // Save current colors
  PColour := fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color;
  BColour := fOptics.imOptics.Picture.Bitmap.Canvas.Brush.Color;
  // Arrow color (as given as argument)
  fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := Colour;
  fOptics.imOptics.Picture.Bitmap.Canvas.Brush.Color := Colour;
  // Calculate the arrow's values
  xLineDelta := x2 - x1;
  yLineDelta := y2 - y1;
  xLineUnitDelta := xLineDelta / SQRT( SQR(xLineDelta) + SQR(yLineDelta) );
  yLineUnitDelta := yLineDelta / SQRt( SQR(xLineDelta) + SQR(yLineDelta) );
  xBase := x2 - ROUND(HeadLength * xLineUnitDelta);                            // (xBase,yBase) is where arrow line is perpendicular to base of triangle
  yBase := y2 - ROUND(HeadLength * yLineUnitDelta);
  xNormalDelta :=  yLineDelta;
  yNormalDelta := -xLineDelta;
  xNormalUnitDelta := xNormalDelta / SQRT( SQR(xNormalDelta) + SQR(yNormalDelta) );
  yNormalUnitDelta := yNormalDelta / SQRt( SQR(xNormalDelta) + SQR(yNormalDelta) );
  // Draw the arrow tip
  fOptics.imOptics.Picture.Bitmap.Canvas.Polygon([Point(x2,y2),
  Point(xBase + ROUND(HeadLength * xNormalUnitDelta), yBase + ROUND(HeadLength * yNormalUnitDelta)),
  Point(xBase - ROUND(HeadLength * xNormalUnitDelta), yBase - ROUND(HeadLength * yNormalUnitDelta)) ]);
  // Restore the colours
  fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := PColour;
  fOptics.imOptics.Picture.Bitmap.Canvas.Brush.Color := BColour;
end;

{************}
{* TfOptics *}
{************}

{ Create bitmap and associate it with the drawing surface picture object }

procedure TfOptics.FormCreate(Sender: TObject);

begin
  Bitmap := TBitmap.Create;
  Bitmap.Width  := imOptics.Width;
  Bitmap.Height := imOptics.Height;
  imOptics.Picture.Graphic := Bitmap;
  DrawareaClean(imOptics.Width, imOptics.Height);
end;

{ Menu item "File > Exit": Exit the application }

procedure TfOptics.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Use simplified lens calculations": Select if simplified calculations should be used for lenses }

procedure TfOptics.mSettingsSimpleClick(Sender: TObject);

begin
  if mSettingsSimple.Checked then begin
    // "Complete" calculation: Calculate focal length by considering the 2 surface radiuses and the refraction index
    mSettingsSimple.Checked := False;
    if LeftStr(cbDevice.Text, 4) = 'Lens' then begin
      laRadius1.Caption := 'Curvature radius 1';
      laRadius2.Visible := True;
      edRadius2.Visible := True;
      laRefraction.Visible := True;
      edRefraction.Visible := True;
    end;
  end
  else begin
    // "Simplified" calculation: Consider focal length = total curvature C / 2 (similar to C = C1 + C2, with C1 = C2 and refraction index = 1.50)
    mSettingsSimple.Checked := True;
    if LeftStr(cbDevice.Text, 4) = 'Lens' then begin
      laRadius1.Caption := 'Total curv. radius';
      laRadius2.Visible := False;
      edRadius2.Visible := False;
      laRefraction.Visible := False;
      edRefraction.Visible := False;
    end;
  end;
end;

{ Menu item "Settings > Show center ray warning": Select if a warning is displayed if the center ray can't be drawn }

procedure TfOptics.mSettingsWarningClick(Sender: TObject);

begin
  if mSettingsWarning.Checked then
    mSettingsWarning.Checked := False
  else
    mSettingsWarning.Checked := True;
end;

{ Menu item "Help > Optics help": Display optics (ray tracing) help  }

procedure TfOptics.mHelpOpticsClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide;
  fHelp.memoHelp.Lines.Clear;
  fHelp.memoHelp.Lines.LoadFromFile('help1.txt');
  fHelp.ShowModal;
end;

{ Menu item "Help > Program help": Display RayTrace program help }

procedure TfOptics.mHelpProgramClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide;
  fHelp.memoHelp.Lines.Clear;
  fHelp.memoHelp.Lines.LoadFromFile('help2.txt');
  fHelp.ShowModal;
end;

{ Menu item "Help > About": Display RayTrace program about }

procedure TfOptics.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnAbout.Visible then
    pnAbout.Hide
  else begin
    S := 'Geometrical optics: Mirrors and thin lenses ray tracing.' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, April-August, 2018';
    pnAbout.Text := S;
    pnAbout.Show;
  end;
end;

{ Button "Trace": Trace the ray for mirror/lens selected }

procedure TfOptics.btTraceClick(Sender: TObject);

var
  X, Y, X1, X2, Y1, Y2, XMax, YMax, OX, OY, IX, IY, FX, F2X, FY, CX, CY, D0X, DX, XStart, XEnd, YM: Integer;
  DTotal, HTotal, DDevice: Real;
  LM, LP, LX, LY: Real;
  RayText, S: string;
  LineStart, Intersection: Boolean;

begin
  // Read user data from form (and automatically adapt the curvature radius sign )
  if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
    laDevice.Caption := 'Mirror'; laDeviceType.Caption := 'Mirror type';
  end
  else begin
    laDevice.Caption := 'Lens'; laDeviceType.Caption := 'Lens type';
  end;
  if edRadius1.Text = '' then
    rR1 := 0
  else
    rR1 := StrToFloat(edRadius1.Text);
  if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
    if RightStr(cbDevice.Text, 9) = '(concave)' then
      rR1 := Abs(rR1)
    else
      rR1 := -Abs(rR1);
  end
  else begin
    if not mSettingsSimple.Checked then begin
      if edRadius2.Text = '' then
        rR2 := 0
      else
        rR2 := StrToFloat(edRadius2.Text);
    end;
    if edRefraction.Text = '' then
      rN := 0
    else
      rN := StrToFloat(edRefraction.Text);
    if RightStr(cbDevice.Text, 9) = '(concave)' then begin
      rR1 := -Abs(rR1);
      if not mSettingsSimple.Checked then
        rR2 := Abs(rR2);
    end
    else begin
      rR1 := Abs(rR1);
      if not mSettingsSimple.Checked then
        rR2 := -Abs(rR2)
    end;
  end;
  edRadius1.Text := FloatToStr(rR1);
  if not mSettingsSimple.Checked then
    edRadius2.Text := FloatToStr(rR2);
  if edObjHeight.Text = '' then
    rHO := 0
  else
    rHO := StrToFloat(edObjHeight.Text);
  if edObjDistance.Text = '' then
    rDO := 0
  else
    rDO := StrToFloat(edObjDistance.Text);
  // Check user data
  S := '';
  if rR1 = 0 then
    S := 'Curvature radius must be greater than 0!'
  else if (S = '') and (LeftStr(cbDevice.Text, 4) = 'Lens') and not mSettingsSimple.Checked then begin
    if rR2 = 0 then
      S := 'Curvature radius 2 must be greater than 0!'
    else if rN <= 1 then
      S := 'Refraction index must be greater than 1!'
    else if (rN < 1.5) or (rN > 2) then
      MessageDlg('Possibly invalid data', 'Refraction index for glass is between 1.5 and 2!', mtWarning, [mbOK], 0);
  end;
  if S = '' then begin
    if rHO <= 0 then
      S := 'Object height must be greater than 0!'
    else if rDO <= 0 then
      S := 'Object distance must be greater than 0!'
  end;
  // If user data ok, proceed with ray tracing
  if S = '' then begin
    // Clear/init some form fields
    fOptics.imOptics.Picture.Bitmap.Canvas.Clear;
    edCase.Text := '';
    meRays.Lines.Clear;
    RayText := 'Ray colors:          blue = P-ray          ';
    if cbDevice.Text <> 'Lens (concave)' then
      RayText += 'cyan = F-ray          ';
    RayText += 'lime = C-ray';
    // Show objects depending on device selected
    if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
      laDeviceType.Caption := 'Mirror type';
      laStrength.Visible := False;
      edStrength.Visible := False;
      imEyeLeft.Visible := True;
      imEyeRight.Visible := False;
    end
    else begin
      laDeviceType.Caption := 'Lens type';
      laStrength.Visible := True;
      edStrength.Visible := True;
      imEyeRight.Visible := True;
      imEyeLeft.Visible := False;
    end;
    // Determine if device is converging or diverging
    if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
      if RightStr(cbDevice.Text, 9) = '(concave)' then
        edDeviceType.Text := 'converging'
      else
        edDeviceType.Text := 'diverging';
    end
    else begin
      if RightStr(cbDevice.Text, 9) = '(concave)' then
        edDeviceType.Text := 'diverging'
      else
        edDeviceType.Text := 'converging';
    end;
    // Calculate focal length
    if LeftStr(cbDevice.Text, 6) = 'Mirror' then
      // Mirror focal length
      rF := rR1 / 2
    else begin
      // Lens focal length (simplified calculation)
      if mSettingsSimple.Checked then begin
        rF := rR1 / 2;
        rR1 /= 2; rR2 := -rR1;
      end
      // Lens focal length ("complete" calculation)
      else
      rF := 1 / ((rN - 1) * (1 / rR1 - 1 / rR2));
    end;
    edFocus.Text := FloatToStr(Int(100 * rF) / 100);
    // Lens strength
    if LeftStr(cbDevice.Text, 4) = 'Lens' then begin
      rD := 1 / rF;
      edStrength.Text := FloatToStr(Int(100 * rD) / 100);
    end;
    // Object distance
    if rDO = rF then
      // If object is placed at distance = focal length: abort (drawing not possible?)
      MessageDlg('Drawing problem', 'Object distance = focal length: Image of infinite size at infinite distannce!', mtError, [mbOK], 0)
      // Proceed with ray tracing if all user data is ok
    else begin
      // Calculate image height, image distance and magnification
      rHI := rF * rHO / (rF - rDO);
      edImHeight.Text := FloatToStr(Int(100 * rHI) / 100);
      rDI := rF * rDO / (rDO - rF);
      edImDistance.Text := FloatToStr(Int(100 * rDI) / 100);
      rM := -rDI / rDO;
      edMagnification.Text := FloatToStr(Int(100 * rM) / 100);
      meImType.Clear;
      // Determine if image is real or virtual
      if rF > 0 then begin
        if rDO > rF then
          S := 'real'
        else
          S := 'virtual';
      end
      else
        S := 'virtual';
      meImType.Lines.AddText(S);
      // Determine if image is upright or inverted
      if rHI > 0 then
        meImType.Lines.AddText('upright')
      else
        meImType.Lines.AddText('inverted');
      // Determine if image is enlarged, reduced or unchanged
      if Abs(rM) > 1 then
        S := 'enlarged'
      else if Abs(rM) < 1 then
        S := 'reduced'
      else
        S := 'unchanged';
      meImType.Lines.AddText(S);
      // Determine image case (cf Optics help text)
      if rF > 0 then begin
        if rDO > rF then
          edCase.Text := 'Case 1 image'
        else
          edCase.Text := 'Case 2 image';
      end
      else
        edCase.Text := 'Case 3 image';
      // Draw axis
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clBlack;
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 1;
      fOptics.imOptics.Picture.Bitmap.Canvas.Line(5, imOptics.Height div 2, imOptics.Width - 5, imOptics.Height div 2);
      // Calculate extrema graph values needed for drawing of rays
      XMax := imOptics.Width - 100; YMax := 150;
      if cbDevice.Text = 'Mirror (concave)' then begin
        // Concave mirror
        if rDO > rR1 then begin
          DTotal := rDO; DDevice := DTotal;
        end
        else if rDO > rF then begin
          DTotal := rDI; DDevice := DTotal;
        end
        else begin
          DTotal := rR1 - rDI; DDevice := rR1;
        end;
      end
      else if cbDevice.Text = 'Mirror (convex)' then begin
        // Convex mirror
        if Abs(rDI) > Abs(rR1) then begin
          DTotal := rDO + Abs(rDI); DDevice := rDO;
        end
        else begin
          DTotal := rDO + Abs(rR1); DDevice := rDO;
        end;
      end
      else if cbDevice.Text = 'Lens (convex)' then begin
        // Convex lens
        if rDO > Abs(rF) then begin
          DTotal := rDO + Abs(rDI); DDevice := rDO;
          rDI := -rDI;
        end
        else begin
          if Abs(rDI) > Abs(rF) then begin
            DTotal := Abs(rDI) + Abs(rF); DDevice := Abs(rDI);
          end
          else begin
            DTotal := 2 * Abs(rF); DDevice := Abs(rF);
          end;
          rDI := -rDI; rF := -rF;
        end;
      end
      else begin
        // Concave lens
        if rDO > Abs(rF) then begin
          DTotal := rDO; DDevice := rDO;
        end
        else begin
          DTotal := Abs(rF); DDevice := Abs(rF);
        end;
        rDI := -rDI; rF := -rF;
        XMax := imOptics.Width - 300;
      end;
      // Graph extremum height value for all 4 devices
      if rHO > Abs(rHI) then
        HTotal := rHO
      else
        HTotal := Abs(rHI);
      // Calculate graph values corresponding to actual ray tracing values
      if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
        CX := Round(XMax * ((DDevice - rR1) / DTotal)) + 50; CY := imOptics.Height div 2;
      end;
      FX := Round(XMax * ((DDevice - rF) / DTotal)) + 50; FY := imOptics.Height div 2;
      if cbDevice.Text = 'Lens (convex)' then
        F2X := Round(XMax * ((DDevice + rF) / DTotal)) + 50;
      OX := Round(XMax * ((DDevice - rDO) / DTotal)) + 50;
      OY := imOptics.Height div 2 - Round(YMax * (rHO / HTotal));
      IX := Round(XMax * ((DDevice - rDI) / DTotal)) + 50;
      IY := imOptics.Height div 2 - Round(YMax * (rHI / HTotal));
      // Draw the device (mirror or lens)
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clSilver;
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 12;
      D0X := Round(XMax * (DDevice / DTotal)) + 50;
      if cbDevice.Text = 'Mirror (concave)' then
        fOptics.imOptics.Picture.Bitmap.Canvas.Arc(D0X - 70, 50, D0X + 10, imOptics.Height - 50, 90*16, -180*16)
      else if cbDevice.Text = 'Mirror (convex)' then
        fOptics.imOptics.Picture.Bitmap.Canvas.Arc(D0X + 70, 50, D0X - 10, imOptics.Height - 50, 90*16, 180*16)
      else if cbDevice.Text = 'Lens (convex)' then begin
        fOptics.imOptics.Picture.Bitmap.Canvas.Arc(D0X - 45, 50, D0X + 35, imOptics.Height - 50, 90*16, 180*16);
        fOptics.imOptics.Picture.Bitmap.Canvas.Arc(D0X - 35, 50, D0X + 45, imOptics.Height - 50, 90*16, -180*16);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(D0X - 8, 50, D0X + 8, 50);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(D0X - 8, imOptics.Height - 50, D0X + 8, imOptics.Height - 50);
      end
      else begin
        D0X -= 1;
        fOptics.imOptics.Picture.Bitmap.Canvas.Arc(D0X - 90, 50, D0X - 10, imOptics.Height - 50, 90*16, -180*16);
        fOptics.imOptics.Picture.Bitmap.Canvas.Arc(D0X + 10, 50, D0X + 95, imOptics.Height - 50, 90*16, 180*16);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(D0X - 45, 50, D0X + 50, 50);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(D0X - 45, imOptics.Height - 50, D0X + 50, imOptics.Height - 50);
      end;
      LM := 0; LP := 0; LX := 0; LY := 0; Intersection := True;
      // Concave mirror ray tracing
      if cbDevice.Text = 'Mirror (concave)' then begin
        // Concave mirror P-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clBlue;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(IX, IY, FX, FY, LM, LP);
        LinesIntersection(LM, LP, 0, OY, Intersection, LX, LY); DX := Round(LX);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(OX, OY, DX, OY);
        LineStart := True;
        for X := DX downto 10 do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            if Y < imOptics.Height - 15 then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
        end;
        DrawArray(X1, Y1, X2, Y2, clBlue);
        LineStart := True;
        if rDO < rF then begin
          for X := DX to IX do begin
            Y := Round(LM * X + LP);
            if X mod 10 < 5 then
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
            else
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
          end;
        end;
        // Concave mirror F-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clAqua;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(OX, OY, FX, FY, LM, LP);
        LinesIntersection(LM, LP, 0, IY, Intersection, LX, LY); DX := Round(LX);
        if rF < 0 then begin
          XStart := OX; XEnd := FX;
        end
        else begin
          XEnd := DX; XStart := OX;
        end;
        LineStart := True;
        for X := XStart to XEnd do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
            X2 := X; Y2 := Y;
          end;
        end;
        LineStart := True;
        if rDO < rF then begin
          for X := DX to IX do begin
            if X mod 10 < 5 then
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, IY)
            else
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, IY);
          end;
        end;
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(10, IY, DX, IY);
        DrawArray(DX, IY, 10, IY, clAqua);
        // Concave mirror C-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clLime;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(OX, OY, CX, CY, LM, LP);
        if not ((Round(LM * D0X + LP) < 60) or (Round(LM * D0X + LP) > imOptics.Height - 60)) and (rDO <> rDI) then begin
          // Draw this ray only if it actually hits the mirror
          XEnd := D0X;
          if (Round(LM * D0X + LP) < 90) or (Round(LM * D0X + LP) > imOptics.Height - 90) then
            // Adapt length of ray drawn for curved ends of mirror (otherwise it goes to far...)
            XEnd := D0X - 14;
          LineStart := True;
          for X := 10 to XEnd do begin
            Y := Round(LM * X + LP);
            if LineStart then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
              X1 := X; Y1 := Y;
              LineStart := False;
            end
            else begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
          DrawArray(X2, Y2, X1, Y1, clLime);
          if rDO < rF then begin
            for X := DX to IX do begin
              Y := Round(LM * X + LP);
              if X mod 10 < 5 then
                fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
              else
                fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
            end;
          end;
        end
        else begin
          // If the ray doesn't hit the mirror, display warrning (if selected so)
          RayText := StringReplace(RayText, 'lime = C-ray', '', []);
          if mSettingsWarning.Checked then
            MessageDlg('Drawing problem', 'Cannot draw a center ray touching the mirror!', mtWarning, [mbOK], 0)
        end;
      end
      // Convex mirror ray tracing
      else if cbDevice.Text = 'Mirror (convex)' then begin
        // Convex mirror P-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clBlue;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(IX, IY, FX, FY, LM, LP);
        LinesIntersection(LM, LP, 0, OY, Intersection, LX, LY); DX := Round(LX);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(OX, OY, DX, OY);
        LineStart := True;
        for X := 10 to DX do begin
          Y := Round(LM * X + LP);
          if X < 80 then
            // Adapt ray length in order not to touch the "left eye"
            YM := 80
          else
            YM := 15;
          if Y > YM then begin
            if LineStart then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
              X1 := X; Y1 := Y;
              LineStart := False;
            end
            else begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
        end;
        DrawArray(X2, Y2, X1, Y1, clBlue);
        for X := DX to FX do begin
          Y := Round(LM * X + LP);
          if X mod 10 < 5 then
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
          else
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
        end;
        // Convex mirror F-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clAqua;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(OX, OY, FX, FY, LM, LP);
        LinesIntersection(LM, LP, 0, IY, Intersection, LX, LY); DX := Round(LX);
        LineStart := True;
        for X := OX to DX do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
            X2 := X; Y2 := Y;
          end;
        end;
        for X := DX to FX do begin
          Y := Round(LM * X + LP);
          if X mod 10 < 5 then
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
          else
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
        end;
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(10, IY, DX, IY);
        DrawArray(DX, IY, 10, IY, clAqua);
        for X := DX to IX do begin
          if X mod 10 < 5 then
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, IY)
          else
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, IY);
        end;
        // Convex mirror C-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clLime;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(OX, OY, CX, CY, LM, LP);
        LineStart := True;
        for X := 10 to D0X do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
            X2 := X; Y2 := Y;
          end;
        end;
        DrawArray(X2, Y2, X1, Y1, clLime);
        for X := D0X to CX do begin
          Y := Round(LM * X + LP);
          if X mod 10 < 5 then
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
          else
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
        end;
      end
      // Convex lens ray tracing
      else if cbDevice.Text = 'Lens (convex)' then begin
        // Convex lens P-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clBlue;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        if rDO > Abs(rF) then
          LineEquation(IX, IY, F2X, FY, LM, LP)
        else
          LineEquation(IX, IY, FX, FY, LM, LP);
        LinesIntersection(LM, LP, 0, OY, Intersection, LX, LY); DX := Round(LX);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(OX, OY, DX, OY);
        LineStart := True;
        for X := DX to imOptics.Width - 10 do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            if Y < imOptics.Height - 15 then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
        end;
        DrawArray(X1, Y1, X2, Y2, clBlue);
        if rDO < Abs(rF) then begin
          for X := IX to DX do begin
            Y := Round(LM * X + LP);
            if X mod 10 < 5 then
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
            else
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
          end;
        end;
        // Convex lens F-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clAqua;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        if rDO > Abs(rF) then begin
          LineEquation(OX, OY, FX, FY, LM, LP);
          LinesIntersection(LM, LP, 0, IY, Intersection, LX, LY); DX := Round(LX);
          LineStart := True;
          for X := OX to DX do begin
            Y := Round(LM * X + LP);
            if LineStart then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
              X1 := X; Y1 := Y;
              LineStart := False;
            end
            else begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
          fOptics.imOptics.Picture.Bitmap.Canvas.Line(DX, IY, imOptics.Width - 10, IY);
          DrawArray(DX, IY, imOptics.Width - 10, IY, clAqua);
        end
        else begin
          LineEquation(OX, OY, F2X, FY, LM, LP);
          LinesIntersection(LM, LP, 0, IY, Intersection, LX, LY); DX := Round(LX);
          LineStart := True;
          for X := OX to DX do begin
            Y := Round(LM * X + LP);
            if LineStart then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
              X1 := X; Y1 := Y;
              LineStart := False;
            end
            else begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
          fOptics.imOptics.Picture.Bitmap.Canvas.Line(DX, IY, imOptics.Width - 10, IY);
          DrawArray(DX, IY, imOptics.Width - 10, IY, clAqua);
          for X := F2X to OX do begin
            Y := Round(LM * X + LP);
            if X mod 10 < 5 then
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
            else
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
          end;
          for X := IX to DX do begin
            Y := IY;
            if X mod 10 < 5 then
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
            else
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
          end;
        end;
        // Convex lens C-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clLime;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(OX, OY, IX, IY, LM, LP);
        LineStart := True;
        for X := OX to imOptics.Width - 10 do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            if X > 1060 then
              // Adapt ray length in order not to touch "case label"
              YM := 60
            else
              YM := 15;
            if Y < imOptics.Height - YM then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
        end;
        DrawArray(X1, Y1, X2, Y2, clLime);
        if rDO < Abs(rF) then begin
          for X := IX to OX do begin
            Y := Round(LM * X + LP);
            if X mod 10 < 5 then
              fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
            else
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
          end;
        end;
      end
      // Concave lens ray tracing
      else if cbDevice.Text = 'Lens (concave)' then begin
        // Concave lens P-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clBlue;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(FX, FY, IX, IY, LM, LP);
        LinesIntersection(LM, LP, 0, OY, Intersection, LX, LY); DX := Round(LX);
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(OX, OY, DX, OY);
        LineStart := True;
        for X := DX to imOptics.Width - 10 do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            if X > imOptics.Width - 80 then
              // Adapt ray length in order not touch "right eye"
              YM := 80
            else
              YM := 15;
            if Y > YM then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
        end;
        DrawArray(X1, Y1, X2, Y2, clBlue);
        for X := FX to DX do begin
          Y := Round(LM * X + LP);
          if X mod 10 < 5 then
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y)
          else
            fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
        end;
        // Concave lens C-ray
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clLime;
        fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 2;
        LineEquation(OX, OY, IX, IY, LM, LP);
        LineStart := True;
        for X := OX to imOptics.Width - 10 do begin
          Y := Round(LM * X + LP);
          if LineStart then begin
            fOptics.imOptics.Picture.Bitmap.Canvas.MoveTo(X, Y);
            X1 := X; Y1 := Y;
            LineStart := False;
          end
          else begin
            if X > 1060 then
              // Adapt ray length in order not to touch the "case label"
              YM := 60
            else
              YM := 15;
            if Y < imOptics.Height - YM then begin
              fOptics.imOptics.Picture.Bitmap.Canvas.LineTo(X, Y);
              X2 := X; Y2 := Y;
            end;
          end;
        end;
        DrawArray(X1, Y1, X2, Y2, clLime);
      end;
      // Draw object and image
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clRed;
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 4;
      fOptics.imOptics.Picture.Bitmap.Canvas.Line(OX, imOptics.Height div 2, OX, OY);
      fOptics.imOptics.Picture.Bitmap.Canvas.EllipseC(OX, OY + 4, 4, 4);
      fOptics.imOptics.Picture.Bitmap.Canvas.Line(IX, imOptics.Height div 2, IX, IY);
      if rHI > 0 then
        fOptics.imOptics.Picture.Bitmap.Canvas.EllipseC(IX, IY + 4, 4, 4)
      else
        fOptics.imOptics.Picture.Bitmap.Canvas.EllipseC(IX, IY - 4, 4, 4);
      // Draw C and F axis ticks and write labels (C, F, Object, Image)
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Color := clBlack;
      fOptics.imOptics.Picture.Bitmap.Canvas.Pen.Width := 1;
      if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
        // 'C' only displayed with mirrors
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(CX, CY - 5, CX, CY + 5);
        fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(CX - 2, CY + 10, 'C');
      end;
      fOptics.imOptics.Picture.Bitmap.Canvas.Line(FX, FY - 5, FX, FY + 5);
      fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(FX - 2, FY + 10, 'F');
      if cbDevice.Text = 'Lens (convex)' then begin
        // For convex lenses, display focus on both side of the mirror
        fOptics.imOptics.Picture.Bitmap.Canvas.Line(F2X, FY - 5, F2X, FY + 5);
        fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(F2X - 2, FY + 10, 'F');
      end;
      if rHO = -rHI then
        fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(OX - 16, imOptics.Height div 2 - 22, 'Object')
      else
        fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(OX - 16, imOptics.Height div 2 + 10, 'Object');
      if (rHI > 0) or (rHO = -rHI) then
        fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(IX - 15, imOptics.Height div 2 + 10, 'Image')
      else
        fOptics.imOptics.Picture.Bitmap.Canvas.TextOut(IX - 15, imOptics.Height div 2 - 22, 'Image');
      // Display ray color settings
      meRays.Text := RayText;
    end;
  end
  // If user data isn't ok, abort with error message
  else
    MessageDlg('Invalid data', S, mtError, [mbOK], 0)
end;

{ Adapt form controls when user selects another device }

procedure TfOptics.cbDeviceChange(Sender: TObject);
begin
  if LeftStr(cbDevice.Text, 6) = 'Mirror' then begin
    laRadius1.Caption := 'Curvature radius';
    laRadius2.Visible := False;
    edRadius2.Visible := False;
    laRefraction.Visible := False;
    edRefraction.Visible := False;
  end
  else begin
    if mSettingsSimple.Checked then begin
      laRadius1.Caption := 'Total curv. radius';
      laRadius2.Visible := False;
      edRadius2.Visible := False;
      laRefraction.Visible := False;
      edRefraction.Visible := False;
    end
    else begin
      laRadius1.Caption := 'Curvature radius 1';
      laRadius2.Visible := True;
      edRadius2.Visible := True;
      laRefraction.Visible := True;
      edRefraction.Visible := True;
    end;
  end;
end;

end.

