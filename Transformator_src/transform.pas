{*******************************************}
{* Main unit for Transformator application *}
{*******************************************}

unit transform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, Menus;

type
  {*************}
  { TfTransform }
  {*************}
  TfTransform = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsZ, mOptionsZNp, mOptionsZNs: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    rbTransformU, rbTransformI, rbTransformZ: TRadioButton;
    Memo1: TMemo;
    Image1, imGraph: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    Label7, Label8, Label9, Label10, Label11: TLabel;
    laZp, laUZp, laZs, laUZs, laTurns: TLabel;
    edUp, edIp, edZp, edNp, edNs, edRatio: TEdit;
    edUs, edIs, edZs: TEdit;
    btAction: TButton;
    tbTurns: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsZNpClick(Sender: TObject);
    procedure mOptionsZNsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbTransformUChange(Sender: TObject);
    procedure rbTransformIChange(Sender: TObject);
    procedure rbTransformZChange(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure tbTurnsChange(Sender: TObject);
  private
    iImageWidth, iImageHeight, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, iCalc: Integer;
    rUp, rUs, rIp, rIs, rZp, rZs, rNp, rNs, rXMax, rYMax: Real;
    sXLabel, sYLabel: string;
    bmDraw: TBitmap;
  end;

var
  fTransform: TfTransform;

implementation

{$R *.lfm}

{ Clean graph by displaying a white rectangle }

procedure GraphClean(var Surface: TImage; W, H: Integer);

begin
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axis and write labels }

procedure DrawAxis(var Surface: TImage; XL, XR, YT, YB: Integer; LabelX, LabelY: string);

var
  YAX, YLX: Integer;

begin
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
  YAX := YB; YLX := YB + 7;
  Surface.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 20, YAX);              // draw X-axis
  Surface.Picture.Bitmap.Canvas.Line(XL, YT - 40, XL, YB + 20);                // draw Y-axis
  Surface.Picture.Bitmap.Canvas.Font.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.TextOut(XR, YLX, LabelX);                      // display X-axis label
  Surface.Picture.Bitmap.Canvas.TextOut(6, 6, LabelY);                         // display Y-axis label
end;

{ Get transformer data from form input fields }

procedure ReadData(Calc: Integer; out Up, Jp, Js, Np, Ns: Real; out Mess: string);

var
  P: Integer;

begin
  Mess := '';
  if fTransform.edUp.Text = '' then begin
    Mess := 'Wert der Eingangsspannung fehlt';
    fTransform.edUp.SetFocus;
  end
  else begin
    Up := StrToFloat(fTransform.edUp.Text);
    if Up < 0 then begin
      Mess := 'Eingangsspannung muss grösser als 0 sein';
      fTransform.edUp.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if Calc <> 2 then begin
      if fTransform.edIp.Text = '' then begin
        Mess := 'Wert der Eingangsstromstärke fehlt';
        fTransform.edIp.SetFocus;
      end
      else begin
        Jp := StrToFloat(fTransform.edIp.Text);
        if Jp < 0 then begin
          Mess := 'Eingangsstromstärke muss grösser als 0 sein';
          fTransform.edIp.SetFocus;
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    if fTransform.edNp.Text = '' then begin
      Mess := 'Anzahl der Primärwindungen fehlt';
      fTransform.edNp.SetFocus;
    end
    else begin
      Np := StrToFloat(fTransform.edNp.Text);
      if (Np < 0) or (Np <> Int(Np)) then begin
        Mess := 'Anzahl der Primärwindungen muss eine Ganzzahl grösser als 0 sein';
        fTransform.edNp.SetFocus;
      end
      else if (Np < 1) or (Np > 100) then begin
        Mess := 'Anzahl der Primärwindungen soll zwischen 1 und 100 sein';
        fTransform.edNp.SetFocus;
      end;
    end;
  end;
  if Mess = '' then begin
    if fTransform.edNs.Text = '' then begin
      MessageDlg('Fehlende Daten', 'Anzahl der Sekundärwindungen fehlt! Angenommen dass sie gleich der Anzahl der Primärwindungen ist.', mtWarning, [mbOK], 0);
      Ns := Np;
      fTransform.edNs.Text := FloatToStrF(Ns, ffFixed, 0, 0);
    end
    else begin
      Ns := StrToFloat(fTransform.edNs.Text);
      if (Ns < 0) or (Ns <> Int(Ns)) then begin
        Mess := 'Anzahl der Sekundärwindungen muss eine Ganzzahl grösser als 0 sein';
        fTransform.edNs.SetFocus;
      end
      else if (Ns < 1) or (Ns > 100) then begin
        Mess := 'Anzahl der Sekundärwindungen soll zwischen 1 und 100 sein';
        fTransform.edNs.SetFocus;
      end;
    end;
  end;
  if Mess = '' then begin
    if Calc = 2 then begin
      if fTransform.edIs.Text = '' then begin
        Mess := 'Wert der Ausgangsstromstärke fehlt';
        fTransform.edIs.SetFocus;
      end
      else begin
        Js := StrToFloat(fTransform.edIs.Text);
        if Js < 0 then begin
          Mess := 'Ausgangsstromstärke muss grösser als 0 sein';
          fTransform.edIs.SetFocus;
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    if (Calc = 1) or ((Calc = 3) and fTransform.mOptionsZNp.Checked) then
      fTransform.tbTurns.Position := Round(Ns)
    else
      fTransform.tbTurns.Position := Round(Np);
  end
  else begin
    P := Pos('soll', Mess);
    if P > 0 then
      MessageDlg('Ungültige Daten', Mess + '!', mtWarning, [mbOK], 0)
    else
      MessageDlg('Fehlerhafte Daten', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Enable/disable user input }

procedure SetInputAccess(Enable: Boolean);

var
  Disable: Boolean;

begin
  Disable := not Enable;
  fTransform.edUp.ReadOnly := Disable; fTransform.edUp.TabStop := Enable;
  fTransform.edIp.ReadOnly := Disable; fTransform.edIp.TabStop := Enable;
  fTransform.edNp.ReadOnly := Disable; fTransform.edNp.TabStop := Enable;
  fTransform.edNs.ReadOnly := Disable; fTransform.edNs.TabStop := Enable;
  fTransform.edIs.ReadOnly := Disable; fTransform.edIs.TabStop := Enable;
end;

{ Voltage transformation calculations }

procedure Voltage(Up, Jp, Np, Ns: Real; out Us, Js: Real);

begin
  Us := Up * (Ns / Np);
  Js := Jp * (Np / Ns);
  fTransform.edUs.Text := FloatToStrF(Us, ffFixed, 0, 2);
  fTransform.edIs.Text := FloatToStrF(Js, ffFixed, 0, 2);
end;

{ Current transformation calculations }

procedure Current(Up, Js, Np, Ns: Real; out Us, Jp: Real);

begin
  Us := Up * (Ns / Np);
  Jp := Js * (Ns / Np);
  fTransform.edUs.Text := FloatToStrF(Us, ffFixed, 0, 2);
  fTransform.edIp.Text := FloatToStrF(Jp, ffFixed, 0, 2);
end;

{ Impedance transformation calculations }

procedure Impedance(Up, Jp, Np, Ns: real; out Us, Js, Zp, Zs: Real);

begin
  Us := Up * (Ns / Np);
  Js := Jp * (Np / Ns);
  Zp := Up / Jp;
  Zs := Us / Js;
  fTransform.edUs.Text := FloatToStrF(Us, ffFixed, 0, 2);
  fTransform.edIs.Text := FloatToStrF(Js, ffFixed, 0, 2);
  fTransform.edZp.Text := FloatToStrF(Zp, ffFixed, 0, 2);
  fTransform.edZs.Text := FloatToStrF(Zs, ffFixed, 0, 2);
end;

{ Transformation calculation (and value display on the graph) }

procedure CalcAndDraw(var Surface: TImage; XL, XR, YT, YB, Calc: Integer; XMax, YMax, Up, Jp, Np, Ns, Us, Js, Zp, Zs: Real);

var
  X, Y: Integer;
  XValue, YValue: Real;

begin
  case Calc of
    1: begin
      // Voltage transformation
      Voltage(Up, Jp, Np, Ns, Us, Js);
      XValue := Ns; YValue := Us;
    end;
    2: begin
      // Current transformation
      Current(Up, Js, Np, Ns, Us, Jp);
      XValue := Np; YValue := Jp;
    end;
    3: begin
      // Impedance transformation
      Impedance(Up, Jp, Np, Ns, Us, Js, Zp, Zs);
      if fTransform.mOptionsZNp.Checked then
        XValue := Ns
      else
        XValue := Np;
      YValue := Zs;
    end;
  end;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlue;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clBlue;
  // X-value (number of primary/secondary turns) on graph
  X := Round((XValue / XMax) * (XR - XL)) + XL;
  // Y-value (voltage/current/impedance) on graph
  Y := YB - Round((YValue / YMax) * (YB - YT));
  // Display value on graph
  Surface.Picture.Bitmap.Canvas.EllipseC(X, Y, 4, 4);
  // Reset pen and brush color
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
end;

{*************}
{ TfTransform }
{*************}

{ Application start: Initialisation }

procedure TfTransform.FormCreate(Sender: TObject);

begin
  // Graph area
  iImageWidth := imGraph.Width; iImageHeight := imGraph.Height;
  iGraphLeft  := 60; iGraphRight  := iImageWidth - 40;
  iGraphTop   := 50; iGraphBottom := iImageHeight - 40;
  // Create a bitmap object and assign dimensions
  bmDraw := TBitmap.Create;
  bmDraw.Width := iImageWidth;
  bmDraw.Height := iImageHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imGraph.Picture.Graphic := bmDraw;
  // Clean the graph and draw axes
  GraphClean(imGraph, iImageWidth, iImageHeight);
  // Start-up = voltage transformation
  iCalc := 1;
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfTransform.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Einstellungen > Impedanzübertragung > ...": Toggle constant turn value }

procedure TfTransform.mOptionsZNpClick(Sender: TObject);

// Primary turns value is constant; trackbar is secondary turns

begin
  mOptionsZNp.Checked := True; mOptionsZNs.Checked := False;
  if iCalc = 3 then begin
    edNp.ReadOnly := False; edNp.TabStop := True; edNp.Color := clDefault;
    edNs.ReadOnly := False;  edNs.TabStop := True;  edNs.Color := clDefault;
    laTurns.Caption := 'Sekundärwindungen';
  end;
end;

procedure TfTransform.mOptionsZNsClick(Sender: TObject);

// Secondary turns value is constant; trackbar is primary turns

begin
  mOptionsZNp.Checked := False; mOptionsZNs.Checked := True;
  if iCalc = 3 then begin
    edNp.ReadOnly := False; edNp.TabStop := True; edNp.Color := clDefault;
    edNs.ReadOnly := False;  edNs.TabStop := True;  edNs.Color := clDefault;
    laTurns.Caption := 'Primärwindungen';
  end;
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfTransform.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Elektronik:' + LineEnding;
  S += 'Eigenschaften eines idealen Transformators.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Mai - Juli 2022.';
  MessageDlg('Über "Transformator"', S, mtInformation, [mbOK], 0);
end;

{ Transformation type selection (user checked corresponding radiobutton) }

procedure TfTransform.rbTransformUChange(Sender: TObject);

begin
  if rbTransformU.Checked then begin
    btAction.Caption := 'Start';
    edNp.ReadOnly := False; edNp.TabStop := True;  edNp.Color := clDefault;
    edNs.ReadOnly := False; edNs.TabStop := True;  edNs.Color := clDefault;
    edIp.ReadOnly := False; edIp.TabStop := True;  edIp.Color := clDefault;
    edIs.ReadOnly := True;  edIs.TabStop := False; edIs.Color := clCream;
    edUs.Text := ''; edIs.Text := '';
    laTurns.Caption := 'Sekundärwindungen';
    tbTurns.Enabled := False;
    GraphClean(imGraph, iImageWidth, iImageHeight);
    laZp.Visible := False; edZp.Visible := False; laUZp.Visible := False;
    laZs.Visible := False; edZs.Visible := False; laUZs.Visible := False;
    edUp.SetFocus;
    iCalc := 1;
  end;
end;

procedure TfTransform.rbTransformIChange(Sender: TObject);

begin
  if rbTransformI.Checked then begin
    btAction.Caption := 'Start';
    edNp.ReadOnly := False; edNp.TabStop := True;  edNp.Color := clDefault;
    edNs.ReadOnly := False; edNs.TabStop := True;  edNs.Color := clDefault;
    edIp.ReadOnly := True;  edIp.TabStop := False; edIp.Color := clCream;
    edIs.ReadOnly := False; edIs.TabStop := True;  edIs.Color := clDefault;
    edUs.Text := ''; edIp.Text := '';
    laTurns.Caption := 'Primärwindungen';
    tbTurns.Enabled := False;
    GraphClean(imGraph, iImageWidth, iImageHeight);
    laZp.Visible := False; edZp.Visible := False; laUZp.Visible := False;
    laZs.Visible := False; edZs.Visible := False; laUZs.Visible := False;
    edUp.SetFocus;
    iCalc := 2;
  end;
end;

procedure TfTransform.rbTransformZChange(Sender: TObject);

begin
  if rbTransformZ.Checked then begin
    btAction.Caption := 'Start';
    edNp.ReadOnly := False; edNp.TabStop := True;  edNp.Color := clDefault;
    edNs.ReadOnly := False; edNs.TabStop := True;  edNs.Color := clDefault;
    edIp.ReadOnly := False; edIp.TabStop := True;  edIp.Color := clDefault;
    edIs.ReadOnly := True;  edIs.TabStop := False; edIs.Color := clCream;
    edUs.Text := ''; edIs.Text := '';
    if mOptionsZNp.Checked then
      laTurns.Caption := 'Sekundärwindungen'
    else
      laTurns.Caption := 'Primärwindungen';
    tbTurns.Enabled := False;
    GraphClean(imGraph, iImageWidth, iImageHeight);
    laZp.Visible := True; edZp.Visible := True; laUZp.Visible := True;
    laZs.Visible := True; edZs.Visible := True; laUZs.Visible := True;
    edZp.Text := ''; edZs.Text := '';
    edUp.SetFocus;
    iCalc := 3;
  end;
end;

{ Button "Start/Stop" pushed: Start resp. stop transformer simulation }

procedure TfTransform.btActionClick(Sender: TObject);

var
  Mess: string;
  Ratio: Real;

begin
  // "Start" pushed: Start transformer simulation
  if btAction.Caption = 'Start' then begin
    mOptionsZ.Enabled := False;                                                // disable option menu items
    ReadData(iCalc, rUp, rIp, rIs, rNp, rNs, Mess);                            // get transformer data from form (user input)
    if Mess = '' then begin
      SetInputAccess(False);                                                   // disable user input
      Ratio := rNp / rNs;
      edRatio.Text := FloatToStrF(Ratio, ffFixed, 0, 2);
      GraphClean(imGraph, iImageWidth, iImageHeight);
      // Determine maximum graph values and graph axes labels
      case iCalc of
        1: begin
          rXMax := 100; rYMax := rUp * (100 / rNp);
          sXLabel := 'Ns'; sYLabel := 'Us [V]';
        end;
        2: begin
          rXMax := 100; rYMax := rIs * (rNs / rNp);
          sXLabel := 'Np'; sYLabel := 'Ip [A]';
        end;
        3: begin
          rXMax := 100;
          if mOptionsZNp.Checked then
            rYMax := (rUp * (100 / rNp)) / (rIp * (rNp / 100))
          else
            rYMax := (rUp * (rNs / rNp)) / (rIp * (rNp / rNs));
          if mOptionsZNp.Checked then
            sXLabel := 'Ns'
          else
            sXLabel := 'Np';
          sYLabel := 'Zs [Ω]';
        end;
      end;
      // Calculate transformation values and display voltage/current/impedance on the graph
      DrawAxis(imGraph, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, sXLabel, sYLabel);
      CalcAndDraw(imGraph, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, iCalc,
        rXMax, rYMax, rUp, rIp, rNp, rNs, rUs, rIs, rZp, rZs);
      tbTurns.Enabled := True;                                                 // user can now use the trackbar for primary/secondary turns value
      btAction.Caption := 'Stop';                                              // next button push will be to stop the simulation
    end;
  end
  // "Stop" pushed: Stop transformer simulation
  else begin
    tbTurns.Enabled := False;                                                  // disable trackbar for primary/secondary turns value
    SetInputAccess(True);                                                      // enable user input (edit fileds)
    mOptionsZ.Enabled := True;                                                 // enable option menu items
    btAction.Caption := 'Start';                                               // next button push will be to start the simulation
  end;
end;

{ Trackbar changes (user chose new turns value): Redo calculations }

procedure TfTransform.tbTurnsChange(Sender: TObject);

begin
  if btAction.Caption = 'Stop' then begin
    // What value the trackbar position actually represents depends on actual type of transformation studied
    case iCalc of
      1: rNs := tbTurns.Position;
      2: rNp := tbTurns.Position;
      3: if mOptionsZNp.Checked then
           rNs := tbTurns.Position
         else
           rNp := tbTurns.Position;
    end;
    // Refill number of turns and turns ratio edit fields
    edNp.Text := FloatToStr(rNp); edNs.Text := FloatToStr(rNs);
    edRatio.Text := FloatToStrF(rNp / rNs, ffFixed, 0, 2);
    // Redo calculation (and display voltage/current/impedance on graph)
    CalcAndDraw(imGraph, iGraphLeft, iGraphRight, iGraphTop, iGraphBottom, iCalc,
      rXMax, rYMax, rUp, rIp, rNp, rNs, rUs, rIs, rZp, rZs);
  end;
end;

end.

