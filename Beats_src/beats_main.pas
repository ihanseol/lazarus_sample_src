{***************************************}
{* Main unit for the Beats application *}
{***************************************}

unit beats_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, beats_help;

type
  {*********}
  { TfBeats }
  {*********}
  TfBeats = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileCalc, mFileAnimation, mFileExit: TMenuItem;
    mOptions, mOptionsSine: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laF1, laF2, laFB: TLabel;
    edF1, edF2, edFB: TEdit;
    imDraw: TImage;
    btCalc: TButton;
    tiBeats: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileCalcClick(Sender: TObject);
    procedure mFileAnimationClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsSineClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure tiBeatsTimer(Sender: TObject);
  private
    iDrawWidth, iDrawHeight, iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight: Integer;
    rF1, rF2, rF2A: Real;
    Bitmap : TBitmap;
  end;

var
  fBeats: TfBeats;

implementation

{$R *.lfm}

{ Clean the graph by displaying a white rectangle }

procedure GraphClean(W, H: Integer);

begin
  fBeats.imDraw.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  fBeats.imDraw.Picture.Bitmap.Canvas.Rectangle(0, 0, W, H);
end;

{ Draw graph axes }

procedure DrawAxes(XL, XR, YT, YB, GH: Integer; UseSine: Boolean);

var
  YAX, YAY1, YAY2, GS, I: Integer;
  LegendX, LegendY: string;

begin
  fBeats.imDraw.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  fBeats.imDraw.Picture.Bitmap.Canvas.Pen.Width := 1;
  GS := (YB - YT - 4 * GH) div 2;                                              // verticak space between the individual graphs
  for I := 1 to 3 do begin
    // Calculate axe position
    case I of
      1: begin
        YAX := YT + GH div 2;                                                  // vertical position of x-axis
        YAY1 := YAX - GH div 2;                                                // top position of y-axis
        YAY2 := YAX + GH div 2;                                                // bottom position of y-axis
        LegendX := 't'; LegendY := 'y' + SUB_1;
      end;
      2: begin
        YAX := YT + GH div 2 + GH + GS;
        YAY1 := YAX - GH div 2;
        YAY2 := YAX + GH div 2;
        LegendX := 't'; LegendY := 'y' + SUB_2;
      end;
      3: begin
        YAX := YT + 3 * GH + 2 * GS;
        YAY1 := YAX - GH;
        YAY2 := YAX + GH;
        LegendX := 't'; LegendY := 'y';
      end;
    end;
    // Draw x-axis
    fBeats.imDraw.Picture.Bitmap.Canvas.Line(XL - 20, YAX, XR + 40, YAX);
    // Draw y-axis (for cosine functions only)
    if not UseSine then begin
      fBeats.imDraw.Picture.Bitmap.Canvas.Line(XL, YAY1 - 10, XL, YAY2 + 10);  // draw y-axis
      fBeats.imDraw.Picture.Bitmap.Canvas.TextOut(XR + 20, YAX + 5, LegendX);  // display x-axis legend
      fBeats.imDraw.Picture.Bitmap.Canvas.TextOut(20, YAY1 - 10, LegendY);     // display y-axis legend
    end;
  end;
end;

{ Draw graphs }

procedure DrawGraphs(XL, XR, YT, YB, GH: Integer; F1, F2: Real; UseSine: Boolean);

var
  YAX, GS, GX, GY, GXMax, GYMax, I: Integer;
  T, Y: Real;

begin
  GS := (YB - YT - 4 * GH) div 2;                                              // vertical space between the individual graphs
  GXMax := XR - XL;                                                            // maximal y-value on graph
  for I := 1 to 3 do begin
    case I of
      1: begin
        YAX := YT + GH div 2;                                                  // vertical position of the x-axis
        GYMax := GH div 2;                                                     // maximum y.value on the graph
      end;
      2: begin
        YAX := YT + GH div 2 + GH + GS;
        GYMax := GH div 2;
      end;
      3: begin
        YAX := YT + 3 * GH + 2 * GS;
        GYMax := GH;
      end;
    end;
    // Draw the graph
    for GX := 0 to GXMax do begin
      // For each x-value on the graph, use an appropriate corresponding time value
      T := GX / 1E4;
      // Calculate y as a function of t
      case I of
        1: begin
          if UseSine then
            Y := 100 * Sin(2 * Pi * F1 * T)
          else
            Y := 100 * Cos(2 * Pi * F1 * T);
          GY := Round((Y / 100) * GYMax);                                      // y-value on the graph
        end;
        2: begin
          if UseSine then
            Y := 100 * Sin(2 * Pi * F2 * T)
          else
            Y := 100 * Cos(2 * Pi * F2 * T);
          GY := Round((Y / 100) * GYMax);
        end;
        3: begin
          if UseSine then
            Y := 100 * Sin(2 * Pi * F1 * T) + 100 * Sin(2 * Pi * F2 * T)
          else
            Y := 100 * Cos(2 * Pi * F1 * T) + 100 * Cos(2 * Pi * F2 * T);
          GY := Round((Y / 200) * GYMax);
        end;
      end;
      fBeats.imDraw.Picture.Bitmap.Canvas.Pen.Width := 2;
      fBeats.imDraw.Picture.Bitmap.Canvas.Pen.Color := clBlue;
      if GX = 0 then
        fBeats.imDraw.Picture.Bitmap.Canvas.MoveTo(XL + GX, YAX - GY)          // first graph point
      else
        fBeats.imDraw.Picture.Bitmap.Canvas.LineTo(XL + GX, YAX - GY);         // other graph points
    end;
  end;
end;

{*********}
{ TfBeats }
{*********}

{ Application start-up: Initialization }

procedure TfBeats.FormCreate(Sender: TObject);

begin
  // Graph area
  iDrawWidth := imDraw.Width; iDrawHeight := imDraw.Height;
  iGraphsLeft := 40; iGraphsRight := iDrawWidth - 60;
  iGraphsTop := 30; iGraphsBottom := iDrawHeight - 20;
  iGraphHeight := 150;
  // Create a bitmap object and assign dimensions
  Bitmap := TBitmap.Create;
  Bitmap.Width  := iDrawWidth;
  Bitmap.Height := iDrawHeight;
  //Assign the bitmap to the image component (the drawing surface)
  imDraw.Picture.Graphic := Bitmap;
  // Apply subscripts
  laF1.Caption := 'First wave frequency f' + SUB_1;
  laF2.Caption := 'Second wave frequency f' + SUB_2;
  // Draw graph area and axes
  GraphClean(iDrawWidth, iDrawHeight);
  DrawAxes(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, mOptionsSine.Checked);
end;

{ Menu item "File > New calculation": Prepare for new calculation }

procedure TfBeats.mFileCalcClick(Sender: TObject);

begin
  tiBeats.Enabled := False;
  GraphClean(iDrawWidth, iDrawHeight);
  DrawAxes(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, mOptionsSine.Checked);
  edF1.ReadOnly := False; edF2.ReadOnly := False;
  edF1.TabStop := True; edF2.TabStop := True;
  mOptions.Enabled := True;
  edF1.Text := ''; edF2.Text := ''; edFB.Text := '';
  edF1.SetFocus;
  btCalc.Enabled := True;
  btCalc.Caption := 'Draw';
end;

{ Menu item "File > New animation": Prepare for new animation }

procedure TfBeats.mFileAnimationClick(Sender: TObject);

begin
  tiBeats.Enabled := False;
  GraphClean(iDrawWidth, iDrawHeight);
  DrawAxes(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, mOptionsSine.Checked);
  edF1.ReadOnly := False; edF2.ReadOnly := False;
  edF1.TabStop := True; edF2.TabStop := True;
  mOptions.Enabled := True;
  edF1.Text := ''; edF2.Text := ''; edFB.Text := '';
  edF1.SetFocus;
  btCalc.Enabled := True;
  btCalc.Caption := 'Start';
end;

{ Menu item "File > Exit": Exit application }

procedure TfBeats.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Use sine functions": Toggle between cosine and sine functions }

procedure TfBeats.mOptionsSineClick(Sender: TObject);

begin
  if mOptionsSine.Checked then
    mOptionsSine.Checked := False
  else
    mOptionsSine.Checked := True;
end;

{ Menu item "Help > Help": Display application help }

procedure TfBeats.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfBeats.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Physics.' + LineEnding;
  S += 'Beats - Superposition of two sound waves.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, December 2022 - January 2023.';
  MessageDlg('About "Beats"', S, mtInformation, [mbOK], 0);
end;

{ Button "Draw/Start/Pause/Resume" pushed: Perform corresponding action }

procedure TfBeats.btCalcClick(Sender: TObject);

var
  Mess: string;

begin
  // Read frequencies from form (user input)
  Mess := '';
  if edF1.Text = '' then
    rF1 := 0
  else
    rF1 := StrToFloat(edF1.Text);
  if edF2.Text = '' then
    rF2 := 0
  else
    rF2 := StrToFloat(edF2.Text);
  // Check entered values; proceed if all ok, otherwise quit with error message
  if rF1 <= 0 then begin
    Mess := 'Invalid frequency f' + SUB_1;
    edF1.SetFocus;
  end
  else if (rF1 < 100) or (rF1 > 1000) then begin
    Mess := 'Frequency f' + SUB_1 + ' should be between 100 and 1000 Hz';
    edF1.SetFocus;
  end
  else if rF2 <= 0 then begin
    Mess := 'Invalid frequency f' + SUB_2;
    edF2.SetFocus;
  end
  else if (rF2 < 100) or (rF2 > 1000) then begin
    Mess := 'Frequency f' + SUB_2 + ' should be between 100 and 1000 Hz';
    edF2.SetFocus;
  end;
  if Mess = '' then begin
    // Frequencies entered ok: Perform action as defined by the button caption
    edFB.Text := FloatToStr(Abs(rF1 - rF2));
    // Button "Draw": Draw the wave curves for user entered frequencies
    if btCalc.Caption = 'Draw' then begin
      GraphClean(iDrawWidth, iDrawHeight);
      DrawAxes(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, mOptionsSine.Checked);
      DrawGraphs(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, rF1, rF2, mOptionsSine.Checked);
    end
    // Button "Start": Start animation with user entered frequencies
    else if btCalc.Caption = 'Start' then begin
      edF1.ReadOnly := True; edF2.ReadOnly := True;
      edF1.TabStop := False; edF2.TabStop := False;
      mOptions.Enabled := False;
      btCalc.Caption := 'Pause';
      rF2A := rF2;
      tiBeats.Enabled := True;                                                 // start the timer (animation code will be executed)
    end
    // Button "Pause": Pause the animation
    else if btCalc.Caption = 'Pause' then begin
      tiBeats.Enabled := False;
      btCalc.Caption := 'Resume';
    end
    // Button "Resume": Resume the animation
    else begin
      btCalc.Caption := 'Pause';
      tiBeats.Enabled := True;
    end;
  end
  else begin
    // Invalid user input: Display error or warning message
    Mess += '!';
    if LeftStr(Mess, 7) = 'Invalid' then
      MessageDlg('Invalid data', Mess, mtError, [mbOK], 0)
    else
      MessageDlg('Inadequate data', Mess, mtWarning, [mbOK], 0);
  end;
end;

{ Animation timer routine }

procedure TfBeats.tiBeatsTimer(Sender: TObject);

// The routine, automatically executed every second, displays the wave curves
// with the second wave's frequency incremented by 5 Hz each time the routine is called

begin
  // Draw the wave curves
  GraphClean(iDrawWidth, iDrawHeight);
  DrawAxes(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, mOptionsSine.Checked);
  DrawGraphs(iGraphsLeft, iGraphsRight, iGraphsTop, iGraphsBottom, iGraphHeight, rF1, rF2A, mOptionsSine.Checked);
  // Fill in second wave and wave superbosition frequency
  edF2.Text := FloatToStr(rF2A); edFB.Text := FloatToStr(Abs(rF1 - rF2A));
  // Increment second wave's frequency
  rF2A += 5;
  if rF2A > 1000 then begin
    // If maximum frequency is reached, stop the animation (= disable the timer)
    tiBeats.Enabled := False;
    btCalc.Caption := 'Start';
    btCalc.Enabled := False;                                                   // user must use one of the "New" commands to continue
  end;
end;

end.

