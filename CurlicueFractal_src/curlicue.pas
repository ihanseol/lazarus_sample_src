{*********************************************}
{* Main unit for CurlicueFractal application *}
{*********************************************}

unit curlicue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, help;

type
  {************}
  { TfCurlicue }
  {************}
  TfCurlicue = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit, mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    edS, edPoints, edLength: TEdit;
    cobS: TComboBox;
    imDraw: TImage;
    btDraw: TButton;
    btUp: TButton;
    btDown: TButton;
    btLeft: TButton;
    btRight: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure btDrawClick(Sender: TObject);
    procedure btUpClick(Sender: TObject);
    procedure btDownClick(Sender: TObject);
    procedure btLeftClick(Sender: TObject);
    procedure btRightClick(Sender: TObject);
    procedure cobSChange(Sender: TObject);
  private
    iCX, iCY: Integer;
    bInitPos: Boolean;
    bmDraw: TBitmap;
  end;

var
  fCurlicue: TfCurlicue;

implementation

{$R *.lfm}

{ Modulo 2π function for real numbers }

function Mod2Pi(D: Double): Double;

begin
  Result := D - (2 * Pi) * Int(D / (2 * Pi));
end;

{ Clear the drawing surface }

procedure SurfaceClear(Surface: TImage);

begin
  Surface.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  Surface.Picture.Bitmap.Canvas.Pen.Color := clBlack;
  Surface.Picture.Bitmap.Canvas.Rectangle(0, 0, Surface.Width, Surface.Height);
end;

{ Init the drawing of the curve }

procedure DrawInit(Surface: TImage);

begin
  SurfaceClear(Surface);
  Surface.Picture.Bitmap.Canvas.Pen.Color := clRed;
  Surface.Picture.Bitmap.Canvas.Pen.Width := 1;
end;

{ Draw the line from given point to point }

procedure DrawLine(Surface: TImage; X0, Y0, X1, Y1: Double; L, CX, CY: Integer);

// CX, CY are the starting coordinates of the curve on the drawing surface

var
  DX0, DY0, DX1, DY1: Integer;

begin
  // Transform curve coordinates to coordinates on the drawing surface
  DX0 := Round(CX + L * X0);
  DY0 := Round(CY - L * Y0);
  DX1 := Round(CX + L * X1);
  DY1 := Round(CY - L * Y1);
  // Draw the line between the 2 points
  Surface.Picture.Bitmap.Canvas.Line(DX0, DY0, DX1, DY1);
end;

{************}
{ TfCurlicue }
{************}

{ Application start: Initialization }

procedure TfCurlicue.FormCreate(Sender: TObject);

begin
  // Create a bitmap object and assign dimensions
  bmDraw := TBitmap.Create;
  bmDraw.Width := imDraw.Width;
  bmDraw.Height := imDraw.Height;
  // Assign the bitmap to the image component (the drawing surface)
  imDraw.Picture.Graphic := bmDraw;
  // Clear the bitmap
  SurfaceClear(imDraw);
  // Fill in the value for π
  // Doing it here and not in the properties sheet makes it independent of decimal separator!
  edS.Text := FloatToStr(3.141592653589793);
  // bInitPos is used to re-initialize or not the starting coordinates of the curve
  bInitPos := True;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfCurlicue.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display application help }

procedure TfCurlicue.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfCurlicue.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Fractals:' + LineEnding;
  S += 'Lazarus/Free Pascal Curlicue Fractal.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, June 2024.';
  MessageDlg('About "CurlicueFractal"', S, mtInformation, [mbOK], 0);
end;

{ Button "Draw" pushed: Draw the fractal curve }

procedure TfCurlicue.btDrawClick(Sender: TObject);

var
  N, L, I: Integer;
  S: Double;
  Theta, Phi, X0, Y0, X1, Y1: Double;

begin
  if (edS.Text <> '') and (edPoints.Text <> '') and (edLength.Text <> '') then begin
    // S = the irrational constant in the fractal formula
    // N = number of curve points to be calculated
    // L = length of the line to be added at each iteration
    S := StrToFloat(edS.Text);
    N := StrToInt(edPoints.Text);
    L := StrToInt(edLength.Text);
    // Initialize θ and ϕ
    Theta := 0; Phi := 0;
    // Set first point
    X0 := 0; Y0 := 0;
    // Drawing initialization (clear surface, set pen color)
    DrawInit(imDraw);
    // Perform N iterations (calculation of curve points; drawing a line from one point to the next)
    for I := 1 to N do begin
      // Coordinates of second point for angle ϕ
      X1 := X0 + Cos(Phi);
      Y1 := Y0 + Sin(Phi);
      // If bInitPos is set to True, set starting point of the curve on the drawing surface
      // For predefined values of s, set starting point in order to best fit the curve on the drawing surface,
      // for user values of s, set starting point at middle and center of drawing surface
      if bInitPos then begin
        case cobS.ItemIndex of
           0: begin
             iCX := imDraw.Width div 4; iCY := 25;
           end;
           1: begin
             iCX := imDraw.Width div 3; iCY := imDraw.Height div 5;
           end;
           2: begin
             iCX := 3 * imDraw.Width div 4; iCY := 3 * imDraw.Height div 4;
           end;
           3: begin
             iCX := imDraw.Width div 4; iCY := imDraw.Height div 3;
           end;
           4: begin
             iCX := imDraw.Width div 2; iCY := imDraw.Height div 3;
           end;
           5: begin
             iCX := imDraw.Width div 3; iCY := imDraw.Height div 2;
           end;
           6: begin
             iCX := imDraw.Width div 3; iCY := 2 * imDraw.Height div 3;
           end;
           7: begin
             iCX := imDraw.Width div 2; iCY := imDraw.Height div 2;
           end;
           8: begin
             iCX := 2 * imDraw.Width div 3; iCY := 2 * imDraw.Height div 3;
           end;
           9: begin
             iCX := 2 * imDraw.Width div 3; iCY := imDraw.Height div 2;
           end;
          10: begin
             iCX := imDraw.Width div 2; iCY := imDraw.Height div 2;
           end;
          11: begin
             iCX := imDraw.Width div 2; iCY := imDraw.Height - 20;
           end;
           else begin
             iCX := imDraw.Width div 2; iCY := imDraw.Height div 2;
           end;
        end;
      end;
      // Draw the line from first to second point
      DrawLine(imDraw, X0, Y0, X1, Y1, L, iCX, iCY);
      // Prepare for next iteration
      X0 := X1;
      Y0 := Y1;
      Phi := Mod2Pi(Theta + Phi);
      Theta := Mod2Pi(Theta + 2 * PI * S);
    end;
    // bInitPos := True is the default; it will be set to False by the methods associated with
    // the repositioning buttons (that call this method to redraw the curve)
    bInitPos := True;
    // The repositioning buttons were disabled at application start (to be sure that the "Draw" button will be used)
    btUp.Enabled := True; btDown.Enabled := True;
    btLeft.Enabled := True; btRight.Enabled := True;
  end
  else begin
    // Invalid user input (only check done: empty fields)
    if edS.Text = '' then
      MessageDlg('Input error', 'Value for S is missing!', mtError, [mbOK], 0)
    else if edPoints.Text = '' then
      MessageDlg('Input error', 'Number of points is missing!', mtError, [mbOK], 0)
    else
      MessageDlg('Input error', 'Line length is missing!', mtError, [mbOK], 0);
  end;
end;

{ Buttons "Up", "Down", "Left", "Right": Move the curve up, down, left, right = Redraw the curve with new starting point}

procedure TfCurlicue.btUpClick(Sender: TObject);

begin
  bInitPos := False;
  iCY -= 10;
  btDraw.Click;
end;

procedure TfCurlicue.btDownClick(Sender: TObject);

begin
    bInitPos := False;
    iCY += 10;
    btDraw.Click;
end;

procedure TfCurlicue.btLeftClick(Sender: TObject);

begin
  bInitPos := False;
  iCX -= 10;
  btDraw.Click;
end;

procedure TfCurlicue.btRightClick(Sender: TObject);

begin
  bInitPos := False;
  iCX += 10;
  btDraw.Click;
end;

{ S value selection (user changed selected value in combobox) }

procedure TfCurlicue.cobSChange(Sender: TObject);

var
  // Predefined S values
  S: array[0..11] of Double = (
    3.141592653589793, 1.61803398874989,  1.414213562373095, 1.732050807568877, 2.23606797749979,  0.707106781186548,
    0.577350269189626, 0.447213595499958, 0.693147180559945, 2.718281828459045, 0.577215664901533, 4.669201609102990
  );

begin
  if cobS.ItemIndex = 12 then begin
    // Selection = "User": Enable input in edit field
    edS.ReadOnly := False;
    edS.TabStop := True;
    edS.SetFocus;
  end
  else begin
    // Selection = predfined: Fill edit field with corr. value
    edS.Text := FloatToStr(S[cobS.ItemIndex]);
    edS.ReadOnly := True;
    edS.TabStop := False;
  end;
end;

end.

