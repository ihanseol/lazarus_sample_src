{************************}
{* Burning Ship fractal *}
{************************}

program bship;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils, crt, graph;

type
  TComplex = record
    Re, Im : Real
  end;

const
  Width   = 800;                                                               // screen width
  Height  = 600;                                                               // screen height
  NColors = 256;
  XMin0   = -2.0; XMax0 = 1.0;                                                 // Burning Ship X-scale (screen adapted)
  YMin0   = -1.5; YMax0 = 1.5;                                                 // Burning Ship Y-scale (screen adapted)
  Key_ESC = #27;                                                               // ESC key ASCII code

var
  XS, YS, Error: Integer;
  XMin, XMax, YMin, YMax, X, Y, XMF, YMF, Zoom: Real;
  GD, GM: SmallInt;                                                            // graphics variables must be smallint
  S: string;
  Key: Char;
  RingsDisplay: Boolean;

{ Draw the BurningShip set }

procedure BurningShip(XS, YS: Integer; X, Y: Real; Rings: Boolean);

const
  NIters = NColors;                                                            // number of iterations
  InsideColor = Black;                                                         // color of pixels belonging to the set
  AroundColor = Blue;                                                          // color around the set and used for iterations <= 16 (if this is selected)

var
  Iter, XYColor: Integer;
  XTemp: Real;
  Z: TComplex;

begin
  Z.Re := X; Z.Im := Y;
  Iter := 0;
  while (Sqr(Z.Re) + Sqr(Z.Im) < 4) and (Iter < NIters) do begin
    XTemp := Sqr(Z.Re) - Sqr(Z.Im) + X;
    Z.Im := Abs(2*Z.Re*Z.Im + Y);
    Z.Re := Abs(XTemp);
    Inc(Iter);
  end;
  if (Iter = NIters) then                                                      // pixel belongs to the set
    XYColor := InsideColor
  else begin
    XYColor := Iter mod NColors;                                               // pixel color depending on number of iterations done
    if XYColor = Black then
      XYColor := AroundColor
    else if not Rings and(Iter <= 16)
      then XYColor := AroundColor;
  end;
  PutPixel(XS, YS, XYColor);
end;

{ Main program }

begin
  repeat
    ClrScr;
    TextColor(Yellow);
    Writeln('Burning Ship fractal');
    Writeln('====================');
    Writeln;
    TextColor(LightGray);
    Writeln('For Help with this program, please look at the');
    Writeln('ReadMe file included with the .zip archive...'); Writeln;
    Writeln('Burning Ship X scale = ', XMin0:0:2, ' to ', XMax0:0:2);
    Writeln('Burning Ship Y scale = ', YMin0:0:2, ' to ', YMax0:0:2);
    Writeln('To "shift" the graphics leftwards, enter X > ', XMin0:0:2);
    Writeln('To "shift" the graphics upwards, enter Y > ', YMin0:0:2);
    Writeln;
    // Enter graphics zoom factor
    repeat
      Write('Zoom (or ENTER for default = 1)              ? '); Readln(S);
      if S = '' then
        Zoom := 1
      else
        Zoom := StrToFloat(S);
    until (Zoom >= 1);
    // Enter starting X value within the Burning Ship fractal scale
    Write('X scale value (or ENTER for default = ', XMin0:0:2, ') ? '); Readln(S);
    if S = '' then
      XMin := XMin0
    else
      XMin := StrToFloat(S);
    XMax := XMax0;
    // Enter starting Y value within the Burning Ship fractal scale
    Write('Y scale value (or ENTER for default = ', YMin0:0:2, ') ? '); Readln(S);
    if S = '' then
      YMin := YMin0
    else
      YMin := StrToFloat(S);
    YMax := YMax0;
    // Choose if or not to display iterations <= 16 ("rings")
    RingsDisplay := True;
    repeat
      Write('Display iterations <= 16 ("rings")           ? '); Readln(S);
      if S = '' then
        S := 'Y';
      S := UpperCase(S);
    until S[1] in ['Y', 'N'];
    if S = 'N' then
      RingsDisplay := False;
    Writeln; Write('"ESC" to exit, any other key continue... ');
    // Init graphics driver
    GM := m800x600; GD := D8bit;                                               // 800 x 600 pixles, 256 colors
    InitGraph(GD, GM, '');                                                     // graph initialization: this opens Graphics output window
    // Check if selected graphics mode is supported
    Error := GraphResult;
    if (Error <> grOk) then begin
      Writeln; Writeln; Writeln ('Graphics error: 640x480x256 graphics mode not supported!');
      Readln;
      Halt (1);
    end;
    // Burning Ship fractal in 800x600x256 graphics mode
    for XS := 1 to Width do begin
      for YS := 1 to Height do begin
        XMF := Width / (XMax - XMin0);
        YMF := Height / (YMax - YMin0);
        X := (XS / XMF) / Zoom + XMin;                                         // scaled x coordinate of pixel
        Y := (YS / YMF) / Zoom + YMin;                                         // scaled y coordinate of pixel
        BurningShip(XS, YS, X, Y, RingsDisplay);
      end;
    end;
    Key := ReadKey;                                                            // wait until a key has been pressed
    if Key = #0 then
      Key := ReadKey;
    CloseGraph;
  until Key = Key_ESC;                                                         // continue until key pressed = ESC
end.

