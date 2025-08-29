{************************}
{* Quadratic Julia sets *}
{************************}

program juliaset;

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
  XMin0   = -2.0; XMax0 = 2.0;                                                 // quadratic Julia set X-scale
  YMin0   = -2.0; YMax0 = 2.0;                                                 // quadratic Julia set Y-scale
  Key_ESC = #27;                                                               // ESC key ASCII code

var
  C: TComplex;                                                                 // complex constant: Z(n+1) = z(n)^2 + C
  Error: Integer;
  XMin, XMax, YMin, YMax, XMF, YMF, Zoom: Real;
  GD, GM: SmallInt;                                                            // graphics variables must be smallint
  S: string;
  Key: Char;

{ Draw the Julia set }

procedure Julia(C: TComplex; XMF, YMF, XMin, YMin, Zoom: Real);

const
  NIters = NColors;                                                            // number of iterations

var
  XS, YS, XYColor, Iter : Integer;
  COld, CNew: TComplex;                                                        // complex numbers z(n) and z(n+1)

begin
  for XS := 0 to Width do begin                                                // for each point on the X-axis
    for YS := 0 to Height do begin                                             // for each point on the Y-axis
      COld.Re := (XS / XMF + XMin) / Zoom;                                     // transform x to real part of complex number
      COld.Im := (YS / YMF + YMin) / Zoom;                                     // transform y to imaginary part of complex number
      Iter := 0;
      // Iterate = calculate next value of (complex numbers) series
      // until the series diverges or the (chosen) maximum of iterations is reached
      while (Iter <= NIters) AND (Sqr(COld.Re) + Sqr(COld.Im) < 4) do begin
        CNew.Re := Sqr(COld.Re) - Sqr(COld.Im) + C.Re;                         // next complex (real part) in the series
        CNew.Im := 2 * COld.Re * COld.Im + C.Im;                               // next complex (imaginary part) in the series
        COld := CNew;                                                          // this complex used for next iteration's calculation
        Inc(Iter);                                                             // next iteration
      end;
      // Set XY-pixel with color depending on number of iterations done
      if Iter < NIters then begin
        XYColor := (Iter) mod NColors;                                           // pixel color depending on number of iterations done
        PutPixel(XS, Height - YS, XYColor);
      end;
    end;
  end;
end;

{ Main program }

begin
  repeat
    ClrScr;
    TextColor(Yellow);
    Writeln('Quadratic Julia sets');
    Writeln('====================');
    Writeln;
    TextColor(LightGray);
    Writeln('For Help with this program, please look at the');
    Writeln('ReadMe file included with the .zip archive...'); Writeln;
    Writeln('Julia set X scale = ', XMin0:0:2, ' to ', XMax0:0:2);
    Writeln('Julia set Y scale = ', YMin0:0:2, ' to ', YMax0:0:2);
    Writeln('To "shift" the graphics leftwards, enter X > ', XMin0:0:2);
    Writeln('To "shift" the graphics downwards, enter Y > ', YMin0:0:2);
    Writeln;
    Writeln('Complex constant C (determining the shape of the set):');
    // Enter complex constant C
    Write('Real part of C (or ENTER for default = -1)     ? '); Readln(S);
    if S = '' then
      C.Re := -1
    else
      C.Re := StrToFloat(S);
    Write('Imaginary part of C (or ENTER for default = 0) ? '); Readln(S);
    if S = '' then
      C.Im := 0
    else
      C.Im := StrToFloat(S);
    // Enter graphics zoom factor
    repeat
      Write('Zoom (or ENTER for default = 1)                ? '); Readln(S);
      if S = '' then
        Zoom := 1
      else
        Zoom := StrToFloat(S);
    until (Zoom >= 1);
    // Enter starting X value within the Julia set scale
    repeat
      Write('X scale value (or ENTER for default = -2.0)    ? '); Readln(S);
      if S = '' then
        XMin := XMin0
      else
        XMin := StrToFloat(S);
    until (XMin > XMin0 -(XMax0 - XMin0)) and (XMin < XMax0);
    XMax := XMax0;
    // Enter starting Y value within the Julia set scale
    repeat
      Write('Y scale value (or ENTER for default = -2.0)    ? '); Readln(S);
      if S = '' then
        YMin := YMin0
      else
        YMin := StrToFloat(S);
    until (YMin > YMin0 -(YMax0 - YMin0)) and (YMin < YMax0);
    YMax := YMax0;
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
    // Julia set in 800x600x256 graphics mode
    XMF := Width / (XMax - XMin0);
    YMF := Height / (YMax - YMin0);
    Julia(C, XMF, YMF, XMin, YMin, Zoom);                                      // draw the Julia set
    Key := ReadKey;                                                            // wait until a key has been pressed
    if Key = #0 then
      Key := ReadKey;
    CloseGraph;
  until Key = Key_ESC;                                                         // continue until key pressed = ESC
end.

