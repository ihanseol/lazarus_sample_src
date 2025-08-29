{******************}
{* Mandelbrot Set *}
{******************}

program mandelbrot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils, crt, graph;

const
  XSMax   = 800;                                                               // screen width
  YSMax   = 600;                                                               // screen height
  NColors = 256;                                                               // number of colors

var
  XS, YS, YSH, Iter, NIters, XYColor, Error: Integer;
  GD, GM: SmallInt;
  X, Y, X0, Y0, XScaleLow, XScaleHigh, XScale0, Zoom, XYZoom, T: Real;
  S: string;

begin
  repeat
    ClrScr;
    TextColor(Yellow);
    Writeln('Mandelbrot Set');
    Writeln('==============');
    Writeln;
    TextColor(LightGray);
    XScaleLow := -2.5; XScaleHigh := 1;                                        // Mandelbrot set X scale
    Writeln('For Help with this program, please look at the');
    Writeln('ReadMe file included with the .zip archive...'); Writeln;
    Writeln('Mandelbrot set X scale = ', XScaleLow:0:2, ' to ', XScaleHigh:0:2);
    Writeln('To "shift" the graphics leftwards, enter X > ', XScaleLow:0:2); Writeln();
    // Number of iterations (determines graphics details and colors)
    repeat
      Write ('Number of iterations (default = 256; 0 to quit) ? '); Readln(S);
      if S = '' then
        NIters := NColors
      else
        NIters := StrToInt(S);
    until (NIters = 0) or (NIters >= 16);
    if NIters > 0 then begin
      // Graphics zoom factor
      repeat
        Write('Zoom (or ENTER for default = 1)                 ? '); Readln(S);
        if S = '' then
          Zoom := 1
        else
          Zoom := StrToFloat(S);
      until (Zoom >= 1);
      XYZoom := (YSMax / 3) * Zoom;                                            // (YSMax / 3) * 1 centers the full Mandelbrot set in the graphics window
      // Starting X value within the Mandelbrot set scale
      repeat
        Write('X scale value (or ENTER for default = -2.5)     ? '); Readln(S);
        if S = '' then
          XScale0 := XScaleLow
        else
          XScale0 := StrToFloat(S);
      until (XScale0 >= XScaleLow -(XScaleHigh - XScaleLow)) and (XScale0 <= XScaleHigh);
      Writeln; Write('Hit ENTER to continue... ');
      // Init graphics driver
      GM := m800x600; GD := D8bit;                                             // 800 x 600 pixles, 256 colors
      InitGraph(GD, GM, '');                                                   // graph initialization: this opens Graphics output window
      // Check if selected graphics mode is supported
      Error := GraphResult;
      if (Error <> grOk) then begin
        Writeln; Writeln; Writeln ('Graphics error: 640x480x256 graphics mode not supported!');
        Readln;
        Halt (1);
      end;
      // Mandelbrot set in 800x600x256 graphics mode
      YSH := YSMax div 2;                                                      // vertical window center
      for YS := 0 to YSH - 1 do begin
        // For each half (as graphics is symetric) graphics window pixel Y-value
        Y0 := YS / XYZoom;                                                     // Y-value scaled to Mandelbrot set
        for XS := 0 to XSMax - 1 do begin
          // For each graphics window pixel X-value
          X0 := XS / XYZoom + XScale0;                                         // X-value scaled to Mandelbrot set
          X := X0; Y := Y0; Iter := 0;                                         // initial X/Y-values
          // Iterate = calculate next value of (complex numbers) series...
          repeat
            T := X;
            X := Sqr(X) - Sqr(Y) + X0;                                         // new X-value
            Y := 2 * T * Y + Y0;                                               // new Y-value
            Inc(Iter);
          // ... until the series diverges or the (chosen) maximum of iterations is reached
          until (Sqr(X) + Sqr(Y) > 4) or (Iter = NIters);
          XYColor := Iter mod NColors;                                         // pixel color depending on number of iterations done
          PutPixel(XS, YSH - YS, XYColor);                                     // display the pixel in upper half graphics window
          PutPixel(XS, YSH + YS, XYColor);                                     // display the pixel in lower half graphics window
        end;
      end;
      // Return to text mode
      Readln;
      CloseGraph;
    end;
  until (NIters = 0);
end.
