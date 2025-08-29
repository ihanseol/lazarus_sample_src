{****************************}
{* Peter de Jong attractors *}
{****************************}

program dejong;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  crt, graph;

type
  TPoint = record
    X, Y : Real
  end;

const
  Width   = 1024;                                                              // screen width
  Height  = 768;                                                               // screen height
  DefaultCenter: TPoint = (X: 0; Y: 0);                                        // drawings default center
  Key_ESC = #27;                                                               // ESC key ASCII code

var
  Error: Integer;
  GD, GM: SmallInt;                                                            // graphics variables must be SmallInt
  A, B, C, D: Real;
  S: string;
  Key: Char;
  Center: TPoint;

{ Draw Peter de Jong attractor }

procedure PdJ(W, H: Integer; Center: TPoint; A, B, C, D: Real);

const
  N = 1000000;                                                                 // number of points to be calculated and drawn

var
  GX, GY, Colour, Count: Integer;
  P1, P2: TPoint;

begin
  Count := 0; Colour := Green;
  P1.X := 0; P1.Y := 0;                                                        // starting with point (0, 0)
  repeat
    // Determine next point coordinates, using de Jong's formulas
    P2.X := Sin(A * P1.Y) - Cos(B * P1.X);
    P2.Y := Sin(C * P1.X) - Cos(D * P1.Y);
    // Calculate corresponding position in the graphics window
    GX := Round(W div 2 + Center.X + (P2.X / 2) * (W / 2));
    GY := H - Round(H div 2 + Center.Y + (P2.Y / 2) * (H / 2));
    // Draw pixel at point's position
    PutPixel(GX, GY, Colour);
    // Actual point becomes the (new) point drawn
    P1 := P2;
    Inc(Count);
  until Count = N;
end;

{**************}
{ Main program }
{**************}

begin
  A := 0; B := 0; C := 0; D := 0;
  // Run the program until user pushes the ESC key
  repeat
    ClrScr;
    TextColor(Yellow);
    Writeln('Peter de Jong attractors');
    Writeln('========================');
    Writeln;
    TextColor(LightGray);
    Writeln('Attractor equations:');
    Writeln('X[n+1] = sin(aY[n]) - cos(bX[n])');
    Writeln('Y[n+1] = sin(cX[n]) - cos(dY[n])');
    Writeln;
    Writeln('For details, please look at the ReadMe file included with the .zip archive...'); Writeln;
    Writeln;
    // Enter parameters a, b, c, d; nothing entered -> parameter will keep previous value
    repeat
      Write('Parameter a ? '); Readln(S);
      if S <> '' then
        Val(S, A, Error);
    until (S = '') or (Error <= 1);
    repeat
      Write('Parameter b ? '); Readln(S);
      if S <> '' then
        Val(S, B, Error);
    until (S = '') or (Error <= 1);
    repeat
      Write('Parameter c ? '); Readln(S);
      if S <> '' then
        Val(S, C, Error);
    until (S = '') or (Error <= 1);
    repeat
      Write('Parameter d ? '); Readln(S);
      if S <> '' then
        Val(S, D, Error);
    until (S = '') or (Error <= 1);
    Writeln;
    // Enter graph center coordinates; nothing entered -> coordinates set to default values
    repeat
      Write('Graph center Xc ? '); Readln(S);
      if S = '' then
        Center.X := DefaultCenter.X
      else
        Val(S, Center.X, Error);
    until (S = '') or (Error <= 1);
    repeat
      Write('Graph center Yc ? '); Readln(S);
      if S = '' then
        Center.Y := DefaultCenter.Y
      else
        Val(S, Center.Y, Error);
    until (S = '') or (Error <= 1);
    Writeln; Write('"ESC" to exit, any other key continue... ');
    // Init graphics driver
    GM := m1024x768; GD := D8bit;                                              // 1024 x 768 pixles, 256 colors
    InitGraph(GD, GM, '');                                                     // graph initialization: this opens Graphics output window
    // Check if selected graphics mode is supported
    Error := GraphResult;
    if (Error <> grOk) then begin
      Writeln; Write ('Graphics error: 1024x768x256 graphics mode not supported! ');
      Readln;
      Halt (1);
    end;
    // Draw de Jong attractor
    PdJ(Width, Height, Center, A, B, C, D);
    // Exit on ESC key; restart on any other key pressed
    Key := ReadKey;
    if Key = #0 then
      Key := ReadKey;
    CloseGraph;
  until Key = Key_ESC;
end.

