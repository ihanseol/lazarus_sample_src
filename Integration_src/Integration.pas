{*************************}
{* Numerical integration *}
{*************************}

unit Integration;

interface

type
  TFunction = function(X: Real): Real;

function Integrate(F: TFunction; A, B: Real; Tol: Real = 1E-9; Depth: Integer = 1000): Real;
function IntegralArea(SF: string; A, B: Real; FN: string = 'plot'): Boolean;

implementation  

uses
  SysUtils, Process;

var
  FA, FB, M, FM, Whole: Real;

{--------------------------------------------------------------------------}
{ Numerical integration using the Adaptive Simpson's method (Lyness, 1969) }
{--------------------------------------------------------------------------}

function Integrate(F: TFunction; A, B: Real; Tol: Real = 1E-9; Depth: Integer = 1000): Real;

// This is a re-write of the code published at the "Rosetta Code" website
// (https://rosettacode.org/wiki/Numerical_integration/Adaptive_Simpson%27s_method#Pascal)

  procedure Integrate_Simpsons_(A, FA, B, FB: Real; var M, FM, Quadval: Real);
  begin
    M := (A + B) / 2;
    FM := F(M);
    Quadval := ((B - A) / 6) * (FA + (4 * FM) + FB);
  end;

  function Integrate_(A, FA, B, FB, Tol, Whole, M, FM: Real; Depth: Integer): Real;
  var
    LM, FLM, Left, RM, FRM, Right, Delta, Tol_, Int: Real;
  begin
    Integrate_Simpsons_(A, FA, M, FM, LM, FLM, Left);
    Integrate_simpsons_(M, FM, B, FB, RM, FRM, Right);
    Delta := Left + Right - Whole;
    Tol_ := Tol / 2;
    if (Depth <= 0) or (Tol_ = Tol) or (Abs(Delta) <= 15 * Tol) then
      Int := Left + Right + (Delta / 15)
    else
      Int := (Integrate_(A, FA, M, FM, Tol_, Left, LM, FLM, Depth - 1) + Integrate_(M, FM, B, FB, Tol_, Right, RM, FRM, Depth - 1));
    Result := Int;
  end;

begin
  FA := F(A);
  FB := F(B);
  Integrate_simpsons_(A, FA, B, FB, M, FM, Whole);
  Result := Integrate_(A, FA, B, FB, Tol, Whole, M, FM, Depth)
end;

{------------------------------------------}
{ Plot of the graph with the integral area }
{------------------------------------------}

function IntegralArea(SF: string; A, B: Real; FN: string = 'plot'): Boolean;

// The plot is done by calling Gnuplot

var
  S: string;
  OK: Boolean;
  OutFile: Text;

begin
  OK := True;
  FN += '.png';
  Assign(OutFile, 'plot.plt'); Rewrite(OutFile);
  Writeln(OutFile, 'set terminal png size 1080,620');
  Writeln(OutFile, 'set output "', FN, '"');
  Writeln(OutFile, 'set grid');
  Writeln(OutFile, 'set key horizontal outside center bottom');
  Writeln(OutFile, 'plot [', FloatToStr(A), ':', FloatToStr(B), '] ',
    SF, ' w filledc y1=0 lc "cyan" t "Integral area",',
    SF, ' w lines lc "blue" lw 3 t "Graph"');
  Close(OutFile);
  if FileExists(FN) then
    DeleteFile(FN);
  OK := RunCommand('gnuplot', ['plot.plt'], S);
  Result := OK;
end;

end.

