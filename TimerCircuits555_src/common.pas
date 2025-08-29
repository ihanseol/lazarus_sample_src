{*********************************************************}
{* Common routines unit for TimerCircuits555 application *}
{*********************************************************}

unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics;

type
  TTimerPins = array[1..8] of Real;
  TStdResistors  = array[1..162] of Real;
  TPStdResistors = ^TStdResistors;

const
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;

function RFormat(R: Real; F: Integer): string;
function IsStandardResistor(pStdResistors: TPStdResistors; R: Real): Boolean;
function GetStandardResistor(pStdResistors: TPStdResistors; R, C, T: Real): Real;
procedure TimerAndLeds(var TimerPins: TTimerPins; var Led1, Led2: TShape);

implementation

{ Calculate (integer) power of a number }

function Power(R: Real; N: Integer): Real;

var
  I: Integer;
  Pow: Real;

begin
  Pow := 1;
  for I := 1 to Abs(N) do
    Pow *= R;
  if N < 0 then
    Pow := 1 / Pow;
  Result := Pow;
end;

{ Format a real number (output as formatted string) }

function RFormat(R: Real; F: Integer): string;

// If the number less than 10^(-F), use scientific notation, otherwise
// display the number as fixed point value with F decimal disgits
// Suppress non-significant digital digits

var
  R0: Real;
  SR: string;

begin
  SR := '';
  if F >= 0 then begin
    R0 := Round(R * Power(10, F)) / Power(10, F);
    if R0 < Power(10, -F) then
      SR := FloatToStrF(R, ffExponent, F, 0)
    else
      SR := FloatToStr(R0);
  end;
  Result := SR;
end;

{ Check if a given resiatance is a standard resistor value }

function IsStandardResistor(pStdResistors: TPStdResistors; R: Real): Boolean;

var
  I: Integer;
  ItIs: Boolean;

begin
  ItIs := False;
  for I := 1 to 162 do begin
    if R = pStdResistors^[I] then
      ItIs := True;
  end;
  Result := ItIs;
end;

{ Determine the best standard resistor value (for a given resistance) }

function GetStandardResistor(pStdResistors: TPStdResistors; R, C, T: Real): Real;

var
  I: Integer;
  RStd, T1, T2: Real;

begin
  RStd := R;
  if not IsStandardResistor(pStdResistors, R) then begin
    if R < pStdResistors^[1] then
      RStd := pStdResistors^[1]
    else if R > pStdResistors^[162] then
      RStd := pStdResistors^[162]
    else begin
      for I := 1 to 161 do begin
        // Find standard resistor for which the time period is as close a possible to tha value wanted
        if (R > pStdResistors^[I]) and (R < pStdResistors^[I + 1]) then begin
          T1 := 1.1 * pStdResistors^[I] * C; T2 := 1.1 * pStdResistors^[I + 1] * C;
          if Abs(T - T1) <= Abs(T - T2) then
            RStd := pStdResistors^[I]
          else
            RStd := pStdResistors^[I + 1];
        end;
      end;
    end;
  end;
  Result := RStd;
end;

{ Turn given LED on }

procedure LedOn(var Led: TShape);

begin
  Led.Brush.Style := bsSolid;
end;

{ Turn given LED off }

procedure LedOff(var Led: TShape);

begin
  Led.Brush.Style := bsDiagCross;
end;

{ Turn given LED on or off (depending on LED connection-type and timer output) }

procedure LedOnOff(var Led: TShape; Connection: string; TimerOutput: Real);

begin
  if Connection = 'source' then begin
    if TimerOutput = 0 then
      LedOn(Led)
    else
      LedOff(Led);
  end
  else begin
    if TimerOutput = 15 then
      LedOn(Led)
    else
      LedOff(Led);
  end;
end;

{ IC-555 simulation routine }

procedure Timer555(var Pins: TTimerPins);

// The routine does not consider the internal components of the 555
// It simply sets output (3) and discharge (7) pins, according to the values at the input pins

var
  Pin3: Real;

begin
  Pin3 := Pins[3];
  if Pins[4] = 0 then
    // If Reset (pin 4) is low, output will low
    Pins[3] := 0
  else begin
    if Pins[2] < (1/3) * Pins[8] then
      // If Trigger (pin 2) less than 1/3 VS, output will be high
      Pins[3] := Pins[8]
    else if Pins[6] > (2/3) * Pins[8] then
      // If Threshold (pin 6) gretaer than 2/3 VS, output will be low
      Pins[3] := 0
    else
      // Otherwise: output will not change
      Pins[3] := Pin3;
  end;
  Pins[7] := Pins[3];                                                          // discharge pin (7) value = output pin (3) value
end;

{ Determine IC-555 ouput value (pin 3) and switch LEDs accordingly on or off }

procedure TimerAndLeds(var TimerPins: TTimerPins; var Led1, Led2: TShape);

begin
  // Determine timer output
  Timer555(TimerPins);
  // Switch the lights on/off
  LedOnOff(Led1, 'source', TimerPins[3]);                                      // LED connected to +VS
  LedOnOff(Led2, 'sink', TimerPins[3]);                                        // LED connected to 0V
end;

end.

