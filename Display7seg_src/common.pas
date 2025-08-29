{***********************************************************}
{* Common electronics routines for Display7seg application *}
{***********************************************************}

unit common;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, math;

type
  TSwitchData = record                                                         // switch description
    Name: string;                                                                 // objects base name
    Data: array[0..2] of Integer;                                                 // objects switch off "top" position: transparent image, long bar shape, short bar shape
    Voltage: Boolean;                                                             // switch connects to +Vs (True) resp. 0V (False)
  end;
  TSwitch = record                                                             // switch
    SwitchImg: TImage;                                                            // transparent image object
    SwitchBLong, SwitchBShort: TShape;                                            // long and short bar shape objects
    SwitchStatus: string;                                                         // switch status: "on" or "off"
    SwitchVoltage: Boolean;                                                       // switch "output": +V (True) resp. 0V (False)
  end;
  TSwitches = array of TSwitch;                                                // array with "switch" elements
  TLED = record                                                                // LED
    LEDShape: TShape;                                                             // shape object
    LEDStatus: string;                                                            // LED status: "on" or "off"
    LEDVoltage: Boolean;                                                          // LED voltage: on = True, off = False
  end;
  TLEDs = array of TLED;                                                       // array with "LED" elements
  TSegment = record                                                            // Display segment
    SegmentShape: TShape;                                                         // shape object
    SegmentStatus: string;                                                        // segment status: "on" or "off"
    SegmentVoltage: Boolean;                                                      // segment voltage: on = True, off = False
  end;
  TSegments = array['a'..'g'] of TSegment;                                     // array with "segment" elements
  TDisplay7S = record                                                          // 7-segment display
    DisplayType: string;                                                          // type: "cc" or "ca"
    CommonVoltage: Boolean;                                                       // voltage at the common LED connection: +VS (True) resp. 0V (False)
    Segments: TSegments;                                                          // array with "segment" elements
    LedVoltages: array['a'..'g'] of Boolean;                                      // voltage at the individual LED connections: +V (True) resp. 0V (False)
  end;
  TIC4511 = record                                                             // BCD to 7-segments decoder IC (for CC display)
    LT, BL, LE: Boolean;                                                          // LT, BL, LE pins
    Inputs: array[1..4] of Boolean;                                               // BCD input pins
    Outputs: array['a'..'g'] of Boolean;                                          // 7-segments output pins
  end;
  TIC7447 = record                                                             // BCD to 7-segments decoder IC (for CA display)
    LT, BI_RBO, RBI: Boolean;                                                     // LT, BI/RBO, RBI pins
    Inputs: array[1..4] of Boolean;                                               // BCD input pins
    Outputs: array['a'..'g'] of Boolean;                                          // 7-segments output pins
  end;
  TIC7490 = record                                                             // 7-segment display counter
    Count: Integer;                                                               // Impulse counter (0 .. 9)
    Outputs: array[1..4] of Boolean;                                              // Counter output pins
  end;

const
  DisplaySegments = 'abcdefg';                                                 // 7-segment display pin numbering string
  SegOutputs: array[0..9] of string[7] = (                                     // segments to turn on (1) or off (0) for numbers 0 - 9
    '1111110', '0110000', '1101101', '1111001', '0110011',
    '1011011', '0011111', '1110000', '1111111', '1110011'
  );
  SUB_Digits: array[0..9] of string = (                                        // UTF-8 number subscripts
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84,
    #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

procedure SwitchOnOff(DSwitches: array of TSwitchData; var Switch: TSwitch; Position: string);
procedure SwitchesAllOnOff(DSwitches: array of TSwitchData; var Switches: TSwitches; Position: string);
procedure LedOnOff(var Led: TLed; Status: string);
procedure LedsAllOnOff(var Leds: TLeds; Status: string);
procedure SegmentOnOff(var Segment: TSegment; Status: string);
procedure SegmentsAllOnOff(var Segments: TSegments; Status: string);
procedure SegmentsZero(var Segments: TSegments);
procedure SegmentsOnOff(var Display7S: TDisplay7S);
procedure SimulIC4511(var IC4511: TIC4511);
procedure SimulIC7447(var IC7447: TIC7447);
procedure SimulIC7490(var IC7490: TIC7490);

implementation

{ Boolean to binary conversion }

function BooleanToBin(Bool: Boolean): Integer;

// Function output is Integer 0 or 1

var
  Bin: Integer;

begin
  if Bool then
    Bin := 1
  else
    Bin := 0;
  Result := Bin;
end;

{ Decimal to binary conversion }

function DecToBin(Dec: Integer): string;

// For numbers from 0 to 15 only!
// Function output is 4-digit string (made of 1s and 0s)

var
  N: Integer;
  Bin: string;

begin
  Bin := '0000';
  for N := 3 downto 0 do begin
    if Dec >= IntPower(2, N) then begin
      Bin[4 - N] := '1';
      Dec -= Round(IntPower(2, N));
    end;
  end;
  Result := Bin;
end;

{ Set given switch to position "on" or "off" }

procedure SwitchOnOff(DSwitches: array of TSwitchData; var Switch: TSwitch; Position: string);

var
  I, IX: Integer;

begin
  // Determine index for switch data array by looking up the actual switch image object's name
  for I := 0 to Length(DSwitches) - 1 do begin
    if DSwitches[I].Name = Copy(Switch.SwitchImg.Name, 3, Length(Switch.SwitchImg.Name)) then
      IX := I;
  end;
  // Switch image and shapes vertical "off" position (defined in its data)
  Switch.SwitchImg.Top    := DSwitches[IX].Data[0];
  Switch.SwitchBLong.Top  := DSwitches[IX].Data[1];
  Switch.SwitchBShort.Top := DSwitches[IX].Data[2];
  // Switch has to be pushed "on"
  if Position = 'on' then begin
    Switch.SwitchStatus := 'on';
    Switch.SwitchVoltage := DSwitches[IX].Voltage;
    // Adjust switch image and shapes vertical position
    Switch.SwitchImg.Top := Switch.SwitchImg.Top + 10;
    Switch.SwitchBLong.Top := Switch.SwitchBLong.Top + 10;
    Switch.SwitchBShort.Top := Switch.SwitchBShort.Top + 10;
  end
  // Switch has to be pushed "off"
  else begin
    Switch.SwitchStatus := 'off';
    Switch.SwitchVoltage := not DSwitches[IX].Voltage;
  end;
end;

{ Set all switches to "on" or "off" position }

procedure SwitchesAllOnOff(DSwitches: array of TSwitchData; var Switches: TSwitches; Position: string);

var
  I: Integer;

begin
  for I := 0 to Length(Switches) - 1 do begin
    // Set position for each switch (array element)
    SwitchOnOff(DSwitches, Switches[I], Position);
  end;
end;

{ Turn a given LED "on" or "off" }

procedure LedOnOff(var Led: TLed; Status: string);

begin
  Led.LEDStatus := Status;
  // LED has to be turned on
  if Led.LEDStatus = 'on' then begin
    Led.LEDVoltage := True;
    Led.LEDShape.Brush.Style := bsSolid;
  end
  // LED has to be turned off
  else begin
    Led.LEDVoltage := False;
    Led.LEDShape.Brush.Style := bsDiagCross;
  end;
end;

{ Turn all LEDs "on" or "off" }

procedure LedsAllOnOff(var Leds: TLeds; Status: string);

var
  I: Integer;

begin
  for I := 0 to Length(Leds) - 1 do begin
    // Set status for each LED (array element)
    LedOnOff(Leds[I], Status);
  end;
end;

{ Turn a given display segment "on" or "off" }

procedure SegmentOnOff(var Segment: TSegment; Status: string);

begin
  Segment.SegmentStatus := Status;
  // Segment has to be turned on
  if Segment.SegmentStatus = 'on' then begin
    Segment.SegmentVoltage := True;
    Segment.SegmentShape.Brush.Style := bsSolid;
  end
  // Segment has to be turned off
  else begin
    Segment.SegmentVoltage := False;
    Segment.SegmentShape.Brush.Style := bsDiagCross;
  end;
end;

{ Turn all display segments "on" or "off" }

procedure SegmentsAllOnOff(var Segments: TSegments; Status: string);

var
  C: Char;

begin
  for C := 'a' to 'g' do begin
    // Set status for each segment (array element)
    SegmentOnOff(Segments[C], Status);
  end;
end;

{ Turn display segments "on" resp. "off" in order to display a "0" }

procedure SegmentsZero(var Segments: TSegments);

var
  C: Char;

begin
  for C := 'a' to 'g' do begin
    // Set status for each segment (array element)
    if C in ['a' .. 'f'] then
      SegmentOnOff(Segments[C], 'on')
    else
      SegmentOnOff(Segments[C], 'off');
  end;
end;

{ Turn display segments "on" resp. "off", depending on the voltage at the corresponding pin (a-g) }

procedure SegmentsOnOff(var Display7S: TDisplay7S);

// To turn a LED on, the voltage at its pin must be the "opposite" of the common voltage:
// +Vs for CC displays, 0V (ground) for CA displays

var
  C: Char;

begin
  for C := 'a' to 'g' do begin
    if Display7S.LedVoltages[C] = Display7S.CommonVoltage then
      SegmentOnOff(Display7S.Segments[C], 'off')
    else
      SegmentOnOff(Display7S.Segments[C], 'on');
  end;
end;

{ IC 4511 simulation }

procedure SimulIC4511(var IC4511: TIC4511);

// Please, note that pins LT, BL and LE are not simulated in this version of the program
// They are supposed to be correctly connected in order to make the IC work as it should:
//   LT = high, BL = high, LE = low

var
  OutVal, I: Integer;
  C: Char;

begin
  // Calculate binary number corr. to the 4 inputs A - D
  OutVal := 0;
  for I := 1 to 4 do begin
    OutVal += BooleanToBin(IC4511.Inputs[I]) * Round(IntPower(2, I - 1));
  end;
  // For inputs 0000 to 1001, display the corr. decimal number
  if OutVal <= 9 then begin
    for I := 1 to 7 do begin
      // Outputs a - g (corr. to display pins a - g)
      if SegOutputs[OutVal][I] = '1' then
        IC4511.Outputs[DisplaySegments[I]] := True                             // turn segment on
      else
        IC4511.Outputs[DisplaySegments[I]] := False;                           // turn segment off
    end;
  end
  // For inputs 0000 to 1001, display nothing (all segments off)
  else begin
    for C := 'a' to 'g' do
      IC4511.Outputs[C] := False;
  end;
end;

{ IC 7447 simulation }

procedure SimulIC7447(var IC7447: TIC7447);

// Please, note that pins LT, BI/RBO and RBI are not simulated in this version of the program
// They are supposed to be correctly connected in order to make the IC work as it should:
//   LT = high, BI/RBO = high, RBI = high

const
  SegOutputs7447: array[10..15] of string[7] = (                               // a - g segments voltage for invalid input (1010 - 1111)
    '0001101', '0011001', '0100011', '1001011', '0001111', '0000000'
  );

var
  OutVal, I: Integer;
  SegOut: string;

begin
  // Calculate binary number corr. to the 4 inputs A - D
  OutVal := 0;
  for I := 1 to 4 do begin
    OutVal += BooleanToBin(IC7447.Inputs[I]) * Round(IntPower(2, I - 1));
  end;
  // Output pins corr. to a number (input 0000 - 1010), or to "some specific symbol" (input 1010 - 1111)
  if OutVal <= 9 then
    SegOut := SegOutputs[OutVal]
  else
    SegOut := SegOutputs7447[OutVal];
  // Output pins a - g (CA display: segment will be turned on by LOW signal!)
  for I := 1 to 7 do begin
    if SegOut[I] = '1' then
      IC7447.Outputs[DisplaySegments[I]] := False                              // turn segment on
    else
      IC7447.Outputs[DisplaySegments[I]] := True;                              // turn segment off
  end;
end;

{ IC 7490 simulation }

procedure SimulIC7490(var IC7490: TIC7490);

// This procedure does not really simulate the 7490 chip. It simply increments a counter variable
// and sets the IC outputs accordingly to the value of this variable

var
  I: Integer;
  OutVal: string;

begin
  // Increment counter (modulo 9)
  Inc(IC7490.Count);
  if IC7490.Count > 9 then
    IC7490.Count := 0;
  // Calculate binary number corr. to counter value
  OutVal := DecToBin(IC7490.Count);
  // Output the counter value to outputs QA - QD (QA is LSB!)
  for I := 1 to 4 do begin
    if OutVal[I] = '1' then
      IC7490.Outputs[5 - I] := True
    else
      IC7490.Outputs[5 - I] := False;
  end;
end;

end.

