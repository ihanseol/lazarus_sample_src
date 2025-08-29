{*****************************************}
{ Main unit for RGBColorMixer application }
{*****************************************}

unit rgbled;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, ComCtrls, help;

type
  TSwitch = record
    SwitchShH, SwitchShV: TShape;
    SwitchIm: TImage;
    SwitchPos: array[0..3] of Integer;
    SwitchStatus: Boolean;
  end;
  TRGB = record
    Red, Green, Blue: Integer;
    XRed, XGreen, XBlue: string;
    Colour: TColor;
    ColourName: string;
  end;
  {*****************}
  { TfRGBColorMixer }
  {*****************}
  TfRGBColorMixer = class(TForm)
    mMenu: TMainMenu;
    mSimulation, mSimulationResetAllOn, mSimulationResetAllOff, mSimulationResetRed: TMenuItem;
    mSimulationResetGreen, mSimulationResetBlue, mSimulationExit: TMenuItem;
    mOptions, mOptionsLed, mOptionsLedCathode, mOptionsLedAnode: TMenuItem;
    mOptionsRGB, mOptionsRGBDec, mOptionsRGBHex, mOptionsNoBrightness: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    imPowerSupply1, imPowerSupply2, imPowerSupply3, imSwitch: TImage;
    Image7, Image8, Image9, Image11: TImage;
    shLed, shSwitchH, shSwitchV: TShape;
    Shape1, Shape2, Shape3, Shape4, Shape5, Shape6, Shape7, Shape8, Shape9: TShape;
    Shape10, Shape11, Shape12, Shape13, Shape14, Shape15, Shape16, Shape17: TShape;
    Shape18, Shape19, Shape20, Shape23, Shape24, Shape25, Shape26, Shape27: TShape;
    Shape28, Shape29, Shape30, Shape32, Shape40, Shape41, Shape42, Shape21: TShape;
    Shape22, Shape31, Shape33, Shape34, Shape43, Shape44, Shape45, Shape46: TShape;
    tbPot1, tbPot2, tbPot3: TTrackBar;
    laVoltage1, laVoltage2: TLabel;
    laT1, laT2, laT3, laP1, laP2, laP3: TLabel;
    laLed, laLedColor, laPot1, laPot2, laPot3: TLabel;
    Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    Label12, Label13, Label14, Label15: TLabel;
    edPot1, edPot2, edPot3, edRed, edGreen, edBlue: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure mSimulationResetAllOnClick(Sender: TObject);
    procedure mSimulationResetAllOffClick(Sender: TObject);
    procedure mSimulationResetRedClick(Sender: TObject);
    procedure mSimulationResetGreenClick(Sender: TObject);
    procedure mSimulationResetBlueClick(Sender: TObject);
    procedure mSimulationExitClick(Sender: TObject);
    procedure mOptionsLedCathodeClick(Sender: TObject);
    procedure mOptionsLedAnodeClick(Sender: TObject);
    procedure mOptionsRGBDecClick(Sender: TObject);
    procedure mOptionsRGBHexClick(Sender: TObject);
    procedure mOptionsNoBrightnessClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure imSwitchClick(Sender: TObject);
    procedure tbPot1Change(Sender: TObject);
    procedure tbPot2Change(Sender: TObject);
    procedure tbPot3Change(Sender: TObject);
  private
    rdSwitch: TSwitch;
    sLedType: string;
  end;

const
  SwitchPos: array[0..3] of Integer = (616, 600, 596, 9);

var
  fRGBColorMixer: TfRGBColorMixer;

implementation

{$R *.lfm}

{ Hexadecimal value of an integer between 0 and 255}

function DecToHex255(N: Integer): string;

const
  Digits = '0123456789ABCDEF';

begin
  Result := Digits[N div 16 + 1] + Digits[N mod 16 + 1];
end;

{ Get LED RGB value for fiven resistance  of the ´three potis (position of the three trackbars) }

function PotisToRBG(PosPot1, PosPot2, PosPot3: Integer): TRGB;

var
  RGB: TRGB;

begin
  RGB.Red := Round(PosPot1 * 255 / 10000); RGB.Green := Round(PosPot2 * 255 / 10000); RGB.Blue := Round(PosPot3 * 255 / 10000);
  RGB.XRed := DecToHex255(RGB.Red); RGB.XGreen := DecToHex255(RGB.Green); RGB.XBlue := DecToHex255(RGB.Blue);
  RGB.Colour := RGBToColor(RGB.Red, RGB.Green, RGB.Blue);
  Result := RGB;
end;

{ Reset circuit to predefined situation }

procedure CircuitReset(LedType, RType: string);

begin
  // Set trackbar positions, corresponding to predefined situation
  if RType = 'off' then begin
    fRGBColorMixer.tbPot1.Position := 0; fRGBColorMixer.tbPot2.Position := 0; fRGBColorMixer.tbPot3.Position := 0;
  end
  else if RType = 'on' then begin
    fRGBColorMixer.tbPot1.Position := 10000; fRGBColorMixer.tbPot2.Position := 10000; fRGBColorMixer.tbPot3.Position := 10000;
  end
  else if RType = 'red' then begin
    fRGBColorMixer.tbPot1.Position := 10000; fRGBColorMixer.tbPot2.Position := 0; fRGBColorMixer.tbPot3.Position := 0;
  end
  else if RType = 'green' then begin
    fRGBColorMixer.tbPot1.Position := 0; fRGBColorMixer.tbPot2.Position := 10000; fRGBColorMixer.tbPot3.Position := 0;
  end
  else if RType = 'blue' then begin
    fRGBColorMixer.tbPot1.Position := 0; fRGBColorMixer.tbPot2.Position := 0; fRGBColorMixer.tbPot3.Position := 10000;
  end;
  // Change circuit layout depending on RGB LED type
  if LedType = 'anode' then begin
    // Common anode RGB LED
    fRGBColorMixer.laLed.Caption := 'Common anode RGB LED';
    // Change voltage polarity
    fRGBColorMixer.laVoltage1.Caption := 'GND'; fRGBColorMixer.laVoltage2.Caption := '+5V';
    // Change power supply unit
    fRGBColorMixer.imPowerSupply1.Picture.LoadFromFile('power_supply2.jpg');
    fRGBColorMixer.imPowerSupply2.Picture.LoadFromFile('power_supply2.jpg');
    fRGBColorMixer.imPowerSupply3.Picture.LoadFromFile('power_supply2.jpg');
    // Change transistors (usage of PNP transistors)
    fRGBColorMixer.laT1.Caption := StringReplace(fRGBColorMixer.laT1.Caption, '8050', '8550', []);
    fRGBColorMixer.laT2.Caption := StringReplace(fRGBColorMixer.laT2.Caption, '8050', '8550', []);
    fRGBColorMixer.laT2.Caption := StringReplace(fRGBColorMixer.laT2.Caption, '8050', '8550', []);
  end
  else begin
    // Common cathode RGB LED
    fRGBColorMixer.laLed.Caption := 'Common cathode RGB LED';
    // Change voltage polarity
    fRGBColorMixer.laVoltage1.Caption := '+5V'; fRGBColorMixer.laVoltage2.Caption := 'GND';
    // Change power supply unit
    fRGBColorMixer.imPowerSupply1.Picture.LoadFromFile('power_supply.jpg');
    fRGBColorMixer.imPowerSupply2.Picture.LoadFromFile('power_supply.jpg');
    fRGBColorMixer.imPowerSupply3.Picture.LoadFromFile('power_supply.jpg');
    // Change transistors (usage of NPN transistors)
    fRGBColorMixer.laT1.Caption := StringReplace(fRGBColorMixer.laT1.Caption, '8550', '8050', []);
    fRGBColorMixer.laT2.Caption := StringReplace(fRGBColorMixer.laT2.Caption, '8550', '8050', []);
    fRGBColorMixer.laT2.Caption := StringReplace(fRGBColorMixer.laT2.Caption, '8550', '8050', []);
  end;
end;

{ Open or close the switch }

procedure DoSwitch(var Switch: TSwitch; Status: string);

begin
  fRGBColorMixer.laLedColor.Visible := False;
  // Open or close the switch, as indicated by procedure parameter
  if (Status = 'on') or (Status = 'off') then begin
    if Status = 'on' then
      Switch.SwitchStatus := True
    else
      Switch.SwitchStatus := False;
  end
  // Open a closed switch, resp. close an opened switch
  else begin
    Switch.SwitchStatus := not Switch.SwitchStatus;
  end;
  // Closed switch components position
  if Switch.SwitchStatus = True then begin
    Switch.SwitchShH.Top := Switch.SwitchPos[0] + Switch.SwitchPos[3];
    Switch.SwitchShV.Top := Switch.SwitchPos[1] + Switch.SwitchPos[3];
    Switch.SwitchIm.Top  := Switch.SwitchPos[2] + Switch.SwitchPos[3];
  end
  // Opened switch components position
  else begin
    Switch.SwitchShH.Top := Switch.SwitchPos[0];
    Switch.SwitchShV.Top := Switch.SwitchPos[1];
    Switch.SwitchIm.Top  := Switch.SwitchPos[2];
  end;
end;

{ Do the simulation }

procedure DoSimulation(Switch: TSwitch; PosPot1, PosPot2, PosPot3: Integer);

const
  NColors = 40;
  HTMLColorCodes: array[0..NColors - 1] of string = (
    '#FFC0CB', '#FF69B4', '#FF1493', '#EE82EE', '#FF00FF', '#800080', '#9400D3', '#4B0082',
    '#FF0000', '#8B0000', '#FFA500', '#FF8C00', '#FF4500', '#FFD700', '#FFFF00', '#FFFFE0',
    '#F0E68C', '#BDB76B', '#ADFF2F', '#00FF00', '#90EE90', '#008000', '#006400', '#9ACD32',
    '#008080', '#00FFFF', '#E0FFFF', '#ADD8E6', '#0000FF', '#00008B', '#D2B48C', '#A52A2A',
    '#800000', '#FFFFFF', '#F5F5DC', '#D3D3D3', '#C0C0C0', '#A9A9A9', '#808080', '#000000'
  );
  HTMLColorNames: array[0..NColors - 1] of string = (
    'pink', 'hot pink', 'deep pink', 'violet', 'magenta', 'purple', 'dark violet', 'indigo',
    'red', 'dark red', 'orange', 'dark orange', 'orange-red', 'gold', 'yellow', 'light yellow',
    'khaki', 'dark khaki', 'green-yellow', 'lime', 'light green', 'green', 'dark green', 'yellow-green',
    'teal', 'cyan', 'light cyan', 'light blue', 'blue', 'dark blue', 'tan', 'brown',
    'maroon', 'white', 'beige', 'light gray', 'silver', 'dark gray', 'gray', 'black'
  );

var
  I: Integer;
  RGB: TRGB;

begin
  // If the switch is open, the RGB LED is off
  if Switch.SwitchStatus = False then begin
    fRGBColorMixer.edRed.Text := ''; fRGBColorMixer.edGreen.Text := ''; fRGBColorMixer.edBlue.Text := '';
    fRGBColorMixer.shLed.Brush.Color := clForm;
    fRGBColorMixer.laLedColor.Visible := True; fRGBColorMixer.laLedColor.Caption := ' LED is off ';
  end
  // If the switch is closed, the RGB LED outputs a given color
  else begin
    fRGBColorMixer.laLedColor.Visible := False;
    RGB := PotisToRBG(PosPot1, PosPot2, PosPot3);                              // Get RGB values for actual poti resistances
    if fRGBColorMixer.mOptionsRGBDec.Checked then begin
      // Display decimal RGB values
      fRGBColorMixer.edRed.Text := IntToStr(RGB.Red);
      fRGBColorMixer.edGreen.Text := IntToStr(RGB.Green);
      fRGBColorMixer.edBlue.Text := IntToStr(RGB.Blue);
    end
    else begin
      // Display hexadecimal RGB values
      fRGBColorMixer.edRed.Text := RGB.XRed;
      fRGBColorMixer.edGreen.Text := RGB.XGreen;
      fRGBColorMixer.edBlue.Text := RGB.XBlue;
    end;
    // Set the color of the light
    fRGBColorMixer.shLed.Brush.Color := RGB.Colour;
    // If this option is selected, set LED color brightness
    if fRGBColorMixer.mOptionsNoBrightness.Checked then begin
      fRGBColorMixer.shLed.Brush.Style := bsSolid;
    end
    else begin
      if (PosPot1 < 2000) and ((PosPot2 < 2000)) and (PosPot3 < 2000) then
        fRGBColorMixer.shLed.Brush.Style := bsBDiagonal
      else if (PosPot1 < 4000) and ((PosPot2 < 4000)) and (PosPot3 < 4000) then
        fRGBColorMixer.shLed.Brush.Style := bsDiagCross
      else
        fRGBColorMixer.shLed.Brush.Style := bsSolid;
    end;
    // Display the RGB LED color name (for those colors with named defined in HTMLColorNames array)
    for I := 0 to NColors - 2 do begin
      if (RGB.XRed = Copy(HTMLColorCodes[I], 2, 2)) and (RGB.XGreen = Copy(HTMLColorCodes[I], 4, 2)) and (RGB.XBlue = Copy(HTMLColorCodes[I], 6, 2)) then begin
        fRGBColorMixer.laLedColor.Visible := True;
        fRGBColorMixer.laLedColor.Caption := ' LED color = ' + HTMLColorNames[I] + ' ';
      end;
    end;
  end;
end;

{*****************}
{ TfRGBColorMixer }
{*****************}

{ Application start: Initializations }

procedure TfRGBColorMixer.FormCreate(Sender: TObject);

const
  SUB_1 = #$E2#$82#$81;
  SUB_2 = #$E2#$82#$82;
  SUB_3 = #$E2#$82#$83;

begin
  // Apply subscripts (potis)
  laP1.Caption := StringReplace(laP1.Caption, '1', SUB_1, []);
  laP2.Caption := StringReplace(laP2.Caption, '2', SUB_2, []);
  laP3.Caption := StringReplace(laP3.Caption, '3', SUB_3, []);
  laPot1.Caption := StringReplace(laPot1.Caption, '1', SUB_1, []);
  laPot2.Caption := StringReplace(laPot2.Caption, '2', SUB_2, []);
  laPot3.Caption := StringReplace(laPot3.Caption, '3', SUB_3, []);
  // Initialize the rdSwitch record
  rdSwitch.SwitchShH := shSwitchH; rdSwitch.SwitchShV := shSwitchV; rdSwitch.SwitchIm := imSwitch;
  rdSwitch.SwitchPos := SwitchPos;
  rdSwitch.SwitchStatus := False;
  // Actual RGB LED type is common cathode LED
  sLedType := 'cathode';
end;

{ Menu item "Simulation > Reset all off": All internal LEDs off situation }

procedure TfRGBColorMixer.mSimulationResetAllOffClick(Sender: TObject);

begin
  CircuitReset(sLedType, 'off');
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

{ Menu item "Simulation > Reset all on": All internal LEDs at a maximum intensity situation }

procedure TfRGBColorMixer.mSimulationResetAllOnClick(Sender: TObject);

begin
  CircuitReset(sLedType, 'on');
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

{ Menu item "Simulation > Reset red": Internal red LED at a maximum intensity (others off) situation }

procedure TfRGBColorMixer.mSimulationResetRedClick(Sender: TObject);

begin
  CircuitReset(sLedType, 'red');
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

{ Menu item "Simulation > Reset green": Internal green LED at a maximum intensity (others off) situation }

procedure TfRGBColorMixer.mSimulationResetGreenClick(Sender: TObject);

begin
  CircuitReset(sLedType, 'green');
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

{ Menu item "Simulation > Reset blue": Internal blue LED at a maximum intensity (others off) situation }

procedure TfRGBColorMixer.mSimulationResetBlueClick(Sender: TObject);

begin
  CircuitReset(sLedType, 'blue');
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

{ Menu item "Simulation > Exit": Exit application }

procedure TfRGBColorMixer.mSimulationExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > RGB LED type > Common cathode": Adapt circuit for usage of a common cathode RGB LED }

procedure TfRGBColorMixer.mOptionsLedCathodeClick(Sender: TObject);

begin
  mOptionsLedAnode.Checked := False; mOptionsLedCathode.Checked := True;
  sLedType := 'cathode';
  // Turn power off and reset the circuit (all internal LEDs off)
  DoSwitch(rdSwitch, 'off');
  CircuitReset(sLedType, 'off');
end;

{ Menu item "Options > RGB LED type > Common anode": Adapt circuit for usage of a common anode RGB LED }

procedure TfRGBColorMixer.mOptionsLedAnodeClick(Sender: TObject);

begin
  mOptionsLedAnode.Checked := True; mOptionsLedCathode.Checked := False;
  sLedType := 'anode';
  // Turn power off and reset the circuit (all internal LEDs off)
  DoSwitch(rdSwitch, 'off');
  CircuitReset(sLedType, 'off');
end;

{ Menu item "Options > RGB values display > Decimal": Select to display decimal RGB values }

procedure TfRGBColorMixer.mOptionsRGBDecClick(Sender: TObject);

begin
  mOptionsRGBDec.Checked := True; mOptionsRGBHex.Checked := False;
end;

{ Menu item "Options > RGB values display > Hexadecimal": Select to display hexadecimal RGB values }

procedure TfRGBColorMixer.mOptionsRGBHexClick(Sender: TObject);

begin
  mOptionsRGBDec.Checked := False; mOptionsRGBHex.Checked := True;
end;

{ Menu item "Options > Disable LED brightness": Toggle to use or not the LED brighness feature }

procedure TfRGBColorMixer.mOptionsNoBrightnessClick(Sender: TObject);

begin
  if mOptionsNoBrightness.Checked then
    mOptionsNoBrightness.Checked := False
  else
    mOptionsNoBrightness.Checked := True;
end;

{ Menu item "Help > Help": Display application help text }

procedure TfRGBColorMixer.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display application about }

procedure TfRGBColorMixer.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Electronics:' + LineEnding;
  S += 'Simple simulation of a color mixing circuit with common anode or common cathode RBG LED.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, October-November 2023.';
  MessageDlg('About "RGBColorMixer"', S, mtInformation, [mbOK], 0);
end;

{ User pusching the power switch (click on switch transparent image): Open resp. close the switch and update simulation }

procedure TfRGBColorMixer.imSwitchClick(Sender: TObject);

begin
  DoSwitch(rdSwitch, 'switch');
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

{ User changing one of the poti resistances (change of one of the trackbar values): Display resistance and update simulation }

procedure TfRGBColorMixer.tbPot1Change(Sender: TObject);

begin
  edPot1.Text := IntToStr(tbPot1.Position);
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

procedure TfRGBColorMixer.tbPot2Change(Sender: TObject);

begin
  edPot2.Text := IntToStr(tbPot2.Position);
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

procedure TfRGBColorMixer.tbPot3Change(Sender: TObject);

begin
  edPot3.Text := IntToStr(tbPot3.Position);
  DoSimulation(rdSwitch, tbPot1.Position, tbPot2.Position, tbPot3.Position);
end;

end.

