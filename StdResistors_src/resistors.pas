{******************************************}
{* Main unit for StdResistors application *}
{******************************************}

unit resistors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, PopupNotifier, help;

type
  TColourShapes = array[0..11] of TShape;
  TRings = array[1..4] of TShape;
  TArray24 = array[1..24] of Integer;
  { TfResistors }
  TfResistors = class(TForm)
    mMenu: TMainMenu;
    mMenuFile: TMenuItem;
    mMenuFileExit: TMenuItem;
    mMenuHelp: TMenuItem;
    mMenuHelpHelp: TMenuItem;
    mMenuHelpAbout: TMenuItem;
    rbResistance: TRadioButton;
    rbResistor: TRadioButton;
    Label1, Label2, Label3: TLabel;
    edResistance: TEdit;
    cbUResistance: TComboBox;
    cbTolerance: TComboBox;
    shBlack: TShape;
    shBrown: TShape;
    shRed: TShape;
    shOrange: TShape;
    shYellow: TShape;
    shGreen: TShape;
    shBlue: TShape;
    shViolet: TShape;
    shGray: TShape;
    shWhite: TShape;
    shGold: TShape;
    shSilver: TShape;
    btClear: TButton;
    Shape1, Shape2, Shape3: TShape;
    shRing1: TShape;
    shRing2: TShape;
    shRing3: TShape;
    shRing4: TShape;
    memoResults: TMemo;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    shRRing1: TShape;
    shRRing2: TShape;
    shRRing3: TShape;
    shRRing4: TShape;
    btGo: TButton;
    btReset: TButton;
    pAbout: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mMenuFileExitClick(Sender: TObject);
    procedure mMenuHelpHelpClick(Sender: TObject);
    procedure mMenuHelpAboutClick(Sender: TObject);
    procedure btGoClick(Sender: TObject);
    procedure btResetClick(Sender: TObject);
    procedure shBlackMouseDown(Sender: TObject);
    procedure shBrownMouseDown(Sender: TObject);
    procedure shRedMouseDown(Sender: TObject);
    procedure shOrangeMouseDown(Sender: TObject);
    procedure shYellowMouseDown(Sender: TObject);
    procedure shGreenMouseDown(Sender: TObject);
    procedure shBlueMouseDown(Sender: TObject);
    procedure shVioletMouseDown(Sender: TObject);
    procedure shGrayMouseDown(Sender: TObject);
    procedure shWhiteMouseDown(Sender: TObject);
    procedure shGoldMouseDown(Sender: TObject);
    procedure shSilverMouseDown(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    iRing: Integer;
    bFirstHelp: Boolean;
    aStdResistances: TArray24;
  end;

const
  Colors: array[0..11] of string = (
    'black', 'brown', 'red', 'orange', 'yellow', 'green', 'blue', 'violet', 'gray', 'white', 'gold', 'silver'
  );

var
  fResistors: TfResistors;
  ColourShapes: TColourShapes;
  Rings, RRings: TRings;

implementation

{$R *.lfm}

{ Read standard resistor values from file }

procedure ReadStdResistors(var Resistances: TArray24);

var
  I: Integer;
  Resistance: string;
  ResistanceFile: Text;

begin
  Assign(ResistanceFile, 'resistors.txt'); Reset(ResistanceFile);
  I := 0;
  while not EoF(ResistanceFile) do begin
    Readln(ResistanceFile, Resistance);
    if Resistance <> '' then begin
      Inc(I);
      Resistances[I] := StrToInt(Resistance);
    end;
  end;
end;

{ Determine standard resistor nearest to given resistance value }

function StandardResistance(R: Real; Resistances: TArray24): Real;

var
  StdX, I: Integer;
  StdR, MinDiff: Real;

begin
  StdR := R;
  I := 0; MinDiff := R; StdX := 0;
  // Lookup the standard resistor table, and return the values that is nearest to the given R
  repeat
    Inc(I);
    if Abs(R - Resistances[I]) < MinDiff then begin
      MinDiff := Abs(R - Resistances[I]);
      StdX := I;
    end;
  until I = 162;
  if StdX <> 0 then
    StdR := Resistances[StdX];
  StandardResistance := StdR;
end;

{ Set given color for given resistor color ring }

procedure ResistorRings(var Ring: Integer; Colour: Integer);

var
  Ok: Boolean;

begin
  // Do this only if "determine resistance" is selected
  if fResistors.rbResistance.Checked then begin
    if Colour = -1 then begin
      // Color of -1 indicates that no ring has to be displayed
      if Ring >= 1 then begin
        Rings[Ring].Brush.Color := $003F85CD;                                  // $003F85CD is color of resistor itself
        Dec(Ring);
      end;
    end
    else begin
      if Ring < 4 then begin
        Inc(Ring); Ok := True;
        if Ring = 4 then begin
          // Tolerance ring: some values are n/a
          if Colour in [5..9] then begin
            MessageDlg('Invalid color', Colors[Colour] + ' is n/a for 4th ring!', mtError, [mbOK], 0);
            Ok := False;
            Dec(Ring);
          end;
        end;
        if Ok then
          // Set ring color
          Rings[Ring].Brush.Color := ColourShapes[Colour].Brush.Color;
      end;
    end;
  end;
end;

{***************}
{* TfResistors *}
{***************}

{ Application start: initialisation }

procedure TfResistors.FormCreate(Sender: TObject);

begin
  // Create array with color shapes
  ColourShapes[0] := shBlack;  ColourShapes[1] := shBrown; ColourShapes[2]  := shRed;  ColourShapes[3]  := shOrange;
  ColourShapes[4] := shYellow; ColourShapes[5] := shGreen; ColourShapes[6]  := shBlue; ColourShapes[7]  := shViolet;
  ColourShapes[8] := shGray;   ColourShapes[9] := shWhite; ColourShapes[10] := shGold; ColourShapes[11] := shSilver;
  // Create arrays with user and result rings
  Rings[1] := shRing1; Rings[2] := shRing2; Rings[3] := shRing3; Rings[4] := shRing4;
  RRings[1] := shRRing1; RRings[2] := shRRing2; RRings[3] := shRRing3; RRings[4] := shRRing4;
  // Read standard resistor values
  ReadStdResistors(aStdResistances);
  // Init actual ring number
  iRing := 0;
  // Init first call to help text routine
  bFirstHelp := True;
end;

{ Menu item "File > Exit": Exit application }

procedure TfResistors.mMenuFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display help text }

procedure TfResistors.mMenuHelpHelpClick(Sender: TObject);

begin
  // The first time help is called, load help text from file
  if bFirstHelp then begin
    fHelp.memoHelp.Lines.LoadFromFile('help.txt');
    bFirstHelp := False;
  end;
  // Show or hide help text
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfResistors.mMenuHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pAbout.Visible then
    pAbout.Hide
  else begin
    S := 'Electronics application:' + Chr(13);
    S += 'Standard resistors calculator' + Chr(13) + Chr(13);
    S += 'Version 1.0, © allu, April, 2018';
    pAbout.Title := 'About StdResistors';
    pAbout.Text := S;
    pAbout.Show;
  end;
end;

{ Button "Go!": Determine resistance }

procedure TfResistors.btGoClick(Sender: TObject);

var
  Colour, Tolerance, Code, P, I, J: Integer;
  Resistance, StdResistance, Mult: Real;
  UResistance, Multiplier, S: string;
  RingValues: array[1..4] of Integer;

begin
  memoResults.Clear;
  // Determine nearest standard resistor for given resistance value
  if rbResistor.Checked then begin
    Shape4.Visible := True; Shape5.Visible := True; Shape6.Visible := True;
    shRRing1.Visible := True; shRRing2.Visible := True; shRRing3.Visible := True; shRRing4.Visible := True;
    for I := 1 to 4 do
      Rings[I].Brush.Color := $003F85CD;                                       // clear all rings
    // Read resistance value
    if edResistance.Text = '' then
      Resistance := 0
    else begin
      S := edResistance.Text;
      // The Val procedure uses '.' as decimal separator; transform from ',' if necessary
      P := Pos(',', S);
      if P > 0 then
        S[P] := '.';
      Val(S, Resistance, Code);
    end;
    if Code <> 0 then
      Resistance := 0;
    // Determine nearest standard resistance
    if Resistance <= 0 then
      // Non numeric or invalid resistance entered
      MessageDlg('Invalid resistance', 'Resistance must have positive value!', mtError, [mbOK], 0)
    else begin
      // Resistance entered = ok; proceed...
      UResistance := cbUResistance.Text;
      // Display resistance value entered by user
      memoResults.Append(' Resistance = ' + FloatToStr(Resistance) + ' ' + UResistance);
      StdResistance := Resistance;
      // Get resistance value between 10 and 100 (for comparison with standard resistors table values)
      Mult := 1;
      if Resistance < 10 then begin
        while StdResistance < 10 do begin
          Mult *= 10;
          StdResistance := Mult * Resistance;
        end;
      end
      else if Resistance > 100 then begin
        while StdResistance > 100 do begin
          Mult /= 10;
          StdResistance := Mult * Resistance;
        end;
      end;
      // Determine standard resistor value
      StdResistance := StandardResistance(StdResistance, aStdResistances);
      StdResistance /= Mult;
      // Check if there exists an appropriate standard resistor corresponding to the resistance entered
      if (UResistance = 'Ω') and (StdResistance < 0.01) then begin
        if 0.01 - StdResistance > 0.002 then
          MessageDlg('Resistance out of range', 'Smallest standard resistor = 0,01 Ω!', mtWarning, [mbOK], 0);
        StdResistance := 0.01;
      end
      else if (UResistance = 'GΩ') and (StdResistance > 91) then begin
        if StdResistance - 91 > 0.2 * 91 then
          MessageDlg('Resistance out of range', 'Largest standard resistor = 91 GΩ!', mtWarning, [mbOK], 0);
        StdResistance := 91;
      end;
      // Display standard resistor value (adapting unit if appropriate)
      S := ' Nearest standard resistance = ' + FloatToStr(StdResistance) + ' ' + UResistance;
      if StdResistance < 1 then begin
        if UResistance <> 'Ω' then begin
          StdResistance *= 1000;
          if UResistance = 'GΩ' then
            UResistance := 'MΩ'
          else if UResistance = 'MΩ' then
            UResistance := 'kΩ'
          else
            UResistance := 'Ω';
          S += ' = ' + FloatToStr(StdResistance) + ' ' + UResistance;;
        end;
      end
      else if StdResistance > 1000 then begin
        if UResistance <> 'GΩ' then begin
          StdResistance /= 1000;
          if UResistance = 'Ω' then
            UResistance := 'kΩ'
          else if UResistance = 'kΩ' then
            UResistance := 'MΩ'
          else
            UResistance := 'GΩ';
          S += ' = ' + FloatToStr(StdResistance) + ' ' + UResistance;;
        end;
      end;
      memoResults.Append(S);
      Resistance := StdResistance;
      // Determine color of 1st and 2nd ring
      while Resistance < 1 do
        Resistance *= 10;
      S := FloatToStr(Resistance);
      if Length(S) = 1 then begin
        shRRing1.Brush.Color := clBlack;
        RingValues[1] := 0;
      end;
      J := 0;
      for I := 1 to Length(S) do begin
        if Copy(S, I, 1) <> ',' then begin
          Inc(J);
          Colour := StrToInt(Copy(S, I, 1));
          if J = 1 then begin
            if Length(S) = 1 then begin
              shRRing2.Brush.Color := ColourShapes[Colour].Brush.Color;
              RingValues[2] := Colour;
            end
            else begin
              shRRing1.Brush.Color := ColourShapes[Colour].Brush.Color;
              RingValues[1] := Colour;
            end;
          end
          else if J = 2 then begin
            shRRing2.Brush.Color := ColourShapes[Colour].Brush.Color;
            RingValues[2] := Colour;
          end;
        end;
      end;
      // Calculate resistance in Ω (transforming resistance value if unit is not Ω)
      if cbUResistance.Text = 'GΩ' then
        StdResistance *= 1e+9
      else if cbUResistance.Text = 'MΩ' then
        StdResistance *= 1e+6
      else if cbUResistance.Text = 'kΩ' then
        StdResistance *= 1e+3;
      // Resistance determined from the 1st and 2nd ring values
      Resistance := 10 * RingValues[1] + RingValues[2];
      // Compare this multiplied by 3rd ring value to determine with actual resistance value
      for I := 0 to 11 do begin
        case I of
          11: Mult := 0.01;
          10: Mult := 0.1;
           0: Mult := 1;
           else begin
             Mult := 1;
             for J := 1 to I do
               Mult *= 10;
           end;
         end;
        if Mult * Resistance = StdResistance then
          // Color of 3rd ring found
          Colour := I;
      end;
      shRRing3.Brush.Color := ColourShapes[Colour].Brush.Color;
      // Determine color of 4th ring
      if cbTolerance.Text = '-----' then
        shRRing4.Brush.Color := $003F85CD
      else begin
        S := LeftStr(cbTolerance.Text, Length(cbTolerance.Text) - 1);
        S := TrimLeft(S);
        Tolerance := StrToInt(S);
        case Tolerance of
          1..4: Colour := Tolerance;
            20: Colour := 0;
            10: Colour := 11;
             5: Colour := 10;
        end;
        shRRing4.Brush.Color := ColourShapes[Colour].Brush.Color;
      end;
    end;
    edResistance.SetFocus;
  end
  // Determine resistance value for given standard resistor (identified by color rings)
  else begin
    Shape4.Visible := False; Shape5.Visible := False; Shape6.Visible := False;
    shRRing1.Visible := False; shRRing2.Visible := False; shRRing3.Visible := False; shRRing4.Visible := False;
    for I := 1 to 4 do
      RRings[I].Brush.Color := $003F85CD;                                      // clear all rings
    // 3 ring colors at least must be entered
    if (Rings[1].Brush.Color = $003F85CD) or (Rings[2].Brush.Color = $003F85CD) or (Rings[3].Brush.Color = $003F85CD) then
      MessageDlg('Invalid ring colors', 'You must at least specify the color of the 3 first rings!', mtError, [mbOK], 0)
    // If 3 rings given, proceed...
    else begin
      // Determine numeric values corresponding to ring colors
      for I := 1 to 4 do begin
        for J := 0 to 11 do begin
          if Rings[I].Brush.Color = ColourShapes[J].Brush.Color then begin
            if I <= 3 then
              // Rings 1-3
              RingValues[I] := J
            else begin
              // Ring 4
              if Rings[4].Brush.Color = $003F85CD then
                RingValues[4] := 20;
              case J of
                 1..4: RingValues[4] := J;
                    0: RingValues[4] := 20;
                   10: RingValues[4] := 5;
                   11: RingValues[4] := 10;
              end;
            end;
          end;
        end;
      end;
      // Check if colors entered correspond to a valid resistor
      if (RingValues[1] = 0) and (RingValues[2] = 0) then
        MessageDlg('Invalid ring colors', '1st and 2nd ring can''t be both black!', mtError, [mbOK], 0)
      else if (RingValues[1] = 10) or (RingValues[2] = 10) then
        MessageDlg('Invalid ring colors', '1st and 2nd ring can''t be gold!', mtError, [mbOK], 0)
      else if (RingValues[1] = 11) or (RingValues[2] = 11) then
        MessageDlg('Invalid ring colors', '1st and 2nd ring can''t be silver!', mtError, [mbOK], 0)
      // Colors ok; display numerical values corresponding to ring colors
      else begin
        // 1st and 2nd ring values
        memoResults.Append(' 1st ring = ' + Colors[RingValues[1]] + '; 1st digit value = ' + FloatToStr(RingValues[1]));
        memoResults.Append(' 2nd ring = ' + Colors[RingValues[2]] + '; 2nd digit value = ' + FloatToStr(RingValues[2]));
        // 3rd ring value
        if RingValues[3] = 10 then
          Multiplier := '0,1'
        else if RingValues[3] = 11 then
          Multiplier := '0,01'
        else begin
          case RingValues[3] of
            0: Multiplier := '1';
            1: Multiplier := '10';
            2: Multiplier := '100';
            3: Multiplier := '1000';
            else Multiplier := '1e+' + IntToStr(RingValues[3]);
          end;
        end;
        memoResults.Append(' 3rd ring = ' + Colors[RingValues[3]] + '; multiplier = ' + Multiplier);
        // 4th ring value (20% if not specified)
        if Rings[4].Brush.Color = $003F85CD then
          memoResults.Append(' 4th ring not specified; tolerance supposed to be 20%')
        else begin
          case RingValues[4] of
            20: Colour := 0;
            10: Colour := 11;
             5: Colour := 10;
             else Colour := RingValues[4];
          end;
          memoResults.Append(' 4th ring = ' + Colors[Colour] + '; tolerance = ' + FloatToStr(RingValues[4]) + '%');
        end;
        // Calculate resistance
        Resistance := 10 * RingValues[1] + RingValues[2];
        // Determine multiplier value
        case RingValues[3] of
          10: Resistance *= 0.1;
          11: Resistance *= 0.01;
           0: Resistance *= 1;
           else begin
             for I := 1 to RingValues[3] do
               Resistance *= 10;
           end;
        end;
        UResistance := 'Ω';
        // Transform resistance (from Ω) if this appropriate
        if Resistance >= 1e+9 then begin
          Resistance /= 1e+9;
          UResistance := 'GΩ';
        end
        else if Resistance >= 1e+6 then begin
          Resistance /= 1e+6;
          UResistance := 'MΩ';
        end
        else if Resistance >= 1e+3 then begin
          Resistance /= 1000;
          UResistance := 'kΩ';
        end;
        // Display resistance
        memoResults.Append('');
        memoResults.Append(' Resistance = ' + FloatToStr(Resistance) + ' ' + UResistance);
      end;
    end;
    iRing := 0;
  end;
end;

{ Button "Reset": Clear form controls }

procedure TfResistors.btResetClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 4 do begin
    Rings[I].Brush.Color := $003F85CD;
    RRings[I].Brush.Color := $003F85CD;
  end;
  iRing := 0;
  memoResults.Clear;
  if rbResistor.Checked then
    edResistance.SetFocus;
end;

{ Clicking the color shapes: Display corresponding resistor rings }

procedure TfResistors.shBlackMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 0);
end;

procedure TfResistors.shBrownMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 1);
end;

procedure TfResistors.shRedMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 2);
end;

procedure TfResistors.shOrangeMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 3);
end;

procedure TfResistors.shYellowMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 4);
end;

procedure TfResistors.shGreenMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 5);
end;

procedure TfResistors.shBlueMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 6);
end;

procedure TfResistors.shVioletMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 7);
end;

procedure TfResistors.shGrayMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 8);
end;

procedure TfResistors.shWhiteMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 9);
end;

procedure TfResistors.shGoldMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 10);
end;

procedure TfResistors.shSilverMouseDown(Sender: TObject);

begin
  ResistorRings(iRing, 11);
end;

{ Button "Clear": undo last ring color selection }

procedure TfResistors.btClearClick(Sender: TObject);

begin
  ResistorRings(iRing, -1);
end;

end.

