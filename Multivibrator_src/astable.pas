{***************************************************************}
{* "Astable multivibrator" unit for Multivibrators application *}
{***************************************************************}

unit astable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TfMVA }
  TfMVA = class(TForm)
    imS0: TImage;
    shSwitch0a, shSwitch0b: TShape;
    shLED1: TShape;
    shLED2: TShape;
    Image1, Image2, Image3, Image4: TImage;
    Image5, Image6, Image7, Image8: TImage;
    Shape1, Shape2, Shape3, Shape4, Shape5: TShape;
    Shape6, Shape7, Shape8, Shape9, Shape10: TShape;
    Shape11, Shape12, Shape13, Shape14, Shape15: TShape;
    Shape16, Shape17, Shape18, Shape19, Shape20: TShape;
    Shape21, Shape22, Shape23, Shape24, Shape25: TShape;
    Shape26, Shape27, Shape28, Shape29, Shape30: TShape;
    Shape31, Shape32, Shape33, Shape34, Shape35, Shape36: TShape;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15: TLabel;
    Label16, Label17, Label18: TLabel;
    edR1, edC1, edR2, edC2: TEdit;
    cobR1, cobC1, cobR2, cobC2: TComboBox;
    laT1, laT2, laFreq: TLabel;
    edT1, edT2, edFreq: TEdit;
    btCalc: TButton;
    btClose: TButton;
    tiMVA: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure imS0Click(Sender: TObject);
    procedure tiMVATimer(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    rT1, rT2: Double;
    sTransistor: string;
    bCalcDone, bSwitch: Boolean;
  public
    rTimeMult: Double;
  end;

var
  fMVA: TfMVA;

implementation

{$R *.lfm}

{*********}
{* TfMVA *}
{*********}

{ Application start: Initialisation }

procedure TfMVA.FormCreate(Sender: TObject);

begin
  bSwitch := False;
  bCalcDone := False;
end;

{ Button "Berechnen": Calculate circuit time values }

procedure TfMVA.btCalcClick(Sender: TObject);

var
  R1, C1, R2, C2, T1, T2, F: Double;
  Mess: string;

begin
  // Power off the circuit
  shSwitch0a.Top := 22;
  shSwitch0b.Top := 34;
  bSwitch := False;
  shLED1.Brush.Style := bsDiagCross;
  shLED2.Brush.Style := bsDiagCross;
  tiMVA.Enabled := False;
  // Get R and C values as user input
  laT1.Caption := 'sec'; laT2.Caption := 'sec'; laFreq.Caption := 'Hz';
  Mess := ''; bCalcDone := False;
  if edR1.Text = '' then
    R1 := 0
  else
    R1 := StrToFloat(edR1.Text);
  if edC1.Text = '' then
    C1 := 0
  else
    C1 := StrToFloat(edC1.Text);
  if R1 <= 0 then begin
    Mess := 'Widerstand R1 muss größer als null sein';
    edR1.SetFocus;
  end
  else if C1 <= 0 then begin
    Mess := 'Kondensator C1 muss größer als null sein';
    edC1.SetFocus;
  end
  else begin
    // If R2 / C2 fields blank, set R2 = R1 / C2 = C1
    if edR2.Text = '' then begin
      R2 := R1;
      edR2.Text  := FloatToStrF(R2, ffFixed, 0, 3);
      cobR2.Text := cobR1.Text;
    end
    else
      R2 := StrToFloat(edR2.Text);
    if edC2.Text = '' then begin
      C2 := C1;
      edC2.Text := FloatToStrF(C2, ffFixed, 0, 3);
      cobC2.Text := cobC1.Text;
    end
    else
      C2 := StrToFloat(edC2.Text);
    // Transform R to Ω and C to F
    case cobR1.ItemIndex of
      1: R1 *= 1E+3;
      2: R1 *= 1E+6;
    end;
    case cobC1.ItemIndex of
      0: C1 *= 1E-6;
      1: C1 *= 1E-9;
      2: C1 *= 1E-12;
    end;
    case cobR2.ItemIndex of
      1: R2 *= 1E+3;
      2: R2 *= 1E+6;
    end;
    case cobC2.ItemIndex of
      0: C2 *= 1E-6;
      1: C2 *= 1E-9;
      2: C2 *= 1E-12;
    end;
    // Calculate duration for the 2 (unstable) states and circuit frequency
    T1 := Ln(2) * R1 * C1; rT1 := T1;
    T2 := Ln(2) * R2 * C2; rT2 := T2;
    F  := 1 / (T1 + T2);
    // bCalcDone = True: With T1 and T2 known, the simulation may now be done
    bCalcDone := True;
    // Adapt time and frequency unit to actual value (for display)
    if T1 < 1E-6 then begin
      T1 *= 1E+6;
      laT1.Caption := 'µsec';
    end
    else if T1 < 1E-3 then begin
      T1 *= 1E+3;
      laT1.Caption := 'msec';
    end;
    if T2 < 1E-6 then begin
      T2 *= 1E+6;
      laT2.Caption := 'µsec';
    end
    else if T2 < 1E-3 then begin
      T2 *= 1E+3;
      laT2.Caption := 'msec';
    end;
    if F < 1E-6 then begin
      F *= 1E+6;
      laFreq.Caption := 'µHz';
    end
    else if F < 1E-3 then begin
      F *= 1E+3;
      laFreq.Caption := 'msec';
    end;
    edT1.Text   := FloatToStrF(T1, ffFixed, 0, 3);
    edT2.Text   := FloatToStrF(T2, ffFixed, 0, 3);
    edFreq.Text := FloatToStrF(F, ffFixed, 0, 3);
  end;
  if Mess <> '' then
    MessageDlg('Eingabefehler', Mess + '!', mtError, [mbOK], 0);
end;

{ Clicking switch S: power on/off }

procedure TfMVA.imS0Click(Sender: TObject);

var
  Mess: string;

begin
  Mess := '';
  bSwitch := not bSwitch;
  // Switch = on: Turn one of the LED on
  if bSwitch then begin
    // Do only if time calculation has been done
    if bCalcDone then begin
      // Check the actual timer interval value and abort if it is to small
      // I found timer interval = 20 msec to be the minimum value to get a proper functionning of the simulation
      if (Round(rTimeMult * rT1 * 1000) < 20) or (Round(rTimeMult * rT2 * 1000) < 20) then
        Mess := 'Zeitintervall < 20 msec. Bitte langsamere Simulationszeit wählen'
      // If all ok, do switch and LED on
      else begin
        shSwitch0a.Top := 33;
        shSwitch0b.Top := 45;
        shLED1.Brush.Style := bsDiagCross;
        shLED2.Brush.Style := bsSolid;
        // Set timer interval to actual state time value
        tiMVA.Interval := Round(rTimeMult * rT2 * 1000);
        // The LED  corresponding to the actual state has been turned on here
        // Thus, the timer routine has to start with switching to the "non-actual" state
        sTransistor := 'T1';
        tiMVA.Enabled := True;
      end;
    end
    else
      Mess := 'Bitte zuerst RC-Werte eingeben und Zeiten berechnen!';
  end
  // Switch = off: turn both LED off
  else begin
    shSwitch0a.Top := 22;
    shSwitch0b.Top := 34;
    shLED1.Brush.Style := bsDiagCross;
    shLED2.Brush.Style := bsDiagCross;
    tiMVA.Enabled := False;
  end;
  if Mess <> '' then begin
    bSwitch := not bSwitch;                                                    // reset the switch
    MessageDlg('Eingabefehler', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Timer routine: Alternatively switch to states 1 and 2 }

procedure TfMVA.tiMVATimer(Sender: TObject);

begin
  if bSwitch and bCalcDone then begin
    // Switch to state with transistor T1 open
    if sTransistor = 'T1' then begin
      shLED1.Brush.Style := bsSolid;
      shLED2.Brush.Style := bsDiagCross;
      tiMVA.Interval := Round(rTimeMult * rT1 * 1000);                         // wait time = time during which transistor T1 is open
      sTransistor := 'T2';                                                     // next state = transistor T2 state
    end
    // Switch to state with transistor T2 open
    else begin
      shLED2.Brush.Style := bsSolid;
      shLED1.Brush.Style := bsDiagCross;
      tiMVA.Interval := Round(rTimeMult * rT2 * 1000);                         // wait time = time during which transistor T2 is open
      sTransistor := 'T1';                                                     // next state = transistor T1 state
    end;
  end;
end;

{ Button "Schliessen": Close the form }

procedure TfMVA.btCloseClick(Sender: TObject);

begin
  // Switch off, LED off and timer stopped
  shSwitch0a.Top := 22;
  shSwitch0b.Top := 34;
  bSwitch := False;
  shLED1.Brush.Style := bsDiagCross;
  shLED2.Brush.Style := bsDiagCross;
  tiMVA.Enabled := False;
  Close;
end;

end.

