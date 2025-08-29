{******************************************************************}
{* "Monostable multivibrator" unit for Multivibrators application *}
{******************************************************************}

unit monostable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TfMVM }
  TfMVM = class(TForm)
    imS0: TImage;
    shSwitch0a,  shSwitch0b: TShape;
    shLED1: TShape;
    shLED2: TShape;
    imTa1: TImage;
    shSwitch1a, shSwitch1b: TShape;
    Image1, Image2, Image3, Image4: TImage;
    Image6, Image7, Image8: TImage;
    Shape1, Shape2, Shape3, Shape5, Shape6: TShape;
    Shape7, Shape8, Shape9, Shape10, Shape11: TShape;
    Shape12, Shape13, Shape14, Shape15, Shape16: TShape;
    Shape17, Shape18, Shape19, Shape20, Shape21: TShape;
    Shape22, Shape23, Shape24, Shape25, Shape26: TShape;
    Shape27, Shape28, Shape29, Shape30, Shape31: TShape;
    Shape33, Shape34, Shape35, Shape36, Shape37: TShape;
    Shape38, Shape39, Shape40, Shape41, Shape42: TShape;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label7, Label8, Label9, Label10, Label11: TLabel;
    Label12, Label15, Label18: TLabel;
    edR1: TEdit;
    cobR1: TComboBox;
    edC1: TEdit;
    cobC1: TComboBox;
    laTime: TLabel;
    edTime: TEdit;
    btCalc: TButton;
    btClose: TButton;
    tiMVM: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure imS0Click(Sender: TObject);
    procedure imTa1Click(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure tiMVMTimer(Sender: TObject);
  private
    rT: Double;
    bCalcDone, bSwitch, bButtonUp: Boolean;
  public
    rTimeMult: Double;                                                         // simulation time multiplicator passed from main form
  end;

var
  fMVM: TfMVM;

implementation

{$R *.lfm}

{*********}
{* TfMVM *}
{*********}

{ Application start: Initialisation }

procedure TfMVM.FormCreate(Sender: TObject);

begin
  bSwitch := False;
  bCalcDone := False;
end;

{ Button "Berechnen": Calculate unstable state duration }

procedure TfMVM.btCalcClick(Sender: TObject);

var
  R1, C1, T: Double;
  Mess: string;

begin
  // Power off the circuit
  shSwitch0a.Top := 22;
  shSwitch0b.Top := 34;
  bSwitch := False;
  shLED1.Brush.Style := bsDiagCross;
  shLED2.Brush.Style := bsDiagCross;
  tiMVM.Enabled := False;
  laTime.Caption := 'sec';
  Mess := ''; bCalcDone := False;
  // Read R and C as user input
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
    // Calculate astable state duration
    T := Ln(2) * R1 * C1; rT := T;
    // bCalcDone = True: With T known, the simulation may now be done
    bCalcDone := True;
    // Adapt time unit to actual value (for display)
    if T < 1E-6 then begin
      T *= 1E+6;
      laTime.Caption := 'µsec';
    end
    else if T < 1E-3 then begin
      T *= 1E+3;
      laTime.Caption := 'msec';
    end;
    edTime.Text := FloatToStrF(T, ffGeneral, 4, 3);;
  end;
  if Mess <> '' then
    MessageDlg('Eingabefehler', Mess + '!', mtError, [mbOK], 0);
end;

{ Clicking switch S: power on/off }

procedure TfMVM.imS0Click(Sender: TObject);

var
  Mess: string;

begin
  Mess := '';
  bSwitch := not bSwitch;
  // Switch = on: Turn the stable state LED on
  if bSwitch then begin
    // Do only if time calculation has been done
    if bCalcDone then begin
      // Check the actual timer interval value and abort if it is to small
      // I found timer interval = 20 msec to be the minimum value to get a proper functionning of the simulation
      if Round(rTimeMult * rT * 1000) < 20 then
        Mess := 'Zeitintervall < 20 msec. Bitte langsamere Simulationszeit wählen'
      // If all ok, do switch and LED on
      else begin
        shSwitch0a.Top := 33;
        shSwitch0b.Top := 45;
        shLED1.Brush.Style := bsDiagCross;
        shLED2.Brush.Style := bsSolid;
      end;
    end
    else
      Mess := 'Bitte zuerst RC-Werte eingeben und Zeit berechnen!';
  end
  // Switch = off: turn both LED off
  else begin
    shSwitch0a.Top := 22;
    shSwitch0b.Top := 34;
    shLED1.Brush.Style := bsDiagCross;
    shLED2.Brush.Style := bsDiagCross;
    tiMVM.Enabled := False;
  end;
  if Mess <> '' then begin
    bSwitch := not bSwitch;                                                    // reset the switch
    MessageDlg('Eingabefehler', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Clicking pushbutton: start the unstable state }

procedure TfMVM.imTa1Click(Sender: TObject);

begin
  // Move the pushbutton down
  shSwitch1a.Top := 396;
  shSwitch1b.Top := 408;
  if bSwitch then begin
    // Turn the unstable state LED on
    shLED1.Brush.Style := bsSolid;
    shLED2.Brush.Style := bsDiagCross;
  end;
  // Start the timer (to first remove the pushbutton up, then switch the simulation back to the stable state)
  bButtonUp := False;
  tiMVM.Interval := 500;
  tiMVM.Enabled := True;
end;

{ Timer routine (Pushbutton move-up and switching simulation back to stable state) }

procedure TfMVM.tiMVMTimer(Sender: TObject);

begin
  // If the pushbutton is down, move it up
  if not bButtonUp then begin
    shSwitch1a.Top := 382;
    shSwitch1b.Top := 394;
    bButtonUp := True;
    // Set time to wait until to switch back to stable state
    if bSwitch and bCalcDone then begin
      tiMVM.Interval := Round(rTimeMult * rT * 1000);
    end;
  end
  // If the pushbutton has already removed up, switch back to stable state
  else begin
    if bSwitch and bCalcDone then begin
      // Turn stable state LED on
      shLED1.Brush.Style := bsDiagCross;
      shLED2.Brush.Style := bsSolid;
      // Simulation done: Stop the timer
      tiMVM.Enabled := False;
    end;
  end;
end;

{ Button "Schliessen": Close the form }

procedure TfMVM.btCloseClick(Sender: TObject);

begin
  // Switch off, LED off and timer stopped
  shSwitch0a.Top := 22;
  shSwitch0b.Top := 34;
  bSwitch := False;
  shLED1.Brush.Style := bsDiagCross;
  shLED2.Brush.Style := bsDiagCross;
  tiMVM.Enabled := False;
  Close;
end;

end.

