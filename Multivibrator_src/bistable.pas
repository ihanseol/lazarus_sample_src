{****************************************************************}
{* "Bistable multivibrator" unit for Multivibrators application *}
{****************************************************************}

unit bistable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type
  { TfMVB }
  TfMVB = class(TForm)
    shSwitch0a: TShape;
    shSwitch0b: TShape;
    imS0: TImage;
    shSwitch1a: TShape;
    shSwitch2a: TShape;
    imTa1: TImage;
    shSwitch1b: TShape;
    shSwitch2b: TShape;
    imTa2: TImage;
    shLED1: TShape;
    shLED2: TShape;
    Image1, Image2, Image3, Image6, Image7, Image8: TImage;
    Shape1, Shape2, Shape3, Shape5, Shape6: TShape;
    Shape8, Shape9, Shape10, Shape11, Shape12: TShape;
    Shape13, Shape14, Shape15, Shape16, Shape17: TShape;
    Shape18, Shape19, Shape20, Shape21, hape22: TShape;
    Shape23, Shape24, Shape25, Shape26, Shape27: TShape;
    Shape29, Shape30, Shape32, Shape33, Shape34: TShape;
    Shape35, Shape36, Shape37, Shape38, Shape39: TShape;
    Shape40, Shape41, Shape42, Shape43, Shape44: TShape;
    Shape45, Shape46, Shape47, Shape48: TShape;
    Label1, Label2, Label3, Label4: TLabel;
    Label7, Label8, Label10, Label18, Label19: TLabel;
    btClose: TButton;
    tiMVB: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure imS0Click(Sender: TObject);
    procedure imTa1Click(Sender: TObject);
    procedure imTa2Click(Sender: TObject);
    procedure tiMVBTimer(Sender: TObject);
  private
    sPushbutton: string;
    bSwitch: Boolean;
  end;

var
  fMVB: TfMVB;

implementation

{$R *.lfm}

{*********}
{* TfMVB *}
{*********}

{ Application start: Initialisation }

procedure TfMVB.FormCreate(Sender: TObject);

begin
  bSwitch := False;
  tiMVB.Enabled := False;
end;

{ Clicking switch S: power on/off }

procedure TfMVB.imS0Click(Sender: TObject);

begin
  bSwitch := not bSwitch;
  if bSwitch then begin
    // Switch = on: Choose LED2 to be on by default
    shSwitch0a.Top := 33;
    shSwitch0b.Top := 45;
    shLED1.Brush.Style := bsDiagCross;
    shLED2.Brush.Style := bsSolid;
  end
  else begin
    // Switch = off: both LED off
    shSwitch0a.Top := 22;
    shSwitch0b.Top := 34;
    shLED1.Brush.Style := bsDiagCross;
    shLED2.Brush.Style := bsDiagCross;
    tiMVB.Enabled := False;                                                    // stop simulation
  end;
end;

{ Clicking pushbutton Ta1: connect T1 with mass simulation }

procedure TfMVB.imTa1Click(Sender: TObject);

begin
  // Move puhbutton down
  shSwitch1a.Top := 396;
  shSwitch1b.Top := 408;
  // If circuit is powered on, turn appropriate LED on
  if bSwitch then begin
    shLED1.Brush.Style := bsSolid;
    shLED2.Brush.Style := bsDiagCross;
  end;
  // Start the timer (just to move the pushbutton up again)
  sPushbutton := 'Ta1';
  tiMVB.Interval := 500;
  tiMVB.Enabled := True;
end;

{ Clicking pushbutton Ta2: connect T2 with mass simulation }

procedure TfMVB.imTa2Click(Sender: TObject);

begin
  // Move puhbutton down
  shSwitch2a.Top := 396;
  shSwitch2b.Top := 408;
  // If circuit is powered on, turn appropriate LED on
  if bSwitch then begin
    shLED1.Brush.Style := bsDiagCross;
    shLED2.Brush.Style := bsSolid;
  end;
  // Start the timer (just to move the pushbutton up again)
  sPushbutton := 'Ta2';
  tiMVB.Interval := 500;
  tiMVB.Enabled := True;
end;

{ Timer routine: move button pushed before up again (after .5 sec) }

procedure TfMVB.tiMVBTimer(Sender: TObject);

begin
  if sPushbutton = 'Ta1' then begin
    // Ta1 pushbutton
    shSwitch1a.Top := 382;
    shSwitch1b.Top := 394;
  end
  else begin
    // Ta1 pushbutton
    shSwitch2a.Top := 382;
    shSwitch2b.Top := 394;
  end;
  // Disable the timer
  tiMVB.Enabled := False;
end;

{ Button "Schliessen": Close the form }

procedure TfMVB.btCloseClick(Sender: TObject);

// Be sure that the switch is open, the LED off and the timer disabled

begin
  shSwitch0a.Top := 22;
  shSwitch0b.Top := 34;
  shLED1.Brush.Style := bsDiagCross;
  shLED2.Brush.Style := bsDiagCross;
  tiMVB.Enabled := False;
  Close;
end;

end.

