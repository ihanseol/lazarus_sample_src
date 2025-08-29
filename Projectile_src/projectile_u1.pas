{***********************************************}
{* Main unit for Projectile motion application *}
{***********************************************}

unit projectile_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,  StdCtrls, Menus, PopupNotifier;

type
  { TfProjectile }
  TfProjectile = class(TForm)
    mMenu: TMainMenu;
    mFile: TMenuItem;
    mFileExit: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    Shape1: TShape;
    imLandscape: TImage;
    imCanon: TImage;
    shCanonball: TShape;
    Label1, Label2, Label3, Label4: TLabel;
    Label5, Label6, Label7, Label8: TLabel;
    edH0: TEdit;
    edV0: TEdit;
    edTheta0: TEdit;
    edX: TEdit;
    edT: TEdit;
    edV: TEdit;
    edTheta: TEdit;
    edHmax: TEdit;
    btCalc: TButton;
    tiShoot: TTimer;
    pnHelp: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure tiShootTimer(Sender: TObject);
  private
    iStep, iTrajectory, iXMax, iYMax: Integer;
    rH0, rXRange, rYMax, rParaboleA, rParaboleB: Real;
    shTrajectory: array[1..60] of TShape;
  end;

const
  CanonBallX = 216;    CanonBallY = 292;
  StartX = CanonBallX; EndX = 1150;
  StartY = CanonBallY; EndY = 620;

var
  fProjectile: TfProjectile;

implementation

{$R *.lfm}

{ Solve quadratic equation ax^2 + bx + c = 0 }

function EqQuad(A, B, C: Real): Real;

var
  R, R1, R2, Delta: Real;
  Mess: string;

begin
  R := -1;                                                                     // return -1 as error if sth went wrong
  Mess := '';
  Delta := Sqr(B) - 4 * A * C;                                                 // discriminent
  if Delta < 0 then                                                            // no real roots
    Mess := 'Parabole equation has no real roots!'
  else begin
    // Calculate the roots and keep the one >= 0
    R1 := (-B - Sqrt(Delta)) / (2 * A); R2 := (-B + Sqrt(Delta)) / (2 * A);
    if R1 >= 0 then
      R := R1
    else if R2 >= 0 then
      R := R2
    else
      Mess := 'Parabole equation has 2 negative roots!';
  end;
  if Mess <> '' then
    MessageDlg('Data error', Mess, mtError, [mbOK], 0);
  EqQuad := R;
end;

{ Transform angle in degrees to radians }

function Rad(ADeg: Real): Real;

begin
  Rad := Adeg * (2 * Pi / 360);
end;

{ Transform angle in radians to degrees }

function Deg(ARad: Real): Real;

begin
  Deg := ARad * (360 / (2 * Pi));
end;

{****************}
{* TfProjectile *}
{****************}

{ Application start: create array with shapes to draw the trajectory }

procedure TfProjectile.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // 60 small blue ellipses being placed and made visible as needed
  // to draw the trajectory taken by the canonball
  for I := 1 to 60 do begin
    shTrajectory[I] := TShape.Create(shTrajectory[I]);
    shTrajectory[I].Parent := Self;
    shTrajectory[I].Shape := stEllipse;
    shTrajectory[I].Width := 5;
    shTrajectory[I].Height := 5;
    shTrajectory[I].Brush.Color := clBlue;
    shTrajectory[I].Pen.Color := clBlue;
    shTrajectory[I].Visible := False;
  end;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfProjectile.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display help text (physics book suggestion) }

procedure TfProjectile.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Hide;
  S := 'For help with projectile motion and the appropriate formulas, please ';
  S += 'have rParaboleA look at rParaboleA Physics book. You can find all kind of free science ';
  S += 'books on the Internet. Suggestion: College Physics, by "OpenStax College".';
  pnHelp.Text := S;
  pnHelp.Visible := True;
end;

{ Menu item "Help > About": Display program about text }

procedure TfProjectile.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Hide;
  S := 'Two-dimensional kinematics (projectile motion):' + Chr(13);
  S += 'Firing rParaboleA canon, placed on rParaboleA hill, at rParaboleA given angle.' + Chr(13) + Chr(13);
  S += 'Version 1.0, © allu, June, 2018';
  pnHelp.Text := S;
  pnHelp.Visible := True;
end;

{ Button "Calculation": Calculate the trajectory values and start the timer for the animated graphics }

procedure TfProjectile.btCalcClick(Sender: TObject);

const
  G = 9.81;

var
  I: Integer;
  V0, Theta0, V0x, V0y, T, Vx, Vy, V, Theta: Real;
  Mess: string;

begin
  Mess := '';
  // Check user values
  if edH0.Text = '' then begin
    Mess := 'You must enter the canon''s position';
    edH0.SetFocus;
  end
  else begin
    rH0 := StrToFloat(edH0.Text);                                              // initial canon position
    if rH0 < 0 then begin
      Mess := 'Canon position must be greater than zero';
      edH0.SetFocus;
    end
    else begin
      if edV0.Text = '' then begin
        Mess := 'You must enter the projectile''s initial speed';
        edV0.SetFocus;
      end
      else begin
        V0 := StrToFloat(edV0.Text);                                           // initial projectile speed
        if V0 < 0 then begin
          Mess := 'Pojectile''s initial speed must be greater than zero';
          edV0.SetFocus;
        end
        else begin
          if (edTheta0.Text = '') then begin
            Mess := 'You must enter the firing angle';
            edTheta0.SetFocus;
          end
          else begin
            Theta0 := StrToFloat(edTheta0.Text);                               // firing angle
            if (Theta0 < 10) or (Theta0 > 90) then begin                       // greater than 90 is sensless; less than 10 is rather arbitrarily
              Mess := 'Firing angle must be between 10º and 90º';
              edTheta0.SetFocus;
            end;
          end;
        end;
      end;
    end;
  end;
  // If user data invalid, display error message
  if Mess <> '' then
    MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0)
  // If user data is ok, proceed with calculation
  else begin
    V0x := V0 * Cos(Rad(Theta0));                                              // horizontal and vertical component of initial speed
    V0y := V0 * Sin(Rad(Theta0));
    T  := EqQuad(g / 2, -V0y, -rH0);                                           // impact time (calculated by finding positive root of quadratic equation))
    // Proceed only if a positive time value has been found
    if T > 0 then begin
      Vx := V0x;                                                               // horizontal and vertical component of impact speed
      Vy := V0y - G * T;
      V  := Sqrt(Sqr(Vx) + Sqr(Vy));                                           // impact speed
      rXRange := Vx * T;                                                       // impact distance = canon range
      Theta := Deg(Arctan(Vy/Vx));                                             // impact angle
      edX.Text := FloatToStrF(rXRange, ffNumber, 0, 2);
      edT.Text := FloatToStrF(T, ffNumber, 0, 2);
      edTheta.Text := FloatToStrF(Theta, ffNumber, 0, 2);
      edV.Text := FloatToStrF(V, ffNumber, 0, 2);
      rYMax := Sqr(V0y) / (2 * G);                                             // maximum projectile height
      edHmax.Text := FloatToStrF(rYMax, ffNumber, 0, 2);
      // The canonball's trajectory is a parabole of the form y = ax^2 + bx
      rParaboleA := -G / (2 * Sqr(V0x));                                       // coefficient 'a' of the equation
      rParaboleB := v0y / V0x;                                                 // coefficient 'b' of the equation
      shCanonball.Left := CanonballX;                                          // show canonball at its intial position in the window
      shCanonball.Top := CanonballY;
      shCanonball.Visible := True;
      // The vertical scaling value is chosen depending on the drawing surface height
      // To get a correct trajectory form, horizontal scaling value must be the same
      // As the impact distance is largely dependent on the initial angle, different
      // scaling must be chosen for different (groups of) angles
      if Theta0 < 15 then
        iYMax := 10
      else if Theta0 < 25 then
        iYMax := 25
      else if Theta0 < 40 then
        iYMax := 60
      else if Theta0 < 50 then
        iYMax := 120
      else
        iYMax := 200;
      iXMax := Round(iYMax * (rXRange / rYMax));
      iStep := 0; iTrajectory := 0;
      for I := 1 to 60 do                                                      // clear curve from previous run
        shTrajectory[I].Visible := False;
      tiShoot.Enabled := True;                                                 // enable the timer (= start animation)
    end;
  end;
end;

{ Timer routine: animated projectile trajectory simulation }

procedure TfProjectile.tiShootTimer(Sender: TObject);

var
  X, Y: Real;
  SX, SY: Integer;

begin
  // Determine x,y values of trajectory parabola for 100 curve points
  X := iStep * (rXRange / 100);
  Y := rParaboleA * Sqr(X) + rParaboleB * X;
  // Determine corresponding coordinates in the drawing window
  SX := StartX + Round(iXMax * (X / rXRange));
  SY := CanonballY - (Round(iYMax * (Y / rYMax)));
  // Draw trajectory and move canonball until impact
  if (SX <= EndX) and (SY <= EndY) then begin
    // Draw the trajectory by placing a small ellipse shape at every 5 points of the curve
    if iStep mod 5 = 0 then begin
      Inc(iTrajectory);
      if iTrajectory <= 60 then begin                                          // maximum of ellipses that have been created
        fProjectile.shTrajectory[iTrajectory].Left := SX + 15;
        fProjectile.shTrajectory[iTrajectory].Top := SY + 10;
        fProjectile.shTrajectory[iTrajectory].Visible := True;
      end;
    end;
    // Move the canonball by changing its shape's 'left' and 'top' properties
    shCanonball.Left := SX;
    shCanonball.Top := SY;
    Inc(iStep);                                                                // next point on the trajectory curve
  end;
  // If impact has been reached, stop the animation (= disable timer)
  if (SX > EndX) or (SY > EndY) then
    tiShoot.Enabled := False;
end;

end.

