{***********************************}
{* Main unit for Plane application *}
{***********************************}

unit plane_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, PopupNotifier, math;

type
  { TfPlane }
  TfPlane = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1,  Label2,  Label3,  Label4,  Label5,  Label6,  Label7,  Label8, Label9: TLabel;
    Label10, Label11, Label12, Label13, Label14, Label15, Label16, Label17: TLabel;
    edPlane1D: TEdit;
    edPlane2D: TEdit;
    edPlane2DE: TEdit;
    edPlane2DN: TEdit;
    Shape1: TShape;
    Image1: TImage;
    imPlane1: TImage;
    imPlane2: TImage;
    edTime: TEdit;
    edPlaneDirection: TEdit;
    edPlaneVelocity: TEdit;
    cobWindDirection: TComboBox;
    edWindVelocity: TEdit;
    edWindAngle: TEdit;
    edPlane2Direction: TEdit;
    edPlane2Velocity: TEdit;
    edPlane2Angle: TEdit;
    btStart: TButton;
    btPause: TButton;
    tiPlane: TTimer;
    pnHelp: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btPauseClick(Sender: TObject);
    procedure tiPlaneTimer(Sender: TObject);
  private
    iT, iTrajectory1, iTrajectory2: Integer;
    rVp, rVw, rTheta, rVp2, rVp2X, rVp2Y, rAp2: Double;
    shTrajectory1, shTrajectory2: array[0..100] of TShape;
  end;

const
  Plane1X = 20;  Plane1Y = 625;
  Plane2X = 20;  Plane2Y = 550;
  StartX  = 20;  EndX = 1200;
  Start2Y = 550; EndY = 70;

var
  fPlane: TfPlane;

implementation

{$R *.lfm}

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

{*********}
{ TfPlane }
{*********}

{ Application start: Create shapes to use to "draw" plane trajectories}

procedure TfPlane.FormCreate(Sender: TObject);

var
  I: Integer;

begin
  // 2 x 101 small blue resp. red ellipses being placed and made
  // visible as needed to draw the trajectories of the 2 planes
  for I := 0 to 100 do begin
    shTrajectory1[I] := TShape.Create(shTrajectory1[I]);
    shTrajectory1[I].Parent := Self;
    shTrajectory1[I].Shape := stEllipse;
    shTrajectory1[I].Width := 5;
    shTrajectory1[I].Height := 5;
    shTrajectory1[I].Brush.Color := clBlue;
    shTrajectory1[I].Pen.Color := clBlue;
    shTrajectory1[I].Visible := False;
    shTrajectory2[I] := TShape.Create(shTrajectory1[I]);
    shTrajectory2[I].Parent := Self;
    shTrajectory2[I].Shape := stEllipse;
    shTrajectory2[I].Width := 5;
    shTrajectory2[I].Height := 5;
    shTrajectory2[I].Brush.Color := clRed;
    shTrajectory2[I].Pen.Color := clRed;
    shTrajectory2[I].Visible := False;
  end;
end;

{ Menu item "File > Exit": Exit application }

procedure TfPlane.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Display (very brief) help text }

procedure TfPlane.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Visible := False;
  S := 'Plane 1: Direction east (no wind).' + Chr(13);
  S += 'Plane 2: Deviated to the north by wind direction north-east (tail) resp. north-west (head).' + Chr(13);
  S += 'Wind angle = 0: full tail-wind; angle = 90º: full cross-wind; angle = 180º: full head-wind.';
  pnHelp.Text := S;
  pnHelp.Visible := True;
end;

{ Menu item "Help > About": Display about text }

procedure TfPlane.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Visible := False;
  S := 'Simple physics (kinematics) simulation:  Addition of two velocities.' + Chr(13) + Chr(13);
  S += 'Version 1.0, © allu, July-August, 2018.';
  pnHelp.Text := S;
  pnHelp.Visible := True;
end;

{ Button "Start/Stop": Start resp. stop the simulation }

procedure TfPlane.btStartClick(Sender: TObject);

var
  I: Integer;
  VwX, VwY: Double;
  Mess: string;

begin
  // Button "Start": Start the simulation
  if btStart.Caption = 'Start' then begin
    Mess := '';
    // Check user values
    if edPlaneVelocity.Text = '' then begin
      Mess := 'You must enter the velocity of the planes';
      edPlaneVelocity.SetFocus;
    end
    else begin
      rVp := StrToFloat(edPlaneVelocity.Text);                                 // plane velocity relative to the wind
      if rVp < 0 then begin
        Mess := 'The plane velocity must be greater than zero';
        edPlaneVelocity.SetFocus;
      end;
    end;
    if Mess = '' then begin
      if edWindVelocity.Text = '' then begin
        Mess := 'You must enter the velocity of the wind';
        edWindVelocity.SetFocus;
      end
      else begin
        rVw := StrToFloat(edWindVelocity.Text);                                // wind velocity
        if rVw < 0 then begin
          Mess := 'The wind velocity must be greater than zero';
          edWindVelocity.SetFocus;
        end
        else if rVw > 0.95 * rVp then begin                                    // to limit deviation to "from east to north", limit wind velocity to 95% of plane velocity
          Mess := 'The wind velocity should be less than ' + FloatToStr(Round(95 * rVp) / 100);
          edWindVelocity.SetFocus;
        end;
      end;
    end;
    if Mess = '' then begin
      if edWindAngle.Text = '' then begin
        Mess := 'You must enter the value of the wind angle';
        edWindAngle.SetFocus;
      end
      else begin
        rTheta := StrToFloat(edWindAngle.Text);                                // wind angle
        if cobWindDirection.Text = 'north-east' then begin
          if (rTheta < 0) or (rTheta > 90) then begin
            Mess := 'For north-east direction, the wind angle must be between 0º and 90º';
            edWindAngle.SetFocus;
          end;
        end
        else begin
          if (rTheta < 90) or (rTheta > 180) then begin
            Mess := 'For north-west direction, the wind angle must be between 90º and 180º';
            edWindAngle.SetFocus;
          end;
        end;
      end;
    end;
    // If user data invalid, display error message
    if Mess <> '' then
      MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0)
    // If user data is ok, proceed with calculation
    else begin
      // Hide the plane trajectories
      for I := 0 to 100 do begin
        shTrajectory1[I].Visible := False;
        shTrajectory2[I].Visible := False;
      end;
      // Reset planes at their intial position in the window
      imPlane1.Left := Plane1X; imPlane1.Top := Plane1Y;
      imPlane2.Left := Plane2X; imPlane2.Top := Plane2Y;
      // Reset time to zero
      edTime.Text := '0'; iT := 0;
      // Velocities calculation
      if cobWindDirection.Text = 'north-east' then begin
        // Cross- and head-wind
        VwX := rVw * Cos(Rad(rTheta));                                         // wind velocity X-component
        VwY := rVw * Sin(Rad(rTheta));                                         // wind velocity y-component
        rVp2 := Sqrt(Sqr(rVp + VwX) + Sqr(VwY));                               // plane 2 total velocity
      end
      else begin
        // Cross- and tail-wind
        VwX := rVw * Sin(Rad(rTheta - 90));                                    // wind velocity x-component
        VwY := rVw * Cos(Rad(rTheta - 90));                                    // wind velocity y-component
        rVp2 := Sqrt(Sqr(rVp - VwX) + Sqr(VwY));                               // plane 2 total velocity
      end;
      edPlane2Velocity.Text := FloatToStrF(rVp2, ffFixed, 0, 2);
      rAp2 := Arcsin(VwY / rVp2);                                              // plane 2 deviation angle
      edPlane2Angle.Text := FloatToStrF(Deg(rAp2), ffFixed, 0, 2);
      rVp2X := Cos(rAp2) * rVp2;                                               // plane 2 velocity x-component
      rVp2Y := Sin(rAp2) * rVp2;                                               // plane 2 velocity y-component
      // Set first point of plane trajectories
      iTrajectory1 := 1; iTrajectory2 := 1;
      shTrajectory1[iTrajectory1].Left := Plane1X - 5;
      shTrajectory1[iTrajectory1].Top := Plane1Y + 23;
      shTrajectory1[iTrajectory1].Visible := True;
      shTrajectory2[iTrajectory2].Left := Plane2X - 5;
      shTrajectory2[iTrajectory2].Top := Plane2Y + 23;
      shTrajectory2[iTrajectory2].Visible := True;
      // Review button settings
      btStart.Caption := 'Stop';
      btPause.Caption := 'Pause';
      btPause.Enabled := True;
      // Start the simulation (enable timer)
      tiPlane.Enabled := True;
    end;
  end
  // Button "Stop": Stop the simulation
  else begin
    tiPlane.Enabled := False;                                                  // stop the timer
    btStart.Caption := 'Start';
    btPause.Caption := 'Pause';
    btPause.Enabled := False;
  end;
end;

{ Button "Pause/Resume": Pause resp. resume the simulation after pause }

procedure TfPlane.btPauseClick(Sender: TObject);

begin
  // Button "Pause"
  if btPause.Caption = 'Pause' then begin
    tiPlane.Enabled := False;
    btPause.Caption := 'Resume';
  end
  // Button "Resume"
  else begin
      tiPlane.Enabled := True;
      btPause.Caption := 'Pause';
  end;
end;

{ Timer unit for plane in the wind displacement simultion }

procedure TfPlane.tiPlaneTimer(Sender: TObject);

var
  S1X, S2X, S1Y, S2Y: Integer;
  X1, X2, Y2, XMax, YMax: Double;
  Secs: string;

begin
  // Display actual time
  if iT < 60 then
    edTime.Text := IntToStr(iT) + ' sec'
  else begin
    Secs := IntToStr(iT mod 60);
    if Length(Secs) = 1 then
      Secs := '0' + Secs;
    edTime.Text := IntToStr(iT div 60) + ':' + Secs + ' min';
  end;
  // Calculate actual plane displacements and plane image positions in the window
  XMax := 60 * 60; YMax := 60 * 60;
  YMax *= (Start2Y - EndY) / (EndX - StartX);                                  // same measurment for both axis!
  // Plane 1
  X1 := iT * rVp;                                                              // displacement
  S1X := Plane1X + Round((X1 / XMax) * (EndX - StartX));                       // window position x-coordinate
  S1Y := Plane1Y;                                                              // window position y-coordinate (no deviation -> constant)
  // Plane 2
  X2 := iT * rVp2X; Y2 := iT * rVp2Y;                                          // displacement x- and y-component
  S2X := Plane2X + Round((X2 / XMax) * (EndX - StartX));                       // window position x-coordinate
  S2Y := Plane2Y + Round((Y2 / YMax) * (EndY - Start2Y));                      // window position y-coordinate
  // Draw next point of plane 1 trajectory and 'move' the plane to its new position
  if S1X <= EndX then begin                                                    // if plane still inside the window
    if iTrajectory1 < 100 then begin                                           // maximum of ellipses that have been created
      if (S1X - 5) - shTrajectory1[iTrajectory1].Left >= 10 then begin         // this creates a certain amount of space between 2 points
        Inc(iTrajectory1);                                                     // 'move' shape to correct position and make it visible
        shTrajectory1[iTrajectory1].Left := S1X - 5;
        shTrajectory1[iTrajectory1].Top := S1Y + 23;
        shTrajectory1[iTrajectory1].Visible := True;
      end;
    end;
    // 'Move' plane 1 to its new position
    edPlane1D.Text := FloatToStrF(X1 / 1000, ffFixed, 0, 3);                   // plane 1 displacement
    imPlane1.Left := S1X;
    imPlane1.Top  := S1Y;
  end;
  // Draw next point of plane 2 trajectory and 'move' the plane to its new position
  if (S2X <= EndX) and (S2Y >= EndY) then begin                                // if plane still inside the window
    if iTrajectory2 < 100 then begin                                           // maximum of ellipses that have been created
      if (S2X - 5) - shTrajectory2[iTrajectory2].Left >= 10 then begin         // this creates a certain amount of space between 2 points
        Inc(iTrajectory2);                                                     // 'move' shape to correct position and make it visible
        shTrajectory2[iTrajectory2].Left := S2X - 5;
        shTrajectory2[iTrajectory2].Top := S2Y + 23;
        shTrajectory2[iTrajectory2].Visible := True;
      end;
    end;
    // 'Move' plane 2 to its new position
    edPlane2DE.Text := FloatToStrF(X2 / 1000, ffFixed, 0, 3);                  // plane 2 displacement x (east)
    edPlane2DN.Text := FloatToStrF(Y2 / 1000, ffFixed, 0, 3);                  // plane 2 displacement y (north)
    edPlane2D.Text := FloatToStrF((iT * rVp2) / 1000, ffFixed, 0, 3);;         // plane 2 total displacement
    imPlane2.Left := S2X;
    imPlane2.Top  := S2Y;
  end;
  // Increment the time
  Inc(iT);
  // If plane 1 has reached right end of window and plane 2 has reached right/top end, stop the simulation
  if (S1X > EndX) and ((S2X > EndX) or (S2Y < EndY)) then begin
    tiPlane.Enabled := False;                                                  // stop timer
    btStart.Caption := 'Start';
    btPause.Caption := 'Pause';
    btPause.Enabled := False;
  end;
end;

end.

