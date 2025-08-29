{*********************************************************}
{* Class definition unit for PlanetaryOrbits application *}
{*********************************************************}

unit orbits_classes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math;

type
  TVector = record
    X, Y: Double;
  end;
  TOrbitInfo = record
    Body: string;
    Mass, Volume, OrbitPeriod: Double;
    Position, Velocity: TVector;
    DrawColor: TColor;
  end;
  TBody = class
    private
      BodyName: string;
      BodyMass, BodyVolume, BodyOrbitPeriod: Double;
      BodyPosition, BodyVelocity: TVector;
      BodyDrawColor: TColor;
    public
      constructor Create(Name: string; Mass, Volume, OPeriod, PX, VY: Double; Colour: TColor);
      destructor Destroy; override;
      function Attraction(Other: TBody): TVector;
      function TotalAttraction(Bodies: array of TBody): TVector;
      procedure NewVelocity(Force: TVector; TimeStep: Cardinal);
      procedure NewPosition(TimeStep: Cardinal);
      function GetOrbitData: TOrbitInfo;
  end;

implementation

{ TBody object creation }

constructor TBody.Create(Name: string; Mass, Volume, OPeriod, PX, VY: Double; Colour: TColor);

begin
  BodyName := Name; BodyMass := Mass; BodyVolume := Volume; BodyOrbitPeriod := OPeriod;
  BodyPosition.X := PX; BodyPosition.Y := 0;
  BodyVelocity.X := 0;  BodyVelocity.Y := VY;
  BodyDrawColor := Colour;
end;

{ TBody object destruction }

destructor TBody.Destroy;

begin
  inherited Destroy;
end;

{ Attraction force exerted by a given body }

function TBody.Attraction(Other: TBody): TVector;

const
  G  = 6.67428E-11;                                                            // gravitational constant

var
  Distance, Force, Theta: Double;
  BodyDistance, AttractionForce: TVector;

begin
  try
    if Self = Other then
      raise Exception.Create('Can''t calculate attraction of ' + Self.BodyName + ' to itself');
    //Compute the distance of the other body
    BodyDistance.X := Other.BodyPosition.X - Self.BodyPosition.X;
    BodyDistance.Y := Other.BodyPosition.Y - Self.BodyPosition.Y;
    Distance := Sqrt(Sqr(BodyDistance.X) + Sqr(BodyDistance.Y));
    // Report an error if the distance is zero; otherwise we'll
    // get a ZeroDivisionError exception further down
    if Distance = 0 then
      raise Exception.Create('Collision between ' + Self.BodyName + ' and ' + Other.BodyName);
    // Compute the force of attraction
    Force := G * Self.BodyMass * Other.BodyMass / Sqr(Distance);
    // Compute the direction of the force.
    Theta := Arctan2(BodyDistance.Y, BodyDistance.X);
    // Compute the x- and y-component of the force
    AttractionForce.X := Cos(Theta) * Force;
    AttractionForce.Y := Sin(Theta) * Force;
    // Force exerted on this body
    Result := AttractionForce;
  except
    on e: Exception do
      MessageDlg('Program error', e.Message + '!', mtError, [mbOK], 0);
  end;
end;

{ Total attraction force exerted by a given bodies }

function TBody.TotalAttraction(Bodies: array of TBody): TVector;

var
  I: Cardinal;
  Force, ForceTotal: TVector;

begin
  // Add up all of the forces exerted on actual body
  ForceTotal.X := 0; ForceTotal.Y := 0;
  for I := 0 to Length(Bodies) - 1 do begin
    if Bodies[I] <> Self then begin                                            // don't calculate the body's attraction to itself
      Force := Self.Attraction(Bodies[I]);
      // x- and y-component of force
      ForceTotal.X += Force.X;
      ForceTotal.Y += Force.Y;
    end;
  end;
  // Total force exerted on this body
  Result := ForceTotal;
end;

{ New velocity as a result of a given force exerted }

procedure TBody.NewVelocity(Force: TVector; TimeStep: Cardinal);

begin
  BodyVelocity.X += Force.X / BodyMass * TimeStep;
  BodyVelocity.Y += Force.Y / BodyMass * TimeStep;
end;

{ New position as a result of a displacement at given velocity during a given time }

procedure TBody.NewPosition(TimeStep: Cardinal);

begin
  BodyPosition.X += BodyVelocity.X * TimeStep;
  BodyPosition.Y += BodyVelocity.Y * TimeStep;
end;

// Get all (actual) information concerning this body (as a TOrbitInfo record)

function TBody.GetOrbitData: TOrbitInfo;

var
  Info: TOrbitInfo;

begin
  with Info do begin
    Body := BodyName; Mass := BodyMass; Volume := BodyVolume; OrbitPeriod := BodyOrbitPeriod;
    Position.X := BodyPosition.X; Position.Y := BodyPosition.Y;
    Velocity.X := BodyVelocity.X; Velocity.Y := BodyVelocity.Y;
    DrawColor := BodyDrawColor;
  end;
  Result := Info;
end;

end.

