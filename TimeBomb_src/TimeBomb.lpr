{**********************************************}
{ Deactivate the time bomb before it explodes! }
{ The application is based on the Windows 95   }
{ game "Time Bomb" by Jeremy Dickson           }
{**********************************************}

program TimeBomb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms, tbomb;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfTimeBomb, fTimeBomb);
  Application.Run;
end.

