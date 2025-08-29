{*************************}
{* Multiple locals clock *}
{*************************}

program Clock;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, clock_u1, clock_u2, clock_u3;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfClock, fClock);
  Application.CreateForm(TfConfig, fConfig);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

