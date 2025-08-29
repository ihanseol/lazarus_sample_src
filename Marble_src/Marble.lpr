{****************}
{* Super Marble *}
{****************}

program Marble;

// The program is based on the Windows 95 application "Super Marble Solitaire",
// distributed as shareware by Wrenware (version 1.1 released in 1995)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms, sms, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMarble, fMarble);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

