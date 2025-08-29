{******************************************}
{*      Chemistry exercise generator      *}
{* Atoms and ions electron configurations *}
{******************************************}

program EConf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  econf_u1, econf_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfEConf, fEConf);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

