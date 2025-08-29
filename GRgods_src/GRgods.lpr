{*****************************}
{* Greek and Roman gods quiz *}
{*****************************}

program GRgods;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  grgods_main, grgods_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfGRgods, fGRgods);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

