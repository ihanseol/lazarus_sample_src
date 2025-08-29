{*******************************************}
{* Reading UTF8 data from a MySQL database *}
{*******************************************}

program MySQL3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  mysql;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMySQL3, fMySQL3);
  Application.Run;
end.

