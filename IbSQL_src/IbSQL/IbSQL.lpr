{*****************************************}
{* Test connection to InterBase database *}
{*****************************************}

program IbSQL;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  ib;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfIbSQL, fIbSQL);
  Application.Run;
end.

