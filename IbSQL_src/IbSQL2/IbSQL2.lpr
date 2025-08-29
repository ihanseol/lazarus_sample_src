{****************************************************************}
{* Test connection to InterBase, using a TODBCConnection object *}
{****************************************************************}

program IbSQL2;

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
  Application.CreateForm(TfIbSQL2, fIbSQL2);
  Application.Run;
end.

