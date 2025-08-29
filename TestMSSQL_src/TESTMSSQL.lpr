{*********************************************************}
{* Test Free Pascal connection with Microsoft SQL Server *}
{*********************************************************}

program TESTMSSQL;

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
  mssql;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMSSQL, fMSSQL);
  Application.Run;
end.

