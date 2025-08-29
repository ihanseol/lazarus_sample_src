{*********************************}
{* Maths trainer: Binary numbers *}
{*********************************}

program Binary;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, bin;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfBinary, fBinary);
  Application.Run;
end.

