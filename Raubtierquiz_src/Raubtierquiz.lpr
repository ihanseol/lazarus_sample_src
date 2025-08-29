{*************************************}
{* Carnivore quiz for 1 or 2 players *}
{*************************************}

program Raubtierquiz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  rquiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfRQ, fRQ);
  Application.Run;
end.

