{*****************}
{*  MEMORY TEST  *}
{*****************}

program MemoryTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  memtest, newtest, displayone, displayall, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfMemTest, fMemTest);
  Application.CreateForm(TfNew, fNew);
  Application.CreateForm(TfOne, fOne);
  Application.CreateForm(TfAll, fAll);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

