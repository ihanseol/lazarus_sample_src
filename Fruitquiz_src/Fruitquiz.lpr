{***************************}
{* Fruit quiz for children *}
{***************************}

program Fruitquiz;

// Change log:
// Version 1.0 (October 2021): Original program "Obstquiz" (German)
// Version 1.0.1 (December 2021): "Obstquiz" modifications
//  - 4 fruits added, some others exchanged
//  - Some pics changed, several not used pics deleted
// Version 2.0 (December 2021):
//  - English version of the application
//  - English and German fruit names

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  fquiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfFruitquiz, fFruitquiz);
  Application.Run;
end.

