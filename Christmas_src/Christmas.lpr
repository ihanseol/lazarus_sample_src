{*******************************}
{* Customisable Christmas tree *}
{*******************************}

program Christmas;

// Change log:
//  - Version 1.0 (February 2020): Original program
//  - Version 1.1 (October 2020):
//     - multi-shape balls
//     - personal message

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, xmas;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfChristmas, fChristmas);
  Application.Run;
end.

