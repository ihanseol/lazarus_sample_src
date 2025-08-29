{******************************************}
{* Arithmetic baubles: Coins calculations *}
{******************************************}

program Coins;

// Change log:
//  Version 1.0 (April 2019): Original program (called Euro) with German GUI and EU coins only
//  Version 2.0 (February 2021):
//    - English GUI
//    - choice between EU and US coins
//    - code review and amelioration

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  coins_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfCoins, fCoins);
  Application.Run;
end.

