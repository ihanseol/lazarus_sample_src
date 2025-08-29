{ *************************************************************************** }
{                            Simple Blackjack Game                            }
{ *************************************************************************** }

program BlackJack;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,                          // this includes the LCL widgetset
  Forms,
  blackjack_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

