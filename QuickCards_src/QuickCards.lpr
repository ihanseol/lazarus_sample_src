{*****************************}
{* Simple card reaction game *}
{*****************************}

program QuickCards;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  quickcards_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfQuickCards, fQuickCards);
  Application.Run;
end.

