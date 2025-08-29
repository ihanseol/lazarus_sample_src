{******************************************}
{* Chemistry: Mineral acid-base reactions *}
{******************************************}

program AcidBase;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, acidbase_main, acidbase_list, acidbase_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfAcidBase, fAcidBase);
  Application.CreateForm(TfList, fList);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

