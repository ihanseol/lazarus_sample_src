{***************}
{*  Flag Quiz  *}
{***************}

program FlagQuiz;

// Change log:
//  Version 1.0 (January 2018): Original program
//  Version 2.0 (March-September 2021):
//    - Options menu, replacing the Language menu: countries language selection
//    - New Tools menu: 1. custom quiz creation; 2. .fqz files registration
//    - Some general modifications (fontsize, dialogboxes instead of popups, usage of LazUTF8 methods...)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  fqz, fqznew;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfFQZ, fFQZ);
  Application.CreateForm(TfFQZNew, fFQZNew);
  Application.Run;
end.

