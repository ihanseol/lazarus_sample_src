{*****************************}
{* Luxembourgish locals quiz *}
{*****************************}

program Luxembourg2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, luxbg2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLuxbg2, fLuxbg2);
  Application.Run;
end.

