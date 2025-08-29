{*****************************************}
{* Creating MS Windows file associations *}
{*****************************************}

program WinFileAssociation;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  wfileassoc;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfWFileAssoc, fWFileAssoc);
  Application.Run;
end.

