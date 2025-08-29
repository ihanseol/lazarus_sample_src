{*****************************************}
{* Language trainer: Chineses characters *}
{*****************************************}

program Chinese;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, chinese_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfChinese, fChinese);
  Application.Run;
end.

