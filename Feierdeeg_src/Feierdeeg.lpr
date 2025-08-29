{**********************************}
{* Dates of the catholic holidays *}
{**********************************}

program Feierdeeg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  holidays;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfFeierdeeg, fFeierdeeg);
  Application.Run;
end.

