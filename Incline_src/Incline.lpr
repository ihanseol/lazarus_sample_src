{*********************************}
{* Physics: Weight on an Incline *}
{*********************************}

program Incline;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  incline_u1, incline_u2;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfIncline, fIncline);
  Application.CreateForm(TfPSelect, fPSelect);
  Application.Run;
end.

