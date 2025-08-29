{**********************************}
{* Biochemistry: Amino acids quiz *}
{**********************************}

program AAQuiz;

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
  quiz;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfAAQuiz, fAAQuiz);
  Application.Run;
end.

