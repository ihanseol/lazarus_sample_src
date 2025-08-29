{*************************************************}
{* DNA properties, transcription and translation *}
{*************************************************}

program DNABasics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  dna, gcode, bases, rseq;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDNA, fDNA);
  Application.CreateForm(TfGenCode, fGenCode);
  Application.CreateForm(TfBases, fBases);
  Application.CreateForm(TfRSeq, fRSeq);
  Application.Run;
end.

