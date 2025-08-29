{***************************************************}
{* Electronics: Operational amplifier applications *}
{***************************************************}

program OpAmplifiers;

// Change log:
// Version 1.0 (September 2019): Original program
// Version 1.0.1 (September 2021):
//   - Pictures optimization (proper display)
//   - Helptext spelling mistakes corrections

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  opamp, opamp1, opamp2, opamp3, opamp4, opamp5, opamp6, ophelp;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfOpAmp, fOpAmp);
  Application.CreateForm(TfOpAmp1, fOpAmp1);
  Application.CreateForm(TfOpAmp2, fOpAmp2);
  Application.CreateForm(TfOpAmp3, fOpAmp3);
  Application.CreateForm(TfOpAmp4, fOpAmp4);
  Application.CreateForm(TfOpAmp5, fOpAmp5);
  Application.CreateForm(TfOpAmp6, fOpAmp6);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

