{******************************************************}
{* Financial mathematics: Interest and loan repayment *}
{******************************************************}

program Loans;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  loan, amortized, addon, compound, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfLoans, fLoans);
  Application.CreateForm(TfAmortized, fAmortized);
  Application.CreateForm(TfAddon, fAddon);
  Application.CreateForm(TfCompound, fCompound);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

