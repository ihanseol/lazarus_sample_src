{*********************************************************************************}
{*                           Simple currency converter                           *}
{* ----------------------------------------------------------------------------- *}
{* Updates by currencies download from https://www.banque-france.fr, using       *}
{* wget.exe (Copyright (c) 1996−2011, 2015, 2018 Free Software Foundation, Inc.) *}
{*********************************************************************************}

program Devises;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  devises_u1,
  devises_u2,
  devises_u3;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDevises, fDevises);
  Application.CreateForm(TfDownload, fDownload);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

