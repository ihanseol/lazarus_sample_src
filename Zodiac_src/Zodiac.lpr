{**********************************}
{* Astrology: Signs of the zodiac *}
{**********************************}

program Zodiac;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  zodiac_main, zodiac_signs, zodiac_data, zodiac_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfZodiac, fZodiac);
  Application.CreateForm(TfSigns, fSigns);
  Application.CreateForm(TfData, fData);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

