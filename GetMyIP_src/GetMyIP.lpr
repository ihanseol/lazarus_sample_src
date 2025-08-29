{***********************************************}
{* Get external IP address from free IP server *}
{* ------------------------------------------- *}
{* The application uses TFPHTTPClient.Get from *}
{* the LNET unit FpHttpClient to do the HTTP   *}
{***********************************************}

program GetMyIP;

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
  getip;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfGetIP, fGetIP);
  Application.Run;
end.

