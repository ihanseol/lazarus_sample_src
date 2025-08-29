{*******************************}
{* Simple LAN messenger client *}
{*******************************}

program LANMsgClient;

// Version history
//   Version 1.0 (December 2023): Original program
//   Version 2.0 (April 2025):
//     - IP address and port validity check
//     - Client identification by user name
//     - Messages relayed to specific client or all clients

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms, lnetvisual,
  client, config;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfLANMsgClient, fLANMsgClient);
  Application.CreateForm(TfConfig, fConfig);
  Application.Run;
end.

