{******************************}
{* Main unit for LANMsgClient *}
{******************************}

unit client;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, lNetComponents, lNet, config;

type
  {****************}
  { TfLANMsgClient }
  {****************}
  TArray = array of string;
  TfLANMsgClient = class(TForm)
    mMenu: TMainMenu;
    mClient, mClientExit: TMenuItem;
    mSettings, mSettingsConfig: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    ltcpClient: TLTCPComponent;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    edServerIP, edServerPort, edMessage, edUser: TEdit;
    edMessages: TMemo;
    lbClients: TListBox;
    shClientStatus: TShape;
    btConnect: TButton;
    btSend: TButton;
    btSendAll: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mClientExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mSettingsConfigClick(Sender: TObject);
    procedure btConnectClick(Sender: TObject);
    procedure btSendClick(Sender: TObject);
    procedure btSendAllClick(Sender: TObject);
    procedure ltcpClientConnect(aSocket: TLSocket);
    {procedure ltcpClientDisconnect(aSocket: TLSocket);}
    procedure ltcpClientError(const msg: string; aSocket: TLSocket);
    procedure ltcpClientReceive(aSocket: TLSocket);
    procedure lbClientsSelectionChange(Sender: TObject; User: boolean);
  private
    iServerPort: Word;
    sServerIP, sUserName, sDestination, sOldDestination, sSource, sOldSource, sMessage: string;
    bStart, bClientConnected: Boolean;
  end;

const
  ConfigFile  = 'lanmsg_client.txt';
  DefaultPort = 5600;

var
  fLANMsgClient: TfLANMsgClient;

implementation

{$R *.lfm}

{ Split a string containing a separator character into array elements }

procedure Split(Str: string; out Arr: TArray; Sep: Char);

var
  P: Integer;

begin
  SetLength(Arr, 0);
  while Str <> '' do begin
    SetLength(Arr, Length(Arr) + 1);
    P := Pos(Sep, Str);
    if P = 0 then begin
      Arr[Length(Arr) - 1] := Str;
      Str := '';
    end
    else begin
      Arr[Length(Arr) - 1] := LeftStr(Str, P - 1);
      Delete(Str, 1, P);
    end;
  end;
end;

{ Pause application for n seconds }

procedure Wait(Interval: Integer);

// I previewed this procedure for the case where the client would go offline before
// the server would be aware of it (send of the "bye" message not done in time)
// As it seems, there is no delay needed (at least not on my system)
// Should you experience any problems, add some delay code here...

begin

end;

{ Read server configuration from text file }

procedure ServerConfigRead(var ServerIP: string; var ServerPort: Word; var UserName: string);

var
  Line: string;
  InFile: Text;

begin
  ServerIP := ''; ServerPort := 0; UserName := '';
  Assign(InFile, ConfigFile); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      if (Length(Line) > 4) and (LeftStr(Line, 3) = 'ip=') then
        ServerIP := Trim(Copy(Line, 4, Length(Line)))
      else if (Length(Line) > 6) and (LeftStr(Line, 5) = 'port=') then
        ServerPort := StrToInt(Trim(Copy(Line, 6, Length(Line))))
      else if (Length(Line) > 6) and (LeftStr(Line, 5) = 'user=') then
        UserName := Trim(Copy(Line, 6, Length(Line)));
    end;
  end;
  Close(InFile);
end;

{ Write server configuration to text file }

procedure ServerConfigWrite(ServerIP: string; ServerPort: Word; UserName: string);

var
  Line: string;
  OutFile: Text;

begin
  Assign(OutFile, ConfigFile); Rewrite(OutFile);
  Line := 'ip=' + ServerIP; Writeln(OutFile, Line);
  Line := 'port=' + IntToStr(ServerPort); Writeln(OutFile, Line);
  Line := 'user=' + UserName; Writeln(OutFile, Line);
  Close(OutFile);
end;

{****************}
{ TfLANMsgClient }
{****************}

{ Application start: Initialization }

procedure TfLANMsgClient.FormCreate(Sender: TObject);

begin
  bStart := True; bClientConnected := False;
  btSend.Enabled := False; btSendAll.Enabled := False;
end;

{ Form becoming active: At application start, read server configuration file (if it exists) }

procedure TfLANMsgClient.FormActivate(Sender: TObject);

begin
  if bStart then begin
    // Application start: Read server configuration file (if it exists)
    if FileExists(ConfigFile) then begin
      ServerConfigRead(sServerIP, iServerPort, sUserName);
    end
    else begin
      sServerIP := ''; iServerPort := DefaultPort; sUserName := '';
    end;
    edServerIP.Text := sServerIP; edServerPort.Text := IntToStr(iServerPort); edUser.Text := sUserName;
    bStart := False;
  end;
  if sServerIP = '' then
    edServerIP.SetFocus
  else if iServerPort = 0 then
    edServerPort.SetFocus
  else if sUserName = '' then
    edUser.SetFocus
  else
    btConnect.SetFocus;
end;

{ Menu item "Client > Exit": Exit application }

procedure TfLANMsgClient.mClientExitClick(Sender: TObject);

begin
  if bClientConnected then begin
    ltcpClient.SendMessage('bye');
    Wait(5);
    ltcpClient.Disconnect(True);
  end;
  Close;
end;

{ Menu item "Settings > Server configuration": Set server IP, port, user name (and save to text file) }

procedure TfLANMsgClient.mSettingsConfigClick(Sender: TObject);

begin
  if not bClientConnected then begin
    if iServerPort = 0 then
      fConfig.edPort.Text := ''
    else
      fConfig.edPort.Text := IntToStr(iServerPort);
    fConfig.edIP.Text := sServerIP;
    fConfig.edUser.Text := sUserName;
    fConfig.sConfig := 'client';
    fConfig.ShowModal;
    if fConfig.sButton = 'save' then begin
      // Settings were changed: Apply and save to file
      iServerPort := fConfig.iServerPort;
      sServerIP   := fConfig.sServerIP;
      sUserName   := fConfig.sUserName;
      edServerPort.Text := IntToStr(iServerPort);
      edServerIP.Text := sServerIP;
      edUser.Text := sUserName;
      ServerConfigWrite(sServerIP, iServerPort, sUserName);
    end;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfLANMsgClient.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Simple LAN messenger client-server application.' + LineEnding;
  S += 'LAN messenger client.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, December 2023 - April 2025.';
  MessageDlg('About "LANMsgClient"', S, mtInformation, [mbOK], 0);
end;

{ Button "Connect/Disconnect": Connect to resp. disconnect from server }

procedure TfLANMsgClient.btConnectClick(Sender: TObject);

var
  ClientConnect: Boolean;

begin
  if btConnect.Caption = 'Connect' then begin
    // Button "Connect": Connect to the server
    sServerIP := edServerIP.Text; iServerPort := StrToInt(edServerPort.Text); sUserName := edUser.Text;
    if (sServerIP <> '') and (iServerPort <> 0) and (sUserName <> '') then begin
      ClientConnect := ltcpClient.Connect(sServerIP, iServerPort);
    end;
  end
  else begin
    // Button "Disconnect": Disconnect from the server
    if bClientConnected then begin
      ltcpClient.SendMessage('bye');
      Wait(15);
      ltcpClient.Disconnect(True);
      bClientConnected := False;
      shClientStatus.Brush.Color := clRed;
      lbClients.Items.Clear;
      edMessages.Lines.Clear; edMessage.Text := '';
      btConnect.Caption := 'Connect';
      btSend.Enabled := False;
      btSendAll.Enabled := False;
    end;
  end;
end;

{ Button "Send": Send message to other client }

procedure TfLANMsgClient.btSendClick(Sender: TObject);

var
  S: string;

begin
  sMessage := edMessage.Text;
  if (sDestination <> '') and (sMessage <> '') then begin
    if sDestination <> sOldDestination then begin
      // Display destination = selected user
      S := '  To ' + sDestination + ':';
      edMessages.Append(S);
      sOldDestination := sDestination;
      sOldSource := '';
    end;
    // Display message
    edMessages.Append(sMessage);
    // Send message to the server ("msg" command)
    S := 'msg ' + sDestination + ' ' + sMessage;
    ltcpClient.SendMessage(S);
  end;
end;

{ Button "Send All": Send message to all clients }

procedure TfLANMsgClient.btSendAllClick(Sender: TObject);

var
  S: string;

begin
  sMessage := edMessage.Text;
  if sMessage <> '' then begin
    if sOldDestination <> 'old' then begin
      // Display destination = all users
      S := '  To all users:';
      edMessages.Append(S);
      sOldDestination := 'all';
      sOldSource := '';
    end;
    // Display message
    edMessages.Append(sMessage);
    // Send message to the server ("msg" command)
    S := 'msg ' + 'all' + ' ' + sMessage;
    ltcpClient.SendMessage(S);
  end;
end;

{ Event triggered when the connection to the server was successful }

procedure TfLANMsgClient.ltcpClientConnect(aSocket: TLSocket);

var
  S: string;

begin
  bClientConnected := True;
  shClientStatus.Brush.Color := clLime;
  // Send username to server
  S := 'user ' + sUserName;
  ltcpClient.SendMessage(S);
  btConnect.Caption := 'Disconnect';
  btSend.Enabled := True;
  btSendAll.Enabled := True;
  sDestination := ''; sOldDestination := ''; sOldSource := '';
end;

{ Event triggered when server disconnects }

{procedure TfLANMsgClient.ltcpClientDisconnect(aSocket: TLSocket);

// This feature seems not to work correctly, the event never being triggered!?

begin
  MessageDlg('Network problem', 'LANMsgServer closed the connection', mtWarning, [mbOK], 0);
  bClientConnected := False;
  shClientStatus.Brush.Color := clRed;
  lbClients.Items.Clear;
  btConnect.Caption := 'Connect';
  btSend.Enabled := False;
  btSendAll.Enabled := False;
end;}

{ Event triggered when there was a communication error (ex: when the server is offline) }

procedure TfLANMsgClient.ltcpClientError(const msg: string; aSocket: TLSocket);

begin
  MessageDlg('Network error', 'LANMsgClient returned the error message: ' + msg, mtError, [mbOK], 0);
end;

{ Event triggered when the client receives a message }

procedure TfLANMsgClient.ltcpClientReceive(aSocket: TLSocket);

var
  P, I: Integer;
  S: string;
  Clients: TArray;

begin
  // Retrieve the message (= command + data)
  aSocket.GetMessage(sMessage);
  if sMessage = 'bye' then begin
    // Command "bye" = Server will go offline: Adapt client status (disconnected)
    MessageDlg('Network problem', 'LANMsgServer closed the connection', mtWarning, [mbOK], 0);
    bClientConnected := False;
    shClientStatus.Brush.Color := clRed;
    lbClients.Items.Clear;
    edMessages.Lines.Clear; edMessage.Text := '';
    btConnect.Caption := 'Connect';
    btSend.Enabled := False;
    btSendAll.Enabled := False;
  end
  else if LeftStr(sMessage, 4) = 'list' then begin
    // Command "list" = Server sent user list: Update listbox content
    Delete(sMessage, 1, 4);
    Split(sMessage, Clients, ' ');
    lbClients.Clear;
    for I := 0 to Length(Clients) - 1 do
      lbClients.Items.AddText(Clients[I]);
  end
  else if LeftStr(sMessage, 3) = 'msg' then begin
    // Command "msg" = Server sent a message from another client: Display sender and message text
    // Server message has format: msg <sender-user> <message-text>
    Delete(sMessage, 1, 4);
    P := Pos(' ', sMessage);
    sSource := Copy(sMessage, 1, P - 1);
    if sSource <> sOldSource then begin
      // Display sender user
      S := '  From ' + sSource + ':';
      edMessages.Append(S);
      sOldSource := sSource;
      sOldDestination := '';
    end;
    // Display message
    Delete(sMessage, 1, P);
    edMessages.Append(sMessage);
  end;
end;

{ Listbox selection change (user selected a client to send a message to) }

procedure TfLANMsgClient.lbClientsSelectionChange(Sender: TObject; User: boolean);

begin
  sDestination := lbClients.GetSelectedText ;
end;

end.

