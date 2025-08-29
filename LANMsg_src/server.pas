{******************************}
{* Main unit for LANMsgServer *}
{******************************}

unit server;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, Grids, lNetComponents, lNet, config;

type
  TArray  = array of string;
  TIPList = array of record
    IP, User: string;
  end;
  {****************}
  { TfLANMsgServer }
  {****************}
  TfLANMsgServer = class(TForm)
    mMenu: TMainMenu;
    mServer, mServerExit, mSettings, mSettingsConfig, mHelp, mHelpAbout: TMenuItem;
    ltcpServer: TLTCPComponent;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    edServerIP, edServerPort: TEdit;
    edServerLog: TMemo;
    sgClients: TStringGrid;
    shServerStatus: TShape;
    btStart: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mServerExitClick(Sender: TObject);
    procedure mSettingsConfigClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure ltcpServerAccept(aSocket: TLSocket);
    procedure ltcpServerReceive(aSocket: TLSocket);
    procedure ltcpServerError(const msg: string; aSocket: TLSocket);
    {procedure ltcpServerDisconnect(aSocket: TLSocket);}
  private
    iServerPort: Word;
    sServerIP, sServerStart, sMessage, sSource, sDestination: string;
    bStart, bServerConnected, bLogSaved: Boolean;
    aIPList: TIPList;
  end;

const
  ConfigFile   = 'lanmsg_server.txt';
  LogDirectory = 'logs';
  DefaultPort  = 5600;

var
  fLANMsgServer: TfLANMsgServer;

implementation

{$R *.lfm}

{ Pause application for n seconds }

procedure Wait(Interval: Integer);

// I previewed this procedure for the case where the server would go offline before
// the clients would be aware of it (send of the "bye" message not done in time)
// As it seems, there is no delay needed (at least not on my system)
// Should you experience any problems, add some delay code here...

begin

end;

{ Read server IP and port from text file }

procedure ServerConfigRead(var ServerIP: string; var ServerPort: Word);

var
  Line: string;
  InFile: Text;

begin
  ServerIP := ''; ServerPort := 0;
  Assign(InFile, ConfigFile); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      if (Length(Line) > 4) and (LeftStr(Line, 3) = 'ip=') then
        ServerIP := Trim(Copy(Line, 4, Length(Line)))
      else if (Length(Line) > 6) and (LeftStr(Line, 5) = 'port=') then
        ServerPort := StrToInt(Trim(Copy(Line, 6, Length(Line))));
    end;
  end;
  Close(InFile);
end;

{ Write server IP and port to text file }

procedure ServerConfigWrite(ServerIP: string; ServerPort: Word);

var
  Line: string;
  OutFile: Text;

begin
  Assign(OutFile, ConfigFile); Rewrite(OutFile);
  Line := 'ip=' + ServerIP; Writeln(OutFile, Line);
  Line := 'port=' + IntToStr(ServerPort); Writeln(OutFile, Line);
  Close(OutFile);
end;

{ Update server log }

function UpdateServerLog(Msg: string): string;

var
  SNow: string;

begin
  DateTimeToString(SNow, 'yyyy.mm.dd hh:mm:ss', Now);
  fLANMsgServer.edServerLog.Lines.AddText(SNow + '  ' + Msg);
  Result := SNow;
end;

{ Send a message (to one specific client or all clients) }

procedure SendMessage(Mess, Dest, Source: string);

var
  I: Integer;

begin
  fLANMsgServer.ltcpServer.IterReset; I := 0;
  while fLANMsgServer.ltcpServer.IterNext do begin
    // For all clients in the internal list
    Inc(I);
    if (Dest = 'all') or (Dest = fLANMsgServer.ltcpServer.Socks[I].PeerAddress) then begin
      // Consider send the message if client is meant to receive it
      if fLANMsgServer.ltcpServer.Iterator.Connected then begin
        // Only send the message if the client is actually connected
        if (Dest <> 'all') or (fLANMsgServer.ltcpServer.Iterator.PeerAddress <> Source) then
          // Do not send to sender who sent to all users (may send to themselves...)
          fLANMsgServer.ltcpServer.SendMessage(Mess, fLANMsgServer.ltcpServer.Iterator);
      end;
    end;
  end;
end;

{ Update IP/user list }

procedure UpdateIPList(IP, User0: string; var IPList: TIPList);

var
  I, J, N: Integer;
  User: string;
  NewIP, UserExists: Boolean;

begin
  NewIP := True; User := User0;
  for I := 0 to Length(IPList) - 1 do begin
    // IP already in the list
    if IPList[I].IP = IP then begin
      NewIP := False;
      // Check if user name is already in use
      N := 1;
      repeat
        UserExists := False;
        for J := 0 to Length(IPList) - 1 do begin
          if IPList[J].User = User then
            UserExists := True;
        end;
        if UserExists then begin
          // If user name already in use, add (1), (2), ...
          Inc(N);
          User := User0 + '(' + IntToStr(N) + ')';
        end;
      until not UserExists;
      // Add user name for this IP
      IPList[I].User := User;
    end;
  end;
  // IP doesn't yet exist
  if NewIP then begin
    // Create new table entry
    SetLength(IPList, Length(IPList) + 1);
    IPList[Length(IPList) - 1].IP := IP;
    IPList[Length(IPList) - 1].User := '';
  end;
end;

{ Get user name for given IP }

function GetUser(var IPList: TIPList; IP: string): string;

var
  I: Integer;
  User: string;

begin
  for I := 0 to Length(IPList) - 1 do begin
    if IPList[I].IP = IP then
      User := IPList[I].User;
  end;
  Result := User;
end;

{ Get IP address for given user name }

function GetIP(var IPList: TIPList; User: string): string;

var
  I: Integer;
  IP: string;

begin
  for I := 0 to Length(IPList) - 1 do begin
    if IPList[I].User = User then
      IP := IPList[I].IP;
  end;
  Result := IP;
end;

{ Rewrite users string grid and send user list to all clients }

procedure UpdateAndSendClientList(IPList: TIPList);

var
  I, N: Integer;
  MsgText, IP, User: string;

begin
  for I := 1 to 10 do begin
    fLANMsgServer.sgClients.Cells[0, I] := '';
    fLANMsgServer.sgClients.Cells[1, I] := '';
  end;
  MsgText := 'list';
  fLANMsgServer.ltcpServer.IterReset;
  N := 0;
  while fLANMsgServer.ltcpServer.IterNext do begin
    // For all clients in the internal list
    if fLANMsgServer.ltcpServer.Iterator.Connected then begin
      // If client is connected, update the string grid and add client IP to the "list" message string
      // "list" message string: list <user1> <user2> ...
      IP := fLANMsgServer.ltcpServer.Iterator.PeerAddress;
      for I := 0 to Length(IPList) - 1 do begin
        if IPList[I].IP = IP then
          User := IPList[I].User;
      end;
      Inc(N);
      fLANMsgServer.sgClients.Cells[0, N] := User;
      fLANMsgServer.sgClients.Cells[1, N] := IP;
      MsgText += ' ' + User;
    end;
  end;
  // Send the user list to all clients
  SendMessage(MsgText, 'all', '');
end;

{ Save server log to file }

procedure SaveLog(ServerStart: string);

var
  Filename: string;

begin
  Filename := GetCurrentDir + '\' + LogDirectory + '\lanmsg' + StringReplace(ServerStart, ':', '_', [rfReplaceAll]);
  Filename := StringReplace(Filename, '.', '_', [rfReplaceAll]) + '.log';
  fLANMsgServer.edServerLog.Lines.SaveToFile(Filename);
end;

{****************}
{ TfLANMsgServer }
{****************}

{ Application start: Initialization }

procedure TfLANMsgServer.FormCreate(Sender: TObject);

begin
  bStart := True; bServerConnected := False;
  bLogsaved := False; sServerStart := '';
end;

{ Form becoming active: At application start, read server configuration file (if it exists) }

procedure TfLANMsgServer.FormActivate(Sender: TObject);

begin
  if bStart then begin
    // Application start: Read server configuration file (if it exists)
    if FileExists(ConfigFile) then begin
      ServerConfigRead(sServerIP, iServerPort);
    end
    else begin
      sServerIP := ''; iServerPort := DefaultPort;
    end;
    edServerIP.Text := sServerIP; edServerPort.Text := IntToStr(iServerPort);
    bStart := False;
  end;
  if sServerIP = '' then
    edServerIP.SetFocus
  else if iServerPort = 0 then
    edServerPort.SetFocus
  else
    btStart.SetFocus;
end;

{ Menu item "Server > Exit": Exit application }

procedure TfLANMsgServer.mServerExitClick(Sender: TObject);

begin
  if bServerConnected then begin
    SendMessage('bye', 'all', '');
    Wait(5);
    ltcpServer.Disconnect(True);
  end;
  if (sServerStart <> '') and (not bLogSaved) then
    SaveLog(sServerStart);                                                     // save server log (if not already done)
  Close;
end;

{ Menu item "Settings > Server configuration": Set server IP and port (and save to text file) }

procedure TfLANMsgServer.mSettingsConfigClick(Sender: TObject);

begin
  if not bServerConnected then begin
    fConfig.sConfig := 'server';
    if iServerPort = 0 then
      fConfig.edPort.Text := ''
    else
      fConfig.edPort.Text := IntToStr(iServerPort);
    fConfig.edIP.Text := sServerIP;
    fConfig.ShowModal;
    if fConfig.sButton = 'save' then begin
      // Settings were changed: Apply and save to file
      iServerPort := fConfig.iServerPort;
      sServerIP   := fConfig.sServerIP;
      edServerPort.Text := IntToStr(iServerPort);
      edServerIP.Text := sServerIP;
      ServerConfigWrite(sServerIP, iServerPort);
    end;
  end;
end;

{ Menu item "Help > About": Display application about }

procedure TfLANMsgServer.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Simple LAN messenger client-server application.' + LineEnding;
  S += 'LAN messenger server.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, December 2023 - April 2025.';
  MessageDlg('About "LANMsgServer"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Stop": Start resp. stop the server }

procedure TfLANMsgServer.btStartClick(Sender: TObject);

var
  I: Integer;

begin
  if btStart.Caption = 'Start' then begin
    // Button "Start": Start the server
    sServerIP := edServerIP.Text; iServerPort := StrToInt(edServerPort.Text);
    if (sServerIP <> '') and (iServerPort <> 0) then begin
      ltcpServer.Host := sServerIP;
      bServerConnected := ltcpServer.Listen(iServerPort);
      if bServerConnected then begin
        shServerStatus.Brush.Color := clLime;
        sServerStart := UpdateServerLog('LAN Messenger server started at IP=' + sServerIP + ', port=' + IntToStr(iServerPort));
        btStart.Caption := 'Stop';
        bLogSaved := False;
      end;
    end;
  end
  else begin
    // Button "Stop": Stop the server
    if bServerConnected then begin
      SendMessage('bye', 'all', '');
      Wait(5);
      ltcpServer.Disconnect(True);
      bServerConnected := False;
      shServerStatus.Brush.Color := clRed;
      for I := 1 to 10 do begin
        sgClients.Cells[0, I] := '';
        sgClients.Cells[1, I] := '';
      end;
      UpdateServerLog('LAN Messenger server stopped');
      SaveLog(sServerStart);
      bLogsaved := True; sServerStart := '';
      btStart.Caption := 'Start';
    end;
  end;
end;

{ Event triggered when a client connects }

procedure TfLANMsgServer.ltcpServerAccept(aSocket: TLSocket);

begin
  UpdateServerLog('LAN Messenger client with IP=' + aSocket.PeerAddress + ' connected');
  UpdateIPList(aSocket.PeerAddress, '', aIPList);                              // Add IP to IP/users list
end;

{ Event triggered when a client disconnects }

{procedure TfLANMsgServer.ltcpServerDisconnect(aSocket: TLSocket);

// This feature seems not to work correctly, the event never being triggered!?

begin
  edServerLog.Lines.AddText('LAN Messenger client with IP=' + aSocket.PeerAddress + ' disconnected');
  UpdateAndSendClientList(aSocket.PeerAddress);
end;}

{ Event triggered when there was a communication error (should not happen...) }

procedure TfLANMsgServer.ltcpServerError(const msg: string; aSocket: TLSocket);

begin
  MessageDlg('Network error', 'LANMsgServer returned the error message: ' + msg, mtError, [mbOK], 0);
end;

{ Event triggered when the server receives a message }

procedure TfLANMsgServer.ltcpServerReceive(aSocket: TLSocket);

var
  P: Integer;
  User: string;

begin
  // Get sender IP and message text
  sSource := aSocket.PeerAddress;  aSocket.GetMessage(sMessage);
  if sMessage = 'bye' then begin
    // Command "bye" = Client will go offline: Send new user list to all clients
    aSocket.Disconnect(True);
    UpdateServerLog('LAN Messenger user ' + GetUser(aIPList, sSource) + ' disconnected');
    UpdateAndSendClientList(aIPList);
  end
  else if LeftStr(sMessage, 4) = 'user' then begin
    // Command "user" = Client sent user name: Update IP/users list and send users list to all clients
    User := Copy(sMessage, 6, Length(sMessage));
    UpdateServerLog('LAN Messenger client with IP=' + sSource + ' sent user name = ' + User);
    UpdateIPList(aSocket.PeerAddress, User, aIPList);
    UpdateAndSendClientList(aIPList);
  end
  else if LeftStr(sMessage, 3) = 'msg' then begin
    // Command "msg" = Server got a message to relay to one or all client(s): Relay the message
    // Server message has format: msg <sender-user> <message-text>
    Delete(sMessage, 1, 4);
    P := Pos(' ', sMessage);
    User := Copy(sMessage, 1, P - 1);
    Delete(sMessage, 1, P);
    if User = 'all' then begin
      // Message for all users
      sDestination := 'all';
    end
    else begin
      // Message for a given user
      sDestination := GetIP(aIPList, User);
    end;
    sMessage := 'msg ' + GetUser(aIPList, sSource) + ' ' + sMessage;
    SendMessage(sMessage, sDestination, sSource);                              // send the message
    // Update server log
    if User = 'all' then
      UpdateServerLog('Relayed message from ' + GetUser(aIPList, sSource) + ' to all users')
    else
      UpdateServerLog('Relayed message from ' + GetUser(aIPList, sSource) + ' to ' + User);
  end;
end;

end.

