{*****************************************}
{* Main unit for NetworkShow application *}
{*****************************************}

unit net;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, Grids, process, cmd, help;

type
  TComputer = record
    Ip, Mac, Name, Dns, Description, State: string;
  end;
  TComputers = array of TComputer;
  {***********}
  { TfNetShow }
  {***********}
  TfNetShow = class(TForm)
    mMenu: TMainMenu;
    mNetwork, mNetworkNew, mNetworkExit: TMenuItem;
    mOptions, mOptionsConsole: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    sgNetwork: TStringGrid;
    btRun: TButton;
    tiCmd: TTimer;
    prCmd: TProcess;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mNetworkNewClick(Sender: TObject);
    procedure mNetworkExitClick(Sender: TObject);
    procedure mOptionsConsoleClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure tiCmdTimer(Sender: TObject);
  private
    iComputer: Integer;
    sDir, sPingPath, sArpPath, sNsLookupPath, sDnsSrv, sTool: string;
    bStart, bCmd: Boolean;
    aComputers: TComputers;
  end;

var
  fNetShow: TfNetShow;

implementation

{$R *.lfm}

{ Search MS Windows system directories for network command line tools }

function GetToolPath(Tool: string): string;

Const
  SearchPath = 'C:\Windows;C:\Windows\System;C:\Windows\System32';

var
  ToolExe, Path: string;
  MessType: TMsgDlgType;

begin
  ToolExe := Tool + '.exe';
  Path := FileSearch(ToolExe, SearchPath, [sfoImplicitCurrentDir]);
  if Path = '' then begin
    if Tool = 'ping' then
      MessType := mtError
    else
      MessType := mtWarning;
    MessageDlg('File error', 'The Windows "' + Tool + '" command has not been found!', MessType, [mbOK], 0);
  end;
  Result := Path;
end;

{ Check IP address validity }

function IpOk(IP: string): Boolean;

var
  V, C, I, P: Integer;
  Num: string;
  Ok: Boolean;

begin
  Ok := True;
  if Length(Ip) < 7 then
    Ok := False
  else begin
    for I := 1 to 4 do begin
      P := Pos('.', Ip);
      if (P > 0) or (I = 4) then begin
        if I = 4 then begin
          Num := Ip;
        end
        else begin
          Num := LeftStr(Ip, P - 1);
          Delete(Ip, 1, P);
        end;
        Val(Num, V, C);
        if C = 0 then begin
          if (V < 0) or (V > 255) then
            Ok := False
          else if (I = 4) and ((V = 0) or (V = 255)) then
            Ok := False;
        end
        else
          Ok := False
      end
      else
        Ok := False;
    end;
  end;
  Result := Ok;
end;

{ Read network description file }

procedure ReadNetwork(Filename: string; out Description, DnsSrv: string; out Computers: TComputers; out Mess: string);

var
  N, I: Integer;
  Line: string;
  DnsInList: Boolean;
  InFile: Text;

begin
  Mess := '';
  Description := ''; DnsSrv := ''; SetLength(Computers, 0);
  Assign(InFile, Filename); Reset(InFile);
  N := -2;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N);
      if N = -1 then
        Description := Trim(Line)                                              // Network description text
      else if N = 0 then begin
        DnsSrv := StringReplace(Trim(Line), 'DNS=', '', []);                   // DNS server address
        if not IPOk(DnsSrv) then
          Mess := 'Invalid IP address specified for DNS server';
      end
      else begin
        SetLength(Computers, N);
        with Computers[N - 1] do begin
          Name := Trim(LeftStr(Line, 15));                                     // host name
          Ip := Trim(Copy(Line, 17, 15));                                      // host IP address
          if (not IPOk(Ip)) and (Mess = '') then
            Mess := 'Invalid IP address specified for host ' + IntToStr(N);
          Description := '';                                                   // host description
          if Length(Line) > 33 then
            Description := Trim(Copy(Line, 33, Length(Line)));
          Mac := ''; Dns := '';
          State := 'unknown';
        end;
      end;
    end;
  end;
  Close(InFile);
  if Mess = '' then begin
    // If the DNS server is not in the list of the hosts to be checked, add it now
    DnsInList := False;
    for I := 0 to Length(Computers) - 1 do begin
      if Computers[I].Ip = DnsSrv then
        DnsInList := True;
    end;
    if not DnsInList then begin
      // Add DNS server to the list
      Inc(N);
      SetLength(Computers, N);
      with Computers[N - 1] do begin
        Name := ''; Description := '';
        Mac := ''; Ip := DnsSrv;
        State := 'unknown';
      end;
    end;
  end
  else begin
    Mess += '! Please, make correction in the network description file.';
    MessageDlg('Data error', Mess, mtError, [mbOK], 0);
  end;
end;

{ Fill the network string grid }

procedure FillNetwork(var Computers: TComputers; ArpPath, NsLookupPath, DnsSrv: string);

// Parse the "console" memo content to retrieve the network information

var
  N, C, R, U, T, I, J, P, P2: Integer;
  DnsState, Host, Ip, S: string;
  Done: Boolean;

begin
  N := 0; I := 0; Done := False; DnsState := 'unknown';
  // Get ping result (for each computer in the list)
  while not Done and (I < fCmd.memoCmd.Lines.Count - 1) do begin
    repeat
      P := Pos('Pinging', fCmd.memoCmd.Lines[I]);                              // start of ping info for current host
      Inc(I);
    until P > 0;
    Inc(N);
    C := 0; R := 0; U := 0; T := 0; Inc(I);
    repeat
      // For each of the 4 pings done, get the result and increment corresp. counter
      S := fCmd.memoCmd.Lines[I];
      if S <> '' then begin
        Inc(C);
        P := Pos('Timeout', S);
        if P > 0 then
          Inc(T)                                                               // ping result = timeout
        else begin
          P := Pos('Reply', S);
          if P > 0 then begin
            P := Pos('TTL=', S);
            if P > 0 then
              Inc(R)                                                           // ping result = reply
            else begin
              Inc(U);                                                          // ping result = unreachable
            end;
          end;
        end;
      end;
      Inc(I);
    until C = 4;                                                               // Windows ping sents 4 ICMP requests by default
    if R > 0 then
      fNetShow.sgNetwork.Cells[5, N] := 'alive'                                // if any reply from the host -> host is alive
    else if U = 4 then
      fNetShow.sgNetwork.Cells[5, N] := 'dead'                                 // if ping result was 4x "unreachable" -> host is dead
    else if T = 4 then
      fNetShow.sgNetwork.Cells[5, N] := 'unknown'                              // if ping result was 4x "timeout" -> host is unknown state (may be alive with ICMP blocking firewall)
    else
      fNetShow.sgNetwork.Cells[5, N] := 'dead';                                // all other results -> host is dead
    if Computers[N - 1].Ip = DnsSrv then
      DnsState := fNetShow.sgNetwork.Cells[5, N];                              // save state of DNS server
    Inc(I);
    if N = Length(Computers) then                                              // all hosts in the list done
      Done := True;
  end;
  // Get arp result (command run only once)
  if ArpPath <> '' then begin
    repeat
      Inc(I);
      P := Pos('arp.exe', fCmd.memoCmd.Lines[I]);                              // start of arp info
    until P > 0;
    Inc(I);
    repeat
      Inc(I);
      P := Pos('Interface', fCmd.memoCmd.Lines[I]);                            // start of arp data
    until P > 0;
    Inc(I);
    // If the IP in the arp table equals the IP of one of the hosts in the list, get the host's MAC address from the table
    repeat
      for J := 0 to Length(Computers) - 1 do begin
        Ip := Trim(Copy(fCmd.memoCmd.Lines[I], 3, 15));
        if Ip = Computers[J].Ip then
          fNetShow.sgNetwork.Cells[2, J + 1] := Trim(Copy(fCmd.memoCmd.Lines[I], 25, 17));
      end;
      Inc(I);
    until Pos(GetCurrentDir, fCmd.memoCmd.Lines[I]) <> 0;                      // end of arp command
    // Get nslookup result (for each computer in the list)
    if NsLookupPath <> '' then begin
      // Info available only if DNS server machine is alive
      if DnsState = 'alive' then begin
        N := 0;
        repeat
          // Do for each computer (IP) in the list
          repeat
            Inc(I);
            P := Pos('nslookup.exe', fCmd.memoCmd.Lines[I]);                   // start of nslookup command
          until P > 0;
          Inc(I);
          repeat
            // Parse lines until DNS name found or end of nslookup command is encountered
            S := fCmd.memoCmd.Lines[I];
            P := Pos('Name', S);                                               // nslookup host DNS name
            P2 := Pos(GetCurrentDir, S);                                       // end of nslookup command
            Host := '';
            if (P > 0) or (P2 > 0) then begin
              // Host's DNS name has been found
              if P > 0 then begin
                Host := StringReplace(S, 'Name:', '', []);
                Host := StringReplace(Host, ' ', '', [rfReplaceAll]);
              end
              else begin
                // Host's DNS name hasn't been found
                Host := '-unknown-';
              end;
              Inc(N);
              fNetShow.sgNetwork.Cells[4, N] := Host;
            end;
            Inc(I);
          until Host <> '';                                                    // host DNS name has been found or is unknown
          Inc(I);
        until N = Length(Computers);                                           // all hosts have been done
      end;
    end;
  end;
end;

{***********}
{ TfNetShow }
{***********}

{ Application start: Initialisation }

procedure TfNetShow.FormCreate(Sender: TObject);

begin
  sDir := GetCurrentDir;
  SetLength(aComputers, 0);
  // Set properties of the Windows process (to run command prompt)
  prCmd.Options := [poUsePipes];
  prCmd.ShowWindow:= swoHIDE;                                                  // command prompt running in the background; display in "console" memo
  prCmd.Executable:= 'cmd';                                                    // command prompt executable = cmd.exe
  // Set "application startup" variable to True
  bStart := True;
end;

{ First window activation: Search Windows Ping program }

procedure TfNetShow.FormActivate(Sender: TObject);

begin
  if bStart then begin
    // Run this code only at application startup
    sPingPath := GetToolPath('ping');
    if sPingPath = '' then
      btRun.Enabled := False
    else begin
      sArpPath := GetToolPath('arp');
      sNsLookupPath := GetToolPath('nslookup');
    end;
    // Set "application start" variable to False (method will no more be executed when form becomes active)
    bStart := False;
  end;
end;

{ Menu item "Network > New": Load network description file }

procedure TfNetShow.mNetworkNewClick(Sender: TObject);

var
  I, J: Integer;
  Filename, Description, Mess: string;

begin
  dlgOpen.InitialDir := sDir;
  dlgOpen.FileName := '';
  if dlgOpen.Execute then begin
    // User has selected a file
    Filename := dlgOpen.FileName;
    ReadNetwork(Filename, Description, sDnsSrv, aComputers, Mess);
    if Mess = '' then begin
      sDir := ExtractFileDir(Filename);                                         // save directory (to open next time at same location)
      // Fill the grid (IP addresses and other given information)
      for I := 1 to 15 do begin
        for J := 0 to 5 do
          sgNetwork.Cells[J, I] := '';
        if I <= Length(aComputers) then begin
          if (I = 1) or (aComputers[I - 1].Name <> aComputers[I - 2].Name) then
            sgNetwork.Cells[0, I] := aComputers[I - 1].Name;
          sgNetwork.Cells[1, I] := aComputers[I - 1].Description;
          sgNetwork.Cells[3, I] := aComputers[I - 1].Ip;
          sgNetwork.Cells[5, I] := aComputers[I - 1].State;
        end;
      end;
      btRun.Enabled := True;
    end
    else
      btRun.Enabled := False;
  end;
end;

{ Menu item "Network > Exit": Exit application }

procedure TfNetShow.mNetworkExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Show console window": Toggle show or not the "console window" }

procedure TfNetShow.mOptionsConsoleClick(Sender: TObject);

begin
  if mOptionsConsole.Checked then
    mOptionsConsole.Checked := False
  else
    mOptionsConsole.Checked := True;
end;

{ Menu item "Help > Help": Open application help text window }

procedure TfNetShow.mHelpHelpClick(Sender: TObject);

begin
  if not fHelp.Visible then
    fHelp.Show
  else
    fHelp.Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfNetShow.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Network tool:' + LineEnding;
  S += 'Show computers actually present on the network.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, February 2022.';
  MessageDlg('About "NetworkShow"', S, mtInformation, [mbOK], 0);
end;

{ Button "Run": Run network scan (ping, arp, nslookup) on computers in the list }

procedure TfNetShow.btRunClick(Sender: TObject);

// Pushing the button only starts the timer, the code to run the tools is within the timer routine

begin
  if Length(aComputers) > 0 then begin
    fCmd.memoCmd.Clear;
    if mOptionsConsole.Checked then
      fCmd.Show;
    iComputer := 0;                                                            // start with first host in the list
    sTool := 'ping'; bCmd := False;                                            // start wizh Ping tool (execution not yet done)
    tiCmd.Enabled := True;                                                     // enable the timer
  end;
end;

{ Timer routine: Run network tools, capture their output, display output in "console memo" (form fCmd) and fill in the network state }

procedure TfNetShow.tiCmdTimer(Sender: TObject);

var
  P: Integer;
  Command: string;
  Buffer: array[0..65535] of Char;

begin
  if not bCmd then begin
    // Command hasn't yet been executed: Do so now
    if not prCmd.Active then
      prCmd.Active := True;
    // The command for the process is the Ping/Arp/NsLookup command line program name, plus its parameters
    if sTool = 'ping' then
      Command := sPingPath + ' ' + aComputers[iComputer].IP + LineEnding
    else if sTool = 'arp' then
      Command := sArpPath + ' -a ' + LineEnding
    else
      Command := sNsLookupPath + ' ' + aComputers[iComputer].IP + LineEnding;
    if (sTool = 'ping') or ((sTool = 'arp') and (sArpPath <> '')) or ((sTool = 'nslookup') and (sNsLookupPath <> '')) then
      // Run Arp and NsLookup only if these programs have been found
      prCmd.Input.Write(Command[1], Length(Command));
    bCmd := True;                                                              // command has now be executed (may read the buffer...)
  end
  else begin
    // Tool has been executed: Capture its output
    if prCmd.Output.NumBytesAvailable > 0 then begin
      // If there is output available in command prompt, write it to the "console memo"
      // Capture will continue until the last line of coutput starts with the path to the current
      // directory, i.e. when, after the Ping has been done, controls comes back to the command line
      while prCmd.Output.NumBytesAvailable > 0 do begin
        FillChar(Buffer, SizeOf(Buffer), #0);
        prCmd.Output.Read(Buffer, SizeOf(Buffer) - 1);
        fCmd.memoCmd.Lines.Add(Buffer);
      end;
      P := Pos(GetCurrentDir, fCmd.memoCmd.Lines[fCmd.memoCmd.Lines.Count - 1]);
      if P > 0 then begin
        // Command for this host is terminated
        prCmd.Active := False;
        if sTool = 'ping' then begin
          // Ping tool terminated: Ping next host in the list; if all hosts are done, run Arp (for first host in the list)
          if iComputer < Length(aComputers) - 1 then begin
            bCmd := False;
            Inc(iComputer);
          end
          else begin
            sTool := 'arp';
            bCmd := False;
            iComputer := 0;
          end;
        end
        else if sTool = 'arp' then begin
          // Arp tool terminated: Run NsLookup (for all hosts in list)
          sTool := 'nslookup';
          bCmd := False;
          iComputer := 0;
        end
        else begin
          // NsLookup tool terminated: NsLookup next host in the list; if all hosts are done, de-activate the timer and fill in the network info
          if iComputer < Length(aComputers) - 1 then begin
            bCmd := False;
            Inc(iComputer);
          end
          else begin
            tiCmd.Enabled := False;
            FillNetwork(aComputers, sArpPath, sNsLookupPath, sDnsSrv);
          end;
        end;
      end;
    end;
  end;
end;

end.

