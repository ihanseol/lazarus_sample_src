{**************************************}
{* Main unit for NetTools application *}
{**************************************}

unit net;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, process, cmd, tools, help;

type
  {************}
  { TfNetTools }
  {************}
  TfNetTools = class(TForm)
    edMac: TMemo;
    mMenu: TMainMenu;
    mFile, mFileTools, mFileExit: TMenuItem;
    mHelp: TMenuItem;
    mHelpHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label6: TLabel;
    edHost, edIPIntEth, edIPIntWifi, edIPExt: TEdit;
    Shape1: TShape;
    rbNetstat, rbTracert, rbIpconfig, rbArp, rbPing, rbRoute, rbNSLookup, rbWhois: TRadioButton;
    memoNetTools: TMemo;
    btRun: TButton;
    prCmd: TProcess;
    tiCmd: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mFileToolsClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure rbArpChange(Sender: TObject);
    procedure rbIpconfigChange(Sender: TObject);
    procedure rbNetstatChange(Sender: TObject);
    procedure rbNSLookupChange(Sender: TObject);
    procedure rbPingChange(Sender: TObject);
    procedure rbRouteChange(Sender: TObject);
    procedure rbTracertChange(Sender: TObject);
    procedure rbWhoisChange(Sender: TObject);
    procedure btRunClick(Sender: TObject);
    procedure tiCmdTimer(Sender: TObject);
  private
    sTool, sCommand: string;
    bStart, bInfo, bCmd: Boolean;
    aTools: TTools;
  end;

var
  fNetTools: TfNetTools;

implementation

{$R *.lfm}

{ Read tools' path from text file}

procedure ReadTools(out Tools: TTools);

Const
  // The paths in the file must be in this order!
  ToolsList: array[0 .. NTools - 1] of string = (
    'hostname', 'getmac', 'ipconfig', 'arp', 'route', 'ping', 'tracert', 'nslookup', 'whois', 'netstat', 'curl'
  );

var
  I: Integer;
  Line: string;
  InFile: Text;

begin
  for I := 0 to NTools - 1 do begin
    // Set tool's name and clear all paths
    Tools[I].Name := ToolsList[I];
    Tools[I].Path := '';
  end;
  if FileExists('nettools.txt') then begin
    // If the file nettools.txt exists, read the tools' path
    Assign(InFile, 'nettools.txt'); Reset(InFile);
    I := 0;
    while not EoF(InFile) do begin
      Readln(InFile, Line); Line := Trim(Line);
      if Line <> '' then begin
        Inc(I);
        Tools[I - 1].Path := Line;
      end;
    end;
    Close(InFile);
  end;
end;

{ Write tools' path to text file }

procedure WriteTools(var Tools: TTools);

var
  I: Integer;
  OutFile: Text;

begin
  Assign(OutFile, 'nettools.txt'); Rewrite(OutFile);
  for I := 0 to NTools - 1 do
    Writeln(OutFile, Tools[I].Path);
  Close(OutFile);
end;

{ Search MS Windows system directories for network command line tools }

procedure SearchTools(var Tools: TTools);

Const
  SearchPath = 'C:\Windows;C:\Windows\System;C:\Windows\System32';

var
  I: Integer;
  Path, Filename: string;

begin
  for I := 0 to NTools - 1 do begin
    // If there was a program indicated in the text file list, use this one
    Path := Tools[I].Path;
    if Path = 'NOT AVAILABLE' then
      Path := '';
    // If there wasn't a program indicated in the text file list, try to find one in the Windows system directories
    if Path = '' then begin
      Filename := Tools[I].Name + '.exe';
      Path := FileSearch (Filename, SearchPath, [sfoImplicitCurrentDir]);
    end;
    if Path <> '' then begin
      // Program has been found
      Tools[I].Path := Path;
    end
    else begin
      // Program hasn't been found: Tool unavailable
      Tools[I].Path := 'NOT AVAILABLE';
      MessageDlg('File not found', Filename + ' not found! ' + Tools[I].Name + ' feature will not be available!', mtWarning, [mbOK], 0);
    end;
  end;
end;

{************}
{ TfNetTools }
{************}

{ Application start: Initialisation }

procedure TfNetTools.FormCreate(Sender: TObject);

begin
  // Set properties of the Windows process (to run command prompt)
  prCmd.Options := [poUsePipes];
  prCmd.ShowWindow:= swoHIDE;                                                  // command prompt running in the background; display in "console" memo
  prCmd.Executable:= 'cmd';                                                    // command prompt executable = cmd.exe
  // Set "application startup" variable to True
  bStart := True;
end;

{ First window activation: Search tool programs and determine computer's network configuration }

procedure TfNetTools.FormActivate(Sender: TObject);

// The simplest way to avoid getting error messages (and application abortion), if during form creation, you try to
// do sth on an object, that is not yet ready, is (at least, I think so?) to place the code within the FormActivate
// (instead of the FormCreate) method, and executing it at application startup, using a Boolean set to True within
// the FormCreate method.

begin
  // Run this code only at application startup
  if bStart then begin
    // Read tools' path from text file
    ReadTools(aTools);
    // Search Windows system directories for tools, that have no path set in the file
    SearchTools(aTools);
    // Rewrite the paths text file
    WriteTools(aTools);
    // Activate the command prompt process
    if not prCmd.Active then
      prCmd.Active := True;
    // Set variables, used by the timer routine (bInfo = True: computer network settings)
    bInfo := True; bCmd := False;
    // First thing to do is to get computer's name (running hostname.exe)
    sTool := 'hostname';
    // Clear the "console memo" (on fCmd form)
    fCmd.memoCmd.Clear;
    // Enable the timer (all code to run the tool and capture its output in the timer routine)
    tiCmd.Enabled := True;
    // Open the "console" window
    fCmd.ShowModal;                                                            // using ShowModal allows to deactivate process and timer when tool execution is done
    // Set "application start" variable to False (method will no more be executed when form becomes activa)
    bStart := False;
  end;
end;

{ Menu item "File > Tools": Open "tools paths" window }

procedure TfNetTools.mFileToolsClick(Sender: TObject);

begin
  fTools.aTools := aTools;
  fTools.ShowModal;
  // Read tools paths from fTools window (as may hav been modified by user)
  aTools := fTools.aTools;
  // Rewrite the tools paths text file
  WriteTools(aTools);
end;

{ Menu item "File > Exit": Exit application }

procedure TfNetTools.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > Help": Open application help text window }

procedure TfNetTools.mHelpHelpClick(Sender: TObject);

begin
  if not fHelp.Visible then
    fHelp.Show
  else
    fHelp.Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfNetTools.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Network tools user interface:' + LineEnding;
  S += 'Simple graphical user interface, to launch the most common MS Windows network related command line programs.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, July 2018 - August 2020.';
  MessageDlg('About "NetTools"', S, mtInformation, [mbOK], 0);
end;

{ Button "Run": Run a given tool }

procedure TfNetTools.btRunClick(Sender: TObject);

// In fact, setting the tool's variables and starting the process and timer
// The code to run the tool and capture its output is within the timer routine

var
  I: Integer;
  Param: string;

begin
  for I := 0 to NTools - 1 do begin
    if aTools[I].Name = sTool then begin
      // Tool found in list: Get its path (= program to be run in command propmpt)
      if aTools[I].Path = 'NOT AVAILABLE' then begin
        // Tool unavailable
        MessageDlg('Invalid selection', 'The ' + sTool + ' feature isn''t available on your computer!', mtError, [mbOK], 0);
      end
      else begin
        // Tool available: configure its parameters (ask user for input, if needed) and run it
        // Note, that user input actually isn't checked...
        Param := '';
        if sTool = 'arp' then
          Param := '-a'
        else if sTool = 'route' then
          Param := 'print'
        else if (sTool = 'ping') or (sTool = 'tracert') then
          Param := InputBox(sTool + ' parameter entry', 'Enter hostname or IP address', '')
        else if sTool = 'nslookup' then
          Param := InputBox(sTool + ' parameter entry', 'Enter IP address', '')
        else if sTool = 'whois' then
          Param := InputBox(sTool + ' parameter entry', 'Enter DNS name or IP address', '');
        // The command for the process is the tool command line program name, plus this program's parameters
        sCommand := aTools[I].Path + ' ' + Param + LineEnding;
        // Activate the process
        if not prCmd.Active then
          prCmd.Active := True;
        // Set variables, used by the timer routine (bInfo = False: one tool to be executed)
        bInfo := False; bCmd := False;
        // Start the timer
        tiCmd.Enabled := True;
        // Open the "console" window
        fCmd.memoCmd.Clear;
        fCmd.ShowModal;
        // As I used ShowModal to open the window, the method stops executing here, until that window is closed by the user
        // Thus the command is terminated with all output captured (or aborted by user) and the process and timer may be deactivated
        tiCmd.Enabled := False;
        prCmd.Active := False;
      end;
    end;
  end;
end;

{ Timer routine: Run tool(s), capture their output, display this output in the "console memo" (form fCmd) }

procedure TfNetTools.tiCmdTimer(Sender: TObject);

var
  Buffer: array[0..65535] of Char;

var
  CmdLines, I, P: Integer;
  Command: string;
  Found: Boolean;

begin
  if bInfo then begin
    // Get computer network settings
    if not bCmd then begin
      // Tool has not yet been run: Do so now
      Command := '';
      // Get program path corresponding to the tool
      for I := 0 to NTools - 1 do begin
        if aTools[I].Name = sTool then begin
          if aTools[I].Path <> 'NOT AVAILABLE' then begin
            if aTools[I].Name = 'curl' then
              Command := aTools[I].Path + ' http://dynupdate.no-ip.com/ip.php' + LineEnding
            else
              Command := aTools[I].Path + LineEnding;
          end;
        end;
      end;
      // Run the program in command prompt
      prCmd.Input.Write(Command[1], Length(Command));
      bCmd := True;                                                            // next action will be to capture the tool's output
    end
    else begin
      // Tool has been run: Capture its output
      if prCmd.Output.NumBytesAvailable > 0 then begin
        // If program output is available
        while prCmd.Output.NumBytesAvailable > 0 do begin
          // Read program output and write it to the "console memo"
          for I := 0 to 65535 do
            Buffer[I] := #0;
          prCmd.Output.Read(Buffer, SizeOf(Buffer) - 1);
          fCmd.memoCmd.Lines.Add(Buffer);
        end;
        // Analyse program output, depending on tool executed in order to determine compuer's network settings
        if sTool = 'hostname' then begin
          // Computer name
          if fCmd.memoCmd.Lines[fCmd.memoCmd.Lines.Count - 1] = GetCurrentDir + '>' then begin
            // Proceed only of last line in command prompt is [current directory]>
            // This makes sure, that the tool execution is terminated and all output has been catched
            edHost.Text := '';
            for I := fCmd.memoCmd.Lines.Count - 2 downto fCmd.memoCmd.Lines.Count - 4 do begin
              // Parse 3 last output lines: if one of them contains data, it is the hostname
              if Length(fCmd.memoCmd.Lines[I]) > 0 then
                edHost.Text := fCmd.memoCmd.Lines[I];
            end;
            CmdLines := fCmd.memoCmd.Lines.Count;                              // actual number of lines in command prompt
            // Next tool to execute is getmac
            sTool := 'getmac';
            bCmd := False;                                                     // next action will be to run a tool
          end;
        end
        else if sTool = 'getmac' then begin
          // MAC addresses
          if fCmd.memoCmd.Lines[fCmd.memoCmd.Lines.Count - 1] = GetCurrentDir + '>' then begin
            edMac.Clear;
            for I := CmdLines + 1 to fCmd.memoCmd.Lines.Count - 2 do begin
              // Parse lines that have been output by getmac. The MAC address mandatorily contains a hyphen
              if Copy(fCmd.memoCmd.Lines[I], 3, 1) = '-' then begin
                //edMac.Lines.AddText(LeftStr(fCmd.memoCmd.Lines[I], 17));
                edMac.Text := edMac.Text + LeftStr(fCmd.memoCmd.Lines[I], 17);
                if Length(fCmd.memoCmd.Lines[I + 1]) > 0 then
                  edMac.Text := edMac.Text + LineEnding;
              end;
            end;
            CmdLines := fCmd.memoCmd.Lines.Count;
            // Next tool to execute is ipconfig
            sTool := 'ipconfig';
            bCmd := False;                                                     // next action will be to run a tool
          end;
        end
        else if sTool = 'ipconfig' then begin
          // Internal IP addresses (limited to 2 network cards: Ethernet and WiFi)
          if fCmd.memoCmd.Lines[fCmd.memoCmd.Lines.Count - 1] = GetCurrentDir + '>' then begin
            edIPIntEth.Text := ''; edIPIntWiFi.Text := '';
            I := CmdLines + 1;
            // Parse lines that have been output by ipconfig. Search for ethernet card and WiFi card entries
            // When found, parse following lines to find the IPv4 entry (with the IP to be displayed)
            repeat
              Inc(I); Found := False;
              P := Pos('Ethernet adapter Ethernet', fCmd.memoCmd.Lines[I]);
              if P > 0 then begin
                // Ethernet card entry found
                repeat
                  Inc(I);
                  // Check if card is actually not connected to the Internet
                  P := Pos('Media', fCmd.memoCmd.Lines[I]);
                  if P > 0 then begin
                    edIPIntEth.Text := 'not connected';
                    Found := True;
                  end;
                  // Check if card has an IPv4 address set
                  P := Pos('IPv4', fCmd.memoCmd.Lines[I]);
                  if P > 0 then begin
                    P := Pos(':', fCmd.memoCmd.Lines[I]) + 2;
                    edIPIntEth.Text := Copy(fCmd.memoCmd.Lines[I], P, Length(fCmd.memoCmd.Lines[I]) - P + 1);
                    Found := True;
                  end;
                until Found or (I > fCmd.memoCmd.Lines.Count - 2);
              end
              else begin
                // WiFi card entry found
                P := Pos('Wireless LAN adapter WiFi', fCmd.memoCmd.Lines[I]);
                if P > 0 then begin
                  repeat
                    Inc(I); Found := False;
                    // Check if card is actually not connected to the Internet
                    P := Pos('Media', fCmd.memoCmd.Lines[I]);
                    if P > 0 then begin
                      edIPIntWiFi.Text := 'not connected';
                      Found := True;
                    end;
                    // Check if card has an IPv4 address set
                    P := Pos('IPv4', fCmd.memoCmd.Lines[I]);
                    if P > 0 then begin
                      P := Pos(':', fCmd.memoCmd.Lines[I]) + 2;
                      edIPIntWiFi.Text := Copy(fCmd.memoCmd.Lines[I], P, Length(fCmd.memoCmd.Lines[I]) - P + 1);
                      Found := True;
                    end;
                  until Found or (I > fCmd.memoCmd.Lines.Count - 2);
                end;
              end;
            until I > fCmd.memoCmd.Lines.Count - 2;
            CmdLines := fCmd.memoCmd.Lines.Count;
            // Next tool to run is curl
            sTool := 'curl';
            bCmd := False;                                                     // next action will be to run a tool
          end;
        end
        else if sTool = 'curl' then begin
          // External IP address
          if fCmd.memoCmd.Lines[fCmd.memoCmd.Lines.Count - 1] = GetCurrentDir + '>' then begin
            edIPExt.Text := ''; Found := False;
            // Parse lines that have been output by curl. If the HTTP request to no-ip.com has returned an IP
            // there will be a line with this IP, i.e. a line starting with a number
            for I := CmdLines + 1 to fCmd.memoCmd.Lines.Count - 2 do begin
              if Length(fCmd.memoCmd.Lines[I]) > 0 then begin
                P := Pos('curl: (6)', fCmd.memoCmd.Lines[I]);
                if P > 0 then begin
                  // This is a common curl error message, when there is no network connection
                  edIPExt.Text := 'not connected';
                  edIPExt.Color := clRed;
                end
                else begin
                  if LeftStr(fCmd.memoCmd.Lines[I], 1)[1] in ['0' .. '9'] then begin
                    edIPExt.Text := fCmd.memoCmd.Lines[I];
                    edIPExt.Color := clLime;
                    Found := True;
                  end;
                end;
              end;
            end;
            if not Found then begin
              // No IP returned: computer is not connected to the Internet
              edIPExt.Text := 'not connected';
              edIPExt.Color := clRed;
            end;
            // All tools needed to determine the computer's network configuration have been run. To avoid any possible mess,
            // the best is to deactivate the process and the timer and reactivate it each time when a tool is executed (when
            // the "Run" button is pushed).
            tiCmd.Enabled := False;
            prCmd.Active := False;
            rbIpconfig.Checked := True;
          end;
        end
      end;
    end;
  end
  else begin
    // Given tool execution resp. capture of its output
    if not bCmd then begin
      // Tool hasn't yet been executed: Do so now
      prCmd.Input.Write(sCommand[1], Length(sCommand));
      bCmd := True;
    end
    else begin
      // Tool has been executed: capture its output
      if prCmd.Output.NumBytesAvailable > 0 then begin
        // If there is output available in command prompt, write it to the "console memo"
        // Capture will continue until the user closes the window (and she has to, as I used ShowModal).
        // The process and timer will then be deactivated (may be somewhat annoying to have to close
        // the window before running another tool, but it's the easiest and surest way to avoid any
        // possible problems and to catch all tool output correctly
        while prCmd.Output.NumBytesAvailable > 0 do begin
          FillChar(Buffer, SizeOf(Buffer), #0);
          prCmd.Output.Read(Buffer, SizeOf(Buffer) - 1);
          fCmd.memoCmd.Lines.Add(Buffer);
        end;
      end;
    end
  end;
end;

{ Tools radio buttons change: Select tool and display tool's details }

procedure TfNetTools.rbArpChange(Sender: TObject);

begin
  if rbArp.Checked then begin
    sTool := 'arp';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "arp" command.');
    memoNetTools.Lines.Append('Utility that may be used for showing your local machine''s address resolution cache, i.e. the IP-to-physical-address translation tables.');
    memoNetTools.Lines.Append('Usage: "arp -a".');
  end;
end;

procedure TfNetTools.rbIpconfigChange(Sender: TObject);

begin
  if rbIpconfig.Checked then begin
    sTool := 'ipconfig';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "ipconfig" command.');
    memoNetTools.Lines.Append('Utility used for finding network information about your local machine (IP addresses, gateway, DNS servers).');
    memoNetTools.Lines.Append('Usage: "ipconfig".');
  end;
end;

procedure TfNetTools.rbNetstatChange(Sender: TObject);

begin
  if rbNetstat.Checked then begin
    sTool := 'netstat';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "netstat" command.');
    memoNetTools.Lines.Append('Utility used for displaying information about your local machine''s TCP and UDP connections and ports.');
    memoNetTools.Lines.Append('Usage: "netstat".');
  end;
end;

procedure TfNetTools.rbNSLookupChange(Sender: TObject);

begin
  if rbNSLookup.Checked then begin
    sTool := 'nslookup';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "NSlookup" command.');
    memoNetTools.Lines.Append('Tool for checking DNS record entries.');
    memoNetTools.Lines.Append('Usage: "nslookup IP-address".');
  end;
end;

procedure TfNetTools.rbPingChange(Sender: TObject);

begin
  if rbPing.Checked then begin
    sTool := 'ping';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "ping" command.');
    memoNetTools.Lines.Append('Networking utility for detecting devices on a network and for troubleshooting network problems.');
    memoNetTools.Lines.Append('Usage: "ping hostname" or "ping IP-address".');
  end;
end;

procedure TfNetTools.rbRouteChange(Sender: TObject);

begin
  if rbRoute.Checked then begin
    sTool := 'route';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "route" command.');
    memoNetTools.Lines.Append('Utility that may be used for displaying your local machine''s route table.');
    memoNetTools.Lines.Append('Usage: "route print".');
  end;
end;

procedure TfNetTools.rbTracertChange(Sender: TObject);

begin
  if rbTracert.Checked then begin
    sTool := 'tracert';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "tracert" command.');
    memoNetTools.Lines.Append('Networking utility for tracing the route to devices on a network.');
    memoNetTools.Lines.Append('Usage: "tracert hostname" or "tracert IP-address".');
  end;
end;

procedure TfNetTools.rbWhoisChange(Sender: TObject);

begin
  if rbWhois.Checked then begin
    sTool := 'whois';
    memoNetTools.Clear;
    memoNetTools.Lines.Append('The "whois" utility.');
    memoNetTools.Lines.Append('Tool used for displaying a domain''s registration record, by looking up the WHOIS database.');
    memoNetTools.Lines.Append('Usage: "whois DNS-name or "whois IP-address".');
  end;
end;

end.

