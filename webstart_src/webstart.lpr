{*********************************************************}
{* Start webbrowser with URL determined from file path   *}
{* passed as command line argument. This utility may in  *}
{* particular be used to start a Perl CGI or PHP script  *}
{* from within Komodo Edit. The program, adapted to my   *}
{* development environment, uses the file webstart.conf  *}
{* to make it usable by other people without the need to *}
{* change it. See the comments in that file for details. *}
{* To use "webstart" with Kommodo Edit, set "Run in" to  *}
{*  - "Command Output Tab" to run it silently            *}
{*  - "New Console" to view the program's output.        *}
{* If an error occurs, a Dos exit code (e.g. 1: file not *}
{* found) or a custon error code (250-253) is returned.  *}
{*-------------------------------------------------------*}
{* Version 1.0, © allu, December 2018 - February 2019    *}
{*********************************************************}

program webstart;

{$mode objfpc}{$H+}

uses
  Crt,
  Dos,
  SysUtils;

type
  TConfig = record
    FileCopy: Boolean;
    ServerPath, CGIPath, PHPPath: string;
    CGISubdirectory, PHPSubdirectory: Boolean;
    SeverName, ServerProtocol, ServerPort: string;
    CGIExtensions, PHPExtensions: array of string;
    BrowserPath, BrowserProgram: string;
  end;

const
  // Configuration file
  ConfFile = 'webstart.conf';
  // Custom error codes (as DOS exit codes, properly reported in Komodo Edit, even when running silently)
  ConfigError = 250;                                                           // configuration file error
  ParamError1 = 251;                                                           // no command line parameter (filename) specified
  ParamError2 = 252;                                                           // to mamy command line parameters specified
  ScriptError = 253;                                                           // can't determine script type (Perl or PHP)

var
  ErrCode: Integer;
  FilePath, Source, Destination, SubDir, WebAddress, ScriptType, ComStr: string;
  WebstartConfig: TConfig;

{ Read program settings from webstart.conf file }

procedure ReadConfiguration(Filename: string; out Conf: TConfig; out ErrCode: Integer);

const
  CGIExt: array[0..1] of string = ('.pl', '.pm');
  PHPExt: array[0..0] of string = ('.php');

var
  NValue, L, P, C: Integer;
  Line, Variable, Value, Ext, ErrMess: string;
  Extensions: array of string;
  InFile: Text;

begin
  ErrCode := 0;
  // Default settings for most variables (others must necessarily be set in the configuration file)
  with Conf do begin
    FileCopy := False;
    ServerPath := '?'; CGIPath := 'cgi-bin'; PHPPath := 'htdocs';
    CGISubdirectory := False; PHPSubdirectory := False;
    SeverName := 'localhost'; ServerProtocol := 'HTTP'; ServerPort := '80';
    CGIExtensions := CGIExt; PHPExtensions := PHPExt;
    BrowserPath := '?'; BrowserProgram := '?';
  end;
  // Read settings from file (use default values, if no value is given)
  Assign(InFile, Filename); Reset(InFile);
  ErrMess := ''; L := 0;
  while not EoF(InFile) and (ErrMess = '') do begin
    Readln(InFile, Line); Inc(L);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin                             // '#' starts a comment line
      P := Pos('=', Line);                                                               // all entries are of the form: variable=value
      // Valid line: "Variable=value" or "Variable="
      if P <> 0 then begin
        Variable := LeftStr(Line, P - 1);
        Value := '';
        if P <> Length(Line) then                                                        // defaults (if any!) will be used if no value specified
          Value := RightStr(Line, Length(Line) - P);
        // Variable "FileCopy"
        if Variable = 'FileCopy' then begin
          if Value = 'true' then
            Conf.FileCopy := True
          else if (Value = 'false') or (Value = '') then
            Conf.FileCopy := False                                                       // default = "false"
          else
            ErrMess := 'Invalid value for variable "FileCopy": ' + Value;                // neither "true" nor "false"
        end
        // Variable "ServerPath"
        else if Variable = 'ServerPath' then begin
          if Conf.FileCopy then begin                                                    // only considered if "FileCopy=true"
            if Value <> '' then
              Conf.ServerPath := Value
            else
              ErrMess := 'Missing value for variable "ServerPath"';                      // no default value (always error message)
          end;
        end
        // Variable "CGIPath"
        else if Variable = 'CGIPath' then begin
          if Conf.FileCopy then begin
            if Value <> '' then
              Conf.CGIPath := Value
            else
              Conf.CGIPath := 'cgi-bin';                                                 // default = "cgi-bin" (common CGI directory on Apache)
          end
        end
        // Variable "PHPPath"
        else if Variable = 'PHPPath' then begin
          if Conf.FileCopy then begin
            if Value <> '' then
              Conf.PHPPath := Value
            else
              Conf.PHPPath := 'htdocs';                                                  // default = "htdocs" (Apache document root)
          end
        end
        // Variable "CGISubdirectory"
        else if Variable = 'CGISubdirectory' then begin
          if Conf.FileCopy then begin
            if Value = 'true' then
              Conf.CGISubdirectory := True
            else if (Value = 'false') or (Value = '') then
              Conf.CGISubdirectory := False                                              // default = "false"
            else
              ErrMess := 'Invalid value for variable "CGISubdirectory": ' + Value;       // neither "true" nor "false"
          end;
        end
        // Variable "PHPSubdirectory"
        else if Variable = 'PHPSubdirectory' then begin
          if Conf.FileCopy then begin
            if Value = 'true' then
              Conf.PHPSubdirectory := True
            else if (Value = 'false') or (Value = '') then
              Conf.PHPSubdirectory := False                                              // default = "false"
            else
              ErrMess := 'Invalid value for variable "PHPSubdirectory": ' + Value;       // neither "true" nor "false"
          end;
        end
        // Variable "SeverName"
        else if Variable = 'SeverName' then begin
          if Value <> '' then
            Conf.SeverName := Value
          else
            Conf.SeverName := 'localhost';                                               // default = "localhost" (local computer)
        end
        // Variable  "ServerProtocol"
        else if Variable = 'ServerProtocol' then begin
          if Value = '' then
            Value := 'HTTP';                                                             // default = "HTTP" (unsecured website access)
          if (Value = 'HTTP') or (Value = 'HTTPS') then
            Conf.ServerProtocol := Value
          else
            ErrMess := 'Invalid value for variable "ServerProtocol": ' + Value;          // neither "HTTP" nor "HTTPS"
        end
        // Variable "ServerPort"
        else if Variable = 'ServerPort' then begin
          if Value <> '' then begin
            Val(Value, NValue, C);
            if C = 0 then
              Conf.ServerPort := Value
            else
              ErrMess := 'Invalid value for variable "ServerPort": ' + Value;            // server port must be numeric
          end
          else begin
            if Conf.ServerProtocol = 'HTTP' then
              Conf.ServerPort := '80'                                                    // standard HTTP port = 80
            else
              Conf.ServerPort := '443';                                                  // standard HTTPS port = 443
          end;
        end
        // Variable = "CGIExtensions"
        else if Variable = 'CGIExtensions' then begin
          if Value <> '' then begin
            SetLength(Extensions, 0);
            repeat
              P := Pos(';', Value);                                                      // several values separated by ";"
              if P = 0 then begin
                Ext := Value;
                Value := '';
              end
              else begin
                Ext := LeftStr(Value, P - 1);
                Delete(Value, 1, P);
              end;
              if (Length(Ext) < 2) or (LeftStr(Ext, 1) <> '.') then                      // extensions must begin with "."
                ErrMess := 'Invalid file extension value for variable "CGIExtensions": ' + Ext
              else begin
                SetLength(Extensions, Length(Extensions) + 1);
                Extensions[Length(Extensions) - 1] := Ext;
              end;
            until (Length(Value) = 0) or (ErrMess <> '');
            Conf.CGIExtensions := Extensions;
          end
          else
            Conf.CGIExtensions := CGIExt;                                                // default = ".pl;.pm" (Perl scripts and modules)
        end
        // Variable = "PHPExtensions"
        else if Variable = 'PHPExtensions' then begin
          if Value <> '' then begin
            SetLength(Extensions, 0);
            repeat
              P := Pos(';', Value);                                                      // several values separated by ";"
              if P = 0 then begin
                Ext := Value;
                Value := '';
              end
              else begin
                Ext := LeftStr(Value, P - 1);
                Delete(Value, 1, P);
              end;
              if (Length(Ext) < 2) or (LeftStr(Ext, 1) <> '.') then                      // extensions must begin with "."
                ErrMess := 'Invalid file extension value for variable "CGIExtensions": ' + Ext
              else begin
                SetLength(Extensions, Length(Extensions) + 1);
                Extensions[Length(Extensions) - 1] := Ext;
              end;
            until (Length(Value) = 0) or (ErrMess <> '');
            Conf.PHPExtensions := Extensions;
          end
          else
            Conf.PHPExtensions := PHPExt;                                                // default = ".php" (PHP scripts)
        end
        // Variable = "BrowserPath"
        else if Variable = 'BrowserPath' then begin
          if Value <> '' then
            Conf.BrowserPath := Value
          else
            ErrMess := 'Missing value for variable "BrowserPath"';                       // no default value (always error message)
        end
        // Variable = "BrowserProgram"
        else if Variable = 'BrowserProgram' then begin
          if Value <> '' then begin
            if (Length(Value) < 4) or (RightStr(Value, 4) <> '.exe') then
              Value += '.exe';                                                           // add extension ".exe" if there isn't any
            Conf.BrowserProgram := Value;
          end
          else
            ErrMess := 'Missing value for variable "BrowserProgram"';                    // no default value (always error message)
        end
        // Invalid variable has been specified
        else
          ErrMess := 'Invalid variable: ' + Variable;                                    // neither "true" nor "false"
      end
      // Invalid line: variable/value pair not recognized
      else
        ErrMess := 'Invalid file format';
    end;
  end;
  Close(InFile);
  // Check if variables without default value have actually be set
  if ErrMess = '' then begin
    if (Conf.FileCopy and (Conf.ServerPath = '?')) or (Conf.BrowserPath = '?') or (Conf.BrowserProgram = '?') then begin
      ErrMess := 'Missing value for variable ';
      if Conf.FileCopy and (Conf.ServerPath = '?') then
        ErrMess += '"ServerPath"'
      else if Conf.BrowserPath = '?' then
        ErrMess += '"BrowserPath"'
      else
        ErrMess += '"BrowserProgram"';
      L := 0;
    end;
  end;
  // If an error occured during configuration reading, display message and set error code
  if ErrMess <> '' then begin
    Writeln; Writeln;
    if L = 0 then
      Writeln('Incomplete configuration file:')
    else
      Writeln('Configuration file error at line ', L, ':');
    Writeln('  ', ErrMess, '!');
    Writeln;
    ErrCode := ConfigError;
  end;
end;

{ Extract last subdirectory from a file path }

function ExtractLastDir(Path: string): string;

var
  I: Integer;
  Dir: string;
  Found: Boolean;

begin
  Path := ExtractFileDir(Path); Dir := '';                                               // directory path (without filename)
  Found := False; I := Length(Path);
  // Search path starting from the end until a "\" found
  repeat
    if Path[I] = '\' then
      Found := True
    else begin
      Dir := Path[I] + Dir;
      Dec(I);
    end;
  until Found or (I = 0);
  ExtractLastDir := Dir;
end;

{ Get type of script (CGI or PHP) from file path (filename) }

function GetScriptType(FilePath: string; Conf: TConfig): string;

var
  I: Integer;
  ScriptType: string;

begin
  ScriptType := '';
  // Check file extension vs. CGI file extensions as defined by actual webstart.conf
  for I := 0 to Length(Conf.CGIExtensions) - 1 do begin
    if RightStr(FilePath, Length(Conf.CGIExtensions[I])) = Conf.CGIExtensions[I] then
      ScriptType := 'cgi';
  end;
    // Check file extension vs. PHP file extensions as defined by actual webstart.conf
  for I := 0 to Length(Conf.PHPExtensions) - 1 do begin
    if RightStr(FilePath, Length(Conf.PHPExtensions[I])) = Conf.PHPExtensions[I] then
      ScriptType := 'php';
  end;
  GetScriptType := ScriptType;
end;

{****************}
{* Main program *}
{****************}

begin
  ClrScr;
  Writeln('WebStart, v1.0, allu, Dec 2018'); Writeln;
  ErrCode := 0;
  if ParamCount = 1 then begin
    // Get file path from command line
    FilePath := ParamStr(1);
    // Get settings from configuration file
    Write('Reading configuration file ', ConfFile, ': ');
    ReadConfiguration(ConfFile, WebStartConfig, ErrCode);
    if ErrCode = 0 then begin
      Writeln('ok');
      // Determine the script type (CGI or PHP; this will determine which subdirectories to use in copy-path and URL)
      ScriptType := GetScriptType(FilePath, WebStartConfig);
      if ScriptType = '' then begin                                                        // file extension did not allow to determine the script type
        Writeln;
        Writeln('File error:');
        Writeln('  Can''t determine script type!');
        ErrCode := ScriptError;
      end
      // Configuration ok, script type known, thus we can go...
      else begin
        if WebStartConfig.FileCopy then begin
          // Copy file from actual location to the webserver (if variable "FileCopy" is set to "true")
          Writeln('Copying file to webserver: ');
          Source := FilePath;
          // Create destination path on webserver
          Destination := WebStartConfig.ServerPath;
          if ScriptType = 'cgi' then begin
            // CGI script
            Destination += '\' + WebStartConfig.CGIPath;
            if WebStartConfig.CGISubdirectory then begin
              SubDir := ExtractLastDir(Source);
              if SubDir <> 'cgi-bin' then                                                // cgi-bin () normaly is alraedy in destination path
                Destination += '\' + SubDir;
            end;
          end
          else begin
            // PHP script
            Destination += '\' + WebStartConfig.PHPPath;
            if WebStartConfig.PHPSubdirectory then
              Destination += '\' + ExtractLastDir(Source);
          end;
          Destination += '\' + ExtractFileName(Source);
          Writeln('  Source:      ', Source);
          Writeln('  Destination: ', Destination);
          // Copy the file using a batch file that runs the MS Windows "COPY" command
          // Syntax used: copy source-path destination-path
          ComStr := '"' + Source + '"' + ' ' + '"' + Destination + '"';
          Exec('copy.bat', ComStr);
          ErrCode := DosExitCode;
          if ErrCode = 0 then
            Writeln('  File copy successfully completed...');
        end
        // No file copy: Path passed as argument is assumed to be webserver path
        else
          Destination := FilePath;
      end;
    end;
    if ErrCode = 0 then begin
      // Create URL corresponding to file path and point specified web browser to this URL
      Writeln('Trying to run script in specified web browser:');
      WebAddress := LowerCase(WebStartConfig.ServerProtocol) + '://';                    // TCP protocol
      WebAddress += WebStartConfig.SeverName;                                            // server name
      if (WebStartConfig.ServerProtocol = 'HTTP') and (WebStartConfig.ServerPort <> '80') or
         (WebStartConfig.ServerProtocol = 'HTTPS') and (WebStartConfig.ServerPort <> '443')then
        WebAddress += ':' + WebStartConfig.ServerPort;                                   // server port
      WebAddress += '/';                                                                 // server root
      if ScriptType = 'cgi' then begin
        WebAddress += WebStartConfig.CGIPath;                                            // URL for CGI scripts
        if WebStartConfig.CGISubdirectory then begin
          SubDir := ExtractLastDir(Destination);
          if SubDir <> 'cgi-bin' then
            WebAddress += '/' + ExtractLastDir(Destination);                             // add subdirectory as in file path (if this is set)
        end;
      end
      else begin
        WebAddress += WebStartConfig.PHPPath;                                            // URL for PHP scripts
        WebAddress := StringReplace(WebAddress, 'htdocs\', '', []);                      // "htdocs" isn't part of the URL
        if WebStartConfig.PHPSubdirectory then
          WebAddress += '/' + ExtractLastDir(Destination);                               // add subdirectory as in file path (if this is set)
      end;
      WebAddress += '/' + ExtractFileName(Destination);                                  // and finally add the filename (script name)
      Writeln('  File path on server: ', Destination);
      Writeln('  Web address (URL):   ', WebAddress);
      Writeln('  Web browser path:    ', WebStartConfig.BrowserPath + '\' + WebStartConfig.BrowserProgram);
      // Start the specified browser using a batch file that runs the MS Windows "START" command
      // Syntax used: start \d webbrowser-path webbrowser-program url
      ComStr := '/d ' + WebStartConfig.BrowserPath + ' ' + WebStartConfig.BrowserProgram + ' ' + WebAddress;
      Exec('start.bat', ComStr);
      ErrCode := DosExitCode;
      if ErrCode = 0 then
        Writeln('  Script successfully opened in webbrowser...');                        // if all ok, the browsers opens with the wanted webpage displayed
    end;
  end
  else begin
    if ParamCount = 0 then begin
      Writeln('Command line error: No file has been specified!');
      ErrCode := ParamError1;
    end
    else begin
      Writeln('Command line error: Invalid number of parameters!');
      ErrCode := ParamError2;
    end;
  end;
  Writeln;
  if ErrCode <> 0 then
    Halt(ErrCode);
end.

