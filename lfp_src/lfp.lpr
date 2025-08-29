{*********************************}
{* Change Linux file permissions *}
{*********************************}

program lfp;

uses
  SysUtils, Crt, Process, LazUTF8;

type
  TArray = array of string;
  TField = record
    Y, X: Integer;
  end;

const
  aGroups: array[0..2] of Char = ('u', 'g', 'o');
  aModes: array[0..2] of Char = ('r', 'w', 'x');
  KEY_Done   = #13;  KEY_Move   = #9;
  KEY_Grant  = '+';  KEY_Revoke = '-';  KEY_Keep   = '=';
  aFields: array[0..8] of TField = (
    (Y:  8; X: 14), (Y:  8; X: 21), (Y:  8; X: 30),
    (Y:  9; X: 14), (Y:  9; X: 21), (Y:  9; X: 30),
    (Y: 10; X: 14), (Y: 10; X: 21), (Y: 10; X: 30)
  );

var
  iField, iI, iP, iY: Integer;
  sOption, sPath, sCommand, sExec, sPermissions, sOutput, sMess: string;
  cKey: Char;
  bReturn: Boolean;
  aPermissions: array[0..8] of Char;
  aParams: TArray;

{ Display program title }

procedure DisplayTitle;

begin
  ClrScr;
  Writeln('Set Linux file permissions.');
  Writeln('lfp, version 1.0, allu May 2024');
  Writeln;
end;

{ Get command and parameters for "RunCommand" function }

procedure GetParams(Option, CommandString: string; out Exec: string; out Parameters: TArray);

var
  IX, P: Integer;

begin
  if Option = 'S' then begin
    // Linux command as superuser: Executable = "sudo"; "chmod" will be first parameter
    Exec := 'sudo';
    SetLength(Parameters, 3);
    Parameters[0] := 'chmod';
    IX := 1;                                                                   // next parameter index (2nd array element)
  end
  else begin
    // Linux command as standard user: Executable = "chmod"
    Exec := 'chmod';
    SetLength(Parameters, 2);
    IX := 0;                                                                   // next parameter index (1st array element)
  end;
  UTF8Delete(CommandString, 1, 6);
  if LeftStr(CommandString, 2) = '-R' then begin
    // Recursive directory change: Add "-R" as next parameter
    SetLength(Parameters, Length(Parameters) + 1);
    Parameters[IX] := '-R';
    Inc(IX);                                                                   // next parameter index (3rd or second array element)
    UTF8Delete(CommandString, 1, 3);
  end;
  // Add following parameters (in the command string they are separated by a space)
  while CommandString <> '' do begin
    P := UTF8Pos(' ', CommandString);
    if P > 0 then begin
      Parameters[IX] := LeftStr(CommandString, P - 1);
      UTF8Delete(CommandString, 1, P);
    end
    else begin
      Parameters[IX] := CommandString;
      CommandString := '';
    end;
    Inc(IX);                                                                   // next parameter index
  end;
end;

{**************}
{ Main program }
{**************}

// This program runs on Unix like operating systems only!

begin
  {$IFNDEF UNIX}
    Writeln('Error: This program only runs on a UNIX based operating system!');
    Write('Hit ENTER to terminate '); Readln;
    Halt;
  {$ENDIF}
  // Loop until user chooses to terminate the program
  repeat
    DisplayTitle;
    // Choose what file permissions to change
    Writeln('Program options:');
    Writeln('  1 = Change permissions of a file');
    Writeln('  2 = Change permissions of a directory');
    Writeln('  3 = Change permissions of a directory and all its content');
    Writeln('  9 = Exit program');
    repeat
      GotoXY(1, 9); ClrEoL;
      Write('Your choice? '); Readln(sOption);
      if Length(sOption) <> 1 then
        sOption := '0';
    until sOption[1] in ['1', '2', '3', '9'];
    DisplayTitle;
    // Ask user for file or directory name
    if sOption <> '9' then begin
      sMess := ''; iY := 6;
      // Ask for filename
      if sOption = '1' then begin
        Write('Enter file path? '); Readln(sPath);
        // If filename doesn't contain wildcards ("*" or "?"), check if file exists
        iP := UTF8Pos('*', sPath);
        if iP = 0 then
          iP := UTF8Pos('?', sPath);
        if iP <> 0 then
          sMess := 'WSorry, this version of lfp does not support wildcards...'
        else begin
          if not FileExists(sPath) then
            sMess := 'File does not exist!'
          else begin
            Writeln; Writeln('File permissions:');
          end;
        end;
      end
      // Ask for directory name
      else begin
        Write('Enter directory path? '); Readln(sPath);
        // Check if directory exists
        if not DirectoryExists(sPath) then
          sMess := 'Directory does not exist!'
        else begin
          Writeln;
          if sOption = '2' then
            Writeln('Directory permissions:')
          else
            Writeln('Recursive directory permissions:')
        end;
      end;
      if sMess = '' then begin
        for iI := 0 to 8 do
          aPermissions[iI] := '=';
        // Display the "permissions grid"
        GotoxY(12, 7); Writeln('Read   Write   Execute');
        for iI := 1 to 3 do begin
          case iI of
            1: Write('  User');
            2: Write('  Group');
            3: Write('  Other');
          end;
          GotoXY(14, 7 + iI);
          Writeln(Key_Keep, '      ', Key_Keep, '        ', Key_Keep);
        end;
        GotoxY(1, 12); Writeln('Use TAB key to move around. Use + to grant, - to revoke a permission.');
        Writeln('Use = to keep a permission as it was. Hit ENTER when done.');
        GotoXY(14, 8);
        iField := 0;                                                           // index for permissions array
        // Loop until the "Done" key has been pressed
        repeat
          cKey := #255;
          // Loop until a valid key has been pressed
          repeat
            cKey := ReadKey;
            if cKey = #0 then
              cKey := ReadKey;                                                 // this is for keys with two codes (#0 + code)
            if cKey in [KEY_Grant, KEY_Revoke, Key_Keep] then begin
              // If key is a "set permissions" key, update the permissions array
              aPermissions[iField] := cKey;
              Write(cKey);                                                     // display new permissions setting in the grid
              cKey := KEY_Move;                                                // automatically move to next grid field
            end;
            if cKey = KEY_Move then begin
              // If key is left arrow, move to next grid position
              Inc(iField);
              if iField = 9 then
                iField := 0;
              GotoXY(aFields[iField].X, aFields[iField].Y);
            end;
          until cKey in [KEY_Move, KEY_Done];
        until cKey = KEY_Done;
        // Build the Linux command string (chmod [-R] permissions path)
        sCommand := 'chmod ';
        if sOption = '3' then
          sCommand += '-R ';
        sPermissions := '';
        for iI := 0 to 8 do begin
          if iI mod 3 = 0 then begin
            // New "user group" (u, g, o)
            if iI <> 0 then begin
              // "User groups" separated by comma
              sPermissions += ',';
            end;
            sPermissions += aGroups[iI div 3];
          end;
          if aPermissions[iI] <> '=' then begin
            // If this permission has to be changed, do so (r+, r-, w+, w-, x+, x-)
            sPermissions += aPermissions[iI] + aModes[iI mod 3];
          end;
        end;
        // Apply corrections to the "permissions string"
        sPermissions := StringReplace(sPermissions, 'u,g', 'g', []);           // this occurs if no user permission is changed
        sPermissions := StringReplace(sPermissions, 'u,o', 'o', []);           // this occurs if no user and no group permission is changed
        sPermissions := StringReplace(sPermissions, 'g,o', 'o', []);           // this occurs if no group permission is changed
        if RightStr(sPermissions, 2) = ',o' then                               // this occurs if no other permission is changed
          Delete(sPermissions, Length(sPermissions) - 1, 2);
        if Length(sPermissions) < 3 then begin
          // If there are no permissions to be changed, don't run the command
          sMess := 'WNo changes made. Keeping actual permissions.';
          iY := 15;
        end
        else begin
          // Change file permissions, running the Linux "chmod" command
          sCommand += sPermissions + ' ' + sPath;
          GotoXY(1, 15);
          Write('Command = ');
          TextColor(Yellow); Writeln(sCommand); TextColor(LightGray);
          repeat
            // Ask user if command has to be run as superuser (also give the possibility to cancel the operation)
            GotoXY(1, 17); ClrEoL;
            Write('Options: R = run; S = run with sudo; C = cancel. Your choice? ');
            Readln(sOption); sOption := UpperCase(sOption);
          until (Length(sOption) = 1) and (sOption[1] in ['R', 'S', 'C']);
          if sOption = 'C' then begin
            // Operation canceled by user
            sMess := 'WOperation canceled by user!';
            iY := 19;
          end
          else begin
            // Change file permissons (run "chmod")
            GetParams(sOption, sCommand, sExec, aParams);
            bReturn := RunCommand(sExec, aParams, sOutput);
            if not bReturn then begin
              // The command should return "True" if it succeeded
              sMess := 'Changing file permissions failed!';
            end;
            iY := 19;
          end;
        end;
      end;
      GotoXY(1, iY);
      if sMess = '' then begin
        // Successful change of file permissions
        Writeln('File permissions have been changed');
      end
      else begin
        // Some error occured (or command was not run for other reasons)
        if LeftStr(sMess, 1) = 'W' then
          Delete(sMess, 1, 1)
        else
          sMess := 'Error: ' + sMess;
        Writeln(sMess);
      end;
      Write('Hit ENTER to continue '); Readln;
    end;
  until sOption = '9';
end.
