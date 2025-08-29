{*************************************************************}
{* Copy of Windows 10 lock screen pictures to user directory *}
{*************************************************************}

program lspics;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, LazUTF8, Dos;

const
  LSPicsDir0 = 'C:\Users\UserName\AppData\Local\Packages\Microsoft.Windows.ContentDeliveryManager_cw5n1h2txyewy\LocalState\Assets';
  SaveDir0   = 'UserPics\Lockscreen';

var
  Seq, N, PX, P, Code, I: Integer;
  User, UserDir, UserDocsDir, UserPicsDir, LSPicsDir, SaveDir, FilenameBase, FilenameSSeq: string;
  Action, Param, SSeq, Source, Destination, Comstr, Mess: string;
  IsError: Boolean;
  LSPicFiles: array of string;
  FileInfo: SysUtils.TSearchRec;

{ Display application help (from file help.txt) }

procedure DisplayHelp;

var
  Line: string;
  HelpFile: Text;

begin
  Writeln;
  Assign(HelpFile, 'help.txt'); Reset(HelpFile);
  while not EoF(HelpFile) do begin
    Readln(HelpFile, Line);
    Writeln(Line);
  end;
  Close(HelpFile);
  Writeln;
end;

{**************}
{ Main program }
{**************}

begin
  Writeln; Writeln('lspics, v1.0, (c) allu, March-April 2021');
  User := GetEnv('USERNAME'); UserDir := GetEnv('USERPROFILE');
  UserDocsDir := UserDir + '\Documents'; UserPicsDir := UserDir + '\Pictures';
  LSPicsDir := StringReplace(LSPicsDir0, 'UserName', User, []);
  SaveDir := StringReplace(SaveDir0, 'UserPics', UserPicsDir, []);
  FilenameBase := 'pic'; FilenameSSeq := '001';
  Action := 'write';
  if ParamCount > 0 then begin
    // If there are command line parameters, use these to replace the default values
    Param := UTF8Trim(ParamStr(1)); PX := 1;
    // If the first parameter is -h, display the program help...
    if Param = '-h' then begin
      DisplayHelp; Action := 'help';
    end
    // ...otherwise, check for the save-directory and the program options
    else begin
      // If the first parameter does not start with "-", it has to be the save-directory
      if LeftStr(Param, 1) <> '-' then begin
        SaveDir := Param;
        SaveDir := StringReplace(SaveDir, 'UserDocs', UserDocsDir, []);
        SaveDir := StringReplace(SaveDir, 'UserPics', UserPicsDir, []);
        Inc(PX);
      end;
      Mess := '';
      // Check for program options
      for I := PX to ParamCount do begin
        if Mess = '' then begin
          Param := ParamStr(I);
          // Option -r: File-rename option
          if LeftStr(Param, 3) = '-r:' then begin
            IsError := False;
            Delete(Param, 1, 3);
            // Extract the filename base and filename sequence start (at given format)
            if Param = '' then
              IsError := True
            else begin
              P := UTF8Pos(',', Param);
              if (P = 1) or (P = UTF8Length(Param)) then
                IsError := True
              else if P = 0 then begin
                FilenameBase := Param;
              end
              else begin
                FileNameBase  := UTF8Copy(Param, 1, P - 1);
                FilenameSSeq := UTF8Copy(Param, P + 1, UTF8Length(Param));
                Val(FilenameSSeq, Seq, Code);
                if Code <> 0 then
                  IsError := True;
              end;
            end;
            if IsError then
              Mess := 'Invalid file rename parameter';
          end
          // Option -o: File override
          else if Param = '-o' then
            Action := 'override'
          // Anything else given as parameter is an error
          else
            Mess := 'Invalid command line parameters';
        end;
      end;
    end;
  end;
  // Check pictures source and destination directory
  if (Action <> 'help') and (Mess = '') then begin
    // Check lockscreen pictures directory
    if not DirectoryExists(LSPicsDir) then
      // This might not happen (at least not on Windows 10)
      Mess := 'Cannot find lockscreen pictures directory'
    else begin
      // Check pictures output directory
      if SaveDir = StringReplace(SaveDir0, 'UserPics', UserPicsDir, []) then begin
        // If save-directory = default output directory and the folder Lockscreen
        // does not exist in the user's Pictures library, create the directory
        if not DirectoryExists(SaveDir) then begin
          if not CreateDir(SaveDir) then
            Mess := 'Cannot create output directory';                          // this might happen, if privacy software blocks app access to the user libs
        end;
      end
      else if not DirectoryExists(SaveDir) then
        // For any other path given, the directory has to exist
        Mess := 'Cannot find output directory';
    end;
  end;
  if (Action <> 'help') and (Mess = '') then begin
    if RightStr(SaveDir, 1) = '\' then
      Delete(SaveDir, Length(SaveDir), 1);
    // File search (all files) for the directory specified (= Lockscreen pictures dir)
    if SysUtils.FindFirst(LSPicsDir + '\*', faAnyFile, FileInfo) = 0 then begin
      // Get first object in directory and continue until all objects have been done
      N := 0;
      repeat
        with FileInfo do begin
          if (Attr and faDirectory) = 0 then begin
            // Consider only files (ignore any subdirs)
            Inc(N);
            SetLength(LSPicFiles, N);
            LSPicFiles[N - 1] := Name;                                         // store all filenames into an array
          end;
        end;
      until SysUtils.FindNext(FileInfo) <> 0;
    end
    else
      // This should normally not happen
      Mess := 'Cannot read lockscreen picture directory';
  end;
  if (Action <> 'help') and (Mess = '') then begin
    Seq := StrToInt(FilenameSSeq) - 1; N := 0;
    // Copy all files of the Lockscreen pictures dir to the save-directory,
    // renaming them according to the template given by the -r option and
    // adding the .jpg extension
    // Copy/rename is done using the Windows-build-in command COPY. The simplest
    // way to do this, is creating a batch file containing the COPY command with
    // the %1 and %2 parameters for source and destination. The batch file may be
    // run by the FPC Exec command; the full path source and destination filenames
    // being passed to the batch file
    for I := 0 to Length(LSPicFiles) - 1 do begin
      if Mess = '' then begin
        Source := LSPicsDir + '\' + LSPicFiles[I];
        Inc(Seq);                                                              // file numbering, starting with the value given in the -r option
        SSeq := IntToStr(Seq);
        while Length(SSeq) < Length(FilenameSSeq) do
          SSeq := '0' + SSeq;                                                  // numbering format (as many digits as given in the -r option)
        Destination := SaveDir + '\' + FilenameBase + SSeq + '.jpg';
        if  (not FileExists(Destination)) or (FileExists(Destination) and (Action = 'override')) then begin
          // Do the copying only if the filename does not exist, or if file override has been specified by the -o option
          ComStr := '"' + Source + '"' + ' ' + '"' + Destination + '"';
          Exec('copy.bat', ComStr); Inc(N);
          if DosExitCode <> 0 then                                             // batch files set an exit code, that may be tested with the variable DosExitCode (of the Dos unit)
            Mess := 'Cannot copy lockscreen picture files';
        end;
      end;
    end;
  end;
  if (Action <> 'help') and (Mess = '') then begin
    // Check if all files have been copied (otherwise display an error message)
    if N < Length(LSPicFiles) then begin
      if N = 0 then
        Mess := 'Files '
      else
        Mess := 'Some files ';
      Mess += 'not copied, because filename already exists'
    end;
  end;
  if Mess <> '' then begin
    // Display error message (if there has been any)
    Writeln('Program error: ', Mess);
    Writeln('Use lspics -h for help...');
  end;
  // If the program has been run without command line options and there has been
  // no error, let the Command Prompt window automatically close. Thus, the user
  // has not to do it manually, if he run the program by double-clicking it.
  if (ParamCount = 0) and (Mess <> '') then begin
    Writeln; Write('Hit ENTER to terminate the program... '); Readln();
  end;
end.

