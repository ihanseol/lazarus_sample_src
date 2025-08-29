{************************************************}
{* Create data file for Luxembourg2 application *}
{* Input files: luxbg2a.txt and luxbgb.txt      *}
{* Output file: luxbg2.dat                      *}
{************************************************}

program luxbg2init;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, LazUtils, LazUTF8, Crt;

type
  TTownship = record
    Township, Canton: string[25];
    Ward: Char;
  end;
  TTownships = array of TTownship;
  TLocal = record
    Local, LocalFr, LocalDe, Township, Canton: string[25];
    Ward: Char;
  end;
  TLocals = array of TLocal;
  TLocalsFile = file of TLocal;

var
  Townships: TTownships;
  Locals: TLocals;

{ Read townships from file luxbg2a.txt }

procedure ReadTownships(out Townships: TTownships);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  Assign(InFile, 'luxbg2a.txt'); Reset(InFile); N := 0;
  while not EOF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Inc(N); SetLength(Townships, N);
      Townships[N - 1].Township := UTF8Copy(Line, 1, 25);
      Townships[N - 1].Canton := UTF8Copy(Line, 26, 25);
      Townships[N - 1].Ward := UTF8Copy(Line, 51, 1)[1];
    end;
  end;
  Close(InFile);
end;

{ Read locals from file luxbg2b.txt }

procedure ReadLocals(var Townships: TTownships; out Locals: TLocals);

var
  N, I, IX: Integer;
  Line: string;
  Township: string[25];
  InFile: Text;

begin
  Assign(InFile, 'luxbg2b.txt'); Reset(InFile); N := 0;
  while not EOF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      // Check if the township given here exists in the luxbg2a.txt file
      Township := UTF8Copy(Line, 26, 25);
      I := 0; IX := -1;
      while (I < Length(Townships)) and (IX = -1) do begin
        if Township = Townships[I].Township then
          IX := I;
        Inc(I);
      end;
      // If all ok, add the local to the list
      if IX <> -1 then begin
        Inc(N); SetLength(Locals, N);
        Locals[N - 1].Local := UTF8Copy(Line, 1, 25);
        Locals[N - 1].LocalFr := UTF8Copy(Line, 51, 25);
        Locals[N - 1].LocalDe := UTF8Copy(Line, 76, 25);
        Locals[N - 1].Township := Townships[IX].Township;
        Locals[N - 1].Canton := Townships[IX].Canton;
        Locals[N - 1].Ward := Townships[IX].Ward;
      end
      // If township is unknown, display error message
      else begin
        Writeln('Local = ', UTF8Trim(UTF8Copy(Line, 1, 25)), ', unknown township = ', UTF8Trim(UTF8Copy(Line, 26, 25)));
      end;
    end;
  end;
  Close(InFile);
end;

{ Write locals to luxbg2.dat file }

procedure WriteDataFile(var Locals: TLocals);

var
  I: Integer;
  OutFile: TLocalsFile;

begin
  Assign(OutFile, 'luxbg2.dat'); Rewrite(OutFile);
  for I := 0 to Length(Locals) - 1 do
    Write(OutFile, Locals[I]);
  Close(OutFile);
  Writeln; Writeln('Number of locals written = ', Length(Locals));
end;

{* ------------ *}
{* Main program *}
{* ------------ *}

begin
  ClrScr;
  Writeln('Luxembourg2 data file initialisation.');
  Writeln('====================================='); Writeln;
  ReadTownships(Townships);
  ReadLocals(Townships, Locals);
  WriteDataFile(Locals);
  Writeln; Writeln;
  Write('Hit ENTER to terminate the program '); Readln;
end.

