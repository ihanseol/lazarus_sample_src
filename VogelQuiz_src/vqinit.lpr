{**********************************************}
{* Create data file for VogelQuiz application *}
{**********************************************}

// Change log:
// Version 1.0 (June 2018): original version
// Version 2.0 (December 2019):
//   - bird text file extended, now also including the English bird names
//   - creation of 2 data files, one with the German, the other with the English bird names

program vqinit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Crt;

type
  TVQRecord = record
    Bird: string[30];
    BirdShort: string[20];
    QuizLevels: string[5];
    FilenameModif: string[1];
  end;

var
  L1, L2, L3, L4, LX, P: Integer;
  Line, Exotic: string;
  VQRecord, VQRecord2: TVQRecord;
  Infile: Text;
  Outfile1, Outfile2: file of TVQRecord;

begin
  ClrScr;
  Writeln('Create data file for VogelQuiz application.');
  Writeln('===========================================');
  Writeln;
  L1 := 0; L2 := 0; L3 := 0; L4 := 0; LX := 0;
  Assign(Infile, 'vogelquiz.txt');  Reset(Infile);
  Assign(OutFile1, 'vogelquiz.dat'); Rewrite(Outfile1);
  Assign(OutFile2, 'birdquiz.dat'); Rewrite(Outfile2);
  while not EoF(Infile) do begin
    Readln(Infile, Line);
    if Line <> '' then begin
      with VQRecord do begin
        Bird := Copy(Line, 1, 30);
        BirdShort := Copy(Line, 35, 20);
        if Copy(Line, 34, 1) = 'X' then
          Exotic := 'X'
        else
          Exotic := '-';
        QuizLevels  := Copy(Line, 31, 3) + '4' + Exotic;
        P := Pos('1', QuizLevels);
        if P > 0 then
          Inc(L1);
        P := Pos('2', QuizLevels);
        if P > 0 then
          Inc(L2);
        P := Pos('3', QuizLevels);
        if P > 0 then
          Inc(L3);
        P := Pos('4', QuizLevels);
        if P > 0 then
          Inc(L4);
        P := Pos('X', QuizLevels);
        if P > 0 then
          Inc(LX);
        FilenameModif := Copy(Line, 28, 1);
      end;
      with VQRecord2 do begin
        Bird := Copy(Line, 61, 30);
        BirdShort := Copy(Line, 91, 20);
        QuizLevels  := VQRecord.QuizLevels;
        FilenameModif := ' ';
      end;
      Write(Outfile1, VQRecord);
      Write(Outfile2, VQRecord2);
    end;
  end;
  Close(Infile); Close(Outfile1); Close(Outfile2);
  Writeln('Birds per quiz level: ', L1, ' - ', L2, ' - ', L3, ' - ', L4, ' - ', LX, '.');
  Writeln; Writeln;
  Write('Hit ENTER to terminate program... '); Readln;
end.

