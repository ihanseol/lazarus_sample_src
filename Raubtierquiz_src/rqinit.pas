{*************************************************}
{* Create data file for RaubtierQuiz application *}
{*************************************************}

program rqinit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  Crt;

type
  TAnimal = record
    Name: string[30];
    ShortName: string[30];
    QuizLevels: string[4];
    FilenameModif: string[1];
  end;

var
  L1, L2, L3, L4, P: Integer;
  Line: string;
  Animal: TAnimal;
  Infile: Text;
  Outfile: file of TAnimal;

begin
  ClrScr;
  Writeln('Create data file for RaubtierQuiz application.');
  Writeln('==============================================');
  Writeln;
  L1 := 0; L2 := 0; L3 := 0; L4 := 0;
  Assign(Infile, 'raubtierquiz.txt');  Reset(Infile);
  Assign(OutFile, 'raubtierquiz.dat'); Rewrite(Outfile);
  while not EoF(Infile) do begin
    Readln(Infile, Line);
    with Animal do begin
      Name := Copy(Line, 1, 30);
      ShortName := Copy(Line, 35, 30);
      QuizLevels := Copy(Line, 31, 3) + '4';
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
      FilenameModif := RightStr(Line, 1);
    end;
    Write(Outfile, Animal);
  end;
  Close(Infile); Close(Outfile);
  Writeln('Animals per quiz level: ', L1, ' - ', L2, ' - ', L3, ' - ', L4, '.');
  Writeln; Writeln;
  Write('Hit ENTER to terminate program... '); Readln;
end.

