{*************************************************}
{* Create data file for RaubtierQuiz application *}
{*************************************************}

program htqinit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils,
  LazUTF8,
  Crt;

type
  TAnimal = record
    Name: string[30];
    ShortName: string[30];
    QuizLevels: string[4];
  end;

var
  L1, L2, L3, L4, P: Integer;
  Line: string;
  Animal: TAnimal;
  Infile: Text;
  Outfile: file of TAnimal;

begin
  ClrScr;
  Writeln('Create data file for HuftierQuiz application.');
  Writeln('=============================================');
  Writeln;
  L1 := 0; L2 := 0; L3 := 0; L4 := 0;
  Assign(Infile, 'huftiere.txt');  Reset(Infile);
  Assign(OutFile, 'huftiere.dat'); Rewrite(Outfile);
  while not EoF(Infile) do begin
    Readln(Infile, Line);
    with Animal do begin
      Name := UTF8Copy(Line, 1, 30);
      ShortName := UTF8Copy(Line, 35, 30);
      QuizLevels := UTF8Copy(Line, 31, 3) + '4';
      P := UTF8Pos('1', QuizLevels);
      if P > 0 then
        Inc(L1);
      P := UTF8Pos('2', QuizLevels);
      if P > 0 then
        Inc(L2);
      P := UTF8Pos('3', QuizLevels);
      if P > 0 then
        Inc(L3);
      P := UTF8Pos('4', QuizLevels);
      if P > 0 then
        Inc(L4);
    end;
    Write(Outfile, Animal);
  end;
  Close(Infile); Close(Outfile);
  Writeln('Animals per quiz level: ', L1, ' - ', L2, ' - ', L3, ' - ', L4, '.');
  Writeln; Writeln;
  Write('Hit ENTER to terminate program... '); Readln;
end.

