{*******************************************}
{*            Directory listing            *}
{* (with export to .txt, .csv, .html file) *}
{*******************************************}

program DirList;

// Because of reference to Microsoft Windows specific file attributes,
// this application may not be build as such on Linux or Mac systems!

// Change log:
// Version 1.0 (January 2021): Original program
// Version 1.1 (May 2021):
//   - Bug fix: because of use of Length instead of UTF8Length, export to textfile
//     resulted in improper column alignment if line contains non-ANSI characters

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  dir, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfDirList, fDirList);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

