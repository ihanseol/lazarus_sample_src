{***********************************}
{* Memory Game - Christmas Edition *}
{***********************************}

program Memory2;

// The application is based on the standard Memory game version 3.2
// The major change is that the pictures are 120x120 (instead of 120x80) pixels,
// this because this kind of pictures are usually portrait format.

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  memory2_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMemory2, fMemory2);
  Application.Run;
end.

