{**************************************}
{* Beginning Chess: Moving the pieces *}
{**************************************}

// Notes:
// The application runs well and correctly, but the code is far from being professionnal!
// Some chess features (mouvement of the pieces, detection of check after piece mouvement) are effectively coded,
// whereas others (invalid king move because of check, checkmate after piece mouvement, invalid move because of
// setting king into check) are read from string variables, defined together with a given game position.

program ChessMoves;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  chess, position, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfChess, fChess);
  Application.CreateForm(TfHelp, fHelp);
  Application.CreateForm(TfPosition, fPosition);
  Application.Run;
end.

