{***********************}
{* Logic game: Kakkuru *}
{***********************}

program Kakkuru;

// Change log:
//   - Version 1.0 (August 2020): Original program
//   - Version 1.1 (September 2020): Bug fix (application aborts with "Clear all" because of index out of range)
//   - Version 1.2 (June 2021):
//       - Removing not used buttons
//       - Using black squares for sum fields (as is said in game description)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms, kakkuru_u1;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TfKakkuru, fKakkuru);
  Application.Run;
end.

