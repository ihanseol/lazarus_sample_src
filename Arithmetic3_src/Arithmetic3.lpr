//
// Arithmetic3: "Die Kinder zählen die Autos" (GUI application version)
//

program Arithmetic3;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  arithmetics3_main,
  arithmetics3_common,
  arithmetics3_userdata, arithmetics3_help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormArithmetic3, FormArithmetic3);
  Application.CreateForm(TFormUserData, FormUserData);
  Application.CreateForm(TFormHelp, FormHelp);
  Application.Run;
end.

