{ ***************************************************************************
  *                               Maths Trainer                             *
  * ----------------------------------------------------------------------- *
  * Equation trainer : Linear equations in 1 variable                       *
  *************************************************************************** }


program Equations1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,                               // this includes the LCL widgetset
  Forms,
  equations1_u1;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.

