{**********************************************}
{* Electronics: 7-Segment Displays Simulation *}
{**********************************************}

program Display7seg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  Forms,
  d7seg, d7sega, d7segc, d7segbcdc, d7segbcda, d7segcount, common, help;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfD7Seg, fD7Seg);
  Application.CreateForm(Tfd7Sega, fd7Sega);
  Application.CreateForm(TfD7Segc, fD7Segc);
  Application.CreateForm(TfD7SegBCDc, fD7SegBCDc);
  Application.CreateForm(TfD7SegBCDa, fD7SegBCDa);
  Application.CreateForm(TfD7SegCounter, fD7SegCounter);
  Application.CreateForm(TfHelp, fHelp);
  Application.Run;
end.

