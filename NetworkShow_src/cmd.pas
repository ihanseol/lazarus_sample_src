{***************************************************}
{* Console window unit for NetworkShow application *}
{***************************************************}

unit cmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Dos;

type
  {*******}
  { TfCmd }
  {*******}
  TfCmd = class(TForm)
    memoCmd: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
  end;

var
  fCmd: TfCmd;

implementation

{$R *.lfm}

{*******}
{ TfCmd }
{*******}

{ Button "Close": Close console window }

procedure TfCmd.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

