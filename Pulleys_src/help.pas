{******************************************}
{* Help text unit for Pulleys application *}
{******************************************}

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfHelp }
  {********}
  TfHelp = class(TForm)
    edHelp: TMemo;
    btClose: TButton;
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

const
  SUP_2 = #$C2#$B2;

var
  fHelp: TfHelp;

implementation

{$R *.lfm}

{********}
{ TfHelp }
{********}

{ Application start: Apply superscripts }

procedure TfHelp.FormCreate(Sender: TObject);

begin
  edHelp.Text := StringReplace(edHelp.Text, 'm/s2', 'm/s' + SUP_2, [rfReplaceAll]);
end;

{ Button "Close": Close the window }

procedure TfHelp.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

