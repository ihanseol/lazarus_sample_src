{********************************************}
{* Data entry unit for Priorite application *}
{********************************************}

unit priorite_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfData }
  TfData = class(TForm)
    btOK: TButton;
    edNQuestions: TEdit;
    Label1: TLabel;
    procedure btOKClick(Sender: TObject);
  end;

var
  fData: TfData;

implementation

{$R *.lfm}

{**********}
{* TfData *}
{**********}

{ Button "OK": Close the form }

procedure TfData.btOKClick(Sender: TObject);

var
  NQ: Integer;

begin
  if edNQuestions.Text = '' then
    edNQuestions.Text := '0';
  NQ := StrToInt(edNQuestions.Text);
  if NQ < 5 then                                                                                   // Minimum of questions arbitrarily set to 5
    MessageDlg('Entrée invalide', 'Au moins 5 questions, s''il-vous-plaît...', mtError, [mbOK], 0)
  else
    Close;
end;

end.

