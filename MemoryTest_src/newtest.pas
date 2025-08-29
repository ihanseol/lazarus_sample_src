{******************************}
{* MemoryTest data entry unit *}
{******************************}

unit newtest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfNew }
  TfNew = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edObjects: TEdit;
    rbDisplay1: TRadioButton;
    rbDisplayAll: TRadioButton;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    sObjects: string;
    bDisplay1, bDisplayAll: Boolean;
  end;

var
  fNew: TfNew;

implementation

{$R *.lfm}

{*********}
{* TfNew *}
{*********}

{ Form activation: Save current settings }

procedure TfNew.FormActivate(Sender: TObject);

begin
  sObjects    := edObjects.Text;
  bDisplay1   := rbDisplay1.Checked;
  bDisplayAll := rbDisplayAll.Checked;
end;

{ Button "Close": If valid data, close the form }

procedure TfNew.btOKClick(Sender: TObject);

begin
  if edObjects.Text = '' then
    edObjects.Text := '5';
  if (StrToInt(edObjects.Text) < 3) or (StrToInt(edObjects.Text) > 50) then begin
    MessageDlg('Memory test', 'Number of objects must be between 3 and 50!', mtError, [mbOK], 0);
    edObjects.SetFocus;
  end
  else
    Close;
end;

{ Button "Cancel": Restore old data and close the form }

procedure TfNew.btCancelClick(Sender: TObject);

begin
  edObjects.Text       := sObjects;
  rbDisplay1.Checked   := bDisplay1;
  rbDisplayAll.Checked := bDisplayAll;
  Close;
end;

end.

