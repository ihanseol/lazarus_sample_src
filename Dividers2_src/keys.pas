{********************************************************}
{* Player keys selection unit for Dividers2 application *}
{********************************************************}

unit keys;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfKeys }
  {********}
  TfKeys = class(TForm)
    StaticText1: TStaticText;
    laPlayer1: TLabel;
    edKey1: TEdit;
    laPlayer2: TLabel;
    edKey2: TEdit;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    cOldKey1, cOldKey2: Char;
  public
    sButton: string;
  end;

var
  fKeys: TfKeys;

implementation

{$R *.lfm}

{********}
{ TfKeys }
{********}

{ Form activation: Save current keys }

procedure TfKeys.FormActivate(Sender: TObject);

begin
  cOldKey1 := Uppercase(edKey1.Text)[1]; cOldKey2 := Uppercase(edKey2.Text)[1];
end;

{ Button "OK": Check keys and, if ok, close the window }

procedure TfKeys.btOKClick(Sender: TObject);

var
  Key1, Key2: Char;

begin
  Key1 := Uppercase(edKey1.Text)[1]; Key2 := Uppercase(edKey2.Text)[1];
  if (not (Key1 in ['A'..'Z', '0'..'9'])) or (not (Key2 in ['A'..'Z', '0'..'9'])) then
     MessageDlg('Invalid settings', 'Player keys must be letters or numbers!', mtError, [mbOK], 0)
  else if Key1 = Key2 then
     MessageDlg('Invalid settings', 'Player keys must be different from each other!', mtError, [mbOK], 0)
  else begin
    sButton := 'ok';                                                           // indicates to the main form, that keys have been modified
    Close;
  end;
end;

{ Button "Cancel": Restore previous keys and close the window }

procedure TfKeys.btCancelClick(Sender: TObject);

begin
  edKey1.Text := cOldKey1; edKey2.Text := cOldKey2;
  sButton := 'cancel';                                                         // indicates to the main form, that action has been canceled
  Close;
end;

end.

