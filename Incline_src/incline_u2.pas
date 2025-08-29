{*********************************************************}
{* Pause options selections unit for Incline application *}
{*********************************************************}

unit incline_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {*************}
  {* TfPSelect *}
  {*************}
  TfPSelect = class(TForm)
    Label1: TLabel;
    rbSel1, rbSel2, rbSel3, rbSel4, rbSel5: TRadioButton;
    laPauseValue: TLabel;
    edPauseValue: TEdit;
    cbAutoPause: TCheckBox;
    btOK: TButton;
    btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure rbSel1Change(Sender: TObject);
    procedure rbSel2Change(Sender: TObject);
    procedure rbSel3Change(Sender: TObject);
    procedure rbSel4Change(Sender: TObject);
    procedure rbSel5Change(Sender: TObject);
  private
    iSelectOld: Integer;
    sPauseValueOld, sPauseValueLabelOld: string;
    bAutoPauseOld: Boolean;
  public
    iSelect: Integer;
    sButton: string;
  end;

var
  fPSelect: TfPSelect;

implementation

{$R *.lfm}

{*************}
{* TfPSelect *}
{*************}

{ Application start: Initialisation }

procedure TfPSelect.FormCreate(Sender: TObject);

begin
  iSelect := 2;
end;

{ Form activation (each time the form is opened): Save actual form values (in case user will push "Cancel") }

procedure TfPSelect.FormActivate(Sender: TObject);

begin
  if rbSel1.Checked then
    iSelectOld := 1
  else if rbSel2.Checked then
    iSelectOld := 2
  else if rbSel3.Checked then
    iSelectOld := 3
  else if rbSel4.Checked then
    iSelectOld := 4
  else if rbSel5.Checked then
    iSelectOld := 5;
  sPauseValueOld := edPauseValue.Text;
  sPauseValueLabelOld := laPauseValue.Caption;
  bAutoPauseOld := cbAutoPause.Checked;
end;

{ Button "OK": Close form (with user selected values active) }

procedure TfPSelect.btOKClick(Sender: TObject);

begin
  sButton := 'ok';                                                             // will tell the main form that user pushed "OK"
  Close;
end;

{ Button "Cancel": Close form (restore previous form values) }

procedure TfPSelect.btCancelClick(Sender: TObject);

begin
  case iSelectOld of
    1: rbSel1.Checked := True;
    2: rbSel2.Checked := True;
    3: rbSel3.Checked := True;
    4: rbSel4.Checked := True;
    5: rbSel5.Checked := True;
  end;
  edPauseValue.Text := sPauseValueOld;
  laPauseValue.Caption := sPauseValueLabelOld;
  cbAutoPause.Checked := bAutoPauseOld;
  iSelect := iSelectOld;
  sButton := 'cancel';                                                         // will tell the main form that user pushed "Cancel"
  Close;
end;

{ Changement of checked-status of the 5 radiobuttons }

// A number indicating the checked radiobutton will be stored in public variable iSelect

procedure TfPSelect.rbSel1Change(Sender: TObject);

begin
  if rbSel1.Checked then begin
    iSelect := 1;
    laPauseValue.Caption := 'Zeit (sec)';
  end;
end;

procedure TfPSelect.rbSel2Change(Sender: TObject);

begin
  if rbSel2.Checked then begin
    iSelect := 2;
    laPauseValue.Caption := 'Zeit (sec)';
  end;
end;

procedure TfPSelect.rbSel3Change(Sender: TObject);

begin
  if rbSel3.Checked then begin
    iSelect := 3;
    laPauseValue.Caption := 'Weg (m)';
  end;
end;

procedure TfPSelect.rbSel4Change(Sender: TObject);

begin
  if rbSel4.Checked then begin
    iSelect := 4;
    laPauseValue.Caption := 'Weg (m)';
  end;
end;

procedure TfPSelect.rbSel5Change(Sender: TObject);

begin
  if rbSel5.Checked then begin
    iSelect := 5;
    laPauseValue.Caption := 'Geschwindigkeit (m/sec)';
  end;
end;

end.

