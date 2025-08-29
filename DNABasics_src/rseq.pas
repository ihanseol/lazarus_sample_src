{**************************************************}
{* Random sequence unit for DNABasics application *}
{**************************************************}

unit rseq;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfRSeq }
  {********}
  TfRSeq = class(TForm)
    Label1, Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    edMin, edMax, edUncertain, edMess: TEdit;
    cbUnknownOnly: TCheckBox;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  public
    iMin, iMax, iUncertain: Integer;
    bUnknownOnly: Boolean;
  end;

var
  fRSeq: TfRSeq;

implementation

{$R *.lfm}

{********}
{ TfRSeq }
{********}

{ Window show-up: Fill in actual sequence values }

procedure TfRSeq.FormActivate(Sender: TObject);

begin
  edMin.Text := IntToStr(iMin); edMax.Text := IntToStr(iMax);
  edUncertain.Text := IntToStr(iUncertain); cbUnknownOnly.Checked := bUnknownOnly;
end;

{ Button "OK" pushed: Get new sequence values from form and close window }

procedure TfRSeq.btOKClick(Sender: TObject);

var
  Mess: string;

begin
  Mess := '';
  if (edMin.Text = '') or (StrToInt(edMin.Text) <= 0) then begin
    Mess := 'Missing or invalid sequence minimum length';
    edMin.SetFocus;
  end
  else
    iMin := StrToInt(edMin.Text);
  if Mess = '' then begin
    if (edMax.Text = '') or (StrToInt(edMax.Text) < iMin) then begin
      Mess := 'Missing or invalid sequence maximum length';
      edMax.SetFocus;
    end
    else
      iMax := StrToInt(edMax.Text);
  end;
  if Mess = '' then begin
    if (edUncertain.Text = '') or (StrToInt(edUncertain.Text) < 0) or (StrToInt(edUncertain.Text) > 100) then begin
      Mess := 'Missing or invalid percentage of uncertain bases';
      edUncertain.SetFocus;
    end
    else
      iUncertain := StrToInt(edUncertain.Text);
  end;
  if Mess = '' then
    bUnknownOnly := cbUnknownOnly.Checked;
  if Mess <> '' then begin
    // Erroneous user data: Display error message
    edMess.Text := Mess;
    edMess.Visible := True;
  end
  else begin
    // All user data ok: Close window
    edMess.Visible := False;
    Close;
  end;
end;

{ Button "Cancel" pushed: Close window }

procedure TfRSeq.btCancelClick(Sender: TObject);

begin
  Close;
end;

end.

