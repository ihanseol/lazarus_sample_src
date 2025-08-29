{**************************************************}
{* Data entry unit for the DCircuits2 application *}
{**************************************************}

unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfData }
  TfData = class(TForm)
    StaticText1: TStaticText;
    Label1: TLabel;  Label2:  TLabel; Label3:  TLabel; Label4:  TLabel; Label5: TLabel;
    Label6: TLabel;  Label7:  TLabel; Label8:  TLabel; Label9:  TLabel; Label10: TLabel;
    Label11: TLabel; Label12: TLabel; Label13: TLabel; Label14: TLabel; Label15: TLabel;
    rbTransistors1: TRadioButton;
    rbTransistors2: TRadioButton;
    rbTransistors3: TRadioButton;
    edVoltage: TEdit;
    edCurrent: TEdit;
    edResistance: TEdit;
    edBeta1: TEdit;
    edBeta2: TEdit;
    edBeta3: TEdit;
    cbMaterial1: TComboBox;
    cbMaterial2: TComboBox;
    cbMaterial3: TComboBox;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure rbTransistors1Change(Sender: TObject);
    procedure rbTransistors2Change(Sender: TObject);
    procedure rbTransistors3Change(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    sVoltage, sCurrent, sBeta1, sBeta2, sBeta3, sMaterial1, sMaterial2, sMaterial3: string;
    bTransistors1, bTransistors2, bTransistors3: Boolean;
  public
    iBeta1, iBeta2, iBeta3: Integer;
    sButton: string;
    bMaterial, bWarningDone: Boolean;
  end;

var
  fData: TfData;

implementation

{$R *.lfm}

{**********}
{* TfData *}
{**********}

{ Form activate/display: Save variables' actual values}

procedure TfData.FormActivate(Sender: TObject);

begin
  bTransistors1 := rbTransistors1.Checked; bTransistors2 := rbTransistors2.Checked; bTransistors3 := rbTransistors3.Checked;
  sVoltage := edVoltage.Text; sCurrent := edCurrent.Text;
  sBeta1 := edBeta1.Text; sMaterial1 := cbMaterial1.Text;
  sBeta2 := edBeta2.Text; sMaterial2 := cbMaterial2.Text;
  sBeta3 := edBeta3.Text; sMaterial3 := cbMaterial3.Text;
  bWarningDone := False;
  edVoltage.SetFocus;
end;

{ Number of transistors changed: enable/disable transistors' characteristics entry }

procedure TfData.rbTransistors1Change(Sender: TObject);

begin
  if rbTransistors1.Checked then begin
    edBeta2.Enabled := False; edBeta3.Enabled := False;
    cbMaterial2.Enabled := False; cbMaterial3.Enabled := False;
  end;
end;

procedure TfData.rbTransistors2Change(Sender: TObject);

begin
  if rbTransistors2.Checked then begin
    edBeta2.Enabled := True; edBeta3.Enabled := False;
    if bMaterial then
      cbMaterial2.Enabled := True;
    cbMaterial3.Enabled := False;
  end;
end;

procedure TfData.rbTransistors3Change(Sender: TObject);

begin
  if rbTransistors3.Checked then begin
    edBeta2.Enabled := True; edBeta3.Enabled := True;
    if bMaterial then begin
      cbMaterial2.Enabled := True;
      cbMaterial3.Enabled := True;
    end;
  end;
end;

{ Button "OK": Check data validity and close the form }

procedure TfData.btOKClick(Sender: TObject);

var
  NR1, NR2: Real;
  Mess, Title: string;

begin
  Title := 'Invalid data'; Mess := '';
  if edVoltage.Text = '' then
    NR1 := 0
  else
    NR1 := StrToFloat(edVoltage.Text);
  if NR1 <= 0 then begin
    Mess := 'Battery voltage must be > 0!';
    edVoltage.SetFocus;
  end
  else begin
    if edCurrent.Text = '' then
      NR1 := 0
    else
      NR1 := StrToFloat(edCurrent.Text);
    if edResistance.Text = '' then
      NR2 := 0
    else
      NR2 := StrToFloat(edResistance.Text);
    if (NR1 <> 0) and (NR2 <> 0) and (StrToFloat(edVoltage.Text) / NR1 <> NR2) then begin
      Mess := 'Load current and resistance must not be specified both!';
      edCurrent.SetFocus;
    end
    else if not ((NR1 > 0) or (NR2 > 0)) then begin
      Mess := 'Load current/resistance must be > 0!';
      edCurrent.SetFocus;
    end
    else begin
      if edBeta1.Text = '' then
        iBeta1 := 0
      else
        iBeta1 := StrToInt(edBeta1.Text);
      if not edBeta2.Enabled then
        iBeta2 := 100
      else if edBeta2.Text = '' then
        iBeta2 := 0
      else
        iBeta2 := StrToInt(edBeta2.Text);
      if not edBeta3.Enabled then
        iBeta3 := 100
      else if edBeta3.Text = '' then
        iBeta3 := 0
      else
        iBeta3 := StrToInt(edBeta3.Text);
      if (iBeta1 <= 0) or (iBeta2 <= 0) or (iBeta3 <= 0) then begin
        Mess := 'Current gain values must be > 0!';
        if iBeta1 <= 0 then
          edBeta1.SetFocus
        else if iBeta2 <= 0 then
          edBeta2.SetFocus
        else
          edBeta3.SetFocus;
      end
      else if not bWarningDone then begin
        if (iBeta1 < 10) or (iBeta2 < 10) or (iBeta3 < 10) or (iBeta1 > 300) or (iBeta2 > 300) or (iBeta3 > 300) then begin
          Title := 'Untypical data'; Mess := 'Typical current gain values are between 10 and 300';
          bWarningDone := True;                                                // in a second attempt, 'outsie range values' will be accepted
        end;
      end;
    end;
  end;
  if Mess = '' then begin
    sButton := 'ok';                                                           // tell main form that "OK" button was pressed
    Close;
  end
  else begin
    if LeftStr(Title, 7) = 'Invalid' then
      MessageDlg(Title, Mess, mtError, [mbOK], 0)
    else
      MessageDlg(Title, Mess, mtWarning, [mbOK], 0)
  end;
end;

{ Button "Cancel": restore data and close the form }

procedure TfData.btCancelClick(Sender: TObject);

begin
  rbTransistors1.Checked := bTransistors1; rbTransistors2.Checked := bTransistors2; rbTransistors3.Checked := bTransistors3;
  edVoltage.Text := sVoltage; edCurrent.Text := sCurrent;
  edBeta1.Text := sBeta1; cbMaterial1.Text := sMaterial1;
  edBeta2.Text := sBeta2; cbMaterial2.Text := sMaterial2;
  edBeta3.Text := sBeta3; cbMaterial3.Text := sMaterial3;
  sButton := 'cancel';                                                         // tell main form that "Cancel" button was pressed
  Close;
end;

end.

