{********************************************}
{* Configuration unit for Clock application *}
{********************************************}

unit clock_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, DateUtils;

type
  {**********}
  { TfConfig }
  {**********}
  TfConfig = class(TForm)
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    Label7, Label8, Label9, Label10, Label11: TLabel;
    edCity, edCity2, edUTC, edDST, edDSTStartTime, edDSTEndTime: TEdit;
    cbDST: TCheckBox;
    cobDSTStartSeq, cobDSTStartDay, cobDSTStartMonth: TComboBox;
    cobDSTEndSeq, cobDSTEndDay, cobDSTEndMonth: TComboBox;
    btSave: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    iDSTStartSeq, iDSTStartDay, iDSTStartMonth, iDSTEndSeq, iDSTEndDay, iDSTEndMonth: Integer;
    sCity, sCity2, sUTC, sDST, sDSTStartTime, sDSTEndTime: string;
    bDST: Boolean;
  public
    sButton: string;
  end;

var
  fConfig: TfConfig;

function IsCorrectOffset(OffsetSign: Char; OffsetTime: string): Boolean;                 // public function (also used by main unit)

implementation

{$R *.lfm}

{ Check format validity of time offset }

function IsCorrectOffset(OffsetSign: Char; OffsetTime: string): Boolean;

var
  IsCorrect: Boolean;
  DT: TTime;

begin
  IsCorrect := True;
  if (OffsetSign <> '+') and (OffsetSign <> '-') then
    IsCorrect := False
  else if (Length(OffsetTime) <> 5) or (not TryStrToTime(OffsetTime, DT)) then           // must be a valid hh:mm time value
    IsCorrect := False;
  Result := IsCorrect;
end;

{**********}
{ TfConfig }
{**********}

{ Window show-up: Save actual form field values }

procedure TfConfig.FormActivate(Sender: TObject);

begin
  sCity := edCity.Text; sCity2 := edCity2.Text;
  sUTC := edUTC.Text;
  bDST := cbDST.Checked; sDST := edDST.Text;
  iDSTStartSeq := cobDSTStartSeq.ItemIndex; iDSTStartDay := cobDSTStartDay.ItemIndex;
  iDSTStartMonth := cobDSTStartMonth.ItemIndex; sDSTStartTime := edDSTStartTime.Text;
  iDSTEndSeq := cobDSTEndSeq.ItemIndex; iDSTEndDay := cobDSTEndDay.ItemIndex;
  iDSTEndMonth := cobDSTEndMonth.ItemIndex; sDSTEndTime := edDSTEndTime.Text;
end;

{ Button "Save": Check validity of user input and close the window }

procedure TfConfig.btSaveClick(Sender: TObject);

var
  Mess: string;
  DT: TTime;

begin
  Mess := '';
  if edCity.Text = '' then begin
    Mess := 'Missing location name!';
    edCity.SetFocus;
  end
  else if not IsCorrectOffset(LeftStr(edUTC.Text, 1)[1], RightStr(edUTC.Text, 5)) then begin
    Mess := 'Invalid UTC offset!';
    edUTC.SetFocus;
  end
  else begin
    if edCity2.Text = '' then
      edCity2.Text := edCity.Text;                                                       // use English name, if local name not set
    // If DST is used, check validity of all DST related data
    if cbDST.Checked then begin
      // Check DST offset validity
      if not IsCorrectOffset(LeftStr(edDST.Text, 1)[1], RightStr(edDST.Text, 5)) then
        Mess := 'Invalid DST offset!'
      // Check DST offset versus UTC offset
      else if ((LeftStr(edDST.Text, 1) = '+') and (CompareTime(StrToTime(RightStr(edDST.Text, 5)), StrToTime(RightStr(edUTC.Text, 5))) <> 1)) or
        ((LeftStr(edDST.Text, 1) = '-') and (CompareTime(StrToTime(RightStr(edDST.Text, 5)), StrToTime(RightStr(edUTC.Text, 5))) <> -1)) then
        Mess := 'DST offset must be greater than UTC offset!';
      if Mess <> '' then
        edDST.SetFocus
      else begin
        // Check DST starting and ending date
        if (cobDSTStartSeq.ItemIndex = -1) or (cobDSTStartDay.ItemIndex = -1) or (cobDSTStartMonth.ItemIndex = -1) then begin
          Mess := 'Incomplete DST start date'
        end
        else if (cobDSTEndSeq.ItemIndex = -1) or (cobDSTEndDay.ItemIndex = -1) or (cobDSTEndMonth.ItemIndex = -1) then begin
          Mess := 'Incomplete DST end date'
        end
        // Check DST starting end ending time
        else if not TryStrToTime(edDSTStartTime.Text, DT) then begin
          Mess := 'Invalid DST start time!';
          edDSTStartTime.SetFocus;
        end
        else if not TryStrToTime(edDSTEndTime.Text, DT) then begin
          Mess := 'Invalid DST end time!';
          edDSTEndTime.SetFocus;
        end;
      end;
    end
    else begin
      // If DST is not used, set DST offset equal to UTC offset
      edDST.Text := edUTC.Text;
    end;
  end;
  // If erroneous input data, display error message
  if Mess <> '' then
    MessageDlg('Input error', Mess, mtError, [mbOK], 0)
  // If all OK, close the window
  else begin
    sButton := 'save';                                                                   // tell main unit that "Save" button has been pushed
    Close;
  end;
end;

{ Button "Cancel": Restore original form field data and close the window }

procedure TfConfig.btCancelClick(Sender: TObject);

begin
  edCity.Text := sCity; edCity2.Text := sCity2;
  edUTC.Text := sUTC;
  cbDST.Checked := bDST; edDST.Text := sDST;
  cobDSTStartSeq.ItemIndex := iDSTStartSeq; cobDSTStartDay.ItemIndex := iDSTStartDay;
  cobDSTStartMonth.ItemIndex := iDSTStartMonth; edDSTStartTime.Text := sDSTStartTime;
  cobDSTEndSeq.ItemIndex := iDSTEndSeq; cobDSTEndDay.ItemIndex := iDSTEndDay;
  cobDSTEndMonth.ItemIndex := iDSTEndMonth; edDSTEndTime.Text := sDSTEndTime;
  sButton := 'cancel';                                                                   // tell main unit that "Cancel" button has been pushed
  Close;
end;

end.

