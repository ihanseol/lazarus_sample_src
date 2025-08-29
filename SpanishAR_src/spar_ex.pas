{*****************************************************}
{* Exercise selection unit for SpanishAR application *}
{*****************************************************}

unit spar_ex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {************}
  { TfSelectEx }
  {************}
  TfSelectEx = class(TForm)
    stVerbs, stMood, stForm, stGender: TStaticText;
    cbRegular, cbIrregular, cbSemiIrregular: TCheckBox;
    cbInd, cbSubj, cbImp, cbPart: TCheckBox;
    cbAff, cbNeg, cbMasc, cbFem: TCheckBox;
    btAll, btDefaults, btNone: TButton;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btAllClick(Sender: TObject);
    procedure btDefaultsClick(Sender: TObject);
    procedure btNoneClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    bRegular, bIrregular, bSemiIrregular: Boolean;
    bInd, bSubj, bImp, bPart: Boolean;
    bAff, bNeg, bMasc, bFem: Boolean;
  public
    sButton: string;
  end;

var
  fSelectEx: TfSelectEx;

implementation

{$R *.lfm}

{ Check/uncheck the various selections (as result of user button click) }

procedure Selection(Select, Defaults: Boolean);

var
  Sel: Boolean;

// Select = True : select all
// Select = False, Defaults = True : select defaults
// Select = False, Defaults = False: unselect all

begin
  Sel := Select;
  if (Select = False) and (Defaults = True) then
    Sel := not Select;
  fSelectEx.cbRegular.Checked := Sel; fSelectEx.cbIrregular.Checked := Sel; fSelectEx.cbSemiIrregular.Checked := Sel;
  fSelectEx.cbInd.Checked := Sel; fSelectEx.cbSubj.Checked := Sel;
  fSelectEx.cbImp.Checked := Select; fSelectEx.cbPart.Checked := Select;
  fSelectEx.cbAff.Checked := Sel; fSelectEx.cbNeg.Checked := Select;
  fSelectEx.cbMasc.Checked := Sel; fSelectEx.cbFem.Checked := Select;
end;

{**************}
{* TfSelectEx *}
{**************}

{ Form activation: Save actual selections }

procedure TfSelectEx.FormActivate(Sender: TObject);

begin
  bRegular := cbRegular.Checked; bIrregular := cbIrregular.Checked; bSemiIrregular := cbSemiIrregular.Checked;
  bInd := cbInd.Checked; bSubj := cbSubj.Checked; bImp := cbImp.Checked; bPart := cbPart.Checked;
  bAff := cbAff.Checked; bNeg := cbNeg.Checked;
  bMasc := cbMasc.Checked; bFem := cbFem.Checked;
end;

{ Button "Select defaults": Check default selections }

procedure TfSelectEx.btDefaultsClick(Sender: TObject);

begin
  Selection(False, True);
end;

{ Button "Select all": Check all selections }

procedure TfSelectEx.btAllClick(Sender: TObject);

begin
  Selection(True, False);
end;

{ Button "Unselect all": Uncheck all selections }

procedure TfSelectEx.btNoneClick(Sender: TObject);

begin
  Selection(False, False);
end;

{ Button "OK": Check validity of user selections and close the window (values will be directly read from the controls) }

procedure TfSelectEx.btOKClick(Sender: TObject);

Var
  Mess: string;
  OK: Boolean;

begin
  Mess := '';
  OK := cbRegular.Checked or cbIrregular.Checked or cbSemiIrregular.Checked;
  if not OK then
    Mess := 'the verbs!'
  else begin
    OK := cbInd.Checked or cbSubj.Checked or cbImp.Checked or cbPart.Checked;
    if not OK then
      Mess := 'the moods!'
    else begin
      OK := cbAff.Checked or cbNeg.Checked;
      if not OK then
        Mess := 'the forms!'
      else begin
        OK := cbMasc.Checked or cbFem.Checked;
        if not OK then
          Mess := 'the genders!'
      end;
    end;
  end;
  if not OK then begin
    Mess := 'You have forgotten to select ' + Mess;
    MessageDlg('Invalid selection', Mess, mtError, [mbOK], 0)
  end
  else begin
    sButton := 'ok';
    Close;
  end;
end;

{ Button "Cancel": Restore selections and close window }

procedure TfSelectEx.btCancelClick(Sender: TObject);

begin
  cbRegular.Checked := bRegular; cbIrregular.Checked := bIrregular; cbSemiIrregular.Checked := bSemiIrregular;
  cbInd.Checked := bInd; cbSubj.Checked := bSubj; cbImp.Checked := bImp; cbPart.Checked := bPart;
  cbAff.Checked := bAff; cbNeg.Checked := bNeg;
  cbMasc.Checked := bMasc; cbFem.Checked := bFem;
  sButton := 'cancel';
  Close;
end;

end.

