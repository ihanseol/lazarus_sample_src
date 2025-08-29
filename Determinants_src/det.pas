{******************************************}
{* Main unit for Determinants application *}
{******************************************}

unit det;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, det3, det4, detcalc;

type
  TEditDet2 = array[1..2, 1..2] of TEdit;
  {*******}
  { TfDet }
  {*******}
  TfDet = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileDet2, mFileDet3, mFileDet4, mFileExit: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    Shape1, Shape2: TShape;
    edD11, edD12, edD21, edD22: TEdit;
    edR11, edR12, edR21, edR22, edRes: TEdit;
    btCalc: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileDet2Click(Sender: TObject);
    procedure mFileDet3Click(Sender: TObject);
    procedure mFileDet4Click(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
  private
    aDet2: TDetVals2;
    edDet2: TEditDet2;
  end;

var
  fDet: TfDet;

implementation

{$R *.lfm}

{*******}
{ TfDet }
{*******}

{ Application start: Initialisation }

procedure TfDet.FormCreate(Sender: TObject);

begin
  // Create array with form's edit fields
  edDet2[1, 1] := edD11; edDet2[1, 2] := edD12; edDet2[2, 1] := edD21; edDet2[2, 2] := edD22;
end;

{ Menu item "File > New 2x2 determinant": Clear 2x2 determinant entry fields }

procedure TfDet.mFileDet2Click(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 1 to 2 do begin
    for J := 1 to 2 do begin
      edDet2[I, J].Text := '';
    end;
  end;
  edR11.Text := ''; edR12.Text := ''; edR21.Text := ''; edR22.Text := ''; edRes.Text := '';
  edD11.SetFocus;
end;

{ Menu item "File > New 3x3 determinant": Display 3x3 determinant form (all code in "det3" unit) }

procedure TfDet.mFileDet3Click(Sender: TObject);

begin
  fDet3.ShowModal;
end;

{ Menu item "File > New 4x4 determinant": Display 4x4 determinant form (all code in "det4" unit) }

procedure TfDet.mFileDet4Click(Sender: TObject);

begin
  fDet4.ShowModal;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfDet.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Help > About": Display application about }

procedure TfDet.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Mathematics: Determinants.' + LineEnding;
  S += 'Calculate the value of 2x2, 3x3 and 4x4 determinants.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2019.';
  MessageDlg('About "Determinants"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculate": Calculate 2x2 determinant }

procedure TfDet.btCalcClick(Sender: TObject);

var
  I, J: Integer;

begin
  // Get determinant elements from form's edit field (no numeric check done!)
  for I := 1 to 2 do begin
    for J := 1 to 2 do
      aDet2[I, J] := StrToFloat(edDet2[I, J].Text);
  end;
  // Fill in the calculation fields
  edR11.Text := edD11.Text; edR12.Text := edD12.Text;
  edR21.Text := edD21.Text; edR22.Text := edD22.Text;
  // Calculate the determinant (all calculation functions in "detcalc" unit)
  edRes.Text := FloatToString(Determinant2(aDet2));
end;

end.

