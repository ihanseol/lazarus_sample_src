{*****************************************************}
{* 3x3 determinant unit for Determinants application *}
{*****************************************************}

unit det3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, detcalc;

type
  TEditDet3 = array[1..3, 1..3] of TEdit;
  TEditDets = array[1..3, 1..2, 1..2] of TEdit;
  TEditMult = array[1..3] of TEdit;
  TEditRes = array[1..3] of TEdit;
  {********}
  { TfDet3 }
  {********}
  TfDet3 = class(TForm)
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10, Label11: TLabel;
    Shape1, Shape2, Shape3, Shape4, Shape5, Shape6, Shape7, Shape8: TShape;
    edD11, edD12, edD13, edD21, edD22, edD23, edD31, edD32, edD33: TEdit;
    edI, edJ, edK: TEdit;
    edI11, edI12, edI21, edI22: TEdit;
    edJ11, edJ12, edJ21, edJ22: TEdit;
    edK11, edK12, edK21, edK22: TEdit;
    edI2, edJ2, edK2: TEdit;
    edResI, edResK, edResJ, edRes: TEdit;
    btCalc: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    aDet3:  TDetVals3;
    aDets2: TDets2;
    aMult2: TMult2;
    edDet3: TEditDet3;
    edDets: TEditDets;
    edMult, edMult2: TEditMult;
    edRes2: TEditRes;
  end;

var
  fDet3: TfDet3;

implementation

{$R *.lfm}

{********}
{ TfDet3 }
{********}

{ Application start: Initialisation }

procedure TfDet3.FormCreate(Sender: TObject);

begin
  // Create arrays with form's edit fields
  edDet3[1, 1] := edD11; edDet3[1, 2] := edD12; edDet3[1, 3] := edD13;
  edDet3[2, 1] := edD21; edDet3[2, 2] := edD22; edDet3[2, 3] := edD23;
  edDet3[3, 1] := edD31; edDet3[3, 2] := edD32; edDet3[3, 3] := edD33;
  edMult[1] := edI; edMult[2] := edJ; edMult[3] := edK;
  edDets[1, 1, 1] := edI11; edDets[1, 1, 2] := edI12; edDets[1, 2, 1] := edI21; edDets[1, 2, 2] := edI22;
  edDets[2, 1, 1] := edJ11; edDets[2, 1, 2] := edJ12; edDets[2, 2, 1] := edJ21; edDets[2, 2, 2] := edJ22;
  edDets[3, 1, 1] := edK11; edDets[3, 1, 2] := edK12; edDets[3, 2, 1] := edK21; edDets[3, 2, 2] := edK22;
  edMult2[1] := edI2; edMult2[2] := edJ2; edMult2[3] := edK2;
  edRes2[1] := edResI; edRes2[2] := edResJ; edRes2[3] := edResK;
end;

{ Form show-up: Clear 3x3 determinant entry fields }

procedure TfDet3.FormActivate(Sender: TObject);

var
  N, I, J: Integer;

begin
  for I := 1 to 3 do begin
    for J := 1 to 3 do begin
      edDet3[I, J].Text := '';
    end;
  end;
  for N := 1 to 3 do begin
    edMult[N].Text := ''; edMult2[N].Text := ''; edRes2[N].Text := '';
    for I := 1 to 2 do begin
      for J := 1 to 2 do begin
        edDets[N, I, J].Text := '';
      end;
    end;
  end;
  edRes.Text := '';
  edD11.SetFocus;
end;

{ Button "Calculation": Calculate 3x3 determinant }

procedure TfDet3.btCalcClick(Sender: TObject);

var
  N, I, J: Integer;

begin
  // Get determinant elements from form's edit field (no numeric check done!)
  for I := 1 to 3 do begin
    for J := 1 to 3 do
      aDet3[I, J] := StrToFloat(edDet3[I, J].Text);
  end;
  // Calculate the 3x3 determinant
  edRes.Text := FloatToString(Determinant3(aDet3));
  // Determinant expansion: Get multiplication values and 2x2 minors
  aMult2 := Det3Multipliers(aDet3);
  aDets2 := Det3Determinants(aDet3);
  // Fill in expansion and calculation values
  for N := 1 to 3 do begin
    edMult[N].Text := FloatToString(aMult2[N]); edMult2[N].Text := edMult[N].Text;
    edRes2[N].Text := FloatToString(Determinant2(aDets2[N]));
    for I := 1 to 2 do begin
      for J := 1 to 2 do begin
        edDets[N, I, J].Text := FloatToString(aDets2[N][I, J]);
      end;
    end;
  end;
end;

{ Button "Close": Close the window }

procedure TfDet3.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

