{*****************************************************}
{* 4x4 determinant unit for Determinants application *}
{*****************************************************}

unit det4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, ExtCtrls, detcalc;

type
  TEditDet4 = array[1..4, 1..4] of TEdit;
  TEditDets = array[1..4, 1..3, 1..3] of TEdit;
  TEditMult = array[1..4] of TEdit;
  TEditRes = array[1..4] of TEdit;
  TEditMultExp = array[1..3] of TEdit;
  TEditDetsExp = array[1..3, 1..2, 1..2] of TEdit;
  {********}
  { TfDet4 }
  {********}
  TfDet4 = class(TForm)
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15: TLabel;
    Label16, Label17, Label18, Label19, Label20, Label21, Label22: TLabel;
    laDetExp: TLabel;
    Shape1, Shape2, Shape3, Shape4, Shape5, Shape6, Shape7, Shape8, Shape9: TShape;
    Shape10, Shape11, Shape12, Shape13, Shape14, Shape15, Shape16, Shape17: TShape;
    edD11, edD12, edD13, edD14: TEdit;
    edD21, edD22, edD23, edD24: TEdit;
    edD31, edD32, edD33, edD34: TEdit;
    edD41, edD42, edD43, edD44: TEdit;
    edI, edJ, edK, edL: TEdit;
    edI11, edI12, edI13, edI21, edI22, edI23, edI31, edI32, edI33: TEdit;
    edJ11, edJ12, edJ13, edJ21, edJ22, edJ23, edJ31, edJ32, edJ33: TEdit;
    edK11, edK12, edK13, edK21, edK22, edK23, edK31, edK32, edK33: TEdit;
    edL11, edL12, edL13, edL21, edL22, edL23, edL31, edL32, edL33: TEdit;
    edI2, edJ2, edK2, edL2: TEdit;
    edResI, edResJ, edResK, edResL, edResExp: TEdit;
    edI1, edJ1, edK1: TEdit;
    edI111, edI112, edI121, edI122: TEdit;
    edJ111, edJ112, edJ121, edJ122: TEdit;
    edK111, edK112, edK121, edK122: TEdit;
    edRes: TEdit;
    btCalc: TButton;
    btExpand1: TButton;
    btExpand2: TButton;
    btExpand3: TButton;
    btExpand4: TButton;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure btExpand1Click(Sender: TObject);
    procedure btExpand2Click(Sender: TObject);
    procedure btExpand3Click(Sender: TObject);
    procedure btExpand4Click(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  private
    bAutoExpand: Boolean;
    aDet4: TDetVals4;
    aDets3: TDets3;
    aMult3: TMult3;
    edDet4: TEditDet4;
    edDets: TEditDets;
    edMult, edMult2: TEditMult;
    edRes2: TEditRes;
    edMultExp: TEditMultExp;
    edDetsExp: TEditDetsExp;
  end;

var
  fDet4: TfDet4;

implementation

{$R *.lfm}

{ Calculate and expand 3x3 determinant (onto form) }

procedure Expand(Det3: TDetVals3; Mult: TEditMultExp; Dets: TEditDetsExp);

var
  N, I, J: Integer;
  Mult2: TMult2;
  Dets2: TDets2;

begin
  // Calculate 3x3 determinant
  fDet4.edResExp.Text := FloatToString(Determinant3(Det3));
  // Determinant expansion: Get multiplication values and 2x2 minors
  Mult2 := Det3Multipliers(Det3);
  Dets2 := Det3Determinants(Det3);
  // Fill in expansion values
  for N := 1 to 3 do begin
    Mult[N].Text := FloatToString(Mult2[N]);
    for I := 1 to 2 do begin
      for J := 1 to 2 do begin
        Dets[N, I, J].Text := FloatToString(Dets2[N][I, J]);
      end;
    end;
  end;
end;

{********}
{ TfDet4 }
{********}

{ Application start: Initialisation }

procedure TfDet4.FormCreate(Sender: TObject);

begin
  // Create arrays with form's edit fields
  edDet4[1, 1] := edD11; edDet4[1, 2] := edD12; edDet4[1, 3] := edD13; edDet4[1, 4] := edD14;
  edDet4[2, 1] := edD21; edDet4[2, 2] := edD22; edDet4[2, 3] := edD23; edDet4[2, 4] := edD24;
  edDet4[3, 1] := edD31; edDet4[3, 2] := edD32; edDet4[3, 3] := edD33; edDet4[3, 4] := edD34;
  edDet4[4, 1] := edD41; edDet4[4, 2] := edD42; edDet4[4, 3] := edD43; edDet4[4, 4] := edD44;
  edMult[1] := edI; edMult[2] := edJ; edMult[3] := edK; edMult[4] := edL;
  edDets[1, 1, 1] := edI11; edDets[1, 1, 2] := edI12; edDets[1, 1, 3] := edI13;
  edDets[1, 2, 1] := edI21; edDets[1, 2, 2] := edI22; edDets[1, 2, 3] := edI23;
  edDets[1, 3, 1] := edI31; edDets[1, 3, 2] := edI32; edDets[1, 3, 3] := edI33;
  edDets[2, 1, 1] := edJ11; edDets[2, 1, 2] := edJ12; edDets[2, 1, 3] := edJ13;
  edDets[2, 2, 1] := edJ21; edDets[2, 2, 2] := edJ22; edDets[2, 2, 3] := edJ23;
  edDets[2, 3, 1] := edJ31; edDets[2, 3, 2] := edJ32; edDets[2, 3, 3] := edJ33;
  edDets[3, 1, 1] := edK11; edDets[3, 1, 2] := edK12; edDets[3, 1, 3] := edK13;
  edDets[3, 2, 1] := edK21; edDets[3, 2, 2] := edK22; edDets[3, 2, 3] := edK23;
  edDets[3, 3, 1] := edK31; edDets[3, 3, 2] := edK32; edDets[3, 3, 3] := edK33;
  edDets[4, 1, 1] := edL11; edDets[4, 1, 2] := edL12; edDets[4, 1, 3] := edL13;
  edDets[4, 2, 1] := edL21; edDets[4, 2, 2] := edL22; edDets[4, 2, 3] := edL23;
  edDets[4, 3, 1] := edL31; edDets[4, 3, 2] := edL32; edDets[4, 3, 3] := edL33;
  edMult2[1] := edI2; edMult2[2] := edJ2; edMult2[3] := edK2; edMult2[4] := edL2;
  edRes2[1] := edResI; edRes2[2] := edResJ; edRes2[3] := edResK; edRes2[4] := edResL;
  edMultExp[1] := edI1; edMultExp[2] := edJ1; edMultExp[3] := edK1;
  edDetsExp[1, 1, 1] := edI111; edDetsExp[1, 1, 2] := edI112;   edDetsExp[1, 2, 1] := edI121; edDetsExp[1, 2, 2] := edI122;
  edDetsExp[2, 1, 1] := edJ111; edDetsExp[2, 1, 2] := edJ112;   edDetsExp[2, 2, 1] := edJ121; edDetsExp[2, 2, 2] := edJ122;
  edDetsExp[3, 1, 1] := edK111; edDetsExp[3, 1, 2] := edK112;   edDetsExp[3, 2, 1] := edK121; edDetsExp[3, 2, 2] := edK122;
end;

{ Form show-up: Clear 4x4 determinant entry fields }

procedure TfDet4.FormActivate(Sender: TObject);

var
  N, I, J: Integer;

begin
  for I := 1 to 4 do begin
    for J := 1 to 4 do begin
      edDet4[I, J].Text := '';
    end;
  end;
  for N := 1 to 4 do begin
    edMult[N].Text := ''; edMult2[N].Text := ''; edRes2[N].Text := '';
    for I := 1 to 3 do begin
      for J := 1 to 3 do begin
        edDets[N, I, J].Text := '';
      end;
    end;
  end;
  for N := 1 to 3 do begin
    edMultExp[N].Text := '';
    for I := 1 to 2 do begin
      for J := 1 to 2 do begin
        edDetsExp[N, I, J].Text := '';
      end;
    end;
  end;
  laDetExp.Caption := '|D1|';
  edRes.Text := ''; edResExp.Text := '';
  edD11.SetFocus;
end;

{ Button "Calculation": Calculate 4x4 determinant }

procedure TfDet4.btCalcClick(Sender: TObject);

var
  N, I, J: Integer;

begin
  // Get determinant elements from form's edit field (no numeric check done!)
  for I := 1 to 4 do begin
    for J := 1 to 4 do
      aDet4[I, J] := StrToFloat(edDet4[I, J].Text);
  end;
  // Calculate the 4x4 determinant
  edRes.Text := FloatToString(Determinant4(aDet4));
  // Determinant expansion: Get multiplication values and 3x3 minors
  aMult3 := Det4Multipliers(aDet4);
  aDets3 := Det4Determinants(aDet4);
  // Fill in expansion and calculation values
  for N := 1 to 4 do begin
    edMult[N].Text := FloatToString(aMult3[N]); edMult2[N].Text := edMult[N].Text;
    edRes2[N].Text := FloatToString(Determinant3(aDets3[N]));
    for I := 1 to 3 do begin
      for J := 1 to 3 do begin
        edDets[N, I, J].Text := FloatToString(aDets3[N][I, J]);
      end;
    end;
  end;
  // Expand first minor 3x3 determinant
  bAutoExpand := True;
  btExpand1.Click;
end;

{ Buttons "Expand |Dx|": 3x3 determinant calculation and expansion }

procedure TfDet4.btExpand1Click(Sender: TObject);

begin
  if bAutoExpand then                                                          // bAutoExpand variable used to avoid circular reference calls to procedures
    bAutoExpand := False
  else
    btCalc.Click;                                                              // to be sure to have accurate values
  laDetExp.Caption := '|D1|';
  Expand(aDets3[1], edMultExp, edDetsExp);
end;

procedure TfDet4.btExpand2Click(Sender: TObject);

begin
  btCalc.Click;                                                                // to be sure to have accurate values
  laDetExp.Caption := '|D2|';
  Expand(aDets3[2], edMultExp, edDetsExp);
end;

procedure TfDet4.btExpand3Click(Sender: TObject);

begin
  btCalc.Click;                                                                // to be sure to have accurate values
  laDetExp.Caption := '|D3|';
  Expand(aDets3[3], edMultExp, edDetsExp);
end;

procedure TfDet4.btExpand4Click(Sender: TObject);

begin
  btCalc.Click;                                                                // to be sure to have accurate values
  laDetExp.Caption := '|D4|';
  Expand(aDets3[4], edMultExp, edDetsExp);
end;

procedure TfDet4.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

