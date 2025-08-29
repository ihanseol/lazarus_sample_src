{**********************************************}
{* Data entry unit for DCircuits3 application *}
{**********************************************}

unit data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfData }
  {********}
  TfData = class(TForm)
    Label1, Label2, Label3, laDiodeMaterial, laDiodePIV: TLabel;
    rbCircuit1, rbCircuit2, rbCircuit3, rbCircuit5, rbCircuit4: TRadioButton;
    cobDiodeType, cobDiodeMaterial, cobDiodePIV: TComboBox;
    edDiodeType: TEdit;
    edDiodePIV: TEdit;
    btSelect: TButton;
    btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btSelectClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure rbCircuit1Change(Sender: TObject);
    procedure rbCircuit2Change(Sender: TObject);
    procedure rbCircuit3Change(Sender: TObject);
    procedure rbCircuit4Change(Sender: TObject);
    procedure rbCircuit5Change(Sender: TObject);
    procedure cobDiodeTypeChange(Sender: TObject);
    procedure cobDiodeMaterialChange(Sender: TObject);
    procedure cobDiodePIVChange(Sender: TObject);
  private
    iCircuit, iType, iMaterial, iPIV: Integer;
    bCircuit1, bCircuit2, bCircuit3, bCircuit4, bCircuit5, bPIV, bPIV2: Boolean;
  public
    sButton: string;
  end;

var
  fData: TfData;

implementation

{$R *.lfm}

{ Set PIV value for actual diode (as selection in PIV combobox) }

procedure SetPIV(IX: Integer);

const
  XPIV: array[1..10] of Integer = (
    0, 3, 5, 6, 7, 0, 1, 2, 3, 4
  );

begin
  fData.cobDiodePIV.ItemIndex := XPIV[IX];
end;

{ Show or hide user entry fields (depending on circuit selected) }

procedure ShowDiodeFields(Circuit: Integer);

var
  DiodeFields: Boolean;

begin
  DiodeFields := True;
  fData.laDiodePIV.Visible := False; fData.cobDiodePIV.Visible := False; fData.edDiodePIV.Visible := False;
  if Circuit = 4 then begin
    // Not accurate for Zener diode circuit
    DiodeFields := False;
  end;
  fData.cobDiodeType.Visible := DiodeFields; fData.edDiodeType.Visible := not DiodeFields;
  fData.laDiodeMaterial.Visible := DiodeFields; fData.cobDiodeMaterial.Visible := DiodeFields;
  if Circuit = 5 then begin
    // PIV used only with diode breakdown circuit
    fData.laDiodePIV.Visible := True;
    if fData.cobDiodeType.ItemIndex = 0 then begin
      // Manual PIV entry for custom diode type
      fData.edDiodePIV.Visible := True
    end
    else begin
      // Fixed PIV value for given diode type
      fData.cobDiodePIV.Visible := True;
      SetPIV(fData.cobDiodeType.ItemIndex);
    end;
  end;
end;

{********}
{ TfData }
{********}

{ Application start: Initialisation }

procedure TfData.FormCreate(Sender: TObject);

begin
  iCircuit := 1;
end;

{ Window show-up: Save actual settings }

procedure TfData.FormActivate(Sender: TObject);

begin
  bCircuit1 := rbCircuit1.Checked; bCircuit2 := rbCircuit2.Checked;
  bCircuit3 := rbCircuit3.Checked; bCircuit4 := rbCircuit4.Checked; bCircuit5 := rbCircuit5.Checked;
  iType := cobDiodeType.ItemIndex; iMaterial := cobDiodeMaterial.ItemIndex;
  bPIV := laDiodePIV.Visible; bPIV2 := edDiodePIV.Visible; iPIV := cobDiodePIV.ItemIndex;
end;

{ Button "Select": Get user selected values and close the window }

procedure TfData.btSelectClick(Sender: TObject);

var
  Ret: Cardinal;
  PIV: Real;
  Mess: string;

begin
  Mess := '';
  if rbCircuit5.Checked then begin
    // Get PIV value (for diode breakdown selection )
    if cobDiodeType.ItemIndex = 0 then begin
      if edDiodePIV.Text = '' then
        PIV := 0
      else
        PIV := StrToFloat(edDiodePIV.Text);
      if PIV <= 0 then
        Mess := 'Diode breakdown voltage must be greater than 0'
      else if (cobDiodeMaterial.Text = 'Silicon') and (PIV > 1000) then begin
        // Check user entry against standard silicon diode PIV
        Ret := MessageDlg('Diode PIV', 'Use PIV > 1000V for silicon diode ?', mtWarning, [mbYes, mbNo], 0);
        if Ret = mrNo then begin
          Mess := 'Cancel';
          edDiodePIV.SetFocus;
        end;
      end
      else if (cobDiodeMaterial.Text = 'Germanium') and (PIV > 400) then begin
        // Check user entry against standard germanium diode PIV
        Ret := MessageDlg('Diode PIV', 'Use PIV > 400V for germanium diode ?', mtWarning, [mbYes, mbNo], 0);
        if Ret = mrNo then begin
          Mess := 'Cancel';
          edDiodePIV.SetFocus;
        end;
      end
    end
    else
      edDiodePIV.Text := cobDiodePIV.Items[cobDiodePIV.ItemIndex];
  end;
  if Mess <> '' then begin
    if Mess <> 'Cancel' then
      MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
  end
  else begin
    sButton := 'select';                                                       // this field is read by the main unit to know which button closed the window
    Close;
  end;
end;

{ Button "Cancel": Restore original settings and close window }

procedure TfData.btCancelClick(Sender: TObject);

begin
  rbCircuit1.Checked := bCircuit1; rbCircuit2.Checked := bCircuit2;
  rbCircuit3.Checked := bCircuit3; rbCircuit4.Checked := bCircuit4; rbCircuit5.Checked := bCircuit5;
  cobDiodeType.ItemIndex := iType; cobDiodeMaterial.ItemIndex := iMaterial;
  laDiodePIV.Visible := bPIV; edDiodePIV.Visible := bPIV2; cobDiodePIV.Visible := not bPIV2; cobDiodePIV.ItemIndex := iPIV;
  sButton := 'cancel';                                                         // to tell the main unit that no selections have been made
  Close;
end;

{ Radiobutton changes: Circuit selection (setting available entry fields as adequate) }

procedure TfData.rbCircuit1Change(Sender: TObject);

begin
  if rbCircuit1.Checked then begin
    iCircuit := 1;
    ShowDiodeFields(iCircuit);
  end;
end;

procedure TfData.rbCircuit2Change(Sender: TObject);

begin
  if rbCircuit2.Checked then begin
    iCircuit := 2;
    ShowDiodeFields(iCircuit);
  end;
end;

procedure TfData.rbCircuit3Change(Sender: TObject);

begin
  if rbCircuit3.Checked then begin
    iCircuit := 3;
    ShowDiodeFields(iCircuit);
  end;
end;

procedure TfData.rbCircuit4Change(Sender: TObject);

begin
  if rbCircuit4.Checked then begin
    iCircuit := 4;
    ShowDiodeFields(iCircuit);
  end;
end;

procedure TfData.rbCircuit5Change(Sender: TObject);

begin
  if rbCircuit5.Checked then begin
    iCircuit := 5;
    ShowDiodeFields(iCircuit);
  end;
end;

{ Selection change in the diode type combobox }

procedure TfData.cobDiodeTypeChange(Sender: TObject);

begin
  if cobDiodeType.ItemIndex <= 5 then
    cobDiodeMaterial.ItemIndex := 0                                            // the first 5 selections are silicon diodes
  else
    cobDiodeMaterial.ItemIndex := 1;                                           // the last 5 selections are germanium diodes
  ShowDiodeFields(iCircuit);
end;

{ Selection change in the diode material combobox }

procedure TfData.cobDiodeMaterialChange(Sender: TObject);

begin
  if cobDiodeType.ItemIndex > 0 then begin
    // Does not concern custom diode type
    if cobDiodeType.ItemIndex <= 5 then
      cobDiodeMaterial.ItemIndex := 0                                          // can't select Ge if diode actually is Si
    else
      cobDiodeMaterial.ItemIndex := 1;                                         // can't select Si if diode actually is Ge
  end;
end;

{ Selection change in the PIV combobox }

procedure TfData.cobDiodePIVChange(Sender: TObject);

begin
  if cobDiodeType.ItemIndex > 0 then
    SetPIV(cobDiodeType.ItemIndex);                                            // set PIV to fixed value for given diode type
end;

end.

