{*****************************************************}
{* Select sample unit for LotkaVolterra2 application *}
{*****************************************************}

unit lv2_samples;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {********}
  { TfLV2S }
  {********}
  TfLV2S = class(TForm)
    Label1: TLabel;
    rbSample1, rbSample2, rbSample3, rbSample4, rbSample5: TRadioButton;
    rbSample6, rbSample7, rbSample8, rbSample9, rbSample10: TRadioButton;
    btSelect: TButton;
    btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btSelectClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    rbSelections: array[1..10] of TRadioButton;
    aSelections : array[1..10] of Boolean;
  public
    iSample: Integer;
    sButton: string;
  end;

var
  fLV2S: TfLV2S;

implementation

{$R *.lfm}

{********}
{ TfLV2S }
{********}

{ Application start: Initialisation }

procedure TfLV2S.FormCreate(Sender: TObject);

begin
  // Create array with sample selection radio buttons
  rbSelections[1] := rbSample1; rbSelections[2] := rbSample2; rbSelections[3] := rbSample3; rbSelections[4] := rbSample4;
  rbSelections[5] := rbSample5; rbSelections[6] := rbSample6; rbSelections[7] := rbSample7; rbSelections[8] := rbSample8;
  rbSelections[9] := rbSample9; rbSelections[10] := rbSample10;
end;

{ Samples window show-up: Save actual radio buttons states }

procedure TfLV2S.FormActivate(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 10 do
    aSelections[I] := rbSelections[I].Checked;
end;

{ Button "Select": Save selection (for retrieval by main unit) and close the window }

procedure TfLV2S.btSelectClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 10 do begin
    if rbSelections[I].Checked then
      iSample := I;
  end;
  sButton := 'select';                                                         // to tell the man unit, that the "Select" button was pushed
  Close;
end;

{ Button "Cancel": Restore original radio buttons states and close the samples window }

procedure TfLV2S.btCancelClick(Sender: TObject);

var
  I: Integer;

begin
  for I := 1 to 10 do
    rbSelections[I].Checked := aSelections[I];
  sButton := 'cancel';                                                         // to tell the man unit, that the "Cancel" button was pushed
  iSample := -1;
  Close;
end;

end.

