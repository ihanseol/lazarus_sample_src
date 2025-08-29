{***************************************************************************************}
{* Unité "sélection verbe pour affichage conjugaison" pour l'application ConjugaisonRE *}
{***************************************************************************************}

unit selectconj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {**************}
  { TfSelectConj }
  {**************}
  TfSelectConj = class(TForm)
    Label1: TLabel;
    rbRompre, rbBattre, rbEntendre: TRadioButton;
    rbCraindre, rbParaitre, rbTraduire: TRadioButton;
    rbIrreg, rbRE: TRadioButton;
    cobIrreg: TComboBox;
    edVerbe: TEdit;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure edVerbeChange(Sender: TObject);
    procedure cobIrregChange(Sender: TObject);
  private
    bRompre, bBattre, bEntendre, bCraindre, bParaitre, btraduire, bIrreg, bRE: Boolean;
  public
    sVerbe, sButton: string;
  end;

var
  fSelectConj: TfSelectConj;

implementation

{$R *.lfm}

{**************}
{ TfSelectConj }
{**************}

{ Form activation: Save actual selections }

procedure TfSelectConj.FormActivate(Sender: TObject);

begin
  bRompre   := rbRompre.Checked;   bBattre   := rbBattre.Checked;   bEntendre := rbEntendre.Checked;
  bParaitre := rbParaitre.Checked; bCraindre := rbCraindre.Checked; btraduire := rbTraduire.Checked;
  bIrreg    := rbIrreg.Checked;    bRE       := rbRE.Checked;
  sVerbe    := edVerbe.Text;
  edVerbe.SetFocus;
end;

{ Button "OK": Save verbe selected/entered to text field to be read by the main unit (and being the one to be conjugated) }

procedure TfSelectConj.btOKClick(Sender: TObject);

begin
  sVerbe := '';
  if rbRE.Checked then begin
    // If "Autre verbe en -RE" is selected, the user must enter a verb manually
    if (edVerbe.Text = '') then
      MessageDlg('Sélection invalide', 'Choisissez un verbe, s''il-vous-plaît!', mtError, [mbOK], 0)
    else
      sVerbe := edVerbe.Text;
  end
  else if rbIrreg.Checked then begin
    // If "Verb irrégulier" is selected, choose verb from combobox
    sVerbe := cobIrreg.Text;
  end
  else begin
    // Other selections: predefined given verbs
    if rbRompre.Checked then
      sVerbe := 'rompre'
    else if rbBattre.Checked then
      sVerbe := 'battre'
    else if rbEntendre.Checked then
      sVerbe := 'entendre'
    else if rbParaitre.Checked then
      sVerbe := 'paraître'
    else if rbCraindre.Checked then
      sVerbe := 'craindre'
    else if rbTraduire.Checked then
      sVerbe := 'traduire';
  end;
  // Close the form if a verb has been selected
  if sVerbe <> '' then begin
    sButton := 'ok';
    Close;
  end;
end;

{ Button "Cancel": Restore form selection as it was at form activation; close the window }

procedure TfSelectConj.btCancelClick(Sender: TObject);

begin
  rbRompre.Checked   := bRompre;   rbBattre.Checked   := bBattre;   rbEntendre.Checked := bEntendre;
  rbParaitre.Checked := bParaitre; rbCraindre.Checked := bCraindre; rbTraduire.Checked := btraduire;
  rbIrreg.Checked    := bIrreg;    rbRE.Checked := bRE;
  edVerbe.Text := sVerbe;
  sButton := 'cancel';
  Close;
end;

{ Auto-check "Autre verbe en -RE" if user enters a verb in the text input file }

procedure TfSelectConj.edVerbeChange(Sender: TObject);

begin
  rbRE.Checked := True;
end;

{ Auto-check "Verbe irrégulier" if user chooses an irregular verb from the list }

procedure TfSelectConj.cobIrregChange(Sender: TObject);

begin
  rbIrreg.Checked := True;
end;

end.

