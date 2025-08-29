{***************************************************************************************}
{* Unité "sélection verbe pour affichage conjugaison" pour l'application ConjugaisonER *}
{***************************************************************************************}

unit selectconj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfSelectConj }
  TfSelectConj = class(TForm)
    Label1: TLabel;
    rbAvoir: TRadioButton;
    rbEtre: TRadioButton;
    rbAller: TRadioButton;
    rbEnAller: TRadioButton;
    rbYAller: TRadioButton;
    rbER: TRadioButton;
    rbEnvoyer: TRadioButton;
    edVerbe: TEdit;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure edVerbeChange(Sender: TObject);
  private
    bAvoir, bEtre, bAller, bYAller, bEnAller, bEnvoyer, bER: Boolean;
  public
    sVerbe, sButton: string;
  end;

var
  fSelectConj: TfSelectConj;

implementation

{$R *.lfm}

{****************}
{* TfSelectConj *}
{****************}

{ Form activation: save actual selections }

procedure TfSelectConj.FormActivate(Sender: TObject);
begin
  bAvoir   := rbAvoir.Checked;   bEtre  := rbEtre.Checked;
  bAller   := rbAller.Checked;   bYAller := rbYaller.Checked; bEnAller := rbEnAller.Checked;
  bEnvoyer := rbEnvoyer.Checked; bER := rbER.Checked;
  sVerbe   := edVerbe.Text;
  edVerbe.SetFocus;
end;

{ Button "OK": Save verbe selected/entered to text field to be read by the main unit (and being the one to be conjugated) }

procedure TfSelectConj.btOKClick(Sender: TObject);

begin
  sVerbe := '';
  if rbER.Checked then begin
    // If "Autre verbe en -ER" is selected, the user must enter a verb manually
    if (edVerbe.Text = '') then
      MessageDlg('Sélection invalide', 'Choisissez un verbe, s''il-vous-plaît!', mtError, [mbOK], 0)
    else
      sVerbe := edVerbe.Text;
  end
  else begin
    // Other selections: predefined given verbs
    if rbAvoir.Checked then
      sVerbe := 'avoir'
    else if rbEtre.Checked then
      sVerbe := 'être'
    else if rbAller.Checked then
      sVerbe := 'aller'
    else if rbYAller.Checked then
      sVerbe := 'y aller'
    else if rbEnAller.Checked then
      sVerbe := 'en aller'
    else if rbEnvoyer.Checked then
      sVerbe := 'envoyer';
  end;
  // Close the form if a verb (verb group) has been seelcted
  if sVerbe <> '' then begin
    sButton := 'ok';
    Close;
  end;
end;

{ Button "Cancel": Restore form selection as it was at form activation; close the window }

procedure TfSelectConj.btCancelClick(Sender: TObject);

begin
  rbAvoir.Checked   := bAvoir; rbEtre.Checked := bEtre;
  rbAller.Checked   := bAller; rbYAller.Checked   := bYAller; rbEnAller.Checked := bEnAller;
  rbEnvoyer.Checked := bEnvoyer; rbER.Checked := bER;
  edVerbe.Text := sVerbe;
  sButton := 'cancel';
  Close;
end;

{ Auto-check "Autre verbe en -ER" if user enters a verb in the text input file }

procedure TfSelectConj.edVerbeChange(Sender: TObject);

begin
  rbER.Checked := True;
end;

end.

