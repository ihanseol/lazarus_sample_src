{**********************************************************}
{* Unité "verbe inconnu" pour l'application ConjugaisonRE *}
{**********************************************************}

unit unknownconj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LazUTF8, common;

type
  {***********}
  { TfUnknown }
  {***********}
  TfUnknown = class(TForm)
    Label1, Label2, Label3, Label4: TLabel;
    Memo1: TMemo;
    cbVImpersonnel, cbPronominal: TCheckBox;
    cbAuxAvoir, cbAuxEtre: TCheckBox;
    cbPassif, cbVPronominal: TCheckBox;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure cbVImpersonnelChange(Sender: TObject);
    procedure cbVPronominalChange(Sender: TObject);
  private
    cVerbeClasse, cAuxiliaire: Char;
    bPassif, bPronominal: Boolean;
  public
    Verbe: TVerbe;
    sButton: string;
  end;

var
  fUnknown: TfUnknown;

implementation

{$R *.lfm}

{***********}
{ TfUnknown }
{***********}

{ Form activation: Save actual selections }

procedure TfUnknown.FormActivate(Sender: TObject);

var
  S: string;

begin
  S := 'Le verbe "' + Verbe.VerbeNom + '" n''est pas connu par le programme. Pour assurer une conjugaison correcte, ';
  S += 'revoyez (et modifier, s''il y a lieu) les paramètres ci-dessous. A noter, que le programme détectera de ';
  S += 'par lui-même les cas de conjugaisons spéciales tels les verbes en -dre, -ttre, -aindre, -aître, -uire, ainsi que ';
  S += 'les composés des verbes irréguliers.';
  Memo1.Clear;
  Memo1.Lines.Text := S;
  if cbVImpersonnel.Checked then
    cVerbeClasse := 'I'
  else if cbVPronominal.Checked then
    cVerbeClasse := 'P'
  else
    cVerbeClasse := '-';
  if cbAuxAvoir.Checked and cbAuxEtre.Checked then
    cAuxiliaire := 'X'
  else if cbAuxAvoir.Checked then
    cAuxiliaire := 'A'
  else if cbAuxEtre.Checked then
    cAuxiliaire := 'E';
  bPassif := cbPassif.Checked;
  bPronominal := cbPronominal.Checked;
  if (LeftStr(Verbe.VerbeNom, 3) = 'se ') or (LeftStr(Verbe.VerbeNom, 2) = 's''') then begin
    // A verb starting with pronoun "se" is necessarily a pronominal verb
    cbVPronominal.Checked := True;
    cbAuxEtre.Checked := True;
    cbAuxAvoir.Checked := False;
    cbPronominal.Checked := True;
    cbPassif.Checked := False;
  end
  else
    // A verb not starting with pronoun "se" is considered not to be a pronominal verb
    cbVPronominal.Checked := False;
end;

{ Button "OK": Check user selections, fill in verb recoed (to be read by main unit) and close the form }

procedure TfUnknown.btOKClick(Sender: TObject);

var
  OK: Boolean;

// The routine detects most incorrect user selections and mostly auto-corrects them

begin
  sButton := 'ok'; OK := True;
  // Error if no auxiliaire selected
  if not (cbAuxAvoir.Checked or cbAuxEtre.Checked) then begin
    MessageDlg('Sélection invalide', 'Vous devez choisir un auxiliaire!', mtError, [mbOK], 0);
    OK := False;
  end
  // All other invalid selections will produce a warning (and a modification of these or other selections)
  else begin
    if cbVImpersonnel.Checked then begin
      if cbPassif.Checked or cbPronominal.Checked then begin
        if cbPassif.Checked then begin
          MessageDlg('Sélection invalide', 'Les verbes impersonnels n''ont pas de passif! Voix passive dé-sélectionnée.', mtWarning, [mbOK], 0);
          cbPassif.Checked := False;
        end;
        if cbPronominal.Checked then begin
          MessageDlg('Sélection invalide', 'Les verbes impersonnels ne sont pas pronominaux! Voix pronominale dé-sélectionnée.', mtWarning, [mbOK], 0);
          cbPronominal.Checked := False;
        end;
        OK := False;
      end;
    end;
    if cbVPronominal.Checked then begin
      if cbAuxAvoir.Checked or not cbAuxEtre.Checked then begin
        if not cbAuxEtre.Checked then
          MessageDlg('Sélection invalide', 'Les verbes pronominaux se conjuguent avec être! Auxiliaire être auto-sélectionné.', mtWarning, [mbOK], 0)
        else
          MessageDlg('Sélection invalide', 'Les verbes pronominaux se conjuguent avec être! Auxiliaire avoir dé-sélectionné.', mtWarning, [mbOK], 0);
        cbAuxEtre.Checked := True;
        cbAuxAvoir.Checked := False;
        OK := False;
      end;
    end;
    if cbAuxEtre.Checked and not cbAuxAvoir.Checked then begin
      if cbPassif.Checked then begin
        MessageDlg('Sélection invalide', 'Les verbes conjugués avec être n''ont pas de passif! Voix passive dé-sélectionnée.', mtWarning, [mbOK], 0);
        cbPassif.Checked := False;
        OK := False;
      end;
    end;
    if cbAuxEtre.Checked and not cbAuxAvoir.Checked then begin
      if not cbVPronominal.Checked and cbPronominal.Checked then begin
        MessageDlg('Sélection invalide', 'Les verbes conjugués avec être n''ont pas de voix pronominale! Voix pronominale dé-sélectionnée.', mtWarning, [mbOK], 0);
        cbPronominal.Checked := False;
        OK := False;
      end;
    end;
  end;
  // There is always a pronominal voice for ponominal verbs
  if cbVPronominal.Checked then
    cbPronominal.Checked := True;
  // Fill in the verb record (with "Conjugaison" set to '?')
  if OK then begin
    if cbVImpersonnel.Checked then
      Verbe.VerbeClass := 'I'
    else if cbVPronominal.Checked then
      Verbe.VerbeClass := 'P'
    else
      Verbe.VerbeClass := '-';
    if cbAuxAvoir.Checked and cbAuxEtre.Checked then
      Verbe.Auxiliaire := 'X'
    else if cbAuxAvoir.Checked then
      Verbe.Auxiliaire := 'A'
    else if cbAuxEtre.Checked then
      Verbe.Auxiliaire := 'E';
    Verbe.VoixPassive := cbPassif.Checked;
    Verbe.VoixPronominale := cbPronominal.Checked;
    Verbe.Conjugaison := '?';
    Close;
  end
  else
    sButton := '';
end;

{ Bouton "Cancel": Restore selections as they were before and close form }

procedure TfUnknown.btCancelClick(Sender: TObject);

begin
  sButton := 'cancel';
  case cVerbeClasse of
    'I': begin
           cbVImpersonnel.Checked := True; cbVPronominal.Checked := False;
         end;
    'P': begin
           cbVImpersonnel.Checked := False; cbVPronominal.Checked := True;
         end;
    '-': begin
           cbVImpersonnel.Checked := False; cbVPronominal.Checked := False;
         end;
  end;
  case cAuxiliaire of
    'A': begin
           cbAuxAvoir.Checked := True; cbAuxEtre.Checked := False;
         end;
    'E': begin
           cbAuxAvoir.Checked := False; cbAuxEtre.Checked := False;
         end;
    'X': begin
           cbAuxAvoir.Checked := True; cbAuxEtre.Checked := True;
         end;
  end;
  cbPassif.Checked := bPassif;
  cbPronominal.Checked := bPronominal;
  Close;
end;

{ "Verbe impersonnel" and "verbe pronominal" are exclusive to each other }

procedure TfUnknown.cbVImpersonnelChange(Sender: TObject);

begin
  if cbVImpersonnel.Checked then
    cbVPronominal.Checked := False;
end;

procedure TfUnknown.cbVPronominalChange(Sender: TObject);

begin
  if cbVPronominal.Checked then begin
    // Error message if "verbe pronominal" is checked for verbs that don't start with "se"
    if (LeftStr(Verbe.VerbeNom, 3) <> 'se ') and (LeftStr(Verbe.VerbeNom, 2) <> 's''') then begin
      if sButton <> 'cancel' then
        MessageDlg('Sélection invalide', 'Le verbe ' + Verbe.VerbeNom + ' n''est pas pronominal. Utilisez le pronom "se" svp.', mtError, [mbOK], 0);
      cbVPronominal.Checked := False;
    end;
  end
  else begin
    // Error message if "verbe pronominal" is unchecked for verbs that do start with "se"
    if (LeftStr(Verbe.VerbeNom, 3) = 'se ') or (LeftStr(Verbe.VerbeNom, 2) = 's''') then begin
      if sButton <> 'cancel' then
        MessageDlg('Sélection invalide', 'Les verbes commençant par "se" sont forcément pronominaux.', mtError, [mbOK], 0);
      cbVPronominal.Checked := True;
    end;
  end;
  if cbVPronominal.Checked then begin
    cbVImpersonnel.Checked := False;
  end;
end;

end.

