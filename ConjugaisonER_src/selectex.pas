{**************************************************************************************}
{* Unité "sélection groupe de verbes pour exercices" pour l'application ConjugaisonER *}
{**************************************************************************************}

unit selectex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  { TfSelectEx }
  TfSelectEx = class(TForm)
    StaticText1: TStaticText;
    laIrreg, laExcept: TLabel;
    cbRegular: TCheckBox;
    cbAller, cbEnvoyer: TCheckBox;
    cbCER, cbGER, cbELER, cbAYER, cbE_ER: TCheckBox;
    stVoix, stModes, stTemps, stFormes, stGenres: TStaticText;
    cbActif, cbPassif, cbPron: TCheckBox;
    cbInd, cbSubj, cbMOthers: TCheckBox;
    cbTSimp, cbTComp: TCheckBox;
    cbFAff, cbFNeg, cbFInt, cbFIntNeg: TCheckBox;
    cbMasc, cbFem: TCheckBox;
    btAll, btDefaults, btNone: TButton;
    btOK: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure laIrregClick(Sender: TObject);
    procedure laExceptClick(Sender: TObject);
    procedure stFormesClick(Sender: TObject);
    procedure stVoixClick(Sender: TObject);
    procedure stModesClick(Sender: TObject);
    procedure stTempsClick(Sender: TObject);
    procedure stGenresClick(Sender: TObject);
    procedure btAllClick(Sender: TObject);
    procedure btDefaultsClick(Sender: TObject);
    procedure btNoneClick(Sender: TObject);
  private
    bCheckIrreg, bCheckExcept, bCheckVoix, bCheckModes, bCheckTemps, bCheckFormes, bCheckGenres: Boolean;
    bRegular, bAller, bEnvoyer, bCER, bGER, bELER, bAYER, bE_ER: Boolean;
    bActif, bPassif, bPron, bInd, bSubj, bMOthers, bTSimp, bTComp: Boolean;
    bFAff, bFNeg, bFInt, bFIntNeg, bMasc, bFem: Boolean;
  public
    sButton: string;
  end;

var
  fSelectEx: TfSelectEx;

implementation

{$R *.lfm}

{ Check/uncheck the various selections (as result of user "toggle-label" click) }

procedure Selection(Select, Defaults: Boolean);

var
  Sel: Boolean;

// Select = True : select all
// Select = False, Defaults = True:  select defaults
// Select = False, Defaults = False: unselect all

begin
  Sel := Select;
  if (Select = False) and (Defaults = True) then
    Sel := not Select;
  fSelectEx.cbRegular.Checked := Sel;
  fSelectEx.cbAller.Checked   := Sel; fSelectEx.cbEnvoyer.Checked := Sel;
  fSelectEx.cbCER.Checked     := Sel; fSelectEx.cbE_ER.Checked    := Sel; fSelectEx.cbGER.Checked     := Sel;
  fSelectEx.cbELER.Checked    := Sel; fSelectEx.cbAYER.Checked    := Sel;
  fSelectEx.cbActif.Checked   := Sel; fSelectEx.cbPassif.Checked  := Sel; fSelectEx.cbPron.Checked    := Sel;
  fSelectEx.cbInd.Checked     := Sel; fSelectEx.cbSubj.Checked    := Sel; fSelectEx.cbMOthers.Checked := Sel;
  fSelectEx.cbTSimp.Checked   := Sel; fSelectEx.cbTComp.Checked   := Sel;
  fSelectEx.cbMasc.Checked    := Sel; fSelectEx.cbFem.Checked     := Sel;
  if (Select = False) and (Defaults = True) then begin
    fSelectEx.cbPassif.Checked  := False; fSelectEx.cbPron.Checked    := False;
  end;
  // "Forme" is not implemented in version 1.0!
  fSelectEx.cbFAff.Checked    := True;   fSelectEx.cbFNeg.Checked    := False;
  fSelectEx.cbFInt.Checked    := False;  fSelectEx.cbFIntNeg.Checked := False;
end;

{**************}
{* TfSelectEx *}
{**************}

{ Form creation: set flags used for checked/unchecked toggles }

procedure TfSelectEx.FormCreate(Sender: TObject);

begin
  bCheckIrreg := True;  bCheckExcept := True;
  bCheckVoix  := False; bCheckModes  := True; bCheckTemps := True;
  bCheckFormes := False; bCheckGenres := True;
end;

{ Form activation: save actual selections }

procedure TfSelectEx.FormActivate(Sender: TObject);

begin
  bRegular := cbRegular.Checked; bAller := cbAller.Checked; bEnvoyer := cbEnvoyer.Checked;
  bCER := cbCER.Checked;  bGER := cbGER.Checked; bELER := cbELER.Checked; bAYER := cbAYER.Checked; bE_ER := cbE_ER.Checked;
  bActif := cbActif.Checked; bPassif := cbPassif.Checked; bPron := cbPron.Checked;
  bInd := cbInd.Checked; bSubj := cbSubj.Checked; bMOthers := cbMOthers.Checked; bTSimp := cbTSimp.Checked; bTComp := cbTComp.Checked;
  bFAff := cbFAff.Checked; bFNeg := cbFNeg.Checked; bFInt := cbFInt.Checked; bFIntNeg := cbFIntNeg.Checked;
  bMasc := cbMasc.Checked; bFem := cbFem.Checked;
end;

{ Button "OK": check selections and close the form (values will be directly read from the controls) }

procedure TfSelectEx.btOKClick(Sender: TObject);

Var
  S: string;
  OK: Boolean;

begin
  S := '';
  OK := cbRegular.Checked or cbAller.Checked or cbEnvoyer.Checked;
  OK := OK or cbCER.Checked or cbE_ER.Checked or cbGER.Checked or cbELER.Checked or cbAYER.Checked;
  if not OK then
    S := 'les verbes!'
  else begin
    OK := cbActif.Checked or cbPassif.Checked or cbPron.Checked;
    if not OK then
      S := 'les voix!'
    else begin
      OK := cbInd.Checked or cbSubj.Checked or cbMOthers.Checked;
      if not OK then
        S := 'les modes!'
      else begin
        OK := cbFAff.Checked or cbFNeg.Checked or cbFInt.Checked or cbFIntNeg.Checked;
        if not OK then
          S := 'les formes!'
        else begin
          OK := cbTSimp.Checked or cbTComp.Checked;
          if not OK then
            S := 'les temps!'
          else begin
            OK := cbMasc.Checked or cbFem.Checked;
            if not OK then
              S := 'les genres!'
          end;
        end;
      end;
    end;
  end;
  if OK then begin
    if cbAller.Checked then begin
      if not (cbRegular.Checked or cbEnvoyer.Checked or cbCER.Checked or cbE_ER.Checked or cbGER.Checked or cbELER.Checked or cbAYER.Checked) then begin
        if cbPassif.Checked then begin
          MessageDlg('Sélection invalide', 'Aller ne se conjugue pas avec la voix passive! Voix passive désélectionnée.', mtWarning, [mbOK], 0);
          OK := False; S := '';
          cbPassif.Checked := False;
        end
      end;
    end
  end;
  if not OK then begin
    if S <> '' then
      MessageDlg('Sélection invalide', 'Vous avez oublié de choisir ' + S, mtError, [mbOK], 0);
  end
  else begin
    sButton := 'ok';
    Close;
  end;
end;

{ Button "Cancel": restore selections and close form }

procedure TfSelectEx.btCancelClick(Sender: TObject);

begin
  cbRegular.Checked := bRegular; cbAller.Checked := bAller; cbEnvoyer.Checked := bEnvoyer;
  cbCER.Checked := bCER;  cbGER.Checked := bGER; cbELER.Checked := bELER; cbAYER.Checked := bAYER; cbE_ER.Checked := bE_ER;
  cbActif.Checked := bActif; cbPassif.Checked := bPassif; cbPron.Checked := bPron;
  cbInd.Checked := bInd; cbSubj.Checked := bSubj; cbMOthers.Checked := bMOthers; cbTSimp.Checked := bTSimp; cbTComp.Checked := bTComp;
  cbFAff.Checked := bFAff; cbFNeg.Checked := bFNeg; cbFInt.Checked := bFInt; cbFIntNeg.Checked := bFIntNeg;
  cbMasc.Checked := bMasc; cbFem.Checked := bFem;
  sButton := 'cancel';
  Close;
end;

{ Button "Sélectionnez défauts": check default selections }

procedure TfSelectEx.btDefaultsClick(Sender: TObject);

begin
  Selection(False, True);
end;

{ Button "Sélectionnez tout": check all selections }

procedure TfSelectEx.btAllClick(Sender: TObject);

begin
  Selection(True, False);
end;

{ Button "Désélectionnez tout": uncheck all selections }

procedure TfSelectEx.btNoneClick(Sender: TObject);

begin
  Selection(False, False);
end;

{ "Verbes irréguliers" and "Cas spéciaux" check/uncheck toggling }

procedure TfSelectEx.laIrregClick(Sender: TObject);

begin
  if bCheckIrreg then begin
    cbAller.Checked := False; cbEnvoyer.Checked := False;
    bCheckIrreg := False;
  end
  else begin
    cbAller.Checked := True; cbEnvoyer.Checked := True;
    bCheckIrreg := True;
  end;
end;

procedure TfSelectEx.laExceptClick(Sender: TObject);

begin
  if bCheckExcept then begin
    cbCER.Checked  := False; cbE_ER.Checked := False; cbGER.Checked := False;
    cbELER.Checked := False; cbAYER.Checked := False;
    bCheckExcept := False;
  end
  else begin
    cbCER.Checked  := True; cbE_ER.Checked := True; cbGER.Checked := True;
    cbELER.Checked := True; cbAYER.Checked := True;
    bCheckExcept := True;
  end;
end;

{ "Voix", "Mode" and "Temps" check/uncheck toggling }

procedure TfSelectEx.stVoixClick(Sender: TObject);

begin
  if bCheckVoix then begin
    cbActif.Checked := False; cbPassif.Checked := False; cbPron.Checked := False;
    bCheckVoix := False;
  end
  else begin
    cbActif.Checked := True; cbPassif.Checked := True; cbPron.Checked := True;
    bCheckVoix := True;
  end;
end;

procedure TfSelectEx.stModesClick(Sender: TObject);

begin
  if bCheckModes then begin
    cbInd.Checked := False; cbSubj.Checked := False; cbMOthers.Checked := False;
    bCheckModes := False;
  end
  else begin
    cbInd.Checked := True; cbSubj.Checked := True; cbMOthers.Checked := True;
    bCheckModes := True;
  end;
end;

procedure TfSelectEx.stTempsClick(Sender: TObject);

begin
  if bCheckTemps then begin
    cbTSimp.Checked := False; cbTComp.Checked := False;
    bCheckTemps := False;
  end
  else begin
    cbTSimp.Checked := True; cbTComp.Checked := True;
    bCheckTemps := True;
  end;
end;

{ "Forme" and "Genre" check/uncheck toggling }

procedure TfSelectEx.stFormesClick(Sender: TObject);

begin
  // "Forme" not implemented in this version (always "forme affirmative")
end;

procedure TfSelectEx.stGenresClick(Sender: TObject);

begin
  if bCheckGenres then begin
    cbMasc.Checked := False; cbFem.Checked := False;
    bCheckGenres := False;
  end
  else begin
    cbMasc.Checked := True; cbFem.Checked := True;
    bCheckGenres := True;
  end;
end;

end.

