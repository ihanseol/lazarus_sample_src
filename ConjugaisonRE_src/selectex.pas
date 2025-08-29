{**************************************************************************************}
{* Unité "sélection groupe de verbes pour exercices" pour l'application ConjugaisonRE *}
{**************************************************************************************}

unit selectex;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {************}
  { TfSelectEx }
  {************}
  TfSelectEx = class(TForm)
    stVerbs: TStaticText;
    stVoix, stModes, stTemps, stFormes, stGenres: TStaticText;
    cbRegular, cbIrregular, cbSemiIrregular: TCheckBox;
    cbActif, cbPassif, cbPron: TCheckBox;
    cbInd, cbSubj, cbMOthers: TCheckBox;
    cbTSimp, cbTComp: TCheckBox;
    cbFAff, cbFNeg, cbFInt, cbFIntNeg: TCheckBox;
    cbMasc, cbFem: TCheckBox;
    btAll, btDefaults, btNone: TButton;
    btOK, btCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure stVerbsClick(Sender: TObject);
    procedure stVoixClick(Sender: TObject);
    procedure stModesClick(Sender: TObject);
    procedure stTempsClick(Sender: TObject);
    procedure stFormesClick(Sender: TObject);
    procedure stGenresClick(Sender: TObject);
    procedure btAllClick(Sender: TObject);
    procedure btDefaultsClick(Sender: TObject);
    procedure btNoneClick(Sender: TObject);
  private
    bCheckVerbs, bCheckVoix, bCheckModes, bCheckTemps, bCheckFormes, bCheckGenres: Boolean;
    bRegular, bIrregular, bSemiIrregular, bActif, bPassif, bPron, bInd, bSubj, bMOthers, bTSimp, bTComp: Boolean;
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
// Select = False, Defaults = True : select defaults
// Select = False, Defaults = False: unselect all

begin
  Sel := Select;
  if (Select = False) and (Defaults = True) then
    Sel := not Select;
  fSelectEx.cbRegular.Checked := Sel; fSelectEx.cbIrregular.Checked := Sel; fSelectEx.cbSemiIrregular.Checked := Sel;
  fSelectEx.cbActif.Checked := Sel; fSelectEx.cbPassif.Checked := Sel; fSelectEx.cbPron.Checked := Sel;
  fSelectEx.cbInd.Checked := Sel; fSelectEx.cbSubj.Checked := Sel; fSelectEx.cbMOthers.Checked := Sel;
  fSelectEx.cbTSimp.Checked := Sel; fSelectEx.cbTComp.Checked := Sel;
  fSelectEx.cbMasc.Checked := Sel; fSelectEx.cbFem.Checked := Sel;
  if (Select = False) and (Defaults = True) then begin
    fSelectEx.cbPassif.Checked := False; fSelectEx.cbPron.Checked := False;
  end;
  // "Forme" is not implemented in version 1.0!
  fSelectEx.cbFAff.Checked := True; fSelectEx.cbFNeg.Checked := False;
  fSelectEx.cbFInt.Checked := False; fSelectEx.cbFIntNeg.Checked := False;
end;

{************}
{ TfSelectEx }
{************}

{ Form creation: Set flags used for checked/unchecked toggles }

procedure TfSelectEx.FormCreate(Sender: TObject);

begin
  bCheckVerbs := True;
  bCheckVoix  := False; bCheckModes  := True; bCheckTemps := True;
  bCheckFormes := False; bCheckGenres := True;
end;

{ Form activation: Save actual selections }

procedure TfSelectEx.FormActivate(Sender: TObject);

begin
  bRegular := cbRegular.Checked; bSemiIrregular := cbSemiIrregular.Checked; bIrregular := cbIrregular.Checked;
  bActif := cbActif.Checked; bPassif := cbPassif.Checked; bPron := cbPron.Checked;
  bInd := cbInd.Checked; bSubj := cbSubj.Checked; bMOthers := cbMOthers.Checked; bTSimp := cbTSimp.Checked; bTComp := cbTComp.Checked;
  bFAff := cbFAff.Checked; bFNeg := cbFNeg.Checked; bFInt := cbFInt.Checked; bFIntNeg := cbFIntNeg.Checked;
  bMasc := cbMasc.Checked; bFem := cbFem.Checked;
end;

{ Button "OK": Check selections and close the form (values will be directly read from the controls) }

procedure TfSelectEx.btOKClick(Sender: TObject);

Var
  S: string;
  OK: Boolean;

begin
  S := '';
  OK := cbRegular.Checked or cbIrregular.Checked or cbSemiIrregular.Checked;
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
  if not OK then begin
    if S <> '' then
      MessageDlg('Sélection invalide', 'Vous avez oublié de choisir ' + S, mtError, [mbOK], 0);
  end
  else begin
    sButton := 'ok';
    Close;
  end;
end;

{ Button "Cancel": Restore selections and close form }

procedure TfSelectEx.btCancelClick(Sender: TObject);

begin
  cbRegular.Checked := bRegular; cbSemiIrregular.Checked := bSemiIrregular; cbIrregular.Checked := bIrregular;
  cbActif.Checked := bActif; cbPassif.Checked := bPassif; cbPron.Checked := bPron;
  cbInd.Checked := bInd; cbSubj.Checked := bSubj; cbMOthers.Checked := bMOthers; cbTSimp.Checked := bTSimp; cbTComp.Checked := bTComp;
  cbFAff.Checked := bFAff; cbFNeg.Checked := bFNeg; cbFInt.Checked := bFInt; cbFIntNeg.Checked := bFIntNeg;
  cbMasc.Checked := bMasc; cbFem.Checked := bFem;
  sButton := 'cancel';
  Close;
end;

{ Button "Sélectionnez défauts": Check default selections }

procedure TfSelectEx.btDefaultsClick(Sender: TObject);

begin
  Selection(False, True);
end;

{ Button "Sélectionnez tout": Check all selections }

procedure TfSelectEx.btAllClick(Sender: TObject);

begin
  Selection(True, False);
end;

{ Button "Désélectionnez tout": Uncheck all selections }

procedure TfSelectEx.btNoneClick(Sender: TObject);

begin
  Selection(False, False);
end;

{ Verb type check/uncheck toggling }

procedure TfSelectEx.stVerbsClick(Sender: TObject);

begin
  if bCheckVerbs then begin
    cbRegular.Checked := False; cbSemiIrregular.Checked := False; cbIrregular.Checked := False;
    bCheckVerbs := False;
  end
  else begin
    cbRegular.Checked := True; cbSemiIrregular.Checked := True; cbIrregular.Checked := True;
    bCheckVerbs := True;
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

