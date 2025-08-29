{******************************************************************}
{* Unité "affichage conjugaison" pour l'application ConjugaisonER *}
{******************************************************************}

unit conj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles, LazUTF8, common;

type
  TEditInd  = array[1..5, 1..6] of TEdit;
  TEditSubj = array[1..2, 1..6] of TEdit;
  { TfConj }
  TfConj = class(TForm)
    stTitle: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6: TLabel;
    laInfinitif, laParticipe, laParticipePasse, laImperatif: TLabel;
    laIndPres, laIndImpf, laIndPass, laIndFut, laIndCond: TLabel;
    laSubjPres, laSubjImpf: TLabel;
    edInfinitif, edParticipe, edParticipePasse: TEdit;
    edImperatif12, edImperatif21, edImperatif22: TEdit;
    edIndPres11, edIndPres12, edIndPres13, edIndPres21, edIndPres22, edIndPres23: TEdit;
    edIndImpf11, edIndImpf12, edIndImpf13, edIndImpf21, edIndImpf22, edIndImpf23: TEdit;
    edIndPass11, edIndPass12, edIndPass13, edIndPass21, edIndPass22, edIndPass23: TEdit;
    edIndFut11, edIndFut12, edIndFut13, edIndFut21, edIndFut22, edIndFut23: TEdit;
    edIndCond11, edIndCond12, edIndCond13, edIndCond21, edIndCond22, edIndCond23: TEdit;
    edSubjPres11, edSubjPres12, edSubjPres13, edSubjPres21, edSubjPres22, edSubjPres23: TEdit;
    edSubjImpf11, edSubjImpf12, edSubjImpf13, edSubjImpf21, edSubjImpf22, edSubjImpf23: TEdit;
    btActif, btPassif, btPronominal: TButton;
    btTSimples, btTComposes: TButton;
    btAffirmatif, btNegatif, btInterrogatif, btIntNegatif: TButton;
    btMasculin, btFeminin: TButton;
    btClose: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btActifClick(Sender: TObject);
    procedure btPassifClick(Sender: TObject);
    procedure btPronominalClick(Sender: TObject);
    procedure btAffirmatifClick(Sender: TObject);
    procedure btNegatifClick(Sender: TObject);
    procedure btInterrogatifClick(Sender: TObject);
    procedure btIntNegatifClick(Sender: TObject);
    procedure btTSimplesClick(Sender: TObject);
    procedure btTComposesClick(Sender: TObject);
    procedure btMasculinClick(Sender: TObject);
    procedure btFemininClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    sVoix, sForme, sTemps, sGenre: string;
    bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept: Boolean;
    Verbe, VerbeType: TVerbe;
    AvoirFile, EtreFile, VerbFile: TINIFile;
  end;

var
  EditInd: TEditInd;
  EditSubj: TEditSubj;
  fConj: TfConj;

implementation

{$R *.lfm}

{ Set labels, depending if tense is simple or compound }

procedure SetLabels(TempsType: string);

var
  Temps: TTemps;

begin
  if TempsType = 'simples' then
    Temps := TempsSimples
  else
    Temps := TempsComposes;
  fConj.laInfinitif.Caption := Modes[1] + ' ' + Temps[1, 1];
  fConj.laParticipe.Caption := Modes[2] + ' ' + Temps[2, 1];
  if TempsType = 'simples' then begin
    // Two smaller fields
    fConj.laParticipePasse.Caption := Modes[2] + ' ' + Temps[2, 2];
    fConj.laParticipePasse.Visible := True;
    fConj.edParticipe.Width := 150;
    fConj.edParticipePasse.Visible := True;
  end
  else begin
    // One larger field
    fConj.laParticipePasse.Visible := False;
    fConj.edParticipe.Width := 272;
    fConj.edParticipePasse.Visible := False;
  end;
  fConj.laImperatif.Caption := Modes[3] + ' ' + Temps[3, 1];
  fConj.laIndPres.Caption := Temps[4, 1]; fConj.laIndImpf.Caption := Temps[4, 2]; fConj.laIndPass.Caption := Temps[4, 3];
  fConj.laIndFut.Caption  := Temps[4, 4]; fConj.laIndCond.Caption := Temps[4, 5];
  fConj.laSubjPres.Caption := Temps[5, 1]; fConj.laSubjImpf.Caption := Temps[5, 2];
end;

{ Adapt seperators between verb parts for display as selected on the main form ("Options > Affichage" menu) }

procedure AdaptSeparators(var VConjugue: string; SepTerm, SepRacine: Boolean);

var
  NSep, P: Integer;

begin
  // Not existing conjugated verb form
  if VConjugue = 'n/a' then
    VConjugue := '-----'
  // Normal case (conjugated verb form exists)
  else begin
    NSep := 1;
    P := Pos('--', VConjugue);
    P := Pos('--', Copy(VConjugue, P + 2, Length(VConjugue)));
    if P > 0 then
      NSep := 2;
    // The conjugated verb as returned by the conjugation routines contains 1 (normal case)
    // or 2 ("futur" and "conditionnel") "--" seperators. Depending on the seperators display
    // options, those are removed or replaced by '-'
    if SepTerm and SepRacine then
      VConjugue := StringReplace(VConjugue, '--', '-', [rfReplaceAll])
    else if SepTerm then begin
      if NSep = 2 then
        VConjugue := StringReplace(VConjugue, '--', '', []);
      VConjugue := StringReplace(VConjugue, '--', '-', []);
    end
    else
      VConjugue := StringReplace(VConjugue, '--', '', [rfReplaceAll]);
  end;
end;

{ Color display of irregularities and/or exceptions }

function FontColor(Verbe: TVerbe; VerbeConj: string; ColorIrreg, ColorExcept: Boolean): TColor;

const
  clOrange = $000080FF;

var
  L, P, I: Integer;
  Racine, Radical, S: string;
  IrregColor: Boolean;
  Colour: TColor;

// The routine returns the color: black = regular conjugation, red = irregularity, orange = exception

begin
  Colour := clDefault;
  if (VerbeConj <> 'n/a') and (RightStr(Verbe.VerbeNom, 2) = 'er') then begin
    // Color display only for -er verbs (not "avoir" and "être")
    if ColorIrreg or ColorExcept then begin
      // Determine "racine" and "radical" of the conjugated verb form
      Racine := UTF8Copy(Verbe.VerbeNom, 1, UTF8Length(Verbe.VerbeNom) - 2);
      Radical := '';
      VerbeConj := StringReplace(VerbeConj, '-t''en', '', []);
      VerbeConj := StringReplace(VerbeConj, '-nous en', '', []);
      VerbeConj := StringReplace(VerbeConj, '-vous en', '', []);
      I := Length(VerbeConj);
      repeat
        S := Copy(VerbeConj, I, 1);
        if (S <> ' ') and (S <> '''') then
          Radical := S + Radical;
        Dec(I);
      until (S = ' ') or (S = '''') or (I = 0);
      P := Pos('--', Radical);
      if P <> 0 then
        Radical := LeftStr(Radical, P - 1);
      if Verbe.VerbeNom = 'en aller' then
        Radical := 'en ' + Radical;
      if Verbe.VerbeNom = 'y aller' then
        Radical := 'y ' + Radical;
      // If "racine" and "radical" are different, determine on how much characters (start of verb) this is the case
      if Racine <> Radical then begin
        if UTF8Length(Racine) < UTF8Length(Radical) then
          L := UTF8Length(Racine) - 1
        else
          L := UTF8Length(Radical) - 1;
        if L < 2 then
          L := 2;
        IrregColor := False;
        // The algorithm will not work with all French words, but works fine with -er verbs. General principle:
        // If the "racine"-"radical" difference is on a minimum length (depending on the total length of the verb),
        // it is considered that the display has to be colored. If the verb is known as irregular, red color will
        // be used, if not display will be in orange. Colored output is overwritten by black if colored display is
        // unchecked in the main form's "Options > Affichage" menu.
        if (UpperCase(Verbe.Conjugaison) = 'I') then begin
          if (UTF8Copy(Racine, 1, L) <> UTF8Copy(Radical, 1, L)) then begin
            IrregColor := True;
            If ColorIrreg then
              Colour := clRed;
          end;
        end;
        if ColorExcept and not IrregColor then
          Colour := clOrange;
      end;
    end;
  end;
  FontColor := Colour;
end;

{ Conjugation of given verb using selectd voice, tense group... and fill in of the conjugation display form fields }

procedure Conjuguer(Verbe, VerbeType: TVerbe; Voix, TempsType, Forme, Genre: string;
  Reforme1990, KeepY, SepTerm, SepRacine, ColorIrreg, ColorExcept: Boolean; var AvoirFile, EtreFile, VerbFile: TINIFile);

var
  M, T, N, P, PE, I: Integer;
  VerbeNom, VerbeConjugue, ConjVoix, Mode, Temps, Nombre, Personne, VerbeConj, S: string;

begin
  VerbeNom := Verbe.VerbeNom;
  // Explicit title of what exactly represents the conjugation shown
  if Verbe.VerbeClass = 'P' then
    // Pronominal verb starting with pronoun "se"
    VerbeNom := DoubleVoyelle('se', UTF8Copy(VerbeNom, 1, 1)) + VerbeNom;
  S := 'Conjugaison du verbe "' + VerbeNom + '"';
  if Voix = 'passif' then
    S += ' au passif'
  else if Voix = 'pronominal' then
    S += ' à la voix pronominale';
  if Forme <> 'affirmative' then begin
    S += ' (forme ' + Forme;
    if Genre = 'masculin' then
      S += ')';
  end;
  if Genre = 'féminin' then begin
    if Forme = 'affirmative' then
      S += ' (féminin)'
    else
      S += '; féminin)';
  end;
  fConj.stTitle.Caption := S;
  // Change labels depending on simlple/compound tense group
  SetLabels(TempsType);
  ConjVoix := Voix;
  // Use "pronominal" (instead of "actif") to conjugate a pronominal-only verb
  if Verbe.VerbeClass = 'P' then
    ConjVoix := 'pronominal';
  // Determine the conjugated verb form for all modes, tenses, numbers and persons
  // (with the voice, form and sex as actually selected). Adapt seperators and color
  // for display as selected and write the conjugated verb form into the correct edit field
  for M := 1 to NModes do begin
    Mode := Modes[M];
    for T := 1 to NTemps[M] do begin
      if TempsType = 'simples' then
        Temps := TempsSimples[M, T]
      else
        Temps := TempsComposes[M, T];
      // "Infinitif" and "Participe"
      if (Mode = 'infinitif') or (Mode = 'participe') then begin
        VerbeConjugue := Conjugaison(Verbe, VerbeType, ConjVoix, Mode, Temps, Forme, '-', '-', Genre, Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
        VerbeConj := VerbeConjugue;
        AdaptSeparators(VerbeConjugue, SepTerm, SepRacine);
        if Mode = 'infinitif' then begin
          fConj.edInfinitif.Text := VerbeConjugue; fConj.edInfinitif.Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept);
        end
        else begin
          if (Temps = 'présent') or (Temps = 'passé composé') then begin
            fConj.edParticipe.Text := VerbeConjugue; fConj.edParticipe.Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept);
          end
          else if Temps = 'passé' then begin
            fConj.edParticipePasse.Text := VerbeConjugue; fConj.edParticipePasse.Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept);
          end;
        end;
      end
      // Impératif
      else if Mode = 'impératif' then begin
        for I := 1 to 3 do begin
          case I of
            1: begin
                 N := 1; P := 2;
               end;
            2: begin
                 N := 2; P := 1;
               end;
            3: begin
                 N := 2; P := 2;
               end;
          end;
          Nombre := Nombres[N]; Personne := Personnes[P];
          VerbeConjugue := Conjugaison(Verbe, VerbeType, ConjVoix, Mode, Temps, Forme, Nombre, Personne, Genre, Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
          VerbeConj := VerbeConjugue;
          AdaptSeparators(VerbeConjugue, SepTerm, SepRacine);
          case I of
            1: begin fConj.edImperatif12.Text := VerbeConjugue; fConj.edImperatif12.Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept); end;
            2: begin fConj.edImperatif21.Text := VerbeConjugue; fConj.edImperatif21.Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept); end;
            3: begin fConj.edImperatif22.Text := VerbeConjugue; fConj.edImperatif22.Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept); end;
          end;
        end;
      end
      // "Indicatif" and "Subjonctif"
      else begin
        for N := 1 to NNombres do begin
          Nombre := Nombres[N];
          for P := 1 to NPersonnes do begin
            Personne := Personnes[P];
            VerbeConjugue := Conjugaison(Verbe, VerbeType, ConjVoix, Mode, Temps, Forme, Nombre, Personne, Genre, Reforme1990, KeepY, AvoirFile, EtreFile, VerbFile);
            VerbeConj := VerbeConjugue;
            AdaptSeparators(VerbeConjugue, SepTerm, SepRacine);
            PE := (N - 1) * 3 + P;
            if Mode = 'indicatif' then begin
              EditInd[T, PE].Text := VerbeConjugue; EditInd[T, PE].Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept);
            end
            else begin
              EditSubj[T, PE].Text := VerbeConjugue; EditSubj[T, PE].Font.Color := FontColor(Verbe, VerbeConj, ColorIrreg, ColorExcept);
            end;
          end;
        end;
      end;
    end;
  end;
end;

{**********}
{* TfConj *}
{**********}

{ Form creation: create array with "indicatif" and "subjonctif" edit fields }

procedure TfConj.FormCreate(Sender: TObject);

begin
  EditInd[1, 1]  := edIndPres11;  EditInd[1, 2]  := edIndPres12;  EditInd[1, 3]  := edIndPres13;
  EditInd[1, 4]  := edIndPres21;  EditInd[1, 5]  := edIndPres22;  EditInd[1, 6]  := edIndPres23;
  EditInd[2, 1]  := edIndImpf11;  EditInd[2, 2]  := edIndImpf12;  EditInd[2, 3]  := edIndImpf13;
  EditInd[2, 4]  := edIndImpf21;  EditInd[2, 5]  := edIndImpf22;  EditInd[2, 6]  := edIndImpf23;
  EditInd[3, 1]  := edIndPass11;  EditInd[3, 2]  := edIndPass12;  EditInd[3, 3]  := edIndPass13;
  EditInd[3, 4]  := edIndPass21;  EditInd[3, 5]  := edIndPass22;  EditInd[3, 6]  := edIndPass23;
  EditInd[4, 1]  := edIndFut11;   EditInd[4, 2]  := edIndFut12;   EditInd[4, 3]  := edIndFut13;
  EditInd[4, 4]  := edIndFut21;   EditInd[4, 5]  := edIndFut22;   EditInd[4, 6]  := edIndFut23;
  EditInd[5, 1]  := edIndCond11;  EditInd[5, 2]  := edIndCond12;  EditInd[5, 3]  := edIndCond13;
  EditInd[5, 4]  := edIndCond21;  EditInd[5, 5]  := edIndCond22;  EditInd[5, 6]  := edIndCond23;
  EditSubj[1, 1] := edSubjPres11; EditSubj[1, 2] := edSubjPres12; EditSubj[1, 3] := edSubjPres13;
  EditSubj[1, 4] := edSubjPres21; EditSubj[1, 5] := edSubjPres22; EditSubj[1, 6] := edSubjPres23;
  EditSubj[2, 1] := edSubjImpf11; EditSubj[2, 2] := edSubjImpf12; EditSubj[2, 3] := edSubjImpf13;
  EditSubj[2, 4] := edSubjImpf21; EditSubj[2, 5] := edSubjImpf22; EditSubj[2, 6] := edSubjImpf23;
end;

{ Form activation: Do conjugation of verb selected by user }

procedure TfConj.FormActivate(Sender: TObject);

// Defaults for "voix", "temps simples/conjugués", etc are set by the main unit

begin
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

{ Buttons "Voix active", "Voix passive", "Voix pronominale": Conjugation with voice selected }

procedure TfConj.btActifClick(Sender: TObject);

begin
  sVoix := 'actif';
  btActif.Font.Style := [fsBold]; btPassif.Font.Style := []; btPronominal.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btPassifClick(Sender: TObject);

// For verbs without "voix passive", the button has been disabled by the main unit

begin
  sVoix := 'passif';
  btActif.Font.Style := []; btPassif.Font.Style := [fsBold]; btPronominal.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btPronominalClick(Sender: TObject);

// For verbs without "voix pronominale", the button has been disabled by the main unit

begin
  sVoix := 'pronominal';
  btActif.Font.Style := []; btPassif.Font.Style := []; btPronominal.Font.Style := [fsBold];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

{ Buttons "Forme affirmative", "Forme négative"...": Conjugation with form selected - not implemented in this version! }

procedure TfConj.btAffirmatifClick(Sender: TObject);

begin

end;

procedure TfConj.btNegatifClick(Sender: TObject);

begin

end;

procedure TfConj.btInterrogatifClick(Sender: TObject);

begin

end;

procedure TfConj.btIntNegatifClick(Sender: TObject);

begin

end;

{ Buttons "Temps simples", "Temps composés"": Conjugation with tense group selected }

procedure TfConj.btTSimplesClick(Sender: TObject);

begin
  sTemps := 'simples';
  btTSimples.Font.Style := [fsBold]; btPassif.Font.Style := []; btTComposes.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btTComposesClick(Sender: TObject);

begin
  sTemps := 'composés';
  btTSimples.Font.Style := []; btPassif.Font.Style := []; btTComposes.Font.Style := [fsBold];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

{ Buttons "Masculin", "Féminin"": Conjugation with sex selected }

procedure TfConj.btMasculinClick(Sender: TObject);

begin
  sGenre := 'masculin';
  btMasculin.Font.Style := [fsBold]; btFeminin.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btFemininClick(Sender: TObject);

begin
  sGenre := 'féminin';
  btMasculin.Font.Style := []; btFeminin.Font.Style := [fsBold];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bReforme1990, bKeepY, bSepTerm, bSepRacine, bColorIrreg, bColorExcept, AvoirFile, EtreFile, VerbFile);
end;

{ Button "Fermer": Close the form }

procedure TfConj.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

