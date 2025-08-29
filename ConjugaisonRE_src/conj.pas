{******************************************************************}
{* Unité "affichage conjugaison" pour l'application ConjugaisonRE *}
{******************************************************************}

unit conj;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, IniFiles, LazUTF8, common;

type
  TEditInd  = array[1..5, 1..6] of TEdit;
  TEditSubj = array[1..2, 1..6] of TEdit;
  {********}
  { TfConj }
  {********}
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
    procedure FormCreate(Sender: TObject);
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
  public
    sVoix, sForme, sTemps, sGenre: string;
    bSepTerm, bSepRacine, bColorIrreg, bColorSame: Boolean;
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

// The routine also removes the "irregularity tags" at the end of the conjugated verb form

begin
  // Not existing conjugated verb form
  if VConjugue = 'n/a' then
    VConjugue := '-----'
  // Normal case (conjugated verb form exists)
  else begin
    // Remove "irregularity tags"
    VConjugue := StringReplace(VConjugue, '!', '', []);
    VConjugue := StringReplace(VConjugue, '$', '', []);
    VConjugue := StringReplace(VConjugue, '^', '', []);
    VConjugue := StringReplace(VConjugue, '*', '', []);
    VConjugue := StringReplace(VConjugue, '+', '', []);
    // Adapt separators
    NSep := 1;
    P := Pos('--', VConjugue);
    P := Pos('--', Copy(VConjugue, P + 2, Length(VConjugue)));
    if P > 0 then
      NSep := 2;
    // The conjugated verb as returned by the conjugation routines contains 1 (normal case)
    // or 2 ("radical suffix" case) "--" seperators. Depending on the seperators display
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

{ Color display of irregularities }

function FontColor(Verbe: TVerbe; Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj: string; ColorIrreg, ColorSame: Boolean): TColor;

const
  clDarkOrange  = $007FFF;
  clDeepPink    = $9314FF;
  clDarkRed     = $00008B;
  clAquamarine3 = $AACD66;

var
  Colour: TColor;

// Color used depends on "irregularity tag" at the end of the conjugated verb form

begin
  Colour := clDefault;
  if (VerbeConj <> 'n/a') and (((Voix <> 'passif') and (TempsType = 'simples')) or ((Voix = 'passif') and (Mode = 'participe') and (Temps = 'passé'))) then begin
    // Conjugated verb forms for compound tenses or passive voice always default color
    if ColorIrreg then begin
      if Verbe.Conjugaison = '-' then begin
        // Regular verbs
        if RightStr(Verbe.VerbeNom, 3) = 'dre' then begin
          // Special case of verbs in -dre
          if (Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier') and (Personne = '3e personne') then
            Colour := clBlue;
        end
        else if RightStr(Verbe.VerbeNom, 4) = 'ttre' then begin
          // Special case of verbs in -ttre
          if ((Mode = 'indicatif') and (Temps = 'présent') and (Nombre = 'singulier')) or
             ((Mode = 'impératif') and (Temps = 'présent') and (Nombre = 'singulier')) then
            Colour := clBlue;
        end;
      end;
      if UpperCase(Verbe.Conjugaison) = 'S' then begin
        // Semi-irregular verbs
        if RightStr(VerbeConj, 1) = '!' then
          Colour := clFuchsia
        else if RightStr(VerbeConj, 1) = '*' then
          Colour := clGreen
        else if RightStr(VerbeConj, 1) = '^' then
          Colour := clAquamarine3
        else if RightStr(VerbeConj, 1) = '$' then
          Colour := clLime
        else if RightStr(VerbeConj, 1) = '+' then
          Colour := clBlue;
      end
      else if UpperCase(Verbe.Conjugaison) = 'I' then begin
        // Irregular verbs
        if RightStr(VerbeConj, 1) = '!' then
          Colour := clRed
        else if RightStr(VerbeConj, 1) = '*' then
          Colour := clDarkRed
        else if RightStr(VerbeConj, 1) = '^' then
          Colour := clDeepPink
        else if RightStr(VerbeConj, 1) = '$' then
          Colour := clDarkOrange
        else if RightStr(VerbeConj, 1) = '+' then
          Colour := clBlue;
      end;
    end;
  end;
  // Use always the same color (if this option has been selected)
  if (Colour <> clDefault) and ColorSame then
    Colour := clRed;
  FontColor := Colour;
end;

{ Conjugation of given verb using selectd voice, tense group... and fill in of the conjugation display form fields }

procedure Conjuguer(Verbe, VerbeType: TVerbe; Voix, TempsType, Forme, Genre: string;
  SepTerm, SepRacine, ColorIrreg, ColorExcept: Boolean; var AvoirFile, EtreFile, VerbFile: TINIFile);

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
        VerbeConjugue := Conjugaison(Verbe, VerbeType, ConjVoix, Mode, Temps, Forme, '-', '-', Genre, AvoirFile, EtreFile, VerbFile);
        VerbeConj := VerbeConjugue;
        AdaptSeparators(VerbeConjugue, SepTerm, SepRacine);
        if Mode = 'infinitif' then begin
          fConj.edInfinitif.Text := VerbeConjugue; fConj.edInfinitif.Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept);
        end
        else begin
          if (Temps = 'présent') or (Temps = 'passé composé') then begin
            fConj.edParticipe.Text := VerbeConjugue; fConj.edParticipe.Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept);
          end
          else if Temps = 'passé' then begin
            fConj.edParticipePasse.Text := VerbeConjugue; fConj.edParticipePasse.Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept);
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
          VerbeConjugue := Conjugaison(Verbe, VerbeType, ConjVoix, Mode, Temps, Forme, Nombre, Personne, Genre, AvoirFile, EtreFile, VerbFile);
          VerbeConj := VerbeConjugue;
          AdaptSeparators(VerbeConjugue, SepTerm, SepRacine);
          case I of
            1: begin fConj.edImperatif12.Text := VerbeConjugue; fConj.edImperatif12.Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept); end;
            2: begin fConj.edImperatif21.Text := VerbeConjugue; fConj.edImperatif21.Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept); end;
            3: begin fConj.edImperatif22.Text := VerbeConjugue; fConj.edImperatif22.Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept); end;
          end;
        end;
      end
      // "Indicatif" and "Subjonctif"
      else begin
        for N := 1 to NNombres do begin
          Nombre := Nombres[N];
          for P := 1 to NPersonnes do begin
            Personne := Personnes[P];
            VerbeConjugue := Conjugaison(Verbe, VerbeType, ConjVoix, Mode, Temps, Forme, Nombre, Personne, Genre, AvoirFile, EtreFile, VerbFile);
            VerbeConj := VerbeConjugue;
            AdaptSeparators(VerbeConjugue, SepTerm, SepRacine);
            PE := (N - 1) * 3 + P;
            if Mode = 'indicatif' then begin
              EditInd[T, PE].Text := VerbeConjugue; EditInd[T, PE].Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept);
            end
            else begin
              EditSubj[T, PE].Text := VerbeConjugue; EditSubj[T, PE].Font.Color := FontColor(Verbe, Voix, Mode, TempsType, Temps, Nombre, Personne, VerbeConj, ColorIrreg, ColorExcept);
            end;
          end;
        end;
      end;
    end;
  end;
end;

{********}
{ TfConj }
{********}

{ Form creation: Create array with "indicatif" and "subjonctif" edit fields }

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
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

{ Buttons "Voix active", "Voix passive", "Voix pronominale": Conjugation with voice selected }

procedure TfConj.btActifClick(Sender: TObject);

begin
  sVoix := 'actif';
  btActif.Font.Style := [fsBold]; btPassif.Font.Style := []; btPronominal.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btPassifClick(Sender: TObject);

// For verbs without "voix passive", the button has been disabled by the main unit

begin
  sVoix := 'passif';
  btActif.Font.Style := []; btPassif.Font.Style := [fsBold]; btPronominal.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btPronominalClick(Sender: TObject);

// For verbs without "voix pronominale", the button has been disabled by the main unit

begin
  sVoix := 'pronominal';
  btActif.Font.Style := []; btPassif.Font.Style := []; btPronominal.Font.Style := [fsBold];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
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
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btTComposesClick(Sender: TObject);

begin
  sTemps := 'composés';
  btTSimples.Font.Style := []; btPassif.Font.Style := []; btTComposes.Font.Style := [fsBold];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

{ Buttons "Masculin", "Féminin"": Conjugation with sex selected }

procedure TfConj.btMasculinClick(Sender: TObject);

begin
  sGenre := 'masculin';
  btMasculin.Font.Style := [fsBold]; btFeminin.Font.Style := [];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

procedure TfConj.btFemininClick(Sender: TObject);

begin
  sGenre := 'féminin';
  btMasculin.Font.Style := []; btFeminin.Font.Style := [fsBold];
  Conjuguer(Verbe, VerbeType, sVoix, sTemps, sForme, sGenre, bSepTerm, bSepRacine, bColorIrreg, bColorSame, AvoirFile, EtreFile, VerbFile);
end;

{ Button "Fermer": Close the form }

procedure TfConj.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

