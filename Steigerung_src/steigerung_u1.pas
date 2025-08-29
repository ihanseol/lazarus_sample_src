{****************************************}
{* Main unit for Steigerung application *}
{****************************************}

unit steigerung_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, Grids, LazUTF8;

type
  TAdjective = record
    AName, AComp: string;
  end;
  TAdjectives = array of TAdjective;
  {**************}
  { TfSteigerung }
  {**************}
  TfSteigerung = class(TForm)
    MainMenu1: TMainMenu;
    mFile, mFileNew, mFileExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsTypes, mSettingsExists: TMenuItem;
    mSettingsType1, mSettingsType2, mSettingsType3, Separator1, mSettingsSup: TMenuItem;
    mHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, laArticle, laQuestion: TLabel;
    edPos, edComp, edSup, edArticle, edEval: TEdit;
    edDetails: TMemo;
    sgEval: TStringGrid;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsType1Click(Sender: TObject);
    procedure mSettingsType2Click(Sender: TObject);
    procedure mSettingsType3Click(Sender: TObject);
    procedure mSettingsSupClick(Sender: TObject);
    procedure mSettingsExistsClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuestions0, iQuestions, iExType0, iExType, iQuestion, iCorrect, iCompType, iX: Integer;
    sComp1, sSup1, sComp2, sSup2, sDetails: string;
    bSupArticle0, bSupArticle, bExistsOnly0, bExistsOnly: Boolean;
    aAdjectives: TAdjectives;
    aDone: array of Boolean;
  end;

var
  fSteigerung: TfSteigerung;

implementation

{$R *.lfm}

{ Format numbers for the grid (right-align) }

function GFormat(N: Integer; S: string): string;

var
  NS: string;

begin
  NS := ' ' + IntToStr(N);
  if N < 10 then
    NS := '  ' + NS
  else if N < 100 then
    NS := ' ' + NS;
  if S <> '' then
    NS += '%';
  Result := NS;
end;

{ Determine "full" superlative from "simple" superlative }

function FullSup(Sup: string; SupArticle: Boolean; Article: string): string;

begin
  if SupArticle then begin
    // Superlative with article
    if Article = 'die*' then begin                                             // die* is used to designate the plural article 'die'
      Article := 'die';
      Sup := Article + ' ' + Sup + 'en';
    end
    else begin
      Sup := Article + ' ' + Sup + 'e';
    end;
  end
  else begin
    // Superlative with "am"
    Sup := 'am ' + Sup + 'en';
  end;
  Result := Sup;
end;

{ Read adjective data from text file }

procedure ReadAdjectives(out Adjectives: TAdjectives);

var
  N: Integer;
  Line: string;
  InFile: Text;

begin
  N := 0; SetLength(Adjectives, N);
  Assign(InFile, 'adjektive.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(N); SetLength(Adjectives, N);
      Adjectives[N - 1].AName := UTF8Trim(UTF8Copy(Line, 1, 15));
      if UTF8Length(Line) < 16 then                                            // if there is no degrees of comparison code, adjective is regular
        Adjectives[N - 1].AComp := 'RR'
      else
        Adjectives[N - 1].AComp := RightStr(Line, 2);
    end;
  end;
  Close(InFile);
end;

{ Adjectives with regular degrees of comparison }

procedure DCRegular(Adjective: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

const
  TwoForms: array[0..2] of string = (
    'falsch', 'froh', 'keusch'
  );

var
  I: Integer;
  Has2Forms: Boolean;

begin
  Comp1 := Adjective + 'er'; Comp2 := Comp1;
  Sup1 := Adjective + 'st';
  Has2Forms := False;
  for I := 0 to Length(TwoForms) - 1 do begin
    if Adjective = TwoForms[I] then
      Has2Forms := True;
  end;
  if Has2Forms then
    Sup2 := Adjective + 'est'
  else
   Sup2 := Sup1;
  Details := 'Regelmäßige Steigerung. ';
  Details += 'Komperativ: Suffix = -er; Superlativ: Suffix = -st. ';
  if Has2Forms then
    Details += 'Bei diesem Adjektiv ist der Superlativ mit Suffix -est ebenfalls gestattet.';
end;

{ Adjectives ending in muet -e }

procedure DCFinalE(Adjective: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

const
  Irregs: array[0..4] of string = (
    'rege', 'träge', 'vage', 'feige', 'trübe'
  );

var
  I: Integer;
  IsIrreg: Boolean;

begin
  Comp1 := Adjective + 'r'; Comp2 := Comp1;
  // Check if adjective has irregular superlative (list)
  IsIrreg := False;
  for I := 0 to Length(Irregs) - 1 do begin
    if Adjective = Irregs[I] then
      IsIrreg := True;
  end;
  if IsIrreg then begin
    // Adjectives from the list drop the -e- of the superlative
    Sup1 := UTF8Copy(Adjective, 1, UTF8Length(Adjective) - 1) + 'st';
  end
  else begin
    // Regular superlative
    Sup1 := Adjective + 'st';
  end;
  Sup2 := Sup1;
  Details := 'Bei Adjektiven, die auf ein unbetontes -e enden, fällt das -e- des Komparativs weg. ';
  Details += 'Das -e- des Superlativs fällt bei einigen dieser Adjektive ebenfalls weg; ';
  if IsIrreg then
    Details += Adjective + ' ist eines von ihnen.'
  else
    Details += Adjective + ' gehört nicht dazu.'
end;

{ Adjectives ending in -el }

procedure DCEndingInEL(Adjective: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

begin
  Sup1 := Adjective + 'st';
  Adjective := UTF8Copy(Adjective, 1, UTF8Length(Adjective) - 2) + 'l';
  Comp1 := Adjective + 'er';
  Comp2 := Comp1; Sup2 := Sup1;
  Details := 'Bei Adjektiven, die auf -el enden, fällt im Komperativ das -e- dieser -el Endung weg. ';
  Details += 'Der Superlativ ist regelmäßig.';
end;

{ Adjectives ending in -er }

procedure DCEndingInER(Adjective: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

const
  Vowals: array[0..4] of string = (
    'i', 'u', 'e', 'o', 'a'
  );

var
  I: Integer;
  Prec: string;
  IsVowal: Boolean;

begin
  Sup1 := Adjective + 'st';
  IsVowal := False;
  Prec := UTF8Copy(Adjective, UTF8Length(Adjective) - 2, 1);
  for I := 0 to Length(Vowals) - 1 do begin
    if Prec = Vowals[I] then
      IsVowal := True;
  end;
  if IsVowal then
    Adjective := UTF8Copy(Adjective, 1, UTF8Length(Adjective) - 2) + 'r';
  Comp1 := Adjective + 'er';
  Comp2 := Comp1; Sup2 := Sup1;
  Details := 'Bei Adjektiven, die auf -er enden, fällt im Komperativ das -e- dieser -er Endung weg, ';
  Details += 'vorausgesetzt dass vor dem -er ein Vokal steht. ';
  if IsVowal then
    Details += 'Das ist bei diesem Adjektiv der Fall. '
  else
    Details += 'Das ist bei diesem Adjektiv nicht der Fall. ';
  Details += 'Der Superlativ ist regelmäßig.';
end;

{ Adjectives with special endings }

procedure DCSpecialEnding(Adjective: string; SpGroup: Integer; SpecialEnding: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

const
  IrregsInD: array[0..11] of string = (
    'blind', 'blond', 'blöd', 'elend', 'fad', 'fremd', 'müd', 'paranoid', 'rund', 'rüd', 'wild', 'wund'
  );
  TwoForms: array[0..0] of string = (
    'berechtigt'
  );

var
  I: Integer;
  IsIrreg, IsTwoForms: Boolean;

begin
  Comp1 := Adjective + 'er'; Comp2 := Comp1;
  Details := 'Bei Adjektiven, ';
  if SpGroup = 1 then begin
    // Adjectives ending in -d, -t, -s, -ß, -x, -z
    if SpecialEnding = 'd' then begin
      // Adjectives ending in 'd': Superlative in -st OR -est
      Details += 'die auf -d enden, bekommt der Superlativ bei einigen von ihnen ein zusätliches -e-; ';
      IsIrreg := False;
      for I := 0 to Length(IrregsInD) - 1 do begin
        if Adjective = IrregsInD[I] then
          IsIrreg := True;
      end;
      if IsIrreg then begin
        Sup1 := Adjective + 'est';
        Details += Adjective + ' ist eines von ihnen und der Superlativ hat so den Suffix -est. ';
      end
      else begin
        Sup1 := Adjective + 'st';
        Details += Adjective + ' gehört nicht dazu und der Superlativ ist regelmäßig.';
      end;
      Sup2 := Sup1;
    end
    else begin
      // Adjectives ending in 't', 's', 'ß', 'x', 'z': Superlative in -est
      Sup1 := Adjective + 'est';
      IsTwoForms := False;
      for I := 0 to Length(TwoForms) - 1 do begin
        if Adjective = TwoForms[I] then
          IsTwoForms := True;
      end;
      if IsTwoForms then
        Sup2 := Adjective + 'st'
      else
        Sup2 := Sup1;
      Details += 'die auf -' + SpecialEnding + ' enden, bekommt der Superlativ (aus Aussprachegründen) ein zusätliches -e- ';
      Details += 'und hat damit den Suffix -est. ';
      if IsTwoForms then
        Details += 'Bei diesem Adjektiv ist der regelmäßige Superlativ auch gestattet. ';
      Details += 'Der Komperativ ist regelmäßig.';
    end;
  end
  else begin
    // Adjectives ending in a double-vowal: Superlative in -st or -est (both correct)
    Details += 'die auf ein Doppelvokal (au, ei, eu) enden, kann der Superlativ ein zusätliches -e- bekommen, ';
    Details += 'muß das aber nicht; er kann also den Suffix -st oder -est haben. ';
    Details += 'Der Komperativ ist regelmäßig.';
    Sup1 := Adjective + 'st'; Sup2 := Adjective + 'est';
  end;
end;

{ Adjectives that must or can get an Umlaut in comperative and superlative}

procedure DCUmlaut(Adjective, Comp: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

const
  Vowals: array[0..2] of string = (
    'u', 'o', 'a'
  );
  SpecialEndings: array[0..5] of string = (
    'd', 't', 's', 'ß', 'x', 'z'
  );

var
  IX, I, J: Integer;
  Vowal, Umlaut: string;
  SpecialEnding: string;

begin
  // Check for irregular comperative (adjectives in -e)
  if UTF8Copy(Adjective, UTF8Length(Adjective), 1) = 'e' then
    Comp1 := Adjective + 'r'
  else
    Comp1 := Adjective + 'er';
  // Check for irregular superlative (adjectives in 'd', 't', 's', 'ß', 'x', 'z')
  SpecialEnding := '';
  for I := 0 to Length(SpecialEndings) - 1 do begin
    if UTF8Copy(Adjective, UTF8Length(Adjective) - Length(SpecialEndings[I]) + 1, Length(SpecialEndings[I])) = SpecialEndings[I] then
      SpecialEnding := SpecialEndings[I];
  end;
  if SpecialEnding <> '' then
    Sup1 := Adjective + 'est'
  else
    Sup1 := Adjective + 'st';
  Comp2 := Comp1; Sup2 := Sup1;
  // Find the vowal that must get the umlaut
  I := UTF8Length(Adjective); Vowal := '';
  repeat
    Dec(I);
    for J := 0 to Length(Vowals) - 1 do begin
      if UTF8Copy(Adjective, I, 1) = Vowals[J] then begin
        IX := I; Vowal := Vowals[J];
      end;
    end;
  until (Vowal <> '') or (I = 1);
  if Vowal <> '' then begin
    if Vowal = 'a' then
      Umlaut := 'ä'
    else if Vowal = 'o' then
      Umlaut := 'ö'
    else
      Umlaut := 'ü';
    Comp1 := UTF8Copy(Comp1, 1, IX - 1) + Umlaut + UTF8Copy(Comp1, IX + 1, Length(Comp1));
    Sup1 := UTF8Copy(Sup1, 1, IX - 1) + Umlaut + UTF8Copy(Sup1, IX + 1, Length(Sup1));
    if Comp = 'UU' then begin
      // These ajectives must have an umlaut
      Comp2 := Comp1; Sup2 := Sup1;
    end;
  end;
  Details := 'Dies ist eines der wenigen deutschen Adjektive, die im Komperativ und Superlativ einen Umlaut ';
  if Comp = 'UU' then
    Details += 'bekommen müssen. '
  else
    Details += 'bekommen können; die Form ohne Umlaut ist ebenfalls gestattet. ';
  if SpecialEnding <> '' then begin
    if UTF8Copy(Sup1, UTF8Length(Sup1) - 2, 3) = 'est' then begin
      Details += 'Bei diesem Adjektiv spielt ausserdem die Regel der Adjektive, die auf -' + SpecialEnding + ' enden: ';
      Details += 'Superlativ Suffix = -est.';
    end;
  end
  else if UTF8Copy(Adjective, UTF8Length(Adjective), 1) = 'e' then
    Details += 'Ausserdem fällt bei diesem Adjektiv, das auf ein unbetontes -e endet, das -e- des Komparativs weg.';
end;

{ Irregular adjectives }

procedure DCIrregular(Adjective: string; out Comp1, Sup1, Comp2, Sup2, Details: string);

const
  Irregs: array[0..5] of string = (
    'gern', 'gut', 'hoch', 'nah', 'viel', 'fit'
  );

var
  IX, I: Integer;

begin
  Comp1 := '?'; Sup1 := '?';
  IX := -1;
  for I := 0 to Length(Irregs) - 1 do begin
    if Adjective = Irregs[I] then
      IX := I;
  end;
  case IX of
    0: begin Comp1 := 'lieber'; Sup1 := 'liebst'; end;
    1: begin Comp1 := 'besser'; Sup1 := 'best'; end;
    2: begin Comp1 := 'höher'; Sup1 := 'höchst'; end;
    3: begin Comp1 := 'näher'; Sup1 := 'nächst'; end;
    4: begin Comp1 := 'mehr'; Sup1 := 'meist'; end;
    5: begin Comp1 := 'fitter'; Sup1 := 'fittest'; end;
  end;
  Comp2 := Comp1; Sup2 := Sup1;
  Details := 'Dies ist eines der wenigen deutschen Adjektive, die eine unregelmäßige Steigerung haben.';
end;

{ Adjectives that do not have degrees of comparison }

procedure DCNone(out Comp1, Sup1, Comp2, Sup2, Details: string);

begin
  Comp1 := ''; Sup1 := '';
  Comp2 := ''; Sup2 := '';
  Details := 'Dieses Adjektiv ist nicht steigerbar.';
end;

{**************}
{ TfSteigerung }
{**************}

{ Application start: Initialisation }

procedure TfSteigerung.FormCreate(Sender: TObject);

begin
  ReadAdjectives(aAdjectives);
  SetLength(aDone, Length(aAdjectives));
  iQuestions0 := 20; iExType0 := 3;
  bSupArticle0 := False; bExistsOnly0 := False;
  Randomize;
  mFileNew.Click;
end;

{ Menu item "Datei > Neu": Start a new test }

procedure TfSteigerung.mFileNewClick(Sender: TObject);

var
  I: Integer;

begin
  // Make application settings now active
  iQuestions := iQuestions0; iExType := iExType0;
  bSupArticle := bSupArticle0; bExistsOnly := bExistsOnly0;
  // Show resp. hide article related controls
  if bSupArticle then begin
    laArticle.Visible := True; edArticle.Visible := True;
  end
  else begin
    laArticle.Visible := False; edArticle.Visible := False;
  end;
  // Reset variables
  iQuestion := 0; iCorrect := 0;
  for I := 0 to Length(aDone) - 1 do
    aDone[I] := False;
  laQuestion.Caption := 'Frage:';
  edPos.Text := ''; edArticle.Text := '';
  edComp.Text := ''; edSup.Text := '';
  edComp.Color := clDefault; edSup.Color := clDefault;
  edEval.Text := ''; edDetails.Lines.Clear;
  for I := 0 to 3 do
    sgEval.Cells[1, I] := '';
  // Disable comperative/superlative edit filed if not part of the test
  edComp.Enabled := True; edSup.Enabled := True;
  if iExType = 1 then
    edSup.Enabled := False
  else if iExType = 2 then
    edComp.Enabled := False;
  // Reset button properties
  btQuestion.Caption := 'Frage'; btQuestion.Enabled := True;
end;

{ Menu item "Datei > Verlassen": Exit the application }

procedure TfSteigerung.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Einstellungen > Anzahl der Fragen ...": User input of number of test questions }

procedure TfSteigerung.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Steigerung der Adjektive', 'Anzahl der Fragen', IntToStr(iQuestions));
  if S <> '' then begin
    iQuestions0 := StrToInt(S);
    if iQuestions0 < 10 then
      iQuestions0 := 10;                                                       // arbitrarily chosen minimum of 10 questions
  end;
end;

{ Menu items "Einstellungen > Steigerungsformen > ...": Selection if test is about comperative, superlative, or both }

procedure TfSteigerung.mSettingsType1Click(Sender: TObject);

begin
  mSettingsType1.Checked := True; mSettingsType2.Checked := False; mSettingsType3.Checked := False;
  iExType0 := 1;
end;

procedure TfSteigerung.mSettingsType2Click(Sender: TObject);

begin
  mSettingsType1.Checked := False; mSettingsType2.Checked := True; mSettingsType3.Checked := False;
  iExType0 := 2;
end;

procedure TfSteigerung.mSettingsType3Click(Sender: TObject);

begin
  mSettingsType1.Checked := False; mSettingsType2.Checked := False; mSettingsType3.Checked := True;
  iExType0 := 3;
end;

{ Menu item "Einstellungen > Steigerungsformen > Superlativ mot Artikel": Toggle superlative with "am" and superlative with article }

procedure TfSteigerung.mSettingsSupClick(Sender: TObject);

begin
  if mSettingsSup.Checked then
    mSettingsSup.Checked := False
  else
    mSettingsSup.Checked := True;
  bSupArticle0 := mSettingsSup.Checked;
end;

{ Menu item "Einstellungen > Nur steigerbare Adjektive": Toggle if adjectives with no degrees of comparison should be used or not }

procedure TfSteigerung.mSettingsExistsClick(Sender: TObject);

begin
  if mSettingsExists.Checked then
    mSettingsExists.Checked := False
  else
    mSettingsExists.Checked := True;
  bExistsOnly0 := mSettingsExists.Checked;
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfSteigerung.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Deutsche Grammatik: Steigerung der Adjektive.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, August 2022.';
  MessageDlg('Über "Steigerung"', S, mtInformation, [mbOK], 0);
end;

{ Button "Frage/Antwort" pushed: Ask new adjective resp. check user answer }

procedure TfSteigerung.btQuestionClick(Sender: TObject);

const
  SpecialEndings: array[0..5] of string = (
    'd', 't', 's', 'ß', 'x', 'z'
  );
  SpecialEndings2: array[0..2] of string = (
    'au', 'ei', 'eu'
  );

var
  Count, SpGroup, I: Integer;
  Article, SpecialEnding, SpecialEnding1, SpecialEnding2: string;
  Found, Correct: Boolean;

begin
  // Button "Frage": Display new adjective
  if btQuestion.Caption = 'Frage' then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Frage ' + IntToStr(iQuestion) + '/' + IntToStr(iQuestions) + ':';
    if bSupArticle then begin
      // Choose random article (if superlative with article is selected)
      I := Random(3);
      case I of
        0: begin edArticle.Text := 'männlich'; Article := 'der'; end;
        1: begin edArticle.Text := 'weiblich';  Article := 'die'; end;
        2: begin edArticle.Text := 'neutral';  Article := 'das'; end;
      end;
    end;
    I := Random(5);
    if I = 0 then begin
      edArticle.Text := edArticle.Text + ', Mehrzahl';
      Article := 'die*';                                                       // code plural article as "die*" (to distinguish from fem. sg.)
    end
    else
      edArticle.Text := edArticle.Text + ', Einzahl';
    // Get new random adjective; repeat this step until "valid" one found
    repeat
      Found := False; Count := 0;
      // Random adjective type. Proceeding this way will generate adjectives of all degrees of comparison types,
      // even for a small number of questions. Adjective type will be an integer from 0 to 7 (or 0 to 6, if those
      // without degrees of comparison are selected to be excluded)
      if bExistsOnly then
        iCompType := Random(13)
      else
        iCompType := Random(14);
      iCompType := (iCompType + 1) div 2;
      // Same loop as before, but including a counter varuable, that allows to exit the loop if no "valid" adjective
      // has been found after a given number of trials. This is necessary, as there is the possiblity that at a given
      // moment there are no more adjectives of some type left and this would lead to an infinite loop...
      repeat
        // Random adjective that has not yet been asked
        repeat
          iX := Random(Length(aAdjectives));
        until not aDone[iX];
        Found := False; Inc(Count);
        // Random adjective must be of the degrees of comparison type that we actually want. If not, continue looping...
        if (aAdjectives[iX].AComp = 'II') and (iCompType = 6) then
          // Irregular adjectives
          Found := True
        else if ((aAdjectives[iX].AComp = 'UU') or (aAdjectives[iX].AComp = 'UR')) and (iCompType = 5) then
          // Adjectives with umlaut
          Found := True
        else if (LeftStr(aAdjectives[iX].AComp, 1) = 'N') and (iCompType = 7) and (not mSettingsExists.Checked) then
          // Adjectives withot degrees of comparison
          Found := True
        else if (aAdjectives[iX].AComp = 'RR') or (LeftStr(aAdjectives[iX].AComp, 1) = 'n') then begin
          // Other adjectives (incl. those that only have degrees of comparison in spoken language)
          if UTF8Copy(aAdjectives[iX].AName, UTF8Length(aAdjectives[iX].AName) - 1, 2) = 'el' then begin
            // Adjectives ending in -el
            if iCompType = 2 then
              Found := True;
          end
          else if (UTF8Copy(aAdjectives[iX].AName, UTF8Length(aAdjectives[iX].AName) - 1, 2) = 'er') and
                  (UTF8Copy(aAdjectives[iX].AName, UTF8Length(aAdjectives[iX].AName) - 2, 3) <> 'eer')then begin
            // Adjectives ending in -er
            if iCompType = 3 then
              Found := True;
          end
          else if (UTF8Copy(aAdjectives[iX].AName, UTF8Length(aAdjectives[iX].AName), 1) = 'e') then begin
            // Adjectives ending in muet -e
            if iCompType = 1 then
              Found := True;
          end
          else begin
            // Check for adjectives with special endings
            SpecialEnding1 := ''; SpecialEnding2 := '';
            for I := 0 to Length(SpecialEndings) - 1 do begin
              if UTF8Copy(aAdjectives[iX].AName, UTF8Length(aAdjectives[iX].AName) - UTF8Length(SpecialEndings[I]) + 1, UTF8Length(SpecialEndings[I])) = SpecialEndings[I] then
                // Adjective ending in -d, -t, -s, -ß, -x, -z
                SpecialEnding1 := SpecialEndings[I];
            end;
            for I := 0 to Length(SpecialEndings2) - 1 do begin
              if UTF8Copy(aAdjectives[iX].AName, UTF8Length(aAdjectives[iX].AName) - 1, 2) = SpecialEndings2[I] then
                // Adjectives ending in double vowal
                SpecialEnding2 := SpecialEndings2[I];
            end;
            if (SpecialEnding1 <> '') or (SpecialEnding2 <> '') then begin
              // Random adjective is adjective with special ending
              if iCompType = 4 then begin
                if SpecialEnding1 <> '' then begin
                  SpGroup := 1; SpecialEnding := SpecialEnding1;
                end
                else begin
                  SpGroup := 2; SpecialEnding := SpecialEnding2;
                end;
                Found := True;
              end;
            end
            else begin
              // Regular adjective without any special considerations (except the spoken language only ones; cf. below)
              if iCompType = 0 then
                Found := True;
            end;
          end;
        end;
        // Include adjectives with degrees of comparison in spoken language only, only if adjectives without degrees of comparison are included
        if (LeftStr(aAdjectives[iX].AComp, 1) = 'n') and mSettingsExists.Checked then
          Found := False
      until Found or (Count > 200);                                            // end inner loop if adjective found or not possible to find for this type
    until Found;                                                               // end outer loop (incl. random type selection) if adjective found
    // Mark this adjective as "has been asked"
    aDone[iX] := True;
    // Fill in the form fields
    edPos.Text := aAdjectives[iX].AName;
    edComp.Text := ''; edSup.Text := '';
    edComp.Color := clDefault; edSup.Color := clDefault;
    edEval.Text := '';
    edDetails.Lines.Clear;
    // Determine the degrees of comparison, depending on actual adjective type
    case iCompType of
      0: DCRegular(aAdjectives[iX].AName, sComp1, sSup1, sComp2, sSup2, sDetails);
      1: DCFinalE(aAdjectives[iX].AName, sComp1, sSup1, sComp2, sSup2, sDetails);
      2: DCEndingInEL(aAdjectives[iX].AName, sComp1, sSup1, sComp2, sSup2, sDetails);
      3: DCEndingInER(aAdjectives[iX].AName, sComp1, sSup1, sComp2, sSup2, sDetails);
      4: DCSpecialEnding(aAdjectives[iX].AName, spGroup, SpecialEnding, sComp1, sSup1, sComp2, sSup2, sDetails);
      5: DCUmlaut(aAdjectives[iX].AName, aAdjectives[iX].AComp, sComp1, sSup1, sComp2, sSup2, sDetails);
      6: DCIrregular(aAdjectives[iX].AName, sComp1, sSup1, sComp2, sSup2, sDetails);
      7: DCNone(sComp1, sSup1, sComp2, sSup2, sDetails);
    end;
    // Some special considerations for adjectives with degrees of comparison in spoken language only
    if LeftStr(aAdjectives[iX].AComp, 1) = 'n' then begin
      sComp2 := ''; sSup2 := '';
      sDetails += LineEnding + 'Außer in der Umgangssprache ist dieses Adjektiv nicht steigerbar.';
    end;
    // Determine "full" form of superlative (with "am" or with actual article, depending on settings)
    if sSup1 <> '' then
      sSup1 := FullSup(sSup1, bSupArticle, Article);
    if sSup2 <> '' then
      sSup2 := FullSup(sSup2, bSupArticle, Article);
    // Set focus to first input field
    if iExType = 2 then
      edSup.SetFocus
    else
      edComp.SetFocus;
    // Next button push will be to check user's answer
    btQuestion.Caption := 'Antwort';
  end
  // Button "Antwort": Check user's answer
  else begin
    Correct := True;
    if (iExType = 1) or (iExType = 3) then begin
      // Check if comperative entered is correct
      if (edComp.Text <> sComp1) and (edComp.Text <> sComp2) then begin
        edComp.Color := clRed;
        Correct := False;
      end;
    end;
    if (iExType = 2) or (iExType = 3) then begin
      // Check if superlative entered is correct
      if (edSup.Text <> sSup1) and (edSup.Text <> sSup2) then begin
        edSup.Color := clRed;
        Correct := False;
      end;
    end;
    // User answer evaluation
    if Correct then begin
      // Degree(s) of comparison asked is/are correct
      edEval.Text := 'Richtig!';
      Inc(iCorrect);
    end
    else begin
      // There is at least on of degrees of comparison asked that isn't correct
      edEval.Text := 'Falsch!';
      if (iExType = 1) or (iExType = 3) then begin
        // Display correct answer for comperative (if comperative has been asked)
        if sComp1 <> '' then begin
          edEval.Text := edEval.Text + ' K: ' + sComp1;
          if sComp2 <> sComp1 then begin
            if sComp2 = '' then
              edEval.Text := edEval.Text + ' (Umgangssprache)'
            else
              edEval.Text := edEval.Text + '/' + sComp2;
          end;
          if iExType = 3 then
            // Both comperative and superlative have been asked
            edEval.Text := edEval.Text + '; ';
        end;
      end;
      if (iExType = 2) or (iExType = 3) then begin
        // Display correct answer for superlative (if superlative has been asked)
        if sSup1 <> '' then begin
          edEval.Text := edEval.Text + ' S: ' + sSup1;
          if sSup2 <> sSup1 then begin
            if sSup2 = '' then
              edEval.Text := edEval.Text + ' (Umgangssprache)'
            else
              edEval.Text := edEval.Text + '/' + sSup2;
          end;
        end;
      end;
    end;
    // Display details for actual degrees of comparison type
    edDetails.Lines.AddText(sDetails);
    // Fill in evaluation grid
    sgEval.Cells[1, 0] := GFormat(iQuestion, '');
    sgEval.Cells[1, 1] := GFormat(iCorrect, '');
    sgEval.Cells[1, 2] := GFormat(iQuestion - iCorrect, '');
    sgEval.Cells[1, 3] := GFormat(Round(100 * (iCorrect / iQuestion)), '%');
    // Next button push will be to display another adjective
    btQuestion.Caption := 'Frage';
    if iQuestion < iQuestions then
      // If there are still adjectives left (number of questions for this test not yet reached), contine...
      btQuestion.SetFocus
    else begin
      // If there aren't any adjectives left (number of questions for this test reached), terminate this test
      MessageDlg('Steigerung der Adjektive', 'Alle Adjektive wurden abgefragt. Ende des Tests.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // block button until a new test is launched
    end;
  end;
end;

end.

