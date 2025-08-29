{***********************************}
{* Main unit of Zodiac application *}
{***********************************}

unit zodiac_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Menus, IniFiles, zodiac_signs, zodiac_data, zodiac_help;

type
  TButtons = array[1..5] of TButton;
  {**********}
  { TfZodiac }
  {**********}
  TfZodiac = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsElemets, mSettingsCompatibility: TMenuItem;
    mSettingsElemets1, mSettingsElemets2, mSettingsElemets3, mSettingsElemets4, mSettingsElemets5: TMenuItem;
    mSettingsCompatibility1, mSettingsCompatibility2: TMenuItem;
    mHelp, mHelpHelp, mHelpZodiac, mHelpInfo, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Image1: TImage;
    Label1, Label2, Label3, Label4, Label5, Label6, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15, Label18, Label19, Label20: TLabel;
    laElementsAssoc, laLatin, laGreek: TLabel;
    edSign, edPeriod: TEdit;
    edSeason, edAge: TEdit;
    edDuality, edQuadruplicity: TEdit;
    edElement, edElementsAssoc: TEdit;
    edPlanet, edColor: TEdit;
    edDay, edNumber: TEdit;
    edPersonality, edNegChar, edCompatibility: TEdit;
    edChar: TMemo;
    btChar1, btChar2, btChar3, btChar4, btChar5: TButton;
    imSign: TImage;
    stEnglish: TStaticText;
    imSymbol: TImage;
    cobMonth, cobDay: TComboBox;
    btSign: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsCompatibility1Click(Sender: TObject);
    procedure mSettingsCompatibility2Click(Sender: TObject);
    procedure mSettingsElemets1Click(Sender: TObject);
    procedure mSettingsElemets2Click(Sender: TObject);
    procedure mSettingsElemets3Click(Sender: TObject);
    procedure mSettingsElemets4Click(Sender: TObject);
    procedure mSettingsElemets5Click(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpZodiacClick(Sender: TObject);
    procedure mHelpInfoClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btChar1Click(Sender: TObject);
    procedure btChar2Click(Sender: TObject);
    procedure btChar3Click(Sender: TObject);
    procedure btChar4Click(Sender: TObject);
    procedure btChar5Click(Sender: TObject);
    procedure btSignClick(Sender: TObject);
    procedure cobMonthChange(Sender: TObject);
    procedure cobDayChange(Sender: TObject);
  private
    iZ, iAssoc: Integer;
    aButtons: TButtons;
  end;

const
  Months: array[1..12] of string = (
    'Januar', 'Februar', 'März', 'April', 'Mai', 'Juni',
    'Juli', 'August', 'September', 'Oktober', 'November', 'Dezember'
  );
  DaysPerMonth: array[1..12] of Integer = (
    31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
  );
  Seasons: array[0..3] of string = (
    'Frühling', 'Sommer', 'Herbst', 'Winter'
  );
  Ages: array[0..3] of string = (
    'Kindheit', 'Jugend', 'Mannheit', 'Alter'
  );
  ZodiacSigns: array[1..12] of string = (
    'Widder', 'Stier', 'Zwillinge', 'Krebs', 'Löwe', 'Jungfrau',
    'Waage', 'Skorpion', 'Schütze', 'Steinbock', 'Wassermann', 'Fische'
  );
  ZodiacSignsEnglish: array[1..12] of string = (
    'Ram', 'Bull', 'Twins', 'Crab', 'Lion', 'Maiden',
    'Scales', 'Scorpion', 'Archer', 'Sea-Goat', 'Water-Bearer', 'Fish'
  );
  ZodiacSignsLatin: array[1..12] of string = (
    'Aries', 'Taurus', 'Gemini', 'Cancer', 'Leo', 'Virgo',
    'Libra', 'Scorpio', 'Sagittarius', 'Capricornus', 'Aquarius', 'Pisces'
  );
  ZodiacSignsGreek: array[1..12] of string = (
    'Κριός', 'Tαῦρος', 'Δίδυμοι', 'Καρκίνος', 'Λέων', 'Παρθένος',
    'Ζυγὁς', 'Σκορπίος', 'Τοξότης', 'Αἰγοκερεύς', 'Ὑδροχόος', 'Ἰχθύες'
  );
  ZodiacPeriods: array[1..12] of string = (
    '0321-0420', '0421-0521', '0522-0621', '0622-0722', '0723-0822', '0823-0922',
    '0923-1022', '1023-1122', '1123-1220', '1221-0119', '0120-0218', '0219-0320'
  );
  ZodiacPlanets: array[1..12] of string = (
    'Mars', 'Venus', 'Merkur', 'Mond', 'Sonne', 'Merkur',
    'Venus', 'Pluto', 'Jupiter', 'Saturn', 'Uranus', 'Neptun'
  );
  ZodiacColors: array[1..12] of string = (
    'rot', 'grün', 'gelb', 'grün', 'orange, rot', 'violett',
    'rosa, gelb', 'rot', 'violett', 'schwarz', 'blau', 'blau, grün'
  );
  ZodiacDays: array[1..12] of string = (
    'Dienstag', 'Freitag', 'Mittwoch', 'Freitag', 'Montag', 'Mittwoch',
    'Samstag', 'Dienstag', 'Donnerstag', 'Sonntag', 'Montag', 'Donnerstag'
  );
  ZodiacNumbers: array[1..12] of Integer = (
    9, 6, 5, 2, 1, 5, 6, 9, 3, 8, 4, 7
  );
  Dualities: array[0..1] of string = (
    'männlich', 'weiblich'
  );
  Quadruplicities: array[0..2] of string = (
    'grundsätzlich', 'fest', 'veränderlich'
  );
  Triplicities: array[0..3] of string = (
    'Feuer', 'Erde', 'Luft', 'Wasser'
  );
  Associations: array[0..4] of string = (
    'Qualität', 'Aggregatzustand', 'Saft', 'Temperament', 'Farbe'
  );
  ElementAssociations: array[0..3, 0..4] of string = (
    ( 'trocken + warm', 'fein (gasförmig)', 'Galle', 'cholerisch', 'gelb' ),
    ( 'trocken + kalt', 'dicht', 'schwarze Galle', 'melancholisch', 'schwarz' ),
    ( 'feucht + warm', 'flüssig', 'Blut', 'sanguinisch', 'rot' ),
    ( 'feucht + kalt', 'zähe', 'Schleim', 'phlegmatisch', 'weiß' )
  );
  ZodiacPersonalities: array[1..12] of string = (
    'dynamisch und entschlossen', 'stark, praktisch, zuverlässig', 'humorvoll, neugierig und freundlich',
    'mitfühlend und verständnisvoll', 'leidenschaftlich und selbstbewusst', 'sanft und loyal',
    'ausgeglichen, freundlich, liebenswürdig', 'entschlossen, konzentriert und gewitzt', 'humorvoll, offen und positiv',
    'weise und ambitioniert', 'freundlich, klever und loyal', 'freundlich und mitfühlend'
  );
  ZodiacNegChars: array[1..12] of string = (
    'cholerisch, impulsiv und voreilig', 'eigensinnig, faul und geizig', 'ängstlich und unentschlossen',
    'empfindlich und selbstsüchtig', 'eifersüchtig und stolz', 'zu analytisch und unflexibel',
    'naiv und unentschlossen', 'obsessiv und misstrauig', 'ungeduldig und oberflächlich',
    'schüchtern und dickköpfig', 'stur und extem', 'faul und übersensibel'
  );
  ZodiacCompatibilities: array[1..12, 1..4] of string = (
    ( 'Zwilling', '#Schütze', 'Löwe', 'Wassermann' ),
    ( 'Steinbock', 'Fische', '#Jungfrau', 'Krebs' ),
    ( '#Wassermann', '#Waage', 'Widder', 'Löwe' ),
    ( 'Fische', 'Stier', '#Skorpion', 'Jungfrau' ),
    ( '#Schütze', 'Widder', 'Zwilling', 'Waage' ),
    ( '#Stier', 'Steinbock', 'Krebs', 'Skorpion' ),
    ( '#Zwilling', 'Wassermann', 'Löwe', 'Schütze' ),
    ( 'Fische', '#Krebs', 'Steinbock', 'Jungfrau' ),
    ( 'Löwe', '#Widder', 'Waage', 'Wassermann' ),
    ( '#Stier', 'Jungfrau', 'Fische', 'Skorpion' ),
    ( '#Zwilling', 'Waage', 'Schütze', 'Widder' ),
    ( 'Krebs', '#Skorpion', 'Stier', 'Steinbock' )
  );

var
  fZodiac: TfZodiac;
  DataFile: TINIFile;

implementation

{ Get zodiac sign index for given birthdate (month, day) }

function GetZodiacIndex(M, D: Integer): Integer;

var
  P0, P1, P2, I, IX: Integer;
  PStart, PEnd: string;

begin
  P0 := 100 * M + D;
  if (M = 1) and (D <= 19) then                                                          // calculation adaption if birthdate from 1.1 - 19.1
    P0 += 1200;
  for I := 1 to 12 do begin
    // Find the period that include the birthdate
    PStart := LeftStr(ZodiacPeriods[I], 4); PEnd := RightStr(ZodiacPeriods[I], 4);
    P1 := 100 * StrToInt(LeftStr(PStart, 2)) + StrToInt(RightStr(PStart, 2));
    P2 := 100 * StrToInt(LeftStr(PEnd, 2)) + StrToInt(RightStr(PEnd, 2));
    if I = 10 then                                                                       // calculation adaption for period from 21.12 - 19.1
      P2 += 1200;
    if (P0 >= P1) and (P0 <= P2) then
      IX := I;
  end;
  Result := IX;
end;

{ Get string form of zodiac period (for given zodiac index) }

function GetZodiacPeriod(Z: Integer): string;

var
  M: Integer;
  Period, PStart, PEnd, D: string;

begin
  PStart := LeftStr(ZodiacPeriods[Z], 4); PEnd := RightStr(ZodiacPeriods[Z], 4);
  M := StrToInt(LeftStr(PStart, 2)); D := RightStr(PStart, 2);
  Period := D + '. ' + Months[M] + ' - ';
  M := StrToInt(LeftStr(PEnd, 2)); D := RightStr(PEnd, 2);
  Period += D + '. ' + Months[M];
  Result := Period;
end;

{ Clear the form fields }

procedure ClearAll(Buttons: TButtons; out Z: Integer);

var
  I: Integer;

begin
  fZodiac.edSign.Text := ''; fZodiac.edPeriod.Text := '';
  fZodiac.edSeason.Text := ''; fZodiac.edAge.Text := '';
  fZodiac.edElement.Text := ''; fZodiac.edElementsAssoc.Text := '';
  fZodiac.edDuality.Text := ''; fZodiac.edQuadruplicity.Text := '';
  fZodiac.edPlanet.Text := ''; fZodiac.edColor.Text := '';
  fZodiac.edDay.Text := ''; fZodiac.edNumber.Text := '';
  fZodiac.edPersonality.Text := '';
  fZodiac.edNegChar.Text := '';
  fZodiac.edCompatibility.Text := '';
  fZodiac.edChar.Clear;
  for I := 1 to 5 do
    Buttons[I].Font.Style := [];
  fZodiac.imSign.Picture.Clear; fZodiac.imSymbol.Picture.Clear;
  fZodiac.laLatin.Caption := ''; fZodiac.laGreek.Caption := ''; fZodiac.stEnglish.Caption := '';
  Z := 0;
end;

{ Display personality characteristics (read from data file) as result of corr. button push }

procedure DisplayCharacteristics(Section: string; Characteristic: Integer; Buttons: TButtons);

var
  I: Integer;
  Key: string;

begin
  Key := 'ch' + IntToStr(Characteristic);
  fZodiac.edChar.Clear;
  fZodiac.edChar.Lines.AddText(DataFile.ReadString(Section, Key, ''));
  // Highlight the button that has been pushed
  for I := 1 to 5 do
    Buttons[I].Font.Style := [];
  Buttons[Characteristic].Font.Style := [fsBold];
end;

{$R *.lfm}

{**********}
{ TfZodiac }
{**********}

{ Application satrt: Initialisation }

procedure TfZodiac.FormCreate(Sender: TObject);

var
  Filename: string;

begin
  // Create array with characteristic buttons
  aButtons[1] := btChar1; aButtons[2] := btChar2; aButtons[3] := btChar3;
  aButtons[4] := btChar4; aButtons[5] := btChar5;
  // Open the data file (as Free Pascal .INI file )
  Filename := './res/zodiac.dat'; DoDirSeparators(Filename);
  DataFile := TINIFile.Create(Filename);
  // Clear form fields
  ClearAll(aButtons, iZ);
  iAssoc := 0;
end;

{ Menu item "Datei > Verlassen": Exit application }

procedure TfZodiac.mFileExitClick(Sender: TObject);

begin
  DataFile.Free;
  Close;
end;

{ Menu items "Einstellungen > Elemente Zuordnung > ...": Choose zodiac elements associations display option }

procedure TfZodiac.mSettingsElemets1Click(Sender: TObject);

begin
  mSettingsElemets1.Checked := True;
  mSettingsElemets2.Checked := False;
  mSettingsElemets3.Checked := False;
  mSettingsElemets4.Checked := False;
  mSettingsElemets5.Checked := False;
  iAssoc := 0;
end;

procedure TfZodiac.mSettingsElemets2Click(Sender: TObject);

begin
  mSettingsElemets1.Checked := False;
  mSettingsElemets2.Checked := True;
  mSettingsElemets3.Checked := False;
  mSettingsElemets4.Checked := False;
  mSettingsElemets5.Checked := False;
  iAssoc := 1;
end;

procedure TfZodiac.mSettingsElemets3Click(Sender: TObject);

begin
  mSettingsElemets1.Checked := False;
  mSettingsElemets2.Checked := False;
  mSettingsElemets3.Checked := True;
  mSettingsElemets4.Checked := False;
  mSettingsElemets5.Checked := False;
  iAssoc := 2;
end;

procedure TfZodiac.mSettingsElemets4Click(Sender: TObject);

begin
  mSettingsElemets1.Checked := False;
  mSettingsElemets2.Checked := False;
  mSettingsElemets3.Checked := False;
  mSettingsElemets4.Checked := True;
  mSettingsElemets5.Checked := False;
  iAssoc := 3;
end;

procedure TfZodiac.mSettingsElemets5Click(Sender: TObject);

begin
  mSettingsElemets1.Checked := False;
  mSettingsElemets2.Checked := False;
  mSettingsElemets3.Checked := False;
  mSettingsElemets4.Checked := False;
  mSettingsElemets5.Checked := True;
  iAssoc := 4;
end;

{ Menu items "Einstellungen > Kompatibilität Details > ...": Choose zodiac signs compatibility display option }

procedure TfZodiac.mSettingsCompatibility1Click(Sender: TObject);

begin
  mSettingsCompatibility1.Checked := True;
  mSettingsCompatibility2.Checked := False;
end;

procedure TfZodiac.mSettingsCompatibility2Click(Sender: TObject);

begin
  mSettingsCompatibility2.Checked := True;
  mSettingsCompatibility1.Checked := False;
end;

{ Menu item "Hilfe > Programmhilfe": Display application help text }

procedure TfZodiac.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.ShowModal;
end;

{ Menu item "Hilfe > Tierkreiszeichen": Display zodiac signs table (+ elements and planets details) }

procedure TfZodiac.mHelpZodiacClick(Sender: TObject);

begin
  if fSigns.Visible then
    fSigns.Close
  else
    fSigns.ShowModal;
end;

{ Menu item "Hilfe > Datenherkunft": Display info about data origin (+ links to corr. websites) }

procedure TfZodiac.mHelpInfoClick(Sender: TObject);

begin
  if fData.Visible then
    fData.Close
  else
    fData.ShowModal;
end;

{ Menu item "Hilfe > Über": Display application about }

procedure TfZodiac.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Astrologie:' + LineEnding;
  S += 'Allgemeine Informationen zu den Sternzeichen und den charakterlichen ';
  S += 'Eigenschaften der in dem entsprechenden Zeitraum geborenen Personen.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, Oktober 2019.';
  MessageDlg('Über "Zodiac"', S, mtInformation, [mbOK], 0);
end;

{ Button "Sternzeichen": Display info about zodiac sign corr. to user entered birthday }

procedure TfZodiac.btSignClick(Sender: TObject);

var
  I: Integer;
  Filename, Compatibility, Compatibilities: string;

begin
  if (cobMonth.ItemIndex >= 1) and (cobDay.ItemIndex >= 1) then begin
    iZ := GetZodiacIndex(cobMonth.ItemIndex, cobDay.ItemIndex);                          // index to all zodiac signs data related arrays
    edSign.Text := ZodiacSigns[iZ];
    edPeriod.Text := GetZodiacPeriod(iZ);
    Filename := LowerCase(ZodiacSigns[iZ]);                                              // zodiac sign image (German filename)
    Filename := StringReplace(Filename, 'ä', 'ae', [rfReplaceAll]);
    Filename := StringReplace(Filename, 'ö', 'oe', [rfReplaceAll]);
    Filename := StringReplace(Filename, 'ü', 'ue', [rfReplaceAll]);
    Filename := './res/' + Filename + '.jpg'; DoDirSeparators(Filename);
    imSign.Picture.LoadFromFile(Filename);
    Filename := LowerCase(ZodiacSignsLatin[iZ]);                                         // zodiac symbol image (Latin filename)
    Filename := './res/' + Filename + '.png'; DoDirSeparators(Filename);
    imSymbol.Picture.LoadFromFile(Filename);
    laLatin.Caption := ZodiacSignsLatin[iZ]; laGreek.Caption := ZodiacSignsGreek[iZ];
    stEnglish.Caption := ZodiacSignsEnglish[iZ];
    // Determine and display the correct aspects for actual zodiac sign
    edSeason.Text := Seasons[(iZ - 1) div 3]; edAge.Text := Ages[(iZ - 1) div 3];
    edDuality.Text := Dualities[(iZ - 1) mod 2]; edQuadruplicity.Text := Quadruplicities[(iZ - 1) mod 3];
    edElement.Text:= Triplicities[(iZ - 1) mod 4];
    laElementsAssoc.Caption := Associations[iAssoc];
    edElementsAssoc.Text := ElementAssociations[(iZ - 1) mod 4, iAssoc];
    // Display other data for actual zodiac sign
    edPlanet.Text := ZodiacPlanets[iZ]; edColor.Text := ZodiacColors[iZ];
    edDay.Text := ZodiacDays[iZ]; edNumber.Text := IntToStr(ZodiacNumbers[iZ]);
    edPersonality.Text := ZodiacPersonalities[iZ];
    edNegChar.Text := ZodiacNegChars[iZ];
    // Display zodiac sign compatibility (as selected in the "Einstellungen" menu )
    Compatibilities := '';
    for I := 1 to 4 do begin
      Compatibility := ZodiacCompatibilities[iZ, I];
      if (mSettingsCompatibility1.Checked and (LeftStr(Compatibility, 1) = '#')) or mSettingsCompatibility2.Checked then begin
        // A '#' at position 1 indicates the "ideal partner" sign
        Compatibility := StringReplace(Compatibility, '#', '', []);
        Compatibilities += Compatibility + ', ';
      end;
    end;
    Compatibilities := LeftStr(Compatibilities, Length(Compatibilities) - 2);
    edCompatibility.Text := Compatibilities;
  end
  else
    // Error message if no birthday has been entered
    MessageDlg('Eingabefehler', 'Bitte, Geburtsdatum eingeben!', mtError, [mbOK], 0);
end;

{ Personality characteristics buttons: Display the corr. details for actual zodiac sign }

procedure TfZodiac.btChar1Click(Sender: TObject);

begin
  if iZ > 0 then begin
    DisplayCharacteristics(ZodiacSigns[iZ], 1, aButtons);
  end;
end;

procedure TfZodiac.btChar2Click(Sender: TObject);

begin
  if iZ > 0 then begin
    DisplayCharacteristics(ZodiacSigns[iZ], 2, aButtons);
  end;
end;

procedure TfZodiac.btChar3Click(Sender: TObject);

begin
  if iZ > 0 then begin
    DisplayCharacteristics(ZodiacSigns[iZ], 3, aButtons);
  end;
end;

procedure TfZodiac.btChar4Click(Sender: TObject);

begin
  if iZ > 0 then begin
    DisplayCharacteristics(ZodiacSigns[iZ], 4, aButtons);
  end;
end;

procedure TfZodiac.btChar5Click(Sender: TObject);

begin
  if iZ > 0 then begin
    DisplayCharacteristics(ZodiacSigns[iZ], 5, aButtons);
  end;
end;

{ User selection of a birth month: Adapt birthdays combobox and clear form fields }

procedure TfZodiac.cobMonthChange(Sender: TObject);

var
  D, I: Integer;
  S: string;

begin
  if cobMonth.ItemIndex >= 1 then begin
    D := cobDay.ItemIndex;
    cobDay.Clear;
    cobDay.Items.AddText('-Tag-');
    for I := 1 to DaysPerMonth[cobMonth.ItemIndex] do begin
      S := IntToStr(I);
      if I < 10 then
        S := '  ' + S;
      cobDay.Items.AddText(S);
    end;
    cobDay.ItemIndex := D;
  end;
  ClearAll(aButtons, iZ);
end;

{ User selection of a birth day: Clear form fields }

procedure TfZodiac.cobDayChange(Sender: TObject);

begin
  ClearAll(aButtons, iZ);
end;

end.

