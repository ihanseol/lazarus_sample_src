{****************************************}
{* Main unit for EuropaQuiz application *}
{****************************************}

unit euroquiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, euroflags;

const
  NCountries = 47;
  NInhGroups = 15;

type
  TEuropeN   = array[1..NCountries] of Integer;
  TEuropeR   = array[1..NCountries] of Real;
  TEuropeS   = array[1..NCountries] of string;
  TInhGroups = array[1..NInhGroups] of string;
  TInhLimits = array[0..NInhGroups] of Real;
  {********}
  { TEQuiz }
  {********}
  TEQuiz  = class(TForm)
    QuizMenu: TMainMenu;
    MenuQuiz, MenuQuizNew, MenuQuizExit: TMenuItem;
    MenuSelect, MenuSelCapital, MenuSelCapital2: TMenuItem;
    MenuSelSurface, MenuSelInhabitants, MenuSelEUEntry, MenuSelFlags: TMenuItem;
    MenuHelp, MenuHelpAbout: TMenuItem;
    Title: TStaticText;
    Label1, Label2, Label3, Label4: TLabel;
    LabelQuizSel: TLabel;
    QCountry, EditQuizItem, QEval: TEdit;
    ComboQuizItem: TComboBox;
    QQuestion, QCorrect, QPercent: TEdit;
    ButtonQuiz: TButton;
    ButtonFlags: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MenuQuizNewClick(Sender: TObject);
    procedure MenuQuizExitClick(Sender: TObject);
    procedure MenuSelCapitalClick(Sender: TObject);
    procedure MenuSelCapital2Click(Sender: TObject);
    procedure MenuSelSurfaceClick(Sender: TObject);
    procedure MenuSelInhabitantsClick(Sender: TObject);
    procedure MenuSelEUEntryClick(Sender: TObject);
    procedure MenuSelFlagsClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure ButtonQuizClick(Sender: TObject);
    procedure ButtonFlagsClick(Sender: TObject);
  private
    iQuizSelection, iQuestion, iCorrect, iCountry: Integer;
    CapitalsList, SurfacesList, SSurfaces, SInhabitants, EUEntriesList, SEUEntries: TEuropeS;
    InhabitantsList: TInhGroups;
    CountriesDone: array[1..NCountries] of Boolean;
  end;

const
  Countries: TEuropeS = (
    'Albanien', 'Andorra', 'Belgien', 'Bosnien-Herzegowina', 'Bulgarien', 'Dänemark', 'Deutschland', 'Estland',
    'Finnland', 'Frankreich', 'Griechenland', 'Großbritannien', 'Irland', 'Island', 'Italien', 'Kosovo', 'Kroatien',
    'Lettland', 'Liechtenstein', 'Litauen', 'Luxemburg', 'Mazedonien', 'Malta', 'Moldawien', 'Monaco', 'Montenegro',
    'Niederlande', 'Norwegen', 'Österreich', 'Polen', 'Portugal', 'Rumänien', 'Russland', 'San Marino', 'Schweden',
    'Schweiz', 'Serbien', 'Slowakei', 'Slowenien', 'Spanien', 'Tschechien', 'Türkei', 'Ukraine', 'Ungarn', 'Vatikan',
    'Weißrussland', 'Zypern'
  );
  Capitals: TEuropeS = (
    'Tirana', 'Andorra la Vella', 'Brüssel', 'Sarajevo', 'Sofia', 'Kopenhagen', 'Berlin', 'Tallinn', 'Helsinki', 'Paris',
    'Athen', 'London', 'Dublin', 'Reykjavik', 'Rom', 'Pristina', 'Zagreb', 'Riga', 'Vaduz', 'Vilnius', 'Luxemburg', 'Skopje',
    'Valletta', 'Chisinau', 'Monaco', 'Podgorica', 'Amsterdam', 'Oslo', 'Wien', 'Warschau', 'Lissabon', 'Bukarest',
    'Moskau', 'San Marino', 'Stockholm', 'Bern', 'Belgrad', 'Bratislava', 'Ljubljana', 'Madrid', 'Prag', 'Ankara', 'Kiew',
    'Budapest', 'Vatikanstadt', 'Minsk', 'Nikosia'
  );
  Surfaces: TEuropeR = (
    28748, 464, 30518, 51129, 110994, 43093, 357114, 45227, 338145, 547026, 131957, 244820, 70284, 103000, 301230, 10887,
    56600, 64600, 160, 65200, 2586, 25714, 316, 33843, 1.95, 13812, 41864, 323759, 83870, 312684, 92289, 238931, 17075300,
    61, 449964, 41290, 88361, 49036, 20152, 504982, 78864, 779452, 603700, 93032, 0.4, 207600, 9251
  );
  Inhabitants: TEuropeR = (
    3.5, 0.075, 10.5, 4.0, 8.0, 5.5, 82.0, 1.5, 5.0, 60.5, 10.5, 60.5, 4.0, 0.30, 58.0, 2.0, 4.5, 2.5, 0.035, 3.5, 0.5, 2.0,
    0.4, 4.5, 0.032, 0.68, 16.0, 4.5, 9.0, 38.0, 10.5, 22.0, 145.0, 0.029, 9.0, 7.5, 10.0, 5.5, 2.0, 44.0, 10.5, 70.5, 47.5,
    11.0, 0.0008, 10.5, 0.78
  );
  // 3000/4000 = candidates; 5000 = not EU member
  EUEntries: TEuropeN = (
  3000, 5000, 1958, 4000, 2007, 1973, 1958, 2004, 1995, 1958, 1981, 1973, 1973, 5000, 1958, 4000, 2013, 2004, 5000, 2004, 1958,
  3000, 2004, 5000, 5000, 4000, 1958, 5000, 1995, 2004, 1986, 2007, 5000, 5000, 1995, 5000, 4000, 2004, 2004, 1986, 2004, 4000,
  5000, 2004, 5000, 5000, 2004
  );
  InhabitantsGroups: TInhGroups = (
    'weniger als 50.000', '50.000 - 500.000', '500.000 - 1 Mio', '1 Mio - 2,5 Mio', '2,5 - 5 Mio', '5 - 7,5 Mio', '7,5 - 10 Mio',
    '10 - 15 Mio', '15 - 20 Mio', '20 - 30 Mio', '30 - 40 Mio', '40 - 50 Mio', '50 - 60 Mio', '60 - 75 Mio', 'mehr als 75 Mio');
  InhabitantsLimits: TInhLimits = (
    0, 0.050, 0.5, 1, 2.5, 5, 7.5, 10, 15, 20, 30, 40, 50, 60, 75, 200);

var
  EQuiz: TEQuiz;

implementation

{$R *.lfm}

{ Reset the form controls }

procedure ClearForm(var NQ, NC: Integer);

begin
  // Set questions and correct answers to 0
  NQ := 0; NC := 0;
  // Reset the form controls
  EQuiz.QCountry.Text := '';  Equiz.EditQuizItem.Text := ''; Equiz.ComboQuizItem.ItemIndex := -1; EQuiz.QEval.Text := '';
  EQuiz.QQuestion.Text := ''; EQuiz.QCorrect.Text := '';
  EQuiz.QPercent.Text := '';  EQuiz.QPercent.Color := clDefault;
  EQuiz.MenuSelect.Enabled := True;                                            // enable quiz selections
  EQuiz.ButtonQuiz.Caption := 'Frage'; EQuiz.ButtonQuiz.Enabled := True;
end;

{ Quiz selection: Display edit field or selection combobox }

procedure QuizSelection(Selection: Integer; var QCapitals, QSurfaces, QEUEntries: TEuropeS; var QInhabitants: TInhGroups);

const
  QuizSelections1: array[1..6] of string = (
    'Hauptstädte', 'Hauptstädte', 'Landesfläche', 'Einwohnerzahl', 'EU Beitrittsjahr', 'Nationalflaggen');
  QuizSelections2: array[1..6] of string = (
    'Hauptstadt', 'Hauptstadt', 'Landesfläche', 'Einwohnerzahl', 'EU Beitritt', 'Nationalflagge');

var
  I: Integer;
  OldEntry: string;

begin
  EQuiz.Title.Caption := 'Europa Quiz : ' + QuizSelections1[Selection] + '.';
  EQuiz.LabelQuizSel.Caption := QuizSelections2[Selection];
  // Capitals direct entry or flags
  if (Selection = 2) or (Selection = 6) then begin
    // User answer will be retrieved from edit field
    EQuiz.EditQuizItem.Visible := True;
    EQuiz.EditQuizItem.Text := '';
    EQuiz.ComboQuizItem.Visible := False;
    // Do or do not display the button to select the flags
    if Selection = 2 then
      EQuiz.ButtonFlags.Visible := False
    else
      EQuiz.ButtonFlags.Visible := True;
  end
  // Capitals, surfaces, inhabitants or EU entry from list
  else begin
    EQuiz.EditQuizItem.Visible := False;
    EQuiz.ButtonFlags.Visible := False;
    EQuiz.ComboQuizItem.Visible := True;                                       // combobox for the list
    EQuiz.ComboQuizItem.Clear;
    case Selection of
      1: begin
           for I := 1 to NCountries do
             EQuiz.ComboQuizItem.Items.Append(QCapitals[I]);                   // capitals list
         end;
      3: begin
           for I := 1 to NCountries do
             EQuiz.ComboQuizItem.Items.Append(QSurfaces[I]);                   // surfaces list
         end;
      4: begin
           for I := 1 to NInhGroups do
             EQuiz.ComboQuizItem.Items.Append(QInhabitants[I]);                // inhabitants list
         end;
      5: begin
           OldEntry := '';
           for I := 1 to NCountries do begin
             // Remove doubles
             if QEUEntries[I] <> OldEntry then begin
               EQuiz.ComboQuizItem.Items.Append(QEUEntries[I]);                // EU entry-years list
               OldEntry := QEUEntries[I];
             end;
           end;
         end;
    end;
  end;
  EQuiz.ButtonQuiz.Caption := 'Frage';
end;

{ Get quiz answer (depending on quiz selected) }

procedure QuizAnswer(Quiz, Country: Integer; var Capitals, Surfaces, EUEntries, Inhabitants: TEuropeS; var Answer: string);

begin
  Answer := '';
  case Quiz of
    1, 2: Answer := Capitals[Country];
       3: Answer := Surfaces[Country];
       4: Answer := Inhabitants[Country];
       5: Answer := EUEntries[Country];
       6: Answer := IntToStr(Country) + '.png';
  end;
end;

{ Format country surfaces }

procedure SurfacesFormat(RSurfaces: TEuropeR; var SSurfaces: TEuropeS);

var
  I, J, K: Integer;
  STemp1, STemp2: string;

begin
  for I := 1 to NCountries do begin
    STemp1 := FloatToStr(RSurfaces[I]);
    if RSurfaces[I] > 1000 then begin
      K := 0; STemp2 := '';
      for J := Length(STemp1) downto 1 do begin
        Inc(K);
        STemp2 := STemp1[J] + STemp2;
        if (J <> 1) and (K mod 3 = 0) then
          STemp2 := '.' + STemp2;                                              // use '.' to separate thousands
      end;
      STemp1 := STemp2;
    end;
    STemp1 += ' km2';                                                          // add 'km2'
    SSurfaces[I] := STemp1;
  end;
end;

{ Format EU entry years }

procedure EUEntriesFormat(NEUEntries: TEuropeN; var SEUEntries: TEuropeS);

var
  I: Integer;

begin
  for I := 1 to NCountries do begin
    // EU members: Entry year
    if NEUEntries[I] < 3000 then
      SEUEntries[I] := 'EU Mitglied seit ' + IntToStr(NEUEntries[I])
    // No or not yet (December 2019) members
    else begin
      if NEUEntries[I] = 3000 then
        SEUEntries[I] := 'Offizieller Beitrittskandidat'
      else if NEUEntries[I] = 4000 then
        SEUEntries[I] := 'Potentieller Beitrittskandidat'
      else
        SEUEntries[I] := 'Nicht Mitglied der EU';
    end;
  end;
end;

{********}
{ TEQuiz }
{********}

{ Application start: Create lists and other initialisations }

procedure TEQuiz.FormCreate(Sender: TObject);

var
  I, J, NTemp: Integer;
  RTemp: Real;
  STemp: string;
  EuropeN: TEuropeN;
  EuropeR: TEuropeR;

begin
  // Sorted capitals list (for combobox)
  CapitalsList := Capitals;
  for I := 1 to NCountries - 1 do begin
    for J := I to NCountries do begin
      if CapitalsList[J] < CapitalsList[I] then begin
        STemp := CapitalsList[I]; CapitalsList[I] := CapitalsList[J]; CapitalsList[J] := STemp;
      end;
    end;
  end;
  // Formatted surfaces list for answer-check (country index) and for combobox (sorted)
  EuropeR := Surfaces;
  for I := 1 to NCountries - 1 do begin
    for J := I to NCountries do begin
      if EuropeR[J] < EuropeR[I] then begin
        RTemp := EuropeR[I]; EuropeR[I] := EuropeR[J]; EuropeR[J] := RTemp;
      end;
    end;
  end;
  SurfacesFormat(EuropeR, SurfacesList);
  SurfacesFormat(Surfaces, SSurfaces);
  // Group-like inhabitants lists for answer-check (country index) and for combobox (the different groups)
  InhabitantsList := InhabitantsGroups;
  for I := 1 to NCountries do begin
    for J := 1 to NInhGroups do
      if (Inhabitants[I] >= InhabitantsLimits[J - 1]) and
         (Inhabitants[I] <= InhabitantsLimits[J]) then
         SInhabitants[I] := InhabitantsGroups[J];
  end;
  // Formatted EU-entries list for answer-check (country index) and for combobox (sorted)
  EuropeN := EUEntries;
  for I := 1 to NCountries - 1 do begin
    for J := I to NCountries do begin
      if EuropeN[J] < EuropeN[I] then begin
        NTemp := EuropeN[I]; EuropeN[I] := EuropeN[J]; EuropeN[J] := NTemp;
      end;
    end;
  end;
  EUEntriesFormat(EuropeN, EUEntriesList);
  EUEntriesFormat(EUEntries, SEUEntries);
  // Variable initialisation
  iQuizSelection := 1;
  QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  ClearForm(iQuestion, iCorrect);
  Randomize;
end;

{ Menu item "Quiz > Neu": Reset the form controls }

procedure TEQuiz.MenuQuizNewClick(Sender: TObject);

begin
  ClearForm(iQuestion, iCorrect);
end;

{ Menu item "Quiz > Verlassen": Exit the application }

procedure TEQuiz.MenuQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Auswahl > Hauptstädte (Liste)": Capitals quiz (selection from list) }

procedure TEQuiz.MenuSelCapitalClick(Sender: TObject);

begin
  if not MenuSelCapital.Checked then begin
    MenuSelCapital.Checked     := True;  MenuSelCapital2.Checked    := False;
    MenuSelSurface.Checked     := False; MenuSelInhabitants.Checked := False;
    MenuSelEUEntry.Checked     := False; MenuSelFlags.Checked       := False;
    iQuizSelection := 1; QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  end;
end;

{ Menu item "Auswahl > Hauptstädte (Eingabe)": Capitals quiz (data entry) }

procedure TEQuiz.MenuSelCapital2Click(Sender: TObject);

begin
  if not MenuSelCapital2.Checked then begin
    MenuSelCapital2.Checked    := True;  MenuSelCapital.Checked     := False;
    MenuSelSurface.Checked     := False; MenuSelInhabitants.Checked := False;
    MenuSelEUEntry.Checked     := False; MenuSelFlags.Checked       := False;
    iQuizSelection := 2; QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  end;
end;

{ Menu item "Auswahl > Landesfläche": Country surface quiz (selection from list) }

procedure TEQuiz.MenuSelSurfaceClick(Sender: TObject);

begin
  if not MenuSelSurface.Checked then begin
    MenuSelSurface.Checked     := True;  MenuSelCapital.Checked     := False;
    MenuSelCapital2.Checked    := False; MenuSelInhabitants.Checked := False;
    MenuSelEUEntry.Checked     := False; MenuSelFlags.Checked       := False;
    iQuizSelection := 3; QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  end;
end;

{ Menu item "Auswahl > Einwohnerzahl": Inhabitants quiz (selection from group-like list) }

procedure TEQuiz.MenuSelInhabitantsClick(Sender: TObject);

begin
  if not MenuSelInhabitants.Checked then begin
    MenuSelInhabitants.Checked := True;  MenuSelCapital.Checked     := False;
    MenuSelCapital2.Checked    := False; MenuSelSurface.Checked     := False;
    MenuSelEUEntry.Checked     := False; MenuSelFlags.Checked       := False;
    iQuizSelection := 4; QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  end;
end;

{ Menu item "Auswahl > EU Beitrittsjahr": EU-entry quiz (selection from list) }

procedure TEQuiz.MenuSelEUEntryClick(Sender: TObject);

begin
  if not MenuSelEUEntry.Checked then begin
    MenuSelEUEntry.Checked     := True;  MenuSelCapital.Checked     := False;
    MenuSelCapital2.Checked    := False; MenuSelSurface.Checked     := False;
    MenuSelInhabitants.Checked := False; MenuSelFlags.Checked       := False;
    iQuizSelection := 5; QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  end;
end;

{ Menu item "Auswahl > Nationalflaggen": Flag quiz (flag image selection) }

procedure TEQuiz.MenuSelFlagsClick(Sender: TObject);

begin
  if not MenuSelFlags.Checked then begin
    MenuSelFlags.Checked       := True;  MenuSelCapital.Checked     := False;
    MenuSelCapital2.Checked    := False; MenuSelSurface.Checked     := False;
    MenuSelInhabitants.Checked := False; MenuSelEUEntry.Checked     := False;
    iQuizSelection := 6; QuizSelection(iQuizSelection, CapitalsList, SurfacesList, EUEntriesList, InhabitantsList);
  end;
end;

{ Menu item "Hilfe > Info": Display about info }

procedure TEQuiz.MenuHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Quiz zum Thema "Die Länder Europas".' + LineEnding;
  S += 'Hauptstädte, Landesfläche, Einwohnerzahl, EU Beitrittsjahr, Flaggen' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, Januar 2018 - März 2020.';
  MessageDlg('Über "EuropaQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Frage/Antwort": Do the quiz (display question or check user's answer) }

procedure TEQuiz.ButtonQuizClick(Sender: TObject);

var
  I: Integer;
  Percent: Real;
  CorrectAnswer, UserAnswer: string;

begin
  // Button "Frage": Display the quiz question
  if Copy(ButtonQuiz.Caption, 1, 5) = 'Frage' then begin
    Inc(iQuestion);
    // Not yet all countries done: continue the quiz
    if iQuestion <= NCountries then begin
      // Get a random country
      if iQuestion = 1 then begin
        for I := 1 to NCountries do
          CountriesDone[I] := False;                                           // at start of quiz, set all countries to "not done"
        MenuSelect.Enabled := False;                                           // disable the quiz selection menu
      end;
      repeat
        iCountry := Random(NCountries) + 1;
      until not CountriesDone[iCountry];                                       // the random country must not be already done
      CountriesDone[iCountry] := True;                                         // mark the country as "done"
      // Clear all answer fields
      QCountry.Text := Countries[iCountry];
      case iQuizSelection of
        1, 3, 4, 5: ComboQuizItem.Text := '';
              2, 6: EditQuizItem.Text  := '';
      end;
      QEval.Text := '';
      // Next button action will be to check user's answer
      ButtonQuiz.Caption := 'Antwort';
    end;
  end
  // Button "Antwort": Check user's answer
  else begin
    case iQuizSelection of
      1, 3, 4, 5: UserAnswer := ComboQuizItem.Text;                            // answer retrieved from edit field
            2, 6: UserAnswer := EditQuizItem.Text;                             // answer retrieved from combobox
    end;
    // Get answer for actual quiz and actual country
    QuizAnswer(iQuizSelection, iCountry, Capitals, SSurfaces, SEUEntries, SInhabitants, CorrectAnswer);
    // Check user's answer
    if UserAnswer = CorrectAnswer then begin
      // Answer is correct
      Inc(iCorrect);
      QEval.Font.Color := clLime;
      QEval.Text := 'Richtig!';
      QCorrect.Text := IntToStr(ICorrect);
    end
    else begin
      // Answer is false
      QEval.Font.Color := clRed;
      QEval.Text := 'Falsch! Richtige Antwort = ' + CorrectAnswer;
    end;
    // Update questions done, correct answers and success percentage
    QQuestion.Text := IntToStr(iQuestion);
    QCorrect.Text := IntToStr(iCorrect);
    Percent := 100 * iCorrect / iQuestion;
    Percent := Int(100 * Percent) / 100;
    QPercent.Text := FloatToStr(Percent) + ' %';
    if Percent < 50 then
       QPercent.Color := clRed
    else if Percent < 60 then
       QPercent.Color := clYellow
    else
       QPercent.Color := clLime;
    // Next button action will be to display a question
    ButtonQuiz.Caption := 'Frage';
    // All countries done: terminate the quiz
    if iQuestion = NCountries then begin
      MessageDlg('Europa Quiz beendet','Alle ' + IntToStr(NCountries) + ' Länder abgefragt', mtInformation, [mbOK], 0);
      ButtonQuiz.Enabled := False;                                             // disable the button ("New" command will set controls ready for new quiz)
    end;
  end;
end;

{ Button "Flagge": Open second form to select a flag image }

procedure TEQuiz.ButtonFlagsClick(Sender: TObject);

begin
  EFlags.ShowModal;
  EditQuizItem.Text := IntToStr(EFlags.iCountry) + '.png';                     // use flag filename as user answer
end;

end.

