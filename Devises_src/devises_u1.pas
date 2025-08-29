{*************************************}
{* Main unit for Devises application *}
{*************************************}

unit devises_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Menus, Grids, devises_u2, devises_u3;

type
  TDevise = record
    Country, Devise, DeviseLong: string;
    Cours: Real;
  end;
  TDevises = array of TDevise;
  {***********}
  { TfDevises }
  {***********}
  TfDevises = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileUpdate, mFileExit: TMenuItem;
    mOptions, mOptionsSort, mOptionsSelLimit, MenuItem2, mOptionsConsole: TMenuItem;
    mOptionsSortDefault, mOptionsSortDevise, mOptionsSortCurrency, mOptionsSortCountry: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    grDevises1, grDevises2: TStringGrid;
    Label3, Label4, Label5, Label6: TLabel;
    Shape1: TShape;
    edDepart: TEdit;
    edArrivee: TEdit;
    cobDepart: TComboBox;
    cobArrivee: TComboBox;
    btCalc: TButton;
    prCmd: TProcess;
    tiCmd: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure mFileUpdateClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsSortDefaultClick(Sender: TObject);
    procedure mOptionsSortDeviseClick(Sender: TObject);
    procedure mOptionsSortCurrencyClick(Sender: TObject);
    procedure mOptionsSortCountryClick(Sender: TObject);
    procedure mOptionsSelLimitClick(Sender: TObject);
    procedure mOptionsConsoleClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCalcClick(Sender: TObject);
    procedure tiCmdTimer(Sender: TObject);
  private
    sDate, sSort: string;
    aDevises: TDevises;
  end;

const
  NDevises = 31; NDevisesPrinc = 16;
  CDevises: array[0 .. NDevises - 1] of string = (
    'USD', 'CAD', 'GBP', 'CHF', 'DKK', 'NOK', 'SEK', 'HUF', 'PLN', 'CZK',
    'JPY', 'ZAR', 'AUD', 'NZD', 'HKD', 'SGD', 'BGN', 'RON', 'HRK', 'TRY',
    'MXN', 'BRL', 'CNY', 'RUB', 'ILS', 'KRW', 'INR', 'IDR', 'MYR', 'PHP', 'THB'
  );
  CDevisesPrinc: array[0 .. NDevisesPrinc - 1] of string = (
    'USD', 'GBP', 'CHF', 'SEK', 'NOK', 'DKK', 'CAD', 'ZAR', 'JPY', 'AUD', 'NZD', 'HKD', 'SGD', 'PLN', 'HUF', 'CZK'
  );
  CCountries: array[0 .. NDevises - 1] of string = (
    'Etats-Unis', 'Canada', 'Grande-Bretagne', 'Suisse', 'Danmark', 'Norvège', 'Suède', 'Hongrie', 'Pologne', 'République tchèque',
    'Japon',  'Afrique du Sud', 'Australie', 'Nouvelle-Zélande', 'Hong-Kong', 'Singapour', 'Bulgarie', 'Roumanie', 'Croatie', 'Turquie',
    'Mexique', 'Brésil', 'Chine', 'Russie', 'Israël', 'Corée du Sud', 'Inde', 'Indonésie', 'Malaisie', 'Philippines', 'Thaïlande'
  );

var
  fDevises: TfDevises;

implementation

{$R *.lfm}

{ Right-align rates in the grid }

function AlignRight(R: Real): string;

var
  S: string;

begin
  S := FloatToStrF(R, ffNumber, 4, 4);
  if R < 10 then
    S := '     ' + S
  else if R < 100 then
    S := '    ' + S
  else if R < 1000 then
    S := '   ' + S
  else if R < 10000 then
    S := ' ' + S;
  S := ' ' + S;
  AlignRight := S;
end;

{ Read currencies from text file devises.txt }

procedure DevisesRead(out DDate: string; out Devises: TDevises);

var
  L, I, J: Integer;
  S: string;
  FDevises: Text;

begin
  SetLength(Devises, 0);
  Assign(FDevises, 'devises.txt'); Reset(FDevises);
  L := 0;
  while not EoF(FDevises) do begin
    Readln(FDevises, S);
    if S <> '' then begin
      Inc(L);
      // First line is date
      if L = 1 then
        DDate := S
      // Other lines are currencies
      else begin
        I := Length(Devises);
        SetLength(Devises, I + 1);
        with Devises[I] do begin
          Devise := Copy(S, 1, 3);                                             // currency 3-letter code
          DeviseLong := Copy(S, 5, 25); Trim(DeviseLong);                      // currency (all-letter) name
          Cours := StrToFloat(Copy(S, 31, Length(S) - 30));                    // currency rate (for 1€)
          // Country is not in file (will be set by program)
          Country := 'Pays inconnu';
          for J := 0 to NDevises - 1 do
            if Devise = CDevises[J] then
              Country := CCountries[J];
        end;
      end;
    end;
  end;
  Close(FDevises);
end;

{ Sort currencies (as selected by the user) }

procedure DevisesSort(Order: string; var Devises: TDevises);

var
  I, J: Integer;
  Item1, Item2: string;
  TempDevise: TDevise;

begin
  // "Arbitrary" default sort order
  if Order = 'default' then begin
    for I := 0 to NDevises - 1 do begin
      for J := I to Length(Devises) - 1 do begin
        if Devises[J].Devise = CDevises[I] then begin
          TempDevise := Devises[J]; Devises[J] := Devises[I]; Devises[I] := TempDevise;
        end;
      end;
    end;
  end
  // Other sort orders
  else begin
    for I := 1 to Length(Devises) - 1 do begin
      for J := 0 to I do begin
        // Sort by currency code
        if Order = 'devise' then begin
          Item1 := Devises[J].Devise; Item2 := Devises[I].Devise;
        end
        // Sort by currency name
        else if Order = 'currency' then begin
          Item1 := Devises[J].DeviseLong; Item2 := Devises[I].DeviseLong;
        end
        // Sort by country name
        else if Order = 'country' then begin
          Item1 := Devises[J].Country; Item2 := Devises[I].Country;
        end;
        if Item1 > Item2 then begin
          TempDevise := Devises[J]; Devises[J] := Devises[I]; Devises[I] := TempDevise;
        end;
      end;
    end;
  end;
end;

{ Display actual currency rates in the 2 tables (grids) }

procedure DevisesDisplay(var Devises: TDevises; SDate: string);

var
  I: Integer;

begin
  fDevises.stTitle.Caption := 'Cours de change Euro (EUR) - Banque de France (' + SDate + ')';
  // "Devises principales" displayed in first table
  for I := 0 to NDevisesPrinc - 1 do begin
    with Devises[I] do begin
      fDevises.grDevises1.Cells[0, I + 1] := Country;
      fDevises.grDevises1.Cells[1, I + 1] := Devise;
      fDevises.grDevises1.Cells[2, I + 1] := DeviseLong;
      fDevises.grDevises1.Cells[3, I + 1] := AlignRight(Cours);
    end;
  end;
  // Other currencies displayed in second table
  for I := NDevisesPrinc to Length(Devises) - 1 do begin
    with Devises[I] do begin
      fDevises.grDevises2.Cells[0, I - 15] := Country;
      fDevises.grDevises2.Cells[1, I - 15] := Devise;
      fDevises.grDevises2.Cells[2, I - 15] := DeviseLong;
      fDevises.grDevises2.Cells[3, I - 15] := AlignRight(Cours);
    end;
  end;
end;

{ Fill the comboboxes with currency selections }

procedure ComboboxFill(Devises: TDevises);

var
  I: Integer;

begin
  fDevises.cobDepart.Clear; fDevises.cobArrivee.Clear;
  fDevises.cobDepart.Items.AddText('EUR');
  fDevises.cobArrivee.Items.AddText('EUR');
  // Fill-in "devises principales" only
  if fDevises.mOptionsSelLimit.Checked then begin
    for I := 0 to NDevisesPrinc - 1 do begin
      fDevises.cobDepart.Items.AddText(CDevisesPrinc[I]);
      fDevises.cobArrivee.Items.AddText(CDevisesPrinc[I]);
    end;
  end
  // Fill-in all currencies
  else begin
    for I := 0 to Length(Devises) - 1 do begin
      fDevises.cobDepart.Items.AddText(Devises[I].Devise);
      fDevises.cobArrivee.Items.AddText(Devises[I].Devise);
    end;
  end;
  // Set "devise de départ" to 'EUR', "devise d'arrivée" to 'USD'
  fDevises.cobDepart.ItemIndex := 0; fDevises.cobArrivee.ItemIndex := 1;
end;

{ Extract currencies from download file devises.tmp and write them into devises.txt }

procedure DevisesReload(out DDate: string; out Devises: TDevises);

var
  Count, TagStart, TagEnd, JX, I, J, P: Integer;
  Line, Devise, DeviseLong, SCours: string;
  Found, DateDone, TableStart, TableEnd, TableFirst, DeviseDone, Done: Boolean;
  InFile, OutFile: Text;

begin
  // First backup devises.txt to devises.bak
  Assign(InFile, 'devises.txt'); Reset(InFile);
  Assign(OutFile, 'devises.bak'); Rewrite(OutFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    Writeln(OutFile, Line);
  end;
  Close(InFile); Close(OutFile);
  SetLength(Devises, 0);
  // Now start devises.txt reload from devises.tmp
  Assign(InFile, 'devises.tmp'); Reset(InFile);
  Assign(OutFile, 'devises.txt'); Rewrite(OutFile);
  Line := ''; Done := False;
  Found := False; DateDone := False; TableStart := False; TableEnd := False; TableFirst := True; Count := 0;
  // Parse the downloaded HTML file and try to find and extract the date and the currency rates
  while not Done do begin
    if EoF(InFile) then
      Done := True
    else begin
      Readln(InFile, Line);
      if not DateDone then begin
        // Use text 'Mise en ligne le' to find the rates date
        P := Pos('Mise en ligne le', Line);
        if P > 0 then begin
          Line := Trim(Copy(Line, P + 17, 10));
          Line := StringReplace(Line, '/', '.', [rfReplaceAll]);
          Writeln(OutFile, Line); Writeln(OutFile);
          DateDone := True;
        end;
      end
      // Consider that the first table (HTML tag '<table') following the date contains the currencies and rates
      else begin
        P := Pos('<table', Line);
        if P > 0 then
          Found := True;
      end;
      // If this table has been found, parse following HTML lines to extract the currencies
      if Found then begin
        if LeftStr(Line, 7) = '<tbody>' then
          // Table data starts after this tag
          TableStart := True
        else if LeftStr(Line, 8) = '</tbody>' then
          // Table data ends after this tag
          TableEnd := True;
        // Lines between table start and table end are supposed to contain the currency data
        // Format expected: 1 HTML row for each currency, columns being the currency name, the code and several rates
        if TableStart and not TableEnd then begin
          if LeftStr(Line, 4) = '<tr>' then begin
            // HTML tag '<tr>' strats a new row (i.e. a new currency)
            DeviseDone := False; JX := 0;
            repeat
              Readln(InFile, Line);
              if LeftStr(Line, 3) = '<td' then begin
                // HTML tag beginning with '<td' starts a new column (determined data concerning this row's currency)
                if not TableFirst then begin
                  // Ignore first row (containing the table headers)
                  TagStart := -1;
                  // Remove all HTML tags (keeping only the table value)
                  for I := 1 to Length(Line) do begin
                    if Line[I] = '<' then
                      TagStart := I;
                    if TagStart > 0 then begin
                      if Line[I] = '>' then begin
                        TagEnd := I;
                        for J := TagStart to TagEnd do
                          Line[J] := '#';
                        TagStart := -1;
                      end;
                    end;
                  end;
                  Line := StringReplace(Line, '#', '', [rfReplaceAll]);
                  // The position of the column is supposed indicating the type of value (name, code or rate)
                  Inc(JX);
                  case JX of
                      1: DeviseLong := Line;                                   // first column = currency name
                      2: Devise := Line;                                       // second column = currency code
                    else SCours := Line;                                       // all following = rates (the last one being the actual one to keep)
                  end;
                end;
              end
              // As Readln is repeated after each '<tr>' detection, a line not starting with '<td' normally can't
              // be sth else than '</tr>', indicating the end of the row (i.e. end of data for this currency)
              else
                DeviseDone := True;
            until DeviseDone;
            // End of row tag '</tr>' indicates that all data for actual currency has been read
            if LeftStr(Line, 5) = '</tr>' then begin
              // Header row does not contain currency data
              if TableFirst then
                TableFirst := False
              // With the info extracted from data rows, write line of devises.txt file
              else begin
                Inc(Count);
                // Consider the 31 first currencies listed only
                if Count <= NDevises then begin
                  for I := 1 to 25 - Length(DeviseLong) do
                    DeviseLong += ' ';
                  // As non-Engish characters are incorrectly counted by the Length function, manually add a space for proper items alignment in the devises.txt file
                  for I := 1 to Length(DeviseLong) do begin
                    if (Copy(DeviseLong, I, 2) = 'é') or (Copy(DeviseLong, I, 2) = 'è') or (Copy(DeviseLong, I, 2) = 'ë') or (Copy(DeviseLong, I, 2) = 'ï') then
                      DeviseLong += ' ';
                  end;
                  Line := Devise + ' ' + DeviseLong;
                  // Remove seperators in rates values (thousands are actually separated by a space, could also be by a dot)
                  for I := 1 to Length(SCours) do begin
                    if not (SCours[I] in ['0' .. '9', ',']) then
                      SCours[I] := '#';
                  end;
                  SCours := StringReplace(SCours, '#', '', [rfReplaceAll]);
                  Line += ' ' + SCours;
                  // Write line for actual currency to devises.txt file
                  Writeln(OutFile, Line);
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    if TableEnd then
      Done := True;
  end;
  Close(InFile); Close(OutFile);
  // Check if parsing did find 31 currencies
  if Count < NDevisesPrinc then begin
    // If there are less than 16 currencies found, consider file format has changed and parsing was a failure
    // In this case, reload the old devises.txt file (from devises.bak created before)
    MessageDlg('Erreur mise-à-jour', 'Le fichier téléchargé ne contient pas les données prévues!', mtError, [mbOK], 0);
    Assign(InFile, 'devises.bak'); Reset(InFile);
    Assign(OutFile, 'devises.txt'); Rewrite(OutFile);
    while not EoF(InFile) do begin
      Readln(InFile, Line);
      Writeln(OutFile, Line);
    end;
    Close(InFile); Close(OutFile);
  end
  else if Count <> NDevises then begin
    // Display warning if there more or less currencies than the expected value of 31
    if Count < NDevises then
      MessageDlg('Problème mise-à-jour', 'Le fichier téléchargé ne contient pas toutes les devises!', mtWarning, [mbOK], 0)
    else
      MessageDlg('Problème mise-à-jour', 'Le fichier téléchargé contient des devises supplémentaires!', mtWarning, [mbOK], 0);
  end;
  // Read the new devises.txt file
  DevisesRead(DDate, Devises);
  // Sort the values in the tables and comboboxes
  fDevises.mOptionsSortDefault.Click;
  DevisesSort('default', Devises);
  // Display the tables with the new rate values
  DevisesDisplay(Devises, DDate);
  // Re-fill the comboboxes
  ComboboxFill(Devises);
end;

{ Currency rates update: Activate the command line process starting wget.bat) }

procedure DevisesUpdate;

const
  Cmd = 'wget.bat' + LineEnding;

begin
  // Activate the process
  if not fDevises.prCmd.Active then
    fDevises.prCmd.Active := True;
  // Activate the timer (its routine will read the processes output)
  fDevises.tiCmd.Enabled := True;
  // Clear the 'console' memo
  fDownload.memoCmd.Clear;
  // Display the 'console' memo (if this option is selectd)
  if fDevises.mOptionsConsole.Checked then
    fDownload.Show;
  // Run 'wget.bat' (that calls wget.exe with all relevant parameters)
  fDevises.prCmd.Input.Write(Cmd[1], Length(Cmd))
end;

{ Get rate for given currency }

function GetCours(Devise: string; Devises: TDevises): Real;

var
  I: Integer;
  Cours: Real;

begin
  for I := 0 to Length(Devises) - 1 do
    if Devise = Devises[I].Devise then
      Cours := Devises[I].Cours;
  GetCours := Cours;
end;

{***********}
{ TfDevises }
{***********}

{ Application start: Initialisation }

procedure TfDevises.FormCreate(Sender: TObject);

begin
  // Set parameters for the command line process
  prCmd.Options := [poUsePipes];
  prCmd.ShowWindow:= swoHIDE;
  prCmd.Executable:= 'cmd';
  // Read currency data from devises.txt
  SetLength(aDevises, 0);
  DevisesRead(sDate, aDevises);
  // Sort and display currencies; fill comboboxes
  sSort := 'default';
  DevisesSort(sSort, aDevises);
  DevisesDisplay(aDevises, sDate);
  ComboboxFill(aDevises);
end;

{ Menu item "Fichier > Mise-à-jour": Update currency rates }

procedure TfDevises.mFileUpdateClick(Sender: TObject);

begin
  DevisesUpdate;
end;

{ Menu item "Fichier > Quitter": Exit application }

procedure TfDevises.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Options > Tri des données dans les tableaux > ...": Sort currencies }

procedure TfDevises.mOptionsSortDefaultClick(Sender: TObject);

begin
  if not mOptionsSortDefault.Checked then begin
    mOptionsSortDefault.Checked := True;
    mOptionsSortDevise.Checked := False;
    mOptionsSortCurrency.Checked := False;
    mOptionsSortCountry.Checked := False;
    sSort := 'default';
    DevisesSort(sSort, aDevises); DevisesDisplay(aDevises, sDate);
    ComboboxFill(aDevises);
  end;
end;

procedure TfDevises.mOptionsSortDeviseClick(Sender: TObject);

begin
  if not mOptionsSortDevise.Checked then begin
    mOptionsSortDefault.Checked := False;
    mOptionsSortDevise.Checked := True;
    mOptionsSortCurrency.Checked := False;
    mOptionsSortCountry.Checked := False;
    sSort := 'devise';
    DevisesSort(sSort, aDevises); DevisesDisplay(aDevises, sDate);
    ComboboxFill(aDevises);
  end;
end;

procedure TfDevises.mOptionsSortCurrencyClick(Sender: TObject);

begin
  if not mOptionsSortCurrency.Checked then begin
    mOptionsSortDefault.Checked := False;
    mOptionsSortDevise.Checked := False;
    mOptionsSortCurrency.Checked := True;
    mOptionsSortCountry.Checked := False;
    sSort := 'currency';
    DevisesSort(sSort, aDevises); DevisesDisplay(aDevises, sDate);
    ComboboxFill(aDevises);
  end;
end;

procedure TfDevises.mOptionsSortCountryClick(Sender: TObject);

begin
  if not mOptionsSortCountry.Checked then begin
    mOptionsSortDefault.Checked := False;
    mOptionsSortDevise.Checked := False;
    mOptionsSortCurrency.Checked := False;
    mOptionsSortCountry.Checked := True;
    sSort := 'country';
    DevisesSort(sSort, aDevises); DevisesDisplay(aDevises, sDate);
    ComboboxFill(aDevises);
  end;
end;

{ Menu item "Options > Limiter la sélection aux devises 'principales'": Toggle display 'devises principales'/all currencies in comboboxes}

procedure TfDevises.mOptionsSelLimitClick(Sender: TObject);

begin
  if mOptionsSelLimit.Checked then
    mOptionsSelLimit.Checked := False
  else
    mOptionsSelLimit.Checked := True;
  ComboboxFill(aDevises);
end;

{ Menu item "Options > Montrer la console lors de la mise-à-jour": Toggle show/show not the 'console' window during update }

procedure TfDevises.mOptionsConsoleClick(Sender: TObject);

begin
  if mOptionsConsole.Checked then
    mOptionsConsole.Checked := False
  else
    mOptionsConsole.Checked := True;
end;

{ Menu item "Aide > Aide programme": Display program help }

procedure TfDevises.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide
  else
    fHelp.Show;
end;

{ Menu item "Aide > Info programme": Display program about }

procedure TfDevises.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := '"Devises" est un convertisseur de devises, se basant sur les cours de change publiés ';
  S += 'sur le site Web de la Banque de France (https://www.banque-france.fr).' + Chr(13) + Chr(13);
  S += 'Version 1.0, © allu, juin 2018 - janvier 2019.';
  MessageDlg('About "Devises"', S, mtInformation, [mbOK], 0);
end;

{ Button "Calculer": Do conversion from "devise de départ" to "devise d'arrivée"}

procedure TfDevises.btCalcClick(Sender: TObject);

var
  Montant1, Montant2, Cours1, Cours2: Real;
  Devise1, Devise2: string;

begin
  edArrivee.Text := '';
  if edDepart.Text = '' then
    Montant1 := 0
  else
    Montant1 := StrToFloat(edDepart.Text);
  if Montant1 <> 0 then begin
    Devise1  := cobDepart.Text; Devise2  := cobArrivee.Text;
    if Devise1 = Devise2 then begin
      Montant2 := Montant1;
      MessageDlg('Données invalides', 'Pas sensé de convertir des ' + Devise1 + ' en ' + Devise2 + '!', mtWarning, [mbOK], 0);
    end
    else begin
      if Devise1 = 'EUR' then
        Cours1 := 1
      else
        Cours1 := GetCours(Devise1, aDevises);
      if Devise2 = 'EUR' then
        Cours2 := 1
      else
        Cours2 := GetCours(Devise2, aDevises);
      Montant2 := (1 / Cours1) * Montant1 * Cours2;
    end;
    edArrivee.Text := FloatToStrF(Montant2, ffNumber, 0, 2);
  end
  else
    MessageDlg('Données invalides', 'Il faut bien entrer un montant!', mtError, [mbOK], 0);
end;

{ Timer routine: Get output of wget process }

procedure TfDevises.tiCmdTimer(Sender: TObject);

var
  I: Integer;
  Finished, OK: Boolean;
  Buffer: array[0..65535] of Char;

begin
  // If there is data output from the process, read it from the buffer and store into the 'console' memo
  if prCmd.Output.NumBytesAvailable > 0 then begin
    while prCmd.Output.NumBytesAvailable > 0 do begin
      FillChar(Buffer, SizeOf(Buffer), #0);
      prCmd.Output.Read(Buffer, SizeOf(Buffer) - 1);
      fDownload.memoCmd.Lines.Add(Buffer);
    end;
  end;
  Finished := False; OK := False;
  // Check if the file download is finished
  for I := 0 to fDownload.memoCmd.Lines.Count - 1 do begin
    if fDownload.memoCmd.Lines[I].Contains('WGET error') or fDownload.memoCmd.Lines[I].Contains('successfully downloaded') then begin
      Finished := True;
      if fDownload.memoCmd.Lines[I].Contains('successfully downloaded') then
        OK := True;
    end;
  end;
  // Download finished: Reload the currency file resp. display error message
  if Finished then begin
    tiCmd.Enabled := False;                                                    // disable the timer
    if OK then
      // If download was success, reload devises.txt
      DevisesReload(sDate, aDevises)
    else
      // If download was failue, display error message
      MessageDlg('Erreur mise-à-jour', 'Pas possible de télécharger les devises actuelles!', mtError, [mbOK], 0);
  end;
end;

end.

