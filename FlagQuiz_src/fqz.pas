{**************************************}
{* Main unit for FlagQuiz application *}
{**************************************}

unit fqz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls, StdCtrls, LazUTF8, LCLIntf, Registry, FileAssoc, fqznew;

type
  {*******}
  { TfFQZ }
  {*******}
  TfFQZ = class(TForm)
    mMenu: TMainMenu;
    mQuiz, mQuizOpen, mQuizExit: TMenuItem;
    mOptions, mOptionsLanguage, mOptionsLanguageEN, mOptionsLanguageDE: TMenuItem;
    mTools, mToolsNewQuiz, mToolsRegister: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    stTitle: TStaticText;
    ImageFlag: TImage;
    Label1: TLabel; Label2: TLabel; Label3, Label4: TLabel; Label5: TLabel;
    cobCountries: TComboBox;
    edEval, edQuestion, edCorrect, edFalse, edSuccess: TEdit;
    btQuestion: TButton;
    dlgOpenFlags: TOpenDialog;
    faFileAssoc: TFileAssociation;
    procedure FormCreate(Sender: TObject);
    procedure mQuizOpenClick(Sender: TObject);
    procedure mQuizExitClick(Sender: TObject);
    procedure mOptionsLanguageENClick(Sender: TObject);
    procedure mOptionsLanguageDEClick(Sender: TObject);
    procedure mToolsNewQuizClick(Sender: TObject);
    procedure mToolsRegisterClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iFlags, iQuestion, iCorrect: Integer;
    sProgDir, sQuizDir, sFlagsDir, sTitleEN, sTitleDE, sCountry: string;
    aFlags: TFlags;
  end;

var
  fFQZ: TfFQZ;

implementation

{$R *.lfm}

{ Clear the form controls }

procedure ClearForm;

begin
  fFQZ.edQuestion.Clear; fFQZ.edCorrect.Clear; fFQZ.edFalse.Clear;
  fFQZ.edSuccess.Clear; fFQZ.edSuccess.Color := clDefault;
  fFQZ.edEval.Clear; fFQZ.btQuestion.Caption := 'Start';
  fFQZ.iQuestion := 0; fFQZ.iCorrect := 0;
end;

{ Set quiz title (read from .fqz file) }

procedure SetTitle(TitleEN, TitleDE: string; NFlags: Integer);

var
  S: string;

begin
  if fFQZ.mOptionsLanguageDE.Checked then
    S := TitleDE                                                                    // German title
  else
    S := TitleEN;                                                                   // English title
  S += ' (' + IntToStr(NFlags) + ').';
  fFQZ.stTitle.Caption := S;
end;

{ Fill combobox with country names of actual quiz flags }

procedure FillCountryList(N: Integer; var Flags: TFlags);

var
  Countrynames: array [1..200] of string;
  I, J: Integer;
  Temp: string;

begin
  fFQZ.cobCountries.Clear;                                                          // remove all entries from combobox
  if N > 200 then                                                                   // be sure items fit in array
    N := 200;
  for I := 1 to N do begin
    if fFQZ.mOptionsLanguageDE.Checked then
      // Language selected = German
      Countrynames[I] := Flags[I].CountryNameDE
    else
      // Language selected = English
      Countrynames[I] := Flags[I].CountryNameEN;
  end;
  // Sort the array elements alphabetically
  for I := 1 to N - 1 do begin
    for J := I + 1 to N do begin
      if Countrynames[I] > Countrynames[J] then begin
        Temp := Countrynames[I];
        Countrynames[I] := Countrynames[J];
        Countrynames[J] := Temp;
      end;
    end;
  end;
  // Add the country names to the combobox
  for I := 1 to N do
    fFQZ.cobCountries.Items.Append(Countrynames[I]);
end;

{ Start a new flag quiz }

procedure NewQuiz(QuizDir, FlagsDir, QuizFileName: string; var NFlags: Integer; var TitleEN, TitleDE: string; var Flags: TFlags);

var
  FlagFileName: string;

begin
  // Read actual quiz flags and fill combobox
  ReadFlags(QuizDir, QuizFileName, NFlags, TitleEN, TitleDE, Flags);                // read flag info from file
  SetTitle(TitleEN, TitleDE, NFlags);                                               // display quiz title
  FillCountryList(NFlags, Flags);                                                   // fill combobox with country names
  // Display a flag (just the first of the set)
  FlagFileName := FlagsDir + '/' + LowerCase(Flags[1].CountryCode) + '.png';
  DoDirSeparators(FlagFileName);
  fFQZ.ImageFlag.Picture.LoadFromFile(FlagFileName);
  // Clear the form
  ClearForm;
end;

{*******}
{ TfFQZ }
{*******}

{ Application start: Initialisation }

procedure TfFQZ.FormCreate(Sender: TObject);

var
  FileName: string;

begin
  {$IFDEF WINDOWS}
    mToolsRegister.Enabled := True;                                                 // file extension registration works on Windows only
  {$ENDIF}
  sProgDir := ExtractFileDir(ParamStr(0));                                          // path to the application executable
  // Application launched by double-click of a .fqz file
  if ParamCount = 1 then begin
    // Flag file path is passed as 1st parameter
    sQuizDir := ExtractFileDir(ParamStr(1));
    FileName := ExtractFileName(ParamStr(1));
  end
  // Application launched by double-click of the executable
  else begin
    // Flag file path is default (APP-DIR/quiz)
    sQuizDir := sProgDir + '/quiz'; DoDirSeparators(sQuizDir);
    // europe_sel.fqz must be present in APP-DIR/quiz!
    FileName := 'europe_sel.fqz';                                                   // default quiz opened at application start
  end;
  iFlags := 0;
  sFlagsDir := sProgDir + '/flags'; DoDirSeparators(sFlagsDir);                     // directory with flag pictures (APP-DIR/flags)
  Randomize;
  NewQuiz(sQuizDir, sFlagsDir, FileName, iFlags, sTitleEN, sTitleDE, aFlags);       // start default quiz
end;

{ Menu item "Quiz > Open": New quiz initialisation with flag info read from selected file }

procedure TfFQZ.mQuizOpenClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpenFlags.InitialDir := sQuizDir;
  dlgOpenFlags.FileName := '';
  if dlgOpenFlags.Execute then begin                                                // user has selected a file
    FileName := dlgOpenFlags.FileName;                                              // get filename
    sQuizDir := ExtractFileDir(FileName);                                           // save directory to directly come to here next time
    FileName := ExtractFileName(FileName);
    NewQuiz(sQuizDir, sFlagsDir, FileName, iFlags, sTitleEN, sTitleDE, aFlags);     // start new quiz with actually selected flag-set
  end;
end;

{ Menu item "Quiz > Exit": Exit the application }

procedure TfFQZ.mQuizExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Options > Language > English": Change language to English }

procedure TfFQZ.mOptionsLanguageENClick(Sender: TObject);

begin
  if not mOptionsLanguageEN.Checked then begin
    mOptionsLanguageEN.Checked := True; mOptionsLanguageDE.Checked := False;
    SetTitle(sTitleEN, sTitleDE, iFlags);                                           // English quiz title
    FillCountryList(iFlags, aFlags);                                                // English country names in combobox
  end;
end;

{ Menu item "Options > Language > German": Change language to German }

procedure TfFQZ.mOptionsLanguageDEClick(Sender: TObject);

begin
  if not mOptionsLanguageDE.Checked then begin
    mOptionsLanguageDE.Checked := True; mOptionsLanguageEN.Checked := False;
    SetTitle(sTitleEN, sTitleDE, iFlags);                                           // German quiz title
    FillCountryList(iFlags, aFlags);                                                // German country names in combobox
  end;
end;

{ Menu item "Tools > Create new quiz": Select flags, that should be part of a custom quiz }

procedure TfFQZ.mToolsNewQuizClick(Sender: TObject);

begin
  fFQZNew.sProgPath := sProgDir;
  fFQZNew.sQuizPath := sQuizDir;
  fFQZNew.sFlagsPath := sFlagsDir;
  if mOptionsLanguageDE.Checked then
    fFQZNew.sQuizLang := 'ger'
  else
    fFQZNew.sQuizLang := 'eng';
  fFQZNew.ShowModal;                                                                // open "New quiz creation" window
end;

{ Menu item "Tools > Register .fqz files": Set file association for .fqz files in Windows registry }

procedure TfFQZ.mToolsRegisterClick(Sender: TObject);

var
  Ret: Cardinal;
  Association, S: string;
  OK: Boolean;
  Registry: TRegistry;

begin
  Association := '';
  try
    Registry := TRegistry.Create;
    Registry.RootKey := HKEY_CLASSES_ROOT;
    if Registry.OpenKeyReadOnly('.fqz') then begin
      // File extension is registered (with or without association)
      Association := Registry.ReadString('');
    end;
    Registry.Free;
  finally
    OK := True;
    if Association <> '' then begin
      // If .fqz files already have a registered association, let user decide to change it or not
      S := 'FQZ files are already registered! Do you really want to change the file association?';
      Ret := MessageDlg('Registry update', S, mtWarning, [mbYes, mbNo], 0, mbNo);
      if Ret = mrNo then
        OK := False;
    end;
    if OK then begin
      // Create new .fqz file association
      faFileAssoc := TFileAssociation.Create(Self);
      faFileAssoc.ApplicationName := 'Flag quiz';
      faFileAssoc.ApplicationDescription := 'Find the country corresponding to the national flag shown on the picture';
      faFileAssoc.Extension := '.fqz';
      faFileAssoc.ExtensionName := 'Flag quiz file';
      faFileAssoc.ExtensionIcon := '"' + sProgDir + '\' + 'FlagQuiz.ico' + '"';
      faFileAssoc.Action := '"' + sProgDir + '\' + 'FlagQuiz.exe' + '"' + ' ' + '"' + '%1' + '"';
      faFileAssoc.ActionName := 'Open';
      faFileAssoc.ActionIcon := '"' + sProgDir + '\' + 'FlagQuiz.ico' + '"';
      faFileAssoc.RegisterForAllUsers := False;
      try
        faFileAssoc.Execute;
        faFileAssoc.ClearIconCache;
        faFileAssoc.Free;
        MessageDlg('Registry update', 'File association for .fqz successfully set.', mtInformation, [mbOK], 0);
      except
        MessageDlg('Registry update', 'Registry error! File association not set!', mtError, [mbOK], 0);
      end;
    end;
  end;
end;

{ Menu item "Help > Help": Display short application help text }

procedure TfFQZ.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Choose a set of flags by opening a .fqz file, using the "Open" command in the "Quiz" menu. ';
  S += 'Choose the country names language in the "Options > Countries language" menu. Push the "Start" button to begin the quiz.';
  MessageDlg('"FlagQuiz" help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display application about }

procedure TfFQZ.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Geography quiz:' + LineEnding;
  S += 'Find the country corresponding to the national flag shown on the picture.' + LineEnding + LineEnding;
  S += 'Version 2.0, © allu, January 2018 - September 2021.';
  MessageDlg('About "FlagQuiz"', S, mtInformation, [mbOK], 0);
end;

{ Button "Start/Next/Check": New quiz question (new flag) resp. check user answer }

procedure TfFQZ.btQuestionClick(Sender: TObject);

var
  F, I: Integer;
  PCorrect: Real;
  FlagFile: string;

begin
  // Button "Start/Next": New quiz question (new flag)
  if (btQuestion.Caption = 'Start') or (btQuestion.Caption = 'Next') then begin
    if btQuestion.Caption = 'Start' then begin
      // First question of actual quiz
      ClearForm;                                                                    // clear form controls
      cobCountries.ItemIndex := 0;                                                  // to avoid a combobox with empty text
    end;
    Inc(iQuestion);
    // All flags done: message, that quiz is terminated
    if iQuestion > iFlags then begin
      MessageDlg('Flag quiz','Quiz over: All flags have been done.', mtInformation, [mbOK], 0);
      ClearForm;                                                                    // clear form controls (also resets button to "Start")
      for I := 1 to iFlags do
        aFlags[I].FlagDone := False;                                                // set all flags to 'not yet used' (for quiz restart)
    end
    // Not yet all flags done: continue quiz (next flag)
    else begin
      edQuestion.Text := IntToStr(iQuestion);
      // Random flag not already used
      repeat
        F := Random(iFlags) + 1;
      until aFlags[F].FlagDone = False;
      aFlags[F].FlagDone := True;                                                   // mark flag as used
      // Memorize country name (in selected language)
      if mOptionsLanguageDE.Checked then
        sCountry := aFlags[F].CountryNameDE
      else
        sCountry := aFlags[F].CountryNameEN;
      // Display the flag
      FlagFile := sFlagsDir + '/' + LowerCase(aFlags[F].CountryCode) + '.png';
      DoDirSeparators(FlagFile);
      ImageFlag.Picture.LoadFromFile(FlagFile);
      // Next action will be to check user's answer
      edEval.Text := '';
      btQuestion.Caption := 'Check';
    end;
  end
  // Button "Check": Check user answer
  else begin
    if cobCountries.Text = sCountry then begin
      // User has entered correct country name
      Inc(iCorrect);
      edEval.Text := 'Correct!'; edEval.Font.Color := clLime;
    end
    else begin
      // User has entered wrong country name
      edEval.Font.Color := clRed;
      edEval.Text := 'False! Correct = ' + sCountry;                                // display correct country name
    end;
    // Display number of correct and false answers
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iQuestion - iCorrect);
    // Calulate and display success percentage
    PCorrect := 100 * (iCorrect / iQuestion);
    PCorrect := Int(PCorrect * 100) / 100;                                          // percentage with 2 decimal digits
    // Color depending on success percentage
    if PCorrect < 50 then
      edSuccess.Color := clRed
    else if PCorrect < 60 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clLime;
    edSuccess.Text := FloatToStr(PCorrect) + ' %';
    // Next action will be to display a new flag (new quiz question)
    btQuestion.Caption := 'Next';
  end;
end;

end.

