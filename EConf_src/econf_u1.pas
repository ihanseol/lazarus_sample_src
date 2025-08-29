{***********************************}
{* Main unit for EConf application *}
{***********************************}

unit econf_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, econf_u2;

type
  TElement = record
    AtomicNumber: Integer;
    Symbol, Name: string;
    MainGroup: Boolean;
    NIons: Integer;
    Ions: array[1..4] of Integer;
  end;
  TElements = array[1..112] of TElement;
  TElementsDone = array[1..112, 0..4] of Boolean;
  TOrbital = record
    Symbol: Char;
    MaxElectrons: Integer;
  end;
  TOrbitals = array[0 .. 3] of TOrbital;
  TSubshells = array[0..18] of string;
  {*********}
  { TfEConf }
  {*********}
  TfEConf = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileExit: TMenuItem;
    mOptions: TMenuItem;
    mOptionsNQuestions, mOptionsAll, mOptionsIons: TMenuItem;
    MenuItem1: TMenuItem;
    mOptionsSymbols, mOptionsAtomicNumber: TMenuItem;
    mHelp, mHelpChemistry, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label5, Label6, Label7, Label8, Label9: TLabel;
    edName: TEdit;
    edSymbol: TEdit;
    edAtomicNumber: TEdit;
    edCharge: TEdit;
    laQuestion: TLabel;
    edQuestion: TEdit;
    laConfiguration: TLabel;
    edConfiguration: TEdit;
    edEval: TEdit;
    edCorrect: TEdit;
    edFalse: TEdit;
    edSuccess: TEdit;
    btQuestion: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsNQuestionsClick(Sender: TObject);
    procedure mOptionsAllClick(Sender: TObject);
    procedure mOptionsIonsClick(Sender: TObject);
    procedure mOptionsAtomicNumberClick(Sender: TObject);
    procedure mOptionsSymbolsClick(Sender: TObject);
    procedure mHelpChemistryClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iQuestions, iExQuestions, iQuestion, iCorrect, iFalse, iElement, iCharge: Integer;
    sConfiguration, sConfigurationDisplay: string;
    bExAll, bExIons: Boolean;
    aElements: TElements;
    aElementsDone: TElementsDone;
  end;

const
  MaxEleMain = 44;
  MaxIonMain = 100;
  MaxEleAll  = 112;
  MaxIonAll  = 150;
  Subshells: TSubshells = (
    '1s', '2s', '2p', '3s', '3p', '4s', '3d', '4p', '5s', '4d', '5p', '6s', '4f', '5d', '6p', '7s', '5f', '6d', '7p'
  );
  Orbitals: TOrbitals = (
    (Symbol: 's'; MaxElectrons: 2),
    (Symbol: 'p'; MaxElectrons: 6),
    (Symbol: 'd'; MaxElectrons: 10),
    (Symbol: 'f'; MaxElectrons: 14)
  );
  SUP_PLUS = #$E2#$81#$BA;
  SUP_MINUS = #$E2#$81#$BB;
  SUP_Digits: array[0..9] of string = (
     #$E2#$81#$B0, #$C2#$B9, #$C2#$B2, #$C2#$B3, #$E2#$81#$B4, #$E2#$81#$B5, #$E2#$81#$B6, #$E2#$81#$B7, #$E2#$81#$B8, #$E2#$81#$B9
  );

var
  fEConf: TfEConf;

implementation

{$R *.lfm}

{ Ion symbols with charge as superscript }

function ElementSymbol(Element: string; Charge: Integer): string;

var
  Symbol: string;

begin
  Symbol := Element;
  if Charge <> 0 then begin
    if Abs(Charge) > 1 then
      Symbol += SUP_Digits[Abs(Charge)];
    if Charge > 0 then
      Symbol += SUP_PLUS
    else
      Symbol += SUP_MINUS;
  end;
  ElementSymbol := Symbol;
end;

{ Read elements from file }

procedure ReadElements(out Elements: TElements);

var
  Z: Integer;
  Line, Group: string;
  InFile: Text;

begin
  Assign(InFile, 'elements.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if (Line <> '') and (LeftStr(Line, 1) <> '#') then begin
      Z := StrToInt(LeftStr(Line, 3));                                         // element atomic number
      if Z <= 112 then begin                                                   // ignore elements with Z > 112
        with Elements[Z] do begin
          AtomicNumber := Z;
          Symbol := Trim(Copy(Line, 5, 2));                                    // element symbol
          Name := Trim(Copy(Line, 8, 13));                                     // element name
          Group := Copy(Line, 22, 2);                                          // element group
          if (Group = 'TM') or (Group = 'LA') or (Group = 'AC') then           // only impoertance here: main group or not
            MainGroup := False
          else
            MainGroup := True;
          NIons := 0;
          if Length(Line) > 34 then begin                                      // rest of line = common ions for this element
            Inc(NIons);
            Ions[1] := StrToInt(Copy(Line, 36, 2));
            if Length(Line) > 37 then begin
              Inc(NIons);
              Ions[2] := StrToInt(Copy(Line, 38, 2));
              if Length(Line) > 39 then begin
                Inc(NIons);
                Ions[3] := StrToInt(Copy(Line, 40, 2));
                if Length(Line) > 41 then begin
                  Inc(NIons);
                  Ions[4] := StrToInt(Copy(Line, 42, 2));
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Start a new test }

procedure NewTest(Questions: Integer; out ExQuestions, Question, QCorrect, QFalse: Integer; out ExAll, ExIons: Boolean; out Done: TElementsDone);

var
  I, J, MaxQuestions: Integer;

begin
  // Clear/reset all
  fEConf.laQuestion.Caption := 'Exercise';
  fEConf.laConfiguration.Caption := 'Electron configuration:';
  fEConf.edName.Text := ''; fEConf.edAtomicNumber.Text := '';
  fEConf.edSymbol.Text := ''; fEConf.edCharge.Text := '';
  fEConf.edConfiguration.Text := ''; fEConf.edEval.Text := '';
  fEConf.edQuestion.Text := ''; fEConf.edCorrect.Text := ''; fEConf.edFalse.Text := '';
  fEConf.edSuccess.Text := ''; fEConf.edSuccess.Color := clForm;
  Question := 0; QCorrect := 0; QFalse := 0;
  for I := 1 to 112 do begin
    for J := 0 to 4 do
      Done[I, J] := False;
  end;
  // "All elements" / "ions" selections now become active
  ExAll := False; ExIons := False;
  if fEConf.mOptionsAll.Checked then
    ExAll := True;
  if fEConf.mOptionsIons.Checked then
    ExIons := True;
  // Determine maximum number of questions for actual settings
  if ExAll and ExIons then
    MaxQuestions := MaxIonAll
  else if ExAll then
    MaxQuestions := MaxEleAll
  else if ExIons then
    MaxQuestions := MaxIonMain
  else
    MaxQuestions := MaxEleMain;
  // Number of questions selected now becomes active
  ExQuestions := Questions;
  if ExQuestions > MaxQuestions then                                           // to avoid choosing more questions than available
    ExQuestions := MaxQuestions;
  // Enable button to start this exercise
  fEConf.btQuestion.Caption := 'Question';
  fEConf.btQuestion.Enabled := True;
end;

{ Determination of the electrons configuration of a given atom/ion }

procedure EConfiguration(Element: TElement; Charge: Integer; out Configuration, DConfiguration: string);

var
  Z, E, SubShellCount, SubShellMax, C1, C2, SX, SSX, I, J: Integer;
  Subshell: string;
  SubshellCounts: array[0..18] of Integer;

begin
  Z := Element.AtomicNumber; E := Z;
  if Charge < 0 then                                                           // for cations, just add the suppl. electrons as usual
    E += Abs(Charge);
  Configuration := ''; DConfiguration := '';
  for I := 0 to 18 do
    SubshellCounts[I] := 0;
  SX := 0; SubshellCount := 0;
  // Count electrons in the different subshells (in order to add them to the subshells as previwed by the Aufbau principle)
  // Ignore anions for the moment (do as if there wasn't a charge )
  for I := 1 to E do begin                                                     // E = Z for atoms/anios, Z + ... for cations
    // Start a new subshell
    if SubshellCount = 0 then begin
      SubShell := Subshells[SX];
      // Determine maximum number of electons
      for J :=  0 to 3 do begin
        if RightStr(SubShell, 1)[1] = Orbitals[J].Symbol then
          SubShellMax := Orbitals[J].MaxElectrons;
      end;
    end;
    // Count electrons for this subshell
    Inc(SubShellCount);
    // Subshell has reached maximum of electrons
    if SubshellCount = SubShellMax then begin
      // Add the electrons counted to this subshell
      SubShellCounts[SX] := SubshellCount;
      // Point to next subshell
      Inc(SX);
      // Reset electron counter for the new subshell
      SubshellCount := 0;
    end
    // Add remaining electrons to the actual (last) subshell for actual element
    else if I = E then
      SubShellCounts[SX] := SubshellCount;
  end;
  // Correct the pointer to last subshell used for actual element
  if SubshellCount = 0 then
    Dec(SX);
  // Now, remove electrons for anions
  if Charge > 0 then begin
    // Special case for transition metals and inner transition metals
    if not Element.MainGroup then begin
      SSX := 0;
      if (Subshells[SX] = '3d') or (Subshells[SX] = '4d') or (Subshells[SX] = '4f') or (Subshells[SX] = '5f') or (Subshells[SX] = '5d') or (Subshells[SX] = '6d') then begin
        // Find s subshell where electrons have to be removed (before removing them from d or f subshells)
        if (Subshells[SX] = '3d') or (Subshells[SX] = '4d') or (Subshells[SX] = '4f') or (Subshells[SX] = '5f') then
          SSX := 1
        else
          SSX := 2;
        // Remove 1 or 2 electrons from s subshell
        if SSX <> 0 then begin
          if SubshellCounts[SX - SSX] >= Charge then begin
            SubshellCounts[SX - SSX] -= Charge;
            Charge := 0;
          end
          else begin
            Charge -= SubshellCounts[SX - SSX];
            SubshellCounts[SX - SSX] := 0;
          end;
        end;
      end;
    end;
  end;
  while Charge > 0 do begin
    // Remove electrons from subshells in inverse order than they have been added by the Aufbau principle
      if SubshellCounts[SX] >= Charge then begin
        SubshellCounts[SX] -= Charge;
        Charge := 0;
      end
      else begin
        Charge -= SubshellCounts[SX];
        SubshellCounts[SX] := 0;
        Dec(SX);
      end;
  end;
  // Create a string represenation of the actual element's electrons configuration
  for I := 0 to 18 do begin
    if SubshellCounts[I] <> 0 then begin
      Configuration += SubShells[I] + IntToStr(SubshellCounts[I]) + ' ';
      DConfiguration += SubShells[I];
      C1 := SubshellCounts[I] div 10; C2 := SubshellCounts[I] mod 10;
      if C1 <> 0 then
        DConfiguration += SUP_Digits[C1];
      DConfiguration += SUP_Digits[C2];
    end;
  end;
end;

{*********}
{ TfEConf }
{*********}

{ Application start: Initialisation }

procedure TfEConf.FormCreate(Sender: TObject);

begin
  ReadElements(aElements);
  iQuestions := 10;
  NewTest(iQuestions, iExQuestions, iQuestion, iCorrect, iFalse, bExAll, bExIons, aElementsDone);
  Randomize;
end;

{ Menu item "File > New": Start new test }

procedure TfEConf.mFileNewClick(Sender: TObject);

begin
  NewTest(iQuestions, iExQuestions, iQuestion, iCorrect, iFalse, bExAll, bExIons, aElementsDone);
end;

{ Menu item "File > Exit": Exit application }

procedure TfEConf.mFileExitClick(Sender: TObject);

begin
  Close;
end;







{ Menu item "Options > Number of questions ...": Get number of test questions from user }

procedure TfEConf.mOptionsNQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Electron configurations', 'Number of questions', IntToStr(iQuestions));
  if S <> '' then
    iQuestions := StrToInt(S);
end;

{ Menu item "Options > Include all elements": Select if exercises include all or only main group elements }

procedure TfEConf.mOptionsAllClick(Sender: TObject);

begin
  if not mOptionsAll.Checked then
    mOptionsAll.Checked := True
  else
    mOptionsAll.Checked := False;
end;

{ Menu item "Options > Include ion exercises": Select if exercises include ions or only (uncharged) atoms }

procedure TfEConf.mOptionsIonsClick(Sender: TObject);

begin
  if not mOptionsIons.Checked then
    mOptionsIons.Checked := True
  else
    mOptionsIons.Checked := False;
end;

{ Menu item "Options > Display element symbol": Select if element symbol is displayed or not }

procedure TfEConf.mOptionsSymbolsClick(Sender: TObject);

begin
  if not mOptionsSymbols.Checked then
    mOptionsSymbols.Checked := True
  else
    mOptionsSymbols.Checked := False;
end;

{ Menu item "Options > Display atomic number": Select if atomic number is displayed or not }

procedure TfEConf.mOptionsAtomicNumberClick(Sender: TObject);

begin
  if not mOptionsAtomicNumber.Checked then
    mOptionsAtomicNumber.Checked := True
  else
    mOptionsAtomicNumber.Checked := False;
end;

{ Menu item "Help > Chemistry help": Display help concerning electrons configurations }

procedure TfEConf.mHelpChemistryClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide;
  fHelp.meHelp.Lines.LoadFromFile('chemistry.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display help concerning the "EConf" program }

procedure TfEConf.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Hide;
  fHelp.meHelp.Lines.LoadFromFile('help.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display about concerning the "EConf" program }

procedure TfEConf.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Chemistry exercise generator: Electron configurations.' + LineEnding;
  S += 'Version 1.0, © allu, September-December 2018.';
  MessageDlg('About "EConf"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate new question resp. check user answer }

procedure TfEConf.btQuestionClick(Sender: TObject);

var
  EX, IX: Integer;
  UserConfiguration: string;
  OK: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + '/' + IntToStr(iExQuestions);
    edConfiguration.Text := ''; edEval.Text := '';
    // Select random element (from those selected)
    repeat
      OK := True;
      EX := Random(112) + 1;
      if not bExAll and not aElements[EX].MainGroup then
        OK := False
      else begin
        if bExIons then
          IX := Random(aElements[EX].NIons + 1)
        else
          IX := 0;
        if aElementsDone[EX, IX] then
          OK := False;
      end;
    until OK;
    iElement := EX;
    if IX = 0 then
      iCharge := 0
    else
      iCharge := aElements[EX].Ions[IX];
    // Fill in the form
    edName.Text := aElements[EX].Name;
    if mOptionsAtomicNumber.Checked then
      edAtomicNumber.Text := IntToStr(aElements[EX].AtomicNumber)
    else
      edAtomicNumber.Text := '';
    if iCharge = 0 then
      edCharge.Text := ''
    else if iCharge > 0 then
      edCharge.Text := '+' + IntToStr(iCharge)
    else
      edCharge.Text := IntToStr(iCharge);
    if mOptionsSymbols.Checked then
      edSymbol.Text := ElementSymbol(aElements[EX].Symbol, iCharge)
    else
      edSymbol.Text := '';
    // Determine the electrons configuration
    EConfiguration(aElements[EX], iCharge, sConfiguration, sConfigurationDisplay);
    // Set this element/ion combination as done
    aElementsDone[EX, IX] := True;
    // Next button push will be a user answer
    btQuestion.Caption := 'Answer';
    edConfiguration.SetFocus;
  end
  // Button "Answer": Check user answer
  else begin
    sConfiguration := StringReplace(sConfiguration, ' ', '', [rfReplaceAll]);
    UserConfiguration := StringReplace(edConfiguration.Text, ' ', '', [rfReplaceAll]);
    edEval.Text := sConfigurationDisplay;
    // Correct answer
    if UserConfiguration = sConfiguration then begin
      Inc(iCorrect);
      edEval.Font.Color := clDefault;
    end
    // False answer
    else begin
      Inc(iFalse);
      edEval.Font.Color := clRed;
    end;
    // Fill in evaluation fields
    edQuestion.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iFalse);
    edSuccess.Text := IntToStr(Round(100 * (iCorrect / iQuestion))) + '%';
    // Next button push will be a further question
    btQuestion.Caption := 'Question';
    btQuestion.SetFocus;
    // If all questions done, display message
    if iQuestion = iExQuestions then begin
      MessageDlg('End of exercise', 'To do another exercise, please choose "New" in the "File" menu!', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

end.

