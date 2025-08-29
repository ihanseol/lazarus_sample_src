{************************************}
{* Main unit for Decay1 application *}
{************************************}

unit decay1_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, decay1_elements, decay1_help;

const
  NElements = 118;

type
  TElement = record
    Name, Symbol: string;
    Z, A: Integer;
    Isotopes: array of record
      A: Integer;
      Decay: array of string;
    end;
  end;
  TElements = array[0 .. NElements - 1] of TElement;
  {**********}
  { TfDecay1 }
  {**********}
  TfDecay1 = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileExit: TMenuItem;
    mSettings, mSettingsQuestions, mSettingsDecay, mSettingsIsotopes: TMenuItem;
    MenuItem1, mSettingsEqu, mSettingsEquAlpha, mSettingsEquBeta: TMenuItem;
    mHelp, mHelpElements, mHelpPhysics, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10: TLabel;
    Label11, Label12, Label13, Label14, Label15: TLabel;
    laQuestion: TLabel;
    edIsotope, edZ, edA, edN: TEdit;
    edDecay, edDecayDetails: TEdit;
    edIsotope2, edZ2, edA2, edN2: TEdit;
    edEquation: TEdit;
    btAlpha, btBetaMinus, btBetaPlus: TButton;
    btElectron, btNeutrino, btAntiNeutrino: TButton;
    btIsotope1, btIsotope2: TButton;
    btPlus, btEquals: TButton;
    edQuestions, edCorrect, edFalse, edSuccess: TEdit;
    imEval: TImage;
    btQuestion: TButton;
    btShow: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsQuestionsClick(Sender: TObject);
    procedure mSettingsDecayClick(Sender: TObject);
    procedure mSettingsIsotopesClick(Sender: TObject);
    procedure mSettingsEquAlphaClick(Sender: TObject);
    procedure mSettingsEquBetaClick(Sender: TObject);
    procedure mHelpElementsClick(Sender: TObject);
    procedure mHelpPhysicsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
    procedure btShowClick(Sender: TObject);
    procedure btAlphaClick(Sender: TObject);
    procedure btBetaMinusClick(Sender: TObject);
    procedure btBetaPlusClick(Sender: TObject);
    procedure btElectronClick(Sender: TObject);
    procedure btNeutrinoClick(Sender: TObject);
    procedure btAntiNeutrinoClick(Sender: TObject);
    procedure btIsotope1Click(Sender: TObject);
    procedure btIsotope2Click(Sender: TObject);
    procedure btPlusClick(Sender: TObject);
    procedure btEqualsClick(Sender: TObject);
  private
    iElement, iIsotope, iDecay, iQuestionsTemp, iQuestions, iQuestion, iCorrect: Integer;
    sDecay, sEquation: string;
    bPTFilled: Boolean;
    recNewElement: TElement;
    aElements: TElements;
  end;

const
  Neutrino = 'v';
  AntiNeutrino = 'ṽ';
  SUP_PLUS = #$E2#$81#$BA;
  SUP_MINUS = #$E2#$81#$BB;
  SUP_Digits: array[0..9] of string = (
    #$E2#$81#$B0, #$C2#$B9, #$C2#$B2, #$C2#$B3, #$E2#$81#$B4, #$E2#$81#$B5, #$E2#$81#$B6, #$E2#$81#$B7, #$E2#$81#$B8, #$E2#$81#$B9
  );
  SUB_Digits: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84, #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  fDecay1: TfDecay1;

implementation

{$R *.lfm}

{ Read isotopes from text file }

procedure ReadIsotopes(out Elements: TElements);

var
  E, I, D, P: Integer;
  Line, S, Isotope, MassNumber, Decay: string;
  Ignore: Boolean;
  InFile: Text;

begin
  Assign(InFile, 'isotopes.txt'); Reset(InFile);
  E := 0;
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Inc(E); I := 0;
      Elements[E - 1].Name := Trim(LeftStr(Line, 15));
      Elements[E - 1].Symbol := Trim(Copy(Line, 17, 2));
      Elements[E - 1].Z := E;
      Elements[E - 1].A := StrToInt(Copy(Line, 33, 3));
      SetLength(Elements[E - 1].Isotopes, 0);                                            // array with isotopes for actual element
      if Length(Line) > 36 then begin
        Line := RightStr(Line, Length(Line) - 36);
        while Line <> '' do begin                                                        // get all of element's isotopes one by one
          P := Pos(';', Line);                                                           // isotopes separated by ";"
          if P > 0 then begin
            S := LeftStr(Line, P - 1);
            Line := RightStr(Line, Length(Line) - P);
          end
          else begin
            S := Line; Line := '';
          end;
          P := Pos(':', S);                                                              // isotope data items separated by ":"
          Isotope := LeftStr(S, P - 1); S := RightStr(S, Length(S) - P);
          if LeftStr(S, 1) <> '-' then begin                                             // continue only if isotope isn't stable (decay data = "-")
            P := Pos('-', Isotope);
            MassNumber := RightStr(Isotope, Length(Isotope) - P);                        // isotope mass number (from X-mmm)
            if RightStr(MassNumber, 1) = '*' then
              MassNumber := LeftStr(MassNumber, Length(MassNumber) - 1);                 // remove "synthetic tag" (not used)
            Ignore := False;
            if (RightStr(MassNumber, 1) = 'm') or (RightStr(MassNumber, 2) = 'm1') or (RightStr(MassNumber, 2) = 'm2') then
              Ignore := True;                                                            // ignore isotopes that are in m, m1, or m2 state
            P := Pos(':', S);
            S := LeftStr(S, P - 1);
            S := StringReplace(S, ',IT', '', []); S := StringReplace(S, ',SF', '', []); S := StringReplace(S, ',γ', '', []);
            if (S <> '') and not Ignore then begin                                       // only normal state isotopes with α, β-, β+, ε decay considered
              Inc(I); SetLength(Elements[E - 1].Isotopes, I); D := 0;                    // array with decay types for actual isotope
              Elements[E - 1].Isotopes[I - 1].A := StrToInt(MassNumber);
              while S <> '' do begin                                                     // get all of isotope's decay taypes one by one
                P := Pos(',', S);
                if P > 0 then begin
                  Decay := LeftStr(S, P - 1);
                  S := RightStr(S, Length(S) - P);
                end
                else begin
                  Decay := S; S := '';
                end;
                Inc(D); SetLength(Elements[E - 1].Isotopes[I - 1].Decay, D);
                Elements[E - 1].Isotopes[I - 1].Decay[D - 1] := Decay;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Clear form fields }

procedure ClearForm(ClearAll: Boolean);

begin
  fDecay1.edZ.Text := ''; fDecay1.edA.Text := ''; fDecay1.edN.Text := '';
  fDecay1.edIsotope2.Text := ''; fDecay1.edZ2.Text := ''; fDecay1.edA2.Text := ''; fDecay1.edN2.Text := '';
  fDecay1.edEquation.Text := '';
  fDecay1.edZ.Color := clDefault; fDecay1.edA.Color := clDefault; fDecay1.edN.Color := clDefault;
  fDecay1.edIsotope2.Color := clDefault; fDecay1.edZ2.Color := clDefault; fDecay1.edA2.Color := clDefault; fDecay1.edN2.Color := clDefault;
  fDecay1.edEquation.Color := clDefault;
  if ClearAll then begin
    // Clear these fields only if boolean argument is set true
    fDecay1.edIsotope.Text := '';
    fDecay1.edDecay.Text := ''; fDecay1.edDecayDetails.Text := '';
    fDecay1.edSuccess.Color := clDefault;
  end;
  fDecay1.imEval.Visible := False;
end;

{ Right-align integer values in periodic table string grids }

function GFormat(N: Integer): string;

var
  SN: string;

begin
  SN := IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  Result := SN;
end;

{ Fill-in periodic table string grids (on form fElements) }

procedure FillPeriodicTable(var Elements: TElements);

var
  I, T, R : Integer;

begin
  for I := 0 to NElements - 1 do begin
    T := I div 25;                                                                       // the one of the 5 tables to use
    R := I mod 25 + 1;                                                                   // row within this table
    fElements.sgElements[T].Cells[0, R] := Elements[I].Name;
    fElements.sgElements[T].Cells[1, R] := '  ' + Elements[I].Symbol;
    fElements.sgElements[T].Cells[2, R] := ' ' + GFormat(Elements[I].Z);
    fElements.sgElements[T].Cells[3, R] := ' ' + GFormat(Elements[I].A);
  end;
end;

{ Get given element's nuclide (the isotope to actually use being defined by an index value for the element's isotopes array) }

function GetNuclide(Element: TElement; IX: Integer): string;

var
  S: string;

begin
  S := IntToStr(Element.Z) + Element.Symbol + '-';                                       // nuclide format: X-nnn
  if IX = -1 then
    S += IntToStr(Element.A)                                                             // a -1 index indicates to use the periodic table mass number
  else
    S += IntToStr(Element.Isotopes[IX].A);                                               // any other index indicates to use the mass number of the corr, isotopes array field
  Result := S;
end;

{ Format nuclide string with atomic number as subscript }

function GetSubscriptNuclide(Nuclide: string): string;

var
  ZLength, I: Integer;
  S: string;

begin
  S := ''; ZLength := 0;
  while Nuclide[ZLength + 1] in ['0'.. '9'] do                                           // determine number of digits of Z
    Inc(ZLength);
  for I := 1 to ZLength do                                                               // transform all these digits to subscripts
    S += SUB_Digits[StrToInt(Nuclide[I])];
  for I := ZLength + 1 to Length(Nuclide) do
    S += Nuclide[I];
  Result := S;
end;

{ Alpha decay calculation routine }

procedure AlphaDecay(var Elements: TElements; EX, IX: Integer; out DaughterElement: TElement; out Equation: string);

var
  ParentElement: TElement;

begin
  ParentElement := Elements[EX];
  // Daughter element data
  DaughterElement.Z := ParentElement.Z - 2;
  DaughterElement.Symbol := Elements[DaughterElement.Z - 1].Symbol;
  DaughterElement.A := Elements[DaughterElement.Z - 1].A;
  // Daughter actual isotope data
  SetLength(DaughterElement.Isotopes, 1);
  DaughterElement.Isotopes[0].A := ParentElement.Isotopes[IX].A - 4;                     // store daughter's mass number in first field of isotopes array
  // Alpha decay equation
  Equation := GetNuclide(ParentElement, IX) + ' → ' + GetNuclide(DaughterElement, 0) + ' + ' + GetNuclide(Elements[1], -1);
  if fDecay1.mSettingsEquAlpha.Checked then
    // Use "α" instead of helium nucleus, if this option is selected
    Equation := StringReplace(Equation, GetNuclide(Elements[1], -1), 'α', []);
end;

{ Beta decay calculation routine }

procedure BetaDecay(var Elements: TElements; EX, IX: Integer; Decay: string; out DaughterElement: TElement; out Equation: string);

var
  ParentElement: TElement;

begin
  ParentElement := Elements[EX];
  // Daughter element data, with Z depending on decay type (β- or β+)
  if Decay = 'β-' then
    DaughterElement.Z := ParentElement.Z + 1
  else
    DaughterElement.Z := ParentElement.Z - 1;
  DaughterElement.Symbol := Elements[DaughterElement.Z - 1].Symbol;
  DaughterElement.A := Elements[DaughterElement.Z - 1].A;
  // Daughter actual isotope data
  SetLength(DaughterElement.Isotopes, 1);
  DaughterElement.Isotopes[0].A := ParentElement.Isotopes[IX].A;                         // store daughter's mass number in first field of isotopes array
  // Beta minus resp. beta plus decay equation
  if Decay = 'β-' then
    Equation := GetNuclide(ParentElement, IX) + ' → ' + GetNuclide(DaughterElement, 0) + ' + β- + ' + AntiNeutrino
  else
    Equation := GetNuclide(ParentElement, IX) + ' → ' + GetNuclide(DaughterElement, 0) + ' + β+ + ' + Neutrino;
  if fDecay1.mSettingsEquBeta.Checked then
    // Use e-/e+ for electron resp. positron, if this option is selected
    Equation := StringReplace(Equation, '+ β', '+ e', []);
end;

{ Epsilon decay calculation routine }

procedure EpsilonDecay(var Elements: TElements; EX, IX: Integer; out DaughterElement: TElement; out Equation: string);

var
  ParentElement: TElement;

begin
  ParentElement := Elements[EX];
  // Daughter element data
  DaughterElement.Z := ParentElement.Z - 1;
  DaughterElement.Symbol := Elements[DaughterElement.Z - 1].Symbol;
  DaughterElement.A := Elements[DaughterElement.Z - 1].A;
  // Daughter actual isotope data
  SetLength(DaughterElement.Isotopes, 1);
  DaughterElement.Isotopes[0].A := ParentElement.Isotopes[IX].A;                         // store daughter's mass number in first field of isotopes array
  // Epsilon decay equation
  Equation := GetNuclide(ParentElement, IX) + ' + e- → ' + GetNuclide(DaughterElement, 0) + ' + ' + Neutrino;
end;

{**********}
{ TfDecay1 }
{**********}

{ Aplication start: Initialisation }

procedure TfDecay1.FormCreate(Sender: TObject);

begin
  btBetaMinus.Caption := 'β' + SUP_MINUS; btBetaPlus.Caption := 'β' + SUP_PLUS;          // proper display of button captures
  ReadIsotopes(aElements);                                                               // read isotopes from file
  iQuestionsTemp := 10;
  bPTFilled := False;                                                                    // boolean, indicating if periodic table already has been filled (see below)
  Randomize;
  mFileNew.Click;                                                                        // start new exercise (by simulating menu item selection)
end;

{ Menu item "File > New": Start a new exercise }

procedure TfDecay1.mFileNewClick(Sender: TObject);

begin
  laQuestion.Caption := 'Question';
  ClearForm(True);
  edQuestions.Text := ''; edCorrect.Text := ''; edFalse.Text := ''; edSuccess.Text := '';
  iQuestions := iQuestionsTemp;                                                          // number of questions entered now becomes active
  iQuestion := 0; iCorrect := 0;
  btQuestion.Caption := 'Question'; btQuestion.Enabled := True;
end;

{ Menu item "File > Exit": Exit application }

procedure TfDecay1.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of questions ...": User entry of number of exercise questions }

procedure TfDecay1.mSettingsQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Radioactive decay', 'Number of questions (at least 5)', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
  if iQuestionsTemp < 5 then                                                             // minimum of questions arbitrarily fixed to 5
    iQuestionsTemp := 5;
end;

{ Menu item "Settings > Display decay details": Toggle display or not short description of decay type }

procedure TfDecay1.mSettingsDecayClick(Sender: TObject);

begin
  if mSettingsDecay.Checked then
    mSettingsDecay.Checked := False
  else
    mSettingsDecay.Checked := True;
  edDecayDetails.Visible := mSettingsDecay.Checked;                                      // make desciption visible or not
end;

{ Menu item "Settings > Enable isotope buttons": Toggle enable or disable button-based entry of isotope nuclides }

procedure TfDecay1.mSettingsIsotopesClick(Sender: TObject);

begin
  if mSettingsIsotopes.Checked then
    mSettingsIsotopes.Checked := False
  else
    mSettingsIsotopes.Checked := True;
  btIsotope1.Enabled := mSettingsIsotopes.Checked;                                       // enable or disable the buttons
  btIsotope2.Enabled := mSettingsIsotopes.Checked;
end;

{ Menu item "Settings > Equations display format": Toggle usage of standard or less common symbols in decay equation }

procedure TfDecay1.mSettingsEquAlphaClick(Sender: TObject);

begin
  if mSettingsEquAlpha.Checked then
    mSettingsEquAlpha.Checked := False
  else
    mSettingsEquAlpha.Checked := True;
end;

procedure TfDecay1.mSettingsEquBetaClick(Sender: TObject);

begin
  if mSettingsEquBeta.Checked then
    mSettingsEquBeta.Checked := False
  else
    mSettingsEquBeta.Checked := True;
end;

{ Menu item "Help > Periodic table": Display periodic table of elements (Z and A values) }

procedure TfDecay1.mHelpElementsClick(Sender: TObject);

begin
  if not bPTFilled then begin                                                            // if the table has not yet being filled, do it now
    FillPeriodicTable(aElements);
    bPTFilled := True;                                                                   // not necessary to fill the table more than once
  end;
  if fElements.Visible then
    fElements.Close
  else
    fElements.Show;
end;

{ Menu item "Help > Physics help": Display radioactive decay help text }

procedure TfDecay1.mHelpPhysicsClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display program usage help text }

procedure TfDecay1.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfDecay1.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Nuclear physics:' + LineEnding;
  S += 'Radioactive decay exercise generator.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, November 2019.';
  MessageDlg('About "Decay1"', S, mtInformation, [mbOK], 0);
end;

{ Button "Question/Answer": Generate question resp. check user answer }

procedure TfDecay1.btQuestionClick(Sender: TObject);

var
  Z1, A1, N1, Z2, A2, N2, Success: Integer;
  Isotope, Isotope2, Equation: string;
  OK: Boolean;

begin
  // Button "Question": Generate new question
  if btQuestion.Caption = 'Question' then begin
    ClearForm(True);
    Inc(iQuestion);
    laQuestion.Caption := 'Question ' + IntToStr(iQuestion) + ' / ' + IntToStr(iQuestions);
    // Get random element until a "valid" one has been found
    repeat
      OK := True;
      iElement := Random(NElements);
      if Length(aElements[iElement].Isotopes) > 0 then begin                             // it must have isotopes (not all isotopes have been filled in...)
        iIsotope := Random(Length(aElements[iElement].Isotopes));
        if Length(aElements[iElement].Isotopes[iIsotope].Decay) > 0 then begin           // the isotope chosen must be radioactive
          iDecay := Random(Length(aElements[iElement].Isotopes[iIsotope].Decay));
          sDecay := aElements[iElement].Isotopes[iIsotope].Decay[iDecay];
          if (sDecay <> 'α') and (sDecay <> 'β-') and (sDecay <> 'β+') and (sDecay <> 'ε') then
            // "Double decays" (such as β-β- or εε) not implemented!
            OK := False;
        end
        else
          OK := False
      end
      else
        OK := False;
    until OK;
    // Display parent nuclide and decay type
    edIsotope.Text := aElements[iElement].Symbol + '-' + IntToStr(aElements[iElement].Isotopes[iIsotope].A);
    edDecay.Text := sDecay;
    edDecay.Text := StringReplace(edDecay.Text, '-', SUP_MINUS, [rfReplaceAll]);         // use superscripts with β-, β+ and e-
    edDecay.Text := StringReplace(edDecay.Text, '+', SUP_PLUS, [rfReplaceAll]);
    // Fill in decay details (if selected not to show, the field is set invisible)
    if sDecay = 'α' then
      edDecayDetails.Text := 'Emission of a ' + SUP_Digits[4] + 'He nucleus'
    else if sDecay = 'β-' then
      edDecayDetails.Text := 'Emission of an electron'
    else if sDecay = 'β+' then
      edDecayDetails.Text := 'Emission of a positron'
    else
      edDecayDetails.Text := 'Capture of an electron';
    // Next button push will be a user answer
    btQuestion.Caption := 'Answer';
    edZ.SetFocus;                                                                        // parent atomic number = first value to enter by user
  end
  // Button "Answer": Check user answer
  else begin
    // Calculate decay values
    if sDecay = 'α' then
      AlphaDecay(aElements, iElement, iIsotope, recNewElement, sEquation)
    else if (sDecay = 'β-') or (sDecay = 'β+') then
      BetaDecay(aElements, iElement, iIsotope, sDecay, recNewElement, sEquation)
    else
      EpsilonDecay(aElements, iElement, iIsotope, recNewElement, sEquation);
    Isotope := recNewElement.Symbol + '-' + IntToStr(recNewElement.Isotopes[0].A);       // nuclide notation
    // Read user values from form
    if edZ.Text = '' then
      Z1 := 0
    else
      Z1 := StrToInt(edZ.Text);
    if edA.Text = '' then
      A1 := 0
    else
      A1 := StrToInt(edA.Text);
    if edN.Text = '' then
      N1 := 0
    else
      N1 := StrToInt(edN.Text);
    Isotope2 := edIsotope2.Text;
    if edZ2.Text = '' then
      Z2 := 0
    else
      Z2 := StrToInt(edZ2.Text);
    if edA2.Text = '' then
      A2 := 0
    else
      A2 := StrToInt(edA2.Text);
    if edN2.Text = '' then
      N2 := 0
    else
      N2 := StrToInt(edN2.Text);
    Equation := edEquation.Text;
    // Give user the liberty to use spaces between symbols, just as she wants
    Equation := StringReplace(Equation, 'β+', 'β#', []);                                 // to avoid adding spaces by '-' / '+' StringReplace
    Equation := StringReplace(Equation, 'e+', 'e#', []);
    Equation := StringReplace(Equation, ' ', '', [rfReplaceAll]);
    Equation := StringReplace(Equation, '→', ' → ', [rfReplaceAll]);
    Equation := StringReplace(Equation, '+', ' + ', [rfReplaceAll]);
    Equation := StringReplace(Equation, 'β#', 'β+', []);
    Equation := StringReplace(Equation, 'e#', 'e+', []);
    // Check user answers; highlight each field with an incorrect value
    OK := True;
    if Z1 <> aElements[iElement].Z then begin
      OK := False;
      edZ.Color := clRed;
    end;
    if A1 <> aElements[iElement].Isotopes[iIsotope].A then begin
      OK := False;
      edA.Color := clRed;
    end;
    if N1 <> aElements[iElement].Isotopes[iIsotope].A - aElements[iElement].Z then begin
      OK := False;
      edN.Color := clRed;
    end;
    if Isotope2 <> Isotope then begin
      OK := False;
      edIsotope2.Color := clRed;
    end;
    if Z2 <> recNewElement.Z then begin
      OK := False;
      edZ2.Color := clRed;
    end;
    if A2 <> recNewElement.Isotopes[0].A then begin
      OK := False;
      edA2.Color := clRed;
    end;
    if N2 <> recNewElement.Isotopes[0].A - recNewElement.Z then begin
      OK := False;
      edN2.Color := clRed;
    end;
    if Equation <> sEquation then begin
      OK := False;
      edEquation.Color := clRed;
    end;
    // All answer values are correct
    if OK then begin
      imEval.Picture.LoadFromFile('correct.png');
      Inc(iCorrect)
    end
    // At least one nswer value is false
    else begin
      imEval.Picture.LoadFromFile('false.png');
    end;
    // Display correct/false icon
    imEval.Visible := True;
    // Fill in evaluation grid
    edQuestions.Text := IntToStr(iQuestion);
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iQuestion - iCorrect);
    Success := Round(100 * (iCorrect / iQuestion));
    edSuccess.Text := IntToStr(Success) + '%';
    // Use different colors for field background, depending on success percentage
    if Success > 60 then
      edSuccess.Color := clLime
    else if Success > 50 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clRed;
    // Next button push will be for a new question generation
    btQuestion.Caption := 'Question';
    // Not yet all questions done: continue
    if iQuestion < iQuestions then begin
      btQuestion.SetFocus;
    end
    // All questions done: end of exercise
    else begin
      MessageDlg('Radioactive decay', 'All questions done. End of exercise.', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;
    end;
  end;
end;

{ Button "Show": Show correct answers }

procedure TfDecay1.btShowClick(Sender: TObject);

var
  Nuclide: string;

begin
  if (iQuestion <> 0) and (btQuestion.Caption = 'Question') then begin
    // Show only after user has given her answers
    ClearForm(False);
    edZ.Text := IntToStr(aElements[iElement].Z); edA.Text := IntToStr(aElements[iElement].Isotopes[iIsotope].A);
    edN.Text := IntToStr(aElements[iElement].Isotopes[iIsotope].A - aElements[iElement].Z);
    edIsotope2.Text := recNewElement.Symbol + '-' + IntToStr(recNewElement.Isotopes[0].A);
    edZ2.Text := IntToStr(recNewElement.Z); edA2.Text := IntToStr(recNewElement.Isotopes[0].A);
    edN2.Text := IntToStr(recNewElement.Isotopes[0].A - recNewElement.Z);
    edEquation.Text := sEquation;
    Nuclide := IntToStr(aElements[iElement].Z) + aElements[iElement].Symbol + '-' + IntToStr(aElements[iElement].Isotopes[iIsotope].A);
    edEquation.Text := StringReplace(edEquation.Text, Nuclide, GetSubscriptNuclide(Nuclide), []);
    Nuclide := IntToStr(recNewElement.Z) + recNewElement.Symbol + '-' + IntToStr(recNewElement.Isotopes[0].A);
    // Equation display with sub- and superscripts
    edEquation.Text := StringReplace(edEquation.Text, Nuclide, GetSubscriptNuclide(Nuclide), []);
    if sDecay = 'α' then
      edEquation.Text := StringReplace(edEquation.Text, '2He-4', GetSubscriptNuclide('2He-4'), []);
    edEquation.Text := StringReplace(edEquation.Text, 'β-', 'β' + SUP_MINUS, []);
    edEquation.Text := StringReplace(edEquation.Text, 'β+', 'β' + SUP_PLUS, []);
    edEquation.Text := StringReplace(edEquation.Text, ' + e-', ' + e' + SUP_MINUS, []);
    edEquation.Text := StringReplace(edEquation.Text, ' + e+', ' + e' + SUP_PLUS, []);
  end;
end;

{ Symbol insert buttons: Insert corresponding string into equation edit field }

procedure TfDecay1.btAlphaClick(Sender: TObject);

begin
  if mSettingsEquAlpha.Checked then
    edEquation.Text := edEquation.Text + 'α'
  else
    edEquation.Text := edEquation.Text + '2He-4';
end;

procedure TfDecay1.btBetaMinusClick(Sender: TObject);

begin
  if mSettingsEquBeta.Checked then
    edEquation.Text := edEquation.Text + 'e-'
  else
    edEquation.Text := edEquation.Text + 'β-';
end;

procedure TfDecay1.btBetaPlusClick(Sender: TObject);

begin
  if mSettingsEquBeta.Checked then
    edEquation.Text := edEquation.Text + 'e+'
  else
    edEquation.Text := edEquation.Text + 'β+';
end;

procedure TfDecay1.btElectronClick(Sender: TObject);

begin
  edEquation.Text := edEquation.Text + 'e-';
end;

procedure TfDecay1.btNeutrinoClick(Sender: TObject);

begin
  edEquation.Text := edEquation.Text + Neutrino;
end;

procedure TfDecay1.btAntiNeutrinoClick(Sender: TObject);

begin
  edEquation.Text := edEquation.Text + AntiNeutrino;
end;

procedure TfDecay1.btIsotope1Click(Sender: TObject);

var
  P: Integer;

begin
  // Nuclide is build by taken into account the corr. isotope, Z and A fields
  if (edZ.Text <> '') and (edA.Text <> '') then begin
    P := Pos('-', edIsotope.Text);
    if P <> 0 then
      edEquation.Text := edEquation.Text + edZ.Text + LeftStr(edIsotope.Text, P - 1) + '-' + edA.Text;
  end;
end;

procedure TfDecay1.btIsotope2Click(Sender: TObject);

var
  P: Integer;

begin
  // Nuclide is build by taken into account the corr. isotope, Z and A fields
  if (edIsotope2.Text <> '') and (edZ2.Text <> '') and (edA2.Text <> '') then begin
    P := Pos('-', edIsotope2.Text);
    if P <> 0 then
      edEquation.Text := edEquation.Text + edZ2.Text + LeftStr(edIsotope2.Text, P - 1) + '-' + edA2.Text;
  end;
end;

procedure TfDecay1.btPlusClick(Sender: TObject);

begin
  edEquation.Text := edEquation.Text + ' + ';
end;

procedure TfDecay1.btEqualsClick(Sender: TObject);

begin
  edEquation.Text := edEquation.Text + ' → ';
end;

end.

