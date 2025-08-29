{*******************************************}
{* Main unit for Stoichiometry application *}
{*******************************************}

unit stoichiometry_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, PopupNotifier;

type
  TEquations = array of string;
  TEquationsDone = array of Boolean;
  TElement = record
    Element: string;
    NElement: Integer;
  end;
  TMolecule = record
    NAtoms: Integer;
    Atoms: array[1..12] of TElement;
  end;
  TSMolecules = array[1..12] of string;
  TMolecules = array[1..6] of TMolecule;
  TElements  = array[1..12] of TElement;
  TElementsX = array[1..6] of Integer;
  TCoefficients = array[1..6] of Integer;
  { TfStoichiometry }
  TfStoichiometry = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileNew, mFileExit: TMenuItem;
    mSettings, mSettingsNQuestions, mSettingsInclude: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    laEquation: TLabel;
    edEquation: TEdit;
    laMolecule1, laMolecule2, laMolecule3, laMolecule4, laMolecule5, laMolecule6: TLabel;
    edCoeffMolecule1, edCoeffMolecule2, edCoeffMolecule3, edCoeffMolecule4, edCoeffMolecule5, edCoeffMolecule6: TEdit;
    edBalancedEquation: TEdit;
    Label1, Label2, Label3, Label4: TLabel;
    edQuestions: TEdit;
    edCorrect: TEdit;
    edFalse: TEdit;
    edSuccess: TEdit;
    btQuestion: TButton;
    pnHelp: TPopupNotifier;
    procedure FormCreate(Sender: TObject);
    procedure mFileNewClick(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsNQuestionsClick(Sender: TObject);
    procedure mSettingsIncludeClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btQuestionClick(Sender: TObject);
  private
    iEquations, iTestEquations, iTestEquation, iCorrect, iFalse, iReactants, iProducts, iMolecules: Integer;
    sEquation: string;
    aEquations: TEquations;
    aEquationsDone: TEquationsDone;
    aCoefficients: array[1..6] of Integer;
    aMolCoeff: array[1..6] of TEdit;
    aMolNames: array[1..6] of TLabel;
  end;

var
  fStoichiometry: TfStoichiometry;
  Combination: array[1..12] of Integer;

implementation

{$R *.lfm}

{ Transform molecule or complete chemical equation by using subscripts for atom numbers }

function SubscriptEquation(Equation0: string): string;

const
  Subscripts: array[0..9] of string = (
    #$E2#$82#$80, #$E2#$82#$81, #$E2#$82#$82, #$E2#$82#$83, #$E2#$82#$84,
    #$E2#$82#$85, #$E2#$82#$86, #$E2#$82#$87, #$E2#$82#$88, #$E2#$82#$89
  );

var
  I: Integer;
  Equation, S: string;
  Ch: Char;

begin
  Equation := '';
  for I := 1 to Length(Equation0) do begin
    S := Copy(Equation0, I, 1); Ch := S[1];
    if Ch in ['0'..'9'] then
      Equation += Subscripts[StrToInt(Ch)]
    else
      Equation += Ch;
  end;
  SubscriptEquation := Equation;
end;

{ Reading chemical equations from text file }

procedure ReadEquations(out N: Integer; out Equations: TEquations);

const
  Filename = 'stoichiometry.txt';

var
  Equation: string;
  InFile: Text;

begin
  Assign(InFile, Filename); Reset(Infile); N := 0;
  while not EoF(Infile) do begin
    Readln(Infile, Equation);
    if Equation <> '' then begin
      // Add spaces before and after '+' and '→'; remove molecule state information
      Equation := StringReplace(Equation, '+', ' + ', [rfReplaceAll]);
      Equation := StringReplace(Equation, '→', ' → ', [rfReplaceAll]);
      Equation := StringReplace(Equation, '(s)', '', [rfReplaceAll]);
      Equation := StringReplace(Equation, '(l)', '', [rfReplaceAll]);
      Equation := StringReplace(Equation, '(g)', '', [rfReplaceAll]);
      Equation := StringReplace(Equation, '(aq)', '', [rfReplaceAll]);
      Inc(N); SetLength(Equations, N);
      Equations[N - 1] := Equation;                                            // store equations into array
    end;
  end;
  Close(Infile);
end;

{ Preparations to start a new test: Reset all }

procedure NewTest(T: Integer; out N, Q, C, F: Integer; out EqDone: TEquationsDone);

var
  I: Integer;

begin
  fStoichiometry.laEquation.Caption := 'Equation';
  fStoichiometry.edEquation.Text := '';
  fStoichiometry.edQuestions.Text := '';
  fStoichiometry.edCorrect.Text := '';
  fStoichiometry.edFalse.Text := '';
  fStoichiometry.edSuccess.Text := '';
  fStoichiometry.edSuccess.Color := clDefault;
  SetLength(EqDone, T);
  for I := 0 to T - 1 do
    EqDone[I] := False;
  N := 10; Q := 0; C := 0; F := 0;
  fStoichiometry.btQuestion.Caption := 'Question';
  fStoichiometry.btQuestion.Enabled := True;
end;

{ The equation balancing procedures are exactly the same as used in my "chemeq" command line program }
{ -------------------------------------------------------------------------------------------------- }

{ Extract atoms from molecule string and store them into a TMolecule record }

procedure GetAtoms(SMolecule: string; var Molecule: TMolecule);

var
  NAtms, NElemnt, Bracket1Elemnt1, Bracket2Elemnt1, BracketElemnt1, BracketElemnt2, NBracketElemnts, I, J: Integer; Elemnt: string;

begin
  I := 0; NAtms := 0;
  // Parse the string character by character
  while I < Length(SMolecule) do begin
    Inc(I);
    // This starts an atoms group
    if SMolecule[I] = '(' then
      Bracket1Elemnt1 := NAtms + 1                                             // remember this group's starting atom index
    // This starts another atoms group
    else if SMolecule[I] = '[' then
      Bracket2Elemnt1 := NAtms + 1                                             // remember this group's starting atom index
    // This ends the one or the other atoms groups
    else if (SMolecule[I] = ')') or (SMolecule[I] = ']') then begin
      // Get first atom's index (depending on which group it actually is)
      if SMolecule[I] = ')' then
        BracketElemnt1 := Bracket1Elemnt1
      else
        BracketElemnt1 := Bracket2Elemnt1;
      // The group's ending atom index
      BracketElemnt2 := NAtms;
      // Brackets are supposed to be followed by 1 or 2 numbers
      if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
        Inc(I);
        NBracketElemnts := StrToInt(SMolecule[I]);
        if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
          Inc(I);
          NBracketElemnts := 10 * NBracketElemnts + StrToInt(SMolecule[I]);
        end;
      end;
      // Multiply occurence of all atoms of the group by the number succeeding the bracket
      for J := BracketElemnt1 to BracketElemnt2 do
        Molecule.Atoms[J].NElement *= NBracketElemnts;
    end
    // An uppercase letter indicates a new atom
    else begin
      if SMolecule[I] in ['A'..'Z'] then begin
        Elemnt := SMolecule[I]; NElemnt := 1; Inc(NAtms);
        // The atom may be just an uppercase letter or an uppercase plus a lowercase letter
        if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['a'..'z']) then begin
          Inc(I);
          Elemnt += SMolecule[I];
        end;
        // The atom letter(s) may be followed by 1 or 2 numbers (of occurence)
        if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['1'..'9']) then begin
          Inc(I);
          NElemnt := StrToInt(SMolecule[I]);
          if (I + 1 <= Length(SMolecule)) and (SMolecule[I + 1] in ['0'..'9']) then begin
            Inc(I);
            NElemnt := 10 * NElemnt + StrToInt(SMolecule[I]);
          end;
        end;
        // Fill in the molecule's atom and its number of occurences
        Molecule.Atoms[NAtms].Element := Elemnt;
        Molecule.Atoms[NAtms].NElement := NElemnt;
      end;
      // Fill in the molecule's total number of atoms
      Molecule.NAtoms := NAtms;
    end;
  end;
end;

{ Extract reactants/products from (partial ) chemical equation }

procedure GetMolecules(Eq: string; var Molecules: TMolecules; var SMolecules: TSMolecules; var NMolecules: Integer);

var
  P: Integer;

begin
  NMolecules := 0;
  repeat
    Inc(NMolecules);                                                           // number of reactants/products
    if NMolecules <= 6 then begin                                              // number arbitrarily limited to 6
      P := Pos(' + ', Eq);
      if P > 0 then
        SMolecules[NMolecules] := LeftStr(Eq, P - 1)                           // molecule as string
      else
        SMolecules[NMolecules] := Eq;
      GetAtoms(SMolecules[NMolecules], Molecules[NMolecules]);                 // molecule as TMolecule record
      Eq := RightStr(Eq, Length(Eq) - (P + 2));
    end;
  until (P = 0) or (NMolecules > 6);
end;

{ Extract elements of reactant/product molecule }

procedure GetElements(Molecules: TMolecules; NMolecules: Integer; var Elements: TElements; var NMolElements: Integer);

var
  I, J, K, KX: Integer;
  NewElement: Boolean;

begin
  NMolElements := 0;                                                           // number of elements
  for I := 1 to NMolecules do begin
    // For each molecule
    for J := 1 to Molecules[I].NAtoms do begin
      // For each of its atoms
      NewElement := True;
      if NMolElements <= 12 then begin                                         // number arbitrarily limited to 12
        // Check if this element is already in the elements array
        for K := 1 to NMolElements do begin
          if Molecules[I].Atoms[J].Element = Elements[K].Element then begin
            // If yes, remember the index in the array
            NewElement := False;
            KX := K;
          end;
        end;
      end;
      if NewElement then
        Inc(NMolElements);
      if NMolElements <= 12 then begin
        if NewElement then begin
          // New element: add it together with the number of occurences to the array
          Elements[NMolElements].Element := Molecules[I].Atoms[J].Element;
          Elements[NMolElements].NElement := Molecules[I].Atoms[J].NElement;
        end
        else
          // Not new element: update number of occurences
          Elements[KX].NElement += Molecules[I].Atoms[J].NElement;
      end;
    end;
  end;
end;

{ Get number of elements occurences (for the coefficients specified) }

procedure GetElementNumber(Molecules: TMolecules; NMolecules: Integer; Coeff: TCoefficients; var Elements: TElements);

var
  N, I, J, K, KX: Integer;
  NewElement: Boolean;

// The procedure is similar to GetElements, except the take in consideration of the coefficients
// and not doing again the calculations already done in that routine

begin
  N := 0;
  for I := 1 to NMolecules do begin
    for J := 1 to Molecules[I].NAtoms do begin
      NewElement := True;
      for K := 1 to N do begin
        if Molecules[I].Atoms[J].Element = Elements[K].Element then begin
          NewElement := False;
          KX := K;
        end;
      end;
      if NewElement then begin
        Inc(N);
        Elements[N].NElement := Coeff[I] * Molecules[I].Atoms[J].NElement;
      end
      else
        Elements[KX].NElement += Coeff[I] * Molecules[I].Atoms[J].NElement;
    end;
  end;
end;

{ Equation balancing (using brute force method) }

procedure TryBalance(M, NMolecules, CoeffMax, NReactants, NProducts, NElements: Integer; Reactants, Products: TMolecules;
  ReactElements, ProdElements: TElements; ProdElementsX: TElementsX; var ReactCoefficients, ProdCoefficients: TCoefficients; var EqBalanced: Boolean);

// The procedure determines all possible combinations of coefficients for the different reactants and products of the equation,
// then for each of these combinations calculates the number of occurences of the different elements. If this number is equal
// for all elements, the equation is balanced and the (recursive) procedure exits.

var
   N, I: Integer;
   Balanced: Boolean;
   RCoefficients, PCoefficients: TCoefficients;

begin
  // Combination has been generated
  if (M > NMolecules) then begin
    // First part of combination values as reactant coefficients
    for I := 1 to NReactants do
      RCoefficients[I] := Combination[I];
    // Second part of combination values as product coefficients
    for I := 1 to NProducts do
      PCoefficients[I] := Combination[NReactants + I];
    // Calculate number of occurences for all elements
    GetElementNumber(Reactants, NReactants, RCoefficients, ReactElements);
    GetElementNumber(Products, NProducts, PCoefficients, ProdElements);
    Balanced := True; I := 0;
    // Compare number of occurences of reactants and products
    repeat
      Inc(I);
      if ReactElements[I].NElement <> ProdElements[ProdElementsX[I]].NElement then
        // If one of these number differs, the equation isn't yet balanced
        Balanced := False;
    until (I = NElements) or not Balanced;
    // All occurence numbers equal: equation is balanced; save coefficients
    if Balanced then begin
      EqBalanced := True;
      ReactCoefficients := RCoefficients;
      ProdCoefficients := PCoefficients;
    end;
  end
  // Combination has to be generated
  else begin
    if not EqBalanced then begin                                               // no need to do so if the equation is already balanced
      // Generate combination values (= coefficients) from 1 to a
      // previously fixed maximum (cf. main program))
      for N := 1 to CoeffMax do begin
        Combination[M] := N;
        // Recursive call of the balancing procedure
        TryBalance(M + 1, NMolecules, CoeffMax, NReactants, NProducts, NElements, Reactants, Products,
          ReactElements, ProdElements, ProdElementsX, ReactCoefficients, ProdCoefficients, EqBalanced);
      end;
    end;
  end;
end;

{ *************** }
{ TfStoichiometry }
{ *************** }

{ Application start: Initialisation }

procedure TfStoichiometry.FormCreate(Sender: TObject);

begin
  // Create arrays with molecule labels and coefficient edit fields
  aMolNames[1] := laMolecule1; aMolNames[2] := laMolecule2; aMolNames[3] := laMolecule3;
  aMolNames[4] := laMolecule4; aMolNames[5] := laMolecule5; aMolNames[6] := laMolecule6;
  aMolCoeff[1] := edCoeffMolecule1; aMolCoeff[2] := edCoeffMolecule2; aMolCoeff[3] := edCoeffMolecule3;
  aMolCoeff[4] := edCoeffMolecule4; aMolCoeff[5] := edCoeffMolecule5; aMolCoeff[6] := edCoeffMolecule6;
  SetLength(aEquations, 0);
  // Read equations from file
  ReadEquations(iEquations, aEquations);
  // Start random number generator
  Randomize;
  // Start a new test
  NewTest(Length(aEquations), iTestEquations, iTestEquation, iCorrect, iFalse, aEquationsDone);
end;

{ Menu item "File > New": Start a new test }

procedure TfStoichiometry.mFileNewClick(Sender: TObject);

begin
  NewTest(Length(aEquations), iTestEquations, iTestEquation, iCorrect, iFalse, aEquationsDone);
end;

{ Menu item "File > Exit": Exit the application }

procedure TfStoichiometry.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Number of questions ...": Get number of exercises from user }

procedure TfStoichiometry.mSettingsNQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Stoichiometry', 'Enter number of test questions', IntToStr(iTestEquations));
  if S <> '' then begin                                                        // if button pressed was "OK" (not "Cancel")
    iTestEquations := StrToInt(S);
    if iTestEquations > iEquations then
      iTestEquations := iEquations - 10;                                       // 10 items security for possible file errors
  end;
end;

{ Menu item "Settings > Include balanced equations": Choose if equations already balanced have to be included as exercises }

procedure TfStoichiometry.mSettingsIncludeClick(Sender: TObject);

begin
  if mSettingsInclude.Checked then
    mSettingsInclude.Checked := False
  else
    mSettingsInclude.Checked := True;
end;

{ Menu item "Help > Help": Display (brief) program help text }

procedure TfStoichiometry.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Hide;
  S := 'Choose the number of test questions in the "Settings" menu, push the "Question" button to ';
  S += 'generate an exercisw, fill in the coefficients for the chemical reaction displayed and push ';
  S += '"Answer" to check your solution. To start a new test, click "New" in the "File" menu.';
  pnHelp.Text := S;
  pnHelp.Show;
end;

{ Menu item "Help > About": Display program about }

procedure TfStoichiometry.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  if pnHelp.Visible then
    pnHelp.Hide;
  S := 'Chemistry exercises generator: Balancing chemical equations.' + Chr(13) + Chr(13);
  S += 'Version 1.0, © allu, August, 2018.';
  pnHelp.Text := S;
  pnHelp.Show;
end;

{ Button "Question/Answer": Generate exercise resp. test user solution }

procedure TfStoichiometry.btQuestionClick(Sender: TObject);

// The chemical equations aren't really generated, but are taken as such from samples in a text file (actually 180 chemical reactions)

var
  NReactElements, NProdElements: Integer;
  NMolecules, CoeffMax, P, I, J, JX: Integer;
  Success: Real;
  EquReactants, EquProducts, EquBalanced, Mess: string;
  EqBalanced, OK: Boolean;
  Reactants, Products: TMolecules;
  SReactants, SProducts: TSMolecules;
  ReactElements, ProdElements: TElements;
  ProdElementsX: TElementsX;
  ReactCoefficients, ProdCoefficients: TCoefficients;

begin
  // Generate chemical equation balancing exercise
  if btQuestion.Caption = 'Question' then begin;
    Inc(iTestEquation);
    edQuestions.Text := IntToStr(iTestEquation);
    Mess := '';
    laEquation.Caption := 'Equation ' + IntToStr(iTestEquation) + '/' + IntToStr(iTestEquations);
    btQuestion.Caption := 'Answer';                                            // change button capture here, as balancing may take some time...
    // Get a random equation not yet done (excluding already balanced equations, if this is selected)
    repeat
      OK := True;
      J := Random(iEquations);
      if aEquationsDone[J] then
        OK := False
      else if (not mSettingsInclude.Checked) and (LeftStr(aEquations[J], 1) = '*') then
        OK := False;
    until OK;
    sEquation := aEquations[J];
    sEquation := StringReplace(sEquation, '*', '', []);
    aEquationsDone[J] := True;
    edEquation.Text := SubscriptEquation(sEquation);                           // display equation with atom numbers as subscripts
    edBalancedEquation.Text := '';
    // Make all invisible here; change later as needed
    for I := 1 to 6 do begin
      aMolNames[I].Visible := False;
      aMolCoeff[I].Visible := False;
      aMolCoeff[I].Color := clDefault;
    end;
    // Extract reactants and products part of the equation
    P := Pos(' → ', sEquation);
    EquReactants := LeftStr(sEquation, P - 1);
    EquProducts  := RightStr(sEquation, Length(sEquation) - (P + 4));
    // Get the reactants/products molecules
    iReactants := 0; iProducts := 0;
    GetMolecules(EquReactants, Reactants, SReactants, iReactants);
    GetMolecules(EquProducts, Products, SProducts, iProducts);
    // Get all reactant/products elements
    NReactElements := 0; NProdElements := 0;
    GetElements(Reactants, iReactants, ReactElements, NReactElements);
    GetElements(Products, iProducts, ProdElements, NProdElements);
    // Maximum number of molecules (reactants + products) arbitrarily fixed to 6
    if iReactants + iProducts > 6 then
      Mess := 'Maximum number of molecules supported = 6!'
    // Number of reactants/products elements arbitrarily fixed to 12
    else if (NReactElements > 12) or (NProdElements > 12) then
      Mess := 'Maximum number of elements supported = 12!'
    // Number of elements must be the same on both sides of the equation
    else if NReactElements <> NProdElements then
      Mess := 'Number of reactants and products elements do not match!'
    // Equation considerd ok: Do the calculations
    else begin
      // Find occurence of each element on both sides of the equation
      for I := 1 to NReactElements do begin
        JX := 0;
        // For each reactants element, find identical product element
        for J := 1 to NProdElements do begin
          if ProdElements[J].Element = ReactElements[I].Element then
            JX := J;
        end;
        if JX = 0 then
          // No product element (identical to given reactant element) found:
          // The equation must contain an error!
          Mess := 'Reactant and product elements do not match';
      end;
      // If there are no errors, try to balance the equation
      if Mess = '' then begin
        // Create array containing the product elements indexes for a given reactant element
        // This is done to avoid to recalculate them in the GetElementNumber procedure
        for I := 1 to NReactElements do begin
          for J := 1 to NReactElements do begin
            if ProdElements[J].Element = ReactElements[I].Element then
              ProdElementsX[I] := J;
          end;
        end;
        // Initial values for the brute force balancing
        NMolecules := iReactants + iProducts;                                // number of values = number of coefficients
        CoeffMax := 6;                                                       // values to be used = 1 .. highest coefficient expected
        EqBalanced := False;
        for I := 1 to 6 do begin
          ReactCoefficients[I] := 1;
          ProdCoefficients[I] := 1;
        end;
        // Try balancing (with increasing highest coefficient expected value)
        // until solution found or abort if no solution found
        repeat
          // Call to the recursive balancing procedure
          // If the Boolean is set to TRUE, the coefficients corresponding to the balanced
          // equation will be contained in the reactants and products arrays
          TryBalance(1, NMolecules, CoeffMax, iReactants, iProducts, NReactElements, Reactants, Products,
            ReactElements, ProdElements, ProdElementsX, ReactCoefficients, ProdCoefficients, EqBalanced);
          // Solution has been found!
          if not EqBalanced then begin
            // No solution found...
            if CoeffMax = 6 then
              // Retry with a bigger value for the highest coefficient expected
              CoeffMax := 12
            else if CoeffMax = 12 then
              // Last trial with even bigger value for the highest coefficient expected
              CoeffMax := 20
            else
              // Abort balancing; ignore this equation (and display error message)
              Mess := 'Could not balance ' + sEquation;
          end;
        until EqBalanced or (Mess <> '');
        // Equation has successfully been balanced: proceed with exercise generation
        if EqBalanced then begin
          J := 0;
          for I := 1 to iReactants do begin
            Inc(J);
            aCoefficients[J] := ReactCoefficients[I];                          // store reactant coefficients into array
            aMolNames[J].Caption := SubscriptEquation(SReactants[I]);          // display reactants (with atom numbers as subscripts)
            aMolCoeff[J].Text := '1';                                          // set reactant coefficients on form to "1"
          end;
          for I := 1 to iProducts do begin
            Inc(J);
            aCoefficients[J] := ProdCoefficients[I];                           // store product coefficients into array
            aMolNames[J].Caption := SubscriptEquation(SProducts[I]);           // display products (with atom numbers as subscripts)
            aMolCoeff[J].Text := '1';                                          // set product coefficients on form to "1"
          end;
          iMolecules := J;
          for I := 1 to iMolecules do begin                                    // Make visible as many form fields as actually used
            aMolNames[I].Visible := True;
            aMolCoeff[I].Visible := True;
          end;
        end;
      end;
    end;
    // Ignore this equation if an error occured (normally this shouldn't happen!)
    if Mess <> '' then begin
      MessageDlg('Program error', Mess + '!', mtError, [mbOK], 0);
      btQuestion.Caption := 'Question';
      Dec(iTestEquation);
    end;
  end
  // Check validity of user answer
  else begin
    EquBalanced := ''; OK := True;
    // Create a string with the balanced equation and display it
    for I := 1 to iMolecules do begin
      if aCoefficients[I] > 1 then
        EquBalanced += IntToStr(aCoefficients[I]);
      EquBalanced += aMolNames[I].Caption;
      if I = iReactants then
        EquBalanced += ' → '
      else if I <> iMolecules then
        EquBalanced += ' + ';
      if StrToInt(aMolCoeff[I].Text) <> aCoefficients[I] then begin
        aMolCoeff[I].Color := clRed;
        OK := False;
      end;
    end;
    edBalancedEquation.Text := EquBalanced;
    // User answer is correct
    if OK then
      Inc(iCorrect)
    // User answer is false
    else
      Inc(iFalse);
    // Display correct and false answers and success percentage
    edCorrect.Text := IntToStr(iCorrect);
    edFalse.Text := IntToStr(iFalse);
    Success := 100 * (iCorrect / iTestEquation);
    edSuccess.Text := IntToStr(Round(Success)) + ' %';
    if Success < 50 then
      edSuccess.Color := clRed
    else if Success < 60 then
      edSuccess.Color := clYellow
    else
      edSuccess.Color := clLime;
    // Next button push will be for another question
    btQuestion.Caption := 'Question';
    // The number of questions selected has been reached...
    if iTestEquation = iTestEquations then begin
      MessageDlg('Test done', 'Choose "New" in the "File" menu to start a new test', mtInformation, [mbOK], 0);
      btQuestion.Enabled := False;                                             // Disable the button (until "New" has been clicked)
    end;
  end;
end;

end.

