{******************************}
{ Balancing chemical equations }
{******************************}

{ Version 1.2 (November 2022): Rebuild after possible problems with prior version... }

uses
  SysUtils, Crt;

const
  KeyESC = #27;

type
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

var
 NReactants, NProducts, NReactElements, NProdElements: Integer;
 NMolecules, CoeffMax, P, I, J, JX: Integer;
 Equation, EqReactants, EqProducts, S: string;
 Key: Char;
 EqBalanced, Aborted: Boolean;
 Reactants, Products: TMolecules;
 SReactants, SProducts: TSMolecules;
 ReactElements, ProdElements: TElements;
 ProdElementsX: TElementsX;
 ReactCoefficients, ProdCoefficients: TCoefficients;
 Combination: array[1..12] of Integer;

procedure DisplayHelp forward;
procedure GetMolecules(Eq: string; var Molecules: TMolecules; var SMolecules: TSMolecules; var NMolecules: Integer) forward;
procedure GetAtoms(SMolecule: string; var Molecule: TMolecule) forward;
procedure GetElements(Molecules: TMolecules; NMolecules: Integer; var Elements: TElements; var NMolElements: Integer) forward;
procedure TryBalance(M, NMolecules, CoeffMax, NReactants, NProducts, NElements: Integer; Reactants, Products: TMolecules;
  ReactElements, ProdElements: TElements; ProdElementsX: TElementsX; var ReactCoefficients, ProdCoefficients: TCoefficients; var EqBalanced: Boolean) forward;
procedure GetElementNumber(Molecules: TMolecules; NMolecules: Integer; Coeff: TCoefficients; var Elements: TElements) forward;

{ Display help text }

procedure DisplayHelp;

begin
  Writeln('Given an unbalanced equation of a chemical reaction, this program may be used');
  Writeln('to calculate the coefficients for the different molecules in order to balance');
  Writeln('the chemical equation.'); Writeln;
  Writeln('Enter the (unbalanced) chemical equation, using the format: ');
  Writeln('  reactant1 + reactant2 ... -> product1 + product2 ...');
  Writeln('Molecules must not have coefficients; their formulas may contain 2 levels of');
  Writeln('element grouping; please, use square brackets for outer and round brackets for');
  Writeln('inner groups.'); Writeln;
  Writeln('Note that the program does NOT check the validity of the equation you enter.');
  Writeln('It just supposes that it''s correct, from the point of view of chemistry as');
  Writeln('well as from the point of view of the formats used.');
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

{--------------}
{ Main program }
{--------------}

begin
  ClrScr;
  TextColor(Yellow);
  Writeln('Chemistry: Balancing chemical equations.');
  Writeln('========================================');
  Writeln;
  TextColor(LightGray);
  // Do chemical equation balancing until user terminates the program
  repeat
    // Read the equation (or "H") from keyboard
    for I := 4 to 24 do begin
      GotoXY(1, I);
      ClrEoL;
    end;
    GotoXY(1, 4);
    Writeln('Equation (or "H" for Help)');
    Write('  ? '); Readln(Equation); Writeln;
    // Entering "H" displays the help text
    if (Equation = 'h') or (Equation = 'H') then
      DisplayHelp
    // Equation considered valid if satisfies some rudimentary checking
    else if (Length(Equation) >= 10) and (Pos(' + ', Equation) > 0) and (Pos(' -> ', Equation) > 0) then begin
      // Extract reactants and products part of the equation
      P := Pos(' -> ', Equation);
      EqReactants := LeftStr(Equation, P - 1);
      EqProducts  := RightStr(Equation, Length(Equation) - (P + 3));
      // Get the reactants/products molecules
      NReactants := 0; NProducts := 0;
      GetMolecules(EqReactants, Reactants, SReactants, NReactants);
      GetMolecules(EqProducts, Products, SProducts, NProducts);
      // Get all reactant/products elements
      NReactElements := 0; NProdElements := 0;
      GetElements(Reactants, NReactants, ReactElements, NReactElements);
      GetElements(Products, NProducts, ProdElements, NProdElements);
      // Number of reactants/products arbitrarily fixed to 6
      if (NReactants > 6) or (NProducts > 6) then
        Writeln('PROGRAM ERROR: Maximum number of reactants/products supported = 6!')
      // Number of reactants/products elements arbitrarily fixed to 12
      else if (NReactElements > 12) or (NProdElements > 12) then
        Writeln('PROGRAM ERROR: Maximum number of elements supported = 12!')
      // Number of elements must be the same on both sides of the equation
      else if NReactElements <> NProdElements then
        Writeln('INPUT ERROR: Number of reactants and products elements do not match!')
      // Equation considerd ok: Do the calculations
      else begin
        // Display table with number of occurence of each element on both sides of the equation
        Writeln('Element     Reactants     Products     Balanced?');
        Writeln('------------------------------------------------');
        Aborted := False;
        for I := 1 to NReactElements do begin
          Write('   ', ReactElements[I].Element);
          if Length(ReactElements[I].Element) = 1 then
            Write(' ');
          Write('          ', ReactElements[I].NElement:2);
          JX := 0;
          // For each reactants element, find identical product element
          for J := 1 to NProdElements do begin
            if ProdElements[J].Element = ReactElements[I].Element then
              JX := J;                                                         // remember product element's index
          end;
          if JX <> 0 then begin
            // Product element (identical to given reactant element) found
            Write('            ', ProdElements[JX].NElement:2, '           ');
            if ReactElements[I].NElement = ProdElements[JX].NElement then
              Writeln('yes')
            else
              Writeln('no');
          end
          else begin
            // No product element (identical to given reactant element) found:
            // The equation must contain an error!
            TextColor(LightRed);
            Writeln('           ERROR!');
            TextColor(LightGray);
            Aborted := True;
          end;
        end;
        // If there are no errors, try to balance the equation
        if not Aborted then begin
          // Create array containing the products elements indexes for a given reactants elements
          // This is done to avoid to recalculate them in the GetElementNumber procedure
          for I := 1 to NReactElements do begin
            for J := 1 to NReactElements do begin
              if ProdElements[J].Element = ReactElements[I].Element then
                ProdElementsX[I] := J;
            end;
          end;
          // Initial values for the brute force balancing
          NMolecules := NReactants + NProducts;                                // number of values = number of coefficients
          CoeffMax := 6;                                                       // values to be used = 1 .. highest coefficient expected
          EqBalanced := False;
          for I := 1 to 6 do begin
            ReactCoefficients[I] := 1;
            ProdCoefficients[I] := 1;
          end;
          // Try balancing (with increasing highest coefficient expected value)
          // until solution found or user chooses to abort
          repeat
            GotoXY(1, 10 + NReactElements); ClrEoL;
            Write('Balancing - trying with maximum coefficient = ', CoeffMax);
            // Call to the recursive balancing procedure
            // If the Boolean is set to TRUE, the coefficients corresponding to the balanced
            // equation will be contained in the reactants and products arrays
            TryBalance(1, NMolecules, CoeffMax, NReactants, NProducts, NReactElements, Reactants, Products,
              ReactElements, ProdElements, ProdElementsX, ReactCoefficients, ProdCoefficients, EqBalanced);
            // Solution has been found: Display it!
            if EqBalanced then begin
              Writeln(': OK');
              Writeln; Writeln('Balanced equation:'); Write('  ');
              for I := 1 to NReactants do begin
                if ReactCoefficients[I] > 1 then
                  Write(ReactCoefficients[I], ' ');
                Write(SReactants[I]);
                if I < NReactants then
                  Write(' + ');
              end;
              Write(' -> ');
              for I := 1 to NProducts do begin
                if ProdCoefficients[I] > 1 then
                  Write(ProdCoefficients[I], ' ');
                Write(SProducts[I]);
                if I < NProducts then
                  Write(' + ');
              end;
              Writeln;
            end
            // No solution found...
            else begin
              if CoeffMax = 6 then
                // Retry with a bigger value for the highest coefficient expected
                CoeffMax := 12
              else begin
                // Give user the choice to continue trying or to abort
                Writeln; Writeln;
                Write('Sorry. Could not balance this equation! Retry [y/n]? '); Readln(S);
                if (S = 'y') or (S = 'Y') then begin
                  GotoXY(1, 12 + NReactElements); ClrEoL;
                  CoeffMax *= 2;
                end
                else
                  Aborted := True;
              end;
            end;
          until EqBalanced or Aborted;
        end;
      end;
    end;
    // Terminating the program with the ESC key (or doing another balancing)
    if Equation = '' then
      Key := #27
    else begin
      Writeln;
      Write('Hit ESC to terminate, any other key to continue...');
      Key := ReadKey;
      if Key = #0 then
        Key := ReadKey;
    end;
  until Key = KeyESC;
end.

