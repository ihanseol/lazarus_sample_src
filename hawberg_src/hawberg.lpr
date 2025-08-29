{****************************}
{*  The Hardy-Weinberg Law  *}
{*  (allu, Mai-June 2018)   *}
{****************************}

program hawberg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  Crt;

const
  MaxAlleles = 5;
  MaxGametes = 5;
  MaxGenotypes = 15;

Type
  TAlleles = array[1..MaxAlleles] of string;
  TAlleleNumbers = array[1..MaxAlleles] of Real;
  TGenotypes = array[1..MaxGenotypes] of string;
  TGenotypeNumbers = array[1..MaxGenotypes] of Real;
  TGametes = array[1..MaxGametes] of string;

var
  NAlleles, NGenotypesMale, NGenotypesFemale, NGametesMaleX, NGametesMaleY, NGametesFemale: Integer;
  Population, LineLength, I, J, Code: Integer;
  Freq: Real;
  Species, Trait, Notes, S: string;
  SexLink, Key: Char;
  Percentage: Boolean;
  Alleles: TAlleles;
  AlleleFrequencies: TAlleleNumbers;
  GenotypesMale, GenotypesFemale: TGenotypes;
  GenotypeCounts, GenotypeFrequencies, HWFrequenciesMale, HWFrequenciesFemale: TGenotypeNumbers;
  GametesMaleX, GametesMaleY, GametesFemale: TGametes;

{ Display program title }

procedure WriteTitle(Title: string);

var
  I: Integer;

begin
  ClrScr; TextColor(Yellow);
  Writeln(Title);
  for I := 1 to Length(Title) do
    Write('=');
  Writeln; Writeln;
  TextColor(LightGray);
end;

{ Partial screen clearing }

procedure ClearLines(Y1, Y2: Integer);

var
  I: Integer;

begin
  for I := Y1 to Y2 do begin
    GotoXY(1, I); ClrEoL;
  end;
  GotoXY(1, Y1);
end;

{ Enter study data: species, trait, study-details, number of alleles and X-linked info }

procedure ReadStudyData(out Species, Trait, Notes: string; out NAlleles: Integer; out SexLink: Char);

var
  C: Integer;
  S: string;

begin
  Write('Species name           ? '); Readln(Species);
  if Species = '' then
    Species := 'Sample species';
  Write('Trait name             ? '); Readln(Trait);
  if Trait = '' then
    Trait := 'sample trait';
  Write('Study details          ? '); Readln(Notes);
  repeat
    Write('Number of alleles      ? '); Readln(S);
    Val(S, NAlleles, C);
  until (C = 0) and (NAlleles >= 1) and (NAlleles <= 5);
  repeat
    Write('Sex-linked trait (y/n) ? '); Readln(S);
    if S = '' then
      SexLink := 'N'
    else begin
      SexLink := ' ';
      if Length(S) = 1 then begin
        if (S = 'Y') or (S = 'y') then
          SexLink := 'X'
        else if (S = 'N') or (S = 'n') then
          SexLink := 'N';
      end;
    end;
  until SexLink in ['N', 'X'];
  Writeln;
end;

{ Enter alleles symbols (1 character, for proper Punnett Square display) }

procedure ReadAlleles(NAlleles: Integer; out Alleles: TAlleles);

var
  I: Integer;
  S: string;
  OK: Boolean;

begin
  Writeln('Alleles:'); Writeln;
  I := 1;
  repeat
    Write('  Allele ', I, ' ? '); Readln(S);
    if S = '' then
      OK := False
    else if Length(S) > 1 then begin
      Writeln('  Please, use 1-character allele symbols!');
      OK := False;
    end
    else begin
      OK := True;
      if I > 1 then begin
        for J := 1 to I - 1 do begin
          if S = Alleles[J] then
            OK := False;
        end;
      end;
    end;
    if OK then begin
      Alleles[I] := S;
      Inc(I);
    end;
  until I > NAlleles;
end;

{ Enter population size }

procedure ReadPopulationSize(out PopSize: Integer; out Percent: Boolean);

var
  S: string;
  C: Integer;

begin
  repeat
    Write('Population size (ENTER = 100%) ? '); Readln(S);
    if S = '' then begin                                                       // if nothing entered, numbers are supposed to be percentages
      PopSize := 100;
      Percent := True;
      C := 0;
    end
    else begin
      Val(S, PopSize, C);
      Percent := False;
    end;
  until (C = 0) and (PopSize >= 100);                                          // arbitrary limit of population size
end;

{ Enter genotype counts (or percentages) }

procedure ReadGenotypeNumbers(var Genotypes: TGenotypes; NGenotypes: Integer; SexLink: Char; out GenotypeCounts: TGenotypeNumbers);

var
  I: Integer;
  Sum: Real;
  S, Txt: string;
  OK: Boolean;

begin
  Txt := 'Genotype counts in the studied population:';
  if SexLink = 'X' then
    Txt := StringReplace(Txt, 'Genotype', 'Male genotype', []);
  if Percentage then
    Txt := StringReplace(Txt, 'counts', 'percentages', []);
  repeat
    ClearLines(6, 45); Writeln(Txt); Writeln;
    Sum := 0;
    for I := 1 to NGenotypes - 1 do begin
      repeat
        Write('  ', Genotypes[I], ' ? '); Readln(S);
        Val(S, GenotypeCounts[I], Code);
        if Code = 0 then begin
          // Use integers for counts (decimal numbers ok if percentages)
          if not Percentage and (GenotypeCounts[I] <> Round(GenotypeCounts[I])) then begin
            GenotypeCounts[I] := Round(GenotypeCounts[I]);
            Writeln('  value rounded to ', GenotypeCounts[I]:0:0);
          end;
          Sum += GenotypeCounts[I];
        end;
      until Code = 0;
    end;
    if Sum <= Population then begin
      // Fill in count for "last" genotype automatically
      GenotypeCounts[NGenotypes] := Population - Sum;
      Write('  ', Genotypes[NGenotypes], ' = ');
      if Percentage and (GenotypeCounts[NGenotypes] <> Round(GenotypeCounts[NGenotypes])) then
        Writeln(GenotypeCounts[NGenotypes]:0:2)
      else begin
        GenotypeCounts[NGenotypes] := Round(GenotypeCounts[NGenotypes]);
        Writeln(GenotypeCounts[NGenotypes]:0:0)
      end;
      Writeln;
      OK := False;
      repeat
        Write('Are these numbers correct (y/n) ? '); Readln(S);
        if S = '' then
          S := 'Y'
        else begin
          if Length(S) = 1 then
            S := UpperCase(S)
          else
            S := '';
        end;
      until (S = 'N') or (S = 'Y');
      if S = 'Y' then
        OK := True;
    end
    // Error message (and data re-input) if sum of counts > population size
    else begin
      S := 'Invalid data: Sum of genotype counts (' + IntToStr(Round(Sum)) + ') > ' + IntToStr(Population) + '!';
      if Percentage then
        S := StringReplace(S, 'counts', 'percentages', []);
      Writeln; Writeln(S);
      Writeln; Write('Hit ENTER for data re-input... '); Readln;
      OK := False;
    end;
  until OK;
end;

{ Determine genotypes from alleles }

procedure GetGenotypes(Alleles: TAlleles; N: Integer; SexLink: Char; var GTMale, GTFemale: TGenotypes; var NGTMale, NGTFemale: Integer);

var
  I, J: Integer;

begin
  NGTMale := 0; NGTFemale := 0;
  for I := 1 to N do begin
    for J := I to N do begin
      Inc(NGTFemale);
      GTFemale[NGTFemale] := Alleles[I] + '/' + Alleles[J];                              // female genotypes
      if SexLink = 'X' then begin
        if J = I then begin
          Inc(NGTMale);
          GTMale[NGTMale] := Alleles[I] + '/' + '-'                                      // male genotypes for X-linked gene
        end;
      end
      else begin
        Inc(NGTMale);
        GTMale[NGTMale] := GTFemale[NGTFemale];                                          // male genotypes for autosomal gene
      end;
    end;
  end;
end;

{ Get genotype frequencies from genotype numbers }

procedure  GetGenotypeFrequencies(var GTNumbers: TGenotypeNumbers; NGT, Pop: Integer; var GTFrequencies: TGenotypeNumbers);

var
  I: Integer;

begin
  for I := 1 to NGT do
    GTFrequencies[I] := GTNumbers[I] / Pop;
end;

{ Calculate frequency of given allele (based on genotype frequency) }

function GetAlleleFrequency(var GTypes: TGenotypes; var GTFrequencies: TGenotypeNumbers; NGT: Integer; Allele: string): Real;

var
  P, I: Integer;
  Frequency: Real;
  Allele1, Allele2: string;

begin
  Frequency := 0;
  for I := 1 to NGT do begin
    P := Pos('/', Gtypes[I]);
    Allele1 := LeftStr(Gtypes[I], P - 1);
    Allele2 := Copy(Gtypes[I], P + 1, Length(Gtypes[I]));
    if (Allele = Allele1) and (Allele = Allele2) then                                    // homozygote
      Frequency += GTFrequencies[I]
    else if (Allele = Allele1) or (Allele = Allele2) then begin
      if Allele2 = '-' then                                                              // X-linked gene (male: Y-chromosome)
        Frequency += GTFrequencies[I]
      else                                                                               // heterozygote
        Frequency += GTFrequencies[I] / 2;
    end;
  end;
  GetAlleleFrequency := Frequency;
end;

{ Calculate allele frequencies based on genotype frequencies }

procedure GetAlleleFrequencies(var Alleles: TAlleles; NAll: Integer; var GTypes: TGenotypes; var GTFrequencies: TGenotypeNumbers; NGT: Integer; var AlFrequencies: TAlleleNumbers);

begin
  for I := 1 to NAll do
    AlFrequencies[I] := GetAlleleFrequency(GTypes, GTFrequencies, NGT, Alleles[I]);
end;

{ Calculate Hardy-Weinberg frequencies }

procedure GetHardyWeinBergFrequencies(var Alleles: TAlleles; var AlleleFreqs: TAlleleNumbers;
  NAlleles: Integer; SexLink, Sex: Char; var HWFreqs: TGenotypeNumbers);

var
  I, J, KF, KM: Integer;

begin
  KF := 0; KM := 0;
  for I := 1 to NAlleles do begin
    for J := I to NAlleles do begin
      Inc(KF);
      // Autosomal gene or "female" calc. for X-linked gene
      if (Sex = 'F') or (SexLink = 'N') then begin
        if Alleles[I] = Alleles[J] then
          HWFreqs[KF] := Sqr(AlleleFreqs[I])                                             // homozygote HW frequency
        else
          HWFreqs[KF] := 2 * AlleleFreqs[I] * AlleleFreqs[J];                            // heterozygote HW frequency
      end
      // "Male" calc. for X-linked gene
      else begin
        if Alleles[I] = Alleles[J] then begin
          Inc(KM);
          HWFreqs[KM] := AlleleFreqs[I];                                                 // HW frequency = allele frequency
        end;
      end;
    end;
  end;
end;

{ Determine gametes from genotypes }

procedure GetGametes(var Gtypes: TGenotypes; NGtypes: Integer; var Gametes: TGametes; var NGam: Integer);

var
  I, P: Integer;
  Allele1, Allele2, OldAllele: string;

begin
  NGam := 0; OldAllele := '';
  for I := 1 to NGtypes do begin
    // Get the 2 alleles
    P := Pos('/', Gtypes[I]);
    Allele1 := LeftStr(Gtypes[I], P - 1);
    Allele2 := Copy(Gtypes[I], P + 1, Length(Gtypes[I]));
    // Get the gametes
    if Allele1 <> OldAllele then begin                                                   // genotypes are supposed ot be ordered a/a, a/b, a/c..., b/b, b/c...
      if Allele2 <> '-' then begin                                                       // normal case
        Inc(NGam);
        Gametes[NGam] := Allele1;
        OldAllele := Allele1;
      end
      else begin                                                                         // a/- genotype (males, X-linked trait)
        NGam := 1; Gametes[1] := '-';
      end;
    end;
  end;
end;

{ Display Hardy-Weinberg equilibrium frequencies }

procedure WriteFrequencies(SexLink: Char; var Genotypes: TGenotypes; var HWFreq: TGenotypeNumbers; NGenotypes: Integer; Sex: Char);

var
  I: Integer;

begin
  if SexLink = 'X' then begin
    if Sex = 'M' then
      Writeln('Equilibrium frequencies (males):')
    else
      Writeln('Equilibrium frequencies (females):')
  end
  else
    Writeln('Equilibrium freqencies:');
  Writeln;
  for I := 1 to NGenotypes do
    Writeln('  Genotype ', Genotypes[I], '  ',  HWFreq[I]:0:5);
end;

{ Draw a Punnett Square showing the Hardy-Weinberg equilibrium frequencies }

procedure WritePunnettSquare(SexLink: Char; var AlleleFreq: TAlleleNumbers;
  var GametesF, GametesM: TGametes; NGametesF, NGametesM: Integer; Sex: Char);

var
  I, J: Integer;

begin
  if SexLink = 'X' then begin
    if Sex = 'M' then
      Writeln('Equilibrium frequencies (males):')
    else
      Writeln('Equilibrium frequencies (females):')
  end
  else
    Writeln('Equilibrium freqencies:');
  Writeln;
  LineLength := 7 + NGametesF * 12 - 2;
  Write('  ');
  for I := 1 to LineLength do
    Write('-');
  Writeln;
  Write('  |   |');
  for I := 1 to NGametesF do begin
    Write('     ', GametesF[I], '     |');
  end;
  Writeln;
  Write('  ');
  for I := 1 to LineLength do
    Write('-');
  Writeln;
  for I := 1 to NGametesM do begin
    Write('  | ', GametesM[I], ' |');
    if Sex = 'M' then begin
      for J := 1 to NGametesF do
        Write('    ', GametesF[J], '/', GametesM[I], '    |');
    end
    else begin
      for J := 1 to NGametesF do begin
        if J < I then
          Write('    ', GametesM[J], '/', GametesF[I], '    |')
        else
          Write('    ', GametesF[I], '/', GametesM[J], '    |');
      end;
    end;
    Writeln; Write('  |   |');
    for J := 1 to NGametesF do begin
      if Sex = 'M' then
        Freq := AlleleFreq[J]
      else
        Freq := AlleleFreq[I] * AlleleFreq[J];
      Write(' ', Freq:8:5, '  |')
    end;
    Writeln;
    Write('  ');
    for J := 1 to LineLength do
      Write('-');
    Writeln;
  end;
end;

{****************}
{* Main Program *}
{****************}

begin
  repeat
    WriteTitle('The Hardy-Weinberg Law.');
    Writeln('Calculating the Hardy-Weinberg equilibrium frequencies for a given trait,');
    Writeln('based on the count of the different genotypes observed in a given population.');
    Writeln('For autosomal traits, the program asks for the number of each genotype, for');
    Writeln('X-linked traits, it asks for the male genotypes, the female ones being deduced');
    Writeln('from these numbers.');
    Writeln;
    ReadStudyData(Species, Trait, Notes, NAlleles, SexLink);
    // Get alleles from user input
    ReadAlleles(NAlleles, Alleles);
    // Determine corresponding genotypes
    GetGenotypes(Alleles, NAlleles, SexLink, GenotypesMale, GenotypesFemale, NGenotypesMale, NGenotypesFemale);
    WriteTitle('The Hardy-Weinberg Law.');
    // Get genotype numbers from user input and compute frequencies
    ReadPopulationSize(Population, Percentage);
    ReadGenotypeNumbers(GenotypesMale, NGenotypesMale, SexLink, GenotypeCounts);
    GetGenotypeFrequencies(GenotypeCounts, NGenotypesMale, Population, GenotypeFrequencies);
    // Calculate allele frequencies
    GetAlleleFrequencies(Alleles, NAlleles, GenotypesMale, GenotypeFrequencies, NGenotypesMale, AlleleFrequencies);
    // Calculate Hardy-Weinberg equilibrium frequencies
    GetHardyWeinBergFrequencies(Alleles, AlleleFrequencies, NAlleles, SexLink, 'M', HWFrequenciesMale);
    GetHardyWeinBergFrequencies(Alleles, AlleleFrequencies, NAlleles, SexLink, 'F', HWFrequenciesFemale);
    // Display frequencies
    S := Species + ', ' + Trait;
    if Notes <> '' then
      S += ' (' + Notes + ')';
    S += '.';
    WriteTitle(S);
    if not Percentage then begin
      Writeln('Studied population size = ', IntToStr(Population), '.');
      Writeln;
    end;
    Write('Allele frequencies: ');
    for I := 1 to NAlleles do begin
      Write(Alleles[I], ': ', AlleleFrequencies[I]:0:5);
      if I < NAlleles then
        Write(', ')
      else
        Writeln('.');
    end;
    Writeln;
    WriteFrequencies(SexLink, GenotypesFemale, HWFrequenciesFemale, NGenotypesFemale, 'F');
    if SexLink = 'X' then begin
      Writeln;
      WriteFrequencies(SexLink, GenotypesMale, HWFrequenciesMale, NGenotypesMale, 'M');
    end;
    Writeln; Writeln;
    Write('Hit ENTER to draw the Punnett Square');
    // Display Punnett Square
    if SexLink = 'X' then
      Write('s');
    Readln;
    // Determine male and female gametes (for Punnett Square drawing)
    GetGametes(GenotypesFemale, NGenotypesFemale, GametesMaleX, NGametesMaleX);          // male X-chromosome
    GetGametes(GenotypesMale, NGenotypesMale, GametesMaleY, NGametesMaleY);              // male Y-chromosome
    GetGametes(GenotypesFemale, NGenotypesFemale, GametesFemale, NGametesFemale);        // female
    S := Species + ', ' + Trait;
    if Notes <> '' then
      S += ' (' + Notes + ')';
    S += '.';
    WriteTitle(S);
    // Display global or (if trait is X-linked) "females" Punnett Square
    WritePunnettSquare(SexLink, AlleleFrequencies, GametesFemale, GametesMaleX, NGametesFemale, NGametesMaleX, 'F');
    // If trait is X-linked, also draw "males" Punnett Square
    if SexLink = 'X' then begin
      Writeln;
      WritePunnettSquare(SexLink, AlleleFrequencies, GametesFemale, GametesMaleY, NGametesFemale, NGametesMaleY, 'M');
    end;
    Writeln; Writeln;
    // Terminate the program with ESC key, or restart
    Write('Hit ESC to exit, any other key to restart... ');
    Key := ReadKey;
    if Key = #00 then
      Key := ReadKey;
  until Key = #27;
end.

