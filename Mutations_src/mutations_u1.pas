{***************************************}
{* Main unit for Mutations application *}
{***************************************}

unit mutations_u1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, ExtCtrls, mutations_u2, mutations_u3;

type
  {*************}
  { TfMutations }
  {*************}
  TfMutations = class(TForm)
    mMenu: TMainMenu;
    mMutation, mMutationAnalysis, mMutationExercise, mMutationExit: TMenuItem;
    mSettings, mSettingsBasesLC, mSettingsAA3, mSettingsExercise: TMenuItem;
    mSettingsExerciseQuestions, mSettingsExerciseSeqLength, MenuItem1, mSettingsExerciseTransEval: TMenuItem;
    mHelp, mHelpGCode, mHelpGenetics, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    edDNA0: TMemo;
    edDNA1: TMemo;
    edProtein0: TMemo;
    edProtein1: TMemo;
    edPosition: TEdit;
    cobType: TComboBox;
    cobEffect: TComboBox;
    imPosition, imType, imEffect, imProtein0, imProtein1: TImage;
    laEval: TLabel;
    sgEval: TStringGrid;
    btAction: TButton;
    btClear: TButton;
    btClean: TButton;
    btCopyDNA: TButton;
    btCopyProtein: TButton;
    procedure FormCreate(Sender: TObject);
    procedure mMutationAnalysisClick(Sender: TObject);
    procedure mMutationExerciseClick(Sender: TObject);
    procedure mMutationExitClick(Sender: TObject);
    procedure mSettingsBasesLCClick(Sender: TObject);
    procedure mSettingsAA3Click(Sender: TObject);
    procedure mSettingsExerciseQuestionsClick(Sender: TObject);
    procedure mSettingsExerciseSeqLengthClick(Sender: TObject);
    procedure mSettingsExerciseTransEvalClick(Sender: TObject);
    procedure mHelpGCodeClick(Sender: TObject);
    procedure mHelpGeneticsClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btActionClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btCleanClick(Sender: TObject);
    procedure btCopyDNAClick(Sender: TObject);
    procedure btCopyProteinClick(Sender: TObject);
  private
    iQuestions, iQuestionsTemp, iLength, iQuestion, iCorrect, iMutationPos: Integer;
    sAction, sMutationType, sMutationEffect: string;
    sDNA0, sDNA1, sProtein0, sProtein1: AnsiString;
  end;

const
  Bases: array[0..3] of Char = (
    'A', 'C', 'G', 'T'
  );
  StopCodons: array[0..2] of string = (
    'TAA', 'TAG', 'TGA'
  );
  MutationTypes: array[0..3] of string = (
    'transition', 'transversion', 'insertion', 'deletion'
  );
  SubstitutionEffects: array[0..3] of string = (
    'silent mutation', 'missense mutation', 'nonsense mutation', 'protein elongation'
  );
  InDelEffects: array[0..2] of string = (
    'frameshift mutation', 'nonsense mutation', 'protein elongation'
  );

var
  fMutations: TfMutations;

implementation

{$R *.lfm}

{ Clear form fields }

procedure ClearForm(ClearDNA: Boolean);

begin
  if ClearDNA then begin
    fMutations.edDNA0.Clear; fMutations.edDNA1.Clear;
  end;
  fMutations.edProtein0.Clear;
  fMutations.edProtein1.Clear;
  fMutations.edPosition.Clear;
  fMutations.cobType.ItemIndex := -1; fMutations.cobEffect.ItemIndex := -1;    // "blank item" in comboboxes
  fMutations.imPosition.Picture.Clear; fMutations.imType.Picture.Clear; fMutations.imEffect.Picture.Clear;
  fMutations.imProtein0.Picture.Clear; fMutations.imProtein1.Picture.Clear;
end;

{ Reset form fields (depending on analyse or exercise) }

procedure ResetForm(Action: string; QuestionsTemp: Integer; out Questions, Question, Correct: Integer);

var
  I: Integer;

begin
  ClearForm(True);
  if Action = 'analyse' then begin
    fMutations.edDNA0.ReadOnly := False; fMutations.edDNA1.ReadOnly := False;
    fMutations.edDNA0.TabStop := True; fMutations.edDNA1.TabStop := True;
    fMutations.edProtein0.ReadOnly := True; fMutations.edProtein1.ReadOnly := True;
    fMutations.edProtein0.TabStop := False; fMutations.edProtein1.TabStop := False;
    fMutations.edPosition.ReadOnly := True; fMutations.edPosition.TabStop := False;
    fMutations.cobType.TabStop := False; fMutations.cobEffect.TabStop := False;
    fMutations.laEval.Visible := False; fMutations.sgEval.Visible := False;
    fMutations.btAction.Caption := 'Analyse';
  end
  else begin
    fMutations.edDNA0.ReadOnly := True; fMutations.edDNA1.ReadOnly := True;
    fMutations.edDNA0.TabStop := False; fMutations.edDNA1.TabStop := False;
    fMutations.edProtein0.ReadOnly := True; fMutations.edProtein1.ReadOnly := True;
    fMutations.edProtein0.TabStop := False; fMutations.edProtein1.TabStop := False;
    fMutations.edPosition.ReadOnly := False; fMutations.edPosition.TabStop := True;
    fMutations.cobType.TabStop := True; fMutations.cobEffect.TabStop := True;
    fMutations.laEval.Visible := True; fMutations.sgEval.Visible := True;
    fMutations.btAction.Caption := 'Question';
    fMutations.btAction.SetFocus;
    for I := 0 to 3 do
      fMutations.sgEval.Cells[1, I] := '';
  end;
  Questions := QuestionsTemp;
  Question := 0; Correct := 0;
  fMutations.btAction.Enabled := True;
end;

{ Right-align stringgrid values }

function GridFormat(N: Integer; S: string): string;

var
  SN: string;

begin
  SN := ' ' + IntToStr(N);
  if N < 10 then
    SN := '  ' + SN
  else if N < 100 then
    SN := ' ' + SN;
  if S = '' then
    SN := ' ' + SN
  else
    SN += S;                                                                   // adding the '%' sign
  Result := SN;
end;

{ Check if DNA sequence is valid }

function IsValidDNA(DNA: AnsiString): Boolean;

// Base codes must be uppercase!

var
  I: Integer;
  IsValid: Boolean;

begin
  if DNA = '' then
    IsValid := False
  else begin
    IsValid := True;
    I := 1;
    while IsValid and (I <= Length(DNA)) do begin
      if not (DNA[I] in ['A', 'C', 'G', 'T']) then
        IsValid := False;
      Inc(I);
    end;
  end;
  Result := IsValid;
end;

{ Check if a codon is a stop codon }

function IsStopCodon(Codon: string): Boolean;

// Codon base codes must be uppercase!

var
  IsStop: Boolean;

begin
  Codon := StringReplace(Codon, 'T', 'U', [rfReplaceAll]);                     // genetic code table uses RNA codons
  IsStop := False;
  if (Codon = 'UAA') or (Codon = 'UAG') or (Codon = 'UGA') then
    IsStop := True;
  Result := IsStop;
end;

{ Check if a base is a purine }

function IsPurine(Base: Char): Boolean;

// Base code must be uppercase!

var
  Purine: Boolean;

begin
  Purine := False;
  if Base in ['A', 'G'] then
    Purine := True;
  Result := Purine;
end;

{ Check if a base is a pyrimidine }

function IsPyrimidine(Base: Char): Boolean;

// Base code must be uppercase!

var
  Pyrimidine: Boolean;

begin
  Pyrimidine := False;
  if Base in ['C', 'T'] then
    Pyrimidine := True;
  Result := Pyrimidine;
end;

{ Translate (DNA) codon to 1-letter amino acid }

function CodonTranslate(Codon: string): Char;

// Codon base codes must be uppercase!

begin
  Codon := StringReplace(Codon, 'T', 'U', [rfReplaceAll]);                     // genetic code table uses RNA codons
  Result := GetGeneticCode(Codon);
end;

{ Translate DNA sequence to 1-letter amino acids protein sequence }

function DNATranslate(DNA: AnsiString): AnsiString;

// Sequence is supposed to be valid DNA (in uppercase!)

var
  I: Integer;
  Codon: string;
  Protein: AnsiString;
  IsStop: Boolean;

begin
  Protein := '';
  I := 1; IsStop := False;
  repeat
    Codon := Copy(DNA, I, 3);
    if IsStopCodon(Codon) then
      IsStop := True                                                           // translation is ended when codon is a stop codon
    else
      Protein += CodonTranslate(Codon);
    I += 3;
  until IsStop or (I + 2 > Length(DNA));
  Result := Protein;
end;

{ Generate random DNA sequence (not containing any stop codon) }

function RandomSequence(L: Integer): AnsiString;

var
  I: Integer;
  Codon: string;
  DNA: AnsiString;
  B1, B2, B3: Char;

begin
  L := (L div 3) * 3;                                                          // sequence with all complete codons
  I := 0; DNA := '';
  while I < L do begin
    repeat
      B1 := Bases[Random(4)]; B2 := Bases[Random(4)]; B3 := Bases[Random(4)];
      Codon := B1 + B2 + B3;
    until not IsStopCodon(Codon);                                              // sequence must not contain a stop codon
    DNA += Codon;
    I += 3;
  end;
  Result := DNA;
end;

{ Do point mutation: transition or transversion at given (codon) position and with given mutation effect }

procedure DoSubstitution(var DNA0: AnsiString; out DNA1: AnsiString; CodonPos: Integer; MutationType, MutationEffect: string; out MutationPos: Integer);

// The original DNA sequence may be altered by this routine (replacement of a coding codon by a stop codon)

var
  BasePos, Loop: Integer;
  StopCodon, Codon: string;
  Base: Char;
  OK: Boolean;

begin
  Loop := 0;                                                                   // counter to avoid a possible infinite loop with given parameters
  DNA1 := DNA0;
  if (MutationEffect = 'nonsense mutation') or (MutationEffect = 'protein elongation') then begin
    // Replacement of the actual codon will be necessary
    repeat
      Inc(Loop);
      OK := True;
      StopCodon := StopCodons[Random(3)];                                       // random stop codon
      BasePos := Random(3) + 1;                                                 // random base position within the stop codon
      repeat
        Base := Bases[Random(4)];                                               // substitution base
      until Base <> StopCodon[BasePos];
      // The substitution type (transition or tranversion) is given
      if MutationType = 'transition' then begin
        if (IsPurine(StopCodon[BasePos]) and IsPyrimidine(Base)) or (IsPyrimidine(StopCodon[BasePos]) and IsPurine(Base)) then
          OK := False
      end
      else if MutationType = 'transversion' then begin
        if (IsPurine(StopCodon[BasePos]) and IsPurine(Base)) or (IsPyrimidine(StopCodon[BasePos]) and IsPyrimidine(Base)) then
          OK := False
      end;
      if OK then begin
        Codon := StopCodon; Codon[BasePos] := Base;                            // the stop codon with substitution done
        if IsStopCodon(Codon) then
          OK := False;                                                         // the modified stop codon must be a coding codon
      end;
    until OK or (Loop > 250);
    if OK then begin
      // Replace actual codons in both DNA sequences by new stop resp. coding codon
      if MutationEffect = 'nonsense mutation' then begin
        // Nonsense mutation: stop codon has to be in mutated DNA
        Delete(DNA1, CodonPos, 3); Insert(StopCodon, DNA1, CodonPos);
        Delete(DNA0, CodonPos, 3); Insert(Codon, DNA0, CodonPos);
      end
      else begin
        // Protein elongation: stop codon has to be in original DNA
        Delete(DNA0, CodonPos, 3); Insert(StopCodon, DNA0, CodonPos);
        Delete(DNA1, CodonPos, 3); Insert(Codon, DNA1, CodonPos);
      end;
      MutationPos := CodonPos + BasePos - 1;                                   // effective mutation position
    end;
  end
  else begin
    // Actual codon doesn't need to be changed (original DNA may be used as given)
    repeat
      Inc(Loop);
      OK := True;
      MutationPos := CodonPos + Random(3);                                     // effective mutation position
      repeat
        Base := Bases[Random(4)];                                              // substitution base
      until Base <> DNA0[MutationPos];
      // The substitution type (transition or tranversion) is given
      if MutationType = 'transition' then begin
        if (IsPurine(DNA0[MutationPos]) and IsPyrimidine(Base)) or (IsPyrimidine(DNA0[MutationPos]) and IsPurine(Base)) then
          OK := False
      end
      else if MutationType = 'transversion' then begin
        if (IsPurine(DNA0[MutationPos]) and IsPurine(Base)) or (IsPyrimidine(DNA0[MutationPos]) and IsPyrimidine(Base)) then
          OK := False
      end;
      if OK then begin
        DNA1[MutationPos] := Base;                                             // do the base substitution
        if IsStopCodon(Copy(DNA1, CodonPos, 3)) then
          OK := False;                                                         // mutated codon must be a coding codon
        if OK Then begin
          if MutationEffect = 'silent mutation' then begin
            // For a silent mutation, the amino acid coded by the mutated codon must not be changed
            if CodonTranslate(Copy(DNA0, CodonPos, 3)) <> CodonTranslate(Copy(DNA1, CodonPos, 3)) then
              OK := False;
          end
          else if MutationEffect = 'missense mutation' then begin
            // For a non silent mutation, the amino acid coded by the mutated codon must be changed
            if CodonTranslate(Copy(DNA0, CodonPos, 3)) = CodonTranslate(Copy(DNA1, CodonPos, 3)) then
              OK := False;
          end
        end;
      end;
    until OK or (Loop > 250);
  end;
  // If it was not possible to do the mutation, return a mutation position of -1
  // The routine will have to be called again (with different parameters)
  if Loop > 250 then
    MutationPos := -1;
end;

{ Do point mutation: 1-base insertion at given (codon) position and with given mutation effect }

procedure DoInsertion(var DNA0: AnsiString; out DNA1: AnsiString; CodonPos: Integer; MutationEffect: string; out MutationPos: Integer);

// The original DNA sequence may be altered by this routine (replacement of a coding codon by a stop codon)

var
  BasePos, Loop: Integer;
  StopCodon, Codon, DNAStop, DNANonStop: string;
  Base: Char;
  OK: Boolean;

begin
  Loop := 0;                                                                   // counter to avoid a possible infinite loop with given parameters
  DNA1 := DNA0;
  if (MutationEffect = 'nonsense mutation') or (MutationEffect = 'protein elongation') then begin
    // Replacement of the actual codon will be necessary
    repeat
      Inc(Loop);
      OK := True;
      StopCodon := StopCodons[Random(3)];                                      // random stop codon
      Base := Bases[Random(4)];                                                // random base
      DNAStop := StopCodon + Base;                                             // mutated DNA: 4 bases starting with stop codon, the first one being the inserted base
      Codon := RightStr(DNAStop, 3);                                           // corresponding codon on original DNA
      if IsStopCodon(Codon) then
        OK := False                                                            // this codon must be a coding codon
      else begin
        DNANonStop := Base + StopCodon;                                        // mutated DNA: 4 bases starting with the inserted base
        if IsStopCodon(LeftStr(DNANonStop, 3)) then
          OK := False;                                                         // the new codon must be a coding codon
      end;
    until OK or (Loop > 250);
    if OK then begin
      if MutationEffect = 'nonsense mutation' then begin
        // Nonsense mutation: stop codon is part of the 4 new bases in mutated DNA
        Delete(DNA0, CodonPos, 3); Insert(Codon, DNA0, CodonPos);
        Delete(DNA1, CodonPos, 3); Insert(DNAStop, DNA1, CodonPos);
      end
      else begin
        // Protein elongation: stop codon is part of the original DNA
        Delete(DNA0, CodonPos, 3); Insert(StopCodon, DNA0, CodonPos);
        Delete(DNA1, CodonPos, 3); Insert(DNANonStop, DNA1, CodonPos);
      end;
      MutationPos := CodonPos;                                                 // effective mutation position
    end;
  end
  else begin
    // Actual codon doesn't need to be changed (original DNA may be used as given)
    DNANonStop := Copy(DNA1, CodonPos, 3);                                     // actual codon where insertion has to be done in mutated DNA
    repeat
      Inc(Loop);
      OK := True;
      repeat
        Base := Bases[Random(4)];                                              // random insertion base
        BasePos := Random(4) + 1;                                              // random mutation position (within or after the codon)
      until (BasePos = 4) or (Base <> DNANonStop[BasePos]);
      Insert(Base, DNANonStop, BasePos);                                       // insert the new base into mutated DNA
      if IsStopCodon(LeftStr(DNANonStop, 3)) then                              // the first 3 bases of the new 4-base sequence must be a coding codon
        OK := False;
    until OK or (Loop > 250);
    if OK then begin
      // Mutated DNA with new 4-base sequence replacing the actual codon (as is in original DNA)
      Delete(DNA1, CodonPos, 3); Insert(DNANonStop, DNA1, CodonPos);
      MutationPos := CodonPos + BasePos - 1;                                   // effective mutation position
    end;
  end;
  // If it was not possible to do the mutation, return a mutation position of -1
  // The routine will have to be called again (with different parameters)
  if Loop > 250 then
    MutationPos := -1;
end;

{ Do point mutation: 1-base deletion at given (codon) position and with given mutation effect }

procedure DoDeletion(var DNA0: AnsiString; out DNA1: AnsiString; CodonPos: Integer; MutationEffect: string; out MutationPos: Integer);

// The original DNA sequence may be altered by this routine (replacement of a coding codon by a stop codon)

var
  BasePos, BasePos2, Loop: Integer;
  StopCodon, Codon, DNAStop0, DNAStop1, DNANonStop0, DNANonStop1: string;
  Base: Char;
  OK: Boolean;

begin
  Loop := 0;                                                                   // counter to avoid a possible infinite loop with given parameters
  DNA1 := DNA0;
  if (MutationEffect = 'nonsense mutation') or (MutationEffect = 'protein elongation') then begin
    // Replacement of the actual codon will be necessary
    repeat
      Inc(Loop);
      OK := True;
      StopCodon := StopCodons[Random(3)];                                      // random stop codon
      DNAStop0 := StopCodon + DNA0[CodonPos + 4];                              // original DNA: 4 bases starting with stop codon
      DNANonStop1 := DNAStop0;
      BasePos := Random(3) + 1;                                                // random deletion position (within the stop codon)
      Delete(DNANonStop1, BasePos, 1);                                         // 3 bases left as correspondance in mutated DNA
      if IsStopCodon(DNANonStop1) then
        OK := False                                                            // these 3 bases must form a coding codon
      else begin
        BasePos2 := Random(3) + 1;
        repeat
          Base := Bases[Random(4)];
        until Base <> StopCodon[BasePos2];
        Codon := StopCodon; Codon[BasePos2] := Base;                           // codon (1 base modification in stop codon)
        if IsStopCodon(Codon) then
          OK := False                                                          // this must be a coding codon
        else begin
          DNANonStop0 := Codon + DNA0[CodonPos + 4];                           // original DNA: 4 bases starting with coding codon
          DNAStop1 := DNANonStop0;
          Delete(DNAStop1, BasePos, 1);                                        // 3 bases left after deletion done as correspondance in mutated DNA
          if not IsStopCodon(DNAStop1) then
            OK := False;                                                       // these 3 bases must form a stop codon
        end;
      end;
    until OK or (Loop > 250);
    if OK then begin
      if MutationEffect = 'nonsense mutation' then begin
        // Nonsense mutation: stop codon is part of mutated DNA
        Delete(DNA0, CodonPos, 4); Insert(DNANonStop0, DNA0, CodonPos);
        Delete(DNA1, CodonPos, 4); Insert(DNAStop1, DNA1, CodonPos);
      end
      else begin
          // Protein elongation: stop codon, formed by the 3 first of the new 4-base sequence, is part of original DNA
          Delete(DNA0, CodonPos, 4); Insert(DNAStop0, DNA0, CodonPos);
          Delete(DNA1, CodonPos, 4); Insert(DNANonStop1, DNA1, CodonPos);
      end;
      MutationPos := CodonPos + BasePos - 1;                                   // effective mutation position
    end;
  end
  else begin
    // Actual codon doesn't need to be changed (original DNA may be used as given)
    DNANonStop0 := Copy(DNA0, CodonPos, 3);
    repeat
      Inc(Loop);
      OK := True;
      BasePos := Random(3) + 1;                                                // random deletion position (within actual codon)
      DNANonStop1 := DNANonStop0;
      Delete(DNANonStop1, BasePos, 1);                                         // 2 bases left in mutated DNA
      if IsStopCodon(DNANonStop1 + Copy(DNA1, CodonPos + 4, 1)) then
        OK := False;                                                           // these 2 bases + the one following the original codon must form a coding codon
    until OK or (Loop > 250);
    if OK then begin
      // Mutated DNA: new 2-base sequence replacing the original codon (as is in original DNA)
      Delete(DNA1, CodonPos, 3); Insert(DNANonStop1, DNA1, CodonPos);
      MutationPos := CodonPos + BasePos - 1;                                   // effective mutation position
    end;
  end;
  // If it was not possible to do the mutation, return a mutation position of -1
  // The routine will have to be called again (with different parameters)
  if Loop > 250 then
    MutationPos := -1;
end;

{ Do point mutation: 1-base substitution, 1-base indel or 3-base insertion at codon position }

procedure DoMutation(var DNA0: AnsiString; out DNA1: AnsiString; MutationType, MutationEffect: string; out MutationPos: Integer);

var
  CodonPos, MaxPos: Integer;
  Codon: string;
  Base: Char;

begin
  if MutationType = 'insert' then
    MaxPos := Length(DNA0) + 1                                                 // for an insert, the base may be added at the end of the sequence
  else
    MaxPos := Length(DNA0);
  repeat
    CodonPos := Random(MaxPos) + 1;                                            // position of the codon that will be mutated
  until CodonPos mod 3 = 1;
  if MutationEffect = 'amino acid insertion' then begin
    // 3 identical bases insertion at codon position
    Base := Bases[Random(4)];
    Codon := Base + Base + Base;
    Insert(Codon, DNA1, CodonPos);
    MutationPos := CodonPos;                                                   // effective mutation position (always a codon position)
  end
  else begin
    // 1-base point mutations
    if (MutationType = 'transition') or (MutationType = 'transversion') then
      DoSubstitution(DNA0, DNA1, CodonPos, MutationType, MutationEffect, MutationPos)
    else if MutationType = 'insertion' then
      DoInsertion(DNA0, DNA1, CodonPos, MutationEffect, MutationPos)
    else
      DoDeletion(DNA0, DNA1, CodonPos, MutationEffect, MutationPos);
  end;
end;

{ Point mutations analyse routine }

procedure MutationAnalyses(var DNA0, DNA1, Protein0, Protein1: AnsiString; out MutationPos: Integer; out MutationType, MutationEffect: string);

var
  NBases, L, IX, I: Integer;

begin
  MutationPos := 0; MutationType := ''; MutationEffect := '';
  if Length(DNA0) > Length(DNA1) then
    L := Length(DNA1)
  else
    L := Length(DNA0);
  if (Copy(DNA0, 1, L) = Copy(DNA1, 1, L)) then begin
    // If the (assumed different) sequences are identical for all bases up to the end of the
    // smallest of the 2 sequences, the mutation must be beyond the last base of this sequence
    IX := L + 1;
  end
  else begin
    // In all other cases, consider the firt non-matching base as the mutation position
    I := 1; IX := 0;
    while (IX = 0) and (I <= L) do begin
      if DNA0[I] <> DNA1[I] then
        IX := I;
      Inc(I);
    end;
  end;
  MutationPos := IX;
  if MutationPos > 0 then begin
    if Length(DNA0) = Length(DNA1) then begin
      // If the 2 DNA sequences are the same length, the mutation must be a substitution
      MutationType := 'substitution';
      // Determine the substitution type (transition or transversion)
      if (IsPurine(DNA0[IX]) and IsPurine(DNA1[IX])) or (IsPyrimidine(DNA0[IX]) and IsPyrimidine(DNA0[IX])) then
        MutationType := 'transition'
      else
        MutationType := 'transversion';
      if Protein1 = Protein0 then
        // If the 2 proteins are identical, the mutation is silent
        MutationEffect := 'silent mutation'
      else begin
        // For non identical proteins, comparison of their length may be used to determine the mutation effect
        if Length(Protein0) = Length(Protein1) then
          MutationEffect := 'missense mutation'
        else if Length(Protein0) > Length(Protein1) then
          MutationEffect := 'nonsense mutation'
        else
          MutationEffect := 'protein elongation';
      end;
    end
    else begin
      // If the 2 DNA sequences are the different length, the mutation must be an indel mutation:
      // Original DNA longer than mutated DNA => deletion, the contrary => insertion
      if Length(DNA0) > Length(DNA1) then
        MutationType := 'deletion'
      else
        MutationType := 'insertion';
      if Length(DNA1) - Length(DNA0) = 3 then
        // 3 bases difference in length: codon insertion (validation of codon position assumed be done)
        MutationEffect := 'amino acid insertion'
      else begin
        // Other indel mutations
        if MutationType = 'insertion' then begin
          // 1 or 2 base insertion: If the length of the 2 proteins are different, there must be a mutation in a stop codon
          // (-> protein elongation), or a mutation resulting in a stop codon (-> protin shrinkage)
          NBases := Length(DNA1) - Length(DNA0);
          if Length(Protein0) > Length(Protein1) then
            MutationEffect := 'nonsense mutation'
          else if Length(Protein0) < Length(Protein1) then
            MutationEffect := 'protein elongation'
          else if (Copy(DNA0, 1, MutationPos - 1) = Copy(DNA1, 1, MutationPos - 1)) and ((MutationPos = Length(DNA1)) or (Copy(DNA0, MutationPos, Length(DNA0)) = Copy(DNA1, MutationPos + NBases, Length(DNA1)))) then
              MutationEffect := 'frameshift mutation';                         // this should always be the case if the 2 protein lengths are equal
        end
        else begin
          // 1 base deletion: Situation similar to the one above, but having to consider that with a base deleted,
          // the last codon of the mutated DNA will be incomplete and thus in the case, where no stop codon is involved,
          // result in a protein with 1 amino acid less than the original sequence
          if Length(Protein0) - Length(Protein1) > 1 then
            MutationEffect := 'nonsense mutation'
          else if Length(Protein0) < Length(Protein1) then
            MutationEffect := 'protein elongation'
          else if (Copy(DNA0, 1, MutationPos - 1) = Copy(DNA1, 1, MutationPos - 1)) and ((MutationPos = Length(DNA0)) or (Copy(DNA0, MutationPos + 1, Length(DNA0)) = Copy(DNA1, MutationPos, Length(DNA1)))) then
            MutationEffect := 'frameshift mutation'
          else
            MutationEffect := 'nonsense mutation';
        end;
      end;
    end;
  end;
end;

{ Insert tags to mark the mutation position, type and/or effect }

procedure MarkMutation(MutationType, MutationEffect: string; MutationPos: Integer; var DNA0, DNA1, Protein0, Protein1: AnsiString);

begin
  if MutationType = 'insertion' then begin
    // Mark the inserted base(s) in the mutated sequence
    Insert('{', DNA1, MutationPos);
    Insert('}', DNA1, MutationPos + Length(DNA1) - Length(DNA0));
  end
  else if MutationType = 'deletion' then begin
    // Mark the deleted base in the original sequence
    Insert('{', DNA0, MutationPos);
    Insert('}', DNA0, MutationPos + 2);
  end
  else begin
    // Mark the substituted base in both sequences
    Insert('{', DNA0, MutationPos);
    Insert('{', DNA1, MutationPos);
    Insert('}', DNA0, MutationPos + 2);
    Insert('}', DNA1, MutationPos + 2);
  end;
  if MutationEffect = 'missense mutation' then begin
    // Mark the old/new amino acid in both sequences
    Insert('{', Protein0, MutationPos div 3 + 1);
    Insert('}', Protein0, MutationPos div 3 + 3);
    Insert('{', Protein1, MutationPos div 3 + 1);
    Insert('}', Protein1, MutationPos div 3 + 3);
  end
  else if MutationEffect = 'nonsense mutation' then begin
    // Mark the "dropped" amino acids in the original sequence
    Insert('{', Protein0, Length(Protein1) + 1);
    Insert('}', Protein0, Length(Protein0) + 1);
  end
  else if MutationEffect = 'protein elongation' then begin
    // Mark the "supplementary" amino acids in the mutated sequence
    Insert('{', Protein1, Length(Protein0) + 1);
    Insert('}', Protein1, Length(Protein1) + 1);
  end
  else if MutationEffect = 'amino acid insertion' then begin
    // Mark the inserted amino acid in the mutated sequence
    Insert('{', Protein1, MutationPos div 3 + 1);
    Insert('}', Protein1, MutationPos div 3 + 3);
  end
  else if MutationEffect = 'frameshift mutation' then begin
    // Mark the beginning of the frameshift (first amino acid resulting from a different frame reading)
    Insert('[', Protein1, (MutationPos - 1) div 3 + 1);
  end;
end;

{*************}
{ TfMutations }
{*************}

{ Application start: Initialisation }

procedure TfMutations.FormCreate(Sender: TObject);

begin
  iQuestionsTemp := 10; iLength := 60;
  Randomize;
  mMutationAnalysis.Click;                                                     // prepare for new mutation analysis
end;

{ Menu item "Mutation > New analysis": Prepare for new mutation analysis }

procedure TfMutations.mMutationAnalysisClick(Sender: TObject);

begin
  sAction := 'analyse';
  ResetForm(sAction, iQuestionsTemp, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Mutation > New exercise": Prepare for new mutation exercise }

procedure TfMutations.mMutationExerciseClick(Sender: TObject);

begin
  sAction := 'exercise';
  ResetForm(sAction, iQuestionsTemp, iQuestions, iQuestion, iCorrect);
end;

{ Menu item "Mutation > Exit": Exit application }

procedure TfMutations.mMutationExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Lowercase base codes": Toggle allowance/usage of lowecase base codes }

procedure TfMutations.mSettingsBasesLCClick(Sender: TObject);

begin
  if mSettingsBasesLC.Checked then
    mSettingsBasesLC.Checked := False
  else
    mSettingsBasesLC.Checked := True;
end;

{ Menu item "Settings > 3-letters amino acid codes": Toggle usage of 1-letter or 3-letters amino acid codes }

procedure TfMutations.mSettingsAA3Click(Sender: TObject);

// This feature isn't actually available (menu item disabled)

begin
  if mSettingsAA3.Checked then
    mSettingsAA3.Checked := False
  else
    mSettingsAA3.Checked := True;
end;

{ Menu item "Settings > Exercise settings > Exercise questions...": User entry of number of exercise questions }

procedure TfMutations.mSettingsExerciseQuestionsClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Biology test', 'Number of questions (at leat 10)', IntToStr(iQuestions));
  if S <> '' then
    iQuestionsTemp := StrToInt(S);
  if iQuestionsTemp < 10 then                                                  // minimum arbitrarily fixed to 10
    iQuestionsTemp := 10;
end;

{ Menu item "Settings > Exercise settings > Sequence length...": User entry of maximum DNA sequence length (for exercises) }

procedure TfMutations.mSettingsExerciseSeqLengthClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Biology test', 'Length of DNA sequence (at least 15 bases)', IntToStr(iLength));
  if S <> '' then begin
    iLength := StrToInt(S);
    if iLength < 15 then                                                       // minimum arbitrarily fixed to 15 bases
      iLength := 15;
  end;
end;

{ Menu item "Settings > Exercise settings > Evaluate translation": Toggle translation evaluation }

procedure TfMutations.mSettingsExerciseTransEvalClick(Sender: TObject);

begin
  if mSettingsExerciseTransEval.Checked then
    mSettingsExerciseTransEval.Checked := False
  else
    mSettingsExerciseTransEval.Checked := True;
end;

{ Menu item "Help > Genetic code": Display standard genetic code table }

procedure TfMutations.mHelpGCodeClick(Sender: TObject);

begin
  if fGenCode.Visible then
    fGenCode.Close
  else
    fGenCode.Show;
end;

{ Menu item "Help > Genetics help": Display genetics (point mutations) help text }

procedure TfMutations.mHelpGeneticsClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help1.txt');
  fHelp.Show;
end;

{ Menu item "Help > Program help": Display program usage help text }

procedure TfMutations.mHelpHelpClick(Sender: TObject);

begin
  fHelp.edHelp.Clear;
  fHelp.edHelp.Lines.LoadFromFile('help2.txt');
  fHelp.Show;
end;

{ Menu item "Help > About": Display program about text }

procedure TfMutations.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Molecular biology: Point mutations.' + LineEnding;
  S += '(Very simple) mutation analysis and exercise generator application.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September-November 2019.';
  MessageDlg('About "Mutations"', S, mtInformation, [mbOK], 0);
end;

{ Button "Analyse/Question/Answer": Analyse mutation resp. generate exercise question resp. check user answer }

procedure TfMutations.btActionClick(Sender: TObject);

var
  MutationPos1, MutationPos2, UMutationPos, L, I: Integer;
  UMutationType, UMutationEffect, IndelBases0, IndelBases, Mess: string;
  IsValidMutation, IsCorrect: Boolean;
  UProtein0, UProtein1, DNA2: AnsiString;

begin
  // Button "Analyse": Get user DNA sequences and analyse point mutation (if there is a "valid" one)
  if btAction.Caption = 'Analyse' then begin
    ClearForm(False);
    // Get original DNA from memo field
    sDNA0 := edDNA0.Text;
    sDNA0 := StringReplace(sDNA0, LineEnding, '', [rfReplaceAll]);             // remove possible lineending characters
    if sDNA0 = '' then
      Mess := 'No DNA sequence specified!'
    else begin
      if mSettingsBasesLC.Checked then begin
        // All routines await the DNA bases in uppercase. Transform if lowecase allowance is selected
        sDNA0 := UpperCase(sDNA0);
      end;
      // Check if sequence is valid and if all codons are complete
      if not IsValidDNA(sDNA0) then
        Mess := 'DNA sequence specified is not valid!'
      else begin
        if Length(sDNA0) mod 3 <> 0 then begin
          Mess := 'DNA sequences with incomplete codons actually not considered by the application! ';
        end;
      end;
    end;
    if Mess = '' then begin
      // Get mutated DNA from memo field
      sDNA1 := edDNA1.Text;
      sDNA1 := StringReplace(sDNA1, LineEnding, '', [rfReplaceAll]);
      if mSettingsBasesLC.Checked then
        sDNA1 := UpperCase(sDNA1);
      if sDNA1 = '' then
        Mess := 'No mutated DNA sequence specified!'
      // Check if sequence is valid and if there actually is a mutation
      else if not IsValidDNA(sDNA1) then
        Mess := 'Mutated DNA sequence specified is not valid!'
      else if sDNA1 = sDNA0 then
        Mess := 'DNA sequences are the same: No mutation specified!'
      else begin
        // Check if the mutation done by the user is one of those supported by the program
        // Note, that some of the calculation routines await that this validation has been done!
        IsValidMutation := True;
        if Length(sDNA0) = Length(sDNA1) then begin
          I := 1;
          while sDNA0[I] = sDNA1[I] do
            Inc(I);
          DNA2 := sDNA1; DNA2[I] := sDNA0[I];
          if DNA2 <> sDNA0 then
            IsValidMutation := False;
        end
        else if Length(sDNA0) - Length(sDNA1) = 1 then begin
          if LeftStr(sDNA0, Length(sDNA1)) = sDNA1 then
            I := Length(sDNA1) + 1
          else begin
            I := 1;
            while sDNA0[I] = sDNA1[I] do
              Inc(I);
          end;
          DNA2 := sDNA0; Delete(DNA2, I, 1);
          if DNA2 <> sDNA1 then
            IsValidMutation := False;
        end
        else if Length(sDNA1) - Length(sDNA0) < 3 then begin
          if LeftStr(sDNA1, Length(sDNA0)) = sDNA0 then
            I := Length(sDNA0) + 1
          else begin
            I := 1;
            while sDNA0[I] = sDNA1[I] do
              Inc(I);
          end;
          DNA2 := sDNA1;
          if Length(sDNA1) - Length(sDNA0) = 1 then
            Delete(DNA2, I, 1)
          else begin
            if DNA2[I + 1] <> DNA2[I] then
              IsValidMutation := False
            else
              Delete(DNA2, I, 2);
          end;
          if IsValidMutation then begin
            if DNA2 <> sDNA0 then
              IsValidMutation := False;
          end;
        end
        else if Length(sDNA1) - Length(sDNA0) = 3 then begin
          if LeftStr(sDNA1, Length(sDNA0)) = sDNA0 then
            I := Length(sDNA0) + 1
          else begin
            I := 1;
            while sDNA0[I] = sDNA1[I] do
              Inc(I);
          end;
          if I mod 3 <> 1 then
            IsValidMutation := False
          else begin
            DNA2 := sDNA1;
            if (DNA2[I + 1] <> DNA2[I]) or (DNA2[I + 2] <> DNA2[I]) then
              IsValidMutation := False
            else
              Delete(DNA2, I, 3);
            if IsValidMutation then begin
              if DNA2 <> sDNA0 then
                IsValidMutation := False;
            end;
          end;
        end
        else
          IsValidMutation := False;
        if  not IsValidMutation then
          Mess := 'This kind of mutation is not considered by the application!';
      end;
    end;
    // With all input data ok, analysis may begin...
    if Mess = '' then begin
      // Translate the DNA sequences to proteins
      sProtein0 := DNATranslate(sDNA0); sProtein1 := DNATranslate(sDNA1);
      // Analyse the mutation: determine mutation position, type and effect
      MutationAnalyses(sDNA0, sDNA1, sProtein0, sProtein1, iMutationPos, sMutationType, sMutationEffect);
      // Display the analysis results
      edPosition.Text := IntToStr(iMutationPos);
      for I := 0 to cobType.Items.Count - 1 do begin
        if cobType.Items[I] = sMutationType then
          cobType.ItemIndex := I;
      end;
      for I := 0 to cobEffect.Items.Count - 1 do begin
        if cobEffect.Items[I] = sMutationEffect then
          cobEffect.ItemIndex := I;
      end;
      // Use tags to mark the mutation position, type and/or effect within the DNA and protein sequences
      MarkMutation(sMutationType, sMutationEffect, iMutationPos, sDNA0, sDNA1, sProtein0, sProtein1);
      if mSettingsBasesLC.Checked then begin
        sDNA0 := LowerCase(sDNA0); sDNA1 := LowerCase(sDNA1);                  // use lowercase base codes if this option is selectes
      end;
      // Redisplay the DNA sequences (with markers); display the protein sequences (also with markers)
      edDNA0.Text := sDNA0; edDNA1.Text := sDNA1;
      edProtein0.Text := sProtein0; edProtein1.Text := sProtein1;
    end
    // Data not adequate to do the analysis: Abort...
    else begin
      MessageDlg('Data error', Mess, mtError, [mbOK], 0);
    end;
  end
  else begin
    // Button "Question": Generate a new exercise (point mutation) question
    if btAction.Caption = 'Question' then begin
      ClearForm(True);
      Inc(iQuestion);
      // Random DNA sequence length (between 15 bases and maximum entered by user)
      repeat
        if iLength < 30 then
          L := Random(iLength - 14) + 15
        else
          L := Random(iLength - (iLength div 2 - 1)) + iLength div 2;
        // Generate random DNA sequence (= original DNA)
        sDNA0 := RandomSequence(L);
        // Get random point mutation type
        sMutationType := MutationTypes[Random(4)];
        // Depending on mutation type, get random mutation effect
        if (sMutationType = 'insertion') or (sMutationType = 'deletion') then begin
          if (sMutationType = 'insertion') and (Random(10) = 0) then
            sMutationEffect := 'amino acid insertion'
          else
            sMutationEffect := InDelEffects[Random(3)];
        end
        else begin
          sMutationEffect := SubstitutionEffects[Random(4)];
        end;
        // Do (or more correctly: try to do) the point mutation with actual sequence and actual parameters
        DoMutation(sDNA0, sDNA1, sMutationType, sMutationEffect, iMutationPos);
      until iMutationPos <> -1;                                                // repeat the loop (with different parameters) until it was possible to do the mutation
      if mSettingsBasesLC.Checked then begin
        sDNA0 := LowerCase(sDNA0); sDNA1 := LowerCase(sDNA1);                  // use lowercase base codes if this option is selected
      end;
      // Display the original and mutated DNA sequences
      edDNA0.Lines.AddText(sDNA0); edDNA1.Lines.AddText(sDNA1);
      // Enable protein entry fields if translation evaluation is selected
      if mSettingsExerciseTransEval.Checked then begin
        edProtein0.ReadOnly := False; edProtein1.ReadOnly := False;
        edProtein0.TabStop := True; edProtein1.TabStop := True;
      end
      else begin
        edProtein0.ReadOnly := True; edProtein1.ReadOnly := True;
        edProtein0.TabStop := False; edProtein1.TabStop := False;
      end;
      btAction.Caption := 'Answer';
    end
    // Button "Answer"; Check user answer
    else begin
      MutationPos1 := iMutationPos; MutationPos2 := iMutationPos;
      if (sMutationType = 'insertion') or (sMutationType = 'deletion') then begin
        // For indel mutations, the actual mutation position may not be determined (e.g. inserting 'A' within
        // or deleting 'A' from a 'AAAAA' sequence). To avoid evaluating a potential correct user answer as false,
        // the program determines all possible valid positions and accepts any of them as correct answer
        if sMutationType = 'insertion' then
          IndelBases0 := Copy(sDNA1, iMutationPos, Length(sDNA1) - Length(sDNA0))
        else
          IndelBases0 := sDNA0[iMutationPos];
        // Check for identical base before actual mutation position
        repeat
          Dec(MutationPos1);
          if sMutationType = 'insertion' then
            IndelBases := Copy(sDNA1, MutationPos1, Length(sDNA1) - Length(sDNA0))
          else
            IndelBases := sDNA0[MutationPos1];
        until (IndelBases <> IndelBases0) or (MutationPos1 = 0);
        // Check for identical base after actual mutation position
        repeat
          Inc(MutationPos2);
          if sMutationType = 'insertion' then
            IndelBases := Copy(sDNA1, MutationPos2, Length(sDNA1) - Length(sDNA0))
          else
            IndelBases := sDNA0[MutationPos2];
        until (IndelBases <> IndelBases0) or (MutationPos2 = Length(sDNA0));
        Inc(MutationPos1); Dec(MutationPos2);
      end;
      // Get user answers
      if edPosition.Text = '' then
        UMutationPos := 0
      else
        UMutationPos := StrToInt(edPosition.Text);
      if cobType.ItemIndex = -1 then
        UMutationType := ''
      else
        UMutationType := cobType.Items[cobType.ItemIndex];
      if cobEffect.ItemIndex = -1 then
        UMutationEffect := ''
      else
        UMutationEffect := cobEffect.Items[cobEffect.ItemIndex];
      // Translate the DNA sequences to proteins
      sProtein0 := DNATranslate(UpperCase(sDNA0)); sProtein1 := DNATranslate(UpperCase(sDNA1));
      // Check user answers (and display a correct resp. false icon for each of them)
      if (UMutationPos >= MutationPos1) and (UMutationPos <= MutationPos2) then  // accept all valid mutation positions as correct
        imPosition.Picture.LoadFromFile('correct.png')
      else begin
        imPosition.Picture.LoadFromFile('false.png');
        IsCorrect := False;
      end;
      if UMutationType = sMutationType then
        imType.Picture.LoadFromFile('correct.png')
      else begin
        imType.Picture.LoadFromFile('false.png');
        IsCorrect := False;
      end;
      if UMutationEffect = sMutationEffect then
        imEffect.Picture.LoadFromFile('correct.png')
      else begin
        imEffect.Picture.LoadFromFile('false.png');
        IsCorrect := False;
      end;
      // If DNA translation is set to be evaluated, do so
      if mSettingsExerciseTransEval.Checked then begin
        UProtein0 := StringReplace(edProtein0.Text, LineEnding, '', [rfReplaceAll]);
        UProtein1 := StringReplace(edProtein1.Text, LineEnding, '', [rfReplaceAll]);
        if UProtein0 = sProtein0 then
          imProtein0.Picture.LoadFromFile('correct.png')
        else begin
          imProtein0.Picture.LoadFromFile('false.png');
          IsCorrect := False;
        end;
        if UProtein1 = sProtein1 then
          imProtein1.Picture.LoadFromFile('correct.png')
        else begin
          imProtein1.Picture.LoadFromFile('false.png');
          IsCorrect := False;
        end;
      end
      else begin
        imProtein0.Picture.Clear; imProtein1.Picture.Clear;
      end;
      // Update evaluation counters
      if IsCorrect then
        Inc(iCorrect);
      sgEval.Cells[1, 0] := GridFormat(iQuestion, '');
      sgEval.Cells[1, 1] := GridFormat(iCorrect, '');
      sgEval.Cells[1, 2] := GridFormat(iQuestion - iCorrect, '');
      sgEval.Cells[1, 3] := GridFormat(Round(100 * (iCorrect / iQuestion)), '%');
      // Mark correct mutation position, type and/or effect within DNA sequences
      MarkMutation(sMutationType, sMutationEffect, iMutationPos, sDNA0, sDNA1, sProtein0, sProtein1);
      edDNA0.Clear; edDNA1.Clear;
      edDNA0.Lines.AddText(sDNA0); edDNA1.Lines.AddText(sDNA1);
      // If DNA translation isn't evaluated, display the protein sequences (with markers)
      if not mSettingsExerciseTransEval.Checked then begin
        edProtein0.Text := sProtein0; edProtein1.Text := sProtein1;
      end;
      btAction.Caption := 'Question';
      // All questions done: end of exercise
      if iQuestion = iQuestions then begin
        MessageDlg('Exercise end', 'All questions have been done. Please, select a new task in the "Mutation" menu.', mtInformation, [mbOK], 0);
        btAction.Enabled := False;                                             // user has to select an action in the "Mutation" menu
      end;
    end;
  end;
end;

{ Button "Clean": Remove all marking tags from DNA and protein sequences }

procedure TfMutations.btCleanClick(Sender: TObject);

begin
  edDNA0.Text:= StringReplace(edDNA0.Text, '{', '', [rfReplaceAll]);
  edDNA0.Text:= StringReplace(edDNA0.Text, '}', '', [rfReplaceAll]);
  edDNA1.Text:= StringReplace(edDNA1.Text, '{', '', [rfReplaceAll]);
  edDNA1.Text:= StringReplace(edDNA1.Text, '}', '', [rfReplaceAll]);
  edProtein0.Text:= StringReplace(edProtein0.Text, '{', '', [rfReplaceAll]);
  edProtein0.Text:= StringReplace(edProtein0.Text, '}', '', [rfReplaceAll]);
  edProtein1.Text:= StringReplace(edProtein1.Text, '{', '', [rfReplaceAll]);
  edProtein1.Text:= StringReplace(edProtein1.Text, '}', '', [rfReplaceAll]);
  edDNA0.Text:= StringReplace(edDNA0.Text, '[', '', [rfReplaceAll]);
  edDNA0.Text:= StringReplace(edDNA0.Text, ']', '', [rfReplaceAll]);
  edDNA1.Text:= StringReplace(edDNA1.Text, '[', '', [rfReplaceAll]);
  edDNA1.Text:= StringReplace(edDNA1.Text, ']', '', [rfReplaceAll]);
  edProtein0.Text:= StringReplace(edProtein0.Text, '[', '', [rfReplaceAll]);
  edProtein0.Text:= StringReplace(edProtein0.Text, ']', '', [rfReplaceAll]);
  edProtein1.Text:= StringReplace(edProtein1.Text, '[', '', [rfReplaceAll]);
  edProtein1.Text:= StringReplace(edProtein1.Text, ']', '', [rfReplaceAll]);
end;

{ Button "Clear all": Clear user entry fields }

procedure TfMutations.btClearClick(Sender: TObject);

begin
  if btAction.Caption = 'Analyse' then begin
    edDNA0.Clear; edDNA1.Clear;
  end;
  if (btAction.Caption = 'Answer') and mSettingsExerciseTransEval.Checked then begin
    edProtein0.Clear; edProtein1.Clear;
  end;
end;

{ Buttons "Copy": Copy original sequence (DNA resp. protein) to mutated sequence field }

procedure TfMutations.btCopyDNAClick(Sender: TObject);

begin
  if btAction.Caption = 'Analyse' then begin
    if edDNA0.Lines.Count > 0 then
      edDNA1.Lines := edDNA0.Lines;
  end;
end;

procedure TfMutations.btCopyProteinClick(Sender: TObject);

begin
  if (btAction.Caption = 'Answer') and mSettingsExerciseTransEval.Checked then begin
    if edProtein0.Lines.Count > 0 then
      edProtein1.Lines := edProtein0.Lines;
  end;
end;

end.

