{***************************************}
{* Main unit for DNABasics application *}
{***************************************}

unit dna;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LCLIntf, bases, gcode, rseq;

type
  TBaseCount = record
    Base: Char;
    Count: Integer;
    Percent: Real;
  end;
  TBaseCounts = array[0..4] of TBaseCount;
  {*******}
  { TfDNA }
  {*******}
  TfDNA = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsLC, mSettingsExtended, mSettingsStart: TMenuItem;
    mSettingsStop, MenuItem1, mSettingsRandom, mSettingsDisplay: TMenuItem;
    mTools, mToolsBaseCodes, mToolsGenCode, mToolsRFrames6: TMenuItem;
    mHelp, mHelpBiology, mHelpHelp, mHelpAbout: TMenuItem;
    stSequence: TStaticText;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8, Label9: TLabel;
    Label10, Label11, Label12, Label13, Label15: TLabel;
    edDNA, edRNA, edProtein: TMemo;
    edTranslation, edProteins: TMemo;
    cobRFrame: TComboBox;
    laRFrame, laUnknown, laUUnknown, laUUnknownP, laGC, laUGC: TLabel;
    edLength, edPurines, edPyrimidines, edUnknown, edPurines2, edPyrimidines2, edUnknownP, edGC: TEdit;
    sgMolWeight, sgBases: TStringGrid;
    btLoadDNA, btRandomDNA, btClearDNA, btSaveDNA: TButton;
    btRevCompl, btTranscription, btAnalyze: TButton;
    btLoadRNA, btRandomRNA, btClearRNA, btSaveRNA: TButton;
    btRevTranscription, btTranslationRNA: TButton;
    btSaveProtein, btCode3: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsLCClick(Sender: TObject);
    procedure mSettingsExtendedClick(Sender: TObject);
    procedure mSettingsStartClick(Sender: TObject);
    procedure mSettingsStopClick(Sender: TObject);
    procedure mSettingsRandomClick(Sender: TObject);
    procedure mSettingsDisplayClick(Sender: TObject);
    procedure mToolsBaseCodesClick(Sender: TObject);
    procedure mToolsGenCodeClick(Sender: TObject);
    procedure mToolsRFrames6Click(Sender: TObject);
    procedure mHelpBiologyClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btLoadDNAClick(Sender: TObject);
    procedure btLoadRNAClick(Sender: TObject);
    procedure btRandomDNAClick(Sender: TObject);
    procedure btRandomRNAClick(Sender: TObject);
    procedure btClearDNAClick(Sender: TObject);
    procedure btClearRNAClick(Sender: TObject);
    procedure btSaveDNAClick(Sender: TObject);
    procedure btSaveRNAClick(Sender: TObject);
    procedure btSaveProteinClick(Sender: TObject);
    procedure btRevComplClick(Sender: TObject);
    procedure btTranscriptionClick(Sender: TObject);
    procedure btAnalyzeClick(Sender: TObject);
    procedure btRevTranscriptionClick(Sender: TObject);
    procedure btTranslationRNAClick(Sender: TObject);
    procedure btCode3Click(Sender: TObject);
    procedure edDNAEditingDone(Sender: TObject);
    procedure edRNAEditingDone(Sender: TObject);
  private
    iSeqMin, iSeqMax, iSeqUncertain, iCharsPerLine, iTransStart, iTransEnd: Integer;
    sDirOpen, sDirSave, sFasta, sStopCodon: string;
    sDNA, sRNA, sProtein: AnsiString;
    bBasesLowercase, bBasesUnknown, bUnknownOnly, bStartCodon, bStopCodon, bAminoAcid3: Boolean;
  end;

const
  DNABases: array[0..14] of Char = (
    'A', 'C', 'G', 'T', 'M', 'R', 'W', 'S', 'Y', 'K', 'V', 'H', 'D', 'B', 'N'
  );
  RNABases: array[0..14] of Char = (
    'A', 'C', 'G', 'U', 'M', 'R', 'W', 'S', 'Y', 'K', 'V', 'H', 'D', 'B', 'N'
  );

var
  fDNA: TfDNA;

implementation

{$R *.lfm}

{ Format base count values (right-align) }

function FormatCount(Count: Integer): string;

var
  N: Real;
  FCount: string;

begin
  if Count = 0 then
    FCount := '    0'
  else begin
    FCount := IntToStr(Count);
    N := 10000;
    while Count < N do begin
      FCount := ' ' + FCount;
      N /= 10;
    end;
  end;
  Result := ' ' + FCount;
end;

{ Format base percent values (right-align) }

function FormatPercent(Percent: Real): string;

var
  FPercent: string;

begin
  FPercent := FloatToStrF(Percent, ffFixed, 0, 2);
  if Percent < 10 then
    FPercent := '  ' + FPercent
  else if Percent < 100 then
    FPercent := ' ' + FPercent;
  Result := FPercent;
end;

{ Format molecular weight values (right-align) }

function FormatMolWeight(MolWeight: Real): string;

var
  N: Real;
  FMolWeight: string;

begin
  FMolWeight := FloatToStrF(MolWeight, ffFixed, 0, 2);
  N := 1E+12;
  while MolWeight < N do begin
    FMolWeight := ' ' + FMolWeight;
    N /= 10;
  end;
  Result := ' ' + FMolWeight;
end;

{ Check if a sequence is valid DNA/RNA }

function IsValidNuclAcid(Molecule: string; NuclAcid: AnsiString; BasesUnknown: Boolean): Boolean;

var
  N, I, J: Integer;
  Valid, OK: Boolean;

begin
  Valid := True;
  if BasesUnknown then
    N := 15                                                                    // all 15 base codes allowed
  else
    N := 4;                                                                    // only A, G, T (U), C allowed
  I := 1;
  while Valid and (I <= Length(NuclAcid)) do begin
    OK := False;
    J := 0;
    while not OK and (J < N) do begin
      if Molecule = 'DNA' then begin
        if NuclAcid[I] = DNABases[J] then
          OK := True;
      end
      else if Molecule = 'RNA' then begin
        if NuclAcid[I] = RNABases[J] then
          OK := True;
      end;
      Inc(J);
    end;
    if not OK then
      Valid := False;
    Inc(I);
  end;
  Result := Valid;
end;

{ Check if codon is a stop codon }

function IsStopCodon(Codon: string): Boolean;

var
  ItIs: Boolean;

begin
  ItIs := False;
  if (Codon = 'UAA') or (Codon = 'UAG') or (Codon = 'UGA') then
    ItIs := True;
  Result := ItIs;
end;

{ Determine nucleobase complement }

function BaseComplement(Base: Char): Char;

const
  BaseCompl: array['A'..'Z'] of Char = (
   'T', 'V', 'G', 'H', '?', '?', 'C', 'D', '?', '?', 'M', '?', 'K',
   'N', '?', '?', '?', 'Y', 'S', 'A', '?', 'B', 'W', '?', 'R', '?'
  );

var
  Compl: Char;

begin
  Compl := '?';
  if Base in ['A' .. 'Z'] then
    Compl := BaseCompl[Base];
  Result := Compl;
end;

{ Determine DNA sequence reverse complement }

function ReverseComplement(DNA: AnsiString): AnsiString;

var
  I: Integer;
  DNA2: AnsiString;

begin
  DNA2 := '';
  for I := Length(DNA) downto 1 do
    DNA2 += BaseComplement(DNA[I]);
  Result := DNA2;
end;

{ DNA to RNA transcription }

function Transcription(DNA: AnsiString): AnsiString;

begin
  Result := StringReplace(DNA, 'T', 'U', [rfReplaceAll]);
end;

{ RNA to DNA reverse transcription }

function ReverseTranscription(RNA: AnsiString): AnsiString;

begin
  Result := StringReplace(RNA, 'U', 'T', [rfReplaceAll]);
end;

{ RNA to protein translation }

procedure Translation(RNA: AnsiString; GeneticCode: TGeneticCode; RFrame: Integer; UseStartCodon, UseStopCodon: Boolean;
  out Protein: AnsiString; out TransStart, TransEnd: Integer; out StopCodon: string);

var
  I, J: Integer;
  Codon: string;
  AminoAcid: Char;
  OK, Stop: Boolean;

begin
  Protein := '';
  TransStart := Pos('AUG', RNA); TransEnd := 0; StopCodon := 'END'; OK := True;
  if UseStartCodon then begin
    // Start translation at start codon
    if TransStart = 0 then begin
      MessageDlg('RNA translation', 'No start codon found!', mtWarning, [mbOK], 0);
      OK := False;
    end;
  end
  else begin
    // Start translation at sequence begin (base 1,2, or 3)
    TransStart := RFrame;
  end;
  if OK then begin
    I := TransStart; Stop := False;
    while not Stop and (I <= Length(RNA)) do begin
      Codon := Copy(RNA, I, 3);
      if Length(Codon) = 3 then begin                                          // exceeding bases will be ignored
        // Apply genetic code to get amino acid corr. to this codon
        AminoAcid := '?'; J := 1;
        while (AminoAcid = '?') and (J <= 64) do begin
          if Codon = GeneticCode[J].Codon then
            AminoAcid := GeneticCode[J].AminoAcid;
          Inc(J);
        end;
        if AminoAcid <> '?' then begin
          if AminoAcid = '_' then begin
            // Codon is a stop codon
            if UseStopCodon then begin
              // Stop translation here, if this option has been selected by the user
              Stop := True;
              TransEnd := I;
              StopCodon := Copy(RNA, I, 3);
            end
            else begin
              // Just continue translating (using underscore as code corr. to stop codon)
              Protein += '_';
            end;
          end
          else begin
            // Codon is a "normal" codon
            Protein += AminoAcid;
          end;
        end
        else begin
          // All codons with uncertain/unknown bases will actually be translated to "unknown amino acid" (code "X")
          Protein += 'X';
        end;
      end;
      I += 3;
    end;
    if TransEnd = 0 then
      TransEnd := Length(RNA);                                                 // RNA has been translated until end of sequence
  end;
end;

{ DNA statistics: base counts, number of purines and pyrimidines, GC count }

procedure DNAStats(DNA: AnsiString; out SeqLength: Integer; out BaseCounts: TBaseCounts; out Purines, Pyrimidines, Unknown: Integer; out GC: Real);

const
  AllPurines = ['A', 'G', 'R'];
  AllPyrimidines = ['C', 'T', 'Y'];
  AllGC = ['G', 'C', 'S'];

var
  Base: Char;
  I, J, IGC: Integer;

begin
  SeqLength := Length(DNA);
  Purines := 0; Pyrimidines := 0; Unknown := 0; IGC := 0;
  // Init the base count array
  for I := 0 to 4 do begin
    if I = 4 then
      BaseCounts[I].Base := '?'                                                // count of all non A, G, T, C bases
    else
      BaseCounts[I].Base := DNABases[I];
    BaseCounts[I].Count := 0;
    BaseCounts[I].Percent := 0;
  end;
  // Base count (A, G, T, C)
  for J := 1 to Length(DNA) do begin
    Base := DNA[J];
    for I := 0 to 3 do begin
      if Base = BaseCounts[I].Base then begin
        Inc(BaseCounts[I].Count);
      end;
    end;
    // Purine, pyrimidine and unknown (not determined) count
    if Base in AllPurines then
      Inc(Purines)
    else if Base in AllPyrimidines then
      Inc(Pyrimidines)
    else
      Inc(Unknown);
    // GC count
    if Base in AllGC then
      Inc(IGC);
  end;
  // Base count (uncertain/unknown)
  BaseCounts[4].Count := SeqLength - (BaseCounts[0].Count + BaseCounts[1].Count + BaseCounts[2].Count + BaseCounts[3].Count);
  // Percentage calculations
  for I := 0 to 4 do
    BaseCounts[I].Percent := 100 * (BaseCounts[I].Count / SeqLength);
  GC := 100 * (IGC / SeqLength);
end;

{ Determine nucleobase molecular weights}

procedure DNAMolWeightBase(Base: Char; out MolWeight1, MolWeight2: Real);

// This routine only works correctly if the bases constituting an
// extended base code (ExtBases array) are ordered by molecular weight

const
  ExtBases: array['A'..'Z'] of string = (
    'A', 'CTG', 'C', 'TAG', '', '', 'G', 'CTA', '', '', 'TG', '', 'CA',
    'CTAG', '', '', '', 'AG', 'CG', 'T', '', 'CAG', 'TA', '', 'CT', ''
  );

var
  I: Integer;
  MolWeight: Real;
  Bases: array[1..2] of Char;

begin
  Bases[1] := LeftStr(ExtBases[Base], 1)[1];                                   // base with smallest molecular weight
  Bases[2] := RightStr(ExtBases[Base], 1)[1];                                  // base with highest molecular weight
  for I := 1 to 2 do begin
    // Two passes: first for smallest, second for highest molecular weight
    case Bases[I] of
      'A': MolWeight := 313.21;
      'C': MolWeight := 289.18;
      'G': MolWeight := 329.21;
      'T': MolWeight := 304.20;
    end;
    if I = 1 then
      MolWeight1 := MolWeight
    else
      MolWeight2 := MolWeight;
  end;
end;

{ Determine DNA sequence molecular weights}

procedure DNAMolWeight(DNA: AnsiString; out MolWeight1, MolWeight2: Real);

const
   OH = 17.01;                                                                 // molecular weight of 1 oxygen + 1 hydrogen

var
  I: Integer;
  BaseMolWeight1, BaseMolWeight2: Real;

begin
  MolWeight1 := 0; MolWeight2 := 0;
  for I := 1 to Length(DNA) do begin
    DNAMolWeightBase(DNA[I], BaseMolWeight1, BaseMolWeight2);
    MolWeight1 += BaseMolWeight1; MolWeight2 += BaseMolWeight2;
  end;
  MolWeight1 += OH; MolWeight2 += OH;
end;

{ Generate a random DNA/RNA sequence }

procedure RandomSequence(Molecule: string; Min, Max, PUnknown: Integer; UnknownOnly: Boolean; out Fasta: string; out Sequence: AnsiString);

// Random sequence will have no stop codon or 1 single stop codon in last third of sequence (frame 1 translation)

var
  L, C, S, R, I, J: Integer;
  Codon: string;

begin
  Fasta := '>Random ' + Molecule + ' sequence'; Sequence := '';
  L := Random(Max - Min + 1) + Min; C := 0;                                    // random sequence length
  for I := 1 to Round(L / 3) do begin
    // Random codons
    repeat
      // Loop codon generation until the codon generated is not a stop codon
      Codon := '';
      // Random codon bases, chosen among those allowed (standard bases only or all bases)
      for J := 1 to 3 do begin
        // The number of uncertain/unknown bases has to depend on the percentage specified by the user
        // The number of such bases generated by this algorithm may be several % less (sometimes a little bit more) than the
        // percentage specified. On the other side, the uncertain/unknown bases will be distributed over the entire sequence
        if Random(100) < 100 - PUnknown then
          R := Random(4)                                                       // standard base code
        else begin
          if 100 * (C / L) < PUnknown then begin
            R := Random(11) + 4;                                               // extended base code
            Inc(C);
            if UnknownOnly then
              R := 14;                                                         // unknown bases (N) only, if so selected by user
          end
          else
            R := Random(4);                                                    // standard base code
        end;
        Codon += RNABases[R];                                                  // add base to the codon
      end;
    until not IsStopCodon(Codon);
    Sequence += Codon;                                                         // add codon to the sequence
  end;
  // Now insert or not a stop codon into the sequence
  S := Random(4);
  if S <> 0 then begin
    case S of
      1: Codon := 'UAA';
      2: Codon := 'UAG';
      3: Codon := 'UGA';
    end;
    L := Length(Sequence);
    S := Random(L - Round(2 * L / 3) + 1) + Round(2 * L / 3);                  // stop codon should be in last 1/3 of the sequence
    while S mod 3 <> 1 do
      Dec(S);
    Insert(Codon, Sequence, S);
  end;
  // Sequence generated so far was RNA; if a DNA seuqnce is wanted, reverse transcript the RNA sequence
  if Molecule = 'DNA' then
    Sequence := ReverseTranscription(Sequence);
end;

{ Get DNA/RNA/protein sequence from TMemo input field and store it into an AnsiString }

procedure GetSequence(Molecule: string; MoleculeMemo: TMemo; BasesLC, BasesUnknown: Boolean; out Fasta: string; out Seq: AnsiString);

var
  First, I: Integer;

begin
  Seq := ''; Fasta := ''; First := 0;
  // Get FASTA header (if there is one)
  if LeftStr(MoleculeMemo.Text, 1) = '>' then begin
    First := 1;
    Fasta := Trim(MoleculeMemo.Lines[0]);
  end;
  // Get sequence data
  for I := First to MoleculeMemo.Lines.Count - 1 do begin
    // Get data line after line, transforming lowercase nucleobase codes to uppercase (if lowercase are allowed)
    if ((Molecule = 'DNA') or (Molecule = 'RNA')) and BasesLC then
      Seq += Uppercase(MoleculeMemo.Lines[I])
    else
      Seq += MoleculeMemo.Lines[I];
  end;
  // Remove all line-ending characters
  Seq := StringReplace(Seq, LineEnding, '', [rfReplaceAll]);
  // Check DNA/RNA sequence validity (setting sequence to empty string if not)
  if ((Molecule = 'DNA') or (Molecule = 'RNA')) and (not IsValidNuclAcid(Molecule, Seq, BasesUnknown)) then begin
    MessageDlg('Data error', 'Invalid ' + Molecule + ' sequence!', mtError, [mbOK], 0);
    Seq := ''; Fasta := '';
  end;
end;

{ Cut off FASTA header (to make it fit into one TMemo field line) }

function ShortFasta(Fasta: string): string;

// The routine tries to cut off the FASTA header in "some logical way"

const
  L = 90;

var
  P: Integer;
  ShFasta: string;

begin
  ShFasta := Fasta;
  if Length(Fasta) > L then begin
    P := Pos('|', Fasta);
    if P = 0 then
      ShFasta := LeftStr(Fasta, L - 3) + '...'
    else begin
      ShFasta := LeftStr(Fasta, P); Delete(Fasta, 1, P);
      repeat
        P := Pos('|', Fasta);
        if P = 0 then
          ShFasta += Fasta
        else
          Delete(Fasta, 1, P);
      until P = 0;
    end;
  end;
  Result := ShFasta;
end;

{ Fill sequence AnsiString into TMemo field (line length having been chosen by user) }

procedure PutSequence(var Memo: TMemo; Fasta: string; Seq: AnsiString; CPL: Integer; Clear: Boolean);

begin
  // The routine is used for both the TMemos visible on the form, and the invisible TMemo used to store the 6-reading-frames translation
  // sequences. In this last case, the TMemo has only to be cleared for the first sequence, and a blank line has to be added for the
  // others. To make all this work correctly, the Boolean "Clear" is used to tell the routine what has to be done.
  if Clear then
    Memo.Lines.Clear
  else
    Memo.Lines.AddText(LineEnding);
  // Display (cut off) FASTA header (if any)
  if Fasta <> '' then
    Memo.Lines.AddText(ShortFasta(Fasta));
  // If number of characters per line is set to zero, display the sequence as a whole ...
  if CPL = 0 then
    Memo.Lines.AddText(Seq)
  // ... otherwise display the sequence over several lines using a given number of characters per line
  else begin
    while Length(Seq) > 0 do begin
      Memo.Lines.AddText(LeftStr(Seq, CPL));
      Delete(Seq, 1, CPL);
    end;
  end;
end;

{ Save sequence to text file }

procedure SaveSequence(Filename, Fasta: string; Seq: AnsiString; CPL: Integer);

var
  Ret: Cardinal;
  FOut: Text;
  DoSave: Boolean;

begin
  DoSave := True;
  // If filename already exists, let decide user to overwrite it or not
  if FileExists(Filename) then begin
    DoSave := False;
    Ret := MessageDlg('Save sequence', 'File already exists. Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
    if Ret = mrYes then
      DoSave := True;
  end;
  // File will normally be saved, but...
  if DoSave then begin
    if ExtractFileExt(Filename) = '.fasta' then begin
      // If the sequence has to be saved as FASTA and there is no FASTA header, ask user what to do
      if Fasta = '' then begin
        DoSave := False;
        Ret := MessageDlg('Save sequence', 'No FASTA header found. Save anyway?', mtWarning, [mbYes, mbNo], 0, mbNo);
        if Ret = mrYes then begin
          Fasta := '>';
          DoSave := True;
        end;
      end;
    end;
  end;
  // All ok, save the file
  if DoSave then begin
    Assign(FOut, Filename); Rewrite(FOut);
    if ExtractFileExt(Filename) = '.fasta' then
      Writeln(FOut, Fasta);                                                    // save FASTA header (for .fasta file)
    if CPL = 0 then begin
      // If characters per line is set to zero, save sequence as a whole ...
      Writeln(FOut, Seq);
    end
    else begin
      // ... otherwise save sequence with a given number of characters per line
      while Length(Seq) > 0 do begin
        Writeln(FOut, LeftStr(Seq, CPL));
        Delete(Seq, 1, CPL);
      end;
    end;
    Close(FOut);
  end;
end;

{*******}
{ TfDNA }
{*******}

{ Application start: Initialization }

procedure TfDNA.FormCreate(Sender: TObject);

begin
  // Start-up form layout
  laUnknown.Visible := False; edUnknown.Visible := False; laUUnknown.Visible := False;
  edUnknownP.Visible := False; laUUnknownP.Visible := False;
  laGC.Top := laUnknown.Top + 6; edGC.Top := edUnknown.Top + 6; laUGC.Top := laUUnknown.Top + 6;
  // Initial "open" and "save" directories
  sDirOpen := GetCurrentDir + '/' + 'samples'; DoDirSeparators(sDirOpen);
  sDirSave := '';
  // Application start-up settings
  bBasesLowercase := True; bBasesUnknown := False; bUnknownOnly := False;
  bStartCodon := False; bStopCodon := False; bAminoAcid3 := False;
  iCharsPerLine := 72; iSeqMin := 100; iSeqMax := 500; iSeqUncertain := 10;
  // Start random generator (for random sequences)
  Randomize;
end;

{ Menu item "File > Exit": Exit application }

procedure TfDNA.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Settings > Allow lowercase base codes": Toggle allow/disallow lowercase base codes }

procedure TfDNA.mSettingsLCClick(Sender: TObject);

begin
  if mSettingsLC.Checked then
    mSettingsLC.Checked := False
  else
    mSettingsLC.Checked := True;
  bBasesLowercase := mSettingsLC.Checked;
end;

{ Menu item "Settings > Allow extended base codes": Toggle allow extended base codes / standard base codes only }

procedure TfDNA.mSettingsExtendedClick(Sender: TObject);

begin
  if mSettingsExtended.Checked then
    mSettingsExtended.Checked := False
  else
    mSettingsExtended.Checked := True;
  bBasesUnknown := mSettingsExtended.Checked;
end;

{ Menu item "Settings > Initiate translation at start codon": Toggle init translation at start codon / at begin of sequence }

procedure TfDNA.mSettingsStartClick(Sender: TObject);

begin
  if mSettingsStart.Checked then begin
    mSettingsStart.Checked := False;
    laRFrame.Visible := True; cobRFrame.Visible := True;
  end
  else begin
    mSettingsStart.Checked := True;
    laRFrame.Visible := False; cobRFrame.Visible := False;
  end;
  bStartCodon := mSettingsStart.Checked;
end;

{ Menu item "Settings > Terminate translation at stop codon": Toggle terminate translation at stop codon / at end of sequence }

procedure TfDNA.mSettingsStopClick(Sender: TObject);

begin
  if mSettingsStop.Checked then
    mSettingsStop.Checked := False
  else
    mSettingsStop.Checked := True;
  bStopCodon := mSettingsStop.Checked;
end;

{ Menu item "Settings > Random sequence properties ...": Open random sequence properties window }

procedure TfDNA.mSettingsRandomClick(Sender: TObject);

begin
  fRSeq.iMin := iSeqMin; fRSeq.iMax := iSeqMax;
  fRSeq.iUncertain := iSeqUncertain; fRSeq.bUnknownOnly := bUnknownOnly;
  fRSeq.ShowModal;
  iSeqMin := fRSeq.iMin; iSeqMax := fRSeq.iMax;
  iSeqUncertain := fRSeq.iUncertain; bUnknownOnly := fRSeq.bUnknownOnly;
end;

{ Menu item "Settings > Sequence display format ...": User input of sequence display format (characters per line) }

procedure TfDNA.mSettingsDisplayClick(Sender: TObject);

var
  S: string;

begin
  S := InputBox('Sequence format', 'Number of base resp. amino acid codes per line', IntToStr(iCharsPerLine));
  if (S <> '') and (StrToInt(S) >= 0) then
    iCharsPerLine := StrToInt(S);
end;

{ Menu item "Tools > Display base code table": Display nucleic acids code table }

procedure TfDNA.mToolsBaseCodesClick(Sender: TObject);

begin
  if fBases.Visible then
    fBases.Hide
  else
    fBases.Show;
end;

{ Menu item "Tools > Display genetic code table": Display genetic code table (using RNA codons) }

procedure TfDNA.mToolsGenCodeClick(Sender: TObject);

begin
  if fGenCode.Visible then
    fGenCode.Hide
  else
    fGenCode.Show;
end;

{ Menu item "Tools > 6 reading frame translation": Translate actual DNA sequence, considering all 6 reading frames }

procedure TfDNA.mToolsRFrames6Click(Sender: TObject);

// The routine uses the content of the DNA TMemo, transcripts it to RNA and then translates the sequence
// and its reverse complement for each of the 3 reading frames. The result of the translation is stored
// into an invisible TMemo, that is then written to a FASTA (or raw data) protein file.

var
  I, J: Integer;
  Ret: Cardinal;
  Fasta, Filename, S: string;
  DoSave, Clear: Boolean;

begin
  // Get sequence from DNA TMemo field
  GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
  Clear := True;
  if sDNA <> '' then begin
    // Transcript the sequence as is resp. transcript the sequence's reverse complement
    for I := 1 to 2 do begin
      if I = 1 then
        sRNA := Transcription(sDNA)
      else
        sRNA := Transcription(ReverseComplement(sDNA));
      if sRNA <> '' then begin
        // Translate sequence for each of the 3 reading frames
        for J := 1 to 3 do begin
          Fasta := sFasta;
          if I = 1 then
            Fasta += ' (reading frame ' + IntToStr(J) + ')'
          else
            Fasta += ' (reverse complement, reading frame ' + IntToStr(J) + ')';
          Translation(sRNA, fGenCode.aGeneticCode, J, bStartCodon, bStopCodon, sProtein, iTransStart, iTransEnd, sStopCodon);
          // Store the protein sequence into the invisible TMemo
          if sProtein <> '' then
            PutSequence(edProteins, Fasta, sProtein, iCharsPerLine, Clear);
          Clear := False;
        end;
      end;
    end;
    // Write the TMemo content (the 6 protein sequences) to a file
    if sDirSave = '' then begin
      S := GetCurrentDir + '/' + 'samples'; DoDirSeparators(S);
      if sDirOpen = S then
        sDirSave := GetCurrentDir + '/' + 'sequences'
      else
        sDirSave := sDirOpen;
    end;
    DoDirSeparators(sDirSave);
    dlgSave.InitialDir := sDirSave;
    dlgSave.FileName := '';
    if dlgSave.Execute then begin
      Filename := dlgSave.FileName;
      sDirSave := ExtractFileDir(Filename);
      DoSave := True;
      if FileExists(Filename) then begin
        // If the filename already exists, let user decide to overwrite it or not
        DoSave := False;
        Ret := MessageDlg('Save sequences', 'File already exists. Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
        if Ret = mrYes then
          DoSave := True;
      end;
      // Write TMemo content to file
      if DoSave then
        edProteins.Lines.SaveToFile(Filename);
    end;
  end;
end;

{ Menu item "Help > Biology help": Point webbrowser to genetics code tutorial at streetinfo.lu }

procedure TfDNA.mHelpBiologyClick(Sender: TObject);

var
  URL: string;

begin
  URL := 'https://www.streetinfo.lu/learning/bio/codegen.html';
  OpenURL(URL);
end;

{ Menu item "Help > Application help": Open application help file in webbrowser }

procedure TfDNA.mHelpHelpClick(Sender: TObject);

begin
  OpenDocument('help.html');
end;

{ Menu item "Help > About": Display application about }

procedure TfDNA.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Molecular biology:' + LineEnding;
  S += 'DNA properties, transcription and translation.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September 2019 - December 2022.';
  MessageDlg('About "DNABasics"', S, mtInformation, [mbOK], 0);
end;

{ "DNA" Button "Load": Load a DNA sequence from file }

procedure TfDNA.btLoadDNAClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpen.InitialDir := sDirOpen;
  if dlgOpen.Execute then begin
    edDNA.Lines.Clear; edRNA.Lines.Clear; edProtein.Lines.Clear; edTranslation.Lines.Clear;
    stSequence.Caption := '';
    FileName := dlgOpen.Filename;
    sDirOpen := ExtractFileDir(Filename);
    // Load sequence into "DNA" TMemo
    edDNA.Lines.LoadFromFile(FileName);
    // Get TMemo content into an AnsiString
    GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
    if sDNA <> '' then begin
      // Extract FASTA header
      if sFasta = '' then
        stSequence.Caption := 'Unknown DNA sequence'
      else
        stSequence.Caption := 'DNA: ' + RightStr(sFasta, Length(sFasta) - 1);
      // Rewrite sequence into "DNA" TMemo (using actual display format)
      PutSequence(edDNA, sFasta, sDNA, iCharsPerLine, True);
    end;
  end;
end;

{ "DNA" Button "Random": Generate a random DNA sequence }

procedure TfDNA.btRandomDNAClick(Sender: TObject);

var
  SeqUncertain: Integer;

begin
  edDNA.Lines.Clear; edRNA.Lines.Clear; edProtein.Lines.Clear; edTranslation.Lines.Clear;
  stSequence.Caption := '';
  if mSettingsExtended.Checked then
    SeqUncertain := iSeqUncertain                                              // maximum percentage of uncertain/unknown bases
  else
    SeqUncertain := 0;                                                         // if extended base codes aren't allowed, set percentage to 0
  RandomSequence('DNA', iSeqMin, iSeqMax, SeqUncertain, bUnknownOnly, sFasta, sDNA);
  stSequence.Caption := RightStr(sFasta, Length(sFasta) - 1);
  // Store sequence into "DNA" TMemo (using actual display format)
  PutSequence(edDNA, sFasta, sDNA, iCharsPerLine, True);
end;

{ "DNA" Button "Clear": Clear DNA sequence TMemo }

procedure TfDNA.btClearDNAClick(Sender: TObject);

begin
  edDNA.Lines.Clear;
end;

{ "DNA" Button "Save": Save DNA sequence to file }

procedure TfDNA.btSaveDNAClick(Sender: TObject);

var
  Filename, S: string;

begin
  if sDirSave = '' then begin
    S := GetCurrentDir + '/' + 'samples'; DoDirSeparators(S);
    if sDirOpen = S then
      sDirSave := GetCurrentDir + '/' + 'sequences'
    else
      sDirSave := sDirOpen;
  end;
  DoDirSeparators(sDirSave);
  dlgSave.InitialDir := sDirSave;
  dlgSave.FileName := '';
  if dlgSave.Execute then begin
    Filename := dlgSave.FileName;
    sDirSave := ExtractFileDir(Filename);
    // Get sequence from "DNA" TMemo into an AnsiString
    GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
    // Save the sequence to a FASTA (or raw data) DNA file
    if sDNA <> '' then
      SaveSequence(Filename, sFasta, sDNA, iCharsPerLine);
  end;
end;

{ "RNA" Button "Load": Load a DNA sequence from file }

procedure TfDNA.btLoadRNAClick(Sender: TObject);

var
  FileName: string;

begin
  if sDirOpen = '' then begin
    sDirOpen := GetCurrentDir + '/' + 'samples'; DoDirSeparators(sDirOpen);
  end;
  dlgOpen.InitialDir := sDirOpen;
  if dlgOpen.Execute then begin
    edDNA.Lines.Clear; edRNA.Lines.Clear; edProtein.Lines.Clear; edTranslation.Lines.Clear;
    stSequence.Caption := '';
    FileName := dlgOpen.Filename;
    sDirOpen := ExtractFileDir(Filename);
    // Load sequence into "RNA" TMemo
    edRNA.Lines.LoadFromFile(FileName);
    // Get TMemo content into an AnsiString
    GetSequence('RNA', edRNA, bBasesLowercase, bBasesUnknown, sFasta, sRNA);
    if sRNA <> '' then begin
      // Extract FASTA header
      if sFasta = '' then
        stSequence.Caption := 'Unknown RNA sequence'
      else
        stSequence.Caption := 'RNA: ' + RightStr(sFasta, Length(sFasta) - 1);
      // Rewrite sequence into "RNA" TMemo (using actual display format)
      PutSequence(edRNA, sFasta, sRNA, iCharsPerLine, True);
    end;
  end;
end;

{ "RNA" Button "Random": Generate a random RNA sequence }

procedure TfDNA.btRandomRNAClick(Sender: TObject);

var
  SeqUncertain: Integer;

begin
  edDNA.Lines.Clear; edRNA.Lines.Clear; edProtein.Lines.Clear; edTranslation.Lines.Clear;
  stSequence.Caption := '';
  if mSettingsExtended.Checked then
    SeqUncertain := iSeqUncertain                                              // maximum percentage of uncertain/unknown bases
  else
    SeqUncertain := 0;                                                         // if extended base codes aren't allowed, set percentage to 0
  RandomSequence('RNA', iSeqMin, iSeqMax, SeqUncertain, bUnknownOnly, sFasta, sRNA);
  stSequence.Caption := RightStr(sFasta, Length(sFasta) - 1);
  // Store sequence into "RNA" TMemo (using actual display format)
  PutSequence(edRNA, sFasta, sRNA, iCharsPerLine, True);
end;

{ "RNA" Button "Clear": Clear RNA sequence TMemo }

procedure TfDNA.btClearRNAClick(Sender: TObject);

begin
  edRNA.Lines.Clear;
end;

{ "RNA" Button "Save": Save RNA sequence to file }

procedure TfDNA.btSaveRNAClick(Sender: TObject);

var
  Filename, S: string;

begin
  if sDirSave = '' then begin
    S := GetCurrentDir + '/' + 'samples'; DoDirSeparators(S);
    if sDirOpen = S then
      sDirSave := GetCurrentDir + '/' + 'sequences'
    else
      sDirSave := sDirOpen;
  end;
  DoDirSeparators(sDirSave);
  dlgSave.InitialDir := sDirSave;
  dlgSave.FileName := '';
  if dlgSave.Execute then begin
    Filename := dlgSave.FileName;
    sDirSave := ExtractFileDir(Filename);
    // Get sequence from "RNA" TMemo into an AnsiString
    GetSequence('RNA', edRNA, bBasesLowercase, bBasesUnknown, sFasta, sRNA);
    // Save the sequence to a FASTA (or raw data) RNA file
    if sRNA <> '' then
      SaveSequence(Filename, sFasta, sRNA, iCharsPerLine);
  end;
end;

{ Button "Reverse complement": Determine DNA sequence reverse complement }

procedure TfDNA.btRevComplClick(Sender: TObject);

begin
  edRNA.Lines.Clear; edProtein.Lines.Clear; edTranslation.Lines.Clear;
  // Get sequence from "DNA" TMemo into an AnsiString
  GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
  // Calculate reverse complement and put it into "DNA" TMemo (using actual format)
  if sDNA <> '' then begin
    sDNA := ReverseComplement(sDNA);
    PutSequence(edDNA, sFasta, sDNA, iCharsPerLine, True);
  end;
end;

{ Button "Transcript": Transcript DNA into a RNA sequence }

procedure TfDNA.btTranscriptionClick(Sender: TObject);

begin
  edRNA.Lines.Clear; edProtein.Lines.Clear; edTranslation.Lines.Clear;
  // Get sequence from "DNA" TMemo into an AnsiString
  GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
  // Do the transcription and put the sequence into the "RNA" TMeno (using actual format)
  if sDNA <> '' then begin
    sRNA := Transcription(sDNA);
    PutSequence(edRNA, sFasta, sRNA, iCharsPerLine, True);
  end;
end;

{ Button "Analyze": Analyze DNA sequence (molecular) weight and base statistics }

procedure TfDNA.btAnalyzeClick(Sender: TObject);

var
  SeqLength, Purines, Pyrimidines, Unknown, I: Integer;
  GC: Real;
  MolWeight1, MolWeight2, MolWeight3, MolWeight4: Real;
  BaseCounts: TBaseCounts;

begin
  // Get sequence from "DNA" TMemo into an AnsiString
  GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
  if sDNA <> '' then begin
    // Determine and fill in DNA bases statistics
    DNAStats(sDNA, SeqLength,  BaseCounts, Purines, Pyrimidines, Unknown, GC);
    edLength.Text := IntToStr(SeqLength);
    for I := 0 to 4 do begin
      if (I <> 4) or mSettingsExtended.Checked then begin
        sgBases.Cells[I + 1, 1] := FormatCount(BaseCounts[I].Count);
        sgBases.Cells[I + 1, 2] := FormatPercent(BaseCounts[I].Percent);
      end
      else begin
        sgBases.Cells[I + 1, 1] := '';
        sgBases.Cells[I + 1, 2] := '';
      end;
    end;
    edPurines.Text := IntToStr(Purines); edPurines2.Text := FloatToStrF(100 * (Purines / SeqLength), ffFixed, 0, 2);
    edPyrimidines.Text := IntToStr(Pyrimidines); edPyrimidines2.Text := FloatToStrF(100 * (Pyrimidines / SeqLength), ffFixed, 0, 2);
    edUnknown.Text := IntToStr(Unknown); edUnknownP.Text := FloatToStrF(100 * (Unknown / SeqLength), ffFixed, 0, 2);
    edGC.Text := FloatToStrF(GC, ffFixed, 0, 2);
    if mSettingsExtended.Checked then begin
      laUnknown.Visible := True; laUUnknown.Visible := True; laUUnknownP.Visible := True;
      edUnknown.Visible := True; edUnknownP.Visible := True;
      laGC.Top := laUnknown.Top + 40; laUGC.Top := laUUnknown.Top + 40; edGC.Top := edUnknown.Top + 40; laUGC.Caption := '% minimum';
    end
    else begin
      laUnknown.Visible := False; laUUnknown.Visible := False; laUUnknownP.Visible := False;
      edUnknown.Visible := False; edUnknownP.Visible := False;
      laGC.Top := laUnknown.Top + 6; laUGC.Top := laUUnknown.Top + 6; edGC.Top := edUnknown.Top + 6; laUGC.Caption := '%';
    end;
    // Determine and fill in DNA sequence molecular weight(s)
    DNAMolWeight(sDNA, MolWeight1, MolWeight2);
    sgMolWeight.Cells[1, 1] := FormatMolWeight(MolWeight1);
    sgMolWeight.Cells[1, 2] := FormatMolWeight(MolWeight2);
    DNAMolWeight(ReverseComplement(sDNA), MolWeight3, MolWeight4);
    sgMolWeight.Cells[2, 1] := FormatMolWeight(MolWeight1 + MolWeight3);
    sgMolWeight.Cells[2, 2] := FormatMolWeight(MolWeight2 + MolWeight4);
  end;
end;

{ Button "Reverse transcript": Reverse transcript RNA into DNA sequence }

procedure TfDNA.btRevTranscriptionClick(Sender: TObject);

begin
  edDNA.Lines.Clear;
  // Get sequence from "RNA" TMemo into an AnsiString
  GetSequence('RNA', edRNA, bBasesLowercase, bBasesUnknown, sFasta, sRNA);
  if sRNA <> '' then begin
    // Do reverse transcription and put sequence into "DNA" TMemo (using actual format)
    sDNA := ReverseTranscription(sRNA);
    PutSequence(edDNA, sFasta, sDNA, iCharsPerLine, True);
  end;
end;

{ Button "Translate": Translate RNA sequence into a protein }

procedure TfDNA.btTranslationRNAClick(Sender: TObject);

var
  ReadingFrame, TL, NT, L, I: Integer;
  S, SL: string;

begin
  edProtein.Lines.Clear;
  // Get sequence from "RNA" TMemo into an AnsiString
  GetSequence('RNA', edRNA, bBasesLowercase, bBasesUnknown, sFasta, sRNA);
  // Get reading frame for this translation
  if sRNA <> '' then begin
    if mSettingsStart.Checked then
      ReadingFrame := 1                                                        // if start codon is considered, use reading frame 1
    else
      ReadingFrame := StrToInt(cobRFrame.Items[cobRFrame.ItemIndex]);          // if start codon isn't considered, use reading frame selected by user
    // Do the translation (using actual parameters)
    Translation(sRNA, fGenCode.aGeneticCode, ReadingFrame, bStartCodon, bStopCodon, sProtein, iTransStart, iTransEnd, sStopCodon);
    if sProtein <> '' then begin
      // Put sequence in "protein" TMemo (using actual format)
      PutSequence(edProtein, sFasta, sProtein, iCharsPerLine, True); bAminoAcid3 := False;
      // Fill in the translation report TMemo
      edTranslation.Lines.Clear;
      TL := iTransEnd - iTransStart + 1;
      if bStopCodon and (sStopCodon <> 'END') then
        Dec(TL);
      if bStartCodon then
        S := 'Nucleic acid translated from start codon AUG at position ' + IntToStr(iTransStart)
      else begin
        S := 'Nucleic acid translated from sequence begin ';
        if ReadingFrame > 1 then
          S += 'at position ' + IntToStr(ReadingFrame) + ' ';
        S += '(not considering start codon)';
      end;
      if sStopCodon = 'END' then begin
        S += ' to sequence end ';
        if mSettingsStop.Checked then
          S += '(no stop codon found)'
        else
          S += '(not considering stop codons)';
        NT := TL mod 3;
        if NT <> 0 then begin
          S += ', ignoring the last ';
          if NT = 1 then
            S += 'base'
          else
            S += '2 bases';
          TL -= NT;
        end;
        S += '.';
      end
      else
        S += ' to stop codon ' + sStopCodon + ' at position ' + IntToStr(iTransEnd) + '.';
      edTranslation.Lines.AddText(S); edTranslation.Lines.AddText(LineEnding);
      edTranslation.Lines.AddText('Translated RNA length = ' + IntToStr(TL) + ' nucleotides.');
      L := Length(sProtein);
      if not bStopCodon then begin
        for I := 1 to Length(sProtein) do begin
          if sProtein[I] = '_' then
            Dec(L);
        end;
      end;
      SL := IntToStr(L);
      if Length(IntToStr(TL)) > Length(SL) then
        SL := ' ' + SL;
      edTranslation.Lines.AddText('Polypeptide length    = ' + SL + ' amino acids.');
    end;
  end;
end;

{ "Protein" Button "Save": Save protein sequence to file }

procedure TfDNA.btSaveProteinClick(Sender: TObject);

var
  CharsPerLine: Integer;
  Filename, S: string;

begin
  if sDirSave = '' then begin
    S := GetCurrentDir + '/' + 'samples'; DoDirSeparators(S);
    if sDirOpen = S then
      sDirSave := GetCurrentDir + '/' + 'sequences'
    else
      sDirSave := sDirOpen;
  end;
  DoDirSeparators(sDirSave);
  dlgSave.InitialDir := sDirSave;
  dlgSave.FileName := '';
  if dlgSave.Execute then begin
    Filename := dlgSave.FileName;
    sDirSave := ExtractFileDir(Filename);
    // Get sequence from "protein" TMemo and store it into an AnsiString
    GetSequence('PROT', edProtein, False, False, sFasta, sProtein);
    if sProtein <> '' then begin
      // Save protein sequence to file (using actual format)
      CharsPerLine := iCharsPerLine;
      if bAminoAcid3 then begin                                                // adapt characters per line if protein is in 3-letter code
        while CharsPerLine mod 3 <> 0 do
          Dec(CharsPerLine);
      end;
      SaveSequence(Filename, sFasta, sProtein, CharsPerLine);
    end;
  end;
end;

{ Button 3-letter code: Display protein sequence, using 3-letter code }

procedure TfDNA.btCode3Click(Sender: TObject);

var
  CharsPerLine: Integer;
  Protein3: AnsiString;

begin
  // Do only if not already is 3-letter code
  if not bAminoAcid3 then begin
    bAminoAcid3 := True;
    // Get sequence from "protein" TMemo and store it into an AnsiString
    GetSequence('PROT', edProtein, False, False, sFasta, sProtein);
    // Determine sequence 3-letter code
    Protein3 := gcode.GetProtein3(sProtein);
    edProtein.Lines.Clear;
    // Adapt characters per line for protein in 3-letter code
    CharsPerLine := iCharsPerLine;
    while CharsPerLine mod 3 <> 0 do
      Dec(CharsPerLine);
    // Put sequence into "protein" TMemo (using actual format)
    PutSequence(edProtein, sFasta, Protein3, CharsPerLine, True);
  end;
end;

{ Manual DNA sequence input: Make sure that it's a valid sequence }

procedure TfDNA.edDNAEditingDone(Sender: TObject);

begin
  edRNA.Lines.Clear; edProtein.Lines.Clear; stSequence.Caption := '';
  // Get sequence from "DNA" TMemo and store it into an AnsiString
  GetSequence('DNA', edDNA, bBasesLowercase, bBasesUnknown, sFasta, sDNA);
  if sDNA <> '' then begin
    // If the sequence is invalid the AnsiString will be empty
    if sFasta = '' then
      stSequence.Caption := 'Custom DNA sequence'
    else
      stSequence.Caption := 'DNA: ' + RightStr(sFasta, Length(sFasta) - 1);
  end;
end;

{ Manual RNA sequence input: Make sure that it's a valid sequence }

procedure TfDNA.edRNAEditingDone(Sender: TObject);

begin
  edDNA.Lines.Clear; edProtein.Lines.Clear; stSequence.Caption := '';
  // Get sequence from "RNA" TMemo and store it into an AnsiString
  GetSequence('RNA', edRNA, bBasesLowercase, bBasesUnknown, sFasta, sRNA);
  if sRNA <> '' then begin
    // If the sequence is invalid the AnsiString will be empty
    if sFasta = '' then
      stSequence.Caption := 'Custom RNA sequence'
    else
      stSequence.Caption := 'RNA: ' + RightStr(sFasta, Length(sFasta) - 1);
  end;
end;

end.

