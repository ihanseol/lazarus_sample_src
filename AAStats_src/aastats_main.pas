{*************************************}
{* Main unit for AAStats application *}
{*************************************}

unit aastats_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  aastats_count, aastats_class, aastats_vol, aastats_polarity, aastats_charge, aastats_others;

const
  AAClasses: array[1..7] of string = (
    'aliphatic', 'sulfur-containing', 'aromatic', 'neutral', 'acidic', 'basic', 'cyclic'
  );
  AAVols: array[1..5] of string = (
    'very small', 'small', 'medium', 'large', 'very large'
  );
  AASideChains: array[1..4] of string = (
    'positively charged', 'negatively charged', 'uncharged polar', 'nonpolar'
  );
  AAPolarities: array[1..2] of string = (
    'polar', 'nonpolar'
  );
  AACharges: array[1..3] of string = (
    'positively charged', 'negatively charged', 'uncharged'
  );
  AAHydropathies: array[1..3] of string = (
    'hydrophobic', 'hydrophilic', 'neutral'
  );
  AADietRequirements: array[1..3] of string = (
    'essential', 'semi-essential', 'non-essential'
  );
  AAMetabolisms: array[1..3] of string = (
    'glucogenic', 'ketogenic', 'glucogenic & ketogenic'
  );

type
  TAminoAcid = record
    AACode: Char;
    AACode3, AAName: string;
    AAMolWeight: Real;
    AAClass, AAVol, AASideChain, AACharge, AAPolarity, AAHydropathy, AADietReq, AAMetabolism: Byte;
  end;
  TAminoAcids = array['A'..'Z'] of TAminoAcid;
  TArray20I = array[1..20] of LongInt;
  TArray20R = array[1..20] of Real;
  TArray2I  = array[1..2] of LongInt;
  TArray3I  = array[1..3] of LongInt;
  TArray4I  = array[1..4] of LongInt;
  TArray5I  = array[1..5] of LongInt;
  TArray7I  = array[1..7] of LongInt;
  {TfAAStats}
  TfAAStats = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mSettings, mSettingsCode1LC, mSettingsCode3UC: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    Label1, Label2: TLabel;
    cobStatistics: TComboBox;
    edProtein: TMemo;
    rbCode1: TRadioButton;
    rbCode3: TRadioButton;
    btLoad: TButton;
    btClear: TButton;
    btCounts: TButton;
    dlgOpen: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mSettingsCode1LCClick(Sender: TObject);
    procedure mSettingsCode3UCClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btCountsClick(Sender: TObject);
    procedure edProteinChange(Sender: TObject);
  private
    iCount: LongInt;
    rMolWeight: Real;
    sFasta: string;
    bNewProtein: Boolean;
    sProtein: AnsiString;
    aCounts: TArray20I;
    aMolWeights: TArray20R;
    aClasses: TArray7I;
    aVolumes: TArray5I;
    aSideChains: TArray4I;
    aPolarities: TArray2I;
    aCharges, aHydropathies, aDietReqs, aMetabolisms: TArray3I;
    aAminoAcids: TAminoAcids;
  end;

var
  fAAStats: TfAAStats;

implementation

{$R *.lfm}

{ Read amino acids data (incl. all classification info) from text file }

procedure ReadAminoAcids(var AA: TAminoAcids);

// All items are at fixed line-positions; classification info is full text
// Classification info in the TAminoAcid record is the index of actual classification in corresponding array constant
// This index is supposed to be the same as the position of the classification item in the corr. grid and charts!

var
  I: Integer;
  Line, S: string;
  Code: Char;
  InFile: Text;

begin
  Assign(InFile, 'aa.txt'); Reset(InFile);
  while not EoF(InFile) do begin
    Readln(InFile, Line);
    if Line <> '' then begin
      Code := LeftStr(Line, 1)[1];
      with AA[Code] do begin
        AACode := Code;
        AACode3 := Copy(Line, 3, 3);
        AAName := Trim(Copy(Line, 7, 13));
        Val(Copy(Line, 21, 8), AAMolWeight);
        AAClass := StrToInt(Copy(Line, 30, 1));
        S := Trim(Copy(Line, 32, 10));
        for I := 1 to 5 do begin
          if S = AAVols[I] then
            AAVol := I;
        end;
        S := Trim(Copy(Line, 43, 18));
        for I := 1 to 4 do begin
          if S = AASideChains[I] then
            AASideChain := I;
        end;
        S := Trim(Copy(Line, 62, 18));
        for I := 1 to 3 do begin
          if S = AACharges[I] then
            AACharge := I;
        end;
        S := Trim(Copy(Line, 81, 8));
        for I := 1 to 2 do begin
          if S = AAPolarities[I] then
            AAPolarity := I;
        end;
        S := Trim(Copy(Line, 90, 11));
        for I := 1 to 3 do begin
          if S = AAHydropathies[I] then
            AAHydropathy := I;
        end;
        S := Trim(Copy(Line, 102, 14));
        for I := 1 to 3 do begin
          if S = AADietRequirements[I] then
            AADietReq := I;
        end;
        S := Trim(Copy(Line, 117, Length(Line)));
        for I := 1 to 3 do begin
          if S = AAMetabolisms[I] then
            AAMetabolism := I;
        end;
      end;
    end;
  end;
  Close(InFile);
end;

{ Amino acids 3-letter to 1-letter code transformation }

function Code3ToCode1(Code3: string; var AminoAcids: TAminoAcids; UC: Boolean): Char;

// 1-letter codes are retrieved from the TAminoAcids record structure
// The UC flag indicates if all uppercase 3-letter codes have to be accepted

var
  I: Integer;
  Code1: Char;

begin
  Code1 := '-';
  I := Ord('A');
  while (I <= Ord('Z')) and (Code1 = '-') do begin
    if (Code3 = AminoAcids[Chr(I)].AACode3) or (UC and (Code3 = UpperCase(AminoAcids[Chr(I)].AACode3))) then
      Code1 := AminoAcids[Chr(I)].AACode;
    Inc(I);
  end;
  Code3ToCode1 := Code1;
end;

{ Validity check of 1-letter amino acid code }

function ValidCode(Code: Char; LC: Boolean): Boolean;

// The LC flag indicates if lowercase amino acid codes have to be accepted

var
  Valid: Boolean;

begin
  Valid := False;
  if LC then
    Code := UpperCase(Code)[1];
  if Code in ['A', 'C'..'I', 'K'..'N', 'P'..'T', 'V', 'W', 'Y'] then
    Valid := True;
  ValidCode := Valid;
end;

{ Amino acids counts by number }

procedure StatsAACounts(Code: Char; var AminoAcids: TAminoAcids; var Counts: TArray20I; var MolWeights: TArray20R);

// The elements of the arrays returned are sorted by the 1-letter code!

const
  AAIndexes: array['A'..'Z'] of Byte = (
    1, 0, 2, 3, 4, 5, 6, 7, 8, 0, 9, 10, 11, 12, 0, 13, 14, 15, 16, 17, 0, 18, 19, 0, 20, 0
  );

var
  IX: Integer;

begin
  IX := AAIndexes[Code];
  Counts[IX] += 1;
  MolWeights[IX] += AminoAcids[Code].AAMolWeight;
end;

{------------------------------------------------------------------------------}
{ In all of the following routines, indexes are supposed to be the same in the }
{ classification constant array, the grids, the charts and the array returned! }
{------------------------------------------------------------------------------}

{ Amino acids counts by structure (= class) }

procedure StatsAAClasses(Code: Char; var AminoAcids: TAminoAcids; var Classes: TArray7I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AAClass;
  Classes[IX] += 1;
end;

{ Amino acids counts by volume (= size) }

procedure StatsAAVolumes(Code: Char; var AminoAcids: TAminoAcids; var Volumes: TArray5I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AAVol;
  Volumes[IX] += 1;
end;

{ Amino acids counts by polarity and sidechain specificity }

procedure StatsAAPolarities(Code: Char; var AminoAcids: TAminoAcids; var Polarities: TArray2I; var Sidechains: TArray4I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AAPolarity;
  Polarities[IX] += 1;
  IX := AminoAcids[Code].AASideChain;
  SideChains[IX] += 1;
end;

{ Amino acids counts by charge and sidechain specificity }

procedure StatsAACharges(Code: Char; var AminoAcids: TAminoAcids; var Charges: TArray3I; var Sidechains: TArray4I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AACharge;
  Charges[IX] += 1;
  IX := AminoAcids[Code].AASideChain;
  SideChains[IX] += 1;
end;

{ Amino acids counts by hydropathy }

procedure StatsAAHydropathies(Code: Char; var AminoAcids: TAminoAcids; var Hydropathies: TArray3I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AAHydropathy;
  Hydropathies[IX] += 1;
end;

{ Amino acids counts by diet-requirements }

procedure StatsAADietRequirements(Code: Char; var AminoAcids: TAminoAcids; var DietReqs: TArray3I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AADietReq;
  DietReqs[IX] += 1;
end;

{ Amino acids counts by metabolism }

procedure StatsAAMetabolisms(Code: Char; var AminoAcids: TAminoAcids; var Metabolisms: TArray3I);

var
  IX: Integer;

begin
  IX := AminoAcids[Code].AAMetabolism;
  Metabolisms[IX] += 1;
end;

{*************}
{* TfAAStats *}
{*************}

{ Application start: Initialisation }

procedure TfAAStats.FormCreate(Sender: TObject);

begin
  // Create TAminoAcids structure, using a 'A'..'Z' array (inexisting codes will be set to '-')
  // B, X and Z are set here (but will be returned as invalid by the 1-letter code validation routine)
  // For all other amino acids, data (incl. all classification info) is read from the file 'aa.txt'
  aAminoAcids['B'].AACode := 'B'; aAminoAcids['B'].AACode3 := 'Asx'; aAminoAcids['B'].AAName := '(D or N)';
  aAminoAcids['J'].AACode := '-'; aAminoAcids['J'].AACode3 := '---'; aAminoAcids['J'].AAName := '(Error)';
  aAminoAcids['O'].AACode := '-'; aAminoAcids['O'].AACode3 := '---'; aAminoAcids['O'].AAName := '(Error)';
  aAminoAcids['U'].AACode := '-'; aAminoAcids['U'].AACode3 := '---'; aAminoAcids['U'].AAName := '(Error)';
  aAminoAcids['X'].AACode := 'X'; aAminoAcids['X'].AACode3 := 'Xxx'; aAminoAcids['X'].AAName := '(Unknown)';
  aAminoAcids['Z'].AACode := 'Z'; aAminoAcids['Z'].AACode3 := 'Glx'; aAminoAcids['Z'].AAName := '(E or Q)';
  ReadAminoAcids(aAminoAcids);                                                 // read amino acids data from file
  // The bNewProtein variable will be set to True each time a new protein has been entered (the memo content has changed).
  // This avoids repetitive assignment of the memo content to the string variable, as well as repetitive addition of the
  // CR/LF (see below) at the end of the memo
  bNewProtein := False;
end;

{ Menu item "File > Exit": Exit the application }

procedure TfAAStats.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu items "Settings > ...": Select to allow or not non-standard code case }

procedure TfAAStats.mSettingsCode1LCClick(Sender: TObject);

begin
  if mSettingsCode1LC.Checked then
    mSettingsCode1LC.Checked := False
  else
    mSettingsCode1LC.Checked := True;
end;

procedure TfAAStats.mSettingsCode3UCClick(Sender: TObject);

begin
  if mSettingsCode3UC.Checked then
    mSettingsCode3UC.Checked := False
  else
    mSettingsCode3UC.Checked := True;
end;

{ Menu item "Help > Help": Display (short) program help text }

procedure TfAAStats.mHelpHelpClick(Sender: TObject);

var
  S: string;

begin
  S := 'Choose the desired classification, enter a protein or load one from file (raw data ';
  S += 'or FASTA), select the appropriate amino acid coding (1 or 3 letters) and push the ';
  S += '"Counts" button. Use the "Settings" menu to allow lowercase 1-letter codes.';
  MessageDlg('"AAStats" Help', S, mtInformation, [mbOK], 0);
end;

{ Menu item "Help > About": Display program about text }

procedure TfAAStats.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Quantitative protein statistics, graphing amino acids by number, class, volume, ';
  S += 'charge, polarity, hydropathy, diet-requirements and metabolism.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, September, 2018.';
  MessageDlg('About "AAStats"', S, mtInformation, [mbOK], 0);
end;

{ Button "Load": Load protein from file }

procedure TfAAStats.btLoadClick(Sender: TObject);

var
   Filename: string;

begin
  if dlgOpen.InitialDir = '' then
    dlgOpen.InitialDir := GetCurrentDir;                                       // start looking in current directory
  if dlgOpen.Execute then begin
    FileName := dlgOpen.Filename;
    edProtein.Lines.LoadFromFile(FileName);
  end;
end;

{ Button "Clear": Clear the protein data (in the data entry memo) }

procedure TfAAStats.btClearClick(Sender: TObject);

begin
  edProtein.Clear;
end;

{ Button "Counts": Do the counting selected and display the corresponding charts }

procedure TfAAStats.btCountsClick(Sender: TObject);

var
  First, I: Integer;
  Code3, Mess: string;
  Code: Char;

begin
  if edProtein.Text <> '' then begin;
    if bNewProtein then begin
      // Add a CR/LF to the Memo; this is necessary because otherwise you
      // get an error if user does not terminate entry with ENTER key!
      edProtein.Lines.Append(Chr(13));
      // Check if protein entered is FASTA format or raw data
      if LeftStr(edProtein.Text, 1) = '>' then begin
        sFasta := RightStr(edProtein.Lines.Strings[0], Length(edProtein.Lines.Strings[0]) - 1);
        First := 1
      end
      else begin
        First := 0;
        sFasta := 'Some unknown protein';
      end;
      sProtein := '';
      // Assign Memo lines to an AnsiString variable
      for I := First to edProtein.Lines.Count do
        if edProtein.Lines.Strings[I] <> '' then
          sProtein += edProtein.Lines.Strings[I];
      sProtein := StringReplace(sProtein, ' ', '', [rfReplaceAll]);              // eliminate blanks (as used to separate codes in many files)
      bNewProtein := False;
    end;
    // Set all counters to 0
    iCount := 0; rMolWeight := 0;
    for I := 1 to 20 do begin
      aCounts[I] := 0;
      aMolWeights[I] := 0;
    end;
    for I := 1 to 7 do
      aClasses[I] := 0;
    for I := 1 to 5 do
      aVolumes[I] := 0;
    for I := 1 to 4 do
      aSideChains[I] := 0;
    for I := 1 to 2 do
      aPolarities[I] := 0;
    for I := 1 to 3 do begin
      aCharges[I] := 0;
      aHydropathies[I] := 0;
      aDietReqs[I]:= 0;
      aMetabolisms[I] := 0;
    end;
    I := 1; Mess := '';
    // Check if protein is valid (valid 1-letter resp. 3-letter amino acids codes)
    if (rbCode3.Checked) and (Length(sProtein) mod 3 <> 0) then
      Mess := 'Invalid 3-letter code format';
    while (I <= Length(sProtein)) and (Mess = '') do begin
      if rbCode1.Checked then begin
        Code := sProtein[I];
        if not ValidCode(Code, mSettingsCode1LC.Checked) then
          Mess := 'Invalid IUP/IUPAC code "' + Code + '" at position ' + IntToStr(I);
      end
      else begin
        Code3 := Copy(sProtein, I, 3);
        Code := Code3ToCode1(Code3, aAminoAcids, mSettingsCode3UC.Checked);
        if not ValidCode(Code, False) then
          Mess := 'Invalid 3-letter code "' + Code3 + '" at position ' + IntToStr(I);
      end;
      // If all codes are valid, proceed with the computing the counts
      if Mess = '' then begin
        Code := UpperCase(Code)[1];
        Inc(iCount); rMolWeight += aAminoAcids[Code].AAMolWeight;              // total (= protein) values
        case cobStatistics.ItemIndex of
          0: StatsAACounts(Code, aAminoAcids, aCounts, aMolWeights);
          1: StatsAAClasses(Code, aAminoAcids, aClasses);
          2: StatsAAVolumes(Code, aAminoAcids, aVolumes);
          3: StatsAAPolarities(Code, aAminoAcids, aPolarities, aSidechains);
          4: StatsAACharges(Code, aAminoAcids, aCharges, aSidechains);
          5: StatsAAHydropathies(Code, aAminoAcids, aHydropathies);
          6: StatsAADietRequirements(Code, aAminoAcids, aDietReqs);
          7: StatsAAMetabolisms(Code, aAminoAcids, aMetabolisms);
        end;
        if rbCode1.Checked then
          Inc(I)
        else
          I += 3;
      end;
    end;
    // Pass data to one of the 6 statistics forms and show this up
    // The form "called" will fill in the grid(s) and display the charts
    if Mess = '' then begin
      // Amino acids by number
      if cobStatistics.ItemIndex = 0 then begin
        fAACount.sFasta := sFasta;
        fAACount.iCount := iCount;
        fAACount.rMolWeight := rMolWeight;
        fAACount.aCounts := aCounts;
        fAACount.aMolWeights := aMolWeights;
        fAACount.ShowModal;
      end
      // Amino acids by structure (class)
      else if cobStatistics.ItemIndex = 1 then begin
        fAAClass.sFasta := sFasta;
        fAAClass.iCount := iCount;
        fAAClass.aCounts := aClasses;
        fAAClass.ShowModal;
      end
      // Amino acids by volume
      else if cobStatistics.ItemIndex = 2 then begin
        fAAVol.sFasta := sFasta;
        fAAVol.iCount := iCount;
        fAAVol.aCounts := aVolumes;
        fAAVol.ShowModal;
      end
      // Amino acids by polarity and sidechain specificity
      else if cobStatistics.ItemIndex = 3 then begin
        fAAPolarity.sFasta := sFasta;
        fAAPolarity.iCount := iCount;
        fAAPolarity.aCounts1 := aPolarities;
        fAAPolarity.aCounts2 := aSideChains;
        fAAPolarity.ShowModal;
      end
      // Amino acids by charge and sidechain specificity
      else if cobStatistics.ItemIndex = 4 then begin
        fAACharge.sFasta := sFasta;
        fAACharge.iCount := iCount;
        fAACharge.aCounts1 := aCharges;
        fAACharge.aCounts2 := aSideChains;
        fAACharge.ShowModal;
      end
      // Amino acids by hydropathy, diet-requirements resp. metabolism
      else begin
        fAAOthers.iClassification := cobStatistics.ItemIndex - 4;              // this variable tells the "called" form which statistics are actually used
        fAAOthers.sFasta := sFasta;
        fAAOthers.iCount := iCount;
        if cobStatistics.ItemIndex = 5 then
          fAAOthers.aCounts := aHydropathies
        else if cobStatistics.ItemIndex = 6 then
          fAAOthers.aCounts := aDietReqs
        else
          fAAOthers.aCounts := aMetabolisms;
        fAAOthers.ShowModal;
      end;
    end
    // Message if protein contains invalid code (note that B, X and Z are not accepted)
    else
      MessageDlg('Invalid data', Mess + '!', mtError, [mbOK], 0);
  end
  // Message if user didn't enter any protein
  else
    MessageDlg('Invalid data', 'Please, enter your protein data!', mtError, [mbOK], 0);
end;

{ Set bNewProtein variable to True if a new protein has been entered }

procedure TfAAStats.edProteinChange(Sender: TObject);

begin
  if edProtein.Text <> '' then
    bNewProtein := True;
end;

end.

