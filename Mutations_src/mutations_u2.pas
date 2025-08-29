{***********************************************}
{* Genetic code unit for Mutations application *}
{***********************************************}

unit mutations_u2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, Menus;

type
  TCodon = record
    Codon: string[3];
    AminoAcid: Char;
  end;
  TGeneticCode = array[1..64] of TCodon;
  {***********}
  { TfGenCode }
  {***********}
  TfGenCode = class(TForm)
    MainMenu1: TMainMenu;
    sgTable1: TStringGrid;
    sgTable2: TStringGrid;
    sgTable3: TStringGrid;
    sgTable4: TStringGrid;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

const
  AminoAcids3: array['A'..'Z'] of string = (
    'Ala', 'Asx', 'Cys', 'Asp', 'Glu', 'Phe', 'Gly', 'His', 'Ile', '???', 'Lys', 'Leu', 'Met',
    'Asn', '???', 'Pro', 'Gln', 'Arg', 'Ser', 'Thr', '???', 'Val', 'Trp', 'Xxx', 'Tyr', 'Glx'
  );

var
  fGenCode: TfGenCode;
  aGeneticCode: TGeneticCode;

function GetGeneticCode(Codon: string): Char;

implementation

{$R *.lfm}

{ Get 3-letters amino acid table value (for a given 1-letter amino acid table value ) }

function TableAminoAcid3(AminoAcid: string): string;

var
  AminoAcid3: string;

begin
  if AminoAcid = 'Start (M)' then
    AminoAcid3 := 'Start (Met)'
  else if AminoAcid = 'Stop!' then
    AminoAcid3 := 'Stop!'
  else
    AminoAcid3 := AminoAcids3[AminoAcid[1]];
  Result := AminoAcid3;
end;

{ Get standard genetic code for a given codon }

function GetGeneticCode(Codon: string): Char;

{ Amino acid is return as 1-letter code }

var
  I: Integer;
  GCode: Char;

begin
  GCode := '?';                                                                // "?" indicates invalid codon
  I := 1;
  while (GCode = '?') and (I <= 64) do begin
    if Codon = aGeneticCode[I].Codon then
      GCode := aGeneticCode[I].AminoAcid;
    Inc(I);
  end;
  Result := GCode;
end;

{***********}
{ TfGenCode }
{***********}

{ Application start: Fill-in 3-letter amino acid table values and create standard genetic code array }

procedure TfGenCode.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 1 to 16 do begin
    // Table fill-in
    if Trim(sgTable1.Cells[0, I]) = 'AUG' then
      sgTable1.Cells[2, I] := '  ' + TableAminoAcid3(Trim(sgTable1.Cells[1, I]))
    else
      sgTable1.Cells[2, I] := '      ' + TableAminoAcid3(Trim(sgTable1.Cells[1, I]));
    sgTable2.Cells[2, I] := '      ' + TableAminoAcid3(Trim(sgTable2.Cells[1, I]));
    sgTable3.Cells[2, I] := '      ' + TableAminoAcid3(Trim(sgTable3.Cells[1, I]));
    if (Trim(sgTable4.Cells[0, I]) = 'UAA') or (Trim(sgTable4.Cells[0, I]) = 'UAG') or (Trim(sgTable4.Cells[0, I]) = 'UGA') then
      sgTable4.Cells[2, I] := '     ' + TableAminoAcid3(Trim(sgTable4.Cells[1, I]))
    else
      sgTable4.Cells[2, I] := '      ' + TableAminoAcid3(Trim(sgTable4.Cells[1, I]));
    // Genetic code array creation
    J := I;
    aGeneticCode[J].Codon := Trim(sgTable1.Cells[0, I]);
    if aGeneticCode[J].Codon = 'AUG' then                                      // "AUG" also indicates start codon
      aGeneticCode[J].AminoAcid := 'M'
    else
      aGeneticCode[J].AminoAcid := Trim(sgTable1.Cells[1, I])[1];
    J += 16;
    aGeneticCode[J].Codon := Trim(sgTable2.Cells[0, I]);
    aGeneticCode[J].AminoAcid := Trim(sgTable2.Cells[1, I])[1];
    J += 16;
    aGeneticCode[J].Codon := Trim(sgTable3.Cells[0, I]);
    aGeneticCode[J].AminoAcid := Trim(sgTable3.Cells[1, I])[1];
    J += 16;
    aGeneticCode[J].Codon := Trim(sgTable4.Cells[0, I]);
    if (aGeneticCode[J].Codon = 'UAA') or (aGeneticCode[J].Codon = 'UAG') or (aGeneticCode[J].Codon = 'UGA') then  // stop codons with translation = "_"
      aGeneticCode[J].AminoAcid := '_'
    else
      aGeneticCode[J].AminoAcid := Trim(sgTable4.Cells[1, I])[1];
  end;
end;

{ Button "Close": Close the genetics code window }

procedure TfGenCode.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

