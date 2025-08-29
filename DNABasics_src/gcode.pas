{***********************************************}
{* Genetic code unit for DNABasics application *}
{***********************************************}

unit gcode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls;

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
    sgTable1, sgTable2, sgTable3, sgTable4: TStringGrid;
    btClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  public
   aGeneticCode: TGeneticCode;
  end;

const
  AminoAcids3: array['A'..'Z'] of string = (
    'Ala', 'Asx', 'Cys', 'Asp', 'Glu', 'Phe', 'Gly', 'His', 'Ile', '???', 'Lys', 'Leu', 'Met',
    'Asn', '???', 'Pro', 'Gln', 'Arg', 'Ser', 'Thr', '???', 'Val', 'Trp', 'Xxx', 'Tyr', 'Glx'
  );

var
  fGenCode: TfGenCode;

// Public functions (used by main unit)

function GetAminoAcid3(AminoAcid: Char): string;
function GetProtein3(Protein: AnsiString): AnsiString;

implementation

{$R *.lfm}

{ Get amino acid 3-letters code (for string-grid tables fill-in) }

function GetAminoAcid3FromTable(AminoAcid: string): string;

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

{ Transform amino acid 1-letter code to amino acid 3-letters code }

function GetAminoAcid3(AminoAcid: Char): string;

var
  AminoAcid3: string;

begin
  if AminoAcid = 'M' then                                                      // translation start (Met)
    AminoAcid3 := 'Met'
  else if AminoAcid = '_' then                                                 // translation end
    AminoAcid3 := '___'
  else
    AminoAcid3 := GetAminoAcid3FromTable(AminoAcid);
  Result := AminoAcid3;
end;

{ Transform protein 1-letter code to protein 3-letters code }

function GetProtein3(Protein: AnsiString): AnsiString;

var
  I: Integer;
  Protein3: AnsiString;

begin
  Protein3 := '';
  for I := 1 to Length(Protein) do
    Protein3 += GetAminoAcid3(Protein[I]);                                     // transform amino acid 1-letter code to amino acid 3-letters code
  Result := Protein3;
end;

{***********}
{ TfGenCode }
{***********}

{ Application start: Complete the genetic code tables }

procedure TfGenCode.FormCreate(Sender: TObject);

var
  I, J: Integer;

begin
  for I := 1 to 16 do begin
    if Trim(sgTable1.Cells[0, I]) = 'AUG' then
      sgTable1.Cells[2, I] := '  ' + GetAminoAcid3FromTable(Trim(sgTable1.Cells[1, I]))
    else
      sgTable1.Cells[2, I] := '      ' + GetAminoAcid3FromTable(Trim(sgTable1.Cells[1, I]));
    sgTable2.Cells[2, I] := '      ' + GetAminoAcid3FromTable(Trim(sgTable2.Cells[1, I]));
    sgTable3.Cells[2, I] := '      ' + GetAminoAcid3FromTable(Trim(sgTable3.Cells[1, I]));
    sgTable4.Cells[2, I] := '      ' + GetAminoAcid3FromTable(Trim(sgTable4.Cells[1, I]));
    J := I;
    aGeneticCode[J].Codon := Trim(sgTable1.Cells[0, I]);
    if aGeneticCode[J].Codon = 'AUG' then
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
    if (aGeneticCode[J].Codon = 'UAA') or (aGeneticCode[J].Codon = 'UAG') or (aGeneticCode[J].Codon = 'UGA') then
      aGeneticCode[J].AminoAcid := '_'
    else
      aGeneticCode[J].AminoAcid := Trim(sgTable4.Cells[1, I])[1];
  end;
end;

{ Button "Close" pushed: Close "Genetic code" window }

procedure TfGenCode.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

