{************************************}
{* Main unit for FoxPro application *}
{************************************}

unit foxpro_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, dbf, db;

type
  {**********}
  { TfFoxPro }
  {**********}
  TfFoxPro = class(TForm)
    StaticText2: TStaticText;
    edProducts: TMemo;
    Label1: TLabel;
    edSupplier: TEdit;
    btSearch: TButton;
    btExit: TButton;
    dbfProducts: TDbf;
    procedure FormCreate(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  private
    iSupplier, N: Integer;
    sProduct: string;
  end;

var
  fFoxPro: TfFoxPro;

implementation

{$R *.lfm}

{**********}
{ TfFoxPro }
{**********}

{ Application start: Open FoxPro file (PRODUCTS table) }

procedure TfFoxPro.FormCreate(Sender: TObject);

begin
  dbfProducts.Open;
  dbfProducts.OpenIndexFile('products.cdx');                                   // you must also open the index file!
end;

{ Button "Search" clicked: Search for products from given supplier }

procedure TfFoxPro.btSearchClick(Sender: TObject);

begin
  if edSupplier.Text <> '' then begin
    iSupplier := StrToInt(edSupplier.Text);
    edProducts.Lines.Clear; N := 0;
    // Selection filter on table field SUPPLIERID
    dbfProducts.Filter := 'supplierid=' + IntToStr(iSupplier);
    dbfProducts.Filtered := True;
    // Read first record
    dbfProducts.First;
    // If there was a record, continue reading until end of file
    while not dbfProducts.EOF do begin
      sProduct := dbfProducts.FieldByName('productnam').AsString;
      edProducts.Append(sProduct);                                             // display product name
      Inc(N);
      dbfProducts.Next;                                                        // read next record
    end;
    // Display number of products found
    if N = 0 then
      edProducts.Append('No products found for supplier = ' + IntToStr(iSupplier))
    else
      edProducts.Append(LineEnding + 'Number of products by supplier (' + IntToStr(iSupplier) + '): ' + IntToStr(N));
  end;
end;

{ Button "Exit" clicked: Close FoxPro file and exit application }

procedure TfFoxPro.btExitClick(Sender: TObject);

begin
  dbfProducts.CloseIndexFile('products.cdx');                                  // close the index file
  dbfProducts.Close;
  Close;
end;

end.

