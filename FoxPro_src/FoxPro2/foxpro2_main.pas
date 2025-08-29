{*************************************}
{* Main unit for FoxPro2 application *}
{*************************************}

unit foxpro2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, dbf, db;

type
  {***********}
  { TfFoxPro2 }
  {***********}
  TfFoxPro2 = class(TForm)
    StaticText2: TStaticText;
    Label2, Label3, Label4, Label5, Label6, Label7: TLabel;
    laPhysical, laLogical, laProduct: TLabel;
    edID, edName, edCategory: TEdit;
    edSupplier, edQuantity, edPrice: TEdit;
    Shape1: TShape;
    btSearch: TButton;
    btAdd: TButton;
    btModify: TButton;
    btDelete: TButton;
    btExit: TButton;
    dbfProducts: TDbf;
    procedure FormCreate(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btModifyClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  private
    iProductID, iCategory, iSupplier, iLastRec: Integer;
    rPrice: Real;
    sProductName, sQuantity, sPrice: string;
  end;

var
  fFoxPro2: TfFoxPro2;

implementation

{$R *.lfm}

procedure RecsDisplay(LastRec: Integer);

begin
  fFoxPro2.dbfProducts.Filtered := False;
  fFoxPro2.laPhysical.Caption := 'Physical records = ' + IntToStr(fFoxPro2.dbfProducts.PhysicalRecordCount);
  fFoxPro2.laLogical.Caption  := 'Logical records = ' + IntToStr(fFoxPro2.dbfProducts.ExactRecordCount);
  fFoxPro2.laProduct.Caption  := 'New product ID = ' + IntToStr(LastRec + 1);
end;

{***********}
{ TfFoxPro2 }
{***********}

{ Application start: Open FoxPro file (PRODUCTS table) }

procedure TfFoxPro2.FormCreate(Sender: TObject);

begin
  dbfProducts.Open;
  dbfProducts.OpenIndexFile('products.cdx');                                   // you must also open the index file
  // Read last record in order to get next product ID for inserting new records
  dbfProducts.Filtered := False;
  dbfProducts.Last;
  iLastRec := dbfProducts.FieldByName('productid').AsInteger;
  RecsDisplay(iLastRec);
end;

{ Button "Search" clicked: Search for product with given ID }

procedure TfFoxPro2.btSearchClick(Sender: TObject);

begin
  if edID.Text <> '' then begin
    edName.Text := ''; edCategory.Text := ''; edSupplier.Text := '';
    edQuantity.Text := ''; edPrice.Text := '';
    iProductID := StrToInt(edID.Text);
    // Selection filter on table field PRODUCTID
    dbfProducts.Filter := 'productid=' + IntToStr(iProductID);
    dbfProducts.Filtered := True;
    // Read record
    dbfProducts.First;
    // If there is a record, display product data
    if not dbfProducts.EOF then begin
      sProductName := dbfProducts.FieldByName('productnam').AsString;
      iCategory := dbfProducts.FieldByName('categoryid').AsInteger;
      iSupplier := dbfProducts.FieldByName('supplierid').AsInteger;
      sQuantity := dbfProducts.FieldByName('quantitype').AsString;
      rPrice := dbfProducts.FieldByName('unitprice').AsCurrency;
      edName.Text := sProductName;
      edCategory.Text := IntToStr(iCategory);
      edSupplier.Text := IntToStr(iSupplier);
      edQuantity.Text := sQuantity;
      edPrice.Text := FloatToStrF(rPrice,ffCurrency, 0, 2);
      edPrice.Text := StringReplace(edPrice.Text, '€', '$', []);
    end
    else
      MessageDlg('Products table', 'Product not found!', mtWarning, [mbOK], 0);
  end
  else
    MessageDlg('Products table', 'Missing product ID!', mtError, [mbOK], 0);
end;

{ Button "Add" clicked: Add new product }

procedure TfFoxPro2.btAddClick(Sender: TObject);

begin
  if (edName.Text <> '') and (edCategory.Text <> '') and (edSupplier.Text <> '') and
      (edQuantity.Text <> '') and (edPrice.Text <> '') then begin
    if (edID.Text <> '') then begin
      MessageDlg('Products table', 'Product ID will be set by application!', mtWarning, [mbOK], 0);
      edID.Text := '';
    end;
    sProductName := edName.Text; iCategory := StrToInt(edCategory.Text); iSupplier := StrToInt(edSupplier.Text);
    sQuantity := edQuantity.Text;
    sPrice := StringReplace(edPrice.Text, ' $', '', []);
    rPrice := StrToFloat(sPrice);
    Inc(iLastRec); edID.Text := IntToStr(iLastRec);
    dbfProducts.AppendRecord([iLastRec, sProductName, iCategory, iSupplier, sQuantity, rPrice]);
    RecsDisplay(iLastRec);
  end
  else
    MessageDlg('Products table', 'Missing product data!', mtError, [mbOK], 0);
end;

{ Button "Modify" clicked: Modify existing product }

procedure TfFoxPro2.btModifyClick(Sender: TObject);

begin
  if edID.Text <> '' then begin
    if (edName.Text <> '') and (edCategory.Text <> '') and (edSupplier.Text <> '') and
      (edQuantity.Text <> '') and (edPrice.Text <> '') then begin
      iProductID := StrToInt(edID.Text);
      // Selection filter on table field PRODUCTID
      dbfProducts.Filter := 'productid=' + IntToStr(iProductID);
      dbfProducts.Filtered := True;
      // Check if record exists
      sProductName := dbfProducts.FieldByName('productnam').AsString;
      if not dbfProducts.EOF then begin
        // Update the record
        dbfProducts.Edit;
        dbfProducts.FieldByName('productid').Value  := StrToInt(edID.Text);
        dbfProducts.FieldByName('productnam').Value := edName.Text;
        dbfProducts.FieldByName('categoryid').Value := StrToInt(edCategory.Text);
        dbfProducts.FieldByName('supplierid').Value := StrToInt(edSupplier.Text);
        dbfProducts.FieldByName('quantitype').Value := edQuantity.Text;
        dbfProducts.FieldByName('unitprice').Value  := StrToFloat(StringReplace(edPrice.Text, ' $', '', []));
        dbfProducts.UpdateRecord;
        dbfProducts.Post;
        RecsDisplay(iLastRec);
      end
      else
        MessageDlg('Products table', 'Invalid product ID!', mtError, [mbOK], 0);
    end
    else
      MessageDlg('Products table', 'Missing product data!', mtError, [mbOK], 0);
  end
  else
    MessageDlg('Products table', 'Missing product ID!', mtError, [mbOK], 0);
end;

{ Button "Delete" clicked: Delete existing product }

procedure TfFoxPro2.btDeleteClick(Sender: TObject);

begin
  if edID.Text <> '' then begin
    edName.Text := ''; edCategory.Text := ''; edSupplier.Text := '';
    edQuantity.Text := ''; edPrice.Text := '';
    iProductID := StrToInt(edID.Text);
    // Selection filter on table field PRODUCTID
    dbfProducts.Filter := 'productid=' + IntToStr(iProductID);
    dbfProducts.Filtered := True;
    // Check if record exists
    sProductName := dbfProducts.FieldByName('productnam').AsString;
    if not dbfProducts.EOF then begin
      // Delete the record
      dbfProducts.Delete;
      RecsDisplay(iLastRec);
    end
    else
      MessageDlg('Products table', 'Invalid product ID!', mtError, [mbOK], 0);
  end
  else
    MessageDlg('Products table', 'Missing product ID!', mtError, [mbOK], 0);
end;

{ Button "Exit" clicked: Close FoxPro file and exit application }

procedure TfFoxPro2.btExitClick(Sender: TObject);

begin
  dbfProducts.CloseIndexFile('products.cdx');
  dbfProducts.Close;
  Close;
end;

end.

