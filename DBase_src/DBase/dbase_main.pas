{***********************************}
{* Main unit for DBase application *}
{***********************************}

unit dbase_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, dbf, db;

type
  {*********}
  { TfDBase }
  {*********}
  TfDBase = class(TForm)
    StaticText2: TStaticText;
    edCustomers: TMemo;
    Label1: TLabel;
    edCountry: TEdit;
    btSearch: TButton;
    btExit: TButton;
    dbfCustomer: TDbf;
    procedure FormCreate(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  private
    N: Integer;
    sCountry, sCustomer: string;
  end;

var
  fDBase: TfDBase;

implementation

{$R *.lfm}

{*********}
{ TfDBase }
{*********}

{ Application start: Open dBASE file (CUSTOMER table) }

procedure TfDBase.FormCreate(Sender: TObject);

begin
  dbfCustomer.Open;
end;

{ Button "Search" clicked: Search for customers from given country }

procedure TfDBase.btSearchClick(Sender: TObject);

begin
  sCountry := edCountry.Text;
  if sCountry <> '' then begin
    edCustomers.Lines.Clear; N := 0;
    // Selection filter on table field COUNTRY
    dbfCustomer.FilterOptions := [foCaseInsensitive];
    dbfCustomer.Filter := 'COUNTRY=' + QuotedStr(sCountry);
    dbfCustomer.Filtered := True;
    // Read first record
    dbfCustomer.First;
    // If there was a record, continue reading until end of file
    while not dbfCustomer.EOF do begin
      sCustomer := dbfCustomer.FieldByName('NAME').AsString;
      sCustomer += ' (' + dbfCustomer.FieldByName('CITY').AsString + ')';
      edCustomers.Append(sCustomer);                                           // display customer info
      Inc(N);
      dbfCustomer.Next;                                                        // read next record
    end;
    // Display number of customers found
    if N = 0 then
      edCustomers.Append('No customers found for country = ' + sCountry)
    else
      edCustomers.Append(LineEnding + 'Number of customers in ' + sCountry + ': ' + IntToStr(N));
  end;
end;

{ Button "Exit" clicked: Close dBASE file and exit application }

procedure TfDBase.btExitClick(Sender: TObject);

begin
  dbfCustomer.Close;
  Close;
end;

end.

