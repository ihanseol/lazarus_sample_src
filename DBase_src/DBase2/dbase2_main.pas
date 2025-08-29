unit dbase2_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, db, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TfDBase2 }

  TfDBase2 = class(TForm)
    btSearch: TButton;
    btRecords: TButton;
    btTable: TButton;
    btIndex: TButton;
    btExit: TButton;
    dbfEmployee: TDbf;
    edID: TEdit;
    edData: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StaticText1: TStaticText;
    procedure btExitClick(Sender: TObject);
    procedure btIndexClick(Sender: TObject);
    procedure btRecordsClick(Sender: TObject);
    procedure btSearchClick(Sender: TObject);
    procedure btTableClick(Sender: TObject);
  private

  public

  end;

var
  fDBase2: TfDBase2;

implementation

{$R *.lfm}

{ TfDBase2 }

{ Button "Create table": Create table Employee.dbf }

procedure TfDBase2.btTableClick(Sender: TObject);

// All table properties, including the field definitions have been set in the TDbf object Properties sheet...

begin
  dbfEmployee.CreateTable;
end;

{ Button "Create index": Add primary key to table Employee.dbf }

procedure TfDBase2.btIndexClick(Sender: TObject);

begin
  dbfEmployee.Exclusive := True;
  dbfEmployee.Open;
  dbfEmployee.AddIndex('employee_id', 'ID', [ixPrimary]);
  dbfEmployee.Close;
  dbfEmployee.Exclusive := False;
end;

{ Button "Add records": Add some data to table Employee.dbf }

procedure TfDBase2.btRecordsClick(Sender: TObject);

begin
  dbfEmployee.Open;
  dbfEmployee.AppendRecord([100, 'Jones', 'Bob', 4400]);
  dbfEmployee.AppendRecord([110, 'Smith', 'Jessica', 4500]);
  dbfEmployee.AppendRecord([120, 'Lincoln', 'Edward', 3800]);
  dbfEmployee.AppendRecord([130, 'Lincoln', 'Melissa', 3500]);
  dbfEmployee.AppendRecord([140, 'Baxter', 'Tom', 5000]);
  dbfEmployee.Close;
end;

{ Button "Search database": Display data for employee with given ID }

procedure TfDBase2.btSearchClick(Sender: TObject);

begin
  if not dbfEmployee.Active then
    dbfEmployee.Open;
  edData.Text := '';
  if edID.Text <> '' then begin
    dbfEmployee.Filter := 'ID=' + edID.Text;
    dbfEmployee.Filtered := True;
    dbfEmployee.First;
    if dbfEmployee.EOF then begin
      edData.Text := 'Not found!';
    end
    else begin
      edData.Text := dbfEmployee.FieldByName('Lastname').AsString;
      edData.Text := edData.Text + ' ' + dbfEmployee.FieldByName('Firstname').AsString;
      edData.Text := edData.Text + ': ' + FloatToStr(dbfEmployee.FieldByName('Salary').AsFloat);
    end;
  end;
end;

{ Button "Exit": Exit application }

procedure TfDBase2.btExitClick(Sender: TObject);

begin
  if dbfEmployee.Active then
    dbfEmployee.Close;
  Close;
end;

end.

