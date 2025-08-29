{**************************************************}
{* Zodiac data origins unit of Zodiac application *}
{**************************************************}

unit zodiac_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLIntf;

type
  {********}
  { TfData }
  {********}
  TfData = class(TForm)
    Memo1: TMemo;
    StaticText1, StaticText2, StaticText3, StaticText4, StaticText5: TStaticText;
    stLink1: TLabel;
    stLink2: TLabel;
    stLink3: TLabel;
    btClose: TButton;
    procedure stLink1Click(Sender: TObject);
    procedure stLink2Click(Sender: TObject);
    procedure stLink3Click(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
  end;

var
  fData: TfData;

implementation

{$R *.lfm}

{********}
{ TfData }
{********}


{ Mouse click on link labels: Point webbrowser to corr. URL }

procedure TfData.stLink1Click(Sender: TObject);

begin
  OpenDocument(stLink1.Caption);
end;

procedure TfData.stLink2Click(Sender: TObject);

begin
  OpenDocument(stLink2.Caption);
end;

procedure TfData.stLink3Click(Sender: TObject);

begin
  OpenDocument(stLink3.Caption);
end;

{ Button "Close": Close data origin window }

procedure TfData.btCloseClick(Sender: TObject);

begin
  Close;
end;

end.

