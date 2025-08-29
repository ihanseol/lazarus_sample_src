{*************************************}
{* Main unit for GetMyIP application *}
{*************************************}

unit getip;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, FpHttpClient;

type
  {*********}
  { TfGetIP }
  {*********}
  TfGetIP = class(TForm)
    mMenu: TMainMenu;
    mHelp: TMenuItem;
    mHelpAbout: TMenuItem;
    Label1: TLabel;
    edIP: TEdit;
    btGet: TButton;
    btExit: TButton;
    procedure mHelpAboutClick(Sender: TObject);
    procedure btGetClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);
  end;

var
  fGetIP: TfGetIP;

implementation

{$R *.lfm}

{ Ask IP Internet server for public IP address (per HTTP) }

function PublicIP(HTTPClient: TFPHTTPClient; ServiceURL: string): string;

var
  IP: string;

begin
  IP := '';
  try
    // Send HTTP request to Internet server
    IP := HTTPClient.Get(ServiceURL);
  except
    on E: Exception do
      // This catches any errors, incl. "unresolved host address" (e.g. if no Internet connection)
      IP := 'ERROR: ' + E.Message;
  end;
  Result := IP;
end;

{*********}
{ TfGetIP }
{*********}

{ Menu item "Help > About": Display application about }

procedure TfGetIP.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Determination of external (= public) IP address using the free IP service at ';
  S += 'ipinfo.io or (if this one fails) dynupdate.no-ip.com.' + LineEnding + LineEnding;
  S += 'Version 1.0, © allu, May 2023.';
  MessageDlg('About "GetMyIP"', S, mtInformation, [mbOK], 0);
end;

{ Button "Get it!" pushed: Get and display public IP address }

procedure TfGetIP.btGetClick(Sender: TObject);

const
  // Free IP servers URLs
  URLList: array[0..1] Of String = (
    'http://ipinfo.io/ip',
    'http://dynupdate.no-ip.com/ip.php'
  );

var
  I: Integer;
  IP, Err, S: string;
  lnHTTPClient: TFPHTTPClient;

begin
  edIP.Text := '';
  try
    lnHTTPClient := TFPHTTPClient.Create(nil);
    I := 0; IP := ''; Err := '';
    // Do HTTP request until IP address is returned (or all servers in the list have been queried)
    repeat
      S := PublicIP(lnHTTPClient, URLList[I]);
      // If there is no response, the function returns the catched error message, preceded by "ERROR: "
      if LeftStr(S, 7) <> 'ERROR: ' then
        IP := S
      else begin
        // Store error messages for all servers
        if Err <> '' then
          Err += LineEnding;
        Err += S;
      end;
      Inc(I);
    until (IP <> '') or (I = Length(URLList));
  finally
    lnHTTPClient.Free;
  end;
  // If one of the servers gave a response, display the IP address
  // Otherwise display the catched error message
  if IP <> '' then
    edIP.Text := IP
  else begin
    Err := StringReplace(Err, 'ERROR: ', '', [rfReplaceAll]);
    MessageDlg('Service connection error', Err, mtError, [mbOK], 0);
  end;
end;

{ Button "Exit" pushed: Exit application }

procedure TfGetIP.btExitClick(Sender: TObject);

begin
  Close;
end;

end.

