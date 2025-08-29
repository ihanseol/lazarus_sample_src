{********************************************************}
{* Configuration unit for LANMsgServer and LANMsgClient *}
{********************************************************}

unit config;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  {**********}
  { TfConfig }
  {**********}
  TfConfig = class(TForm)
    Label1, Label2, Label3: TLabel;
    laUser: TLabel;
    edIP, edPort, edUser: TEdit;
    btSave: TButton;
    btCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    iPortSave: Word;
    sIPSave, sUserNameSave: string;
  public
    iServerPort: Word;
    sConfig, sServerIP, sUserName, sButton: string;
  end;

var
  fConfig: TfConfig;

implementation

{$R *.lfm}

{**********}
{ TfConfig }
{**********}

{ Form becoming active: Save actual IP, port (and user name) }

procedure TfConfig.FormActivate(Sender: TObject);

begin
  if edPort.Text = '' then
    iPortSave := 0
  else
    iPortSave := StrToInt(edPort.Text);
  sIPSave := edIP.Text;
  if sConfig = 'server' then begin
    // Hide user field in server configuration
    laUser.Visible := False;
    edUser.Visible := False;
  end
  else begin
    // Save user name (client configuration)
    laUser.Visible := True;
    edUser.Visible := True;
    sUserName := edUser.Text;
  end;
end;

{ Button "Save": Set new IP, port (and user) }

procedure TfConfig.btSaveClick(Sender: TObject);

var
  N1, N2, N3, N4, P1, P2, P3, C1, C2, C3, C4: Integer;
  SN1, SN2, SN3, SN4, Mess: string;
  OK: Boolean;

begin
  Mess := '';
  if edIP.Text = '' then
    Mess := 'Missing server IP!'
  else begin
    OK := True;
    sServerIP := edIP.Text;
    // Check if input is valid IP address
    if Length(sServerIP) < 7 then
      OK := False
    else begin
      P1 := Pos('.', sServerIP);
      P2 := Pos('.', Copy(sServerIP, P1 + 1, Length(sServerIP)));
      P3 := Pos('.', Copy(sServerIP, P2 + 1, Length(sServerIP)));
      if P1 * P2 * P3 = 0 then
        OK := False
      else begin
        SN1 := Copy(sServerIP, 1, P1 - 1);
        SN2 := Copy(sServerIP, P1 + 1, P2 - 1);
        SN3 := Copy(sServerIP, P2 + 1, P3 - 1);
        SN4 := Copy(sServerIP, P3 + 1, Length(sServerIP));
        Val(SN1, N1, C1); Val(SN2, N2, C2);
        Val(SN3, N3, C3); Val(SN4, N4, C4);
        if (C1 <> 0) or (C2 <> 0) or (C3 <> 0) or (C4 <> 0) then
          OK := False
        else begin
          if (N1 < 1) or (N1 > 254) or (N2 < 1) or (N2 > 254) or (N3 < 1) or (N3 > 254) or (N4 < 1) or (N4 > 254) then
            OK := False;
        end;
      end;
    end;
    if not OK then
      Mess := 'Invalid server IP!';
  end;
  if Mess <> '' then
    edIP.SetFocus;
  if Mess = '' then begin
    if edPort.Text = '' then
      Mess := 'Missing server port!'
    else begin
      iServerPort := StrToInt(edPort.Text);
      // Check if entry is valid port
      if (iServerPort < 1) or (iServerPort > 65534) then
        Mess := 'Invalid server port!'
      else if (sConfig = 'server') and (iserverPort <= 1023) then
        Mess := 'Please, use a port >= 1024...';                               // do not allow "system ports"
    end;
    if Mess <> '' then
      edPort.SetFocus
  end;
  if Mess = '' then begin
    if (sConfig = 'client') and (edUser.Text = '') then
      Mess := 'Missing user name!'
    else begin
      sUserName := edUser.Text;
      // Check user name (no spaces)
      P1 := Pos(' ', sUserName);
      if P1 > 0 then
        Mess := 'Invalid user name!'
    end;
    if Mess <> '' then
      edUser.SetFocus;
  end;
  if Mess = '' then begin
    // If all input is ok, close the window
    sButton := 'save';                                                         // this tells the "calling form" that settings have been changed
    Close;
  end
  else
    MessageDlg('Configuration error', Mess, mtError, [mbOK], 0);
end;

{ Button "Cancel": Restore initial IP and port }

procedure TfConfig.btCancelClick(Sender: TObject);

begin
  iServerPort := iPortSave;
  sServerIP := sIPSave;
  if sConfig = 'client' then
    sUserName := sUserNameSave;
  sButton := 'cancel';                                                         // this tells the "calling form" that settings have NOT been changed
  Close;
end;

end.

