{************************************************}
{* Main unit for WinFileAssociation application *}
{************************************************}

unit wfileassoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus, ExtCtrls, LCLIntf, Registry, FileAssoc;

type
  {**************}
  { TfWFileAssoc }
  {**************}
  TfWFileAssoc = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mAdvanced, mAdvancedParams: TMenuItem;
    mHelp, mHelpReadMe, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    Memo1: TMemo;
    Label1, Label2, Label3, Label4, Label5: TLabel;
    Label6, Label7, Label8, Label9, Label10: TLabel;
    edExt, edAssociation: TEdit;
    edNewAssociation, edFileDescription: TEdit;
    edAppName, edAppExecutable, edAppParams: TEdit;
    cbAllUsers: TCheckBox;
    edWarning: TEdit;
    imExtIcon, imAppIcon: TImage;
    btCheck: TButton;
    btCreate: TButton;
    btExtIcon, btAppIcon, btAppExe: TButton;
    dlgOpen: TOpenDialog;
    faFileAssoc: TFileAssociation;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mAdvancedParamsClick(Sender: TObject);
    procedure mHelpReadMeClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure btCheckClick(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure btExtIconClick(Sender: TObject);
    procedure btAppIconClick(Sender: TObject);
    procedure btAppExeClick(Sender: TObject);
    procedure edExtChange(Sender: TObject);
  private
    sExtension, sAssociation, sExtIconPath, sAppIconPath, sDir: string;
  end;

var
  fWFileAssoc: TfWFileAssoc;

implementation

{$R *.lfm}

{**************}
{ TfWFileAssoc }
{**************}

{ Application start: Initialisation }

procedure TfWFileAssoc.FormCreate(Sender: TObject);

begin
  sDir := 'C:\';
end;

{ Menu item "File > Exit": Exit application }

procedure TfWFileAssoc.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu item "Advanced > Enable advanced settings": Give user access to advanced settings edit fields }

procedure TfWFileAssoc.mAdvancedParamsClick(Sender: TObject);

begin
  if mAdvancedParams.Checked then begin
    mAdvancedParams.Checked := False;
  end
  else begin
    mAdvancedParams.Checked := True;
    MessageDlg('Advanced settings', 'This option should only be enabled by advanced users and when really needed!', mtWarning, [mbOK], 0);
  end;
  edNewAssociation.ReadOnly := not mAdvancedParams.Checked;
  edAppParams.ReadOnly := not mAdvancedParams.Checked;
end;

{ Menu item "Help > Read me first": Start text editor with "ReadMeFirst.txt" displayed }

procedure TfWFileAssoc.mHelpReadMeClick(Sender: TObject);

begin
  OpenDocument('ReadMeFirst.txt');
end;

{ Menu item "Help > About": Display application about }

procedure TfWFileAssoc.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Microsoft Windows file associations.' + LineEnding;
  S += 'Association of a given file extension with a given application, with choice of custom file type description and icon' + LineEnding + LineEnding;
  S += 'Version 1.0.1, © allu, January-August 2021.';
  MessageDlg('About "WinFileAssociation"', S, mtInformation, [mbOK], 0);
end;

{ Button "Check": Check registry for existing file association }

procedure TfWFileAssoc.btCheckClick(Sender: TObject);

var
  Registry: TRegistry;

begin
  sExtension := LowerCase(edExt.Text);
  if sExtension <> '' then begin
    if LeftStr(sExtension, 1) <> '.' then
      sExtension := '.' + sExtension;
    edExt.Text := sExtension;
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CLASSES_ROOT;
      if Registry.OpenKeyReadOnly(sExtension) then begin
        sAssociation := Registry.ReadString('');
        if sAssociation <> '' then
          // File extension has an association
          edAssociation.Text := sAssociation
        else
          // File extension is registered but hasn't an association
          edAssociation.Text := '--not associated--';
      end
      else
        // File extension is not registered (thus, of course, hasn't an association)
        edAssociation.Text := '--not registered--';
    finally
      edNewAssociation.SetFocus;
      btCreate.Enabled := True;                                                // after check has been done, give user access to "Create" button
      Registry.Free;
    end;
    // Clear all edit fields
    edNewAssociation.Text := ''; imExtIcon.Picture.Clear; sExtIconPath := '';
    edAppName.Text := ''; imAppIcon.Picture.Clear; sAppIconPath := '';
    edFileDescription.Text := ''; edAppExecutable.Text := ''; edAppParams.Text := '%1';
    if mAdvancedParams.Checked then
      edNewAssociation.SetFocus
    else
      edAppName.SetFocus;
  end;
end;

{ Button "Create": Create file association }

procedure TfWFileAssoc.btCreateClick(Sender: TObject);

var
  Ret, I, P: Cardinal;
  App, Ext, ExtName, DefaultAssociation, Mess, S: string;
  OK: Boolean;
  Registry: TRegistry;

begin
  Mess := '';
  if edAppName.Text = '' then begin
    // Application name is mandatory
    Mess := 'You must enter an application name';
    edAppName.SetFocus;
  end
  else begin
    faFileAssoc.ApplicationName := edAppName.Text;
    Ext := UpperCase(sExtension);
    if LeftStr(Ext, 1) = '.' then
      Delete(Ext, 1, 1);
    // Default association registry key: appname.AssocFile.EXT
    DefaultAssociation := faFileAssoc.ApplicationName + '.AssocFile.' + Ext;
    DefaultAssociation := StringReplace(DefaultAssociation, ' ', '', [rfReplaceAll]);
    OK := True;
    if (not mAdvancedParams.Checked) or (mAdvancedParams.Checked and (edNewAssociation.Text = '')) then begin
      // Use default key
      edNewAssociation.Text := DefaultAssociation;
      ExtName := Ext;
    end
    else begin
      // Use custom key (must have format: appname.AssocFile.something)
      App := StringReplace(faFileAssoc.ApplicationName, ' ', '', [rfReplaceAll]); S := edNewAssociation.Text;
      P := Pos('.', S);
      if (P = 0) or (P = Length(S)) then
        OK := False
      else if LeftStr(S, P - 1) <> App then
        OK := False
      else begin
        Delete(S, 1, P);
        P := Pos('.', S);
        if (P = 0) or (P = Length(S)) then
          OK := False
        else if LeftStr(S, P - 1) <> 'AssocFile' then
          OK := False
        else begin
          Delete(S, 1, P);
          ExtName := S;
          for I := 1 to Length(S) do begin
            if (Copy(S, I, 1) = ' ') or (Copy(S, I, 1) = '.') then
              OK := False;
          end;
        end;
      end;
    end;
    if (not OK) and (Mess = '') then begin
      // Association key entered not supported by application
      // (may not be able to determine values to set for TFileAssociation object)
      Mess := 'AssocError';
      edNewAssociation.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if edAppExecutable.Text = '' then begin
      // Application executable is mandatory
      Mess := 'You must enter an application executable';
      edAppExecutable.SetFocus;
    end;
  end;
  if Mess = '' then begin
    if edFileDescription.Text = '' then begin
      // If user didn't enter a file description, use a default
      if ExtName = Ext then
        // For appname.AssocFile.EXT, use: EXT file
        edFileDescription.Text := Ext + ' file'
      else begin
        // For appname.AssocFile.something, use this something, replacing '_' by spaces
        // (allows for example auto-file-description "config file" from key "appname.AssocFile.config_file")
        edFileDescription.Text := StringReplace(ExtName, '_', ' ', [rfReplaceAll]);
        ExtName := StringReplace(ExtName, ' ', '', [rfReplaceAll]);
      end;
    end;
    if edAppParams.Text = '' then begin
      // Application parameters are mandatory (is nearly always: %1)
      MessageDlg('Missing data"', 'No application parameter specified - assuming: "%1"!', mtWarning, [mbOK], 0);
      edAppParams.Text := '%1';
    end;
    // Fill in the TFileAssociation object's properties
    faFileAssoc.Extension := sExtension;                                       // will give name of first registry key: sExtension
    faFileAssoc.ExtensionName := ExtName;                                      // will give second key: appname.FileAssoc.ExtName
    if sExtIconPath <> '' then
      // File type icon is not mandatory, but should always be set
      faFileAssoc.ExtensionIcon := '"' + sExtIconPath + '"';
    // Action to be performed, when file of this extension is double-clicked
    faFileAssoc.ActionName := 'Open';                                          // application always sets action = Open
    faFileAssoc.Action := '"' + edAppExecutable.Text + '"' + ' ' + '"' + edAppParams.Text + '"';  // path to the application executable
    if sAppIconPath <> '' then
      faFileAssoc.ActionIcon := '"' + sAppIconPath + '"';                      // may set application icon here (usage?)
    faFileAssoc.RegisterForAllUsers := cbAllUsers.Checked;                     // register for all or current user
    OK := True;
    if LeftStr(edAssociation.Text, 5) <> '--not' then begin
      // If this extension already has an association, ask user confirmation before overwriting it
      OK := False;
      S := 'There is already a file association for extension ' + sExtension + '. Do you really want to change it ?';
      Ret := MessageDlg('Registry update', S, mtWarning, [mbYes, mbNo], 0, mbNo);
      if Ret = mrYes then
        OK := True;
    end;
    if OK then begin
      // Create file association (write registry keys)
      try
        faFileAssoc.Execute;                                                     // apply file association registry update
        faFileAssoc.ClearIconCache;                                              // rebuild icons
        // As I did not see a way to set the file description with the TFileAssociation object,
        // I set it here, by directly accessing the registry (via a TRegistry object)
        Registry := TRegistry.Create;
        Registry.RootKey := HKEY_CLASSES_ROOT;
        Registry.OpenKey(edNewAssociation.Text, True);
        Registry.WriteString('', edFileDescription.Text);
        Registry.Free;
        // Successful file association creation
        MessageDlg('Registry update', 'The new file association for ' + sExtension + ' has been successfully created!', mtInformation, [mbOK], 0);
      except
        // Failure of file association creation (may arrive because Administrator priviledges are needed to do registration for all users)
        MessageDlg('Registry error', 'The new file association for ' + sExtension + ' could not be created!?', mtError, [mbOK], 0);
      end;
    end;
  end;
  if Mess <> '' then begin
    // Error message in the case of invalid user input data
    if Mess = 'AssocError' then
      MessageDlg('Program error', 'File association entered not supported by this application!', mtError, [mbOK], 0)
    else
      MessageDlg('Data error', Mess + '!', mtError, [mbOK], 0);
  end;
end;

{ Button "Browse" (extension icon): Browse for icon file }

procedure TfWFileAssoc.btExtIconClick(Sender: TObject);

begin
  dlgOpen.InitialDir := sDir;
  dlgOpen.FileName := '';
  dlgOpen.Title := 'Open icon file';
  dlgOpen.Filter := 'Icon files|*.ico|All files|*.*';
  dlgOpen.FilterIndex := 1;
  if dlgOpen.Execute then begin
    sExtIconPath := dlgOpen.Filename;
    sExtIconPath := StringReplace(sExtIconPath, '"', '', [rfReplaceAll]);
    sDir := ExtractFileDir(sExtIconPath);
    imExtIcon.Picture.LoadFromFile(sExtIconPath);
  end;
end;

{ Button "Browse" (application icon): Browse for icon file }

procedure TfWFileAssoc.btAppIconClick(Sender: TObject);

begin
  dlgOpen.InitialDir := sDir;
  dlgOpen.FileName := '';
  dlgOpen.Title := 'Open icon file';
  dlgOpen.Filter := 'Icon files|*.ico|All files|*.*';
  dlgOpen.FilterIndex := 1;
  if dlgOpen.Execute then begin
    sAppIconPath := dlgOpen.Filename;
    sAppIconPath := StringReplace(sAppIconPath, '"', '', [rfReplaceAll]);
    sDir := ExtractFileDir(sAppIconPath);
    imAppIcon.Picture.LoadFromFile(sAppIconPath);
  end;
end;

{ Button "Browse" (executable): Browse for application executable file }

procedure TfWFileAssoc.btAppExeClick(Sender: TObject);

var
  ExePath: string;

begin
  dlgOpen.InitialDir := sDir;
  dlgOpen.FileName := '';
  dlgOpen.Title := 'Open application executable';
  dlgOpen.Filter := 'Executables|*.exe|All files|*.*';
  dlgOpen.FilterIndex := 1;
  if dlgOpen.Execute then begin
    ExePath := dlgOpen.Filename;
    ExePath := StringReplace(ExePath, '"', '', [rfReplaceAll]);
    sDir := ExtractFileDir(ExePath);
    edAppExecutable.Text := ExePath;
  end;
end;

{ User modification of "extension" edit field: Disable "Create" button }

procedure TfWFileAssoc.edExtChange(Sender: TObject);

begin
  btCreate.Enabled := False;                                                   // button re-enabled only when "Check" is pushed
end;

end.

