{***************************************************}
{* New quiz creation unit for FlagQuiz application *}
{***************************************************}

unit fqznew;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LazUTF8;

type
  TFlags = array[1..200] of record
    CountryCode, Continent: string[2];
    CountryNameEN, CountryNameDE: string;
    FlagDone: Boolean;
  end;
  {**********}
  { TfFQZNew }
  {**********}
  TfFQZNew = class(TForm)
    Memo1: TMemo;
    Label1, Label2, Label4, Label5: TLabel;
    edQuizTitleEN, edQuizTitleDE, edTemplateFile: TEdit;
    lbTemplateCountries, lbNewCountries: TListBox;
    btBrowse: TButton;
    btMove, btRemove: TButton;
    btMoveAll, btRemoveAll: TButton;
    btSave, btCancel: TButton;
    dlgOpenFlags: TOpenDialog;
    dlgSaveFlags: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btMoveClick(Sender: TObject);
    procedure btRemoveClick(Sender: TObject);
    procedure btMoveAllClick(Sender: TObject);
    procedure btRemoveAllClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    sSourcePath, sDestPath, sWorldPath: string;
  public
    sProgPath, sFlagsPath, sQuizPath, sQuizLang: string;
  end;

var
  fFQZNew: TfFQZNew;

// Global procedure (also used by main unit)

procedure ReadFlags(Dir, FileName: string; out NFlags: Integer; out TitleEN, TitleDE: string; out Flags: TFlags);

implementation

{$R *.lfm}

{ Fill the template listbox with country names from selected .fqz file }

procedure FillTemplate(Dir, FileName, Language: string);

var
  P: Integer;
  NameEN, NameDE, S: string;
  FlagsFile: Text;

begin
  FileName := Dir + '/' + FileName; DoDirSeparators(FileName);
  Assign(FlagsFile, FileName); Reset(FlagsFile);                                    // open the flag file to retrieve country names
  fFQZNew.lbTemplateCountries.Items.Clear; fFQZNew.lbNewCountries.Items.Clear;
  fFQZNew.edQuizTitleEN.Text := ''; fFQZNew.edQuizTitleDE.Text := '';
  // Process .fqz file lines
  while not EoF(FlagsFile) do begin
    Readln(FlagsFile, S);
    if (S <> '') and (LeftStr(S, 2) <> '**') then begin                             // ignore empty lines and quiz title
      S := UTF8Copy(S, 7, UTF8Length(S));                                           // field 3 = country names (English, German)
      P := UTF8Pos(';', S);                                                         // English/German names separator (;)
      if P = 0 then begin
        // Only one country name given: both names set to this one
        NameEN := S; NameDE := NameEN;
      end
      else begin
        // Two country names given: extract different English and German name
        NameEN := UTF8Copy(S, 1, P - 1); NameDE := UTF8Copy(S, P + 1, UTF8Length(S));
      end;
      // Add country name to listbox (using actually selected language)
      if Language = 'eng' then
        fFQZNew.lbTemplateCountries.Items.AddText(NameEN)
      else
        fFQZNew.lbTemplateCountries.Items.AddText(NameDE);
    end;
  end;
  Close(FlagsFile);
end;

{ Read flags info from .fqz file }

procedure ReadFlags(Dir, FileName: string; out NFlags: Integer; out TitleEN, TitleDE: string; out Flags: TFlags);

var
  FlagsFile: Text;
  P: Integer;
  S: string;

begin
  NFlags := 0;
  FileName := Dir + '/' + FileName; DoDirSeparators(FileName);
  Assign(FlagsFile, FileName); Reset(FlagsFile);
  // Process file lines
  while not EoF(FlagsFile) do begin
    Readln(FlagsFile, S);
    if S <> '' then begin                                                           // ignore empty lines
      if LeftStr(S, 2) = '**' then begin
        // Line containing quiz title
        S := UTF8Copy(S, 7, UTF8Length(S));
        P := UTF8Pos(';', S);                                                       // English/German title separator (;)
        if P = 0 then begin
          // Only one country name given: use for both languages
          TitleEN := S;
          TitleDE := S;
        end
        else begin
          // Two country names given: extract different English and German name
          TitleEN := UTF8Copy(S, 1, P - 1);
          TitleDE := UTF8Copy(S, P + 1, UTF8Length(S));
        end;
      end
      else begin
        // Line containing flag info
        Inc(NFlags);                                                                // increment number of flags in this set
        Flags[NFlags].CountryCode := UTF8Copy(S, 1, 2);                             // filed 1 = country code
        Flags[NFlags].Continent := UTF8Copy(S, 4, 2);                               // field 2 = continent (not used, in fact)
        S := UTF8Copy(S, 7, UTF8Length(S));                                         // field 3 = country names (English, German)
        P := UTF8Pos(';', S);                                                       // English/German names separator (;)
        if P = 0 then begin
          // Only one country name given: use for both languages
          Flags[NFlags].CountryNameEN := S;
          Flags[NFlags].CountryNameDE := S;
        end
        else begin
          // Two country names given: extract different English and German name
          Flags[NFlags].CountryNameEN := UTF8Copy(S, 1, P - 1);
          Flags[NFlags].CountryNameDE := UTF8Copy(S, P + 1, UTF8Length(S));
        end;
        // Set this flag as 'not yet done in actual quiz'
        Flags[NFlags].FlagDone := False;
      end;
    end;
  end;
  Close(FlagsFile);
end;

{ Create a new .fqz file (with countries of the "New quiz" listbox) }

procedure WriteFlagFile(WorldDir, QuizDir, QuizFileName, QuizTitle: string; var Countries: TListBox);

var
  N, I, J, JX: Integer;
  WorldFileName, Title1, Title2, S: string;
  Flags: TFlags;
  OutFile: Text;

begin
  // Information needed to fill all fields of the .fqz file lines extrcted from world.fqz (containing all world countries)
  WorldFileName := 'world.fqz';
  ReadFlags(WorldDir, WorldFileName, N, Title1, Title2, Flags);                     // read flag info from world.fqz
  // Create the new .fqz file
  QuizFileName := QuizDir + '/' + QuizFileName; DoDirSeparators(QuizFileName);
  Assign(OutFile, QuizFileName); Rewrite(OutFile);
  // Write quiz title
  Writeln(OutFile, '**    ', QuizTitle); Writeln(OutFile);
  // Write flag info
  for I := 0 to Countries.Items.Count - 1 do begin
    // Proceed if listbox item isn't an empty string
    if Countries.Items[I] <> '' then begin
      // Find this country in the world.fqz file to retrieve flag info
      J := 1; JX := -1;
      repeat
        if (Countries.Items[I] = Flags[J].CountryNameEN) or (Countries.Items[I] = Flags[J].CountryNameDE) then
          JX := J;
        Inc(J);
      until (JX <> -1) or (J > 200);
      // Create the .fqz file line for this country (flag)
      if JX <> -1 then begin
        with Flags[JX] do begin
          S := CountryCode + ' ' + Continent + ' ';
          S += CountryNameEN;
          if CountryNameDE <> CountryNameEN then
            S += ';' + CountryNameDE;
        end;
        Writeln(OutFile, S);
      end;
    end;
  end;
  Close(OutFile);
end;

{**********}
{ TfFQZNew }
{**********}

{ Application start: Initialisation }

procedure TfFQZNew.FormCreate(Sender: TObject);

begin
  sSourcePath := '';                                                                // path to read .fqz template file from
  sDestPath := '';                                                                  // path to write new .fqz file to
  sWorldPath := '';                                                                 // path where to find world.fqz file
end;

{ Window activation: Initialistion for actual user selections }

procedure TfFQZNew.FormActivate(Sender: TObject);

begin
  if sSourcePath = '' then                                                          // If no Open has yet been done, use path passed by main unit
    sSourcePath := sQuizPath;                                                       // If no Save has yet been done, use same path as for Open
  if sDestPath = '' then
    sDestPath := sSourcePath;
  // File world.fqz must be present in APP-DIR/quiz!
  sWorldPath := sProgPath + '/quiz';
  // Clear form fields
  edTemplateFile.Text := ''; edQuizTitleEN.Text := '';
  lbTemplateCountries.Items.Clear; lbNewCountries.Items.Clear;
  btBrowse.SetFocus;
end;

{ Button "Browse": Let user choose the .fqz file to be used as template }

procedure TfFQZNew.btBrowseClick(Sender: TObject);

var
  FileName: string;

begin
  dlgOpenFlags.InitialDir := sSourcePath;
  dlgOpenFlags.FileName := '';
  if dlgOpenFlags.Execute then begin
    FileName := dlgOpenFlags.FileName;
    sSourcePath := ExtractFileDir(FileName);                                        // save directory to open directly here next time
    FileName := ExtractFileName(FileName);
    edTemplateFile.Text := FileName;
    FillTemplate(sSourcePath, FileName, sQuizLang);                                 // fill the template listbox with countries of file selected
  end;
end;

{ Button "→": Move selected country from template listbox to new quiz listbox }

procedure TfFQZNew.btMoveClick(Sender: TObject);

var
  IX, I: Integer;

begin
  // Do only if there is a listbox item selected
  if lbTemplateCountries.ItemIndex >= 0 then begin
    // Fill up empty items (created by Remove); if they are all filled, add country to end of list
    IX := -1;
    if lbNewCountries.Items.Count > 0 then begin
      I := 0;
      repeat
        if lbNewCountries.Items[I] = '' then
          IX := I;
        Inc(I);
      until (IX <> -1) or (I = lbNewCountries.Items.Count);
    end;
    if IX = -1 then
      // Add country at end of list
      lbNewCountries.Items.AddText(lbTemplateCountries.Items[lbTemplateCountries.ItemIndex])
    else
      // Insert country by replacing an empty list item
      lbNewCountries.Items[IX] := lbTemplateCountries.Items[lbTemplateCountries.ItemIndex];
    // Remove this country from the template listbox
    lbTemplateCountries.Items[lbTemplateCountries.ItemIndex] := '';
  end;
end;

{ Button "←": Move selected country from new quiz listbox to template listbox }

procedure TfFQZNew.btRemoveClick(Sender: TObject);

var
  IX, I: Integer;

begin
  // Do only if there is a listbox item selected
  if lbNewCountries.ItemIndex >= 0 then begin
    // Fill up empty items (created by Remove); if they are all filled, add country to end of list
    IX := -1;
    if lbTemplateCountries.Items.Count > 0 then begin
      I := 0;
      repeat
        if lbTemplateCountries.Items[I] = '' then
          IX := I;
        Inc(I);
      until (IX <> -1) or (I = lbTemplateCountries.Items.Count);
    end;
    if IX = 1 then
      lbTemplateCountries.Items.AddText(lbNewCountries.Items[lbNewCountries.ItemIndex])
    else
      lbTemplateCountries.Items[IX] := lbNewCountries.Items[lbNewCountries.ItemIndex];
    lbNewCountries.Items[lbNewCountries.ItemIndex] := '';
  end;
end;

{ Button "Move all": Move all countries from template listbox to new quiz listbox }

procedure TfFQZNew.btMoveAllClick(Sender: TObject);

var
  I: Integer;

begin
  if lbTemplateCountries.Items.Count > 0 then begin
    lbNewCountries.Items.Clear;
    // Fill new quiz listbox
    for I := 0 to lbTemplateCountries.Items.Count - 1 do
      lbNewCountries.Items.AddText(lbTemplateCountries.Items[I]);
    // Clear template listbox
    lbTemplateCountries.Items.Clear;
  end;
end;

{ Button "Remove all": Move all countries from new quiz listbox to template listbox }

procedure TfFQZNew.btRemoveAllClick(Sender: TObject);

var
  I: Integer;

begin
  if lbNewCountries.Items.Count > 0 then begin
    lbTemplateCountries.Items.Clear;
    // Fill template listbox
    for I := 0 to lbNewCountries.Items.Count - 1 do
      lbTemplateCountries.Items.AddText(lbNewCountries.Items[I]);
    // Clear new quiz listbox
    lbNewCountries.Items.Clear;
  end;
end;

{ Button "Save": Create new flag quiz file and close the window }

procedure TfFQZNew.btSaveClick(Sender: TObject);

var
  FileName, QuizTitle: string;

begin
  if lbNewCountries.Items.Count > 0 then begin
    // New quiz listbox contains some items (actually not checking if there are some that are not empty... )
    if (edQuizTitleEN.Text <> '') or (edQuizTitleDE.Text <> '') then begin
      // One of the quiz title edit fields has been filled in
      if (edQuizTitleEN.Text <> '') and (edQuizTitleDE.Text <> '') then
        QuizTitle := edQuizTitleEN.Text + ';' + edQuizTitleDE.Text
      else if edQuizTitleEN.Text <> '' then
        QuizTitle := edQuizTitleEN.Text
      else
        QuizTitle := edQuizTitleDE.Text;
      dlgSaveFlags.InitialDir := sDestPath;
      dlgSaveFlags.FileName := '';
      // Get directory and name for file from Save As dialog
      if dlgSaveFlags.Execute then begin
        FileName := dlgSaveFlags.FileName;
        // Do not override any existing file (this avoids to delete the files included with the application)
        if FileExists(FileName) then
          MessageDlg('Flag quiz', 'File already exists. Please, choose another FileName!', mtError, [mbOK], 0)
        else begin
          // Create new flag quiz file
          sDestPath := ExtractFileDir(FileName);                                    // save directory to open directly here next time
          FileName := ExtractFileName(FileName);
          WriteFlagFile(sWorldPath, sDestPath, FileName, QuizTitle, lbNewCountries);
          MessageDlg('Flag quiz', 'New flag quiz file successfully created.', mtError, [mbOK], 0);
          Close;
        end;
      end;
    end
    else
      MessageDlg('Flag quiz', 'Please enter a quiz title!', mtError, [mbOK], 0);
  end
  else
    MessageDlg('Flag quiz', 'No countries selected for the new quiz!', mtError, [mbOK], 0);
end;

{ Button "Cancel": Close the window (no new quiz created) }

procedure TfFQZNew.btCancelClick(Sender: TObject);

begin
  Close;
end;

end.

