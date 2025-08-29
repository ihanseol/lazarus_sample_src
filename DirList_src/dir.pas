{*************************************}
{* Main unit for DirList application *}
{*************************************}

unit dir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls, Grids, LazUTF8, Dos, help;

type
  TFileData = record
    FName, FExt, FType, FSSize, FDate, FAttr: string;
    FSize: LongWord;
  end;
  {***********}
  { TfDirList }
  {***********}
  TfDirList = class(TForm)
    mMenu: TMainMenu;
    mFile, mFileExit: TMenuItem;
    mOptions, mOptionsCaseSort, mOptionsLowExt, mOptionsOverride: TMenuItem;
    mHelp, mHelpHelp, mHelpAbout: TMenuItem;
    StaticText1: TStaticText;
    edDir: TEdit;
    btBrowse: TButton;
    sgFiles: TStringGrid;
    Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8: TLabel;
    cbListFiles, cbListFolders: TCheckBox;
    cbIncludeHidden: TCheckBox;
    cbIncludeSystem: TCheckBox;
    cbTypeD, cbTypeF, cbTypeSD, cbTypeSF: TCheckBox;
    cobSort: TComboBox;
    cbSortDesc, cbSortSysFirst, cbSortDirFirst: TCheckBox;
    rbExport1: TRadioButton;
    rbExport2: TRadioButton;
    rbExport3: TRadioButton;
    edExportFile, edSeparator: TEdit;
    btList, btExport: TButton;
    laFiles, laFolders: TLabel;
    dlgDirOpen: TSelectDirectoryDialog;
    dlgSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure mFileExitClick(Sender: TObject);
    procedure mOptionsCaseSortClick(Sender: TObject);
    procedure mOptionsLowExtClick(Sender: TObject);
    procedure mOptionsOverrideClick(Sender: TObject);
    procedure mHelpAboutClick(Sender: TObject);
    procedure mHelpHelpClick(Sender: TObject);
    procedure btBrowseClick(Sender: TObject);
    procedure btExportClick(Sender: TObject);
    procedure btListClick(Sender: TObject);
    procedure rbExport1Change(Sender: TObject);
    procedure rbExport2Change(Sender: TObject);
    procedure rbExport3Change(Sender: TObject);
    procedure cbIncludeSystemChange(Sender: TObject);
  private
    iMaxNameLen, iMaxExtLen, iTotal: Integer;
    sListDir, sSaveDir, sDir, sSeparator: string;
    aFileData: array of TFileData;
    aIndexes: array of Integer;
  end;

var
  fDirList: TfDirList;

implementation

{$R *.lfm}

{ Fill string with given character }

function FillChar(N: Integer; Ch: Char): string;

var
  I: Integer;
  S: string;

begin
  S := '';
  if N > 0 then begin
    for I := 1 to N do
      S += Ch;
  end;
  Result := S;
end;

{ Create folder listing text file }

procedure ExportTXT(Filename, Dir: string; var FileData: TStringGrid; NFiles, MaxNL, MaxEL: Integer);

const
  ColSpacing = '     ';

var
  I: Integer;
  Line: string;
  OutFile: Text;

begin
  try
    Assign(OutFile, Filename); Rewrite(OutFile);
    // Write title and column names (file/folder characteristics)
    Line := 'Folder: ' + Dir; Writeln(OutFile, Line);
    Line := FillChar(UTF8Length(Line), '='); Writeln(OutFile, Line); Writeln(OutFile);
    Line := 'Name' + FillChar(MaxNL - 4, ' ') + ColSpacing;
    Line += 'Ext' + FillChar(MaxEL - 3, ' ') + ColSpacing;
    Line += 'Type ' + ColSpacing;
    Line += '   Size  ' + ColSpacing;
    Line += '   Date    ' + ColSpacing;
    Line += 'Attributes';
    Writeln(OutFile, Line);
    Line := FillChar(UTF8Length(Line), '-'); Writeln(OutFile, Line);
    // Write objrcts characteristics values
    for I := 1 to NFiles do begin
      Line := FileData.Cells[0, I] + FillChar(MaxNL - UTF8Length(FileData.Cells[0, I]), ' ') + ColSpacing;
      if FileData.Cells[1, I] <> '' then
        Line += FileData.Cells[1, I];
      Line += FillChar(MaxEL - UTF8Length(FileData.Cells[1, I]), ' ') + ColSpacing;
      if FileData.Cells[2, I] <> '' then
        Line += FileData.Cells[2, I];
      Line += FillChar(5 - UTF8Length(FileData.Cells[2, I]), ' ') + ColSpacing;
      if FileData.Cells[3, I] <> '' then
        Line += FileData.Cells[3, I];
      Line += FillChar(10 - UTF8Length(FileData.Cells[3, I]), ' ') + ColSpacing;
      Line += FileData.Cells[4, I] + ColSpacing;
      Line += FileData.Cells[5, I] + ColSpacing;
      Writeln(OutFile, Line);
    end;
    Close(OutFile);
  except
    MessageDlg('DirList', 'Could not create export file!', mtError, [mbOK], 0);
  end;
end;

{ Create folder listing CSV file }

procedure ExportCSV(Filename: string; var FileData: TStringGrid; NFiles: Integer; Separator: string);

var
  I: Integer;
  Line: string;
  OutFile: Text;

begin
  // Usage of \t as tab character
  if Separator = '\t' then
    Separator := Chr(9);
  try
    Assign(OutFile, Filename); Rewrite(OutFile);
    // Write column names (file/folder characteristics)
    Line := '"Name"' + Separator + '"Extension"' + Separator + '"Type"' + Separator;
    Line += '"Size"' + Separator + '"Date"' + Separator + '"Attributes"';
    Writeln(OutFile, Line);
    // Write objrcts characteristics values
    for I := 1 to NFiles do begin
      Line := '"' + FileData.Cells[0, I] + '"' + Separator;
      if FileData.Cells[1, I] <> '' then
        Line += '"' + FileData.Cells[1, I] + '"';
      Line += Separator;
      if FileData.Cells[2, I] <> '' then
        Line += '"' + FileData.Cells[2, I] + '"';
      Line += Separator;
      if FileData.Cells[3, I] <> '' then
        Line += '"' + Trim(FileData.Cells[3, I]) + '"';
      Line += Separator;
      Line += '"' + FileData.Cells[4, I] + '"' + Separator;
      Line += '"' + Trim(FileData.Cells[5, I]) + '"';
      Writeln(OutFile, Line);
    end;
    Close(OutFile);
  except
    MessageDlg('DirList', 'Could not create export file!', mtError, [mbOK], 0);
  end;
end;

{ Create folder listing HTML file }

procedure ExportHTML(Filename, Dir: string; var FileData: TStringGrid; NFiles: Integer);

var
  I: Integer;
  Line: string;
  OutFile: Text;

begin
  try
    Assign(OutFile, Filename); Rewrite(OutFile);
    // Write HTML document header
    Writeln(OutFile, '<!DOCTYPE html><html>');
    Writeln(OutFile, '<head><meta http-equiv="content-type" content="text/html; charset=UTF-8"/>');
    Writeln(OutFile, '<meta name="viewport" content="width=device-width, initial-scale=1"/>');
    Writeln(OutFile, '<title>Folder listing</title></head>');
    // HTML body with title and table for the file/folder information
    Writeln(OutFile, '<body>');
    Writeln(OutFile, '<h2>Directory listing of ' + Dir + '</h2>');
    Writeln(OutFile, '<table border="0" style="border-spacing:0">');
    // HTML table header (column names)
    Line := '<tr  style="background-color:Cyan">';
    Line += '<th style="text-align:left">Name</th><td width="50px">&nbsp;</td><th style="text-align:left">Ext</th><td width="50px">&nbsp;</td>';
    Line += '<th style="text-align:left">Type</th><td width="50px">&nbsp;</td><th style="text-align:center">Size</th><td width="50px">&nbsp;</td>';
    Line += '<th style="text-align:center">Date</th><td width="50px">&nbsp;</td><th style="text-align:center">Attributes</th></tr>';
    Writeln(OutFile, Line);
    // HTML table data (objrcts characteristics values)
    for I := 1 to NFiles do begin
      if I mod 2 = 1 then begin
        // All unpair rows with white background
        Writeln(OutFile, '<tr style="background-color:White">');
      end
      else begin
        // All pair rows with light-blue background
        Writeln(OutFile, '<tr style="background-color:LightCyan">');
      end;
      Writeln(OutFile, '  <td>', FileData.Cells[0, I], '</td>');
      Writeln(OutFile, '  <td width="50px">&nbsp;</td>');
      if FileData.Cells[1, I] = '' then
        Line := '&nbsp;'
      else
        Line := FileData.Cells[1, I];
      Writeln(OutFile, '  <td>', Line, '</td>');
      Writeln(OutFile, '  <td width="50px">&nbsp;</td>');
      if FileData.Cells[2, I] = '' then
        Line := '&nbsp;'
      else
        Line := FileData.Cells[2, I];
      Writeln(OutFile, '  <td>', Line, '</td>');
      Writeln(OutFile, '  <td width="50px">&nbsp;</td>');
      if FileData.Cells[3, I] = '' then
        Line := '&nbsp;'
      else
        Line := Trim(FileData.Cells[3, I]);
      Writeln(OutFile, '  <td style="text-align:right">', Line, '</td>');
      Writeln(OutFile, '  <td width="50px">&nbsp;</td>');
      Writeln(OutFile, '  <td>', FileData.Cells[4, I], '</td>');
      Writeln(OutFile, '  <td width="50px">&nbsp;</td>');
      Writeln(OutFile, '  <td style="text-align:center">', Trim(FileData.Cells[5, I]), '</td>');
      Writeln(OutFile, '</tr>');
    end;
    // End of table, HTML body and document
    Writeln(OutFile, '</table>');
    Writeln(OutFile, '</body>');
    Writeln(OutFile, '</html>');
    Close(OutFile);
  except
    MessageDlg('DirList', 'Could not create export file!', mtError, [mbOK], 0);
  end;
end;

{***********}
{ TfDirList }
{***********}

{ Application start: Initialisation }

procedure TfDirList.FormCreate(Sender: TObject);

begin
  sListDir := GetEnv('HOMEDRIVE') + GetEnv('HOMEPATH');                        // start browsing in user folder
  sSaveDir := sListDir + '\Documents';                                         // default save location
  sSeparator := ';';
end;

{ Menu "File > Exit": Exit application }

procedure TfDirList.mFileExitClick(Sender: TObject);

begin
  Close;
end;

{ Menu "Options > Letter case dependent sort": Toggle to consider letter case for sort or not }

procedure TfDirList.mOptionsCaseSortClick(Sender: TObject);

begin
  if mOptionsCaseSort.Checked then
    mOptionsCaseSort.Checked := False
  else
    mOptionsCaseSort.Checked := True;
end;

{ Menu "Options > Lowercase file extensions": Toggle to display all file extensions with lowercase or not }

procedure TfDirList.mOptionsLowExtClick(Sender: TObject);

begin
  if mOptionsLowExt.Checked then
    mOptionsLowExt.Checked := False
  else
    mOptionsLowExt.Checked := True;
end;

{ Menu "Options > Default file override without asking": Toggle to overwrite export file called "dirlist" without confirmation or not }

procedure TfDirList.mOptionsOverrideClick(Sender: TObject);

begin
  if mOptionsOverride.Checked then
    mOptionsOverride.Checked := False
  else
    mOptionsOverride.Checked := True;
end;

{ Menu "Help > Help": Open help text window }

procedure TfDirList.mHelpHelpClick(Sender: TObject);

begin
  if fHelp.Visible then
    fHelp.Close
  else
    fHelp.Show;
end;

{ Menu "Help > About": Display application about }

procedure TfDirList.mHelpAboutClick(Sender: TObject);

var
  S: string;

begin
  S := 'Microsoft Windows directory lister.' + LineEnding + LineEnding;
  S += 'Version 1.1, © allu, January-May 2021.';
  MessageDlg('About "DirList"', S, mtInformation, [mbOK], 0);
end;

{ Button "Browse": Select folder, that the directory listing should be made of }

procedure TfDirList.btBrowseClick(Sender: TObject);

var
  I, J: Integer;

begin
  dlgDirOpen.InitialDir := sListDir;
  dlgDirOpen.FileName := '';
  if dlgDirOpen.Execute then begin
    // User has selected a directory
    edDir.Text := dlgDirOpen.FileName;
    for I := 1 to sgFiles.RowCount - 1 do begin
      for J := 0 to sgFiles.ColCount - 1 do
        sgFiles.Cells[J, I] := '';
    end;
    laFiles.Visible := False; laFolders.Visible := False;
    sListDir := ExtractFileDir(edDir.Text);                                    // save parent directory (to open next time at same location)
  end;
end;

{ Button "List": Create directory listing and display in the string grid }

procedure TfDirList.btListClick(Sender: TObject);

var
  FCount, DCount, KX, L, I, J, P: Integer;
  FileSize: Real;
  Filename, Ext, FileSizeUnit, SFileSize, Attributes, SortType, Key, S: string;
  Exclude: Boolean;
  Keys: array of string;
  FileInfo: SysUtils.TSearchRec;

begin
  if (edDir.Text <> '') and (cbListFiles.Checked or cbListFolders.Checked) then begin
    if DirectoryExists(edDir.Text) then begin
      // Be sure that the directory exists
      FCount := 0; DCount := 0;                                                // File and folder counters
      SetLength(aFileData, 0);                                                 // array with file information records as elements
      sgFiles.RowCount := 21;                                                  // default number of rows in the string grid
      // Clear the string grid
      for I := 1 to sgFiles.RowCount - 1 do begin
        for J := 0 to sgFiles.ColCount - 1 do
          sgFiles.Cells[J, I] := '';
      end;
      // Adapt directory name (if necessary)
      sDir := edDir.Text;
      if RightStr(sDir, 1) <> '\' then
        sDir += '\';
      // File search (all files) for actual directory
      if SysUtils.FindFirst(sDir + '*', faAnyFile, FileInfo) = 0 then begin
        iMaxNameLen := 0; iMaxExtLen := 0;                                     // max. name and extension lengths (used for proper sort keys + formatting)
        // Get first object in folder and contnue until all objects have been done
        repeat
           Exclude := False;
           with FileInfo do begin
             if ((Attr and faDirectory) <> 0) then begin
               // Object is a folder (subdirectory)
               if (FileInfo.Name = '.') or (FileInfo.Name = '..') then
                 // Ignore the two "good old DOS" folders
                 Exclude := True
               else if not cbListFolders.Checked then
                 // Ignore folder, if only files have to be displayed
                 Exclude := True;
             end
             else begin
               // Object is a file
               if not cbListFiles.Checked then
                 // Ignore file, if only folders have to be displayed
                 Exclude := True;
             end;
             if not Exclude then begin
               // Include or exclude system and hidden objects (as selected by user)
               if ((Attr and faSysFile) <> 0) and (not cbIncludeSystem.Checked) then
                 Exclude := True
               else if ((Attr and faHidden) <> 0) and (not cbIncludeHidden.Checked) then
                 Exclude := True;
             end;
             if not Exclude then begin
               // Increment object counters
               if (Attr and faDirectory) <> 0 then
                 Inc(DCount)
               else
                 Inc(FCount);
               iTotal := DCount + FCount;
               // Set length of dynamic array and add rows to string grid, if needed
               SetLength(aFileData, iTotal);
               if iTotal + 1 > sgFiles.RowCount then
                 sgFiles.RowCount := sgFiles.RowCount + 1;
               // Get file name and extension
               Filename := ExtractFilename(FileInfo.Name);
               I := UTF8Length(Filename); P := 0;
               while (I >= 1) and (P = 0) do begin
                 if UTF8Copy(Filename, I, 1) = '.' then
                   P := I;
                 Dec(I);
               end;
               if P = 0 then begin
                 aFileData[iTotal - 1].FName := Filename;
                 aFileData[iTotal - 1].FExt := '';
               end
               else begin
                 aFileData[iTotal - 1].FName := UTF8Copy(Filename, 1, P - 1);
                 aFileData[iTotal - 1].FExt := UTF8Copy(Filename, P + 1, UTF8Length(Filename));
                 if mOptionsLowExt.Checked then
                   aFileData[iTotal - 1].FExt := UTF8LowerCase(aFileData[iTotal - 1].FExt);
               end;
               // Adapt max. file name and extension lengths
               if UTF8Length(aFileData[iTotal - 1].FName) > iMaxNameLen then
                 iMaxNameLen := UTF8Length(aFileData[iTotal - 1].FName);
               if UTF8Length(aFileData[iTotal - 1].FExt) > iMaxExtLen then
                 iMaxExtLen := UTF8Length(aFileData[iTotal - 1].FExt);
               // Determine object type: DIR, SDIR, file, sfile
               aFileData[iTotal - 1].FType := '';
               if (Attr and faDirectory) <> 0 then begin
                 if ((Attr and faSysFile) <> 0) and cbTypeSD.Checked then
                   aFileData[iTotal - 1].FType := 'SDIR'
                 else
                   aFileData[iTotal - 1].FType := 'DIR'
               end
               else begin
                 if ((Attr and faSysFile) <> 0) and cbTypeSF.Checked then
                   aFileData[iTotal - 1].FType := 'sfile'
                 else
                   aFileData[iTotal - 1].FType := 'file';
               end;
               // Get file size
               aFileData[iTotal - 1].FSize := 0; aFileData[iTotal - 1].FSSize := '';
               if not ((Attr and faDirectory) <> 0) then begin
                 // Size is returned for files only: Adapt size unit
                 FileSize := FileInfo.Size;
                 if FileSize >= 1024 * 1024 * 1024 then begin
                   FileSize := FileSize / (1024 * 1024 * 1024); FileSizeUnit := 'GB';
                 end
                 else if FileSize >= 1024 * 1024 then begin
                   FileSize := FileSize / (1024 * 1024); FileSizeUnit := 'MB';
                 end
                 else begin
                   FileSize := FileSize / 1024; FileSizeUnit := 'kB';
                 end;
                 aFileData[iTotal - 1].FSize := FileInfo.Size;                 // size in bytes (as number) used for sort on size
                 aFileData[iTotal - 1].FSSize := FloatToStrF(FileSize, ffFixed, 3, 3) + ' ' + FileSizeUnit; // formatted size string used for display
               end;
               // Get file modification date
               aFileData[iTotal - 1].FDate := FormatDateTime('yyyy-mm-dd', FileDateToDateTime(FileInfo.Time));
               // Get file attributes (faSysFile and faHidden are Windows specific -> not portable!)
               if (Attr and faDirectory) <> 0 then
                 Attributes := 'D'
               else
                 Attributes := '─';
               if (Attr and faSysFile) <> 0 then
                 Attributes += 'S'
               else
                 Attributes += '─';
               if (Attr and faHidden) <> 0 then
                 Attributes += 'H'
               else
                 Attributes += '─';
               if (Attr and faReadOnly) <> 0 then
                 Attributes += 'R'
               else
                 Attributes += '─';
               if (Attr and faArchive) <> 0 then
                 Attributes += 'A'
               else
                 Attributes += '─';
               aFileData[iTotal - 1].FAttr := Attributes;
             end;
           end;
        until SysUtils.FindNext(FileInfo) <> 0;
        // Prepare directory listing sort. Sort will consist in creation of a sort key, string that will actually be sorted and
        // when two elements have to be swapped, exchange of the index values, pointing to the elements of the file information  array
        SetLength(aIndexes, iTotal); SetLength(Keys, iTotal);
        for I := 0 to iTotal - 1 do
          aIndexes[I] := I;
        for I := 0 to iTotal - 1 do begin
          // Sort key always contains the object name (thus whatever is the first sort criteria, within this sort will be on name)
          Keys[I] := aFileData[I].FName; L := UTF8Length(aFileData[I].FName);
          Keys[I] += FillChar(iMaxNameLen - L, ' ');                           // all values in sort key must be same length!
          // Sort on extension (if so selected) or otherwise sort extension for equal names
          Ext := aFileData[I].FExt; L := UTF8Length(Ext);
          Ext += FillChar(iMaxExtLen - L, ' ');
          if cobSort.Text = 'Extension' then
            Keys[I] := Ext + Keys[I]
          else
            Keys[I] := Keys[I] + Ext;
          // Sort on file date or size
          if cobSort.Text = 'Date' then
            Keys[I] := aFileData[I].FDate + Keys[I]
          else if cobSort.Text = 'Size' then begin
            SFileSize := IntToStr(aFileData[I].FSize); L := UTF8Length(SFileSize);
            SFileSize := FillChar(12 - L, '0') + SFileSize;
            Keys[I] := SFileSize + Keys[I];
          end;
          // Sort on file type, considering which types have to be used, if folders have to be first, if system objects have to preceed others
          if RightStr(aFileData[I].FType, 4) = 'file' then begin
            if aFileData[I].FType = 'sfile' then begin
              if cbSortSysFirst.Checked then
                SortType := '10'
              else
                SortType := '11';
            end
            else
              SortType := '11';
          end
          else begin
            if cbSortDirFirst.Checked then begin
              if aFileData[I].FType = 'SDIR' then begin
                if cbSortSysFirst.Checked then
                  SortType := '00'
                else
                  SortType := '01';
              end
              else
                SortType := '01';
            end
            else begin
              if aFileData[I].FType = 'SDIR' then begin
                if cbSortSysFirst.Checked then
                  SortType := '10'
                else
                  SortType := '11';
              end
              else
                SortType := '11';
            end;
          end;
          if cbSortDesc.Checked then begin
            // Adapt the type part of the sort key for descending sort order (folders and/or system objects should always be first, if so selected)
            for J := 1 to 2 do begin
              if SortType[J] = '0' then
                SortType[J] := '1'
              else
                SortType[J] := '0';
            end;
          end;
          Keys[I] := SortType + Keys[I];
          // Consider letter case dependent sort or not
          if not mOptionsCaseSort.Checked then
            Keys[I] := UTF8LowerCase(Keys[I]);
        end;
        // Directory listing sort (simple bubble sort)
        for I := 0 to iTotal - 2 do begin
          for J := I + 1 to iTotal - 1 do begin
            if (cbSortDesc.Checked and (Keys[J] > Keys[I])) or (not cbSortDesc.Checked and (Keys[J] < Keys[I])) then begin
              Key := Keys[I]; Keys[I] := Keys[J]; Keys[J] := Key;                // swap sort keys
              KX := aIndexes[I]; aIndexes[I] := aIndexes[J]; aIndexes[J] := KX;  // update indexes, pointing to the fileinfo array elements
            end;
          end;
        end;
        // Fill the string grid
        for I := 0 to iTotal - 1 do begin
          sgFiles.Cells[0, I + 1] := aFileData[aIndexes[I]].FName;
          sgFiles.Cells[1, I + 1] := aFileData[aIndexes[I]].FExt;
          if (aFileData[aIndexes[I]].FType <> 'file') or ((aFileData[aIndexes[I]].FType = 'file') and cbTypeF.Checked) then
            sgFiles.Cells[2, I + 1] := aFileData[aIndexes[I]].FType;
          if RightStr(aFileData[aIndexes[I]].FType, 4) = 'file' then begin
            FileSize := StrToFloat(LeftStr(aFileData[aIndexes[I]].FSSize, UTF8Length(aFileData[aIndexes[I]].FSSize) - 2));
            SFileSize := aFileData[aIndexes[I]].FSSize;
            if FileSize < 10 then
              SFileSize := '  ' + SFileSize
            else if FileSize < 100 then
              SFileSize := ' ' + SFileSize;
            sgFiles.Cells[3, I + 1] := SFileSize;
          end;
          sgFiles.Cells[4, I + 1] := aFileData[aIndexes[I]].FDate;
          sgFiles.Cells[5, I + 1] := '   ' + aFileData[aIndexes[I]].FAttr;
        end;
        // Display total files and folders
        laFolders.Visible := False; laFiles.Visible := False;
        if cbListFiles.Checked then begin
          laFiles.Visible := True;
          if cbListFolders.Checked then
            S := FillChar(UTF8Length(IntToStr(DCount)) - UTF8Length(IntToStr(FCount)), ' ');
          laFiles.Caption := 'Files:   ' + S + IntToStr(FCount);
        end;
        if cbListFolders.Checked then begin
          laFolders.Visible := True;
          if cbListFiles.Checked then
            S := FillChar(UTF8Length(IntToStr(FCount)) - UTF8Length(IntToStr(DCount)), ' ');
          laFolders.Caption := 'Folders: ' + S + IntToStr(DCount);
        end;
      end;
    end
    else begin
      // Invalid directory (user did some manual editing...)
      MessageDlg('DirList', 'Invalid or non existing directory!', mtError, [mbOK], 0);
    end;
  end;
end;

{ Button "Export": Write directory listing to file }

procedure TfDirList.btExportClick(Sender: TObject);

var
  Ret: Cardinal;
  Filename, F: string;
  CreateFile: Boolean;

begin
  if Length(aFileData) > 0 then begin
    // File save dialog box choices depending on export file type selected
    dlgSave.InitialDir := sSaveDir; dlgSave.FileName := 'dirlist';
    if rbExport1.Checked then begin
      dlgSave.DefaultExt := '.txt';
      dlgSave.Filter := 'Text files (*.txt)|*.txt|All files|*.*';
    end
    else if rbExport2.Checked then begin
      dlgSave.DefaultExt := '.csv';
      dlgSave.Filter := 'CSV files (*.csv)|*.csv|Text files (*.txt)|*.txt|All files|*.*';
    end
    else if rbExport3.Checked then begin
      dlgSave.DefaultExt := '.html';
      dlgSave.Filter := 'HTML files (*.html, *.htm)|*.html|All files|*.*';
    end;
    CreateFile := False;
    if dlgSave.Execute then begin
      // User has entered a filename
      Filename := dlgSave.FileName;
      sSaveDir := ExtractFileDir(Filename);                                      // save directory (to open next time at same location)
      // Override existing file only, if user wants so
      CreateFile := True;
      if FileExists(Filename) then begin
        CreateFile := False;
        F := ExtractFilename(Filename);
        if mOptionsOverride.Checked then begin
          if rbExport1.Checked and (F = 'dirlist.txt') then
            CreateFile := True
          else if rbExport2.Checked and (F = 'dirlist.csv') then
            CreateFile := True
          else if rbExport3.Checked and (F = 'dirlist.html') then
            CreateFile := True;
        end;
        if not CreateFile then begin
          Ret := MessageDlg('DirList', 'File already exists. Do you want to override it?', mtWarning, [mbYes, mbNo], 0, mbNo);
          if Ret = mrYes then
          CreateFile := True;
        end;
      end;
      // Export the directory listing (create text, CSV or HTML file)
      if CreateFile then begin
        if rbExport1.Checked then begin
          ExportTXT(Filename, edDir.Text, sgFiles, iTotal, iMaxNameLen, iMaxExtLen);
        end
        else if rbExport2.Checked then begin
          sSeparator := edSeparator.Text;
          ExportCSV(Filename, sgFiles, iTotal, sSeparator);
        end
        else begin
          ExportHTML(Filename, edDir.Text, sgFiles, iTotal);
        end;
      end;
    end;
  end;
end;

{ Automatic include of hidden files, if system files are selected to be included }

procedure TfDirList.cbIncludeSystemChange(Sender: TObject);

begin
  if cbIncludeSystem.Checked then
    cbIncludeHidden.Checked := True;
end;

{ Export file type selection: Enable/disable separator field }

procedure TfDirList.rbExport1Change(Sender: TObject);

begin
  // Export file = text (no separator)
  if rbExport1.Checked then begin
    // Save actual separator (for eventually reusing it)
    if edSeparator.Text = '' then
      sSeparator := ';'
    else
      sSeparator := edSeparator.Text;
    edSeparator.Text := ''; edSeparator.Enabled := False;
  end;
end;

procedure TfDirList.rbExport2Change(Sender: TObject);

begin
  // Export file CSV (with default or custom separator)
  if rbExport2.Checked then begin
    // If user did not enter a separator, display the last one used
    if edSeparator.Text = '' then
      edSeparator.Text := sSeparator;
    edSeparator.Enabled := True;
  end;
end;

procedure TfDirList.rbExport3Change(Sender: TObject);

begin
  // Export file = HTML (no separator)
  if rbExport3.Checked then begin
    // Save actual separator (for eventually reusing it)
    if edSeparator.Text = '' then
      sSeparator := ';'
    else
      sSeparator := edSeparator.Text;
    edSeparator.Text := ''; edSeparator.Enabled := False;
  end;
end;

end.

